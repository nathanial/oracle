/-
  Oracle - Synchronous Client
  Non-streaming client for OpenRouter API
-/

import Lean.Data.Json
import Wisp
import Oracle.Core.Config
import Oracle.Core.Error
import Oracle.Core.Types
import Oracle.Core.Tool
import Oracle.Retry
import Oracle.Request.ChatRequest
import Oracle.Response.ChatResponse
import Oracle.Response.Usage
import Oracle.Response.GenerationStats
import Oracle.Response.Model

namespace Oracle

open Lean Json
open Wisp

/-- Options for chat completion -/
structure ChatOptions where
  temperature : Option Float := none
  maxTokens : Option Nat := none
  topP : Option Float := none
  stop : Option (Array String) := none
  tools : Option (Array Tool) := none
  toolChoice : Option ToolChoice := none
  deriving Inhabited

/-- OpenRouter API client -/
structure Client where
  config : Config
  httpClient : HTTP.Client

namespace Client

/-- Create a new client with the given configuration -/
def new (config : Config) : Client :=
  { config := config
    httpClient := HTTP.Client.new
      |>.withTimeout config.timeout }

/-- Create a new client with just an API key -/
def withApiKey (apiKey : String) : Client :=
  new (Config.simple apiKey)

/-- Create a new client with API key and model -/
def withModel (apiKey : String) (model : String) : Client :=
  new (Config.withModel apiKey model)

/-- Build an HTTP request for chat completion -/
def buildRequest (c : Client) (req : ChatRequest) : Wisp.Request :=
  let httpReq := Wisp.Request.post (c.config.chatEndpoint)
    |>.withBearerToken c.config.apiKey
    |>.withJson req.toJsonString
    |>.withTimeout c.config.timeout

  let withReferer := match c.config.siteUrl with
    | some url => httpReq.withHeader "HTTP-Referer" url
    | none => httpReq

  match c.config.siteName with
    | some name => withReferer.withHeader "X-Title" name
    | none => withReferer

/-- Parse an API error from response body -/
def parseApiError (body : String) : OracleError :=
  match Json.parse body with
  | .ok json =>
    match json.getObjVal? "error" with
    | .ok errObj =>
      let code := errObj.getObjValAs? String "code" |>.toOption |>.getD "unknown"
      let message := errObj.getObjValAs? String "message" |>.toOption |>.getD body
      .apiError code message
    | .error _ => .parseError s!"Unexpected response: {body}"
  | .error _ => .parseError s!"Invalid JSON response: {body}"

/-- Execute a chat completion request -/
def chat (c : Client) (req : ChatRequest) : IO (OracleResult ChatResponse) := do
  let httpReq := buildRequest c { req with stream := false }
  let task ← c.httpClient.execute httpReq
  let result := task.get

  match result with
  | .error e =>
    match e with
    | .timeoutError msg => return .error (.timeoutError msg)
    | .connectionError msg => return .error (.networkError msg)
    | .sslError msg => return .error (.networkError s!"SSL error: {msg}")
    | _ => return .error (.networkError s!"{e}")
  | .ok response =>
    let bodyStr := String.fromUTF8! response.body

    if response.status == 200 then
      match Json.parse bodyStr with
      | .ok json =>
        match fromJson? json with
        | .ok chatResp => return .ok chatResp
        | .error e => return .error (.parseError s!"Failed to parse response: {e}")
      | .error e => return .error (.parseError s!"Invalid JSON: {e}")
    else if response.status == 401 then
      return .error (.authError "Invalid API key")
    else if response.status == 429 then
      let retryAfter := response.header "Retry-After" >>= String.toNat?
      return .error (.rateLimitError retryAfter)
    else
      return .error (parseApiError bodyStr)

/-- Simple completion: send messages and get the response content -/
def complete (c : Client) (messages : Array Message) (opts : ChatOptions := {}) : IO (OracleResult String) := do
  let req : ChatRequest := {
    model := c.config.model
    messages := messages
    temperature := opts.temperature
    maxTokens := opts.maxTokens
    topP := opts.topP
    stop := opts.stop
    tools := opts.tools
    toolChoice := opts.toolChoice
  }

  match ← c.chat req with
  | .ok resp =>
    match resp.content with
    | some content => return .ok content
    | none =>
      if resp.hasToolCalls then
        return .error (.parseError "Response contains tool calls, not text content")
      else
        return .error (.parseError "Empty response content")
  | .error e => return .error e

/-- Send a single prompt and get the response -/
def prompt (c : Client) (userPrompt : String) (systemPrompt : Option String := none) : IO (OracleResult String) := do
  let messages := match systemPrompt with
    | some sys => #[Message.system sys, Message.user userPrompt]
    | none => #[Message.user userPrompt]
  c.complete messages

/-- Chat with tools and get the full response (for tool calls) -/
def chatWithTools (c : Client) (messages : Array Message) (tools : Array Tool)
    (opts : ChatOptions := {}) : IO (OracleResult ChatResponse) := do
  let req : ChatRequest := {
    model := c.config.model
    messages := messages
    temperature := opts.temperature
    maxTokens := opts.maxTokens
    topP := opts.topP
    stop := opts.stop
    tools := some tools
    toolChoice := opts.toolChoice.orElse (fun _ => some .auto)
  }
  c.chat req

/-- Get generation statistics for a completed request -/
def getGeneration (c : Client) (generationId : String) : IO (OracleResult GenerationStats) := do
  let httpReq := Wisp.Request.get (c.config.generationEndpoint generationId)
    |>.withBearerToken c.config.apiKey
    |>.withTimeout c.config.timeout

  let task ← c.httpClient.execute httpReq
  let result := task.get

  match result with
  | .error e =>
    match e with
    | .timeoutError msg => return .error (.timeoutError msg)
    | .connectionError msg => return .error (.networkError msg)
    | .sslError msg => return .error (.networkError s!"SSL error: {msg}")
    | _ => return .error (.networkError s!"{e}")
  | .ok response =>
    let bodyStr := String.fromUTF8! response.body

    if response.status == 200 then
      match Json.parse bodyStr with
      | .ok json =>
        -- The API wraps the response in a "data" field
        let dataJson := json.getObjVal? "data" |>.toOption |>.getD json
        match fromJson? dataJson with
        | .ok stats => return .ok stats
        | .error e => return .error (.parseError s!"Failed to parse generation stats: {e}")
      | .error e => return .error (.parseError s!"Invalid JSON: {e}")
    else if response.status == 401 then
      return .error (.authError "Invalid API key")
    else if response.status == 429 then
      let retryAfter := response.header "Retry-After" >>= String.toNat?
      return .error (.rateLimitError retryAfter)
    else
      return .error (parseApiError bodyStr)

/-- List all available models -/
def listModels (c : Client) : IO (OracleResult ModelsResponse) := do
  let httpReq := Wisp.Request.get c.config.modelsEndpoint
    |>.withBearerToken c.config.apiKey
    |>.withTimeout c.config.timeout

  let task ← c.httpClient.execute httpReq
  let result := task.get

  match result with
  | .error e =>
    match e with
    | .timeoutError msg => return .error (.timeoutError msg)
    | .connectionError msg => return .error (.networkError msg)
    | .sslError msg => return .error (.networkError s!"SSL error: {msg}")
    | _ => return .error (.networkError s!"{e}")
  | .ok response =>
    let bodyStr := String.fromUTF8! response.body

    if response.status == 200 then
      match Json.parse bodyStr with
      | .ok json =>
        match fromJson? json with
        | .ok models => return .ok models
        | .error e => return .error (.parseError s!"Failed to parse models: {e}")
      | .error e => return .error (.parseError s!"Invalid JSON: {e}")
    else if response.status == 401 then
      return .error (.authError "Invalid API key")
    else if response.status == 429 then
      let retryAfter := response.header "Retry-After" >>= String.toNat?
      return .error (.rateLimitError retryAfter)
    else
      return .error (parseApiError bodyStr)

/-- Get a specific model by ID -/
def getModel (c : Client) (modelId : String) : IO (OracleResult Model) := do
  match ← c.listModels with
  | .ok models =>
    match models.findById modelId with
    | some model => return .ok model
    | none => return .error (.apiError "not_found" s!"Model not found: {modelId}")
  | .error e => return .error e

-- ============================================================================
-- Retry-enabled methods
-- ============================================================================

/-- Chat with automatic retry on transient errors -/
def chatWithRetry (c : Client) (req : ChatRequest) (cfg : RetryConfig := {}) : IO (RetryResult ChatResponse) :=
  Retry.withRetry cfg (c.chat req)

/-- Complete with automatic retry on transient errors -/
def completeWithRetry (c : Client) (messages : Array Message) (opts : ChatOptions := {}) (cfg : RetryConfig := {}) : IO (RetryResult String) :=
  Retry.withRetry cfg (c.complete messages opts)

/-- Prompt with automatic retry on transient errors -/
def promptWithRetry (c : Client) (userPrompt : String) (systemPrompt : Option String := none) (cfg : RetryConfig := {}) : IO (RetryResult String) :=
  Retry.withRetry cfg (c.prompt userPrompt systemPrompt)

/-- Get generation stats with automatic retry -/
def getGenerationWithRetry (c : Client) (generationId : String) (cfg : RetryConfig := {}) : IO (RetryResult GenerationStats) :=
  Retry.withRetry cfg (c.getGeneration generationId)

/-- List models with automatic retry -/
def listModelsWithRetry (c : Client) (cfg : RetryConfig := {}) : IO (RetryResult ModelsResponse) :=
  Retry.withRetry cfg c.listModels

end Client

end Oracle
