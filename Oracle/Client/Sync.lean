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
-- Image Generation Methods
-- ============================================================================

/-- Extract all images from response (from images field) -/
def extractImages (resp : ChatResponse) : Array ImageOutput :=
  match resp.message with
  | some msg => msg.images.getD #[]
  | none => #[]

/-- Extract first image from response (checks images field first, then content parts) -/
private def extractImage (resp : ChatResponse) : Option ImageSource :=
  match resp.message with
  | some msg =>
    -- First check the images field (new format from image generation models)
    match msg.images with
    | some imgs =>
      if h : 0 < imgs.size then
        some (imgs[0].toImageSource)
      else
        none
    | none =>
      -- Fall back to content parts (legacy format)
      match msg.content with
      | .parts parts =>
        parts.findSome? fun
          | .image source _ => some source
          | .text _ => none
      | .string _ => none
  | none => none

/-- Generate an image from a text prompt using an image-generation model.
    Returns the image as an ImageSource if successful. -/
def generateImage (c : Client) (prompt : String) (aspectRatio : Option String := none)
    (opts : ChatOptions := {}) : IO (OracleResult (Option ImageSource)) := do
  let req := ChatRequest.simple c.config.model prompt
    |>.withImageGeneration aspectRatio
    |>.withMaxTokens (opts.maxTokens.getD 4096)
  match ← c.chat req with
  | .ok resp => return .ok (extractImage resp)
  | .error e => return .error e

/-- Generate an image and return as a base64 data URL string.
    This is convenient for embedding directly in HTML or storing. -/
def generateImageDataUrl (c : Client) (prompt : String) (aspectRatio : Option String := none)
    (opts : ChatOptions := {}) : IO (OracleResult (Option String)) := do
  match ← c.generateImage prompt aspectRatio opts with
  | .ok (some source) => return .ok (some source.toDataUrl)
  | .ok none => return .ok none
  | .error e => return .error e

/-- Base64 decoding lookup table. Returns 255 for invalid characters, 0-63 for valid. -/
private def base64DecodeTable : Array UInt8 := Id.run do
  let mut table : Array UInt8 := Array.replicate 256 255
  -- A-Z = 0-25
  for i in [:26] do
    table := table.set! ('A'.toNat + i) i.toUInt8
  -- a-z = 26-51
  for i in [:26] do
    table := table.set! ('a'.toNat + i) (i + 26).toUInt8
  -- 0-9 = 52-61
  for i in [:10] do
    table := table.set! ('0'.toNat + i) (i + 52).toUInt8
  -- + = 62, / = 63
  table := table.set! '+'.toNat 62
  table := table.set! '/'.toNat 63
  return table

/-- Decode a base64 string to bytes -/
private def decodeBase64 (s : String) : Option ByteArray := Id.run do
  let chars := s.toList.filter fun c => c != '\n' && c != '\r' && c != ' '
  let mut result := ByteArray.empty
  let mut buffer : UInt32 := 0
  let mut bits : Nat := 0

  for c in chars do
    if c == '=' then
      continue  -- padding
    let idx := c.toNat
    if idx >= 256 then return none
    let val := base64DecodeTable[idx]!
    if val == 255 then return none

    buffer := (buffer <<< 6) ||| val.toUInt32
    bits := bits + 6

    if bits >= 8 then
      bits := bits - 8
      let byte := ((buffer >>> bits.toUInt32) &&& 0xFF).toUInt8
      result := result.push byte

  return some result

/-- Generate an image and save the base64 data to a file.
    Returns the file path on success. -/
def generateImageToFile (c : Client) (prompt : String) (filePath : String)
    (aspectRatio : Option String := none) (opts : ChatOptions := {}) : IO (OracleResult String) := do
  match ← c.generateImage prompt aspectRatio opts with
  | .ok (some source) =>
    match source with
    | .base64 _mediaType data =>
      -- Decode base64 and write to file
      match decodeBase64 data with
      | some bytes =>
        IO.FS.writeBinFile filePath bytes
        return .ok filePath
      | none => return .error (.parseError "Failed to decode base64 image data")
    | .url url =>
      -- For URL sources, return an error - caller should fetch the URL
      return .error (.parseError s!"Image returned as URL, not base64: {url}")
  | .ok none => return .error (.parseError "No image in response")
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
