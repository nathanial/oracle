/-
  Oracle - Streaming Client
  Streaming client for OpenRouter API using SSE
-/

import Lean.Data.Json
import Wisp
import Chronicle
import Oracle.Core.Config
import Oracle.Core.Error
import Oracle.Core.Types
import Oracle.Request.ChatRequest
import Oracle.Response.Delta
import Oracle.Response.StreamAccumulator
import Oracle.Client.Sync

namespace Oracle

open Lean Json
open Wisp

/-- A stream of chat completion chunks -/
structure ChatStream where
  /-- The underlying SSE stream -/
  sseStream : Wisp.HTTP.SSE.Stream
  /-- Whether the stream has ended -/
  finished : IO.Ref Bool
  /-- Optional logger for debugging -/
  logger : Option Chronicle.Logger := none

namespace ChatStream

/-- Create a chat stream from an SSE stream -/
def fromSSE (sse : Wisp.HTTP.SSE.Stream) (logger : Option Chronicle.Logger := none) : IO ChatStream := do
  let finished ← IO.mkRef false
  return { sseStream := sse, finished := finished, logger := logger }

/-- Receive the next chunk from the stream -/
partial def recv (s : ChatStream) : IO (Option StreamChunk) := do
  if ← s.finished.get then
    return none

  let event? ← s.sseStream.recv
  match event? with
  | none =>
    if let some l := s.logger then
      l.trace "SSE stream ended (no more events)"
    s.finished.set true
    return none
  | some event =>
    -- Log raw SSE event at trace level
    if let some l := s.logger then
      let preview := if event.data.length > 100 then event.data.take 100 ++ "..." else event.data
      l.trace s!"SSE event: type={event.event} data={preview}"

    -- Check for [DONE] signal
    if event.data.trim == "[DONE]" then
      if let some l := s.logger then
        l.trace "SSE [DONE] signal received"
      s.finished.set true
      return none
    -- Skip keep-alive comments (empty data)
    else if event.data.trim.isEmpty then
      if let some l := s.logger then
        l.trace "SSE empty event (keep-alive), skipping"
      s.recv  -- Recurse to get next event
    else
      -- Parse the chunk
      match Json.parse event.data with
      | .error e =>
        if let some l := s.logger then
          l.warn s!"SSE JSON parse error: {e}"
          l.warn s!"SSE raw data: {event.data.take 200}"
        s.recv  -- Skip unparseable chunks
      | .ok json =>
        match fromJson? json with
        | .error e =>
          if let some l := s.logger then
            l.warn s!"SSE chunk decode error: {e}"
          s.recv  -- Skip malformed chunks
        | .ok chunk => return some chunk

/-- Iterate over all chunks in the stream -/
partial def forEach (s : ChatStream) (f : StreamChunk → IO Unit) : IO Unit := do
  let rec loop : IO Unit := do
    let chunk? ← s.recv
    match chunk? with
    | some chunk =>
      f chunk
      loop
    | none => return ()
  loop

/-- Collect all content from the stream into a single string -/
partial def collectContent (s : ChatStream) : IO String := do
  let contentRef ← IO.mkRef ""
  s.forEach fun chunk => do
    if let some content := chunk.content then
      let current ← contentRef.get
      contentRef.set (current ++ content)
  contentRef.get

/-- Collect all chunks from the stream -/
partial def collect (s : ChatStream) : IO (Array StreamChunk) := do
  let chunksRef ← IO.mkRef #[]
  s.forEach fun chunk => do
    let current ← chunksRef.get
    chunksRef.set (current.push chunk)
  chunksRef.get

/-- Collect full stream state (content and tool calls). -/
partial def collectState (s : ChatStream) : IO StreamAccumulator := do
  let stateRef ← IO.mkRef StreamAccumulator.empty
  s.forEach fun chunk => do
    let state ← stateRef.get
    stateRef.set (state.mergeChunk chunk)
  stateRef.get

/-- Collect all completed tool calls from the stream. -/
partial def collectToolCalls (s : ChatStream) : IO (Array ToolCall) := do
  let state ← s.collectState
  return state.completedToolCalls

/-- Print content to stdout as it arrives (for interactive use).
    Returns (content, chunkCount) for debugging. -/
partial def printContentWithCount (s : ChatStream) : IO (String × Nat) := do
  let contentRef ← IO.mkRef ""
  let countRef ← IO.mkRef 0
  let stdout ← IO.getStdout
  s.forEach fun chunk => do
    countRef.modify (· + 1)
    if let some content := chunk.content then
      IO.print content
      stdout.flush  -- Ensure content is displayed immediately
      let current ← contentRef.get
      contentRef.set (current ++ content)
  IO.println ""  -- Final newline
  let content ← contentRef.get
  let count ← countRef.get
  pure (content, count)

/-- Print content to stdout as it arrives (for interactive use) -/
partial def printContent (s : ChatStream) : IO String := do
  let (content, _) ← s.printContentWithCount
  pure content

end ChatStream

namespace Client

/-- Execute a streaming chat completion request -/
def chatStream (c : Client) (req : ChatRequest) : IO (OracleResult ChatStream) := do
  -- Log request with message details
  if let some logger := c.config.logger then
    logger.debug s!"API request: {c.config.model} ({req.messages.size} messages)"
    -- Log each message at trace level for debugging
    let mut i := 0
    for msg in req.messages do
      let role := msg.role.toString
      let preview := match msg.content with
        | .string s => if s.length > 100 then s.take 100 ++ "..." else s
        | .parts ps => s!"[{ps.size} content parts]"
      -- Replace newlines with spaces for single-line log entry
      let preview := preview.replace "\n" " "
      logger.trace s!"  [{i}] {role}: {preview}"
      i := i + 1

  let streamReq := { req with stream := true }
  let jsonBody := streamReq.toJsonString

  -- Log the actual JSON body at trace level
  if let some logger := c.config.logger then
    logger.trace s!"Request JSON ({jsonBody.length} chars): {jsonBody.take 500}"
    if jsonBody.length > 500 then
      logger.trace s!"Request JSON (continued): {jsonBody.drop 500 |>.take 500}"

  let httpReq := buildRequest c streamReq
  let task ← c.httpClient.executeStreaming httpReq
  let result := task.get

  match result with
  | .error e =>
    -- Log network errors
    if let some logger := c.config.logger then
      logger.error s!"API network error: {e}"
    match e with
    | .timeoutError msg => return .error (.timeoutError msg)
    | .connectionError msg => return .error (.networkError msg)
    | .sslError msg => return .error (.networkError s!"SSL error: {msg}")
    | _ => return .error (.networkError s!"{e}")
  | .ok streamResp =>
    if streamResp.status == 200 then
      -- Log success
      if let some logger := c.config.logger then
        logger.info s!"API response: status=200, streaming started"
      let sseStream ← Wisp.HTTP.SSE.Stream.fromStreaming streamResp
      let chatStream ← ChatStream.fromSSE sseStream c.config.logger
      return .ok chatStream
    else if streamResp.status == 401 then
      if let some logger := c.config.logger then
        logger.error "API error: Invalid API key (401)"
      return .error (.authError "Invalid API key")
    else if streamResp.status == 429 then
      if let some logger := c.config.logger then
        logger.warn "API rate limited (429)"
      return .error (.rateLimitError none)
    else
      -- For error responses, we need to read the body
      let body ← streamResp.readAllBody
      let bodyStr := String.fromUTF8! body
      if let some logger := c.config.logger then
        logger.error s!"API error: status={streamResp.status}"
        logger.error s!"API error response body: {bodyStr}"
      return .error (parseApiError bodyStr)

/-- Stream a completion and collect the full content -/
def completeStream (c : Client) (messages : Array Message) (opts : ChatOptions := {}) : IO (OracleResult ChatStream) := do
  let req : ChatRequest := {
    model := c.config.model
    messages := messages
    temperature := opts.temperature
    maxTokens := opts.maxTokens
    topP := opts.topP
    stop := opts.stop
    tools := opts.tools
    toolChoice := opts.toolChoice
    stream := true
  }
  c.chatStream req

/-- Stream a prompt and print output as it arrives -/
def promptStream (c : Client) (userPrompt : String) (systemPrompt : Option String := none) : IO (OracleResult String) := do
  let messages := match systemPrompt with
    | some sys => #[Message.system sys, Message.user userPrompt]
    | none => #[Message.user userPrompt]

  match ← c.completeStream messages with
  | .ok stream => .ok <$> stream.printContent
  | .error e => return .error e

end Client

end Oracle
