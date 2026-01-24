/-
  Oracle/Reactive/Client.lean

  Reactive wrapper around Oracle.Client providing push-based streaming
  and observable request state.
-/
import Reactive.Host.Spider.Core
import Reactive.Host.Spider.Async
import Reactive.Core.AsyncState
import Oracle.Client.Sync
import Oracle.Client.Stream
import Oracle.Reactive.Types
import Oracle.Reactive.Stream

namespace Oracle.Reactive

open Reactive Reactive.Host
open Oracle

/-- Output from a non-streaming reactive request -/
structure RequestOutput (a : Type) where
  /-- Observable state of the request -/
  state : Dyn (RequestState OracleError a)
  /-- Cancel the request (prevents result from updating state) -/
  cancel : IO Unit

/-- Output from a streaming reactive request -/
structure StreamingRequestOutput where
  /-- Observable state of the request/stream -/
  requestState : Dyn (RequestState OracleError StreamOutput)
  /-- Fires for each chunk when streaming -/
  chunks : Evt StreamChunk
  /-- Current accumulated content -/
  content : Dyn String
  /-- Current accumulated tool calls -/
  toolCalls : Dyn (Array ToolCallAccumulator)
  /-- Full stream state -/
  streamState : Dyn StreamState
  /-- Fires when stream completes successfully -/
  completed : Evt StreamState
  /-- Fires when stream or request errors -/
  errored : Evt OracleError
  /-- Cancel the request/stream -/
  cancel : IO Unit

/-- Reactive wrapper around Oracle.Client.
    Provides push-based streaming and observable request state. -/
structure ReactiveClient where
  /-- The underlying Oracle client -/
  client : Client

namespace ReactiveClient

/-- Create a new ReactiveClient from an Oracle Client -/
def new (client : Client) : ReactiveClient :=
  { client }

/-- Create a ReactiveClient with just an API key -/
def withApiKey (apiKey : String) : ReactiveClient :=
  new (Client.withApiKey apiKey)

/-- Create a ReactiveClient with API key and model -/
def withModel (apiKey : String) (model : String) : ReactiveClient :=
  new (Client.withModel apiKey model)

/-- Get the underlying client configuration -/
def config (rc : ReactiveClient) : Config :=
  rc.client.config

/-- Execute a non-streaming chat request with observable state.

    Returns a RequestOutput with:
    - `state`: Dynamic tracking idle → loading → ready/error
    - `cancel`: Function to cancel and discard the result

    The request runs in a background task. State updates are fired
    within frames for glitch-free propagation. -/
def chat (rc : ReactiveClient) (req : ChatRequest) : SpiderM (RequestOutput ChatResponse) := ⟨fun env => do
  -- Create state dynamic
  let (stateDyn, updateState) ← createDynamic env.timelineCtx (RequestState.loading : RequestState OracleError ChatResponse)
  let framedUpdate := fun v => env.withFrame (updateState v)

  -- Cancellation flag
  let canceledRef ← IO.mkRef false

  -- Spawn background task
  let _ ← IO.asTask (prio := .dedicated) do
    match ← rc.client.chat req with
    | .ok response =>
      let canceled ← canceledRef.get
      if !canceled then
        framedUpdate (.ready response)
    | .error err =>
      let canceled ← canceledRef.get
      if !canceled then
        framedUpdate (.error err)

  pure {
    state := stateDyn
    cancel := canceledRef.set true
  }
⟩

/-- Execute a streaming chat request with full reactive output.

    Returns a StreamingRequestOutput with:
    - `requestState`: Overall request state (loading → streaming → ready/error)
    - `chunks`: Event for each received chunk
    - `content`: Accumulated content Dynamic
    - `toolCalls`: Accumulated tool calls Dynamic
    - `streamState`: Full StreamState Dynamic
    - `completed`: Event when stream finishes
    - `errored`: Event when error occurs
    - `cancel`: Function to cancel everything

    The initial request runs in a background task. Once streaming starts,
    chunks are pumped to events via streamToEvents. -/
def chatStream (rc : ReactiveClient) (req : ChatRequest) : SpiderM StreamingRequestOutput := ⟨fun env => do
  -- Create request state dynamic
  let (requestStateDyn, updateRequestState) ← createDynamic env.timelineCtx (RequestState.loading : RequestState OracleError StreamOutput)
  let framedUpdateRequest := fun v => env.withFrame (updateRequestState v)

  -- Create error event
  let (erroredEvt, fireErrored) ← Reactive.Event.newTrigger env.timelineCtx
  let framedFireErrored := fun v => env.withFrame (fireErrored v)

  -- Create placeholder dynamics that get switched when stream starts
  let (contentDyn, updateContent) ← createDynamic env.timelineCtx ""
  let (toolCallsDyn, updateToolCalls) ← createDynamic env.timelineCtx (#[] : Array ToolCallAccumulator)
  let (streamStateDyn, updateStreamState) ← createDynamic env.timelineCtx StreamState.empty
  let (chunksEvt, fireChunk) ← Reactive.Event.newTrigger env.timelineCtx
  let (completedEvt, fireCompleted) ← Reactive.Event.newTrigger env.timelineCtx

  let framedUpdateContent := fun v => env.withFrame (updateContent v)
  let framedUpdateToolCalls := fun v => env.withFrame (updateToolCalls v)
  let framedUpdateStreamState := fun v => env.withFrame (updateStreamState v)
  let framedFireChunk := fun v => env.withFrame (fireChunk v)
  let framedFireCompleted := fun v => env.withFrame (fireCompleted v)

  -- Cancellation state
  let canceledRef ← IO.mkRef false
  let streamOutputRef ← IO.mkRef (none : Option StreamOutput)

  -- Spawn background task for initial request
  let _ ← IO.asTask (prio := .dedicated) do
    match ← rc.client.chatStream { req with stream := true } with
    | .ok chatStream =>
      let canceled ← canceledRef.get
      if canceled then return ()

      -- Set state to streaming
      framedUpdateRequest .streaming

      -- Pump the stream manually (not using streamToEvents since we already have the dynamics)
      let stateRef ← IO.mkRef StreamState.empty
      let rec loop : IO Unit := do
        let canceled ← canceledRef.get
        if canceled then return ()

        match ← chatStream.recv with
        | none =>
          -- Stream ended
          let canceled ← canceledRef.get
          if canceled then return ()
          let finalState ← stateRef.get
          let finalState := finalState.finish none
          stateRef.set finalState
          framedUpdateStreamState finalState
          framedFireCompleted finalState
          -- Create a minimal StreamOutput for the ready state
          let dummyOutput : StreamOutput := {
            chunks := chunksEvt
            content := contentDyn
            toolCalls := toolCallsDyn
            state := streamStateDyn
            completed := completedEvt
            errored := erroredEvt
            cancel := canceledRef.set true
          }
          framedUpdateRequest (.ready dummyOutput)

        | some chunk =>
          let canceled ← canceledRef.get
          if canceled then return ()

          -- Merge chunk into state
          let state ← stateRef.get
          let state' := state.mergeChunk chunk
          stateRef.set state'

          -- Update all dynamics and fire chunk event
          framedUpdateContent state'.content
          framedUpdateToolCalls state'.toolCalls
          framedUpdateStreamState state'
          framedFireChunk chunk

          -- Check if this was the final chunk
          if state'.finished then
            framedFireCompleted state'
            let dummyOutput : StreamOutput := {
              chunks := chunksEvt
              content := contentDyn
              toolCalls := toolCallsDyn
              state := streamStateDyn
              completed := completedEvt
              errored := erroredEvt
              cancel := canceledRef.set true
            }
            framedUpdateRequest (.ready dummyOutput)
          else
            loop

      -- Run with error handling
      try
        loop
      catch e =>
        let canceled ← canceledRef.get
        if !canceled then
          let err := OracleError.networkError (toString e)
          framedFireErrored err
          framedUpdateRequest (.error err)

    | .error err =>
      let canceled ← canceledRef.get
      if !canceled then
        framedFireErrored err
        framedUpdateRequest (.error err)

  pure {
    requestState := requestStateDyn
    chunks := chunksEvt
    content := contentDyn
    toolCalls := toolCallsDyn
    streamState := streamStateDyn
    completed := completedEvt
    errored := erroredEvt
    cancel := canceledRef.set true
  }
⟩

/-- Convenience method for streaming a simple prompt.

    Creates a chat request with optional system prompt and streams the response. -/
def promptStream (rc : ReactiveClient) (userPrompt : String) (systemPrompt : Option String := none)
    : SpiderM StreamingRequestOutput := do
  let messages := match systemPrompt with
    | some sys => #[Message.system sys, Message.user userPrompt]
    | none => #[Message.user userPrompt]

  let req : ChatRequest := {
    model := rc.client.config.model
    messages := messages
  }

  rc.chatStream req

/-- Execute a non-streaming prompt with observable state.

    Returns a RequestOutput with the response content as a String. -/
def prompt (rc : ReactiveClient) (userPrompt : String) (systemPrompt : Option String := none)
    : SpiderM (RequestOutput String) := ⟨fun env => do
  let messages := match systemPrompt with
    | some sys => #[Message.system sys, Message.user userPrompt]
    | none => #[Message.user userPrompt]

  -- Create state dynamic
  let (stateDyn, updateState) ← createDynamic env.timelineCtx (RequestState.loading : RequestState OracleError String)
  let framedUpdate := fun v => env.withFrame (updateState v)

  -- Cancellation flag
  let canceledRef ← IO.mkRef false

  -- Spawn background task
  let _ ← IO.asTask (prio := .dedicated) do
    match ← rc.client.complete messages with
    | .ok content =>
      let canceled ← canceledRef.get
      if !canceled then
        framedUpdate (.ready content)
    | .error err =>
      let canceled ← canceledRef.get
      if !canceled then
        framedUpdate (.error err)

  pure {
    state := stateDyn
    cancel := canceledRef.set true
  }
⟩

/-- Execute a chat request with tools and observable state -/
def chatWithTools (rc : ReactiveClient) (messages : Array Message) (tools : Array Tool)
    (opts : ChatOptions := {}) : SpiderM (RequestOutput ChatResponse) := do
  let req : ChatRequest := {
    model := rc.client.config.model
    messages := messages
    temperature := opts.temperature
    maxTokens := opts.maxTokens
    topP := opts.topP
    stop := opts.stop
    tools := some tools
    toolChoice := opts.toolChoice.orElse (fun _ => some .auto)
  }
  rc.chat req

end ReactiveClient

end Oracle.Reactive
