/-
  Oracle/Reactive/Integration.lean

  Integration helpers for reactive Oracle usage.
  Provides event-driven streaming, throttling, and retry combinators.
-/
import Reactive.Host.Spider.Core
import Reactive.Host.Spider.Event
import Reactive.Host.Spider.Async
import Reactive.Core.AsyncState
import Reactive.Core.Retry
import Chronos
import Oracle.Reactive.Types
import Oracle.Reactive.Client

namespace Oracle.Reactive

open Reactive
open Reactive.Host
open Oracle

/-- Start a streaming request for each event occurrence.
    Each new event cancels any previous in-progress stream.

    Uses generation counters for staleness detection - previous streams
    continue running but their updates are discarded.

    Returns a Dynamic that holds the current StreamingRequestOutput if
    a stream is active, or none if idle. -/
def streamOnEvent (client : ReactiveClient) (requests : Evt ChatRequest)
    : SpiderM (Dyn (Option StreamingRequestOutput)) := ⟨fun env => do
  let generationRef ← IO.mkRef (0 : Nat)
  let (outputDyn, updateOutput) ← createDynamic env.timelineCtx (none : Option StreamingRequestOutput)
  let framedUpdate := fun v => env.withFrame (updateOutput v)

  -- Track current cancel function
  let currentCancelRef ← IO.mkRef (pure () : IO Unit)

  let unsub ← Reactive.Event.subscribe requests fun req => do
    -- Cancel previous stream
    let prevCancel ← currentCancelRef.get
    prevCancel

    -- Increment generation
    let generation ← generationRef.modifyGet fun g => (g + 1, g + 1)

    -- Start new stream
    let output ← (client.chatStream req).run env
    currentCancelRef.set output.cancel

    -- Only update if still current generation
    let currentGen ← generationRef.get
    if currentGen == generation then
      framedUpdate (some output)

      -- Subscribe to completion to clear the output
      let unsub ← Reactive.Event.subscribe output.completed fun _ => do
        let currentGen ← generationRef.get
        if currentGen == generation then
          framedUpdate none

      env.currentScope.register unsub

      -- Subscribe to errors to clear the output
      let unsubErr ← Reactive.Event.subscribe output.errored fun _ => do
        let currentGen ← generationRef.get
        if currentGen == generation then
          framedUpdate none

      env.currentScope.register unsubErr

  env.currentScope.register unsub
  pure outputDyn
⟩

/-- Rate-limited version of streamOnEvent.
    Uses debounce to prevent rapid-fire requests.

    @param debounceTime Duration to wait for quiet period before starting request -/
def throttledStreamOnEvent (client : ReactiveClient) (debounceTime : Chronos.Duration)
    (requests : Evt ChatRequest) : SpiderM (Dyn (Option StreamingRequestOutput)) := ⟨fun env => do
  -- Debounce the requests first
  let debouncedReqs ← Event.debounceM debounceTime requests |>.run env

  -- Then use streamOnEvent on debounced requests
  (streamOnEvent client debouncedReqs).run env
⟩

/-- Internal: attempt loop for streamWithRetry -/
private partial def streamWithRetryLoop
    (framedUpdate : Reactive.AsyncState (Reactive.RetryState × OracleError) StreamingRequestOutput → IO Unit)
    (config : Reactive.RetryConfig) (client : ReactiveClient) (req : ChatRequest)
    (env : SpiderEnv) (canceledRef : IO.Ref Bool) (currentCancelRef : IO.Ref (IO Unit))
    (retryState : Reactive.RetryState) : IO Unit := do
  let canceled ← canceledRef.get
  if canceled then return ()

  -- Start the stream
  let output ← (client.chatStream req).run env

  -- Check if request state indicates immediate error
  let initialState ← output.requestState.sample
  match initialState with
  | .error err =>
    -- Check if retryable
    if err.isRetryable && retryState.canRetry config then
      let now ← IO.monoMsNow
      let newState := retryState.recordRetryFailure now (toString err)
      let delayMs := retryState.backoffDelayMs config
      IO.sleep (UInt32.ofNat delayMs)
      streamWithRetryLoop framedUpdate config client req env canceledRef currentCancelRef newState
    else
      let now ← IO.monoMsNow
      let finalState := { retryState with lastAttemptTime := now, lastError := some (toString err) }
      framedUpdate (Reactive.AsyncState.error (finalState, err))

  | _ =>
    -- Request started (loading or streaming), success
    currentCancelRef.set output.cancel
    framedUpdate (Reactive.AsyncState.ready output)

    -- Subscribe to errors for post-connection failures
    let unsub ← Reactive.Event.subscribe output.errored fun err => do
      -- Post-connection errors don't retry
      let now ← IO.monoMsNow
      let finalState := { retryState with lastAttemptTime := now, lastError := some (toString err) }
      framedUpdate (Reactive.AsyncState.error (finalState, err))

    env.currentScope.register unsub

/-- Execute a streaming request with retry on failure.

    Retries the entire request (not individual chunks) when the initial
    connection fails with a retryable error.

    @param config Retry configuration (max retries, backoff, etc.)
    @param client The ReactiveClient to use
    @param req The ChatRequest to send

    Returns a Dynamic that tracks the overall state including retry attempts. -/
def streamWithRetry (config : Reactive.RetryConfig) (client : ReactiveClient) (req : ChatRequest)
    : SpiderM (Dyn (Reactive.AsyncState (Reactive.RetryState × OracleError) StreamingRequestOutput)) := ⟨fun env => do
  let (stateDyn, updateState) ← createDynamic env.timelineCtx (Reactive.AsyncState.loading : Reactive.AsyncState (Reactive.RetryState × OracleError) StreamingRequestOutput)
  let framedUpdate := fun v => env.withFrame (updateState v)

  -- Cancellation
  let canceledRef ← IO.mkRef false
  let currentCancelRef ← IO.mkRef (pure () : IO Unit)

  let _ ← IO.asTask (prio := .dedicated) do
    streamWithRetryLoop framedUpdate config client req env canceledRef currentCancelRef Reactive.RetryState.initial

  pure stateDyn
⟩

/-- Internal: attempt loop for chatOnEventWithRetry -/
private partial def chatOnEventWithRetryLoop
    (framedUpdate : Reactive.AsyncState (Reactive.RetryState × OracleError) ChatResponse → IO Unit)
    (config : Reactive.RetryConfig) (client : Client) (req : ChatRequest)
    (generationRef : IO.Ref Nat) (generation : Nat)
    (retryState : Reactive.RetryState) : IO Unit := do
  let currentGen ← generationRef.get
  if currentGen != generation then return ()

  match ← client.chat req with
  | .ok response =>
    let currentGen ← generationRef.get
    if currentGen == generation then
      framedUpdate (Reactive.AsyncState.ready response)

  | .error err =>
    let currentGen ← generationRef.get
    if currentGen != generation then return ()

    if err.isRetryable && retryState.canRetry config then
      let now ← IO.monoMsNow
      let newState := retryState.recordRetryFailure now (toString err)
      let delayMs := retryState.backoffDelayMs config
      IO.sleep (UInt32.ofNat delayMs)
      chatOnEventWithRetryLoop framedUpdate config client req generationRef generation newState
    else
      let now ← IO.monoMsNow
      let finalState := { retryState with lastAttemptTime := now, lastError := some (toString err) }
      framedUpdate (Reactive.AsyncState.error (finalState, err))

/-- Execute a non-streaming request for each event with retry logic.

    Similar to asyncOnEventWithRetry from Reactive.Host.Spider.Async,
    but specialized for Oracle requests. -/
def chatOnEventWithRetry (config : Reactive.RetryConfig) (client : ReactiveClient)
    (requests : Evt ChatRequest) : SpiderM (Dyn (Reactive.AsyncState (Reactive.RetryState × OracleError) ChatResponse)) := ⟨fun env => do
  let generationRef ← IO.mkRef (0 : Nat)
  let (stateDyn, updateState) ← createDynamic env.timelineCtx (Reactive.AsyncState.pending : Reactive.AsyncState (Reactive.RetryState × OracleError) ChatResponse)
  let framedUpdate := fun v => env.withFrame (updateState v)

  let unsub ← Reactive.Event.subscribe requests fun req => do
    let generation ← generationRef.modifyGet fun g => (g + 1, g + 1)
    updateState Reactive.AsyncState.loading

    let _ ← IO.asTask (prio := .dedicated) do
      chatOnEventWithRetryLoop framedUpdate config client.client req generationRef generation Reactive.RetryState.initial

  env.currentScope.register unsub
  pure stateDyn
⟩

/-- Create a simple prompt event handler.

    Given an event of prompt strings, creates streaming requests and returns
    accumulated content. Useful for simple Q&A interfaces. -/
def promptOnEvent (client : ReactiveClient) (prompts : Evt String) (systemPrompt : Option String := none)
    : SpiderM (Dyn String × Evt String) := ⟨fun env => do
  -- Map prompts to ChatRequests
  let nodeId ← env.timelineCtx.freshNodeId
  let (reqEvt, fireReq) ← Reactive.Event.newTriggerWithId nodeId

  let unsub ← Reactive.Event.subscribe prompts fun prompt => do
    let messages := match systemPrompt with
      | some sys => #[Message.system sys, Message.user prompt]
      | none => #[Message.user prompt]
    let req : ChatRequest := {
      model := client.client.config.model
      messages := messages
    }
    fireReq req

  env.currentScope.register unsub

  -- Use streamOnEvent to handle the requests
  let streamDyn ← (streamOnEvent client reqEvt).run env

  -- Extract content from current stream
  let (contentDyn, updateContent) ← createDynamic env.timelineCtx ""
  let framedUpdateContent := fun v => env.withFrame (updateContent v)

  -- Track completed responses
  let (completedEvt, fireCompleted) ← Reactive.Event.newTrigger env.timelineCtx
  let framedFireCompleted := fun v => env.withFrame (fireCompleted v)

  -- Subscribe to stream changes
  let streamUnsub ← Reactive.Event.subscribe streamDyn.updated fun streamOpt => do
    match streamOpt with
    | some stream =>
      -- Subscribe to content updates
      let unsub ← Reactive.Event.subscribe stream.content.updated fun content =>
        framedUpdateContent content
      env.currentScope.register unsub

      -- Subscribe to completion
      let unsub2 ← Reactive.Event.subscribe stream.completed fun state =>
        framedFireCompleted state.content
      env.currentScope.register unsub2
    | none =>
      pure ()

  env.currentScope.register streamUnsub

  pure (contentDyn, completedEvt)
⟩

end Oracle.Reactive
