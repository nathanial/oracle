/-
  Oracle/Reactive/Stream.lean

  Converts pull-based SSE ChatStream to push-based Reactive events.
  Spawns a background task to pump the stream and fire events in frames.
-/
import Reactive.Host.Spider.Core
import Oracle.Client.Stream
import Oracle.Reactive.Types

namespace Oracle.Reactive

open Reactive.Host
open Oracle

/-- Output from a reactive stream.
    Provides observable state and events for stream consumption. -/
structure StreamOutput where
  /-- Fires for each chunk received -/
  chunks : Evt StreamChunk
  /-- Current accumulated content -/
  content : Dyn String
  /-- Current accumulated tool calls -/
  toolCalls : Dyn (Array ToolCallAccumulator)
  /-- Full stream state -/
  state : Dyn StreamState
  /-- Fires when stream completes successfully -/
  completed : Evt StreamState
  /-- Fires when stream errors -/
  errored : Evt OracleError
  /-- Cancel the stream (prevents further updates) -/
  cancel : IO Unit

/-- Pump stream events into reactive updates (non-terminating). -/
private partial def pumpStream (stream : ChatStream) (canceledRef : IO.Ref Bool)
    (stateRef : IO.Ref StreamState)
    (framedUpdateContent : String → IO Unit)
    (framedUpdateToolCalls : Array ToolCallAccumulator → IO Unit)
    (framedUpdateState : StreamState → IO Unit)
    (framedFireChunk : StreamChunk → IO Unit)
    (framedFireCompleted : StreamState → IO Unit) : IO Unit := do
  let canceled ← canceledRef.get
  if canceled then return ()

  match ← stream.recv with
  | none =>
    let canceled ← canceledRef.get
    if canceled then return ()

    let finalState ← stateRef.get
    let finalState := finalState.finish none
    stateRef.set finalState
    framedUpdateState finalState
    framedFireCompleted finalState

  | some chunk =>
    let canceled ← canceledRef.get
    if canceled then return ()

    let state ← stateRef.get
    let state' := state.mergeChunk chunk
    stateRef.set state'

    framedUpdateContent state'.content
    framedUpdateToolCalls state'.toolCalls
    framedUpdateState state'
    framedFireChunk chunk

    if state'.finished then
      framedFireCompleted state'
    else
      pumpStream stream canceledRef stateRef framedUpdateContent framedUpdateToolCalls
        framedUpdateState framedFireChunk framedFireCompleted

/-- Convert a pull-based ChatStream into push-based reactive events.

    Spawns a background task that:
    1. Reads chunks from the ChatStream
    2. Updates Dynamics with accumulated state
    3. Fires events for each chunk
    4. Handles completion and errors

    **Cancellation semantics**: Calling `cancel` on the output sets a flag
    that prevents further state updates. The underlying stream continues
    but results are discarded. For true cancellation, the stream's network
    connection would need to be closed.

    **Thread safety**: Updates are fired within frames using `env.withFrame`
    to ensure glitch-free propagation. -/
def streamToEvents (stream : ChatStream) : SpiderM StreamOutput := ⟨fun env => do
  -- Create triggers for events
  let (chunksEvt, fireChunk) ← Reactive.Event.newTrigger env.timelineCtx
  let (completedEvt, fireCompleted) ← Reactive.Event.newTrigger env.timelineCtx
  let (erroredEvt, fireErrored) ← Reactive.Event.newTrigger (a := OracleError) env.timelineCtx

  -- Create dynamics for state
  let (contentDyn, updateContent) ← createDynamic env.timelineCtx ""
  let (toolCallsDyn, updateToolCalls) ← createDynamic env.timelineCtx (#[] : Array ToolCallAccumulator)
  let (stateDyn, updateState) ← createDynamic env.timelineCtx StreamState.empty

  -- Cancellation flag
  let canceledRef ← IO.mkRef false

  -- Framed update helpers
  let framedUpdateContent := fun v => env.withFrame (updateContent v)
  let framedUpdateToolCalls := fun v => env.withFrame (updateToolCalls v)
  let framedUpdateState := fun v => env.withFrame (updateState v)
  let framedFireChunk := fun v => env.withFrame (fireChunk v)
  let framedFireCompleted := fun v => env.withFrame (fireCompleted v)
  let framedFireErrored := fun v => env.withFrame (fireErrored v)

  -- Spawn background task to pump the stream
  let _ ← IO.asTask (prio := .dedicated) do
    let stateRef ← IO.mkRef StreamState.empty
    -- Run the loop with error handling
    try
      pumpStream stream canceledRef stateRef framedUpdateContent framedUpdateToolCalls
        framedUpdateState framedFireChunk framedFireCompleted
    catch e =>
      let canceled ← canceledRef.get
      if !canceled then
        let err := OracleError.networkError (toString e)
        let state ← stateRef.get
        framedUpdateState (state.finish (some (toString err)))
        framedFireErrored err

  pure {
    chunks := chunksEvt
    content := contentDyn
    toolCalls := toolCallsDyn
    state := stateDyn
    completed := completedEvt
    errored := erroredEvt
    cancel := canceledRef.set true
  }
⟩

/-- Subscribe to stream chunks and accumulate content.
    A simpler alternative when you only need the final content.
    Returns a Dynamic that updates with accumulated content. -/
def streamContent (stream : ChatStream) : SpiderM (Dyn String × Evt Unit) := ⟨fun env => do
  let output ← (streamToEvents stream).run env

  -- Map completed event to Unit
  let nodeId ← env.timelineCtx.freshNodeId
  let (doneEvt, fireDone) ← Reactive.Event.newTriggerWithId nodeId
  let unsub ← Reactive.Event.subscribe output.completed fun _ => fireDone ()
  env.currentScope.register unsub

  pure (output.content, doneEvt)
⟩

/-- Subscribe to stream and collect final state when complete.
    Returns a Dynamic that holds the final StreamState once streaming finishes. -/
def streamToFinal (stream : ChatStream) : SpiderM (Dyn (Option StreamState)) := ⟨fun env => do
  let output ← (streamToEvents stream).run env

  -- Hold the completed event value
  let (finalDyn, updateFinal) ← createDynamic env.timelineCtx (none : Option StreamState)
  let framedUpdate := fun v => env.withFrame (updateFinal v)

  let unsub ← Reactive.Event.subscribe output.completed fun state =>
    framedUpdate (some state)
  env.currentScope.register unsub

  pure finalDyn
⟩

end Oracle.Reactive
