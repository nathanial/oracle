/-
  Oracle - Streaming Accumulator Types
  Helpers for accumulating streaming tool call deltas.
-/

import Oracle.Response.Delta

namespace Oracle

/-- Build an array with `n` copies of a value. -/
private def mkArray {α : Type} (n : Nat) (value : α) : Array α :=
  let rec loop : Nat → Array α → Array α
    | 0, acc => acc
    | n + 1, acc => loop n (acc.push value)
  loop n #[]

/-- Accumulates tool call deltas into a complete tool call. -/
structure ToolCallAccumulator where
  /-- Index in the tool calls array -/
  index : Nat
  /-- Tool call ID (arrives in first delta) -/
  id : Option String := none
  /-- Tool type (usually "function") -/
  type : Option String := none
  /-- Function name (arrives in first delta) -/
  functionName : Option String := none
  /-- Accumulated function arguments JSON string -/
  functionArguments : String := ""
  deriving Repr, BEq, Inhabited

namespace ToolCallAccumulator

/-- Create a new accumulator from an index -/
def new (index : Nat) : ToolCallAccumulator :=
  { index := index }

/-- Merge a tool call delta into this accumulator -/
def merge (acc : ToolCallAccumulator) (delta : ToolCallDelta) : ToolCallAccumulator :=
  let acc := match delta.id with
    | some id => { acc with id := some id }
    | none => acc
  let acc := match delta.type with
    | some t => { acc with type := some t }
    | none => acc
  let acc := match delta.function with
    | some f =>
      let acc := match f.name with
        | some name => { acc with functionName := some name }
        | none => acc
      match f.arguments with
      | some args => { acc with functionArguments := acc.functionArguments ++ args }
      | none => acc
    | none => acc
  acc

/-- Convert to a complete ToolCall if all required fields are present -/
def toToolCall? (acc : ToolCallAccumulator) : Option ToolCall :=
  match acc.id, acc.functionName with
  | some id, some name =>
    some {
      id := id
      type := acc.type.getD "function"
      function := {
        name := name
        arguments := acc.functionArguments
      }
    }
  | _, _ => none

/-- Check if this accumulator has enough data to form a complete tool call -/
def isComplete (acc : ToolCallAccumulator) : Bool :=
  acc.id.isSome && acc.functionName.isSome

end ToolCallAccumulator

/-- Accumulator for streaming content and tool calls. -/
structure StreamAccumulator where
  /-- Accumulated content so far -/
  content : String := ""
  /-- Number of chunks received -/
  chunkCount : Nat := 0
  /-- Accumulated tool calls (indexed by position) -/
  toolCalls : Array ToolCallAccumulator := #[]
  /-- Whether the stream has finished -/
  finished : Bool := false
  /-- Finish reason if available -/
  finishReason : Option String := none
  deriving Repr, Inhabited

namespace StreamAccumulator

/-- Create an empty stream accumulator -/
def empty : StreamAccumulator := {}

/-- Append content to the accumulator -/
def appendContent (s : StreamAccumulator) (content : String) : StreamAccumulator :=
  { s with content := s.content ++ content, chunkCount := s.chunkCount + 1 }

/-- Mark the stream as finished -/
def finish (s : StreamAccumulator) (reason : Option String := none) : StreamAccumulator :=
  { s with finished := true, finishReason := reason }

/-- Return all complete tool calls accumulated so far. -/
def completedToolCalls (s : StreamAccumulator) : Array ToolCall :=
  s.toolCalls.filterMap (·.toToolCall?)

/-- Merge a stream chunk into accumulated state. -/
def mergeChunk (state : StreamAccumulator) (chunk : StreamChunk) : StreamAccumulator :=
  let state := { state with chunkCount := state.chunkCount + 1 }

  if h : 0 < chunk.choices.size then
    let choice := chunk.choices[0]

    let state := match choice.delta.content with
      | some c => { state with content := state.content ++ c }
      | none => state

    let state := match choice.delta.toolCalls with
      | some deltas =>
        deltas.foldl (init := state) fun st delta =>
          let toolCalls := if delta.index < st.toolCalls.size
            then st.toolCalls
            else st.toolCalls ++ (mkArray (delta.index + 1 - st.toolCalls.size) (ToolCallAccumulator.new delta.index))
          let acc := toolCalls[delta.index]!
          let acc' := acc.merge delta
          { st with toolCalls := toolCalls.set! delta.index acc' }
      | none => state

    let state := match choice.finishReason with
      | some reason => state.finish (some reason)
      | none => state

    state
  else
    state

end StreamAccumulator

end Oracle
