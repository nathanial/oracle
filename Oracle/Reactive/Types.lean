/-
  Oracle/Reactive/Types.lean

  Core types for reactive Oracle integration.
  Provides state types for streaming, tool call accumulation, and conversation management.
-/
import Reactive.Host.Spider.Core
import Oracle.Core.Types
import Oracle.Core.Error
import Oracle.Response.Delta

namespace Oracle.Reactive

open Reactive.Host

/-- Build an array with `n` copies of a value. -/
private def mkArray {α : Type} (n : Nat) (value : α) : Array α :=
  let rec loop : Nat → Array α → Array α
    | 0, acc => acc
    | n + 1, acc => loop n (acc.push value)
  loop n #[]

/-- State of an API request.
    Unlike AsyncState, this has an explicit `streaming` state for SSE responses. -/
inductive RequestState (e a : Type) where
  | idle
  | loading
  | streaming
  | ready : a → RequestState e a
  | error : e → RequestState e a
  deriving Repr, BEq

namespace RequestState

instance : Inhabited (RequestState e a) where
  default := .idle

/-- Map a function over the success value -/
def map (f : a → b) : RequestState e a → RequestState e b
  | .idle => .idle
  | .loading => .loading
  | .streaming => .streaming
  | .ready v => .ready (f v)
  | .error err => .error err

/-- Map a function over the error value -/
def mapError (f : e → e') : RequestState e a → RequestState e' a
  | .idle => .idle
  | .loading => .loading
  | .streaming => .streaming
  | .ready v => .ready v
  | .error err => .error (f err)

/-- Check if the state is idle -/
def isIdle : RequestState e a → Bool
  | .idle => true
  | _ => false

/-- Check if the state is loading -/
def isLoading : RequestState e a → Bool
  | .loading => true
  | _ => false

/-- Check if the state is streaming -/
def isStreaming : RequestState e a → Bool
  | .streaming => true
  | _ => false

/-- Check if the state is ready with a value -/
def isReady : RequestState e a → Bool
  | .ready _ => true
  | _ => false

/-- Check if the state is an error -/
def isError : RequestState e a → Bool
  | .error _ => true
  | _ => false

/-- Check if the state is terminal (ready or error) -/
def isTerminal : RequestState e a → Bool
  | .ready _ => true
  | .error _ => true
  | _ => false

/-- Check if the state is in progress (loading or streaming) -/
def isInProgress : RequestState e a → Bool
  | .loading => true
  | .streaming => true
  | _ => false

/-- Get the success value if ready, otherwise none -/
def toOption : RequestState e a → Option a
  | .ready v => some v
  | _ => none

/-- Get the error if present, otherwise none -/
def toError : RequestState e a → Option e
  | .error err => some err
  | _ => none

/-- Get the success value or a default -/
def getOrElse (default : a) : RequestState e a → a
  | .ready v => v
  | _ => default

end RequestState

/-- Accumulates tool call deltas into a complete tool call.
    Tool calls arrive in pieces across multiple streaming chunks. -/
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

/-- State accumulated during streaming.
    Updated incrementally as chunks arrive. -/
structure StreamState where
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

namespace StreamState

/-- Create an empty stream state -/
def empty : StreamState := {}

/-- Append content to the stream state -/
def appendContent (s : StreamState) (content : String) : StreamState :=
  { s with content := s.content ++ content, chunkCount := s.chunkCount + 1 }

/-- Mark the stream as finished -/
def finish (s : StreamState) (reason : Option String := none) : StreamState :=
  { s with finished := true, finishReason := reason }

end StreamState

/-- Manages conversation history with observable state. -/
structure Conversation where
  /-- System prompt if any -/
  systemPrompt : Option String := none
  /-- Message history -/
  messages : Array Message := #[]
  deriving Repr, Inhabited

namespace Conversation

/-- Create an empty conversation -/
def empty : Conversation := {}

/-- Create a conversation with a system prompt -/
def withSystemPrompt (prompt : String) : Conversation :=
  { systemPrompt := some prompt }

/-- Add a user message to the conversation -/
def addUser (conv : Conversation) (content : String) : Conversation :=
  { conv with messages := conv.messages.push (Message.user content) }

/-- Add an assistant message to the conversation -/
def addAssistant (conv : Conversation) (content : String) : Conversation :=
  { conv with messages := conv.messages.push (Message.assistant content) }

/-- Add a message to the conversation -/
def addMessage (conv : Conversation) (msg : Message) : Conversation :=
  { conv with messages := conv.messages.push msg }

/-- Get all messages including system prompt as first message -/
def allMessages (conv : Conversation) : Array Message :=
  match conv.systemPrompt with
  | some prompt => #[Message.system prompt] ++ conv.messages
  | none => conv.messages

/-- Clear conversation history (keeps system prompt) -/
def clear (conv : Conversation) : Conversation :=
  { conv with messages := #[] }

/-- Get the number of messages (excluding system prompt) -/
def messageCount (conv : Conversation) : Nat :=
  conv.messages.size

/-- Check if conversation is empty -/
def isEmpty (conv : Conversation) : Bool :=
  conv.messages.isEmpty

/-- Get the last message if any -/
def lastMessage? (conv : Conversation) : Option Message :=
  conv.messages.back?

end Conversation

/-- Helper to merge a stream chunk into accumulated stream state -/
def StreamState.mergeChunk (state : StreamState) (chunk : StreamChunk) : StreamState :=
  let state := { state with chunkCount := state.chunkCount + 1 }

  -- Process the first choice (most common case)
  if h : 0 < chunk.choices.size then
    let choice := chunk.choices[0]

    -- Append content if present
    let state := match choice.delta.content with
      | some c => { state with content := state.content ++ c }
      | none => state

    -- Merge tool call deltas if present
    let state := match choice.delta.toolCalls with
      | some deltas =>
        deltas.foldl (init := state) fun st delta =>
          -- Ensure we have an accumulator for this index
          let toolCalls := if delta.index < st.toolCalls.size
            then st.toolCalls
            else st.toolCalls ++ (mkArray (delta.index + 1 - st.toolCalls.size) (ToolCallAccumulator.new delta.index))

          -- Update the accumulator at the index
          let acc := toolCalls[delta.index]!
          let acc' := acc.merge delta
          { st with toolCalls := toolCalls.set! delta.index acc' }
      | none => state

    -- Check for finish reason
    let state := match choice.finishReason with
      | some reason => state.finish (some reason)
      | none => state

    state
  else
    state

end Oracle.Reactive
