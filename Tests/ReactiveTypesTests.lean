/-
  Reactive types tests (pure helpers)
-/

import Crucible
import Oracle.Reactive.Types

namespace Tests.ReactiveTypesTests

open Crucible
open Oracle
open Oracle.Reactive

testSuite "Reactive Types"

test "RequestState map and mapError" := do
  let state : RequestState String Nat := .ready 2
  let mapped := state.map (· + 1)
  shouldBe mapped (RequestState.ready 3)

  let errState : RequestState String Nat := .error "oops"
  let mappedErr := errState.mapError (fun e => e ++ "!")
  shouldBe mappedErr (RequestState.error "oops!")

test "Conversation helpers manage messages and system prompt" := do
  let conv := Conversation.withSystemPrompt "sys"
    |>.addUser "hi"
    |>.addAssistant "hello"
  shouldBe conv.messageCount 2
  shouldBe conv.isEmpty false
  shouldBe (conv.lastMessage? >>= fun m => some m.role) (some Role.assistant)
  let all := conv.allMessages
  shouldBe all.size 3
  shouldBe all[0]!.role Role.system
  shouldBe all[1]!.role Role.user

test "Conversation.clear keeps system prompt" := do
  let conv := Conversation.withSystemPrompt "sys"
    |>.addUser "hi"
  let cleared := conv.clear
  shouldBe cleared.messageCount 0
  let all := cleared.allMessages
  shouldBe all.size 1
  shouldBe all[0]!.role Role.system

test "StreamState.mergeChunk accumulates content and tool calls" := do
  let delta : ToolCallDelta := {
    index := 0
    id := some "call_1"
    type := some "function"
    function := some { name := some "search", arguments := some "{}" }
  }
  let chunk : StreamChunk := {
    id := "chunk_1"
    choices := #[{
      index := 0
      delta := {
        content := some "hi"
        toolCalls := some #[delta]
      }
    }]
  }
  let state := (StreamState.empty).mergeChunk chunk
  shouldBe state.content "hi"
  shouldBe state.chunkCount 1
  shouldBe state.toolCalls.size 1
  shouldBe state.toolCalls[0]!.isComplete true

end Tests.ReactiveTypesTests
