/-
  ChatStream SSE handling tests
-/

import Crucible
import Oracle
import Wisp

namespace Tests.ChatStreamTests

open Crucible
open Oracle
open Wisp

testSuite "ChatStream"

private def sseEvent (data : String) : String :=
  s!"data: {data}\n\n"

private def sseEmptyEvent : String :=
  "data:\n\n"

private def makeStream (events : Array String) : IO ChatStream := do
  let channel ← Std.CloseableChannel.Sync.new (α := ByteArray)
  for ev in events do
    channel.send ev.toUTF8
  channel.close
  let mockResp : Wisp.StreamingResponse := {
    status := 200
    headers := Wisp.Headers.empty
    bodyChannel := channel
  }
  let sseStream ← Wisp.HTTP.SSE.Stream.fromStreaming mockResp
  ChatStream.fromSSE sseStream

private def contentChunkJson (text : String) : String :=
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "chunk_1"),
    ("choices", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("delta", Lean.Json.mkObj [
          ("content", Lean.Json.str text)
        ])
      ]
    ])
  ]
  json.compress

test "recv parses content chunks" := do
  let stream ← makeStream #[sseEvent (contentChunkJson "Hello")]
  let chunk? ← stream.recv
  match chunk? with
  | some chunk => shouldBe chunk.content (some "Hello")
  | none => throw (IO.userError "Expected chunk")

test "recv skips keep-alive empty events" := do
  let stream ← makeStream #[sseEmptyEvent, sseEvent (contentChunkJson "Next")]
  let chunk? ← stream.recv
  match chunk? with
  | some chunk => shouldBe chunk.content (some "Next")
  | none => throw (IO.userError "Expected chunk after keep-alive")

test "recv skips invalid JSON events" := do
  let stream ← makeStream #[
    sseEvent "{not json}",
    sseEvent (contentChunkJson "Valid")
  ]
  let chunk? ← stream.recv
  match chunk? with
  | some chunk => shouldBe chunk.content (some "Valid")
  | none => throw (IO.userError "Expected valid chunk after invalid JSON")

test "recv returns none on [DONE]" := do
  let stream ← makeStream #[
    sseEvent (contentChunkJson "Hello"),
    sseEvent "[DONE]"
  ]
  let first? ← stream.recv
  match first? with
  | some chunk => shouldBe chunk.content (some "Hello")
  | none => throw (IO.userError "Expected first chunk")
  let done? ← stream.recv
  shouldBe done?.isNone true

test "collectToolCalls accumulates deltas" := do
  let delta1 := Lean.Json.mkObj [
    ("index", Lean.Json.num 0),
    ("id", Lean.Json.str "call_1"),
    ("type", Lean.Json.str "function"),
    ("function", Lean.Json.mkObj [
      ("name", Lean.Json.str "search"),
      ("arguments", Lean.Json.str "{\"q\":\"")
    ])
  ]
  let delta2 := Lean.Json.mkObj [
    ("index", Lean.Json.num 0),
    ("function", Lean.Json.mkObj [
      ("arguments", Lean.Json.str "hi\"}")
    ])
  ]
  let chunk1 := Lean.Json.mkObj [
    ("id", Lean.Json.str "chunk_1"),
    ("choices", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("delta", Lean.Json.mkObj [
          ("tool_calls", Lean.Json.arr #[delta1])
        ])
      ]
    ])
  ]
  let chunk2 := Lean.Json.mkObj [
    ("id", Lean.Json.str "chunk_2"),
    ("choices", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("delta", Lean.Json.mkObj [
          ("tool_calls", Lean.Json.arr #[delta2])
        ]),
        ("finish_reason", Lean.Json.str "stop")
      ]
    ])
  ]

  let stream ← makeStream #[
    sseEvent chunk1.compress,
    sseEvent chunk2.compress,
    sseEvent "[DONE]"
  ]
  let calls ← stream.collectToolCalls
  shouldBe calls.size 1
  shouldBe calls[0]!.id "call_1"
  shouldBe calls[0]!.function.name "search"
  shouldBe calls[0]!.function.arguments "{\"q\":\"hi\"}"

end Tests.ChatStreamTests
