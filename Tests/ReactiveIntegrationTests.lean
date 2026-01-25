/-
  Reactive integration tests using mock streams
-/

import Crucible
import Oracle
import Oracle.Reactive.Stream
import Reactive.Host.Spider.Core
import Wisp

namespace Tests.ReactiveIntegrationTests

open Crucible
open Oracle
open Oracle.Reactive
open Reactive.Host
open Wisp

testSuite "Reactive Integration"

private def sseEvent (data : String) : String :=
  s!"data: {data}\n\n"

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

private def waitUntil (attempts : Nat) (delayMs : UInt32) (cond : IO Bool) : IO Bool := do
  let rec loop : Nat → IO Bool
    | 0 => pure false
    | n + 1 => do
      if (← cond) then
        pure true
      else
        IO.sleep delayMs
        loop n
  loop attempts

test "streamToEvents emits completion and content" := do
  let stream ← makeStream #[
    sseEvent (contentChunkJson "Hello"),
    sseEvent "[DONE]"
  ]
  let (stateOpt, content) ← SpiderM.runFresh do
    let output ← Oracle.Reactive.streamToEvents stream
    let doneRef ← SpiderM.liftIO (IO.mkRef (none : Option StreamState))
    let _ ← SpiderM.liftIO <| Reactive.Event.subscribe output.completed fun state =>
      doneRef.set (some state)
    let _ ← SpiderM.liftIO <| waitUntil 50 20 (do
      return (← doneRef.get).isSome)
    let stateOpt ← SpiderM.liftIO doneRef.get
    let content ← SpiderM.liftIO output.content.sample
    return (stateOpt, content)

  match stateOpt with
  | some (state : StreamState) =>
    shouldBe state.content "Hello"
    shouldBe content "Hello"
  | none => throw (IO.userError "Expected completion state")

test "streamToFinal captures final state" := do
  let stream ← makeStream #[
    sseEvent (contentChunkJson "Hi"),
    sseEvent (contentChunkJson " there"),
    sseEvent "[DONE]"
  ]
  let finalOpt ← SpiderM.runFresh do
    let finalDyn ← Oracle.Reactive.streamToFinal stream
    let _ ← SpiderM.liftIO <| waitUntil 50 20 (do
      return (← finalDyn.sample).isSome)
    SpiderM.liftIO finalDyn.sample

  match finalOpt with
  | some (state : StreamState) => shouldBe state.content "Hi there"
  | none => throw (IO.userError "Expected final state")

end Tests.ReactiveIntegrationTests
