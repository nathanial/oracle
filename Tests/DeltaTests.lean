/-
  Streaming delta parsing tests
-/

import Crucible
import Oracle

namespace Tests.DeltaTests

open Crucible
open Oracle

testSuite "Streaming Deltas"

test "FunctionCallDelta fromJson parses fields" := do
  let json := Lean.Json.mkObj [
    ("name", Lean.Json.str "search"),
    ("arguments", Lean.Json.str "{\"q\":\"hi\"}")
  ]
  match Lean.fromJson? json with
  | .ok (delta : FunctionCallDelta) =>
    shouldBe delta.name (some "search")
    shouldBe delta.arguments (some "{\"q\":\"hi\"}")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "ToolCallDelta fromJson parses fields" := do
  let json := Lean.Json.mkObj [
    ("index", Lean.Json.num 0),
    ("id", Lean.Json.str "call_1"),
    ("type", Lean.Json.str "function"),
    ("function", Lean.Json.mkObj [
      ("name", Lean.Json.str "lookup"),
      ("arguments", Lean.Json.str "{}")
    ])
  ]
  match Lean.fromJson? json with
  | .ok (delta : ToolCallDelta) =>
    shouldBe delta.index 0
    shouldBe delta.id (some "call_1")
    shouldBe delta.type (some "function")
    match delta.function with
    | some func =>
      shouldBe func.name (some "lookup")
      shouldBe func.arguments (some "{}")
    | none => throw (IO.userError "Expected function delta")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "DeltaContent fromJson parses role/content/tool calls" := do
  let json := Lean.Json.mkObj [
    ("role", Lean.Json.str "assistant"),
    ("content", Lean.Json.str "hi"),
    ("tool_calls", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("id", Lean.Json.str "call_1"),
        ("type", Lean.Json.str "function"),
        ("function", Lean.Json.mkObj [
          ("name", Lean.Json.str "search"),
          ("arguments", Lean.Json.str "{\"q\":\"hi\"}")
        ])
      ]
    ])
  ]
  match Lean.fromJson? json with
  | .ok (delta : DeltaContent) =>
    shouldBe delta.role (some Role.assistant)
    shouldBe delta.content (some "hi")
    shouldSatisfy delta.toolCalls.isSome "should parse tool calls"
    shouldBe delta.toolCalls.get!.size 1
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "DeltaContent.isEmpty detects empty delta" := do
  let delta : DeltaContent := {}
  shouldBe delta.isEmpty true

test "StreamChunk helpers read first choice" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "chunk_1"),
    ("choices", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("delta", Lean.Json.mkObj [
          ("content", Lean.Json.str "hello")
        ]),
        ("finish_reason", Lean.Json.str "stop")
      ]
    ])
  ]
  match Lean.fromJson? json with
  | .ok (chunk : StreamChunk) =>
    shouldBe chunk.content (some "hello")
    shouldBe chunk.finishReason (some "stop")
    shouldBe chunk.isFinished true
  | .error e => throw (IO.userError s!"Parse failed: {e}")

end Tests.DeltaTests
