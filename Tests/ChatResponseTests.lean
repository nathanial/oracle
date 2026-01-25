/-
  ChatResponse parsing tests
-/

import Crucible
import Oracle

namespace Tests.ChatResponseTests

open Crucible
open Oracle

testSuite "ChatResponse Parsing"

test "ChatResponse fromJson parses content and finish reason" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "resp_1"),
    ("model", Lean.Json.str "test-model"),
    ("choices", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("message", Lean.Json.mkObj [
          ("role", Lean.Json.str "assistant"),
          ("content", Lean.Json.str "Hello")
        ]),
        ("finish_reason", Lean.Json.str "stop")
      ]
    ])
  ]
  match Lean.fromJson? json with
  | .ok (resp : ChatResponse) =>
    shouldBe resp.content (some "Hello")
    shouldBe resp.finishReason (some "stop")
    shouldBe resp.hasToolCalls false
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "ChatResponse toolCalls parses tool call array" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "resp_2"),
    ("model", Lean.Json.str "test-model"),
    ("choices", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("message", Lean.Json.mkObj [
          ("role", Lean.Json.str "assistant"),
          ("content", Lean.Json.null),
          ("tool_calls", Lean.Json.arr #[
            Lean.Json.mkObj [
              ("id", Lean.Json.str "call_1"),
              ("type", Lean.Json.str "function"),
              ("function", Lean.Json.mkObj [
                ("name", Lean.Json.str "search"),
                ("arguments", Lean.Json.str "{\"q\":\"hi\"}")
              ])
            ]
          ])
        ]),
        ("finish_reason", Lean.Json.str "tool_calls")
      ]
    ])
  ]
  match Lean.fromJson? json with
  | .ok (resp : ChatResponse) =>
    shouldBe resp.hasToolCalls true
    match resp.toolCalls with
    | some calls =>
      shouldBe calls.size 1
      shouldBe calls[0]!.id "call_1"
      shouldBe calls[0]!.function.name "search"
    | none => throw (IO.userError "Expected tool calls")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "ChatResponse.content returns none for empty string" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "resp_3"),
    ("model", Lean.Json.str "test-model"),
    ("choices", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("index", Lean.Json.num 0),
        ("message", Lean.Json.mkObj [
          ("role", Lean.Json.str "assistant"),
          ("content", Lean.Json.str "")
        ])
      ]
    ])
  ]
  match Lean.fromJson? json with
  | .ok (resp : ChatResponse) =>
    shouldBe resp.content none
  | .error e => throw (IO.userError s!"Parse failed: {e}")

end Tests.ChatResponseTests
