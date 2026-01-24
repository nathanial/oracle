/-
  JSON serialization tests
-/

import Crucible
import Oracle

namespace Tests.JsonTests

open Crucible
open Oracle

testSuite "JSON Serialization"

test "Role toJson/fromJson roundtrip" := do
  let roles := #[Role.system, Role.user, Role.assistant, Role.tool, Role.developer]
  for role in roles do
    let json := Lean.toJson role
    match Lean.fromJson? json with
    | .ok parsed => shouldBe parsed role
    | .error e => throw (IO.userError s!"Failed to parse role: {e}")

test "Message toJson includes required fields" := do
  let msg := Message.user "Hello, world!"
  let json := Lean.toJson msg
  match json.getObjValAs? String "role" with
  | .ok role => shouldBe role "user"
  | .error _ => throw (IO.userError "Missing role field")
  match json.getObjValAs? String "content" with
  | .ok content => shouldBe content "Hello, world!"
  | .error _ => throw (IO.userError "Missing content field")

test "Message.system creates system message" := do
  let msg := Message.system "You are helpful"
  shouldBe msg.role Role.system
  shouldBe msg.content.asString "You are helpful"

test "Message.toolResponse includes tool_call_id" := do
  let msg := Message.toolResponse "call_123" "Result data"
  shouldBe msg.role Role.tool
  shouldBe msg.toolCallId (some "call_123")
  let json := Lean.toJson msg
  match json.getObjValAs? String "tool_call_id" with
  | .ok id => shouldBe id "call_123"
  | .error _ => throw (IO.userError "Missing tool_call_id field")

test "FunctionCall toJson" := do
  let fc : FunctionCall := { name := "get_weather", arguments := "{\"city\":\"NYC\"}" }
  let json := Lean.toJson fc
  match json.getObjValAs? String "name" with
  | .ok name => shouldBe name "get_weather"
  | .error _ => throw (IO.userError "Missing name field")

test "ToolCall toJson includes all fields" := do
  let tc : ToolCall := {
    id := "call_abc"
    type := "function"
    function := { name := "search", arguments := "{}" }
  }
  let json := Lean.toJson tc
  match json.getObjValAs? String "id" with
  | .ok id => shouldBe id "call_abc"
  | .error _ => throw (IO.userError "Missing id field")
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "function"
  | .error _ => throw (IO.userError "Missing type field")

test "Tool toJson" := do
  let tool := Tool.create "calculate" (some "Performs math") none
  let json := Lean.toJson tool
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "function"
  | .error _ => throw (IO.userError "Missing type field")

test "ToolChoice toJson - none" := do
  let choice := ToolChoice.none
  let json := Lean.toJson choice
  match json.getStr? with
  | .ok s => shouldBe s "none"
  | .error _ => throw (IO.userError "Expected string")

test "ToolChoice toJson - auto" := do
  let choice := ToolChoice.auto
  let json := Lean.toJson choice
  match json.getStr? with
  | .ok s => shouldBe s "auto"
  | .error _ => throw (IO.userError "Expected string")

test "ToolChoice toJson - function" := do
  let choice := ToolChoice.function "my_func"
  let json := Lean.toJson choice
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "function"
  | .error _ => throw (IO.userError "Expected object with type")

test "Message toJson/fromJson roundtrip" := do
  let msg := Message.user "Hello, world!"
  let json := Lean.toJson msg
  match Lean.fromJson? json with
  | .ok (parsed : Message) =>
    shouldBe parsed.role msg.role
    shouldBe parsed.content msg.content
  | .error e => throw (IO.userError s!"Failed to parse message: {e}")

test "Message fromJson handles null content" := do
  let json := Lean.Json.mkObj [
    ("role", Lean.Json.str "assistant"),
    ("tool_calls", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("id", Lean.Json.str "call_1"),
        ("type", Lean.Json.str "function"),
        ("function", Lean.Json.mkObj [
          ("name", Lean.Json.str "get_weather"),
          ("arguments", Lean.Json.str "{}")
        ])
      ]
    ])
  ]
  match Lean.fromJson? json with
  | .ok (msg : Message) =>
    shouldBe msg.role Role.assistant
    shouldBe msg.content.asString ""  -- Should default to empty string
    shouldSatisfy msg.toolCalls.isSome "should have tool calls"
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "Message fromJson with tool_call_id" := do
  let json := Lean.Json.mkObj [
    ("role", Lean.Json.str "tool"),
    ("content", Lean.Json.str "Result"),
    ("tool_call_id", Lean.Json.str "call_abc")
  ]
  match Lean.fromJson? json with
  | .ok (msg : Message) =>
    shouldBe msg.role Role.tool
    shouldBe msg.toolCallId (some "call_abc")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "FunctionCall toJson/fromJson roundtrip" := do
  let fc : FunctionCall := { name := "test", arguments := "{\"x\":1}" }
  let json := Lean.toJson fc
  match Lean.fromJson? json with
  | .ok (parsed : FunctionCall) =>
    shouldBe parsed.name fc.name
    shouldBe parsed.arguments fc.arguments
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "ToolCall toJson/fromJson roundtrip" := do
  let tc : ToolCall := {
    id := "call_123"
    type := "function"
    function := { name := "calc", arguments := "{}" }
  }
  let json := Lean.toJson tc
  match Lean.fromJson? json with
  | .ok (parsed : ToolCall) =>
    shouldBe parsed.id tc.id
    shouldBe parsed.type tc.type
    shouldBe parsed.function.name tc.function.name
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.JsonTests
