/-
  Oracle Test Suite
-/

import Crucible
import Oracle

open Crucible
open Oracle

/-- Check if a string contains a substring -/
def String.containsSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

-- ============================================================================
-- JSON serialization tests
-- ============================================================================

namespace Tests.JsonTests

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
  shouldBe msg.content "You are helpful"

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

#generate_tests

end Tests.JsonTests

-- ============================================================================
-- ChatRequest tests
-- ============================================================================

namespace Tests.RequestTests

testSuite "Chat Request"

test "ChatRequest.simple creates minimal request" := do
  let req := ChatRequest.simple "gpt-4" "Hello"
  shouldBe req.model "gpt-4"
  shouldBe req.messages.size 1
  shouldBe req.messages[0]!.role Role.user
  shouldBe req.messages[0]!.content "Hello"

test "ChatRequest.withTemperature sets temperature" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withTemperature 0.7
  shouldBe req.temperature (some 0.7)

test "ChatRequest.withMaxTokens sets max tokens" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withMaxTokens 100
  shouldBe req.maxTokens (some 100)

test "ChatRequest.withStreaming enables streaming" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withStreaming
  shouldBe req.stream true

test "ChatRequest.withSystem prepends system message" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withSystem "Be helpful"
  shouldBe req.messages.size 2
  shouldBe req.messages[0]!.role Role.system
  shouldBe req.messages[1]!.role Role.user

test "ChatRequest toJson includes required fields" := do
  let req := ChatRequest.simple "gpt-4" "Hello"
  let json := Lean.toJson req
  match json.getObjValAs? String "model" with
  | .ok m => shouldBe m "gpt-4"
  | .error _ => throw (IO.userError "Missing model")
  match json.getObjVal? "messages" with
  | .ok _ => pure ()
  | .error _ => throw (IO.userError "Missing messages")

#generate_tests

end Tests.RequestTests

-- ============================================================================
-- Config tests
-- ============================================================================

namespace Tests.ConfigTests

testSuite "Config"

test "Config.simple creates minimal config" := do
  let cfg := Config.simple "sk-test-key"
  shouldBe cfg.apiKey "sk-test-key"
  shouldBe cfg.model "anthropic/claude-sonnet-4"

test "Config.chatEndpoint returns correct URL" := do
  let cfg := Config.simple "key"
  shouldBe cfg.chatEndpoint "https://openrouter.ai/api/v1/chat/completions"

test "Config with custom base URL" := do
  let cfg : Config := { apiKey := "key", baseUrl := "https://custom.api/v1" }
  shouldBe cfg.chatEndpoint "https://custom.api/v1/chat/completions"

#generate_tests

end Tests.ConfigTests

-- ============================================================================
-- Error tests
-- ============================================================================

namespace Tests.ErrorTests

testSuite "Error Handling"

test "OracleError.isRetryable for rate limit" := do
  let err := OracleError.rateLimitError (some 30)
  shouldBe err.isRetryable true

test "OracleError.isRetryable for network error" := do
  let err := OracleError.networkError "Connection failed"
  shouldBe err.isRetryable true

test "OracleError.isRetryable for auth error" := do
  let err := OracleError.authError "Invalid key"
  shouldBe err.isRetryable false

test "OracleError toString" := do
  let err := OracleError.httpError 404 "Not found"
  let s := toString err
  shouldSatisfy (s.containsSubstr "404") "error message contains status code"

#generate_tests

end Tests.ErrorTests

-- ============================================================================
-- Response parsing tests
-- ============================================================================

namespace Tests.ResponseTests

testSuite "Response Parsing"

test "Usage fromJson parses token counts" := do
  let json := Lean.Json.mkObj [
    ("prompt_tokens", Lean.Json.num 10),
    ("completion_tokens", Lean.Json.num 20),
    ("total_tokens", Lean.Json.num 30)
  ]
  match Lean.fromJson? json with
  | .ok (usage : Usage) =>
    shouldBe usage.promptTokens 10
    shouldBe usage.completionTokens 20
    shouldBe usage.totalTokens 30
  | .error e => throw (IO.userError s!"Parse failed: {e}")

#generate_tests

end Tests.ResponseTests

-- ============================================================================
-- Main entry point
-- ============================================================================

def main : IO UInt32 := runAllSuites
