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
  shouldBe req.messages[0]!.content.asString "Hello"

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

test "Config.generationEndpoint returns correct URL" := do
  let cfg := Config.simple "key"
  shouldBe (cfg.generationEndpoint "gen_123") "https://openrouter.ai/api/v1/generation?id=gen_123"

test "Config.generationEndpoint with custom base URL" := do
  let cfg : Config := { apiKey := "key", baseUrl := "https://custom.api/v1" }
  shouldBe (cfg.generationEndpoint "abc") "https://custom.api/v1/generation?id=abc"



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

test "GenerationStats fromJson parses all fields" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "gen_abc123"),
    ("total_cost", Lean.Json.num 0.0015),
    ("created_at", Lean.Json.str "2024-01-15T10:30:00Z"),
    ("model", Lean.Json.str "anthropic/claude-sonnet-4"),
    ("tokens_prompt", Lean.Json.num 100),
    ("tokens_completion", Lean.Json.num 50)
  ]
  match Lean.fromJson? json with
  | .ok (stats : GenerationStats) =>
    shouldBe stats.id "gen_abc123"
    shouldBe stats.model "anthropic/claude-sonnet-4"
    shouldBe stats.promptTokens 100
    shouldBe stats.completionTokens 50
    shouldBe stats.totalTokens 150
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "GenerationStats fromJson handles optional fields" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "gen_xyz"),
    ("total_cost", Lean.Json.num 0.001),
    ("created_at", Lean.Json.str "2024-01-15T10:30:00Z"),
    ("model", Lean.Json.str "gpt-4"),
    ("tokens_prompt", Lean.Json.num 50),
    ("tokens_completion", Lean.Json.num 25),
    ("native_tokens_prompt", Lean.Json.num 48),
    ("native_tokens_completion", Lean.Json.num 23),
    ("app_id", Lean.Json.num 12345)
  ]
  match Lean.fromJson? json with
  | .ok (stats : GenerationStats) =>
    shouldBe stats.nativeTokensPrompt (some 48)
    shouldBe stats.nativeTokensCompletion (some 23)
    shouldBe stats.appId (some 12345)
  | .error e => throw (IO.userError s!"Parse failed: {e}")



end Tests.ResponseTests

-- ============================================================================
-- JSON Utilities tests
-- ============================================================================

namespace Tests.JsonUtilsTests

testSuite "JSON Utilities"

test "withOptionalFields filters none values" := do
  let json := Oracle.Json.withOptionalFields [
    ("required", some (Lean.Json.str "value")),
    ("optional", none),
    ("another", some (Lean.Json.num 42))
  ]
  -- Should have 2 fields, not 3
  match json.getObj? with
  | .ok obj =>
    shouldBe obj.size 2
    match json.getObjValAs? String "required" with
    | .ok v => shouldBe v "value"
    | .error _ => throw (IO.userError "Missing required field")
    match json.getObjValAs? Nat "another" with
    | .ok v => shouldBe v 42
    | .error _ => throw (IO.userError "Missing another field")
  | .error _ => throw (IO.userError "Expected object")

test "withOptionalFields preserves all some values" := do
  let json := Oracle.Json.withOptionalFields [
    ("a", some (Lean.Json.str "1")),
    ("b", some (Lean.Json.str "2")),
    ("c", some (Lean.Json.str "3"))
  ]
  match json.getObj? with
  | .ok obj => shouldBe obj.size 3
  | .error _ => throw (IO.userError "Expected object")

test "withOptionalFields handles empty list" := do
  let json := Oracle.Json.withOptionalFields []
  match json.getObj? with
  | .ok obj => shouldBe obj.size 0
  | .error _ => throw (IO.userError "Expected object")



end Tests.JsonUtilsTests

-- ============================================================================
-- Model tests
-- ============================================================================

namespace Tests.ModelTests

testSuite "Model Parsing"

test "Model fromJson parses basic fields" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "anthropic/claude-sonnet-4"),
    ("name", Lean.Json.str "Claude Sonnet 4"),
    ("context_length", Lean.Json.num 200000),
    ("pricing", Lean.Json.mkObj [
      ("prompt", Lean.Json.num 0.003),
      ("completion", Lean.Json.num 0.015)
    ])
  ]
  match Lean.fromJson? json with
  | .ok (model : Model) =>
    shouldBe model.id "anthropic/claude-sonnet-4"
    shouldBe model.name "Claude Sonnet 4"
    shouldBe model.contextLength 200000
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "Model fromJson handles string pricing" := do
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.str "test/model"),
    ("context_length", Lean.Json.num 4096),
    ("pricing", Lean.Json.mkObj [
      ("prompt", Lean.Json.str "0.001"),
      ("completion", Lean.Json.str "0.002")
    ])
  ]
  match Lean.fromJson? json with
  | .ok (model : Model) =>
    shouldBe model.id "test/model"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "ModelsResponse findById" := do
  let model1 : Model := {
    id := "model-a", name := "Model A", contextLength := 4096,
    pricing := { prompt := 0.001, completion := 0.002 }
  }
  let model2 : Model := {
    id := "model-b", name := "Model B", contextLength := 8192,
    pricing := { prompt := 0.002, completion := 0.004 }
  }
  let response : ModelsResponse := { data := #[model1, model2] }
  match response.findById "model-b" with
  | some m => shouldBe m.name "Model B"
  | none => throw (IO.userError "Model not found")

test "ModelsResponse withTools filters correctly" := do
  let model1 : Model := {
    id := "with-tools", name := "With Tools", contextLength := 4096,
    pricing := { prompt := 0.001, completion := 0.002 },
    supportsTools := true
  }
  let model2 : Model := {
    id := "no-tools", name := "No Tools", contextLength := 4096,
    pricing := { prompt := 0.001, completion := 0.002 },
    supportsTools := false
  }
  let response : ModelsResponse := { data := #[model1, model2] }
  let toolModels := response.withTools
  shouldBe toolModels.size 1
  shouldBe toolModels[0]!.id "with-tools"



end Tests.ModelTests

-- ============================================================================
-- Retry tests
-- ============================================================================

namespace Tests.RetryTests

testSuite "Retry Logic"

test "RetryConfig.default has reasonable values" := do
  let cfg := RetryConfig.default
  shouldBe cfg.maxRetries 3
  shouldBe cfg.initialDelayMs 1000
  shouldBe cfg.useJitter true

test "RetryConfig.none has zero retries" := do
  let cfg := RetryConfig.none
  shouldBe cfg.maxRetries 0

test "RetryConfig.delayForAttempt with exponential backoff" := do
  let cfg : RetryConfig := { initialDelayMs := 1000, backoffMultiplier := 2.0 }
  shouldBe (cfg.delayForAttempt 0) 1000
  shouldBe (cfg.delayForAttempt 1) 2000
  shouldBe (cfg.delayForAttempt 2) 4000

test "RetryConfig.delayForAttempt respects maxDelayMs" := do
  let cfg : RetryConfig := { initialDelayMs := 1000, maxDelayMs := 3000, backoffMultiplier := 2.0 }
  shouldBe (cfg.delayForAttempt 0) 1000
  shouldBe (cfg.delayForAttempt 1) 2000
  shouldBe (cfg.delayForAttempt 2) 3000  -- Capped at max
  shouldBe (cfg.delayForAttempt 3) 3000  -- Still capped



end Tests.RetryTests

-- ============================================================================
-- New request parameter tests
-- ============================================================================

namespace Tests.NewParamsTests

testSuite "New Request Parameters"

test "ChatRequest.withTopK sets top_k" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withTopK 40
  shouldBe req.topK (some 40)

test "ChatRequest.withRepetitionPenalty sets penalty" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withRepetitionPenalty 1.2
  shouldBe req.repetitionPenalty (some 1.2)

test "ChatRequest.withMinP sets min_p" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withMinP 0.1
  shouldBe req.minP (some 0.1)

test "ChatRequest.withLogprobs enables logprobs" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withLogprobs 5
  shouldBe req.logprobs (some true)
  shouldBe req.topLogprobs (some 5)

test "ChatRequest.withParallelToolCalls enables parallel" := do
  let req := ChatRequest.simple "gpt-4" "Hi" |>.withParallelToolCalls
  shouldBe req.parallelToolCalls (some true)

test "ChatRequest toJson includes new parameters" := do
  let req := ChatRequest.simple "gpt-4" "Hi"
    |>.withTopK 40
    |>.withRepetitionPenalty 1.1
  let json := Lean.toJson req
  match json.getObjValAs? Nat "top_k" with
  | .ok k => shouldBe k 40
  | .error _ => throw (IO.userError "Missing top_k")

test "ChatRequest toJson includes logit_bias as object" := do
  let req := ChatRequest.simple "gpt-4" "Hi"
    |>.withLogitBias [(100, 1.0), (200, -1.0)]
  let json := Lean.toJson req
  match json.getObjVal? "logit_bias" with
  | .ok biasJson =>
    match biasJson.getObjValAs? Float "100" with
    | .ok v => shouldBe v 1.0
    | .error _ => throw (IO.userError "Missing token 100 in logit_bias")
  | .error _ => throw (IO.userError "Missing logit_bias")



end Tests.NewParamsTests

-- ============================================================================
-- Config endpoint tests
-- ============================================================================

namespace Tests.EndpointTests

testSuite "Config Endpoints"

test "Config.modelsEndpoint returns correct URL" := do
  let cfg := Config.simple "key"
  shouldBe cfg.modelsEndpoint "https://openrouter.ai/api/v1/models"



end Tests.EndpointTests

-- ============================================================================
-- Vision/Multimodal tests
-- ============================================================================

namespace Tests.VisionTests

testSuite "Vision/Multimodal Support"

test "ImageSource.url toDataUrl returns URL unchanged" := do
  let src := ImageSource.url "https://example.com/image.jpg"
  shouldBe src.toDataUrl "https://example.com/image.jpg"

test "ImageSource.base64 toDataUrl creates data URL" := do
  let src := ImageSource.base64 "image/png" "iVBORw0KGgo="
  shouldBe src.toDataUrl "data:image/png;base64,iVBORw0KGgo="

test "MessageContent.string asString returns content" := do
  let content : MessageContent := .string "Hello"
  shouldBe content.asString "Hello"

test "MessageContent.parts asString extracts text parts" := do
  let content : MessageContent := .parts #[
    .text "First",
    .image (.url "https://example.com/img.jpg"),
    .text " Second"
  ]
  shouldBe content.asString "First Second"

test "Message.userWithImageUrl creates multimodal message" := do
  let msg := Message.userWithImageUrl "Describe this:" "https://example.com/cat.jpg"
  shouldBe msg.role Role.user
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 2
    match ps[0]! with
    | .text t => shouldBe t "Describe this:"
    | _ => throw (IO.userError "Expected text part first")
    match ps[1]! with
    | .image src detail =>
      shouldBe src.toDataUrl "https://example.com/cat.jpg"
      shouldBe detail "auto"
    | _ => throw (IO.userError "Expected image part second")
  | .string _ => throw (IO.userError "Expected parts content")

test "Message.userWithImageUrls creates message with multiple images" := do
  let msg := Message.userWithImageUrls "Compare:" #["url1", "url2"] "high"
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 3
  | .string _ => throw (IO.userError "Expected parts content")

test "Message.userWithBase64Image creates message with base64 image" := do
  let msg := Message.userWithBase64Image "Analyze:" "image/png" "iVBOR=" "low"
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 2
    match ps[1]! with
    | .image src detail =>
      shouldBe src.toDataUrl "data:image/png;base64,iVBOR="
      shouldBe detail "low"
    | _ => throw (IO.userError "Expected image part")
  | .string _ => throw (IO.userError "Expected parts content")

test "Message.userWithImages accepts mixed sources" := do
  let msg := Message.userWithImages "Mixed:" #[
    .url "https://example.com/img.jpg",
    .base64 "image/jpeg" "abc123"
  ]
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 3
  | .string _ => throw (IO.userError "Expected parts content")

test "ContentPart.text toJson serializes correctly" := do
  let part : ContentPart := .text "Hello"
  let json := Lean.toJson part
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "text"
  | .error _ => throw (IO.userError "Missing type field")
  match json.getObjValAs? String "text" with
  | .ok txt => shouldBe txt "Hello"
  | .error _ => throw (IO.userError "Missing text field")

test "ContentPart.image toJson serializes with image_url" := do
  let part : ContentPart := .image (.url "https://example.com/img.jpg") "high"
  let json := Lean.toJson part
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "image_url"
  | .error _ => throw (IO.userError "Missing type field")
  match json.getObjVal? "image_url" with
  | .ok imgObj =>
    match imgObj.getObjValAs? String "url" with
    | .ok url => shouldBe url "https://example.com/img.jpg"
    | .error _ => throw (IO.userError "Missing url field")
    match imgObj.getObjValAs? String "detail" with
    | .ok d => shouldBe d "high"
    | .error _ => throw (IO.userError "Missing detail field")
  | .error _ => throw (IO.userError "Missing image_url field")

test "ContentPart fromJson parses text part" := do
  let json := Lean.Json.mkObj [
    ("type", Lean.Json.str "text"),
    ("text", Lean.Json.str "Hello world")
  ]
  match Lean.fromJson? json with
  | .ok (part : ContentPart) =>
    match part with
    | .text t => shouldBe t "Hello world"
    | .image _ _ => throw (IO.userError "Expected text part")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "ContentPart fromJson parses image_url part" := do
  let json := Lean.Json.mkObj [
    ("type", Lean.Json.str "image_url"),
    ("image_url", Lean.Json.mkObj [
      ("url", Lean.Json.str "https://example.com/img.png"),
      ("detail", Lean.Json.str "low")
    ])
  ]
  match Lean.fromJson? json with
  | .ok (part : ContentPart) =>
    match part with
    | .image src detail =>
      shouldBe src.toDataUrl "https://example.com/img.png"
      shouldBe detail "low"
    | .text _ => throw (IO.userError "Expected image part")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "ContentPart fromJson parses base64 data URL" := do
  let json := Lean.Json.mkObj [
    ("type", Lean.Json.str "image_url"),
    ("image_url", Lean.Json.mkObj [
      ("url", Lean.Json.str "data:image/png;base64,iVBORw0KGgo="),
      ("detail", Lean.Json.str "auto")
    ])
  ]
  match Lean.fromJson? json with
  | .ok (part : ContentPart) =>
    match part with
    | .image src _ =>
      match src with
      | .base64 mediaType data =>
        shouldBe mediaType "image/png"
        shouldBe data "iVBORw0KGgo="
      | .url _ => throw (IO.userError "Expected base64 source")
    | .text _ => throw (IO.userError "Expected image part")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "MessageContent.string toJson produces string" := do
  let content : MessageContent := .string "Hello"
  let json := Lean.toJson content
  match json.getStr? with
  | .ok s => shouldBe s "Hello"
  | .error _ => throw (IO.userError "Expected string JSON")

test "MessageContent.parts toJson produces array" := do
  let content : MessageContent := .parts #[.text "Hi"]
  let json := Lean.toJson content
  match json.getArr? with
  | .ok arr => shouldBe arr.size 1
  | .error _ => throw (IO.userError "Expected array JSON")

test "MessageContent fromJson parses string" := do
  let json := Lean.Json.str "Hello"
  match Lean.fromJson? json with
  | .ok (content : MessageContent) =>
    shouldBe content.asString "Hello"
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "MessageContent fromJson parses array of parts" := do
  let json := Lean.Json.arr #[
    Lean.Json.mkObj [("type", Lean.Json.str "text"), ("text", Lean.Json.str "Hello")]
  ]
  match Lean.fromJson? json with
  | .ok (content : MessageContent) =>
    match content with
    | .parts ps => shouldBe ps.size 1
    | .string _ => throw (IO.userError "Expected parts")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "Message with multimodal content roundtrips" := do
  let msg := Message.userWithImageUrl "Describe:" "https://example.com/cat.jpg"
  let json := Lean.toJson msg
  match Lean.fromJson? json with
  | .ok (parsed : Message) =>
    shouldBe parsed.role msg.role
    match parsed.content with
    | .parts ps => shouldBe ps.size 2
    | .string _ => throw (IO.userError "Expected parts after roundtrip")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")



end Tests.VisionTests

-- ============================================================================
-- Main entry point
-- ============================================================================

def main : IO UInt32 := runAllSuites
