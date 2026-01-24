/-
  ChatRequest tests
-/

import Crucible
import Oracle

namespace Tests.RequestTests

open Crucible
open Oracle

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
