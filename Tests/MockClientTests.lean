/-
  Mock client tests
-/

import Crucible
import Oracle
import Oracle.Agent

namespace Tests.MockClientTests

open Crucible
open Oracle
open Oracle.Agent

testSuite "Mock Client"

test "MockChat returns responses in sequence" := do
  let resp1 := mockResponseWithContent "r1" "First"
  let resp2 := mockResponseWithContent "r2" "Second"
  let mock ← MockChat.new #[resp1, resp2]

  let dummyReq : ChatRequest := ChatRequest.simple "model" "test"

  match ← mock.call dummyReq with
  | .ok r => shouldBe r.content (some "First")
  | .error _ => throw (IO.userError "Expected success")

  match ← mock.call dummyReq with
  | .ok r => shouldBe r.content (some "Second")
  | .error _ => throw (IO.userError "Expected success")

test "MockChat returns error when exhausted" := do
  let resp := mockResponseWithContent "r1" "Only one"
  let mock ← MockChat.new #[resp]

  let dummyReq : ChatRequest := ChatRequest.simple "model" "test"

  -- First call succeeds
  match ← mock.call dummyReq with
  | .ok _ => pure ()
  | .error _ => throw (IO.userError "First call should succeed")

  -- Second call fails
  match ← mock.call dummyReq with
  | .ok _ => throw (IO.userError "Should have failed")
  | .error (OracleError.apiError code _) => shouldBe code "mock_exhausted"
  | .error _ => throw (IO.userError "Expected apiError")

test "MockChat.reset restarts sequence" := do
  let resp := mockResponseWithContent "r1" "Response"
  let mock ← MockChat.new #[resp]

  let dummyReq : ChatRequest := ChatRequest.simple "model" "test"

  -- First call
  let _ ← mock.call dummyReq

  -- Reset and call again
  mock.reset
  match ← mock.call dummyReq with
  | .ok r => shouldBe r.content (some "Response")
  | .error _ => throw (IO.userError "Should succeed after reset")

test "MockChat.callCount tracks calls" := do
  let resp := mockResponseWithContent "r1" "Test"
  let mock ← MockChat.new #[resp, resp, resp]

  let dummyReq : ChatRequest := ChatRequest.simple "model" "test"

  shouldBe (← mock.callCount) 0

  let _ ← mock.call dummyReq
  shouldBe (← mock.callCount) 1

  let _ ← mock.call dummyReq
  shouldBe (← mock.callCount) 2

test "mockResponseWithToolCalls creates valid response" := do
  let toolCall := mockToolCall "id_1" "my_func" "{\"arg\":1}"
  let resp := mockResponseWithToolCalls "resp_id" #[toolCall]

  shouldBe resp.id "resp_id"
  shouldBe resp.hasToolCalls true
  match resp.toolCalls with
  | some calls =>
    shouldBe calls.size 1
    shouldBe calls[0]!.id "id_1"
    shouldBe calls[0]!.function.name "my_func"
  | none => throw (IO.userError "Expected tool calls")

test "mockResponseWithContent creates valid response" := do
  let resp := mockResponseWithContent "resp_id" "Hello world"

  shouldBe resp.id "resp_id"
  shouldBe resp.content (some "Hello world")
  shouldBe resp.hasToolCalls false

test "mockToolCall creates valid tool call" := do
  let tc := mockToolCall "call_123" "search" "{\"query\":\"test\"}"

  shouldBe tc.id "call_123"
  shouldBe tc.type "function"
  shouldBe tc.function.name "search"
  shouldBe tc.function.arguments "{\"query\":\"test\"}"

end Tests.MockClientTests
