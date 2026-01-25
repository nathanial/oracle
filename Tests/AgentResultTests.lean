/-
  Agent result tests
-/

import Crucible
import Oracle
import Oracle.Agent

namespace Tests.AgentResultTests

open Crucible
open Oracle
open Oracle.Agent

testSuite "Agent Result"

test "AgentResult.isSuccess for completed state" := do
  let result : AgentResult := {
    messages := #[]
    finalContent := some "done"
    iterations := 1
    state := .completed #[] "done"
  }
  shouldBe result.isSuccess true
  shouldBe result.hitToolLimit false
  shouldBe result.isError false

test "AgentResult.hitToolLimit for toolLimit state" := do
  let result : AgentResult := {
    messages := #[]
    finalContent := none
    iterations := 10
    state := .toolLimit #[]
  }
  shouldBe result.isSuccess false
  shouldBe result.hitToolLimit true
  shouldBe result.isError false

test "AgentResult.isError for error state" := do
  let result : AgentResult := {
    messages := #[]
    finalContent := none
    iterations := 1
    state := .error #[] (OracleError.networkError "fail")
  }
  shouldBe result.isSuccess false
  shouldBe result.hitToolLimit false
  shouldBe result.isError true
  shouldBe result.isStopped false

test "AgentResult.isStopped for stopped state" := do
  let result : AgentResult := {
    messages := #[]
    finalContent := none
    iterations := 0
    state := .stopped #[]
  }
  shouldBe result.isSuccess false
  shouldBe result.hitToolLimit false
  shouldBe result.isError false
  shouldBe result.isStopped true

test "AgentResult.error? returns error" := do
  let err := OracleError.rateLimitError (some 30)
  let result : AgentResult := {
    messages := #[]
    finalContent := none
    iterations := 1
    state := .error #[] err
  }
  match result.error? with
  | some (OracleError.rateLimitError _) => pure ()
  | _ => throw (IO.userError "Expected rate limit error")

end Tests.AgentResultTests
