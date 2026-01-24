/-
  New request parameter tests
-/

import Crucible
import Oracle

namespace Tests.NewParamsTests

open Crucible
open Oracle

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
