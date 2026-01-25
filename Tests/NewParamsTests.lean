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

test "ResponseFormat.jsonSchema toJson includes schema fields" := do
  let schema := Lean.Json.mkObj [
    ("type", Lean.Json.str "object")
  ]
  let format := ResponseFormat.jsonSchema schema "result" false
  let json := Lean.toJson format
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "json_schema"
  | .error _ => throw (IO.userError "Missing type")
  match json.getObjVal? "json_schema" with
  | .ok schemaObj =>
    match schemaObj.getObjValAs? String "name" with
    | .ok name => shouldBe name "result"
    | .error _ => throw (IO.userError "Missing name")
    match schemaObj.getObjValAs? Bool "strict" with
    | .ok strict => shouldBe strict false
    | .error _ => throw (IO.userError "Missing strict")
  | .error _ => throw (IO.userError "Missing json_schema")

test "ChatRequest toJson includes response_format" := do
  let schema := Lean.Json.mkObj [("type", Lean.Json.str "object")]
  let req := { (ChatRequest.simple "gpt-4" "Hi") with
    responseFormat := some (ResponseFormat.jsonSchema schema) }
  let json := Lean.toJson req
  match json.getObjVal? "response_format" with
  | .ok _ => pure ()
  | .error _ => throw (IO.userError "Missing response_format")

test "ChatRequest toJson includes modalities and image_config" := do
  let req := ChatRequest.simple "gpt-4" "Make an image"
    |>.withImageGeneration (some "16:9")
  let json := Lean.toJson req
  match json.getObjVal? "modalities" with
  | .ok arr =>
    match arr.getArr? with
    | .ok values => shouldBe values.size 2
    | .error _ => throw (IO.userError "modalities should be array")
  | .error _ => throw (IO.userError "Missing modalities")
  match json.getObjVal? "image_config" with
  | .ok cfg =>
    match cfg.getObjValAs? String "aspect_ratio" with
    | .ok ratio => shouldBe ratio "16:9"
    | .error _ => throw (IO.userError "Missing aspect_ratio")
  | .error _ => throw (IO.userError "Missing image_config")

test "ChatRequest toJson includes penalties and seed" := do
  let req := ChatRequest.simple "gpt-4" "Hi"
    |>.withPresencePenalty 0.3
    |>.withFrequencyPenalty 0.4
    |>.withSeed 42
  let json := Lean.toJson req
  match json.getObjValAs? Float "presence_penalty" with
  | .ok v => shouldBe v 0.3
  | .error _ => throw (IO.userError "Missing presence_penalty")
  match json.getObjValAs? Float "frequency_penalty" with
  | .ok v => shouldBe v 0.4
  | .error _ => throw (IO.userError "Missing frequency_penalty")
  match json.getObjValAs? Nat "seed" with
  | .ok v => shouldBe v 42
  | .error _ => throw (IO.userError "Missing seed")

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
