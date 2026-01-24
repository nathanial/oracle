/-
  Model tests
-/

import Crucible
import Oracle

namespace Tests.ModelTests

open Crucible
open Oracle

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
