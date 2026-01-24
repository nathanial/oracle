/-
  Response parsing tests
-/

import Crucible
import Oracle

namespace Tests.ResponseTests

open Crucible
open Oracle

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
