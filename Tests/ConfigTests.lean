/-
  Config tests
-/

import Crucible
import Oracle

namespace Tests.ConfigTests

open Crucible
open Oracle

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
