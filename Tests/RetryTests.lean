/-
  Retry tests
-/

import Crucible
import Oracle

namespace Tests.RetryTests

open Crucible
open Oracle

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
