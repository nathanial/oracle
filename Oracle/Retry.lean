/-
  Oracle - Retry Logic
  Automatic retry with exponential backoff for transient errors
-/

import Oracle.Core.Error

namespace Oracle

/-- Configuration for retry behavior -/
structure RetryConfig where
  /-- Maximum number of retry attempts (0 = no retries) -/
  maxRetries : Nat := 3
  /-- Initial delay between retries in milliseconds -/
  initialDelayMs : Nat := 1000
  /-- Maximum delay between retries in milliseconds -/
  maxDelayMs : Nat := 30000
  /-- Multiplier for exponential backoff (e.g., 2.0 = double each time) -/
  backoffMultiplier : Float := 2.0
  /-- Whether to add jitter to delays to avoid thundering herd -/
  useJitter : Bool := true
  deriving Repr, Inhabited

namespace RetryConfig

/-- No retries -/
def none : RetryConfig := { maxRetries := 0 }

/-- Default retry config (3 retries, exponential backoff) -/
def default : RetryConfig := {}

/-- Aggressive retry config for critical operations -/
def aggressive : RetryConfig := {
  maxRetries := 5
  initialDelayMs := 500
  maxDelayMs := 60000
}

/-- Calculate delay for a given attempt number (0-indexed) -/
def delayForAttempt (cfg : RetryConfig) (attempt : Nat) : Nat :=
  let baseDelay := cfg.initialDelayMs.toFloat * (cfg.backoffMultiplier ^ attempt.toFloat)
  let cappedDelay := min baseDelay cfg.maxDelayMs.toFloat
  cappedDelay.toUInt64.toNat

end RetryConfig

/-- Result of a retry operation -/
structure RetryResult (α : Type) where
  /-- The final result (success or last error) -/
  result : OracleResult α
  /-- Number of attempts made -/
  attempts : Nat
  /-- Total time spent on retries in milliseconds -/
  totalDelayMs : Nat
  deriving Repr

namespace Retry

/-- Sleep for the specified number of milliseconds -/
private def sleepMs (ms : Nat) : IO Unit := do
  IO.sleep ms.toUInt32

/-- Add jitter to a delay (±25%) -/
private def addJitter (delayMs : Nat) : IO Nat := do
  -- Simple jitter: random value between 75% and 125% of delay
  let jitterRange := delayMs / 4
  if jitterRange == 0 then return delayMs
  let randomOffset ← IO.rand 0 (jitterRange * 2)
  return delayMs - jitterRange + randomOffset

/-- Execute an IO action with automatic retry on retryable errors -/
def withRetry (cfg : RetryConfig) (action : IO (OracleResult α)) : IO (RetryResult α) := do
  let mut attempts := 0
  let mut totalDelay := 0
  let mut lastResult : OracleResult α := .error (.networkError "No attempts made")

  for _ in [:cfg.maxRetries + 1] do
    attempts := attempts + 1
    lastResult ← action

    match lastResult with
    | .ok _ =>
      -- Success, return immediately
      return { result := lastResult, attempts, totalDelayMs := totalDelay }
    | .error e =>
      if !e.isRetryable || attempts > cfg.maxRetries then
        -- Not retryable or out of retries
        return { result := lastResult, attempts, totalDelayMs := totalDelay }
      else
        -- Calculate delay and wait
        let baseDelay := cfg.delayForAttempt (attempts - 1)

        -- Use Retry-After header if available for rate limits
        let delay ← match e with
          | .rateLimitError (some retryAfter) =>
            pure (retryAfter * 1000)  -- Convert seconds to ms
          | _ =>
            if cfg.useJitter then addJitter baseDelay else pure baseDelay

        sleepMs delay
        totalDelay := totalDelay + delay

  return { result := lastResult, attempts, totalDelayMs := totalDelay }

/-- Execute with default retry config -/
def withDefaultRetry (action : IO (OracleResult α)) : IO (OracleResult α) := do
  let result ← withRetry RetryConfig.default action
  return result.result

/-- Execute with no retries (just run once) -/
def noRetry (action : IO (OracleResult α)) : IO (OracleResult α) := do
  let result ← withRetry RetryConfig.none action
  return result.result

end Retry

end Oracle
