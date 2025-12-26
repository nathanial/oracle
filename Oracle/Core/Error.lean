/-
  Oracle - Error Types
  Error handling for OpenRouter API interactions
-/

namespace Oracle

/-- Errors that can occur when using the Oracle client -/
inductive OracleError where
  /-- HTTP error with status code -/
  | httpError (status : UInt32) (message : String)
  /-- JSON parsing error -/
  | parseError (message : String)
  /-- Network/connection error -/
  | networkError (message : String)
  /-- Authentication error (invalid API key) -/
  | authError (message : String)
  /-- Rate limit exceeded -/
  | rateLimitError (retryAfter : Option Nat)
  /-- API error returned by OpenRouter -/
  | apiError (code : String) (message : String)
  /-- Timeout error -/
  | timeoutError (message : String)
  deriving Repr, Inhabited

namespace OracleError

instance : ToString OracleError where
  toString
    | .httpError status msg => s!"HTTP {status}: {msg}"
    | .parseError msg => s!"Parse error: {msg}"
    | .networkError msg => s!"Network error: {msg}"
    | .authError msg => s!"Authentication error: {msg}"
    | .rateLimitError (some retry) => s!"Rate limit exceeded, retry after {retry}s"
    | .rateLimitError none => "Rate limit exceeded"
    | .apiError code msg => s!"API error ({code}): {msg}"
    | .timeoutError msg => s!"Timeout: {msg}"

/-- Check if error is retryable -/
def isRetryable : OracleError → Bool
  | .rateLimitError _ => true
  | .networkError _ => true
  | .timeoutError _ => true
  | .httpError status _ => status >= 500
  | _ => false

end OracleError

/-- Result type for Oracle operations -/
abbrev OracleResult (α : Type) := Except OracleError α

end Oracle
