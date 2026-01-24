/-
  Error tests
-/

import Crucible
import Oracle
import Tests.Support

namespace Tests.ErrorTests

open Crucible
open Oracle

testSuite "Error Handling"

test "OracleError.isRetryable for rate limit" := do
  let err := OracleError.rateLimitError (some 30)
  shouldBe err.isRetryable true

test "OracleError.isRetryable for network error" := do
  let err := OracleError.networkError "Connection failed"
  shouldBe err.isRetryable true

test "OracleError.isRetryable for auth error" := do
  let err := OracleError.authError "Invalid key"
  shouldBe err.isRetryable false

test "OracleError toString" := do
  let err := OracleError.httpError 404 "Not found"
  let s := toString err
  shouldSatisfy (s.containsSubstr "404") "error message contains status code"

end Tests.ErrorTests
