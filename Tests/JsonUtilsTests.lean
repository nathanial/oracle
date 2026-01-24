/-
  JSON utilities tests
-/

import Crucible
import Oracle

namespace Tests.JsonUtilsTests

open Crucible
open Oracle

testSuite "JSON Utilities"

test "withOptionalFields filters none values" := do
  let json := Oracle.Json.withOptionalFields [
    ("required", some (Lean.Json.str "value")),
    ("optional", none),
    ("another", some (Lean.Json.num 42))
  ]
  -- Should have 2 fields, not 3
  match json.getObj? with
  | .ok obj =>
    shouldBe obj.size 2
    match json.getObjValAs? String "required" with
    | .ok v => shouldBe v "value"
    | .error _ => throw (IO.userError "Missing required field")
    match json.getObjValAs? Nat "another" with
    | .ok v => shouldBe v 42
    | .error _ => throw (IO.userError "Missing another field")
  | .error _ => throw (IO.userError "Expected object")

test "withOptionalFields preserves all some values" := do
  let json := Oracle.Json.withOptionalFields [
    ("a", some (Lean.Json.str "1")),
    ("b", some (Lean.Json.str "2")),
    ("c", some (Lean.Json.str "3"))
  ]
  match json.getObj? with
  | .ok obj => shouldBe obj.size 3
  | .error _ => throw (IO.userError "Expected object")

test "withOptionalFields handles empty list" := do
  let json := Oracle.Json.withOptionalFields []
  match json.getObj? with
  | .ok obj => shouldBe obj.size 0
  | .error _ => throw (IO.userError "Expected object")

end Tests.JsonUtilsTests
