/-
  Oracle - Token Usage
  Token usage information from completions
-/

import Lean.Data.Json

namespace Oracle

open Lean Json

/-- Token usage information -/
structure Usage where
  promptTokens : Nat
  completionTokens : Nat
  totalTokens : Nat
  deriving Repr, Inhabited, BEq

namespace Usage

instance : FromJson Usage where
  fromJson? json := do
    let promptTokens ← json.getObjValAs? Nat "prompt_tokens"
    let completionTokens ← json.getObjValAs? Nat "completion_tokens"
    let totalTokens ← json.getObjValAs? Nat "total_tokens"
    return { promptTokens, completionTokens, totalTokens }

instance : ToJson Usage where
  toJson u := Json.mkObj [
    ("prompt_tokens", toJson u.promptTokens),
    ("completion_tokens", toJson u.completionTokens),
    ("total_tokens", toJson u.totalTokens)
  ]

end Usage

end Oracle
