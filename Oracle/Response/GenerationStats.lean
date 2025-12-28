/-
  Oracle - Generation Statistics
  Response types for querying generation statistics
-/

import Lean.Data.Json

namespace Oracle

open Lean Json

/-- Statistics for a single generation (API response) -/
structure GenerationStats where
  /-- Generation ID -/
  id : String
  /-- Total cost in USD -/
  totalCost : Float
  /-- When the generation was created -/
  createdAt : String
  /-- Model used for the generation -/
  model : String
  /-- Application ID (if applicable) -/
  appId : Option Nat := none
  /-- Number of prompt tokens -/
  promptTokens : Nat
  /-- Number of completion tokens -/
  completionTokens : Nat
  /-- Total tokens (prompt + completion) -/
  totalTokens : Nat
  /-- Native prompt tokens (model's tokenizer) -/
  nativeTokensPrompt : Option Nat := none
  /-- Native completion tokens (model's tokenizer) -/
  nativeTokensCompletion : Option Nat := none
  deriving Repr, Inhabited

namespace GenerationStats

instance : FromJson GenerationStats where
  fromJson? json := do
    let id ← json.getObjValAs? String "id"
    let totalCost ← json.getObjValAs? Float "total_cost"
    let createdAt ← json.getObjValAs? String "created_at"
    let model ← json.getObjValAs? String "model"
    let appId := json.getObjValAs? Nat "app_id" |>.toOption
    let promptTokens ← json.getObjValAs? Nat "tokens_prompt"
    let completionTokens ← json.getObjValAs? Nat "tokens_completion"
    let totalTokens := promptTokens + completionTokens
    let nativeTokensPrompt := json.getObjValAs? Nat "native_tokens_prompt" |>.toOption
    let nativeTokensCompletion := json.getObjValAs? Nat "native_tokens_completion" |>.toOption
    return {
      id, totalCost, createdAt, model, appId,
      promptTokens, completionTokens, totalTokens,
      nativeTokensPrompt, nativeTokensCompletion
    }

instance : ToJson GenerationStats where
  toJson gs := Json.mkObj [
    ("id", Json.str gs.id),
    ("total_cost", toJson gs.totalCost),
    ("created_at", Json.str gs.createdAt),
    ("model", Json.str gs.model),
    ("tokens_prompt", toJson gs.promptTokens),
    ("tokens_completion", toJson gs.completionTokens),
    ("total_tokens", toJson gs.totalTokens)
  ]

end GenerationStats

end Oracle
