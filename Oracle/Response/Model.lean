/-
  Oracle - Model Types
  Response types for the models list endpoint
-/

import Lean.Data.Json

namespace Oracle

open Lean Json

/-- Pricing information for a model -/
structure ModelPricing where
  /-- Cost per prompt token (in USD) -/
  prompt : Float
  /-- Cost per completion token (in USD) -/
  completion : Float
  /-- Cost per image (for vision models) -/
  image : Option Float := none
  /-- Cost per request (fixed fee) -/
  request : Option Float := none
  deriving Repr, Inhabited

/-- Model metadata from the OpenRouter API -/
structure Model where
  /-- Model identifier (e.g., "anthropic/claude-sonnet-4") -/
  id : String
  /-- Human-readable name -/
  name : String
  /-- Model description -/
  description : Option String := none
  /-- Context window size in tokens -/
  contextLength : Nat
  /-- Pricing information -/
  pricing : ModelPricing
  /-- Maximum completion tokens (if limited) -/
  maxCompletionTokens : Option Nat := none
  /-- Whether the model supports tool/function calling -/
  supportsTools : Bool := false
  /-- Whether the model supports vision/images -/
  supportsVision : Bool := false
  /-- Whether the model supports streaming -/
  supportsStreaming : Bool := true
  /-- Top provider for this model -/
  topProvider : Option String := none
  /-- Model architecture/family -/
  architecture : Option String := none
  deriving Repr, Inhabited

namespace ModelPricing

/-- Parse a string to Float (handles "0.123" format) -/
private def parseFloatString (s : String) : Option Float :=
  match s.splitOn "." with
  | [intPart] => intPart.toNat?.map Float.ofNat
  | [intPart, fracPart] =>
    let intVal := intPart.toNat?.getD 0
    let fracVal := fracPart.toNat?.getD 0
    let fracDigits := fracPart.length
    if fracDigits == 0 then some (Float.ofNat intVal)
    else
      let divisor := Float.pow 10.0 (Float.ofNat fracDigits)
      some (Float.ofNat intVal + Float.ofNat fracVal / divisor)
  | _ => none

instance : FromJson ModelPricing where
  fromJson? json := do
    -- Pricing can be string or number, handle both
    let parsePrice (field : String) : Except String Float := do
      match json.getObjVal? field with
      | .ok val =>
        match val with
        | .num n => return n.toFloat
        | .str s => match parseFloatString s with
          | some f => return f
          | none => return 0.0
        | _ => return 0.0
      | .error _ => return 0.0
    let prompt ← parsePrice "prompt"
    let completion ← parsePrice "completion"
    let image := (parsePrice "image").toOption
    let request := (parsePrice "request").toOption
    return { prompt, completion, image, request }

instance : ToJson ModelPricing where
  toJson p := Json.mkObj [
    ("prompt", toJson p.prompt),
    ("completion", toJson p.completion)
  ]

end ModelPricing

namespace Model

instance : FromJson Model where
  fromJson? json := do
    let id ← json.getObjValAs? String "id"
    let name := json.getObjValAs? String "name" |>.toOption |>.getD id
    let description := json.getObjValAs? String "description" |>.toOption
    let contextLength := json.getObjValAs? Nat "context_length" |>.toOption |>.getD 4096

    -- Parse pricing from nested object
    let pricing : ModelPricing ← match json.getObjVal? "pricing" with
      | .ok pricingJson => fromJson? pricingJson
      | .error _ => pure { prompt := 0.0, completion := 0.0 }

    let maxCompletionTokens : Option Nat := do
      let tp ← json.getObjVal? "top_provider" |>.toOption
      tp.getObjValAs? Nat "max_completion_tokens" |>.toOption

    -- Parse capabilities
    let supportsTools := json.getObjValAs? Bool "supports_tool_use" |>.toOption |>.getD false
    let supportsVision := json.getObjValAs? Bool "supports_vision" |>.toOption |>.getD false
    let supportsStreaming := json.getObjValAs? Bool "supports_streaming" |>.toOption |>.getD true

    let topProvider : Option String := do
      let tp ← json.getObjVal? "top_provider" |>.toOption
      tp.getObjValAs? String "name" |>.toOption
    let architecture : Option String := do
      let arch ← json.getObjVal? "architecture" |>.toOption
      arch.getObjValAs? String "modality" |>.toOption

    return {
      id, name, description, contextLength, pricing,
      maxCompletionTokens, supportsTools, supportsVision, supportsStreaming,
      topProvider, architecture
    }

instance : ToJson Model where
  toJson m := Json.mkObj [
    ("id", Json.str m.id),
    ("name", Json.str m.name),
    ("context_length", toJson m.contextLength),
    ("pricing", toJson m.pricing)
  ]

/-- Check if a model supports a minimum context length -/
def hasMinContext (m : Model) (minTokens : Nat) : Bool :=
  m.contextLength >= minTokens

/-- Get the cost for a given number of tokens -/
def estimateCost (m : Model) (promptTokens : Nat) (completionTokens : Nat) : Float :=
  m.pricing.prompt * promptTokens.toFloat + m.pricing.completion * completionTokens.toFloat

end Model

/-- Response from the models list endpoint -/
structure ModelsResponse where
  data : Array Model
  deriving Repr, Inhabited

namespace ModelsResponse

instance : FromJson ModelsResponse where
  fromJson? json := do
    let dataJson ← json.getObjVal? "data"
    let arr ← dataJson.getArr?
    let models ← arr.mapM fromJson?
    return { data := models }

/-- Filter models by capability -/
def withTools (r : ModelsResponse) : Array Model :=
  r.data.filter (·.supportsTools)

/-- Filter models by vision support -/
def withVision (r : ModelsResponse) : Array Model :=
  r.data.filter (·.supportsVision)

/-- Filter models by minimum context length -/
def withMinContext (r : ModelsResponse) (minTokens : Nat) : Array Model :=
  r.data.filter (·.hasMinContext minTokens)

/-- Find a model by ID -/
def findById (r : ModelsResponse) (id : String) : Option Model :=
  r.data.find? (·.id == id)

end ModelsResponse

end Oracle
