/-
  Oracle - Chat Request Types
  Request types for chat completions
-/

import Lean.Data.Json
import Oracle.Core.Types
import Oracle.Core.Tool
import Oracle.Json

namespace Oracle

open Lean Json

-- ============================================================================
-- Image Generation Types
-- ============================================================================

/-- Modality types supported by OpenRouter -/
inductive Modality where
  | text
  | image
  deriving Repr, BEq, Inhabited

/-- Image generation configuration -/
structure ImageConfig where
  /-- Aspect ratio (e.g., "16:9", "1:1", "9:16") -/
  aspectRatio : Option String := none
  deriving Inhabited

-- ============================================================================
-- Response Format
-- ============================================================================

/-- Response format specification -/
inductive ResponseFormat where
  | text
  | jsonObject
  | jsonSchema (schema : Json) (name : String := "response") (strict : Bool := true)
  deriving Inhabited

/-- Chat completion request -/
structure ChatRequest where
  /-- Model to use -/
  model : String
  /-- Messages in the conversation -/
  messages : Array Message
  /-- Sampling temperature (0-2) -/
  temperature : Option Float := none
  /-- Maximum tokens to generate -/
  maxTokens : Option Nat := none
  /-- Nucleus sampling parameter -/
  topP : Option Float := none
  /-- Whether to stream the response -/
  stream : Bool := false
  /-- Stop sequences -/
  stop : Option (Array String) := none
  /-- Available tools -/
  tools : Option (Array Tool) := none
  /-- Tool choice mode -/
  toolChoice : Option ToolChoice := none
  /-- Presence penalty (-2 to 2) -/
  presencePenalty : Option Float := none
  /-- Frequency penalty (-2 to 2) -/
  frequencyPenalty : Option Float := none
  /-- Response format -/
  responseFormat : Option ResponseFormat := none
  /-- Random seed for reproducibility -/
  seed : Option Nat := none
  /-- Top-K sampling: only consider the top K tokens -/
  topK : Option Nat := none
  /-- Repetition penalty (1.0 = no penalty, >1.0 = discourage repetition) -/
  repetitionPenalty : Option Float := none
  /-- Minimum probability threshold for token selection -/
  minP : Option Float := none
  /-- Top-A sampling: filters tokens by probability -/
  topA : Option Float := none
  /-- Token ID bias mapping (token_id -> bias value) -/
  logitBias : Option (List (Nat × Float)) := none
  /-- Whether to return log probabilities -/
  logprobs : Option Bool := none
  /-- Number of top log probabilities to return (0-20) -/
  topLogprobs : Option Nat := none
  /-- Whether to allow parallel tool calls -/
  parallelToolCalls : Option Bool := none
  /-- Modalities for the request (e.g., ["text", "image"] for image generation) -/
  modalities : Option (Array Modality) := none
  /-- Image generation configuration -/
  imageConfig : Option ImageConfig := none
  deriving Inhabited

namespace ChatRequest

/-- Create a chat request from model and messages -/
def create (model : String) (messages : Array Message) : ChatRequest :=
  { model, messages }

/-- Create a chat request with a single user message -/
def simple (model : String) (prompt : String) : ChatRequest :=
  { model, messages := #[Message.user prompt] }

/-- Set temperature -/
def withTemperature (r : ChatRequest) (temp : Float) : ChatRequest :=
  { r with temperature := some temp }

/-- Set max tokens -/
def withMaxTokens (r : ChatRequest) (tokens : Nat) : ChatRequest :=
  { r with maxTokens := some tokens }

/-- Enable streaming -/
def withStreaming (r : ChatRequest) : ChatRequest :=
  { r with stream := true }

/-- Set tools -/
def withTools (r : ChatRequest) (tools : Array Tool) : ChatRequest :=
  { r with tools := some tools }

/-- Set tool choice -/
def withToolChoice (r : ChatRequest) (choice : ToolChoice) : ChatRequest :=
  { r with toolChoice := some choice }

/-- Add a system message at the beginning -/
def withSystem (r : ChatRequest) (system : String) : ChatRequest :=
  { r with messages := #[Message.system system] ++ r.messages }

/-- Add a message to the conversation -/
def addMessage (r : ChatRequest) (msg : Message) : ChatRequest :=
  { r with messages := r.messages.push msg }

/-- Set top-K sampling -/
def withTopK (r : ChatRequest) (k : Nat) : ChatRequest :=
  { r with topK := some k }

/-- Set repetition penalty -/
def withRepetitionPenalty (r : ChatRequest) (penalty : Float) : ChatRequest :=
  { r with repetitionPenalty := some penalty }

/-- Set minimum probability threshold -/
def withMinP (r : ChatRequest) (p : Float) : ChatRequest :=
  { r with minP := some p }

/-- Set top-A sampling -/
def withTopA (r : ChatRequest) (a : Float) : ChatRequest :=
  { r with topA := some a }

/-- Set logit bias for specific tokens -/
def withLogitBias (r : ChatRequest) (bias : List (Nat × Float)) : ChatRequest :=
  { r with logitBias := some bias }

/-- Enable log probability output -/
def withLogprobs (r : ChatRequest) (topN : Nat := 0) : ChatRequest :=
  { r with logprobs := some true, topLogprobs := if topN > 0 then some topN else none }

/-- Enable parallel tool calls -/
def withParallelToolCalls (r : ChatRequest) : ChatRequest :=
  { r with parallelToolCalls := some true }

/-- Set top-P (nucleus sampling) -/
def withTopP (r : ChatRequest) (p : Float) : ChatRequest :=
  { r with topP := some p }

/-- Set presence penalty -/
def withPresencePenalty (r : ChatRequest) (penalty : Float) : ChatRequest :=
  { r with presencePenalty := some penalty }

/-- Set frequency penalty -/
def withFrequencyPenalty (r : ChatRequest) (penalty : Float) : ChatRequest :=
  { r with frequencyPenalty := some penalty }

/-- Set seed for reproducibility -/
def withSeed (r : ChatRequest) (seed : Nat) : ChatRequest :=
  { r with seed := some seed }

/-- Enable image generation modality -/
def withImageGeneration (r : ChatRequest) (aspectRatio : Option String := none) : ChatRequest :=
  { r with
    modalities := some #[.text, .image]
    imageConfig := if aspectRatio.isSome then some { aspectRatio } else none }

end ChatRequest

-- JSON serialization instances

namespace Role

instance : ToJson Role where
  toJson r := Json.str r.toString

instance : FromJson Role where
  fromJson? json := do
    let s ← json.getStr?
    match Role.fromString? s with
    | some r => return r
    | none => throw s!"Invalid role: {s}"

end Role

namespace ContentPart

instance : ToJson ContentPart where
  toJson
    | .text content => Json.mkObj [("type", Json.str "text"), ("text", Json.str content)]
    | .image source detail => Json.mkObj [
        ("type", Json.str "image_url"),
        ("image_url", Json.mkObj [("url", Json.str source.toDataUrl), ("detail", Json.str detail)])
      ]

instance : FromJson ContentPart where
  fromJson? json := do
    let partType ← json.getObjValAs? String "type"
    match partType with
    | "text" =>
      let text ← json.getObjValAs? String "text"
      return .text text
    | "image_url" =>
      let imgObj ← json.getObjVal? "image_url"
      let urlStr ← imgObj.getObjValAs? String "url"
      let detail := imgObj.getObjValAs? String "detail" |>.toOption |>.getD "auto"
      -- Parse data URL or regular URL
      let source := if urlStr.startsWith "data:" then
        -- Parse "data:image/png;base64,..."
        match urlStr.splitOn ";base64," with
        | [header, data] =>
          let mediaType := header.drop 5  -- Remove "data:"
          ImageSource.base64 mediaType data
        | _ => ImageSource.url urlStr
      else
        ImageSource.url urlStr
      return .image source detail
    | _ => throw s!"Unknown content part type: {partType}"

end ContentPart

namespace MessageContent

instance : ToJson MessageContent where
  toJson
    | .string s => Json.str s
    | .parts ps => toJson ps

instance : FromJson MessageContent where
  fromJson? json :=
    match json.getStr? with
    | .ok s => return .string s
    | .error _ =>
      match json.getArr? with
      | .ok jsonArr => do
        let parts ← jsonArr.mapM fromJson?
        return .parts parts
      | .error _ => throw "Content must be string or array of content parts"

end MessageContent

namespace FunctionCall

instance : ToJson FunctionCall where
  toJson fc := Json.mkObj [
    ("name", Json.str fc.name),
    ("arguments", Json.str fc.arguments)
  ]

instance : FromJson FunctionCall where
  fromJson? json := do
    let name ← json.getObjValAs? String "name"
    let arguments ← json.getObjValAs? String "arguments"
    return { name, arguments }

end FunctionCall

namespace ToolCall

instance : ToJson ToolCall where
  toJson tc := Json.mkObj [
    ("id", Json.str tc.id),
    ("type", Json.str tc.type),
    ("function", toJson tc.function)
  ]

instance : FromJson ToolCall where
  fromJson? json := do
    let id ← json.getObjValAs? String "id"
    let tcType := json.getObjValAs? String "type" |>.toOption |>.getD "function"
    let function ← json.getObjVal? "function" >>= fromJson?
    return { id, type := tcType, function }

end ToolCall

namespace Message

instance : ToJson Message where
  toJson msg := Json.withOptionalFields [
    ("role", some (toJson msg.role)),
    ("content", some (toJson msg.content)),
    ("name", msg.name.map Json.str),
    ("tool_call_id", msg.toolCallId.map Json.str),
    ("tool_calls", msg.toolCalls.map toJson)
  ]

instance : FromJson Message where
  fromJson? json := do
    let roleStr ← json.getObjValAs? String "role"
    let role := Role.fromString? roleStr |>.getD .assistant
    -- Content can be null for tool calls, string, or array of content parts
    let content : MessageContent ← match json.getObjVal? "content" with
      | .ok contentJson =>
        match (fromJson? contentJson : Except String MessageContent) with
        | .ok mc => pure mc
        | .error _ => pure (MessageContent.string "")
      | .error _ => pure (MessageContent.string "")
    let name := json.getObjValAs? String "name" |>.toOption
    let toolCallId := json.getObjValAs? String "tool_call_id" |>.toOption
    let toolCalls : Option (Array ToolCall) := do
      let tcJson ← json.getObjVal? "tool_calls" |>.toOption
      let jsonArr ← tcJson.getArr?.toOption
      jsonArr.mapM fromJson? |>.toOption
    return { role, content, name, toolCallId, toolCalls }

end Message

namespace ToolFunction

instance : ToJson ToolFunction where
  toJson tf := Json.withOptionalFields [
    ("name", some (Json.str tf.name)),
    ("description", tf.description.map Json.str),
    ("parameters", tf.parameters),
    ("strict", tf.strict.map Json.bool)
  ]

end ToolFunction

namespace Tool

instance : ToJson Tool where
  toJson t := Json.mkObj [
    ("type", Json.str t.type),
    ("function", toJson t.function)
  ]

end Tool

namespace ToolChoice

instance : ToJson ToolChoice where
  toJson
    | .none => Json.str "none"
    | .auto => Json.str "auto"
    | .required => Json.str "required"
    | .function name => Json.mkObj [
        ("type", Json.str "function"),
        ("function", Json.mkObj [("name", Json.str name)])
      ]

end ToolChoice

namespace ResponseFormat

instance : ToJson ResponseFormat where
  toJson
    | .text => Json.mkObj [("type", Json.str "text")]
    | .jsonObject => Json.mkObj [("type", Json.str "json_object")]
    | .jsonSchema schema name strict => Json.mkObj [
        ("type", Json.str "json_schema"),
        ("json_schema", Json.mkObj [
          ("name", Json.str name),
          ("schema", schema),
          ("strict", Json.bool strict)
        ])
      ]

end ResponseFormat

namespace Modality

instance : ToJson Modality where
  toJson
    | .text => Json.str "text"
    | .image => Json.str "image"

instance : FromJson Modality where
  fromJson? json := do
    let s ← json.getStr?
    match s with
    | "text" => return .text
    | "image" => return .image
    | _ => throw s!"Invalid modality: {s}"

end Modality

namespace ImageConfig

instance : ToJson ImageConfig where
  toJson cfg := Json.withOptionalFields [
    ("aspect_ratio", cfg.aspectRatio.map Json.str)
  ]

instance : FromJson ImageConfig where
  fromJson? json := do
    let aspectRatio := json.getObjValAs? String "aspect_ratio" |>.toOption
    return { aspectRatio }

end ImageConfig

namespace ChatRequest

/-- Convert logit bias list to JSON object -/
private def logitBiasToJson (bias : List (Nat × Float)) : Json :=
  Json.mkObj (bias.map fun (tokenId, value) => (toString tokenId, toJson value))

instance : ToJson ChatRequest where
  toJson req := Json.withOptionalFields [
    ("model", some (Json.str req.model)),
    ("messages", some (toJson req.messages)),
    ("stream", some (Json.bool req.stream)),
    ("temperature", req.temperature.map toJson),
    ("max_tokens", req.maxTokens.map toJson),
    ("top_p", req.topP.map toJson),
    ("stop", req.stop.map toJson),
    ("tools", req.tools.map toJson),
    ("tool_choice", req.toolChoice.map toJson),
    ("presence_penalty", req.presencePenalty.map toJson),
    ("frequency_penalty", req.frequencyPenalty.map toJson),
    ("response_format", req.responseFormat.map toJson),
    ("seed", req.seed.map toJson),
    ("top_k", req.topK.map toJson),
    ("repetition_penalty", req.repetitionPenalty.map toJson),
    ("min_p", req.minP.map toJson),
    ("top_a", req.topA.map toJson),
    ("logit_bias", req.logitBias.map logitBiasToJson),
    ("logprobs", req.logprobs.map Json.bool),
    ("top_logprobs", req.topLogprobs.map toJson),
    ("parallel_tool_calls", req.parallelToolCalls.map Json.bool),
    ("modalities", req.modalities.map toJson),
    ("image_config", req.imageConfig.map toJson)
  ]

/-- Convert request to JSON string -/
def toJsonString (req : ChatRequest) : String :=
  (toJson req).compress

end ChatRequest

end Oracle
