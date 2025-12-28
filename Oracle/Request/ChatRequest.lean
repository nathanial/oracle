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
    ("content", some (Json.str msg.content)),
    ("name", msg.name.map Json.str),
    ("tool_call_id", msg.toolCallId.map Json.str),
    ("tool_calls", msg.toolCalls.map toJson)
  ]

instance : FromJson Message where
  fromJson? json := do
    let roleStr ← json.getObjValAs? String "role"
    let role := Role.fromString? roleStr |>.getD .assistant
    -- Content can be null for tool calls, default to empty string
    let content := json.getObjValAs? String "content" |>.toOption |>.getD ""
    let name := json.getObjValAs? String "name" |>.toOption
    let toolCallId := json.getObjValAs? String "tool_call_id" |>.toOption
    let toolCalls : Option (Array ToolCall) := do
      let tcJson ← json.getObjVal? "tool_calls" |>.toOption
      let arr ← tcJson.getArr?.toOption
      arr.mapM fromJson? |>.toOption
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

namespace ChatRequest

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
    ("seed", req.seed.map toJson)
  ]

/-- Convert request to JSON string -/
def toJsonString (req : ChatRequest) : String :=
  (toJson req).compress

end ChatRequest

end Oracle
