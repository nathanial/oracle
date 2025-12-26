/-
  Oracle - Chat Request Types
  Request types for chat completions
-/

import Lean.Data.Json
import Oracle.Core.Types
import Oracle.Core.Tool

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
  toJson msg :=
    let base := [
      ("role", toJson msg.role),
      ("content", Json.str msg.content)
    ]
    let withName := match msg.name with
      | some n => base ++ [("name", Json.str n)]
      | none => base
    let withToolCallId := match msg.toolCallId with
      | some id => withName ++ [("tool_call_id", Json.str id)]
      | none => withName
    let withToolCalls := match msg.toolCalls with
      | some calls => withToolCallId ++ [("tool_calls", toJson calls)]
      | none => withToolCallId
    Json.mkObj withToolCalls

end Message

namespace ToolFunction

instance : ToJson ToolFunction where
  toJson tf :=
    let base := [("name", Json.str tf.name)]
    let withDesc := match tf.description with
      | some d => base ++ [("description", Json.str d)]
      | none => base
    let withParams := match tf.parameters with
      | some p => withDesc ++ [("parameters", p)]
      | none => withDesc
    let withStrict := match tf.strict with
      | some s => withParams ++ [("strict", Json.bool s)]
      | none => withParams
    Json.mkObj withStrict

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
  toJson req :=
    let base := [
      ("model", Json.str req.model),
      ("messages", toJson req.messages),
      ("stream", Json.bool req.stream)
    ]

    let withTemp := match req.temperature with
      | some t => base ++ [("temperature", toJson t)]
      | none => base

    let withMaxTokens := match req.maxTokens with
      | some n => withTemp ++ [("max_tokens", toJson n)]
      | none => withTemp

    let withTopP := match req.topP with
      | some p => withMaxTokens ++ [("top_p", toJson p)]
      | none => withMaxTokens

    let withStop := match req.stop with
      | some s => withTopP ++ [("stop", toJson s)]
      | none => withTopP

    let withTools := match req.tools with
      | some t => withStop ++ [("tools", toJson t)]
      | none => withStop

    let withToolChoice := match req.toolChoice with
      | some c => withTools ++ [("tool_choice", toJson c)]
      | none => withTools

    let withPresence := match req.presencePenalty with
      | some p => withToolChoice ++ [("presence_penalty", toJson p)]
      | none => withToolChoice

    let withFrequency := match req.frequencyPenalty with
      | some f => withPresence ++ [("frequency_penalty", toJson f)]
      | none => withPresence

    let withFormat := match req.responseFormat with
      | some f => withFrequency ++ [("response_format", toJson f)]
      | none => withFrequency

    let withSeed := match req.seed with
      | some s => withFormat ++ [("seed", toJson s)]
      | none => withFormat

    Json.mkObj withSeed

/-- Convert request to JSON string -/
def toJsonString (req : ChatRequest) : String :=
  (toJson req).compress

end ChatRequest

end Oracle
