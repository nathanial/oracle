/-
  Oracle - Chat Response Types
  Response types for chat completions
-/

import Lean.Data.Json
import Oracle.Core.Types
import Oracle.Request.ChatRequest
import Oracle.Response.Usage

namespace Oracle

open Lean Json

/-- A single choice in the response -/
structure Choice where
  index : Nat
  message : Message
  finishReason : Option String
  deriving Repr, Inhabited

/-- Chat completion response -/
structure ChatResponse where
  id : String
  model : String
  created : Option Nat := none
  choices : Array Choice
  usage : Option Usage := none
  deriving Repr, Inhabited

namespace Choice

instance : FromJson Choice where
  fromJson? json := do
    let index ← json.getObjValAs? Nat "index"
    let messageJson ← json.getObjVal? "message"

    -- Parse message
    let roleStr ← messageJson.getObjValAs? String "role"
    let role := Role.fromString? roleStr |>.getD .assistant

    -- Content can be null for tool calls, string, or array of content parts
    let content : MessageContent ← match messageJson.getObjVal? "content" with
      | .ok contentJson =>
        match (fromJson? contentJson : Except String MessageContent) with
        | .ok mc => pure mc
        | .error _ => pure (MessageContent.string "")
      | .error _ => pure (MessageContent.string "")

    -- Parse tool calls if present
    let toolCalls : Option (Array ToolCall) := do
      let toolCallsJson ← messageJson.getObjVal? "tool_calls" |>.toOption
      let arr ← toolCallsJson.getArr?.toOption
      let calls ← arr.mapM fun tc => do
        let id ← tc.getObjValAs? String "id" |>.toOption
        let tcType := tc.getObjValAs? String "type" |>.toOption |>.getD "function"
        let funcJson ← tc.getObjVal? "function" |>.toOption
        let name ← funcJson.getObjValAs? String "name" |>.toOption
        let arguments := funcJson.getObjValAs? String "arguments" |>.toOption |>.getD "{}"
        return { id := id, type := tcType, function := { name := name, arguments := arguments } : ToolCall }
      return calls

    -- Parse images if present (for image generation responses)
    let images : Option (Array ImageOutput) := do
      let imagesJson ← messageJson.getObjVal? "images" |>.toOption
      let arr ← imagesJson.getArr?.toOption
      let imgs ← arr.mapM fun imgObj => do
        -- Format: { "type": "image_url", "image_url": { "url": "..." } }
        let imageUrlObj ← imgObj.getObjVal? "image_url" |>.toOption
        let url ← imageUrlObj.getObjValAs? String "url" |>.toOption
        return { url : ImageOutput }
      return imgs

    let finishReason := json.getObjValAs? String "finish_reason" |>.toOption

    let message : Message := {
      role := role
      content := content
      toolCalls := toolCalls
      images := images
    }

    return { index, message, finishReason }

end Choice

namespace ChatResponse

instance : FromJson ChatResponse where
  fromJson? json := do
    let id ← json.getObjValAs? String "id"
    let model ← json.getObjValAs? String "model"
    let created := json.getObjValAs? Nat "created" |>.toOption

    let choicesJson ← json.getObjVal? "choices"
    let choicesArr ← choicesJson.getArr?
    let choices ← choicesArr.mapM fromJson?

    let usage : Option Usage := do
      let usageJson ← json.getObjVal? "usage" |>.toOption
      fromJson? usageJson |>.toOption

    return { id, model, created, choices, usage }

/-- Get the content of the first choice -/
def content (r : ChatResponse) : Option String :=
  if h : 0 < r.choices.size then
    let msg := r.choices[0].message
    let text := msg.content.asString
    if text.isEmpty then none else some text
  else
    none

/-- Get the first choice's message -/
def message (r : ChatResponse) : Option Message :=
  if h : 0 < r.choices.size then
    some r.choices[0].message
  else
    none

/-- Get tool calls from the first choice -/
def toolCalls (r : ChatResponse) : Option (Array ToolCall) :=
  r.message >>= (·.toolCalls)

/-- Check if the response contains tool calls -/
def hasToolCalls (r : ChatResponse) : Bool :=
  match r.toolCalls with
  | some calls => calls.size > 0
  | none => false

/-- Get the finish reason of the first choice -/
def finishReason (r : ChatResponse) : Option String :=
  if h : 0 < r.choices.size then
    r.choices[0].finishReason
  else
    none

/-- Get images from the first choice -/
def images (r : ChatResponse) : Option (Array ImageOutput) :=
  r.message >>= (·.images)

/-- Check if the response contains images -/
def hasImages (r : ChatResponse) : Bool :=
  match r.images with
  | some imgs => imgs.size > 0
  | none => false

/-- Get the first image from the response -/
def firstImage? (r : ChatResponse) : Option ImageOutput :=
  r.message >>= (·.firstImage?)

end ChatResponse

end Oracle
