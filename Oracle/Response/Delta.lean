/-
  Oracle - Streaming Delta Types
  Types for streaming response chunks
-/

import Lean.Data.Json
import Oracle.Core.Types

namespace Oracle

open Lean Json

/-- Function call delta in a streaming chunk -/
structure FunctionCallDelta where
  name : Option String := none
  arguments : Option String := none
  deriving Repr, Inhabited

/-- Tool call delta in a streaming chunk -/
structure ToolCallDelta where
  index : Nat
  id : Option String := none
  type : Option String := none
  function : Option FunctionCallDelta := none
  deriving Repr, Inhabited

/-- Delta content in a streaming chunk -/
structure DeltaContent where
  role : Option Role := none
  content : Option String := none
  toolCalls : Option (Array ToolCallDelta) := none
  deriving Inhabited

/-- A single choice in a streaming chunk -/
structure DeltaChoice where
  index : Nat
  delta : DeltaContent
  finishReason : Option String := none
  deriving Inhabited

/-- A streaming response chunk -/
structure StreamChunk where
  id : String
  model : Option String := none
  created : Option Nat := none
  choices : Array DeltaChoice
  deriving Inhabited

namespace FunctionCallDelta

instance : FromJson FunctionCallDelta where
  fromJson? json := do
    let name := json.getObjValAs? String "name" |>.toOption
    let arguments := json.getObjValAs? String "arguments" |>.toOption
    return { name, arguments }

end FunctionCallDelta

namespace ToolCallDelta

instance : FromJson ToolCallDelta where
  fromJson? json := do
    let index ← json.getObjValAs? Nat "index"
    let id := json.getObjValAs? String "id" |>.toOption
    let tcType := json.getObjValAs? String "type" |>.toOption
    let function : Option FunctionCallDelta := match json.getObjVal? "function" with
      | .ok funcJson => fromJson? funcJson |>.toOption
      | .error _ => none
    return { index, id, type := tcType, function }

end ToolCallDelta

namespace DeltaContent

instance : FromJson DeltaContent where
  fromJson? json := do
    let roleStr := json.getObjValAs? String "role" |>.toOption
    let role := roleStr >>= Role.fromString?
    let content := json.getObjValAs? String "content" |>.toOption

    let toolCalls : Option (Array ToolCallDelta) := do
      let toolsJson ← json.getObjVal? "tool_calls" |>.toOption
      let arr ← toolsJson.getArr? |>.toOption
      arr.mapM (fromJson? (α := ToolCallDelta)) |>.toOption

    return { role, content, toolCalls }

/-- Check if the delta has any content -/
def isEmpty (d : DeltaContent) : Bool :=
  d.role.isNone && d.content.isNone && d.toolCalls.isNone

end DeltaContent

namespace DeltaChoice

instance : FromJson DeltaChoice where
  fromJson? json := do
    let index ← json.getObjValAs? Nat "index"
    let deltaJson ← json.getObjVal? "delta"
    let delta ← fromJson? deltaJson
    let finishReason := json.getObjValAs? String "finish_reason" |>.toOption
    return { index, delta, finishReason }

end DeltaChoice

namespace StreamChunk

instance : FromJson StreamChunk where
  fromJson? json := do
    let id ← json.getObjValAs? String "id"
    let model := json.getObjValAs? String "model" |>.toOption
    let created := json.getObjValAs? Nat "created" |>.toOption
    let choicesJson ← json.getObjVal? "choices"
    let choicesArr ← choicesJson.getArr?
    let choices ← choicesArr.mapM (fromJson? (α := DeltaChoice))
    return { id, model, created, choices }

/-- Get the content from the first choice's delta -/
def content (c : StreamChunk) : Option String :=
  if h : 0 < c.choices.size then
    c.choices[0].delta.content
  else
    none

/-- Get the finish reason from the first choice -/
def finishReason (c : StreamChunk) : Option String :=
  if h : 0 < c.choices.size then
    c.choices[0].finishReason
  else
    none

/-- Check if this is the final chunk -/
def isFinished (c : StreamChunk) : Bool :=
  c.finishReason.isSome

end StreamChunk

end Oracle
