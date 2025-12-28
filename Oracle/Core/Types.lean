/-
  Oracle - Core Types
  Message roles and content types for chat completions
-/

import Lean.Data.Json

namespace Oracle

open Lean Json

/-- Chat message role -/
inductive Role where
  | system
  | user
  | assistant
  | tool
  | developer
  deriving Repr, BEq, Inhabited, DecidableEq

namespace Role

def toString : Role → String
  | .system => "system"
  | .user => "user"
  | .assistant => "assistant"
  | .tool => "tool"
  | .developer => "developer"

def fromString? : String → Option Role
  | "system" => some .system
  | "user" => some .user
  | "assistant" => some .assistant
  | "tool" => some .tool
  | "developer" => some .developer
  | _ => none

instance : ToString Role where
  toString := Role.toString

end Role

/-- Image source: URL or base64 encoded data -/
inductive ImageSource where
  | url (url : String)
  | base64 (mediaType : String) (data : String)
  deriving Repr, Inhabited, BEq

namespace ImageSource

/-- Convert to data URL format for API -/
def toDataUrl : ImageSource → String
  | .url u => u
  | .base64 mediaType data => s!"data:{mediaType};base64,{data}"

end ImageSource

/-- A content part in a multimodal message -/
inductive ContentPart where
  | text (content : String)
  | image (source : ImageSource) (detail : String := "auto")
  deriving Repr, Inhabited, BEq

/-- Message content: either simple string or array of parts -/
inductive MessageContent where
  | string (content : String)
  | parts (parts : Array ContentPart)
  deriving Repr, Inhabited, BEq

namespace MessageContent

/-- Get plain text content (for backward compatibility) -/
def asString : MessageContent → String
  | .string s => s
  | .parts ps => String.join (ps.toList.filterMap fun
    | .text s => some s
    | .image _ _ => none)

end MessageContent

/-- Function call made by the model -/
structure FunctionCall where
  name : String
  arguments : String  -- JSON string of arguments
  deriving Repr, Inhabited, BEq

/-- Tool call made by the model -/
structure ToolCall where
  id : String
  type : String := "function"
  function : FunctionCall
  deriving Repr, Inhabited, BEq

/-- Chat message -/
structure Message where
  role : Role
  content : MessageContent
  name : Option String := none
  toolCallId : Option String := none  -- For tool responses
  toolCalls : Option (Array ToolCall) := none  -- For assistant messages with tool calls
  deriving Repr, Inhabited

namespace Message

/-- Create a system message -/
def system (content : String) : Message :=
  { role := .system, content := .string content }

/-- Create a user message -/
def user (content : String) : Message :=
  { role := .user, content := .string content }

/-- Create an assistant message -/
def assistant (content : String) : Message :=
  { role := .assistant, content := .string content }

/-- Create a tool response message -/
def toolResponse (toolCallId : String) (content : String) : Message :=
  { role := .tool, content := .string content, toolCallId := some toolCallId }

/-- Create a developer message -/
def developer (content : String) : Message :=
  { role := .developer, content := .string content }

/-- Create a user message with text and image URLs -/
def userWithImageUrls (text : String) (urls : Array String) (detail : String := "auto") : Message := {
  role := .user
  content := .parts (#[.text text] ++ urls.map fun url => .image (.url url) detail)
}

/-- Create a user message with a single image URL -/
def userWithImageUrl (text : String) (url : String) (detail : String := "auto") : Message :=
  userWithImageUrls text #[url] detail

/-- Create a user message with base64 image data -/
def userWithBase64Image (text : String) (mediaType : String) (data : String) (detail : String := "auto") : Message := {
  role := .user
  content := .parts #[.text text, .image (.base64 mediaType data) detail]
}

/-- Create a user message with multiple images (URLs or base64) -/
def userWithImages (text : String) (images : Array ImageSource) (detail : String := "auto") : Message := {
  role := .user
  content := .parts (#[.text text] ++ images.map fun src => .image src detail)
}

end Message

end Oracle
