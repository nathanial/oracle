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

/-- An image output from a model response -/
structure ImageOutput where
  /-- The image URL (typically a base64 data URL like "data:image/png;base64,...") -/
  url : String
  deriving Repr, Inhabited, BEq

namespace ImageOutput

/-- Check if this is a base64 data URL -/
def isBase64 (img : ImageOutput) : Bool :=
  img.url.startsWith "data:"

/-- Extract the media type from a data URL (e.g., "image/png") -/
def mediaType? (img : ImageOutput) : Option String :=
  if img.url.startsWith "data:" then
    let rest := img.url.drop 5  -- drop "data:"
    some (rest.takeWhile (· != ';'))
  else
    none

/-- Find the position of a substring in a string, returning the character offset -/
private def findSubstr (s : String) (sub : String) : Option Nat := Id.run do
  if sub.isEmpty then return none
  let subLen := sub.length
  let sLen := s.length
  for i in [:sLen] do
    if i + subLen > sLen then return none
    let slice := (s.drop i).take subLen
    if slice == sub then return some i
  return none

/-- Extract raw base64 data from a data URL -/
def base64Data? (img : ImageOutput) : Option String :=
  let marker := ";base64,"
  match findSubstr img.url marker with
  | some idx => some (img.url.drop (idx + marker.length))
  | none => none

/-- Convert an ImageOutput to an ImageSource -/
def toImageSource (img : ImageOutput) : ImageSource :=
  if img.isBase64 then
    match img.mediaType?, img.base64Data? with
    | some mediaType, some data => .base64 mediaType data
    | _, _ => .url img.url
  else
    .url img.url

end ImageOutput

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
  images : Option (Array ImageOutput) := none  -- For image generation responses
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

/-- Check if message contains generated images -/
def hasImages (msg : Message) : Bool :=
  match msg.images with
  | some imgs => imgs.size > 0
  | none => false

/-- Get the first image if any -/
def firstImage? (msg : Message) : Option ImageOutput :=
  msg.images >>= (·[0]?)

/-- Format a message for debugging output -/
def format (msg : Message) (indent : String := "") : String :=
  let roleLabel := msg.role.toString.toUpper
  let content := msg.content.asString
  let header := s!"{indent}[{roleLabel}]"
  let lines := if content.isEmpty then #[header] else #[s!"{header} {content}"]
  let toolCallLines := match msg.toolCalls with
    | none => #[]
    | some calls => calls.map fun call =>
        s!"{indent}  → {call.function.name}({call.function.arguments})"
  let imageLines := match msg.images with
    | none => #[]
    | some imgs => imgs.mapIdx fun i img =>
        let preview := if img.url.length > 50 then img.url.take 50 ++ "..." else img.url
        s!"{indent}  [IMAGE {i}] {preview}"
  String.intercalate "\n" (lines ++ toolCallLines ++ imageLines).toList

/-- Format an array of messages as a conversation -/
def formatConversation (msgs : Array Message) (indent : String := "") : String :=
  let separator := s!"{indent}{'─'.toString.pushn '─' 40}"
  let formatted := msgs.map (format · indent)
  String.intercalate s!"\n{separator}\n" formatted.toList

/-- Print a message to stdout -/
def print (msg : Message) : IO Unit :=
  IO.println (format msg)

/-- Print an array of messages as a conversation -/
def printConversation (msgs : Array Message) : IO Unit :=
  IO.println (formatConversation msgs)

end Message

end Oracle
