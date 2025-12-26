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
  content : String
  name : Option String := none
  toolCallId : Option String := none  -- For tool responses
  toolCalls : Option (Array ToolCall) := none  -- For assistant messages with tool calls
  deriving Repr, Inhabited

namespace Message

/-- Create a system message -/
def system (content : String) : Message :=
  { role := .system, content := content }

/-- Create a user message -/
def user (content : String) : Message :=
  { role := .user, content := content }

/-- Create an assistant message -/
def assistant (content : String) : Message :=
  { role := .assistant, content := content }

/-- Create a tool response message -/
def toolResponse (toolCallId : String) (content : String) : Message :=
  { role := .tool, content := content, toolCallId := some toolCallId }

/-- Create a developer message -/
def developer (content : String) : Message :=
  { role := .developer, content := content }

end Message

end Oracle
