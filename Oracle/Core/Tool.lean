/-
  Oracle - Tool/Function Types
  Types for tool/function calling support
-/

import Lean.Data.Json

namespace Oracle

open Lean Json

/-- Function definition for tool calling -/
structure ToolFunction where
  /-- Name of the function -/
  name : String
  /-- Description of what the function does -/
  description : Option String := none
  /-- JSON Schema for the function parameters -/
  parameters : Option Json := none
  /-- Whether the function requires strict schema validation -/
  strict : Option Bool := none
  deriving Inhabited

namespace ToolFunction

/-- Create a tool function with just a name -/
def simple (name : String) : ToolFunction :=
  { name := name }

/-- Create a tool function with name and description -/
def withDescription (name : String) (description : String) : ToolFunction :=
  { name := name, description := some description }

/-- Add parameters schema to a function -/
def setParameters (f : ToolFunction) (params : Json) : ToolFunction :=
  { f with parameters := some params }

end ToolFunction

/-- Tool definition -/
structure Tool where
  /-- Type of tool (currently only "function" is supported) -/
  type : String := "function"
  /-- Function definition -/
  function : ToolFunction
  deriving Inhabited

namespace Tool

/-- Create a function tool -/
def fromFunction (f : ToolFunction) : Tool :=
  { type := "function", function := f }

/-- Create a function tool from name and description -/
def create (name : String) (description : Option String := none) (parameters : Option Json := none) : Tool :=
  { function := { name := name, description := description, parameters := parameters } }

end Tool

/-- Tool choice mode -/
inductive ToolChoice where
  /-- No tools will be called -/
  | none
  /-- Model can choose to call a tool or not -/
  | auto
  /-- Model must call at least one tool -/
  | required
  /-- Model must call the specified function -/
  | function (name : String)
  deriving Repr, Inhabited, BEq

end Oracle
