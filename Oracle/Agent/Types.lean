/-
  Oracle - Agent Types
  Core types for agentic loop support
-/

import Lean.Data.Json
import Oracle.Core.Types
import Oracle.Core.Tool
import Oracle.Core.Error
import Oracle.Request.ChatRequest

namespace Oracle.Agent

open Lean Json Oracle

/-- Handler for a single tool -/
structure ToolHandler where
  /-- The tool definition -/
  tool : Tool
  /-- Execute the tool with the given arguments (as JSON) -/
  execute : Json → IO (Except String String)

namespace ToolHandler

/-- Create a tool handler from a tool and execute function -/
def create (tool : Tool) (execute : Json → IO (Except String String)) : ToolHandler :=
  { tool, execute }

/-- Create a tool handler with just a name and execute function -/
def simple (name : String) (execute : Json → IO (Except String String)) : ToolHandler :=
  { tool := Tool.create name, execute }

/-- Create a tool handler with name, description, and execute function -/
def withDescription (name : String) (description : String) (execute : Json → IO (Except String String)) : ToolHandler :=
  { tool := Tool.create name (some description), execute }

end ToolHandler

/-- Registry of tool handlers -/
structure ToolRegistry where
  /-- Registered handlers -/
  handlers : Array ToolHandler
  deriving Inhabited

namespace ToolRegistry

/-- Create an empty registry -/
def empty : ToolRegistry := { handlers := #[] }

/-- Register a tool handler -/
def register (reg : ToolRegistry) (handler : ToolHandler) : ToolRegistry :=
  { handlers := reg.handlers.push handler }

/-- Get all tools from the registry -/
def tools (reg : ToolRegistry) : Array Tool :=
  reg.handlers.map (·.tool)

/-- Find a handler by name -/
def findHandler (reg : ToolRegistry) (name : String) : Option ToolHandler :=
  reg.handlers.find? fun h => h.tool.function.name == name

/-- Execute a tool by name with the given arguments -/
def execute (reg : ToolRegistry) (name : String) (args : Json) : IO (Except String String) := do
  match reg.findHandler name with
  | some handler => handler.execute args
  | none => return .error s!"Unknown tool: {name}"

end ToolRegistry

/-- Optional request settings for agent chat calls. -/
structure AgentRequestOptions where
  temperature : Option Float := none
  maxTokens : Option Nat := none
  topP : Option Float := none
  stop : Option (Array String) := none
  presencePenalty : Option Float := none
  frequencyPenalty : Option Float := none
  responseFormat : Option ResponseFormat := none
  seed : Option Nat := none
  topK : Option Nat := none
  repetitionPenalty : Option Float := none
  minP : Option Float := none
  topA : Option Float := none
  logitBias : Option (List (Nat × Float)) := none
  logprobs : Option Bool := none
  topLogprobs : Option Nat := none
  parallelToolCalls : Option Bool := none
  toolChoice : Option ToolChoice := none
  modalities : Option (Array Modality) := none
  imageConfig : Option ImageConfig := none
  deriving Inhabited

/-- State of the agent loop -/
inductive AgentState where
  /-- Agent is still running -/
  | running (messages : Array Message) (iteration : Nat)
  /-- Agent completed successfully with final content -/
  | completed (messages : Array Message) (finalContent : String)
  /-- Agent stopped by a hook or external condition -/
  | stopped (messages : Array Message)
  /-- Agent hit the tool iteration limit -/
  | toolLimit (messages : Array Message)
  /-- Agent encountered an error -/
  | error (messages : Array Message) (err : OracleError)
  deriving Repr, Inhabited

namespace AgentState

/-- Check if the agent is still running -/
def isRunning : AgentState → Bool
  | .running _ _ => true
  | _ => false

/-- Check if the agent completed successfully -/
def isCompleted : AgentState → Bool
  | .completed _ _ => true
  | _ => false

/-- Check if the agent hit the tool limit -/
def isToolLimit : AgentState → Bool
  | .toolLimit _ => true
  | _ => false

/-- Check if the agent was stopped by a hook or external condition -/
def isStopped : AgentState → Bool
  | .stopped _ => true
  | _ => false

/-- Check if the agent encountered an error -/
def isError : AgentState → Bool
  | .error _ _ => true
  | _ => false

/-- Check if the agent is in a terminal state (not running) -/
def isTerminal : AgentState → Bool
  | .running _ _ => false
  | _ => true

/-- Get the messages from any state -/
def messages : AgentState → Array Message
  | .running msgs _ => msgs
  | .completed msgs _ => msgs
  | .stopped msgs => msgs
  | .toolLimit msgs => msgs
  | .error msgs _ => msgs

/-- Get the final content if completed -/
def finalContent? : AgentState → Option String
  | .completed _ content => some content
  | _ => none

/-- Get the error if in error state -/
def error? : AgentState → Option OracleError
  | .error _ err => some err
  | _ => none

end AgentState

/-- Hooks for task management and progress reporting. -/
structure AgentHooks where
  /-- Return true to stop the agent loop. -/
  shouldStop : AgentState → IO Bool := fun _ => pure false
  /-- Called when the agent state updates. -/
  onState : AgentState → IO Unit := fun _ => pure ()
  deriving Inhabited

/-- Configuration for the agent loop -/
structure AgentConfig where
  /-- Maximum number of iterations before stopping -/
  maxIterations : Nat := 10
  /-- Model to use for the agent -/
  model : String := "anthropic/claude-sonnet-4"
  /-- Tool registry with handlers -/
  registry : ToolRegistry := ToolRegistry.empty
  /-- Optional request settings for agent chat calls -/
  requestOptions : AgentRequestOptions := {}
  /-- Hooks for task management and progress reporting -/
  hooks : AgentHooks := {}
  /-- Optional system prompt -/
  systemPrompt : Option String := none
  deriving Inhabited

namespace AgentConfig

/-- Get all tools from the config's registry -/
def tools (cfg : AgentConfig) : Array Tool :=
  cfg.registry.tools

/-- Create a config with a registry -/
def withRegistry (registry : ToolRegistry) : AgentConfig :=
  { registry }

/-- Set the model -/
def withModel (cfg : AgentConfig) (model : String) : AgentConfig :=
  { cfg with model }

/-- Set the system prompt -/
def withSystemPrompt (cfg : AgentConfig) (prompt : String) : AgentConfig :=
  { cfg with systemPrompt := some prompt }

/-- Set max iterations -/
def withMaxIterations (cfg : AgentConfig) (n : Nat) : AgentConfig :=
  { cfg with maxIterations := n }

/-- Set request options -/
def withRequestOptions (cfg : AgentConfig) (opts : AgentRequestOptions) : AgentConfig :=
  { cfg with requestOptions := opts }

/-- Set hooks for task management and progress reporting. -/
def withHooks (cfg : AgentConfig) (hooks : AgentHooks) : AgentConfig :=
  { cfg with hooks := hooks }

end AgentConfig

end Oracle.Agent
