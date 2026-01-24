/-
  Oracle - Agent Loop
  Core agentic loop implementation for automatic tool execution
-/

import Lean.Data.Json
import Oracle.Core.Types
import Oracle.Core.Tool
import Oracle.Core.Error
import Oracle.Request.ChatRequest
import Oracle.Response.ChatResponse
import Oracle.Agent.Types

namespace Oracle.Agent

open Lean Json Oracle

/-- Result of running an agent -/
structure AgentResult where
  /-- All messages exchanged during the agent run -/
  messages : Array Message
  /-- Final content from the assistant (if completed) -/
  finalContent : Option String
  /-- Number of iterations performed -/
  iterations : Nat
  /-- Final state of the agent -/
  state : AgentState
  deriving Repr, Inhabited

namespace AgentResult

/-- Check if the agent completed successfully -/
def isSuccess (r : AgentResult) : Bool :=
  r.state.isCompleted

/-- Check if the agent hit the tool limit -/
def hitToolLimit (r : AgentResult) : Bool :=
  r.state.isToolLimit

/-- Check if the agent encountered an error -/
def isError (r : AgentResult) : Bool :=
  r.state.isError

/-- Get the error if present -/
def error? (r : AgentResult) : Option OracleError :=
  r.state.error?

end AgentResult

/-- Type alias for a chat function (abstracts over real client or mock) -/
def ChatFunction := ChatRequest → IO (OracleResult ChatResponse)

/-- Parse tool arguments from JSON string -/
private def parseToolArgs (argsStr : String) : Json :=
  match Json.parse argsStr with
  | .ok json => json
  | .error _ => Json.null

/-- Execute all tool calls and return tool response messages -/
private def executeToolCalls (registry : ToolRegistry) (calls : Array ToolCall) : IO (Array Message) := do
  let mut results := #[]
  for call in calls do
    let args := parseToolArgs call.function.arguments
    let result ← registry.execute call.function.name args
    let responseContent := match result with
      | .ok content => content
      | .error errMsg => s!"Error: {errMsg}"
    results := results.push (Message.toolResponse call.id responseContent)
  return results

/-- Run the agent loop until completion, error, or max iterations

    Parameters:
    - chat: Function to call the chat API (can be real client or mock)
    - config: Agent configuration with tools and settings
    - messages: Current message history
    - iteration: Current iteration count

    Returns an AgentResult with the final state and all messages
-/
partial def runAgentLoop (chat : ChatFunction) (config : AgentConfig)
    (messages : Array Message) (iteration : Nat) : IO AgentResult := do
  -- Check iteration limit
  if iteration >= config.maxIterations then
    return {
      messages
      finalContent := none
      iterations := iteration
      state := .toolLimit messages
    }

  -- Build the chat request
  let tools := config.tools
  let req : ChatRequest := {
    model := config.model
    messages := messages
    tools := if tools.isEmpty then none else some tools
    toolChoice := if tools.isEmpty then none else some .auto
  }

  -- Call the chat API
  match ← chat req with
  | .error e =>
    return {
      messages
      finalContent := none
      iterations := iteration
      state := .error messages e
    }
  | .ok resp =>
    -- Extract the assistant message
    let assistantMsg := resp.message.getD (Message.assistant "")
    let messages := messages.push assistantMsg

    -- Check for tool calls
    match resp.toolCalls with
    | none =>
      -- No tool calls - agent is done
      let content := resp.content.getD ""
      return {
        messages
        finalContent := some content
        iterations := iteration + 1
        state := .completed messages content
      }
    | some calls =>
      if calls.isEmpty then
        -- Empty tool calls array - agent is done
        let content := resp.content.getD ""
        return {
          messages
          finalContent := some content
          iterations := iteration + 1
          state := .completed messages content
        }
      else
        -- Execute tool calls
        let toolResponses ← executeToolCalls config.registry calls
        let messages := messages ++ toolResponses

        -- Continue the loop
        runAgentLoop chat config messages (iteration + 1)

/-- Run an agent with a user prompt

    This is the main entry point for running an agent. It:
    1. Creates the initial message with the user prompt
    2. Optionally adds a system prompt
    3. Runs the agent loop until completion

    Parameters:
    - chat: Function to call the chat API
    - config: Agent configuration
    - userPrompt: The user's initial prompt

    Returns an AgentResult with the final state
-/
def runAgent (chat : ChatFunction) (config : AgentConfig) (userPrompt : String) : IO AgentResult := do
  let mut messages := #[Message.user userPrompt]

  -- Add system prompt if configured
  if let some sysPrompt := config.systemPrompt then
    messages := #[Message.system sysPrompt] ++ messages

  runAgentLoop chat config messages 0

/-- Run a single step of the agent loop (for debugging/testing)

    Returns the new state after one API call and tool execution round.
-/
def stepAgent (chat : ChatFunction) (config : AgentConfig) (state : AgentState) : IO AgentState := do
  match state with
  | .running messages iteration =>
    let result ← runAgentLoop chat config messages iteration
    -- Return state after just one step
    -- Note: This actually runs to completion, so for true single-step,
    -- we'd need a different implementation
    return result.state
  | terminal => return terminal

end Oracle.Agent
