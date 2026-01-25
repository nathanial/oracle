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

/-- Check if the agent was stopped by a hook or external condition -/
def isStopped (r : AgentResult) : Bool :=
  r.state.isStopped

/-- Get the error if present -/
def error? (r : AgentResult) : Option OracleError :=
  r.state.error?

/-- Format the conversation history for debugging -/
def formatConversation (r : AgentResult) (indent : String := "") : String :=
  let stateStr := match r.state with
    | .running _ iter => s!"running (iteration {iter})"
    | .completed _ _ => "completed"
    | .stopped _ => "stopped"
    | .toolLimit _ => "tool limit reached"
    | .error _ e => s!"error: {repr e}"
  let header := s!"{indent}=== Agent Result: {stateStr}, {r.iterations} iterations ==="
  let conversation := Message.formatConversation r.messages indent
  s!"{header}\n{conversation}"

/-- Print the conversation history to stdout -/
def printConversation (r : AgentResult) : IO Unit :=
  IO.println (formatConversation r)

end AgentResult

/-- Type alias for a chat function (abstracts over real client or mock) -/
def ChatFunction := ChatRequest → IO (OracleResult ChatResponse)

/-- Parse tool arguments from JSON string. -/
private def parseToolArgs (argsStr : String) : Except String Json :=
  match Json.parse argsStr with
  | .ok json => .ok json
  | .error err => .error err

/-- Build a chat request from config and messages. -/
private def buildRequest (config : AgentConfig) (messages : Array Message) : ChatRequest :=
  let tools := config.tools
  let toolChoice :=
    if tools.isEmpty then
      none
    else
      config.requestOptions.toolChoice.orElse (fun _ => some .auto)
  {
    model := config.model
    messages := messages
    tools := if tools.isEmpty then none else some tools
    toolChoice := toolChoice
    temperature := config.requestOptions.temperature
    maxTokens := config.requestOptions.maxTokens
    topP := config.requestOptions.topP
    stop := config.requestOptions.stop
    presencePenalty := config.requestOptions.presencePenalty
    frequencyPenalty := config.requestOptions.frequencyPenalty
    responseFormat := config.requestOptions.responseFormat
    seed := config.requestOptions.seed
    topK := config.requestOptions.topK
    repetitionPenalty := config.requestOptions.repetitionPenalty
    minP := config.requestOptions.minP
    topA := config.requestOptions.topA
    logitBias := config.requestOptions.logitBias
    logprobs := config.requestOptions.logprobs
    topLogprobs := config.requestOptions.topLogprobs
    parallelToolCalls := config.requestOptions.parallelToolCalls
    modalities := config.requestOptions.modalities
    imageConfig := config.requestOptions.imageConfig
  }

/-- Execute all tool calls and return tool response messages -/
private def executeToolCalls (registry : ToolRegistry) (calls : Array ToolCall) : IO (Array Message) := do
  let mut results := #[]
  for call in calls do
    let responseContent ← match parseToolArgs call.function.arguments with
      | .ok args =>
        let result ← registry.execute call.function.name args
        pure <| match result with
          | .ok content => content
          | .error errMsg => s!"Error: {errMsg}"
      | .error errMsg =>
        pure s!"Error: Invalid tool arguments for {call.function.name}: {errMsg}"
    results := results.push (Message.toolResponse call.id responseContent)
  return results

/-- Result of a single agent step. -/
private structure StepResult where
  state : AgentState
  iterationsDelta : Nat := 0

/-- Emit a state update to hooks and return it. -/
private def emitState (config : AgentConfig) (state : AgentState) : IO AgentState := do
  config.hooks.onState state
  return state

/-- Run a single step of the agent loop. -/
private def runAgentStep (chat : ChatFunction) (config : AgentConfig)
    (messages : Array Message) (iteration : Nat) : IO StepResult := do
  let runningState : AgentState := .running messages iteration
  if ← config.hooks.shouldStop runningState then
    let state ← emitState config (.stopped messages)
    return { state := state }

  if iteration >= config.maxIterations then
    let state ← emitState config (.toolLimit messages)
    return { state := state }

  let req := buildRequest config messages

  match ← chat req with
  | .error e =>
    let state ← emitState config (.error messages e)
    return { state := state }
  | .ok resp =>
    let assistantMsg := resp.message.getD (Message.assistant "")
    let messages := messages.push assistantMsg
    let runningState : AgentState := .running messages iteration
    if ← config.hooks.shouldStop runningState then
      let state ← emitState config (.stopped messages)
      return { state := state, iterationsDelta := 1 }

    match resp.toolCalls with
    | none =>
      let content := resp.content.getD ""
      let state ← emitState config (.completed messages content)
      return { state := state, iterationsDelta := 1 }
    | some calls =>
      if calls.isEmpty then
        let content := resp.content.getD ""
        let state ← emitState config (.completed messages content)
        return { state := state, iterationsDelta := 1 }
      else
        let toolResponses ← executeToolCalls config.registry calls
        let messages := messages ++ toolResponses
        let state ← emitState config (.running messages (iteration + 1))
        return { state := state, iterationsDelta := 1 }

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
  let step ← runAgentStep chat config messages iteration
  let totalIterations := iteration + step.iterationsDelta
  match step.state with
  | .running msgs nextIteration =>
    runAgentLoop chat config msgs nextIteration
  | .completed msgs content =>
    return {
      messages := msgs
      finalContent := some content
      iterations := totalIterations
      state := step.state
    }
  | .stopped msgs =>
    return {
      messages := msgs
      finalContent := none
      iterations := totalIterations
      state := step.state
    }
  | .toolLimit msgs =>
    return {
      messages := msgs
      finalContent := none
      iterations := totalIterations
      state := step.state
    }
  | .error msgs err =>
    return {
      messages := msgs
      finalContent := none
      iterations := totalIterations
      state := .error msgs err
    }

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
    return (← runAgentStep chat config messages iteration).state
  | terminal => return terminal

end Oracle.Agent
