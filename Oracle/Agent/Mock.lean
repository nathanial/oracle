/-
  Oracle - Mock Client for Testing
  Provides mock chat functionality for testing agentic loops without API calls
-/

import Lean.Data.Json
import Oracle.Core.Types
import Oracle.Core.Error
import Oracle.Response.ChatResponse
import Oracle.Response.Usage
import Oracle.Request.ChatRequest

namespace Oracle.Agent

open Lean Json Oracle

/-- A mock chat function for testing -/
structure MockChat where
  /-- Responses to return in sequence -/
  responses : Array ChatResponse
  /-- Current index (mutable reference) -/
  indexRef : IO.Ref Nat

namespace MockChat

/-- Create a new mock chat with the given responses -/
def new (responses : Array ChatResponse) : IO MockChat := do
  let indexRef ← IO.mkRef 0
  return { responses, indexRef }

/-- Call the mock, returning the next response in sequence -/
def call (mock : MockChat) (_req : ChatRequest) : IO (OracleResult ChatResponse) := do
  let idx ← mock.indexRef.get
  if h : idx < mock.responses.size then
    mock.indexRef.set (idx + 1)
    return .ok mock.responses[idx]
  else
    return .error (OracleError.apiError "mock_exhausted" "No more mock responses available")

/-- Reset the mock to the beginning -/
def reset (mock : MockChat) : IO Unit :=
  mock.indexRef.set 0

/-- Get the number of calls made -/
def callCount (mock : MockChat) : IO Nat :=
  mock.indexRef.get

end MockChat

/-- Helper to build a ChatResponse with tool calls -/
def mockResponseWithToolCalls (id : String) (toolCalls : Array ToolCall) (model : String := "mock-model") : ChatResponse :=
  let message : Message := {
    role := .assistant
    content := .string ""
    toolCalls := some toolCalls
  }
  let choice : Choice := {
    index := 0
    message := message
    finishReason := some "tool_calls"
  }
  {
    id := id
    model := model
    choices := #[choice]
    usage := some { promptTokens := 10, completionTokens := 5, totalTokens := 15 }
  }

/-- Helper to build a ChatResponse with content (no tool calls) -/
def mockResponseWithContent (id : String) (content : String) (model : String := "mock-model") : ChatResponse :=
  let message : Message := {
    role := .assistant
    content := .string content
  }
  let choice : Choice := {
    index := 0
    message := message
    finishReason := some "stop"
  }
  {
    id := id
    model := model
    choices := #[choice]
    usage := some { promptTokens := 10, completionTokens := 20, totalTokens := 30 }
  }

/-- Helper to create a ToolCall -/
def mockToolCall (id : String) (name : String) (arguments : String := "{}") : ToolCall := {
  id := id
  type := "function"
  function := { name := name, arguments := arguments }
}

/-- Helper to build a ChatResponse that simulates an error being handled -/
def mockResponseWithError (id : String) (errorMessage : String) (model : String := "mock-model") : ChatResponse :=
  mockResponseWithContent id s!"I encountered an error: {errorMessage}" model

end Oracle.Agent
