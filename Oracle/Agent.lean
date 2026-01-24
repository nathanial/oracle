/-
  Oracle - Agent Module

  Provides agentic loop support for automatic tool execution and multi-turn workflows.

  ## Quick Start

  ```lean
  import Oracle
  import Oracle.Agent

  def main : IO Unit := do
    let client := Oracle.Client.withApiKey (← IO.getEnv "OPENROUTER_API_KEY" |>.map (·.getD ""))

    -- Define tool handlers
    let weatherTool := ToolHandler.withDescription "get_weather" "Get weather for a city" fun args => do
      let city := args.getObjValAs? String "city" |>.toOption |>.getD "Unknown"
      return .ok s!"Weather in {city}: Sunny, 72°F"

    let registry := ToolRegistry.empty.register weatherTool
    let config : AgentConfig := {
      registry
      systemPrompt := some "You are a helpful assistant with access to weather information."
    }

    -- Run the agent
    let result ← runAgent client.chat config "What's the weather in San Francisco?"

    match result.finalContent with
    | some content => IO.println content
    | none => IO.println s!"Agent did not complete: {repr result.state}"
  ```

  ## Testing with Mocks

  ```lean
  -- Create mock responses
  let toolCall := mockToolCall "call_1" "get_weather" "{\"city\":\"NYC\"}"
  let resp1 := mockResponseWithToolCalls "resp_1" #[toolCall]
  let resp2 := mockResponseWithContent "resp_2" "The weather in NYC is sunny."

  let mock ← MockChat.new #[resp1, resp2]
  let result ← runAgentLoop mock.call config messages 0
  ```
-/

import Oracle.Agent.Types
import Oracle.Agent.Mock
import Oracle.Agent.Loop
