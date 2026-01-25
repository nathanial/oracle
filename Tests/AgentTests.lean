/-
  Agentic loop tests
-/

import Crucible
import Oracle
import Oracle.Agent
import Tests.Support

namespace Tests.AgentTests

open Crucible
open Oracle
open Oracle.Agent

testSuite "Agentic Loop"

test "single tool call and response" := do
  -- Setup: mock returns tool call, then final response
  let toolCall := mockToolCall "call_1" "get_weather" "{\"city\":\"NYC\"}"
  let resp1 := mockResponseWithToolCalls "resp_1" #[toolCall]
  let resp2 := mockResponseWithContent "resp_2" "The weather in NYC is sunny."

  let mock ← MockChat.new #[resp1, resp2]

  -- Tool that returns weather
  let weatherTool := ToolHandler.withDescription "get_weather" "Get weather for a city" fun _args =>
    return .ok "Sunny, 72°F"

  let registry := ToolRegistry.empty.register weatherTool
  let config : AgentConfig := { registry }

  let result ← runAgentLoop mock.call config #[Message.user "What's the weather in NYC?"] 0

  shouldBe result.iterations 2
  shouldBe result.finalContent (some "The weather in NYC is sunny.")
  shouldSatisfy result.isSuccess "should complete successfully"

test "max iterations prevents infinite loop" := do
  -- Mock always returns tool calls
  let toolCall := mockToolCall "call_1" "search" "{}"
  let resp := mockResponseWithToolCalls "resp" #[toolCall]
  -- Create array of 20 identical responses
  let responses := #[resp, resp, resp, resp, resp, resp, resp, resp, resp, resp,
                     resp, resp, resp, resp, resp, resp, resp, resp, resp, resp]
  let mock ← MockChat.new responses

  let searchTool := ToolHandler.simple "search" fun _ => return .ok "found"
  let registry := ToolRegistry.empty.register searchTool
  let config : AgentConfig := { maxIterations := 3, registry }

  let result ← runAgentLoop mock.call config #[Message.user "Search forever"] 0

  shouldBe result.iterations 3
  shouldSatisfy result.hitToolLimit "should hit tool limit"

test "handles tool execution error gracefully" := do
  let toolCall := mockToolCall "call_1" "failing_tool" "{}"
  let resp1 := mockResponseWithToolCalls "resp_1" #[toolCall]
  let resp2 := mockResponseWithContent "resp_2" "I encountered an error."
  let mock ← MockChat.new #[resp1, resp2]

  let failingTool := ToolHandler.simple "failing_tool" fun _ =>
    return .error "Tool crashed!"

  let registry := ToolRegistry.empty.register failingTool
  let config : AgentConfig := { registry }

  let result ← runAgentLoop mock.call config #[Message.user "Run failing tool"] 0

  -- Should continue with error message as tool response
  shouldBe result.iterations 2
  -- Check that the tool response contains the error
  let toolResponses := result.messages.filter fun m => m.role == .tool
  shouldBe toolResponses.size 1
  shouldSatisfy (toolResponses[0]!.content.asString.containsSubstr "Error") "tool response should contain error"

test "invalid tool arguments produce error response" := do
  let toolCall := mockToolCall "call_1" "calculate" "{\"x\":1"
  let resp1 := mockResponseWithToolCalls "resp_1" #[toolCall]
  let resp2 := mockResponseWithContent "resp_2" "Handled error."
  let mock ← MockChat.new #[resp1, resp2]

  let calledRef ← IO.mkRef false
  let tool := ToolHandler.simple "calculate" fun _ => do
    calledRef.set true
    return .ok "ok"

  let registry := ToolRegistry.empty.register tool
  let config : AgentConfig := { registry }

  let result ← runAgentLoop mock.call config #[Message.user "Compute"] 0

  let toolResponses := result.messages.filter fun m => m.role == .tool
  shouldBe toolResponses.size 1
  shouldSatisfy (toolResponses[0]!.content.asString.containsSubstr "Invalid tool arguments") "should mention invalid arguments"
  shouldBe (← calledRef.get) false

test "multiple tool calls in single response" := do
  let toolCalls := #[
    mockToolCall "call_1" "tool_a" "{}",
    mockToolCall "call_2" "tool_b" "{}"
  ]
  let resp1 := mockResponseWithToolCalls "resp_1" toolCalls
  let resp2 := mockResponseWithContent "resp_2" "Done with both tools."
  let mock ← MockChat.new #[resp1, resp2]

  let toolA := ToolHandler.simple "tool_a" fun _ => return .ok "A result"
  let toolB := ToolHandler.simple "tool_b" fun _ => return .ok "B result"
  let registry := ToolRegistry.empty.register toolA |>.register toolB
  let config : AgentConfig := { registry }

  let result ← runAgentLoop mock.call config #[Message.user "Run both tools"] 0

  shouldBe result.iterations 2
  -- Messages should have both tool responses
  let toolResponses := result.messages.filter fun m => m.role == .tool
  shouldBe toolResponses.size 2

test "no tool calls returns immediately" := do
  let resp := mockResponseWithContent "resp_1" "Hello! How can I help?"
  let mock ← MockChat.new #[resp]

  let config : AgentConfig := { registry := ToolRegistry.empty }
  let result ← runAgentLoop mock.call config #[Message.user "Hello"] 0

  shouldBe result.iterations 1
  shouldBe result.finalContent (some "Hello! How can I help?")
  shouldSatisfy result.isSuccess "should complete successfully"

test "API error propagates correctly" := do
  -- Create a mock that returns an error
  let errorMock : ChatFunction := fun _ =>
    return .error (OracleError.networkError "Connection failed")

  let config : AgentConfig := { registry := ToolRegistry.empty }
  let result ← runAgentLoop errorMock config #[Message.user "Hello"] 0

  shouldSatisfy result.isError "should have error state"
  match result.error? with
  | some (OracleError.networkError msg) => shouldBe msg "Connection failed"
  | _ => throw (IO.userError "Expected network error")

test "tool with JSON arguments" := do
  let toolCall := mockToolCall "call_1" "calculate" "{\"x\":10,\"y\":5,\"op\":\"add\"}"
  let resp1 := mockResponseWithToolCalls "resp_1" #[toolCall]
  let resp2 := mockResponseWithContent "resp_2" "The result is 15."
  let mock ← MockChat.new #[resp1, resp2]

  let calcTool := ToolHandler.withDescription "calculate" "Perform calculation" fun args => do
    let x := args.getObjValAs? Int "x" |>.toOption |>.getD 0
    let y := args.getObjValAs? Int "y" |>.toOption |>.getD 0
    let op := args.getObjValAs? String "op" |>.toOption |>.getD "add"
    let result := if op == "add" then x + y else x - y
    return .ok s!"{result}"

  let registry := ToolRegistry.empty.register calcTool
  let config : AgentConfig := { registry }

  let result ← runAgentLoop mock.call config #[Message.user "Calculate 10 + 5"] 0

  shouldBe result.iterations 2
  shouldSatisfy result.isSuccess "should complete successfully"

test "unknown tool returns error message" := do
  let toolCall := mockToolCall "call_1" "unknown_tool" "{}"
  let resp1 := mockResponseWithToolCalls "resp_1" #[toolCall]
  let resp2 := mockResponseWithContent "resp_2" "I couldn't find that tool."
  let mock ← MockChat.new #[resp1, resp2]

  -- Empty registry - tool won't be found
  let config : AgentConfig := { registry := ToolRegistry.empty }

  let result ← runAgentLoop mock.call config #[Message.user "Use unknown tool"] 0

  shouldBe result.iterations 2
  -- The tool response should contain "Unknown tool"
  let toolResponses := result.messages.filter fun m => m.role == .tool
  shouldBe toolResponses.size 1
  shouldSatisfy (toolResponses[0]!.content.asString.containsSubstr "Unknown tool") "should mention unknown tool"

test "runAgent with system prompt" := do
  let resp := mockResponseWithContent "resp_1" "I am a helpful assistant."
  let mock ← MockChat.new #[resp]

  let config : AgentConfig := {
    registry := ToolRegistry.empty
    systemPrompt := some "You are a helpful assistant."
  }

  let result ← runAgent mock.call config "Hello"

  shouldBe result.iterations 1
  -- Should have system message first
  shouldBe result.messages[0]!.role Role.system
  shouldBe result.messages[1]!.role Role.user

test "multi-turn tool usage" := do
  -- First: model calls tool A
  let call1 := mockToolCall "call_1" "search" "{\"query\":\"weather\"}"
  let resp1 := mockResponseWithToolCalls "resp_1" #[call1]

  -- Second: model calls tool B based on search results
  let call2 := mockToolCall "call_2" "format" "{\"data\":\"sunny\"}"
  let resp2 := mockResponseWithToolCalls "resp_2" #[call2]

  -- Third: model returns final answer
  let resp3 := mockResponseWithContent "resp_3" "Based on my search, the weather is sunny and formatted nicely."
  let mock ← MockChat.new #[resp1, resp2, resp3]

  let searchTool := ToolHandler.simple "search" fun _ => return .ok "sunny"
  let formatTool := ToolHandler.simple "format" fun _ => return .ok "**sunny**"
  let registry := ToolRegistry.empty.register searchTool |>.register formatTool
  let config : AgentConfig := { registry }

  let result ← runAgentLoop mock.call config #[Message.user "What's the weather?"] 0

  shouldBe result.iterations 3
  shouldSatisfy result.isSuccess "should complete successfully"
  -- Should have multiple tool responses
  let toolResponses := result.messages.filter fun m => m.role == .tool
  shouldBe toolResponses.size 2

test "empty tool calls array treated as completion" := do
  -- Response with empty tool calls array (some models do this)
  let message : Message := {
    role := .assistant
    content := .string "Here's my answer."
    toolCalls := some #[]  -- Empty array
  }
  let choice : Choice := {
    index := 0
    message := message
    finishReason := some "stop"
  }
  let resp : ChatResponse := {
    id := "resp_1"
    model := "mock-model"
    choices := #[choice]
  }
  let mock ← MockChat.new #[resp]

  let config : AgentConfig := { registry := ToolRegistry.empty }
  let result ← runAgentLoop mock.call config #[Message.user "Hello"] 0

  shouldBe result.iterations 1
  shouldBe result.finalContent (some "Here's my answer.")
  shouldSatisfy result.isSuccess "should complete successfully"

test "AgentConfig request options are applied" := do
  let reqRef ← IO.mkRef (none : Option ChatRequest)
  let chat : ChatFunction := fun req => do
    reqRef.set (some req)
    return .ok (mockResponseWithContent "resp_1" "ok")

  let tool := ToolHandler.simple "noop" fun _ => return .ok "done"
  let registry := ToolRegistry.empty.register tool
  let opts : AgentRequestOptions := {
    temperature := some 0.2
    topP := some 0.9
    stop := some #["END"]
    toolChoice := some .required
    parallelToolCalls := some true
  }
  let config : AgentConfig := { registry, requestOptions := opts }

  let _ ← runAgentLoop chat config #[Message.user "Hello"] 0

  match ← reqRef.get with
  | some req =>
    shouldBe req.temperature (some 0.2)
    shouldBe req.topP (some 0.9)
    shouldBe req.stop (some #["END"])
    shouldBe req.toolChoice (some .required)
    shouldBe req.parallelToolCalls (some true)
    shouldSatisfy req.tools.isSome "tools should be set from registry"
  | none => throw (IO.userError "Expected request to be captured")

test "stepAgent runs a single iteration" := do
  let toolCall := mockToolCall "call_1" "tool" "{}"
  let resp1 := mockResponseWithToolCalls "resp_1" #[toolCall]
  let resp2 := mockResponseWithContent "resp_2" "Done."
  let mock ← MockChat.new #[resp1, resp2]

  let tool := ToolHandler.simple "tool" fun _ => return .ok "ok"
  let registry := ToolRegistry.empty.register tool
  let config : AgentConfig := { registry }

  let state0 : AgentState := .running #[Message.user "Hello"] 0
  let state1 ← stepAgent mock.call config state0
  match state1 with
  | .running msgs iter =>
    shouldBe iter 1
    let toolResponses := msgs.filter fun m => m.role == .tool
    shouldBe toolResponses.size 1
  | _ => throw (IO.userError "Expected running state after first step")

  let state2 ← stepAgent mock.call config state1
  match state2 with
  | .completed _ content => shouldBe content "Done."
  | _ => throw (IO.userError "Expected completed state after second step")

test "hooks can stop before calling chat" := do
  let callCount ← IO.mkRef 0
  let chat : ChatFunction := fun _ => do
    callCount.modify (· + 1)
    return .ok (mockResponseWithContent "resp" "ok")

  let hooks : AgentHooks := { shouldStop := fun _ => pure true }
  let config : AgentConfig := { hooks := hooks }

  let result ← runAgentLoop chat config #[Message.user "Hello"] 0
  shouldSatisfy result.isStopped "should stop early"
  shouldBe (← callCount.get) 0

test "hooks can stop after receiving tool calls" := do
  let toolCall := mockToolCall "call_1" "tool" "{}"
  let resp := mockResponseWithToolCalls "resp_1" #[toolCall]
  let mock ← MockChat.new #[resp]

  let hooks : AgentHooks := {
    shouldStop := fun state => do
      let last := state.messages.back?
      return last.bind (·.toolCalls) |>.isSome
  }

  let tool := ToolHandler.simple "tool" fun _ => return .ok "ok"
  let registry := ToolRegistry.empty.register tool
  let config : AgentConfig := { registry, hooks }

  let result ← runAgentLoop mock.call config #[Message.user "Hello"] 0
  shouldSatisfy result.isStopped "should stop after tool call"
  let toolResponses := result.messages.filter fun m => m.role == .tool
  shouldBe toolResponses.size 0

test "onState hook receives updates" := do
  let countRef ← IO.mkRef 0
  let hooks : AgentHooks := { onState := fun _ => countRef.modify (· + 1) }
  let resp := mockResponseWithContent "resp_1" "Hello!"
  let mock ← MockChat.new #[resp]
  let config : AgentConfig := { hooks }

  let _ ← runAgentLoop mock.call config #[Message.user "Hello"] 0
  shouldBe (← countRef.get) 1

end Tests.AgentTests
