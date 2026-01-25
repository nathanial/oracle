/-
  Agent types tests
-/

import Crucible
import Oracle
import Oracle.Agent
import Tests.Support

namespace Tests.AgentTypesTests

open Crucible
open Oracle
open Oracle.Agent

testSuite "Agent Types"

test "ToolRegistry.empty has no handlers" := do
  let reg := ToolRegistry.empty
  shouldBe reg.handlers.size 0
  shouldBe reg.tools.size 0

test "ToolRegistry.register adds handler" := do
  let handler := ToolHandler.simple "test" fun _ => return .ok "done"
  let reg := ToolRegistry.empty.register handler
  shouldBe reg.handlers.size 1
  shouldBe reg.tools.size 1
  shouldBe reg.tools[0]!.function.name "test"

test "ToolRegistry.findHandler finds by name" := do
  let handler := ToolHandler.simple "my_tool" fun _ => return .ok "result"
  let reg := ToolRegistry.empty.register handler
  match reg.findHandler "my_tool" with
  | some h => shouldBe h.tool.function.name "my_tool"
  | none => throw (IO.userError "Handler not found")

test "ToolRegistry.findHandler returns none for unknown" := do
  let reg := ToolRegistry.empty
  shouldBe (reg.findHandler "unknown").isNone true

test "ToolRegistry.execute runs handler" := do
  let handler := ToolHandler.simple "echo" fun args =>
    let msg := args.getObjValAs? String "message" |>.toOption |>.getD "default"
    return .ok s!"Echo: {msg}"
  let reg := ToolRegistry.empty.register handler

  match ← reg.execute "echo" (Lean.Json.mkObj [("message", Lean.Json.str "hello")]) with
  | .ok result => shouldBe result "Echo: hello"
  | .error e => throw (IO.userError s!"Unexpected error: {e}")

test "ToolRegistry.execute returns error for unknown tool" := do
  let reg := ToolRegistry.empty
  match ← reg.execute "unknown" Lean.Json.null with
  | .ok _ => throw (IO.userError "Expected error for unknown tool")
  | .error msg => shouldSatisfy (msg.containsSubstr "Unknown tool") "should mention unknown tool"

test "AgentConfig default values" := do
  let config : AgentConfig := {}
  shouldBe config.maxIterations 10
  shouldBe config.model "anthropic/claude-sonnet-4"
  shouldBe config.systemPrompt none
  shouldBe config.tools.size 0
  shouldBe config.requestOptions.toolChoice none
  shouldBe (← config.hooks.shouldStop (.running #[] 0)) false

test "AgentConfig.withModel sets model" := do
  let config := AgentConfig.withRegistry ToolRegistry.empty |>.withModel "gpt-4"
  shouldBe config.model "gpt-4"

test "AgentConfig.withSystemPrompt sets prompt" := do
  let config := (default : AgentConfig).withSystemPrompt "Be helpful"
  shouldBe config.systemPrompt (some "Be helpful")

test "AgentConfig.withMaxIterations sets limit" := do
  let config := (default : AgentConfig).withMaxIterations 5
  shouldBe config.maxIterations 5

test "AgentConfig.withRequestOptions sets options" := do
  let opts : AgentRequestOptions := { temperature := some 0.5 }
  let config := (default : AgentConfig).withRequestOptions opts
  shouldBe config.requestOptions.temperature (some 0.5)

test "AgentConfig.withHooks sets hooks" := do
  let hooks : AgentHooks := { shouldStop := fun _ => pure true }
  let config := (default : AgentConfig).withHooks hooks
  shouldBe (← config.hooks.shouldStop (.running #[] 0)) true

test "AgentState predicates" := do
  let running := AgentState.running #[] 0
  let completed := AgentState.completed #[] "done"
  let stopped := AgentState.stopped #[]
  let toolLimit := AgentState.toolLimit #[]
  let error := AgentState.error #[] (OracleError.networkError "fail")

  shouldBe running.isRunning true
  shouldBe running.isTerminal false

  shouldBe completed.isCompleted true
  shouldBe completed.isTerminal true

  shouldBe stopped.isStopped true
  shouldBe stopped.isTerminal true

  shouldBe toolLimit.isToolLimit true
  shouldBe toolLimit.isTerminal true

  shouldBe error.isError true
  shouldBe error.isTerminal true

test "AgentState.finalContent?" := do
  let completed := AgentState.completed #[] "my content"
  let running := AgentState.running #[] 0

  shouldBe completed.finalContent? (some "my content")
  shouldBe running.finalContent? none

test "AgentState.error?" := do
  let err := OracleError.authError "bad key"
  let errorState := AgentState.error #[] err
  let completed := AgentState.completed #[] "done"

  match errorState.error? with
  | some (OracleError.authError msg) => shouldBe msg "bad key"
  | _ => throw (IO.userError "Expected auth error")

  shouldBe completed.error?.isNone true

end Tests.AgentTypesTests
