/-
  OpenRouter integration tests (optional, require env vars)
-/

import Crucible
import Oracle
import Oracle.Reactive
import Reactive.Host.Spider.Core
import Tests.Support

namespace Tests.OpenRouterIntegrationTests

open Crucible
open Oracle
open Oracle.Reactive
open Reactive.Host

testSuite "OpenRouter Integration"

private def integrationEnabled : IO Bool := do
  match (← IO.getEnv "OPENROUTER_RUN_INTEGRATION") with
  | some flag =>
    let flag := flag.trim.toLower
    return flag == "1" || flag == "true" || flag == "yes"
  | none => return false

private def getApiKey : IO (Option String) := do
  if !(← integrationEnabled) then
    return none
  match (← IO.getEnv "OPENROUTER_API_KEY") with
  | some key =>
    let key := key.trim
    pure (if key.isEmpty then none else some key)
  | none => pure none

private def getModel : IO String := do
  match (← IO.getEnv "OPENROUTER_MODEL") with
  | some model =>
    let model := model.trim
    pure (if model.isEmpty then Models.claudeSonnet else model)
  | none => pure Models.claudeSonnet

private def getBaseUrl : IO String := do
  match (← IO.getEnv "OPENROUTER_BASE_URL") with
  | some url =>
    let url := url.trim
    pure (if url.isEmpty then "https://openrouter.ai/api/v1" else url)
  | none => pure "https://openrouter.ai/api/v1"

private def buildClient (apiKey : String) : IO Client := do
  let model ← getModel
  let baseUrl ← getBaseUrl
  let config : Config := { apiKey := apiKey, model := model, baseUrl := baseUrl }
  return Client.new config

private def waitUntil (attempts : Nat) (delayMs : UInt32) (cond : IO Bool) : IO Bool := do
  let rec loop : Nat → IO Bool
    | 0 => pure false
    | n + 1 => do
      if (← cond) then
        pure true
      else
        IO.sleep delayMs
        loop n
  loop attempts

test "prompt returns content" (timeout := 20000) := do
  match ← getApiKey with
  | none =>
    IO.println "Skipping OpenRouter integration (set OPENROUTER_RUN_INTEGRATION=1 and OPENROUTER_API_KEY)."
    return ()
  | some key =>
    let client ← buildClient key
    match ← client.prompt "Reply with OK." with
    | .ok content =>
      shouldSatisfy (content.toLower.containsSubstr "ok") "should contain ok"
    | .error err =>
      throw (IO.userError s!"OpenRouter error: {err}")

test "reactive promptStream completes" (timeout := 30000) := do
  match ← getApiKey with
  | none =>
    IO.println "Skipping OpenRouter integration (set OPENROUTER_RUN_INTEGRATION=1 and OPENROUTER_API_KEY)."
    return ()
  | some key =>
    let client ← buildClient key
    let reactive := ReactiveClient.new client
    let (stateOpt, content) ← SpiderM.runFresh do
      let output ← reactive.promptStream "Reply with OK."
      let doneRef ← SpiderM.liftIO (IO.mkRef (none : Option Oracle.Reactive.StreamState))
      let _ ← SpiderM.liftIO <| Reactive.Event.subscribe output.completed fun state =>
        doneRef.set (some state)
      let _ ← SpiderM.liftIO <| waitUntil 120 250 (do
        return (← doneRef.get).isSome)
      let stateOpt ← SpiderM.liftIO doneRef.get
      let content ← SpiderM.liftIO output.content.sample
      return (stateOpt, content)
    match stateOpt with
    | some _ =>
      shouldSatisfy (content.toLower.containsSubstr "ok") "should contain ok"
    | none => throw (IO.userError "Expected reactive completion")

end Tests.OpenRouterIntegrationTests
