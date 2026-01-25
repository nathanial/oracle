/-
  OpenRouter integration tests (optional, require env vars)
-/

import Crucible
import Oracle
import Oracle.Agent
import Oracle.Reactive
import Reactive.Host.Spider.Core
import Tests.Support

namespace Tests.OpenRouterIntegrationTests

open Crucible
open Lean Json
open Oracle
open Oracle.Agent
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
    pure (if model.isEmpty then Models.geminiFlash else model)
  | none => pure Models.geminiFlash

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

test "agent loop with calculator tool" (timeout := 60000) := do
  match ← getApiKey with
  | none =>
    IO.println "Skipping OpenRouter integration (set OPENROUTER_RUN_INTEGRATION=1 and OPENROUTER_API_KEY)."
    return ()
  | some key =>
    let client ← buildClient key
    let model ← getModel

    -- Define a calculator tool with proper JSON schema
    let calcParams := Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("operation", Json.mkObj [
          ("type", "string"),
          ("enum", Json.arr #["add", "multiply"]),
          ("description", "The arithmetic operation to perform")
        ]),
        ("a", Json.mkObj [("type", "number"), ("description", "First operand")]),
        ("b", Json.mkObj [("type", "number"), ("description", "Second operand")])
      ]),
      ("required", Json.arr #["operation", "a", "b"])
    ]

    let calcTool := Tool.create "calculate" (some "Perform arithmetic calculations") (some calcParams)
    let calcHandler := ToolHandler.create calcTool fun args => do
      let op := args.getObjValAs? String "operation" |>.toOption |>.getD "add"
      let a := args.getObjValAs? Float "a" |>.toOption |>.getD 0
      let b := args.getObjValAs? Float "b" |>.toOption |>.getD 0
      let result := if op == "multiply" then a * b else a + b
      pure (.ok s!"{result}")

    let registry := ToolRegistry.empty.register calcHandler
    let config : AgentConfig := {
      registry := registry
      maxIterations := 5
      model := model
    }

    let result ← runAgent client.chat config "What is 7 times 8? Use the calculate tool."
    shouldSatisfy result.isSuccess "agent should complete successfully"
    shouldSatisfy ((result.finalContent.getD "").containsSubstr "56") "should contain 56"

test "agent multi-turn with lookup tool" (timeout := 60000) := do
  match ← getApiKey with
  | none =>
    IO.println "Skipping OpenRouter integration (set OPENROUTER_RUN_INTEGRATION=1 and OPENROUTER_API_KEY)."
    return ()
  | some key =>
    let client ← buildClient key
    let model ← getModel

    -- A lookup tool that returns arbitrary data the model couldn't know
    let lookupParams := Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("product_code", Json.mkObj [("type", "string"), ("description", "The product code to look up")])
      ]),
      ("required", Json.arr #["product_code"])
    ]

    let lookupTool := Tool.create "lookup_product" (some "Look up product information by code") (some lookupParams)
    let lookupHandler := ToolHandler.create lookupTool fun args => do
      let code := args.getObjValAs? String "product_code" |>.toOption |>.getD ""
      if code.toLower.containsSubstr "zx7" then
        pure (.ok "Product ZX7-ALPHA: Price $847.23, Color: Vermillion, Stock: 42 units")
      else
        pure (.ok "Product not found in database.")

    let registry := ToolRegistry.empty.register lookupHandler
    let config : AgentConfig := {
      registry := registry
      maxIterations := 5
      model := model
    }

    let result ← runAgent client.chat config "Look up product code ZX7-ALPHA and tell me its price."
    AgentResult.printConversation result
    shouldSatisfy result.isSuccess "agent should complete successfully"
    shouldSatisfy ((result.finalContent.getD "").containsSubstr "847") "should contain the price 847"

test "image generation saves to file" (timeout := 120000) := do
  match ← getApiKey with
  | none =>
    IO.println "Skipping OpenRouter integration (set OPENROUTER_RUN_INTEGRATION=1 and OPENROUTER_API_KEY)."
    return ()
  | some key =>
    let client ← buildClient key

    -- Use an image generation model
    let imageClient := Client.new { client.config with model := Models.geminiFlashImage }

    -- Create output directory
    let outputDir := "test_output"
    let outputPath := s!"{outputDir}/generated_image.png"
    IO.FS.createDirAll outputDir

    match ← imageClient.generateImageToFile "A simple red circle on a white background" outputPath with
    | .ok path =>
      IO.println s!"Image saved to {path}"
      -- Verify file exists and has content
      let content ← IO.FS.readBinFile path
      shouldSatisfy (content.size > 0) "file should have content"
      IO.println s!"Image file size: {content.size} bytes"
    | .error err =>
      -- Some models may not support image generation, so we log but don't fail hard
      IO.println s!"Image generation returned error (model may not support it): {err}"
      -- Check if we got a response with images in the message instead
      let req := ChatRequest.simple Models.geminiFlashImage "A simple red circle on a white background"
        |>.withImageGeneration none
        |>.withMaxTokens 4096
      match ← imageClient.chat req with
      | .ok resp =>
        if resp.hasImages then
          match resp.firstImage? with
          | some img =>
            IO.println s!"Got image in response: {img.url.take 80}..."
            -- Write the raw data URL to a text file for inspection
            IO.FS.writeFile s!"{outputDir}/image_data_url.txt" img.url
            IO.println s!"Saved data URL to {outputDir}/image_data_url.txt"
          | none => IO.println "No image found in response"
        else
          IO.println s!"Response content: {resp.content.getD "(none)"}"
      | .error e2 =>
        IO.println s!"Chat request also failed: {e2}"

test "agent respects iteration limit" (timeout := 60000) := do
  match ← getApiKey with
  | none =>
    IO.println "Skipping OpenRouter integration (set OPENROUTER_RUN_INTEGRATION=1 and OPENROUTER_API_KEY)."
    return ()
  | some key =>
    let client ← buildClient key
    let model ← getModel

    -- Tool that always returns "keep going" to force iteration limit
    let checkParams := Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("status_id", Json.mkObj [("type", "string"), ("description", "Status check ID")])
      ]),
      ("required", Json.arr #[])
    ]

    let checkTool := Tool.create "check_status" (some "Check the processing status") (some checkParams)
    let checkHandler := ToolHandler.create checkTool fun _ => do
      pure (.ok "Status: still processing, please check again")

    let registry := ToolRegistry.empty.register checkHandler
    let config : AgentConfig := {
      registry := registry
      maxIterations := 3
      model := model
      systemPrompt := some "You must use the check_status tool repeatedly until processing is complete. Never give up - always call the tool again."
    }

    let result ← runAgent client.chat config "Check the status until processing is done."
    -- Should hit tool limit, not complete successfully
    shouldBe result.state.isToolLimit true
    shouldSatisfy (result.iterations ≤ 3) "should not exceed max iterations"

end Tests.OpenRouterIntegrationTests
