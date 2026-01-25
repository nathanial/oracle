/-
  Oracle - OpenRouter Client Library for Lean 4

  A type-safe client for the OpenRouter API with streaming
  and tool/function calling support.

  ## Quick Start

  ```lean
  import Oracle

  def main : IO Unit := do
    let client := Oracle.Client.withApiKey (← IO.getEnv "OPENROUTER_API_KEY" |>.map (·.getD ""))

    -- Simple prompt
    match ← client.prompt "What is 2 + 2?" with
    | .ok response => IO.println response
    | .error e => IO.println s!"Error: {e}"

    -- Streaming
    match ← client.promptStream "Tell me a story" with
    | .ok content => IO.println s!"Got: {content}"
    | .error e => IO.println s!"Error: {e}"
  ```
-/

-- Core types
import Oracle.Core.Types
import Oracle.Core.Config
import Oracle.Core.Error
import Oracle.Core.Tool

-- JSON utilities
import Oracle.Json

-- Retry logic
import Oracle.Retry

-- Request types
import Oracle.Request.ChatRequest

-- Response types
import Oracle.Response.Usage
import Oracle.Response.ChatResponse
import Oracle.Response.Delta
import Oracle.Response.StreamAccumulator
import Oracle.Response.GenerationStats
import Oracle.Response.Model

-- Client
import Oracle.Client.Sync
import Oracle.Client.Stream
