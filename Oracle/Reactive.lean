/-
  Oracle/Reactive.lean

  Reactive FRP integration for Oracle.

  This module provides push-based streaming and observable state management
  for OpenRouter API interactions using the Reactive FRP library.

  ## Quick Start

  ```lean
  import Oracle.Reactive

  def main : IO Unit := SpiderM.runFresh do
    let client := Oracle.Reactive.ReactiveClient.withApiKey (← liftIO <| IO.getEnv "OPENROUTER_API_KEY" |>.map (·.getD ""))

    -- Simple streaming prompt
    let output ← client.promptStream "Tell me a joke"

    -- Subscribe to content as it arrives
    let _ ← output.content.subscribe fun content =>
      liftIO <| IO.print s!"\r{content}"

    -- Wait for completion
    let _ ← output.completed.subscribe fun state =>
      liftIO <| IO.println s!"\nDone! ({state.chunkCount} chunks)"

    -- Keep running until complete
    liftIO <| IO.sleep 30000
  ```

  ## Key Types

  - `RequestState e a` - Request lifecycle: idle → loading → streaming → ready/error
  - `StreamState` - Accumulated streaming state (content, tool calls, etc.)
  - `StreamOutput` - Reactive outputs from a stream (chunks event, content dynamic, etc.)
  - `ReactiveClient` - Wrapper around Oracle.Client with reactive methods
  - `ConversationManager` - Multi-turn conversation with observable history

  ## Patterns

  ### Event-Driven Streaming

  ```lean
  -- Stream for each request event, canceling previous on new
  let outputDyn ← streamOnEvent client requestEvents

  -- Rate-limited version
  let outputDyn ← throttledStreamOnEvent client (500.ms) requestEvents
  ```

  ### Retry Logic

  ```lean
  -- Stream with automatic retry on transient failures
  let stateDyn ← streamWithRetry RetryConfig.default client request
  ```

  ### Conversation Management

  ```lean
  let conv ← ConversationManager.withSystemPrompt client "You are a helpful assistant"

  -- Observable conversation history
  let _ ← conv.conversation.subscribe fun history =>
    liftIO <| IO.println s!"Messages: {history.messageCount}"

  -- Send messages
  liftIO <| conv.sendMessage "Hello!"

  -- React to responses
  let _ ← conv.assistantResponded.subscribe fun response =>
    liftIO <| IO.println s!"Assistant: {response}"
  ```

  ## Module Structure

  - `Oracle.Reactive.Types` - Core types (RequestState, StreamState, etc.)
  - `Oracle.Reactive.Stream` - SSE stream to event conversion
  - `Oracle.Reactive.Client` - ReactiveClient wrapper
  - `Oracle.Reactive.Conversation` - ConversationManager
  - `Oracle.Reactive.Integration` - Helper combinators
-/

import Oracle.Reactive.Types
import Oracle.Reactive.Stream
import Oracle.Reactive.Client
import Oracle.Reactive.Conversation
import Oracle.Reactive.Integration
