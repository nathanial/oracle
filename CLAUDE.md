# CLAUDE.md - Oracle

Type-safe OpenRouter API client library for Lean 4.

## Build & Test

```bash
lake build         # Build library
lake test          # Run unit tests
./run-integration-tests.sh  # Run integration tests (requires .env with OPENROUTER_API_KEY)
```

## Project Structure

```
Oracle.lean           # Main module, exports all public API
Oracle/
  Core/
    Types.lean        # Message, Role, Content types
    Config.lean       # Client configuration
    Error.lean        # OracleError, OracleResult types
    Tool.lean         # Tool/function calling definitions
  Client/
    Sync.lean         # Synchronous client (chat, complete, prompt)
    Stream.lean       # Streaming client (SSE-based)
  Request/
    ChatRequest.lean  # Request construction with builder pattern
  Response/
    ChatResponse.lean # Response parsing
    Delta.lean        # Streaming delta chunks
    StreamAccumulator.lean  # Accumulates streamed content
    Usage.lean        # Token usage stats
    GenerationStats.lean    # Generation metadata
    Model.lean        # Model info types
  Json.lean           # JSON serialization helpers
  Retry.lean          # Retry logic for transient errors
  Reactive.lean       # FRP integration (Event/Dynamic streams)
Tests/                # Unit and integration tests
```

## Dependencies

- **wisp** - HTTP client (provides curl FFI)
- **crucible** - Test framework
- **chronicle** - Logging
- **reactive** - FRP primitives for streaming

## Key Types

```lean
-- Client creation
Oracle.Client.withApiKey : String → Client
Oracle.Client.withModel : String → String → Client

-- Messages
Oracle.Message.system : String → Message
Oracle.Message.user : String → Message
Oracle.Message.assistant : String → Message

-- Errors
Oracle.OracleError : Type
Oracle.OracleResult α := Except OracleError α
```

## Common Patterns

### Simple prompt
```lean
let client := Oracle.Client.withApiKey apiKey
match ← client.prompt "Hello" with
| .ok response => IO.println response
| .error e => IO.println s!"Error: {e}"
```

### Multi-turn conversation
```lean
let messages := #[
  Oracle.Message.system "You are helpful.",
  Oracle.Message.user "Question",
  Oracle.Message.assistant "Previous response",
  Oracle.Message.user "Follow-up"
]
match ← client.complete messages with
| .ok response => ...
```

### Streaming
```lean
match ← client.promptStream "Tell me a story" with
| .ok content => IO.println content
| .error e => ...
```

### Tool calling
```lean
let tool := Oracle.Tool.create "get_weather" (some "Get weather") schema
match ← client.chatWithTools messages #[tool] with
| .ok response =>
  if response.hasToolCalls then
    -- Handle tool calls
```

### Custom config
```lean
let config : Oracle.Config := {
  apiKey := "sk-or-..."
  model := "anthropic/claude-3.5-sonnet"
  timeout := 120000
}
let client := Oracle.Client.new config
```

## Integration Tests

Integration tests require a live API key and are skipped by default. To run them:

1. Create `.env` with `OPENROUTER_API_KEY=your-key`
2. Run `./run-integration-tests.sh`

The tests use `OPENROUTER_RUN_INTEGRATION=1` to enable live API calls.

## Curl Link Args

This project requires curl. The lakefile includes platform-specific link args for libcurl. On macOS, it looks in `/opt/homebrew/lib` and `/usr/local/lib`.
