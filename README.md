# Oracle

A type-safe OpenRouter API client library for Lean 4.

## Features

- **Chat Completions** - Send messages and receive AI responses
- **Streaming** - Real-time token streaming via Server-Sent Events (SSE)
- **Tool Calling** - Function/tool calling support for agentic workflows
- **Type Safety** - Full Lean type checking with `ToJson`/`FromJson` instances
- **Error Handling** - Comprehensive error types with retry detection

## Installation

Add to your `lakefile.lean`:

```lean
require oracle from git "https://github.com/nathanial/oracle" @ "v0.0.1"
```

## Quick Start

```lean
import Oracle

def main : IO Unit := do
  -- Create a client with your API key
  let client := Oracle.Client.withApiKey (← IO.getEnv "OPENROUTER_API_KEY" |>.map (·.getD ""))

  -- Simple prompt
  match ← client.prompt "What is the capital of France?" with
  | .ok response => IO.println response
  | .error e => IO.println s!"Error: {e}"
```

## Usage

### Basic Chat Completion

```lean
import Oracle

def example : IO Unit := do
  let client := Oracle.Client.withApiKey "sk-or-..."

  -- Single prompt
  match ← client.prompt "Hello!" with
  | .ok response => IO.println response
  | .error e => IO.println s!"Error: {e}"

  -- With system prompt
  match ← client.prompt "What's 2+2?" (systemPrompt := some "You are a math tutor.") with
  | .ok response => IO.println response
  | .error e => IO.println s!"Error: {e}"
```

### Multi-turn Conversation

```lean
import Oracle

def conversation : IO Unit := do
  let client := Oracle.Client.withApiKey "sk-or-..."

  let messages := #[
    Oracle.Message.system "You are a helpful assistant.",
    Oracle.Message.user "What's the weather like?",
    Oracle.Message.assistant "I don't have access to real-time weather data.",
    Oracle.Message.user "Then tell me a joke instead."
  ]

  match ← client.complete messages with
  | .ok response => IO.println response
  | .error e => IO.println s!"Error: {e}"
```

### Streaming Responses

```lean
import Oracle

def streaming : IO Unit := do
  let client := Oracle.Client.withApiKey "sk-or-..."

  -- Stream and print tokens as they arrive
  match ← client.promptStream "Tell me a story" with
  | .ok content => IO.println s!"Final content: {content}"
  | .error e => IO.println s!"Error: {e}"

  -- Or handle the stream manually
  match ← client.completeStream #[Oracle.Message.user "Count to 10"] with
  | .ok stream =>
    stream.forEach fun chunk =>
      if let some content := chunk.content then
        IO.print content
    IO.println ""
  | .error e => IO.println s!"Error: {e}"
```

### Tool/Function Calling

```lean
import Oracle

def toolCalling : IO Unit := do
  let client := Oracle.Client.withApiKey "sk-or-..."

  -- Define a tool
  let weatherTool := Oracle.Tool.create
    "get_weather"
    (some "Get the current weather for a location")
    (some (Lean.Json.mkObj [
      ("type", "object"),
      ("properties", Lean.Json.mkObj [
        ("location", Lean.Json.mkObj [
          ("type", "string"),
          ("description", "City name")
        ])
      ]),
      ("required", Lean.Json.arr #["location"])
    ]))

  let messages := #[Oracle.Message.user "What's the weather in Paris?"]

  match ← client.chatWithTools messages #[weatherTool] with
  | .ok response =>
    if response.hasToolCalls then
      match response.toolCalls with
      | some calls =>
        for call in calls do
          IO.println s!"Tool: {call.function.name}"
          IO.println s!"Args: {call.function.arguments}"
      | none => pure ()
    else
      IO.println s!"Response: {response.content.getD ""}"
  | .error e => IO.println s!"Error: {e}"
```

### Custom Configuration

```lean
import Oracle

def customConfig : IO Unit := do
  let config : Oracle.Config := {
    apiKey := "sk-or-..."
    model := "anthropic/claude-3.5-sonnet"
    baseUrl := "https://openrouter.ai/api/v1"
    siteUrl := some "https://myapp.com"
    siteName := some "My Application"
    timeout := 120000  -- 2 minutes
  }

  let client := Oracle.Client.new config

  let req := Oracle.ChatRequest.simple config.model "Hello!"
    |>.withTemperature 0.7
    |>.withMaxTokens 1000

  match ← client.chat req with
  | .ok response => IO.println s!"{response.content.getD ""}"
  | .error e => IO.println s!"Error: {e}"
```

## API Reference

### Types

#### `Oracle.Message`
```lean
structure Message where
  role : Role
  content : String
  name : Option String := none
  toolCallId : Option String := none
  toolCalls : Option (Array ToolCall) := none
```

Constructors:
- `Message.system : String → Message`
- `Message.user : String → Message`
- `Message.assistant : String → Message`
- `Message.toolResponse : String → String → Message`

#### `Oracle.Config`
```lean
structure Config where
  apiKey : String
  model : String := "anthropic/claude-sonnet-4"
  baseUrl : String := "https://openrouter.ai/api/v1"
  siteUrl : Option String := none
  siteName : Option String := none
  timeout : UInt64 := 60000
```

#### `Oracle.OracleError`
```lean
inductive OracleError where
  | httpError (status : UInt32) (message : String)
  | parseError (message : String)
  | networkError (message : String)
  | authError (message : String)
  | rateLimitError (retryAfter : Option Nat)
  | apiError (code : String) (message : String)
  | timeoutError (message : String)
```

### Client Methods

| Method | Description |
|--------|-------------|
| `Client.new` | Create client with full config |
| `Client.withApiKey` | Create client with just API key |
| `Client.withModel` | Create client with API key and model |
| `Client.chat` | Send a chat request, get full response |
| `Client.complete` | Send messages, get content string |
| `Client.prompt` | Send single prompt, get response |
| `Client.chatStream` | Send request, get streaming response |
| `Client.completeStream` | Send messages, get stream |
| `Client.promptStream` | Stream single prompt |
| `Client.chatWithTools` | Chat with tool calling |

## Supported Models

Oracle works with any model available on OpenRouter. Some popular options:

- `anthropic/claude-sonnet-4` (default)
- `anthropic/claude-3.5-sonnet`
- `anthropic/claude-3-opus`
- `openai/gpt-4o`
- `openai/gpt-4-turbo`
- `google/gemini-pro-1.5`
- `meta-llama/llama-3.1-70b-instruct`

See [OpenRouter Models](https://openrouter.ai/models) for the full list.

## Building

```bash
lake build        # Build the library
lake test         # Run tests
```

## Dependencies

- [wisp](../wisp) - HTTP client library
- [crucible](../crucible) - Test framework

## License

MIT License - see [LICENSE](LICENSE) for details.
