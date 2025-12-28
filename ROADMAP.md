# Oracle Library Roadmap

This document outlines improvements, new features, and code cleanup opportunities for the Oracle OpenRouter client library.

## Current State Summary

The Oracle library provides a full-featured OpenRouter API client with:
- Synchronous and streaming chat completions
- Tool/function calling support
- Multimodal/vision support for images (URL and base64)
- Automatic retry with exponential backoff and jitter
- Complete JSON serialization with roundtrip support
- Model discovery and metadata querying
- Generation statistics for cost tracking
- Extended sampling parameters (top_k, min_p, logprobs, etc.)
- Integration with the wisp HTTP client library

**Files:**
- `Oracle/Core/Types.lean` - Message, Role, ToolCall, FunctionCall, ImageSource, ContentPart, MessageContent types
- `Oracle/Core/Config.lean` - Client configuration with endpoint helpers
- `Oracle/Core/Error.lean` - Error types and OracleResult
- `Oracle/Core/Tool.lean` - Tool/function definitions
- `Oracle/Json.lean` - JSON utilities (withOptionalFields helper)
- `Oracle/Retry.lean` - Retry configuration and exponential backoff
- `Oracle/Request/ChatRequest.lean` - Chat request builder with all parameters
- `Oracle/Response/ChatResponse.lean` - Response parsing
- `Oracle/Response/Delta.lean` - Streaming delta types
- `Oracle/Response/Usage.lean` - Token usage tracking
- `Oracle/Response/GenerationStats.lean` - Generation statistics for cost tracking
- `Oracle/Response/Model.lean` - Model metadata and filtering
- `Oracle/Client/Sync.lean` - Synchronous client with retry support
- `Oracle/Client/Stream.lean` - Streaming client
- `Tests/Main.lean` - Test suite (52 tests)

---

## Feature Proposals

### [Priority: Medium] Provider Routing Options

**Description:** Add support for OpenRouter's provider selection and routing features:
- Model variants (`:free`, `:online`, `:nitro`, `:floor`)
- Provider preferences and ordering
- Fallback configuration

**Rationale:** OpenRouter's multi-provider routing is a key feature that differentiates it from single-provider APIs.

**Affected Files:**
- New file: `Oracle/Provider.lean` - Provider preference types
- `Oracle/Request/ChatRequest.lean` - Add provider fields

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Assistant Prefill

**Description:** Support starting the assistant's response with a specific prefix (assistant prefill).

**Rationale:** This is useful for guiding model responses, especially for structured output or specific formatting requirements.

**Affected Files:**
- `Oracle/Request/ChatRequest.lean` - Add prefill support to message handling
- `Oracle/Core/Types.lean` - Document prefill pattern

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] Conversation Builder

**Description:** Add a higher-level conversation builder that manages message history, system prompts, and context.

**Rationale:** Simplifies multi-turn conversations and reduces boilerplate for common patterns.

**Affected Files:**
- New file: `Oracle/Conversation.lean` - Conversation monad and builder

**Estimated Effort:** Medium

**Dependencies:** None

---

### ~~[Priority: Medium] Image/Vision Support~~ ✅ COMPLETED (v0.4.0)

**Description:** Add support for multimodal messages with image content (base64 or URL).

**Implementation:**
- Added `ImageSource` type with `.url` and `.base64` variants
- Added `ContentPart` type with `.text` and `.image` variants
- Added `MessageContent` type with `.string` and `.parts` variants
- Updated `Message.content` from `String` to `MessageContent`
- Added helper constructors: `userWithImageUrl`, `userWithImageUrls`, `userWithBase64Image`, `userWithImages`
- Added JSON serialization for all new types with roundtrip support
- 18 new tests for vision/multimodal support

**Affected Files:**
- `Oracle/Core/Types.lean` - New types and message helpers
- `Oracle/Request/ChatRequest.lean` - JSON instances for ContentPart, MessageContent
- `Oracle/Response/ChatResponse.lean` - Updated to handle MessageContent
- `Oracle/Client/Stream.lean` - Updated message preview logging
- `Tests/Main.lean` - Vision test suite

---

### [Priority: Low] Embeddings Endpoint

**Description:** Add support for text embeddings via OpenRouter (if/when supported).

**Rationale:** Would enable semantic search and RAG applications.

**Affected Files:**
- New file: `Oracle/Embeddings.lean`
- `Oracle/Core/Config.lean` - Add embeddings endpoint

**Estimated Effort:** Medium

**Dependencies:** OpenRouter embeddings API availability

---

### [Priority: Low] Response Caching

**Description:** Optional response caching layer for identical requests with deterministic parameters.

**Rationale:** Reduces API costs for repeated queries, especially useful during development.

**Affected Files:**
- New file: `Oracle/Cache.lean` - Cache configuration and logic
- Could integrate with cellar library

**Estimated Effort:** Medium

**Dependencies:** cellar library (optional)

---

## Code Improvements

### [Priority: Medium] Extract HTTP Error Handling

**Current State:** Error handling logic is duplicated between `Client.chat` (lines 81-109) and `Client.chatStream` (lines 105-130).

**Proposed Change:** Extract shared error handling into a common function.

**Benefits:** DRY principle, consistent error handling, easier maintenance.

**Affected Files:**
- `Oracle/Client/Sync.lean`
- `Oracle/Client/Stream.lean`
- Consider new file: `Oracle/Client/Common.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Add Timeout to Streaming Reads

**Current State:** Streaming chunks are read without individual timeouts - only the initial connection has a timeout.

**Proposed Change:** Add configurable timeout for individual chunk reads to detect stalled streams.

**Benefits:** Better handling of network issues during long streaming responses.

**Affected Files:**
- `Oracle/Client/Stream.lean`
- `Oracle/Core/Config.lean` - Add `streamChunkTimeout` option

**Estimated Effort:** Medium

---

### [Priority: Medium] Streaming Tool Call Accumulation

**Current State:** `StreamChunk` parses tool call deltas individually but there's no built-in way to accumulate them into complete tool calls.

**Proposed Change:** Add a `StreamAccumulator` that collects deltas and provides complete tool calls when finished.

**Benefits:** Proper tool calling support in streaming mode.

**Affected Files:**
- `Oracle/Response/Delta.lean`
- `Oracle/Client/Stream.lean` - Add accumulator-based methods

**Estimated Effort:** Medium

---

### [Priority: Medium] Type-Safe Model Names

**Current State:** Model names are plain strings, allowing typos and invalid model IDs.

**Proposed Change:** Create a `ModelId` type with common model constants and validation.

**Benefits:** Compile-time validation for common models, better documentation.

**Affected Files:**
- New file: `Oracle/Models.lean` - ModelId type and constants
- `Oracle/Core/Config.lean` - Use ModelId

**Estimated Effort:** Small

---

### [Priority: Low] Add Deriving Handlers for Custom Types

**Current State:** Some types manually implement `Repr` or lack useful instances.

**Proposed Change:** Add deriving clauses for `Hashable`, `Ord` where appropriate.

**Benefits:** Enables use in maps/sets, sorting.

**Affected Files:**
- `Oracle/Core/Types.lean`
- `Oracle/Core/Error.lean`

**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: High] Add Documentation Comments

**Issue:** Most functions lack doc comments explaining parameters, return values, and usage examples.

**Location:** All files

**Action Required:**
1. Add `/-- ... -/` doc comments to all public functions
2. Add usage examples in module-level comments
3. Document error conditions and edge cases

**Estimated Effort:** Medium

---

### [Priority: Medium] Missing Tests for Response Parsing

**Issue:** Test suite covers JSON serialization and request building but lacks comprehensive response parsing tests, especially for:
- `ChatResponse` parsing with tool calls
- `StreamChunk` parsing
- Error response parsing
- Edge cases (empty content, null fields)

**Location:** `Tests/Main.lean`

**Action Required:**
1. Add response parsing test suite with sample JSON payloads
2. Add streaming delta parsing tests
3. Add error parsing tests
4. Add edge case tests

**Estimated Effort:** Medium

---

### [Priority: Medium] Remove Unused `name` Field from Message ToJson

**Issue:** The `name` field is included in `Message.ToJson` but rarely used by OpenRouter API.

**Location:** `Oracle/Request/ChatRequest.lean` lines 149-152

**Action Required:** Verify if `name` field is needed and document its purpose, or remove if unused.

**Estimated Effort:** Small

---

### [Priority: Medium] Consistent Use of Option vs Default Values

**Issue:** Some types use `Option` with `none` default, others use default values directly. For example:
- `ToolFunction.strict : Option Bool := none` vs could be `Bool := false`
- `Config.timeout : UInt64 := 60000` vs could be `Option UInt64`

**Location:** Multiple files

**Action Required:** Establish and document a convention for when to use `Option` vs default values.

**Estimated Effort:** Small

---

### [Priority: Low] String Helper Duplication

**Issue:** `String.containsSubstr` is defined locally in tests (line 12) but could be moved to a shared utility.

**Location:** `Tests/Main.lean` line 12

**Action Required:** Move to a shared utilities module or use staple library if it provides this.

**Estimated Effort:** Small

---

### [Priority: Low] Consider Moving curl Link Args

**Issue:** `curlLinkArgs` in `lakefile.lean` duplicates configuration from wisp.

**Location:** `lakefile.lean` lines 13-25

**Action Required:** Investigate if this can be inherited from wisp dependency or centralized.

**Estimated Effort:** Small

---

### [Priority: Low] Add Integration Test Examples

**Issue:** No integration tests that actually call the OpenRouter API (even if skipped by default).

**Location:** `Tests/Main.lean`

**Action Required:**
1. Add integration test file with real API calls
2. Guard behind environment variable check
3. Add examples for common use cases

**Estimated Effort:** Medium

---

## Architecture Considerations

### Module Organization

The current organization is clean but could benefit from:
1. Separating JSON instances into dedicated files for cleaner imports
2. Adding a `Oracle/Client.lean` that re-exports both Sync and Stream
3. Consider `Oracle/DSL.lean` for builder syntax if API grows

### API Consistency

Ensure consistent patterns:
- All builder methods should return the modified type (fluent interface)
- All async operations should return `OracleResult`
- Streaming operations should have both callback and collect variants

### Integration Points

Consider deeper integration with:
- **chronicle** - Add logging hooks for request/response tracing
- **cellar** - Response caching layer
- **protolean** - Structured output schema validation

---

## Version History

- **v0.4.0** - Multimodal/Vision support
  - Added `ImageSource` type for URL and base64 image sources
  - Added `ContentPart` type for text and image content parts
  - Added `MessageContent` type to support both string and multipart content
  - Updated `Message.content` from `String` to `MessageContent` (backward compatible)
  - Added helper constructors: `userWithImageUrl`, `userWithImageUrls`, `userWithBase64Image`, `userWithImages`
  - Full JSON roundtrip support for multimodal messages
- **v0.3.0** - Models endpoint, automatic retry, extended parameters
  - Added `GET /api/v1/models` endpoint with `listModels`, `getModel` methods
  - Added `Oracle/Retry.lean` with exponential backoff and jitter
  - Added retry-enabled methods: `chatWithRetry`, `promptWithRetry`, etc.
  - Added new request parameters: `top_k`, `repetition_penalty`, `min_p`, `top_a`, `logit_bias`, `logprobs`, `top_logprobs`, `parallel_tool_calls`
  - Added `Model`, `ModelPricing`, `ModelsResponse` types with filtering helpers
- **v0.2.0** - JSON improvements, generation statistics
  - Added `FromJson Message` for proper roundtrip serialization
  - Added `Json.withOptionalFields` helper to reduce boilerplate
  - Refactored `Message.ToJson`, `ToolFunction.ToJson`, `ChatRequest.ToJson` to use helper
  - Added `GET /api/v1/generation` endpoint with `getGeneration` method
  - Added `GenerationStats` type for token accounting
- **v0.1.0** - Initial implementation with sync/streaming chat completions, tool calling
