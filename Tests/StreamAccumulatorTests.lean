/-
  Stream accumulator tests
-/

import Crucible
import Oracle

namespace Tests.StreamAccumulatorTests

open Crucible
open Oracle

testSuite "Stream Accumulator"

test "mergeChunk accumulates tool call deltas" := do
  let delta1 : ToolCallDelta := {
    index := 0
    id := some "call_1"
    type := some "function"
    function := some { name := some "search", arguments := some "{\"query\":\"" }
  }
  let delta2 : ToolCallDelta := {
    index := 0
    function := some { arguments := some "test\"}" }
  }

  let chunk1 : StreamChunk := {
    id := "chunk_1"
    choices := #[{
      index := 0
      delta := { toolCalls := some #[delta1] }
    }]
  }
  let chunk2 : StreamChunk := {
    id := "chunk_1"
    choices := #[{
      index := 0
      delta := { toolCalls := some #[delta2] }
      finishReason := some "stop"
    }]
  }

  let state1 := (StreamAccumulator.empty).mergeChunk chunk1
  let state2 := state1.mergeChunk chunk2

  shouldBe state2.finished true
  shouldBe state2.finishReason (some "stop")

  let calls := state2.completedToolCalls
  shouldBe calls.size 1
  shouldBe calls[0]!.id "call_1"
  shouldBe calls[0]!.function.name "search"
  shouldBe calls[0]!.function.arguments "{\"query\":\"test\"}"

end Tests.StreamAccumulatorTests
