/-
  Oracle Test Suite
-/

import Crucible
import Tests.JsonTests
import Tests.RequestTests
import Tests.ConfigTests
import Tests.ErrorTests
import Tests.ResponseTests
import Tests.JsonUtilsTests
import Tests.ModelTests
import Tests.RetryTests
import Tests.NewParamsTests
import Tests.EndpointTests
import Tests.VisionTests
import Tests.DeltaTests
import Tests.ChatResponseTests
import Tests.ReactiveTypesTests
import Tests.ChatStreamTests
import Tests.ReactiveIntegrationTests
import Tests.OpenRouterIntegrationTests
import Tests.AgentTests
import Tests.AgentTypesTests
import Tests.MockClientTests
import Tests.AgentResultTests
import Tests.StreamAccumulatorTests
import Tests.ImageOutputTests

open Crucible

def main : IO UInt32 := do
  let result <- runAllSuites
  _ <- Wisp.FFI.globalCleanup
  _ <- Wisp.HTTP.Client.shutdown
  pure result
