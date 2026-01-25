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
import Tests.AgentTests
import Tests.AgentTypesTests
import Tests.MockClientTests
import Tests.AgentResultTests
import Tests.StreamAccumulatorTests

open Crucible

def main : IO UInt32 := runAllSuites
