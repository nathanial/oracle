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
import Tests.AgentTests
import Tests.AgentTypesTests
import Tests.MockClientTests
import Tests.AgentResultTests

open Crucible

def main : IO UInt32 := runAllSuites
