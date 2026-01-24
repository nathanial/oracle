/-
  Config endpoint tests
-/

import Crucible
import Oracle

namespace Tests.EndpointTests

open Crucible
open Oracle

testSuite "Config Endpoints"

test "Config.modelsEndpoint returns correct URL" := do
  let cfg := Config.simple "key"
  shouldBe cfg.modelsEndpoint "https://openrouter.ai/api/v1/models"

end Tests.EndpointTests
