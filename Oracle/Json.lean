/-
  Oracle - JSON Utilities
  Helper functions for JSON serialization
-/

import Lean.Data.Json

namespace Oracle

namespace Json

open Lean Json

/-- Build a JSON object from a list of fields with optional values.
    Fields with `none` values are omitted from the output. -/
def withOptionalFields (fields : List (String × Option Json)) : Json :=
  Json.mkObj (fields.filterMap fun (k, v?) => v?.map (k, ·))

end Json

end Oracle
