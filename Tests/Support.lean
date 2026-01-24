/-
  Test support utilities
-/

def String.containsSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1
