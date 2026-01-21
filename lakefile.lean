import Lake
open Lake DSL System

package oracle where
  version := v!"0.1.0"
  precompileModules := true

require wisp from git "https://github.com/nathanial/wisp" @ "v0.0.1"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.8"
require chronicle from git "https://github.com/nathanial/chronicle" @ "v0.0.1"

-- Curl link args (inherited from wisp, but needed for our executables)
def curlLinkArgs : Array String :=
  if Platform.isOSX then
    #["-L/opt/homebrew/lib",
      "-L/usr/local/lib",
      "-L/opt/homebrew/anaconda3/lib",
      "-lcurl",
      "-Wl,-rpath,/opt/homebrew/lib",
      "-Wl,-rpath,/opt/homebrew/anaconda3/lib",
      "-Wl,-rpath,/usr/local/lib"]
  else if Platform.isWindows then
    #["-lcurl"]
  else
    #["-lcurl", "-Wl,-rpath,/usr/lib", "-Wl,-rpath,/usr/local/lib"]

@[default_target]
lean_lib Oracle where
  roots := #[`Oracle]
  moreLinkArgs := curlLinkArgs

lean_lib Tests where
  roots := #[`Tests]
  moreLinkArgs := curlLinkArgs

@[test_driver]
lean_exe oracle_tests where
  root := `Tests.Main
  moreLinkArgs := curlLinkArgs
