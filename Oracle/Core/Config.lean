/-
  Oracle - Configuration
  Client configuration for OpenRouter API
-/

namespace Oracle

/-- OpenRouter API configuration -/
structure Config where
  /-- OpenRouter API key (required) -/
  apiKey : String
  /-- Default model to use -/
  model : String := "anthropic/claude-sonnet-4"
  /-- Base URL for the API -/
  baseUrl : String := "https://openrouter.ai/api/v1"
  /-- Site URL for HTTP-Referer header (for leaderboard rankings) -/
  siteUrl : Option String := none
  /-- Site name for X-Title header (for leaderboard rankings) -/
  siteName : Option String := none
  /-- Request timeout in milliseconds -/
  timeout : UInt64 := 60000
  deriving Repr, Inhabited

namespace Config

/-- Create a minimal config with just an API key -/
def simple (apiKey : String) : Config :=
  { apiKey := apiKey }

/-- Create a config with API key and model -/
def withModel (apiKey : String) (model : String) : Config :=
  { apiKey := apiKey, model := model }

/-- Create a config from just an API key (alias for simple) -/
def fromApiKey (apiKey : String) : Config :=
  { apiKey := apiKey }

/-- Set the site URL for HTTP-Referer header -/
def setSiteUrl (c : Config) (url : String) : Config :=
  { c with siteUrl := some url }

/-- Set the site name for X-Title header -/
def setSiteName (c : Config) (name : String) : Config :=
  { c with siteName := some name }

/-- Set the request timeout -/
def setTimeout (c : Config) (ms : UInt64) : Config :=
  { c with timeout := ms }

/-- Get the chat completions endpoint URL -/
def chatEndpoint (c : Config) : String :=
  s!"{c.baseUrl}/chat/completions"

end Config

end Oracle
