/-
  Oracle - Configuration
  Client configuration for OpenRouter API
-/

import Chronicle

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
  /-- Optional logger for request/response logging -/
  logger : Option Chronicle.Logger := none

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

/-- Set the logger for request/response logging -/
def setLogger (c : Config) (logger : Chronicle.Logger) : Config :=
  { c with logger := some logger }

/-- Get the chat completions endpoint URL -/
def chatEndpoint (c : Config) : String :=
  s!"{c.baseUrl}/chat/completions"

/-- Get the generation statistics endpoint URL -/
def generationEndpoint (c : Config) (generationId : String) : String :=
  s!"{c.baseUrl}/generation?id={generationId}"

/-- Get the models list endpoint URL -/
def modelsEndpoint (c : Config) : String :=
  s!"{c.baseUrl}/models"

end Config

-- Predefined model identifiers
namespace Models

/-- Google Gemini 2.5 Flash for image generation (Nano Banana) -/
def geminiFlashImage : String := "google/gemini-2.5-flash-image"

def geminiFlash : String := "google/gemini-3-flash-preview"

/-- Google Gemini 3 Pro for image generation (Nano Banana Pro) -/
def geminiProImage : String := "google/gemini-3-pro-image-preview"

/-- Default Claude model -/
def claudeSonnet : String := "anthropic/claude-sonnet-4"

end Models

end Oracle
