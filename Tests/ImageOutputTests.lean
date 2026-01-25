/-
  Tests for ImageOutput type and image generation response parsing
-/

import Crucible
import Oracle
import Lean.Data.Json

namespace Tests.ImageOutputTests

open Crucible
open Lean Json
open Oracle

testSuite "ImageOutput"

test "isBase64 returns true for data URLs" := do
  let img : ImageOutput := { url := "data:image/png;base64,iVBORw0KGgo..." }
  shouldBe img.isBase64 true

test "isBase64 returns false for regular URLs" := do
  let img : ImageOutput := { url := "https://example.com/image.png" }
  shouldBe img.isBase64 false

test "mediaType? extracts image/png" := do
  let img : ImageOutput := { url := "data:image/png;base64,iVBORw0KGgo..." }
  shouldBe img.mediaType? (some "image/png")

test "mediaType? extracts image/jpeg" := do
  let img : ImageOutput := { url := "data:image/jpeg;base64,/9j/4AAQ..." }
  shouldBe img.mediaType? (some "image/jpeg")

test "mediaType? returns none for non-data URLs" := do
  let img : ImageOutput := { url := "https://example.com/image.png" }
  shouldBe img.mediaType? none

test "base64Data? extracts base64 content" := do
  let img : ImageOutput := { url := "data:image/png;base64,iVBORw0KGgo" }
  shouldBe img.base64Data? (some "iVBORw0KGgo")

test "base64Data? returns none for non-data URLs" := do
  let img : ImageOutput := { url := "https://example.com/image.png" }
  shouldBe img.base64Data? none

test "toImageSource converts base64 data URL to ImageSource.base64" := do
  let img : ImageOutput := { url := "data:image/png;base64,iVBORw0KGgo" }
  let source := img.toImageSource
  match source with
  | .base64 mediaType data =>
    shouldBe mediaType "image/png"
    shouldBe data "iVBORw0KGgo"
  | .url _ => throw (IO.userError "Expected base64 source")

test "toImageSource converts regular URL to ImageSource.url" := do
  let img : ImageOutput := { url := "https://example.com/image.png" }
  let source := img.toImageSource
  match source with
  | .url u => shouldBe u "https://example.com/image.png"
  | .base64 _ _ => throw (IO.userError "Expected url source")

testSuite "Message Images"

test "hasImages returns false for empty message" := do
  let msg := Message.user "Hello"
  shouldBe msg.hasImages false

test "hasImages returns true when images present" := do
  let msg : Oracle.Message := {
    role := .assistant
    content := .string "Here's your image"
    images := some #[{ url := "data:image/png;base64,abc" }]
  }
  shouldBe msg.hasImages true

test "firstImage? returns first image" := do
  let msg : Oracle.Message := {
    role := .assistant
    content := .string "Here's your image"
    images := some #[
      { url := "data:image/png;base64,first" : ImageOutput },
      { url := "data:image/png;base64,second" : ImageOutput }
    ]
  }
  match msg.firstImage? with
  | some img => shouldBe img.url "data:image/png;base64,first"
  | none => throw (IO.userError "Expected image")

test "firstImage? returns none when no images" := do
  let msg := Message.user "Hello"
  shouldBe msg.firstImage? none

testSuite "ChatResponse Image Parsing"

test "parses images from response JSON" := do
  let json := Json.mkObj [
    ("id", "gen-123"),
    ("model", "flux/schnell"),
    ("choices", Json.arr #[
      Json.mkObj [
        ("index", 0),
        ("message", Json.mkObj [
          ("role", "assistant"),
          ("content", "Here's the image you requested."),
          ("images", Json.arr #[
            Json.mkObj [
              ("type", "image_url"),
              ("image_url", Json.mkObj [
                ("url", "data:image/png;base64,iVBORw0KGgo...")
              ])
            ]
          ])
        ]),
        ("finish_reason", "stop")
      ]
    ])
  ]

  match fromJson? json with
  | .ok (resp : ChatResponse) =>
    shouldBe resp.hasImages true
    match resp.firstImage? with
    | some img =>
      shouldBe img.isBase64 true
      shouldBe img.mediaType? (some "image/png")
    | none => throw (IO.userError "Expected image in response")
  | .error e => throw (IO.userError s!"Parse error: {e}")

test "parses response without images" := do
  let json := Json.mkObj [
    ("id", "chat-123"),
    ("model", "gpt-4"),
    ("choices", Json.arr #[
      Json.mkObj [
        ("index", 0),
        ("message", Json.mkObj [
          ("role", "assistant"),
          ("content", "Hello, how can I help?")
        ]),
        ("finish_reason", "stop")
      ]
    ])
  ]

  match fromJson? json with
  | .ok (resp : ChatResponse) =>
    shouldBe resp.hasImages false
    shouldBe resp.firstImage? none
  | .error e => throw (IO.userError s!"Parse error: {e}")

test "parses multiple images" := do
  let json := Json.mkObj [
    ("id", "gen-456"),
    ("model", "flux/schnell"),
    ("choices", Json.arr #[
      Json.mkObj [
        ("index", 0),
        ("message", Json.mkObj [
          ("role", "assistant"),
          ("content", "Generated images"),
          ("images", Json.arr #[
            Json.mkObj [
              ("type", "image_url"),
              ("image_url", Json.mkObj [("url", "data:image/png;base64,img1")])
            ],
            Json.mkObj [
              ("type", "image_url"),
              ("image_url", Json.mkObj [("url", "data:image/png;base64,img2")])
            ]
          ])
        ]),
        ("finish_reason", "stop")
      ]
    ])
  ]

  match fromJson? json with
  | .ok (resp : ChatResponse) =>
    match resp.images with
    | some imgs =>
      shouldBe imgs.size 2
      shouldBe imgs[0]!.url "data:image/png;base64,img1"
      shouldBe imgs[1]!.url "data:image/png;base64,img2"
    | none => throw (IO.userError "Expected images array")
  | .error e => throw (IO.userError s!"Parse error: {e}")

end Tests.ImageOutputTests
