/-
  Vision/multimodal tests
-/

import Crucible
import Oracle

namespace Tests.VisionTests

open Crucible
open Oracle

testSuite "Vision/Multimodal Support"

test "ImageSource.url toDataUrl returns URL unchanged" := do
  let src := ImageSource.url "https://example.com/image.jpg"
  shouldBe src.toDataUrl "https://example.com/image.jpg"

test "ImageSource.base64 toDataUrl creates data URL" := do
  let src := ImageSource.base64 "image/png" "iVBORw0KGgo="
  shouldBe src.toDataUrl "data:image/png;base64,iVBORw0KGgo="

test "MessageContent.string asString returns content" := do
  let content : MessageContent := .string "Hello"
  shouldBe content.asString "Hello"

test "MessageContent.parts asString extracts text parts" := do
  let content : MessageContent := .parts #[
    .text "First",
    .image (.url "https://example.com/img.jpg"),
    .text " Second"
  ]
  shouldBe content.asString "First Second"

test "Message.userWithImageUrl creates multimodal message" := do
  let msg := Message.userWithImageUrl "Describe this:" "https://example.com/cat.jpg"
  shouldBe msg.role Role.user
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 2
    match ps[0]! with
    | .text t => shouldBe t "Describe this:"
    | _ => throw (IO.userError "Expected text part first")
    match ps[1]! with
    | .image src detail =>
      shouldBe src.toDataUrl "https://example.com/cat.jpg"
      shouldBe detail "auto"
    | _ => throw (IO.userError "Expected image part second")
  | .string _ => throw (IO.userError "Expected parts content")

test "Message.userWithImageUrls creates message with multiple images" := do
  let msg := Message.userWithImageUrls "Compare:" #["url1", "url2"] "high"
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 3
  | .string _ => throw (IO.userError "Expected parts content")

test "Message.userWithBase64Image creates message with base64 image" := do
  let msg := Message.userWithBase64Image "Analyze:" "image/png" "iVBOR=" "low"
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 2
    match ps[1]! with
    | .image src detail =>
      shouldBe src.toDataUrl "data:image/png;base64,iVBOR="
      shouldBe detail "low"
    | _ => throw (IO.userError "Expected image part")
  | .string _ => throw (IO.userError "Expected parts content")

test "Message.userWithImages accepts mixed sources" := do
  let msg := Message.userWithImages "Mixed:" #[
    .url "https://example.com/img.jpg",
    .base64 "image/jpeg" "abc123"
  ]
  match msg.content with
  | .parts ps =>
    shouldBe ps.size 3
  | .string _ => throw (IO.userError "Expected parts content")

test "ContentPart.text toJson serializes correctly" := do
  let part : ContentPart := .text "Hello"
  let json := Lean.toJson part
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "text"
  | .error _ => throw (IO.userError "Missing type field")
  match json.getObjValAs? String "text" with
  | .ok txt => shouldBe txt "Hello"
  | .error _ => throw (IO.userError "Missing text field")

test "ContentPart.image toJson serializes with image_url" := do
  let part : ContentPart := .image (.url "https://example.com/img.jpg") "high"
  let json := Lean.toJson part
  match json.getObjValAs? String "type" with
  | .ok t => shouldBe t "image_url"
  | .error _ => throw (IO.userError "Missing type field")
  match json.getObjVal? "image_url" with
  | .ok imgObj =>
    match imgObj.getObjValAs? String "url" with
    | .ok url => shouldBe url "https://example.com/img.jpg"
    | .error _ => throw (IO.userError "Missing url field")
    match imgObj.getObjValAs? String "detail" with
    | .ok d => shouldBe d "high"
    | .error _ => throw (IO.userError "Missing detail field")
  | .error _ => throw (IO.userError "Missing image_url field")

test "ContentPart fromJson parses text part" := do
  let json := Lean.Json.mkObj [
    ("type", Lean.Json.str "text"),
    ("text", Lean.Json.str "Hello world")
  ]
  match Lean.fromJson? json with
  | .ok (part : ContentPart) =>
    match part with
    | .text t => shouldBe t "Hello world"
    | .image _ _ => throw (IO.userError "Expected text part")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "ContentPart fromJson parses image_url part" := do
  let json := Lean.Json.mkObj [
    ("type", Lean.Json.str "image_url"),
    ("image_url", Lean.Json.mkObj [
      ("url", Lean.Json.str "https://example.com/img.png"),
      ("detail", Lean.Json.str "low")
    ])
  ]
  match Lean.fromJson? json with
  | .ok (part : ContentPart) =>
    match part with
    | .image src detail =>
      shouldBe src.toDataUrl "https://example.com/img.png"
      shouldBe detail "low"
    | .text _ => throw (IO.userError "Expected image part")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "ContentPart fromJson parses base64 data URL" := do
  let json := Lean.Json.mkObj [
    ("type", Lean.Json.str "image_url"),
    ("image_url", Lean.Json.mkObj [
      ("url", Lean.Json.str "data:image/png;base64,iVBORw0KGgo="),
      ("detail", Lean.Json.str "auto")
    ])
  ]
  match Lean.fromJson? json with
  | .ok (part : ContentPart) =>
    match part with
    | .image src _ =>
      match src with
      | .base64 mediaType data =>
        shouldBe mediaType "image/png"
        shouldBe data "iVBORw0KGgo="
      | .url _ => throw (IO.userError "Expected base64 source")
    | .text _ => throw (IO.userError "Expected image part")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "MessageContent.string toJson produces string" := do
  let content : MessageContent := .string "Hello"
  let json := Lean.toJson content
  match json.getStr? with
  | .ok s => shouldBe s "Hello"
  | .error _ => throw (IO.userError "Expected string JSON")

test "MessageContent.parts toJson produces array" := do
  let content : MessageContent := .parts #[.text "Hi"]
  let json := Lean.toJson content
  match json.getArr? with
  | .ok arr => shouldBe arr.size 1
  | .error _ => throw (IO.userError "Expected array JSON")

test "MessageContent fromJson parses string" := do
  let json := Lean.Json.str "Hello"
  match Lean.fromJson? json with
  | .ok (content : MessageContent) =>
    shouldBe content.asString "Hello"
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "MessageContent fromJson parses array of parts" := do
  let json := Lean.Json.arr #[(
    Lean.Json.mkObj [("type", Lean.Json.str "text"), ("text", Lean.Json.str "Hello")]
  )]
  match Lean.fromJson? json with
  | .ok (content : MessageContent) =>
    match content with
    | .parts ps => shouldBe ps.size 1
    | .string _ => throw (IO.userError "Expected parts")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

test "Message with multimodal content roundtrips" := do
  let msg := Message.userWithImageUrl "Describe:" "https://example.com/cat.jpg"
  let json := Lean.toJson msg
  match Lean.fromJson? json with
  | .ok (parsed : Message) =>
    shouldBe parsed.role msg.role
    match parsed.content with
    | .parts ps => shouldBe ps.size 2
    | .string _ => throw (IO.userError "Expected parts after roundtrip")
  | .error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.VisionTests
