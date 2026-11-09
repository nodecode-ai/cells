;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-gemini-media)

(nlk:define-contract image-request
  (:doc "Generate one image with a Gemini image model (Nano Banana), or guide one with reference pictures. The answer carries the path of the file it wrote: say where it is, and what the picture shows. Write a prompt the way an image model reads it - subject, setting, style, light, framing - not as a question. Pass pictures in images to edit one, keep a character consistent, or combine products; the prompt then says what to make of them, e.g. \"the same fox at dusk, keep its markings\".")
  ("prompt" :text :required t
   :doc "what to draw, or what to make of the pictures given")
  ("aspect" :string :default "square" :options '("landscape" "square" "portrait")
   :doc "the shape: landscape (16:9), square (1:1) or portrait (9:16)")
  ("image_size" :string :default "auto" :options '("auto" "1K" "2K" "4K")
   :doc "how large: 1K, 2K or 4K. auto takes the model's own default, and a model that only renders 1K is sent 1K whatever is asked")
  ("model" :string :default ""
   :doc "a Gemini image model id, when it should differ from the cell's image_model: gemini-nano-banana-2.1, gemini-3-pro-image, gemini-3.1-flash-image, gemini-3.1-flash-lite-image, gemini-2.5-flash-image. Call gemini_models to see what the key offers")
  ("images" :array :of :string
   :doc "reference pictures: local file paths, https URLs, or data: URIs. Up to 14 are sent, PNG, JPEG, WebP, HEIC or HEIF, 15 MB each. Omit for a fresh image"))

(nlk:define-contract video-request
  (:doc "Generate one video with a Gemini video model (Omni). The answer carries the path of the file it wrote: say where it is, how long it runs and what happens in it. A video takes from tens of seconds to a few minutes, so tell the operator it is rendering before you call. With image the picture given becomes the first frame; with images the character or scene in them is what moves.")
  ("prompt" :text :required t
   :doc "what happens: subject, action, camera move, setting, light")
  ("aspect" :string :default "landscape" :options '("landscape" "portrait")
   :doc "landscape (16:9) or portrait (9:16); Gemini video renders no other shape")
  ("resolution" :string :default "720p" :options '("360p" "720p" "1080p" "4k")
   :doc "the output size; a value between two supported ones is rounded up")
  ("duration" :integer :default 8 :doc "seconds of video, 3 to 10; a value outside that window is clamped into it, and the API renders no other length")
  ("model" :string :default ""
   :doc "a Gemini video model id, when it should differ from the cell's video_model: gemini-omni-1.1-flash is the default")
  ("image" :string
   :doc "a first frame: a local file path, an https URL, or a data: URI")
  ("images" :array :of :string
   :doc "reference pictures for the character or scene, up to 6 in all, counting image. Omit for text-to-video"))

(nlk:define-contract model-list-request
  (:doc "List the Gemini image and video models this key can call, as the live model list reports them, with the built-in list behind it when the API cannot be reached. Call it before choosing a model for gemini_image or gemini_video.")
  ("kind" :string :default "all" :options '("all" "image" "video")
   :doc "which models to list"))
