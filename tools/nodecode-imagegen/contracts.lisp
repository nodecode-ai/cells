;;;; contracts.lisp --- generate_image's arguments, one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-imagegen)

(nlk:define-contract image-request
  (:doc "Generate one image, or edit the images given, through the operator's OpenAI Images-compatible gateway. The answer carries the path of the file it wrote: say where it is, and what the picture shows. Write a prompt the way an image model reads it - subject, setting, style, light, framing - not as a question. To edit, pass the pictures in images (local file paths or https URLs); the same prompt then says what to change, e.g. \"make the sky dusk, keep the fox\".")
  ("prompt" :text :required t
   :doc "what to draw, or what to change when images are given")
  ("aspect" :string :default "square" :options '("landscape" "square" "portrait")
   :doc "the shape of the picture: landscape (1536x1024), square (1024x1024) or portrait (1024x1536). Ignored by a gateway-defined model, which picks its own size")
  ("model" :string :default ""
   :doc "the model id the gateway serves, when it should differ from the operator's setting (a Gemini image model behind the gateway, Flux, gpt-image-1...). On the GPT image models a quality may ride the name: gpt-image-2-high, gpt-image-2.5-sunburst-low")
  ("images" :array :of :string
   :doc "source images to edit: absolute local paths, or https URLs. One image is an edit of that picture; up to 16 are sent together. Omit for a fresh generation"))
