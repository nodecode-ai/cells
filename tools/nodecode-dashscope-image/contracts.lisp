;;;; contracts.lisp --- dashscope_image's arguments, one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-dashscope-image)

(nlk:define-contract image-request
  (:doc "Generate one image, or restyle the one given, with Alibaba's Wan 2.7 models through the operator's DashScope (Qwen Cloud) key. The answer carries the path of the file it wrote: say where it is, and what the picture shows. Write a prompt the way an image model reads it - subject, setting, style, light, framing - not as a question. With images, the same prompt says what to change, e.g. \"make the sky dusk, keep the fox\".")
  ("prompt" :text :required t
   :doc "what to draw, or what to change when a source image is given")
  ("aspect" :string :default "square" :options '("landscape" "square" "portrait")
   :doc "the shape of the picture: landscape (1280x720), square (1024x1024) or portrait (720x1280)")
  ("model" :string :default ""
   :doc "wan2.7-image (fast, text only) or wan2.7-image-pro (higher fidelity, image-to-image), when it should differ from the operator's setting. A source image upgrades wan2.7-image to the pro model on its own")
  ("images" :array :of :string
   :doc "source images for an edit: absolute local paths, https URLs, or data: URIs. Up to 4, each at most 10 MB on disk; a local file is sent base64-inlined. Omit for a fresh generation"))
