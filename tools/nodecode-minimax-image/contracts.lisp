;;;; contracts.lisp --- minimax_image's arguments, one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-minimax-image)

(nlk:define-contract image-request
  (:doc "Draw one image, or restyle the one given, with MiniMax's Image-01 models through the operator's MiniMax key. The answer carries the path of the file it wrote: say where it is, and what the picture shows. Write a prompt the way an image model reads it - subject, setting, style, light, framing - not as a question; 1500 characters is all MiniMax reads. With images, the same prompt says how the subject should look in the new picture, e.g. \"the same fox, in a cyberpunk street at night\".")
  ("prompt" :text :required t
   :doc "what to draw, or how the subject should look when a source image is given")
  ("aspect" :string :default "square" :options '("landscape" "square" "portrait")
   :doc "the shape of the picture: landscape (16:9), square (1:1) or portrait (9:16)")
  ("model" :string :default ""
   :doc "image-01 (text-to-image) or image-01-live (subject reference), when it should differ from the operator's setting. A source image upgrades image-01 to image-01-live on its own")
  ("images" :array :of :string
   :doc "source pictures whose subject is kept: absolute local paths, https URLs, or data: URIs. Local files are base64-inlined, each at most 10 MB. Omit for a fresh generation"))
