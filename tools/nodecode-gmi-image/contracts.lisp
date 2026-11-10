;;;; contracts.lisp --- gmi_image's arguments, one declaration.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-gmi-image)

(nlk:define-contract image-request
  (:doc "Draw one image with GMI Cloud's image models - Seedream, Gemini and the rest of their live catalog - or edit a picture already on the web, through the operator's GMI key. Every call spends GMI credits. The answer carries the path of the file it wrote: say where it is, and what the picture shows. Write a prompt the way an image model reads it - subject, setting, style, light, framing - not as a question. With images, the same prompt says how the subject of those pictures should look in the new one, e.g. \"the same boat, at sunset\".")
  ("prompt" :text :required t
   :doc "what to draw, or how the source pictures should look when images are given")
  ("aspect" :string :default "landscape" :options '("landscape" "square" "portrait")
   :doc "the shape of the picture: landscape (16:9), square (1:1) or portrait (9:16); a model that names fixed sizes gets the nearest one it offers")
  ("model" :string :default ""
   :doc "a GMI model id, when it should differ from the one the cell picks: seedream-5.0-lite, gemini-2.5-flash-image, gpt-image-2-generate or anything else the catalog lists")
  ("images" :array :of :string
   :doc "public http(s) URLs of pictures to edit or to take a subject from: up to 3, and no local paths, because GMI fetches them from its own side"))
