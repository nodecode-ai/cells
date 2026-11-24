;;;; contracts.lisp --- the two tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from the declaration, the value the body runs on is
;;;; materialized from it, and a call that does not satisfy it is refused
;;;; before the body sees it.

(in-package #:nodecode-image-studio)

(nlk:define-contract generation-request
  (:doc "Draw a picture with FAL or Vertex AI, using the key the operator set in the image-studio section. The answer carries the path of every file written: say where it is, and what the picture shows. Write the prompt the way an image model reads it - subject, setting, style, light, framing - not as a question. Give references to work from a picture the user already has: the prompt then says what to draw or change, e.g. \"the same fox, at dusk, wide shot\".")
  ("prompt" :text :required t
   :doc "what to draw, or what to make of the references")
  ("provider" :string :default ""
   :doc "fal or vertex. Blank uses the section's provider. Each needs its own key")
  ("model" :string :default ""
   :doc "the provider's model id, when it should differ from the section's model and the default (fal-ai/flux-2-pro on FAL, gemini-nano-banana-2.1 on Vertex AI)")
  ("aspect_ratio" :string :default "1:1"
   :doc "the picture's shape, as width:height: 1:1, 4:5, 3:2, 16:9, 9:16 or 21:9. A model that does not take one picks its own")
  ("resolution" :string :default "1K" :options '("1K" "2K" "4K")
   :doc "1K, 2K or 4K. Vertex AI's Gemini image models take it; FAL models ignore it")
  ("count" :integer :default 1 :slot image-count
   :doc "how many pictures to draw, 1 to 4")
  ("references" :array :of :string
   :doc "pictures to draw from: https URLs on both providers; local paths too on Vertex AI, which sends their bytes. Omit for a fresh generation"))

(nlk:define-contract gallery-request
  (:doc "List the pictures this cell drew, newest first, with the prompt and provider each came from. Ask for it when the user asks what was generated, or to find a picture to work from again.")
  ("limit" :integer :default 10
   :doc "how many entries to list, 1 to 50"))
