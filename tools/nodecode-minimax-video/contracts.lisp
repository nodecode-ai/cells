;;;; contracts.lisp --- generate_video's and video_status's arguments, one
;;;; declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from the declaration, the value the body runs on is
;;;; materialized from it, and a call that does not satisfy it is refused
;;;; before the body sees it.

(in-package #:nodecode-minimax-video)

(nlk:define-contract video-request
  (:doc "Generate one video with MiniMax's Hailuo models. A prompt alone is text-to-video; a first-frame image animates that picture; reference images animate a character from them (S2V-01). The render is a MiniMax task: the answer names the file when it finished inside the call's wait, and the task id to hand to video_status when it did not. Say where the file is; do not paste its bytes. Camera moves are written into the prompt in brackets, as MiniMax reads them: [Push in], [Pan left], [Static shot], and a cut as \"[Push in], then [Pull out]\".")
  ("prompt" :text :required t
   :doc "the shot: subject, action, setting, light, camera. MiniMax renders 6 or 10 seconds of it")
  ("model" :string
   :doc "a MiniMax video model: MiniMax-Hailuo-2.3 (text and image), MiniMax-Hailuo-2.3-Fast (image), MiniMax-Hailuo-02, T2V-01, T2V-01-Director, I2V-01, I2V-01-Director, I2V-01-live, S2V-01 (subject reference). Omit for the operator's setting")
  ("image" :string
   :doc "image-to-video: the first frame, as an https URL, a data URI, or a local file path (sent as a data URI, at most 10 MB). Omit for text-to-video")
  ("reference_images" :array :of :string
   :doc "subject-reference-to-video: the character to animate, as https URLs or local paths. Used only when image is absent")
  ("duration" :integer
   :doc "6 or 10 seconds. Anything above 10 becomes 10 and anything else becomes 6, the only two lengths MiniMax renders")
  ("resolution" :string
   :doc "512P, 720P, 768P or 1080P, when the model takes it. Omit for the operator's setting, else 768P on the Hailuo models and 720P on the older ones")
  ("wait_seconds" :integer
   :doc "how long this call waits for the render before answering the task id: 0 answers as soon as MiniMax accepts the job. Omit for the operator's setting, usually 600"))

(nlk:define-contract video-task
  (:doc "Read one MiniMax video task by the id generate_video answered with. A task that is still rendering answers its state; one that has finished downloads the video and answers its path.")
  ("task_id" :text :required t
   :doc "the task_id generate_video or an earlier video_status answered")
  ("wait_seconds" :integer
   :doc "how long this call may wait for a render that is still running: 0 answers the state at once. Omit for the operator's setting"))
