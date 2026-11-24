;;;; contracts.lisp --- the tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-remesher)

(defun no-arguments (description)
  "The schema of a tool whose call carries nothing."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:make-json-object))))

(nlk:define-contract remesher-generate
  (:doc "Submit one bounded image-to-GLB job to the configured Remesher CLI and answer its job_id at once. The image is read from disk and, with the comfy3d backend, uploaded to the ComfyUI server the CLI's config names; with native-local the CLI starts a local server of its own and may take the GPU. Poll remesher_job every few seconds, then read the artifacts with remesher_assets. Never replay a job whose outcome is ambiguous.")
  ("image" :text :required t
   :doc "the absolute path of a local PNG, JPEG or WebP image on black, foreground-masked and square-composited (max 20 MiB); the cell does not remove backgrounds")
  ("timeout_seconds" :integer :default 900
   :doc "how long one job may take, from 1 to 1800 seconds; the CLI is killed at the bound")
  ("target_face_num" :integer
   :doc "the simplified face target for a native backend, from 1000 to 200000 (default 80000); omit with a preset or the two conflict")
  ("native_preset" :string :options '("quality-1536-solid768" "fast")
   :doc "override the configured native job recipe: quality-1536-solid768 (1536 shape, solid repair, 4096 atlas, no decimation; expensive) or fast (512 shape, no repair, 1024 atlas, 80000 faces). Native backends only"))

(nlk:define-contract remesher-job-read
  (:doc "Read one image-to-GLB job's persisted state by the job_id remesher_generate answered: queued, running, succeeded, failed, timed_out or interrupted, with the log path and, on success, its artifacts.")
  ("job_id" :text :required t
   :doc "the 32 lowercase hex job id remesher_generate answered"))

(nlk:define-contract remesher-assets-read
  (:doc "List one successful job's GLB artifacts: each path, its size and its SHA-256, so an artifact can be verified where it is consumed. A receipt whose files are gone or changed answers integrity_error and no assets.")
  ("job_id" :text :required t
   :doc "the 32 lowercase hex job id remesher_generate answered"))
