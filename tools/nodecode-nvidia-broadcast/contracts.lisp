;;;; contracts.lisp --- the ten tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.
;;;;
;;;; The names, arguments and answer shapes are the original plugin's; only
;;;; the spelling of the arguments is Nodecode's snake_case, mapped onto the
;;;; gateway's camelCase in cell.lisp.

(in-package #:nodecode-nvidia-broadcast)

(nlk:define-contract state-read
  (:doc "Read NVIDIA Broadcast's current effects, as effectId -> {enabled, ...params}, grouped by camera, microphone and speaker. Read-only. A section whose effects are on but whose device is unselected comes back under warnings: configured, not currently applied.")
  ("effect_type" :string :default ""
   :doc "narrow the answer to one section: camera, microphone or speaker. Omit for all three"))

(nlk:define-contract effects-describe
  (:doc "Rich detail on NVIDIA Broadcast's effects on demand: what each one does, its guidance, its GPU and beta badges, its availability, and the parameters it takes with their ranges. Use it to explain effects or weigh a tradeoff; it is not needed to switch one on.")
  ("effect_id" :string :default ""
   :doc "one effect by the id the server instructions list (noise_removal, background_blur, virtual_key_light); omit for every effect")
  ("section" :string :default ""
   :doc "narrow to one section: camera, microphone or speaker"))

(nlk:define-contract effects-set
  (:doc "Turn NVIDIA Broadcast effects on or off and tune them, in ONE call: put every effect you want to change in the effects array, each with the effectId the server instructions list. Disabling an effect that is already off is harmless. Its answer carries the resulting state, so do not follow it with get_broadcast_state just to confirm. Or pass only action \"restore_previous_live\" to restore the Live snapshot that file processing suspended.")
  ("effects" :array :of :object
   :doc "the changes, e.g. [{\"effectId\": \"noise_removal\", \"enabled\": true}, {\"effectId\": \"background_blur\", \"enabled\": true, \"params\": {\"strength\": 0.7}}]. Numeric params are normalized 0-1; one effectId appears at most once per call")
  ("action" :string :default ""
   :doc "the one exception to effects: \"restore_previous_live\" restores the Live snapshot file processing suspended. Send it alone"))

(nlk:define-contract device-list
  (:doc "List the cameras, microphones and speakers NVIDIA Broadcast can use, with the active one, and for cameras the current resolution plus availableResolutions for the mode in effect. Always call it fresh: devices can be plugged or unplugged between turns, and an id from an earlier turn may be gone.")
  ("kind" :string :default "all"
   :doc "camera, microphone, speaker or all (the default)"))

(nlk:define-contract device-set
  (:doc "Switch the active camera, microphone or speaker, with a deviceId from a fresh list_devices. For a camera, add width and height (and frameRate) to switch device and resolution in one call: a camera switch restarts the stream, so one call is faster than two. Report from the answer: success, device, resolution; alreadyActive means nothing changed; success with frameRateMismatch means the size applied but the frame rate differs.")
  ("kind" :text :required t
   :doc "camera, microphone or speaker")
  ("device_id" :text :required t
   :doc "the id list_devices just answered for that device")
  ("width" :integer
   :doc "camera only: the width to switch to together with height, from that camera's availableResolutions")
  ("height" :integer
   :doc "camera only: the height to switch to together with width")
  ("frame_rate" :number
   :doc "camera only: the frame rate, when the size is offered at more than one. Never guess one: ask the user which"))

(nlk:define-contract resolution-set
  (:doc "Change the active camera's resolution: deviceId from a fresh list_devices, with width and height. A size offered at several frame rates needs frameRate too, or the answer comes back requiresUserInput with the options - ask the user which frame rate, never pick one. Switching the camera and its resolution in one set_active_device call is cheaper than this one: use this only when the device stays as it is.")
  ("device_id" :text :required t
   :doc "the active camera's id, from a fresh list_devices")
  ("width" :integer :required t
   :doc "the width, from that camera's availableResolutions")
  ("height" :integer :required t
   :doc "the height, from that camera's availableResolutions")
  ("frame_rate" :number
   :doc "the frame rate, when the size is offered at more than one"))

(nlk:define-contract file-submit
  (:doc "Submit one local media file for NVIDIA Broadcast to process with the given effects. The answer carries an id and a batchId: keep the batchId and send it with every other file of the same batch, then take one snapshot for the whole batch with wait_for_processing_update. There is no overwrite input: a colliding output is rejected, while a retry replaces only that job's own partial output.")
  ("input_path" :text :required t
   :doc "the absolute path of the local media file to process")
  ("effects" :array :of :object :required t
   :doc "the effects to apply, shaped as in set_effects, each with its own params nested under \"params\". Speaker effects do not apply to files")
  ("batch_id" :string :default ""
   :doc "the batchId the batch's first submission returned, to add this file to that batch")
  ("output_folder" :string :default ""
   :doc "the folder to write the result into")
  ("output_path" :string :default ""
   :doc "the exact path to write the result to, instead of output_folder"))

(nlk:define-contract file-jobs
  (:doc "Look up one file-processing job, or list the recent ones, as a snapshot. Use wait_for_processing_update to wait for a job to move: do not poll this in a loop.")
  ("job_ids" :array :of :string
   :doc "the ids to look up; omit to list the recent jobs")
  ("batch_id" :string :default ""
   :doc "narrow to one batch")
  ("limit" :integer
   :doc "the most jobs to answer"))

(nlk:define-contract file-wait
  (:doc "The one way to wait for file processing: it wakes for job-status and queue-pause changes, not progress ticks. Take a batch snapshot without cursor, then pass the cursor the answer returns to wait for the next change. Read status, summary and every job: completed means every file completed, mixed means terminal outcomes differ - mixed is not success.")
  ("batch_id" :string :default ""
   :doc "the batch to watch")
  ("job_ids" :array :of :string
   :doc "the jobs to watch, when they are not one batch")
  ("cursor" :string :default ""
   :doc "the cursor the previous answer returned, to wait for the next change")
  ("timeout_ms" :integer
   :doc "how long this wait may take, in milliseconds"))

(nlk:define-contract file-control
  (:doc "Control NVIDIA Broadcast's file-processing queue: pause or resume it (queue-wide), or cancel or retry jobs by id. Cancellation is final until the user asks again, and a queue the user paused is never resumed without asking them in the conversation. Read the answer's event and jobIds; a retry that answers no_change means there is nothing to retry.")
  ("action" :text :required t
   :doc "pause, resume, cancel or retry")
  ("job_ids" :array :of :string
   :doc "the jobs to cancel or retry")
  ("all" :boolean
   :doc "cancel or retry every job that can be, instead of naming ids"))
