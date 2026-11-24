;;;; contracts.lisp --- the seven tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from the declaration, the value the body runs on is
;;;; materialized from it, and a call that does not satisfy it is refused
;;;; before the body sees it.

(in-package #:nodecode-glorya)

(nlk:define-contract create-request
  (:doc "Start a Glorya job: an image ad, a video ad, a landing page, a website or an ad campaign, made on the person's own Glorya brand. The answer names the job_id and its Glorya link; Glorya works in the background, and glorya_job follows it. A campaign spends real ad money.")
  ("kind" :string :required t
   :options '("image_ad" "video_ad" "landing_page" "website" "campaign")
   :doc "what to make: image_ad, video_ad, landing_page, website or campaign")
  ("brief" :text :required t
   :doc "what to advertise or build, in the person's words: the product, the offer, the audience")
  ("format" :string :slot format-id
   :doc "video_ad only: the video style id from /glorya formats, e.g. ugc. Omit for Glorya's pick")
  ("review" :boolean
   :doc "campaign only: build the ads and wait for approval before anything launches")
  ("platform" :string :options '("meta" "google_ads" "tiktok")
   :doc "campaign only: where the ads run. Omit for Glorya's pick")
  ("daily_budget" :integer
   :doc "campaign only: the daily budget in the account's currency, e.g. 30"))

(nlk:define-contract job-request
  (:doc "One Glorya job as Glorya has it now: its status, its progress, the question it is waiting on, and the files a finished job made. wait_seconds lets the call sit while the job runs.")
  ("job_id" :text :required t
   :doc "the job_id a create call, /glorya jobs, or a refine returned")
  ("wait_seconds" :integer
   :doc "how long to wait for a change before answering, 0 to 45. Omit to answer at once"))

(nlk:define-contract answer-request
  (:doc "Answer the question a Glorya job is waiting on. Use the request_id and, when the question offers choices, the exact label of the one the person picked.")
  ("job_id" :text :required t :doc "the job that asked")
  ("answer" :text :required t :doc "the choice's exact label, or the person's own words")
  ("request_id" :string :doc "the request_id the question named"))

(nlk:define-contract refine-request
  (:doc "Change a Glorya result the person wants different: a new headline, another colour, a shorter cut. Glorya keeps the job and makes the change on it.")
  ("job_id" :text :required t :doc "the job to change")
  ("instruction" :text :required t :doc "what should change, in the person's words"))

(nlk:define-contract publish-request
  (:doc "Put a finished landing page or website live on the internet, on a glorya.ai address.")
  ("job_id" :text :required t :doc "the landing page or website job")
  ("slug" :string :doc "the address to publish under; omit for Glorya's own"))

(nlk:define-contract attach-request
  (:doc "Download the finished images or video of a Glorya job into this machine and answer their paths, so the person can be shown the files themselves instead of a link.")
  ("job_id" :text :required t :doc "the finished job")
  ("max_files" :integer :doc "how many files at most, 1 to 8; omit for the operator's setting"))

(nlk:define-contract jobs-request
  (:doc "The Glorya jobs this cell follows, newest first: id, kind, title, status, open questions. scope chat is this conversation only; all is every conversation this machine started a job in.")
  ("scope" :string :options '("chat" "all") :doc "chat (default) or all"))
