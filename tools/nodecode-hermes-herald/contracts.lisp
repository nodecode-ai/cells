;;;; contracts.lisp --- the six tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-hermes-herald)

(nlk:define-contract dispatch-request
  (:doc "Start one task on the Hermes Agent profile this cell is pointed at, and answer its run id at once. The run has its own session, model, skills and tools on that profile, and keeps going after this call returns: read it with hermes_check (a whole run takes seconds to hours), stop it with hermes_cancel. The brief does not inherit this conversation: put the goal, the files and paths, the constraints, the output format and how to verify the work in `message`, and put standing role text in `instructions`.")
  ("message" :text :required t
   :doc "the self-contained brief: goal, paths, constraints, output format, verification")
  ("instructions" :string :default ""
   :doc "optional system instructions for the target run, e.g. a role or a house style")
  ("model" :string :default ""
   :doc "optional model route alias the target advertises (see hermes_models); omit for the target's own default or the one this cell configures"))

(nlk:define-contract run-check
  (:doc "Read the status of one or more dispatched runs from the target: running, completed, failed, cancelled or waiting_for_approval, with the final summary once there is one. Poll one run, or several at once, in a single call; a run on another profile, or one that a target restart forgot, answers not_found.")
  ("run_ids" :array :of :string :required t
   :doc "the run ids hermes_dispatch answered, e.g. [\"8f14e45fceea167a\"]"))

(nlk:define-contract run-cancel
  (:doc "Ask the target to stop one run. Cancellation is cooperative: the target finishes the step it is on, so poll with hermes_check afterwards. The run's partial work stays on the target.")
  ("run_id" :text :required t
   :doc "the run id hermes_dispatch answered"))

(nlk:define-contract model-list
  (:doc "List the model routes the target profile advertises for dispatch: the aliases you may pass as `model` to hermes_dispatch or hermes_chat, the model each one resolves to, and the provider each is pinned to. A short list means the target declares few routes, not that the model does not exist: the target's own config names them.")
  ("query" :string :default ""
   :doc "optional text the alias has to contain, e.g. \"code\" or \"fast\""))

(nlk:define-contract chat-request
  (:doc "Talk to the target profile synchronously and answer its reply in this turn: for a conversation whose later turns depend on earlier ones. The target keeps the session, so a second call continues the same thread unless `new_session` is true. Unlike hermes_dispatch this blocks this turn until the target answers, so keep the ask small.")
  ("message" :text :required t
   :doc "what to say to the target profile")
  ("instructions" :string :default ""
   :doc "optional system instructions, sent with this one request")
  ("model" :string :default ""
   :doc "optional model route alias the target advertises; omit for the target's default")
  ("new_session" :boolean :default nil
   :doc "start a fresh conversation on the target instead of continuing the saved one"))

(nlk:define-contract run-ledger
  (:doc "List the runs this cell dispatched from this machine, newest first: the run id, target, model, status and the brief's opening words. It is this side's record, written at dispatch and refreshed when a run is read; the target stays the authority on a run's live status, so follow a record with hermes_check.")
  ("limit" :integer :default 10 :slot max-runs
   :doc "how many runs to answer, newest first"))
