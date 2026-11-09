;;;; contracts.lisp --- the codex_truly tool's arguments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, and a call that does not satisfy it is
;;;; refused before the body sees it.

(in-package #:nodecode-codex-truly)

(nlk:define-contract request
  (:doc "The Codex context window of a ChatGPT/Codex subscription: what the account's own model catalog advertises, what the backend measurably accepts, and the scoped providers.<id>.models entry that hands the model the larger window. action='status' reports per model the advertised window, the catalog's published ceiling, the window Nodecode's config sets now, and the largest input a live probe accepted. action='apply' writes context_window for every model still below the target (dry_run=true previews and writes nothing). action='revert' takes back exactly the values apply wrote. action='probe' re-measures one model's true ceiling by bisection against the live backend, which spends subscription quota, so run it only when the person asks. Use it when a Codex model's context fills up sooner than the person expects, or when they ask how much context their subscription really gives them. Note that a 900K window makes every turn submit far more input, so it is worth it for long documents and large repositories and wasteful for short chats.")
  ("action" :text :required t :options '("status" "apply" "revert" "probe")
   :doc "status (read-only), apply, revert, or probe (spends subscription quota)")
  ("model" :string :default ""
   :doc "probe only: the model id to measure; empty measures the first model the catalog offers")
  ("target_context_window" :integer
   :doc "apply only: the window to grant each model, 272000 to 918000; the section's target when empty")
  ("dry_run" :boolean :default nil
   :doc "apply only: report what would be written, and write nothing"))
