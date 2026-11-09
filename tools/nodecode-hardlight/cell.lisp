;;;; cell.lisp --- the guide, the section, five tools and /hardlight.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin registers five stateless tools, a skill, a slash
;;;; command and one prompt line. Here the skill is +PRIMER+, which the model
;;;; reads with (help :hardlight) and whose one line rides every request; the
;;;; five tools are the same five over the same shipped data; /hardlight is
;;;; the command. Nothing is stored, nothing is fetched, nothing is executed.

(in-package #:nodecode-hardlight)

(defparameter +version+ "0.2.0"
  "The version of the plugin this cell is a rewrite of.")

(defparameter +primer+
  "Hardlight UI is design guidance for frontends that belong to their subject and let people
finish real tasks. It ships original style grammars and three local checks; it does not create
taste, call another model, fetch inspiration, or certify usability.
Work in this order, and keep an established direction when the job is a bounded refinement:
  1. Establish task and authority. Name the visitor, the concrete job, the object or
     information, and an observable success. Say whether this is refinement, extension,
     redesign or greenfield. Preserve facts and working behavior; flag unknown prices,
     features, rights and assets instead of inventing them. Exit: task sentence, authority,
     scope and factual/asset gaps.
  2. Resolve navigation before surface polish. List destinations with labels, entry and
     return paths; separate global navigation, local views and object actions; define
     current location, direct links, Back/Forward, mobile topology and recovery. Run
     hardlight_navigation on the intended graph and fix missing targets, unreachable pages
     and unintended dead ends. Exit: route table and expected primary/recovery paths.
  3. Generate genuinely competing concepts when the direction is open: three that differ in
     structure, sequence, density, asset premise or interaction, not three palettes on one
     template. hardlight_direction offers reference-derived candidates, not a ranking.
     Exit: three materially different structures with task tradeoffs.
  4. Choose one premise, write the invariants (hierarchy, alignment, type roles, semantic
     color, spacing, state language), name a real proof artifact and an honest asset plan.
     If an unrelated product name would fit the result unchanged, revise the premise.
  5. Implement the task and its full applicable states (default, loading, empty, filtered,
     error, success, permission, keyboard), in the project's own stack. Load layout,
     platform, typography, color, motion and performance guidance as the work needs it, and
     measure explicit color pairs with hardlight_contrast and the role they really play.
  6. Challenge the actual result: initial wide and narrow views, keyboard, focus, direct
     URLs, history, recovery, reduced motion, missing assets. Skim the source with
     hardlight_inspect and read the files that matter; its findings are advisory.
  7. Record the shipped decisions: semantic tokens, type roles, layout and navigation
     rules, component behavior, assets, motion, exceptions, and what stayed untested.
What the checks never prove: a contrast ratio is one painted pair, a navigation analysis is
a declared graph, an inspection is a bounded heuristic sweep. Say which checks did not run.
Read a topic with hardlight_reference: workflow, navigation, states, typography, color,
motion, review, systems, browser, layout, platform, performance, atlas, style, index."
  "What (help :hardlight) answers while the cell runs: the original's skill, in the cell's voice.")

;;; --- what a call carries -----------------------------------------------------

(nlk:define-contract reference-call
  (:doc "Read one of the shipped design guides, the style atlas, or one exact style grammar.")
  ("topic" :string :default "index" :options +reference-topics+
           :doc "the guide to read: index (topics and style IDs), atlas (the whole library), style (one grammar), or a guide name")
  ("style" :string :default "" :doc "the exact atlas ID, required by topic style"))

;;; --- the bodies --------------------------------------------------------------

(defun run-reference (call)
  "hardlight_reference: one guide, the atlas, or one style grammar."
  (multiple-value-bind (text why)
      (reference-text (reference-call-topic call) (reference-call-style call))
    (if why
        (nle:failure (format nil "ERROR: ~a" why))
        text)))

(defun run-contrast (call)
  "hardlight_contrast: the pairs of CALL measured."
  (check-contrast call))

(defun run-navigation (call)
  "hardlight_navigation: the declared graph of CALL analyzed."
  (analyze-navigation call))

(defun run-direction (call)
  "hardlight_direction: the direction packet for CALL."
  (make-direction call))

(defun run-inspect (call)
  "hardlight_inspect: the project walked, bounded, read-only."
  (let* ((asked (inspect-call-max-files call))
         (section-default (or (setting :max-files) 120))
         (max-files (if (plusp asked) asked section-default))
         (root (let ((named (inspect-call-root call)))
                 (if (and named (plusp (length (string-trim " " named)))) named (or (setting :root) "")))))
    (cond ((or (< max-files 1) (> max-files 500))
           (nlk:encode-json-object
            (nlk:json-object "ok" nil "error" "max_files must be an integer in 1..500")))
          (t (inspect-project root max-files)))))

;;; --- the operator's command --------------------------------------------------

(defun run-command (args session-id)
  "One /hardlight invocation: help, the style IDs, the version, or one guide."
  (declare (ignore session-id))
  (let ((action (string-downcase (string-trim '(#\Space #\Tab) (or args "")))))
    (cond
      ((zerop (length action))
       (format nil "Hardlight UI: /hardlight help | styles | version | TOPIC (workflow, navigation, color, ...). ~
For frontend work, read (help :hardlight), then use hardlight_reference, hardlight_direction, ~
hardlight_inspect, hardlight_contrast and hardlight_navigation."))
      ((equal action "help")
       (format nil "Hardlight UI: /hardlight help | styles | version | TOPIC~%~a" +primer+))
      ((equal action "version")
       (format nil "Hardlight UI ~a (Nodecode cell)" +version+))
      ((equal action "styles")
       (index-text))
      (t (multiple-value-bind (text why) (reference-text action "")
           (if why (format nil "ERROR: ~a" why) text))))))

;;; --- the cell ---------------------------------------------------------------

(nlk:define-peripheral hardlight :not-running t)

(nle:define-cell hardlight
  (:section ("hardlight")
    (:guide "nothing here is required: root is the frontend project hardlight_inspect reads when a call names none, and max_files bounds how many of its files one call reads")
    ("root" :string :default ""
     :doc "the frontend project hardlight_inspect reads when a call names none")
    ("max_files" :integer :default 120 :min 1
     :doc "how many files one inspection reads before it stops (1..500)"))
  (:help :hardlight
         "hardlight: frontend design guidance, style grammars and local checks - (help :hardlight)"
         +primer+)
  (:tool "hardlight_reference" (nlk:contract 'reference-call) #'run-reference)
  (:tool "hardlight_direction" (nlk:contract 'direction-call) #'run-direction)
  (:tool "hardlight_contrast" (nlk:contract 'contrast-call) #'run-contrast)
  (:tool "hardlight_navigation" (nlk:contract 'navigation-call) #'run-navigation)
  (:tool "hardlight_inspect" (nlk:contract 'inspect-call) #'run-inspect)
  (:command "hardlight" 'run-command
            :description "Hardlight: design guidance, style grammars and local checks"
            :argument-hint "help|styles|version|TOPIC"))
