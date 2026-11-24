;;;; direction.lisp --- deterministic reference-derived direction packets.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's hardlight/direction.py and hardlight/brief.py, in Lisp:
;;;; three candidate grammars for a brief (the surface's own defaults unless
;;;; the caller names exact IDs), the facts the packet is still missing, an
;;;; unanswered design worksheet and a verification matrix nothing has run
;;;; yet. Reference selection, never generated artwork and never a taste score.

(in-package #:nodecode-hardlight)

(defparameter +surfaces+ '("marketing" "product" "reading" "commerce" "portfolio")
  "The surfaces a brief can declare.")

(defparameter +authorities+ '("refinement" "extension" "redesign" "greenfield")
  "How much of an incumbent the work may move: the first two preserve it.")

(defparameter +surface-defaults+
  '(("marketing" . ("kinetic-poster" "organic-humanist" "swiss-editorial"))
    ("product" . ("systemic-product" "data-atelier" "wayfinding"))
    ("reading" . ("swiss-editorial" "editorial-luxe" "quiet-craft"))
    ("commerce" . ("editorial-luxe" "organic-humanist" "expressive-material"))
    ("portfolio" . ("cinematic-gallery" "bauhaus-play" "spatial-technical")))
  "The three grammars a surface offers when the caller names none.")

(defparameter +context-fields+
  '(("audience" :text . "Name the people and situation the interface serves.")
    ("tasks" :list . "List concrete jobs and observable successful results.")
    ("content" :list . "Identify actual content, objects and missing product facts.")
    ("assets" :list . "Inventory available assets, rights and fallbacks.")
    ("identity" :text . "Record incumbent tokens, navigation and behaviors to retain.")
    ("browsers" :text . "State the browser/device target and fallback expectations.")
    ("constraints" :list . "Record performance, accessibility, delivery and operational constraints."))
  "Every field a context may carry, and the question it answers.")

(defparameter +craft-references+
  '("layout" "platform" "typography" "color" "motion" "performance")
  "The guides a candidate is developed against.")

(defparameter +design-decisions+
  '(("thesis" . "Name the subject-specific idea, the primary user job and the real object, content or proof artifact that makes the idea visible.")
    ("in invariants" . "Define alignment, hierarchy, type roles, semantic color and spacing rules that remain coherent beyond the opening screen.")
    ("signature" . "Choose one recognizable interaction or material gesture serving a real job; specify its immediate, keyboard and reduced-motion equivalents.")
    ("mobile" . "Describe the narrow-screen transformation: content priority, grouping, navigation and artwork. Include long copy, text resizing and relevant writing directions.")
    ("budget" . "Choose browser support, asset/font/effect budgets and baseline fallbacks. Separate proposed targets from measured evidence.")
    ("critique" . "Try an unrelated subject name: identify which decisions would have to change. Revise generic decisions; record a real risk and the observation that would challenge it."))
  "The worksheet a packet hands back unanswered, one decision each.")

(defparameter +verification-matrix+
  '(("tasks" . "Primary/recovery paths, direct URLs, Back/Forward and applicable state transitions.")
    ("responsive" . "Initial and engaged wide, narrow and intermediate views; 320 CSS pixel reflow, long copy and text resizing.")
    ("input" . "Keyboard order, visible/unobscured focus, touch alternatives and relevant target-size exceptions.")
    ("preferences" . "Reduced motion, forced colors, supported themes and fallback fonts; preserve information and controls.")
    ("platform" . "Chosen capabilities detected in the target browsers and exercised with enhancements unavailable.")
    ("paint" . "Actual supported color pairs and roles; unsupported syntax or composite paint stays unknown.")
    ("performance" . "Asset bytes and reproducible lab observations with device/build context; field metrics require real visits.")
    ("composition" . "Human review against the authored premise and real content; no numerical beauty score."))
  "What has to be observed before a design claim stands; every row starts not run.")

(defparameter +ia-questions+
  '("Who is using this surface and what job must they finish?"
    "Which destinations and direct links support each primary task?"
    "What labels, current-location cues and Back/Forward behavior are needed?"
    "How does mobile navigation preserve the same destinations?"
    "How do search, filters, permissions and recovery persist or reset?")
  "The information-architecture questions a direction leaves to its author.")

(defparameter +state-matrix+
  '(("default" . "Show meaningful content and primary task.")
    ("loading" . "Explain progress without hiding current location.")
    ("empty" . "Explain why and offer a real next action.")
    ("error" . "Preserve input and provide actionable recovery.")
    ("success" . "Confirm the action and its next destination.")
    ("permission" . "Explain unavailable actions and allowed alternatives.")
    ("keyboard" . "Visible focus, logical order and no traps."))
  "The states an implemented surface is expected to answer for.")

;;; --- the boundary -----------------------------------------------------------

(nlk:define-contract design-context
  (:doc "Facts the caller already knows about the project. Omit an unknown field rather than guessing; nothing here is inferred.")
  ("audience" :string :default "" :doc "the people and situation the interface serves")
  ("tasks" :array :of :string :doc "concrete jobs and observable successful results")
  ("content" :array :of :string :doc "actual content, objects and missing product facts")
  ("assets" :array :of :string :doc "available assets, rights and fallbacks")
  ("identity" :string :default "" :slot identity-tokens
              :doc "incumbent tokens, navigation and behaviors to retain")
  ("browsers" :string :default "" :doc "the browser/device target and fallback expectations")
  ("constraints" :array :of :string :doc "performance, accessibility, delivery and operational constraints"))

(nlk:define-contract direction-call
  (:doc "Build a deterministic direction packet for a brief: three reference grammars for the surface (or the exact IDs named), the facts still missing, an unanswered design worksheet and a verification matrix. References, not generated artwork or a taste ranking.")
  ("brief" :text :required t :doc "the brief, in one or two sentences: the subject and the job")
  ("surface" :string :default "product" :options '("marketing" "product" "reading" "commerce" "portfolio")
            :doc "what kind of surface this is; it selects the default grammars")
  ("authority" :string :default "greenfield" :options '("refinement" "extension" "redesign" "greenfield")
              :doc "refinement and extension preserve the incumbent's tokens, identity, navigation and working behavior")
  ("styles" :array :of :string :doc "1..3 exact atlas IDs; empty takes the surface's three")
  ("context" :object :of design-context :doc "facts already known; omit what is unknown"))

;;; --- the packet -------------------------------------------------------------

(defparameter +context-slots+ '(("identity" . "identity-tokens"))
  "The context keys whose record slot is not the key itself.")

(defun context-slot (key)
  "KEY as the record's slot name, in the symbol the derived reader uses."
  (intern (format nil "DESIGN-CONTEXT-~:@(~a~)" (or (cdr (assoc key +context-slots+ :test #'equal)) key))
          :nodecode-hardlight))

(defun context-value (context key kind)
  "CONTEXT's KEY as a string (or a list of strings), empty when it carries none."
  (if (null context)
      (if (eq kind :text) "" '())
      (let ((value (funcall (context-slot key) context)))
        (if (eq kind :text) (or value "") (coerce value 'list)))))

(defun supplied-p (context key kind)
  (let ((value (context-value context key kind)))
    (if (eq kind :text) (plusp (length (string-trim " " value))) (not (null value)))))

(defun direction-gaps (context preserve)
  "The facts the packet is missing: one line a field, or the original's two
canned lines when the caller supplied no context at all."
  (if (null context)
      (if preserve
          (list "Existing navigation, tokens, brand assets and user-tested behavior were not supplied; do not infer them.")
          (list "Audience, primary jobs, content inventory and available asset rights require confirmation."))
      (loop for (key kind . prompt) in +context-fields+
            unless (supplied-p context key kind)
              collect (format nil "~a: ~a" key prompt))))

(defun direction-candidates (surface styles)
  "The exact styles the packet offers, or (values NIL WHY)."
  (let ((ids (if (plusp (length styles)) (coerce styles 'list) (cdr (assoc surface +surface-defaults+ :test #'equal)))))
    (cond ((> (length ids) 3)
           (values nil "styles must be 1..3 distinct exact atlas IDs"))
          ((not (= (length ids) (length (remove-duplicates ids :test #'equal))))
           (values nil "styles must be distinct exact atlas IDs"))
          (t
           (let ((unknown (remove-if #'find-style ids)))
             (if unknown
                 (values nil (format nil "unknown style ID~p; choose one of: ~{~a~^, ~}"
                                     (length unknown) (style-ids)))
                 (values (mapcar #'find-style ids) nil)))))))

(defun write-context (out context)
  (when context
    (format out "~%supplied context:~%")
    (loop for (key kind . prompt) in +context-fields+
          when (supplied-p context key kind)
            do (let ((value (context-value context key kind)))
                 (if (eq kind :text)
                     (format out "  ~a: ~a~%" key value)
                     (format out "  ~a: ~{~a~^; ~}~%" key value))))))

(defun make-direction (call)
  "hardlight_direction: the packet for CALL, as prose a designer can work from."
  (let* ((brief (direction-call-brief call))
         (surface (direction-call-surface call))
         (authority (direction-call-authority call))
         (preserve (member authority '("refinement" "extension") :test #'equal))
         (context (direction-call-context call)))
    (multiple-value-bind (candidates why)
        (direction-candidates surface (or (direction-call-styles call) #()))
      (if why
          (nle:failure (format nil "ERROR: ~a" why))
          (with-output-to-string (out)
            (format out "Hardlight direction~%brief: ~a~%surface: ~a~%authority: ~a~%preserve incumbent: ~a~2%"
                    brief surface authority (if preserve "yes" "no"))
            (format out "authority guidance: ~a~2%"
                    (if preserve
                        "Preserve incumbent tokens, identity, navigation and working behavior; propose compatible changes only."
                        "Confirm retained constraints before adopting a new direction."))
            (format out "missing context:~%")
            (dolist (gap (direction-gaps context preserve)) (format out "  - ~a~%" gap))
            (write-context out context)
            (format out "~%design contract (unanswered: complete these with the supplied facts and the actual project):~%")
            (format out "  - structure: ~a~%"
                    (if preserve
                        "Preserve the incumbent composition and navigation; explain the smallest compatible change and its tradeoff."
                        "Compare three concepts that differ in at least two of composition, sequence, density, assets or interaction. State each task tradeoff and select one."))
            (loop for (id . prompt) in +design-decisions+
                  do (format out "  - ~a: ~a~%" id prompt))
            (format out "~%candidates (reference grammar only: develop a thesis and a real asset plan for this brief; do not copy a source site):~%")
            (loop for candidate in candidates
                  for index from 1
                  do (format out "~%[~a]~%~a" index (style-text candidate)))
            (format out "~%craft references: ~{~a~^, ~}~%" +craft-references+)
            (format out "~%state matrix:~%")
            (loop for (state . requirement) in +state-matrix+
                  do (format out "  - ~a: ~a~%" state requirement))
            (format out "~%information architecture questions:~%")
            (dolist (question +ia-questions+) (format out "  - ~a~%" question))
            (format out "~%verification matrix (nothing is run yet):~%")
            (loop for (check . evidence) in +verification-matrix+
                  do (format out "  - ~a: not run — ~a~%" check evidence))
            (format out "~%asset plan: inventory real owned or licensed imagery, diagrams and content; use labeled placeholders until assets exist. Never invent testimonials, metrics or evidence.~%")
            (format out "signature plan: choose one coherent structural signature from a candidate and connect it to the primary user job, not decoration alone.~%")
            (format out "verification plan:~%")
            (dolist (step '("Map destinations and primary task paths before styling."
                            "Inspect incumbent tokens before changes."
                            "Test direct links, Back/Forward, keyboard and mobile routes."
                            "Verify loading, empty, error, permission and success states."
                            "Measure explicit contrast roles and reduced-motion behavior."
                            "Inspect wide and narrow screenshots; automated checks do not certify beauty, usability or WCAG conformance."))
              (format out "  - ~a~%" step))
            (format out "~%limitations:~%")
            (dolist (limitation '("Deterministic reference selection, not scientific taste ranking or generated original artwork."
                                  "Explicit style choices are preserved, including those outside usual surface recommendations."))
              (format out "  - ~a~%" limitation)))))))
