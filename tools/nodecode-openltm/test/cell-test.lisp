;;;; cell-test.lisp --- lifecycle, the eight tools, /openltm, and a turn's facts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong choice and an integer under its floor refused, a required
;;;; user_id that refuses), the eight tools through the seam the model calls
;;;; them by, /openltm with and without a query, and a finished turn's frames
;;;; going through the rule table into the store. Nothing here reaches the
;;;; network: the store is this scratch home's own file.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defmacro with-fresh-store (&body body)
  "BODY with an empty store: the files deleted, the lists and counters cleared."
  `(progn
     (nodecode-openltm::reset-store)
     (unwind-protect (progn ,@body)
       (ignore-errors (nodecode-openltm::reset-store)))))

(defun openltm-tool (name &rest arguments)
  "Run the registered tool NAME with ARGUMENTS -- alternating key/value -- as
dispatch does, and answer what it answers."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) (apply #'nlk:make-json-object arguments))))

(defun openltm-brain (&aux (entry (nle::find-tool "openltm_brain_stats")))
  (funcall (third entry) (nlk:make-json-object)))

(defun openltm-start-fresh (&rest pairs)
  "Reset the store, then start the cell over PAIRS."
  (nodecode-openltm::reset-store)
  (openltm-start "user_id" "tester" pairs))

(defmacro with-openltm-running ((&rest pairs) &body body)
  `(with-fresh-store
     (with-cell-stop ((openltm-start-fresh ,@pairs))
       ,@body)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "openltm" "OPENLTM-CELL-" :start nodecode-openltm:start-cell)

(define-cell-lifecycle-tests "openltm"
  (:fixture with-fresh-store)
  (:config "user_id" "tester")
  (:hooks :frame 'nle::read-harness-sections)
  (:help :openltm)
  (:command "openltm")
  (:running (is (nle::find-tool "openltm_recall") "openltm_recall is registered")
            (is (nle::find-tool "openltm_learn"))
            (is (nle::find-tool "openltm_forget"))
            (is (nle::find-tool "openltm_context"))
            (is (nle::find-tool "openltm_relate"))
            (is (nle::find-tool "openltm_graph"))
            (is (nle::find-tool "openltm_brain_stats"))
            (is (nle::find-tool "openltm_stale"))
            (is (equal "tester" (nodecode-openltm::setting :user-id)) "the section's user")
            (is (= 30 (nodecode-openltm::setting :decay-days)) "the default half-life")
            (is (equal '() nodecode-openltm::*memories*) "an empty brain"))
  (:stopped (is (null (nle::find-tool "openltm_recall")) "stop unregisters the tools")
            (is (null (nle::find-tool "openltm_brain_stats"))))
  (:refused ("decay_days" 0) ("importance_default" 0) ("user_id" 7))
  (:idle nodecode-openltm:openltm-error (nodecode-openltm::run-command "" nil)))

(deftest openltm-cell-a-required-setting-refuses ()
  ;; user_id is the section's one required member: the setup panel names it,
  ;; and a tool called without it refuses in the same words, naming the key.
  (with-fresh-store
    (is (search "user_id"
                (format nil "~{~a~^; ~}"
                        (nlk:section-problems (nlk:find-section '("openltm")) nil))) "the section reports user_id as required")
    (with-cell-stop ((openltm-start "capture" nil))
      (let ((answer (openltm-tool "openltm_learn" "content" "A fact worth keeping.")))
        (is (search "user_id" answer) "the tool refuses, naming the setting")
        (is (equal '() nodecode-openltm::*memories*) "and stores nothing")))))

;;; --- the tools -------------------------------------------------------------

(deftest openltm-cell-learn-then-recall ()
  (with-openltm-running ()
    (let ((stored (openltm-tool "openltm_learn" "content" "The operator drinks green tea in the morning."
                                 "title" "Green tea" "category" "preference" "importance" 4)))
      (is (search "Stored #1" stored) stored))
    (openltm-tool "openltm_learn" "content" "The deploy script must never touch the live database.")
    (openltm-tool "openltm_learn" "content" "The parser breaks on an empty frame."
                   "category" "gotcha")
    (let ((hits (openltm-tool "openltm_recall" "query" "green tea")))
      (is (search "#1 [preference]" hits) hits)
      (is (search "Green tea" hits) "the title is shown")
      (is (not (search "#3" hits)) "nothing else matched"))
    (let ((hits (openltm-tool "openltm_recall" "query" "database" "category" "constraint")))
      (is (search "#2 [constraint]" hits) hits))
    (let ((none (openltm-tool "openltm_recall" "query" "kubernetes")))
      (is (search "No memory matched" none) none))
    (is (equal 3 (length nodecode-openltm::*memories*)) "three memories kept")
    (is (probe-file (nodecode-openltm::memory-file)) "the store is a file in this home")
    (let ((stats (openltm-brain)))
      (is (search "brain: 3 mem" stats) stats)
      (is (search "gotcha 1" stats) "the rule table filed the parser fact as a gotcha"))
    (is (search "No memory is scoped" (openltm-tool "openltm_context" "project" "atlas")) "a project with no memory says so")))

(deftest openltm-cell-the-same-fact-twice-is-confirmation ()
  (with-openltm-running ()
    (openltm-tool "openltm_learn" "content" "Always run the tests before a push.")
    (let ((again (openltm-tool "openltm_learn" "content" "  always run the tests before a push. ")))
      (is (search "Reinforced #1" again) again)
      (is (search "2 times" again) "the confirmation is counted"))
    (is (equal 1 (length nodecode-openltm::*memories*)) "one memory, not two")
    (is (equal 2 (getf (first nodecode-openltm::*memories*) :confirms)))))

(deftest openltm-cell-links-graph-and-forget ()
  (with-openltm-running ()
    (openltm-tool "openltm_learn" "content" "The cache is keyed by the store stamp.")
    (openltm-tool "openltm_learn" "content" "A restamp leaves stale cache entries behind.")
    (let ((linked (openltm-tool "openltm_relate" "source_id" 2 "target_id" 1
                                 "relationship_type" "supports")))
      (is (search "Linked #2 supports #1" linked) linked))
    (is (search "already" (openltm-tool "openltm_relate" "source_id" 2 "target_id" 1
                                         "relationship_type" "supports")) "a link said twice is said once")
    (let ((graph (openltm-tool "openltm_graph" "memory_id" 2)))
      (is (search "#1 [pattern]" graph) graph)
      (is (search "supports #1" graph) "the link is named"))
    (is (search "must be one of" (openltm-tool "openltm_relate" "source_id" 1 "target_id" 2
                                                "relationship_type" "causes")) "a type outside the six is refused")
    (is (search "Forgot #2" (openltm-tool "openltm_forget" "id" 2)))
    (is (equal 1 (length nodecode-openltm::*memories*)))
    (is (equal '() nodecode-openltm::*relations*) "the link went with it")
    (is (search "no memory #9" (openltm-tool "openltm_forget" "id" 9)) "an unknown id refuses")))

(deftest openltm-cell-stale-and-context ()
  (with-openltm-running ()
    (openltm-tool "openltm_learn" "content" "The build uses the pinned SBCL."
                   "project" "atlas" "category" "architecture")
    (openltm-tool "openltm_learn" "content" "Never write the live database from a test."
                   "project" "atlas" "category" "constraint")
    (let ((context (openltm-tool "openltm_context" "project" "atlas")))
      (is (search "for project \"atlas\"" context) context)
      (is (search "architecture:" context) "grouped by category")
      (is (search "constraint:" context)))
    (is (search "Flagged #1 stale" (openltm-tool "openltm_stale" "action" "flag"
                                                  "memory_id" 1 "reason" "the pin moved")))
    (let ((listed (openltm-tool "openltm_stale" "action" "list")))
      (is (search "#1" listed) listed)
      (is (search "the pin moved" listed) "the reason is kept"))
    (is (search "Revalidated #1" (openltm-tool "openltm_stale" "action" "revalidate"
                                                "memory_id" 1)))
    (is (search "No memory is flagged stale" (openltm-tool "openltm_stale" "action" "list")))
    (is (search "brain: 2 mem" (openltm-brain)) "the store survives a reload")
    ;; the file is the authority: a start reads it back
    (nodecode-openltm::load-store)
    (is (equal 2 (length nodecode-openltm::*memories*)) "two memories read back")
    (is (equal "constraint" (getf (second nodecode-openltm::*memories*) :category)) "with their categories")))

(deftest openltm-cell-a-turn-adds-what-the-rules-say ()
  (with-openltm-running ()
    ;; the operator's constraint and the answer's discovery, as the gateway
    ;; publishes them
    (cell-publish "nodecode-openltm"
                  (cell-frame-op "turn.input_committed" :turn "t1"
                                 :payload (nlk:json-object "role" "user"
                                                            "message" "I prefer that you always run the tests before a push. Thanks.")))
    (cell-publish "nodecode-openltm"
                  (cell-frame-op "turn.assistant_message_completed" :turn "t1"
                                 :payload (nlk:json-object "message" "Noted. The fix is to pin the toolchain.")))
    (cell-publish "nodecode-openltm" (cell-frame-op "turn.completed" :turn "t1"))
    (is (= 2 (length nodecode-openltm::*memories*))
        (format nil "one fact a side: ~s" (mapcar (lambda (m) (getf m :content)) nodecode-openltm::*memories*)))
    (let ((learned (first nodecode-openltm::*memories*)))
      (is (equal "constraint" (getf learned :category)) "the user's line filed as a constraint")
      (is (search "always run the tests" (getf learned :content)) (getf learned :content))
      (is (equal "gotcha" (getf (second nodecode-openltm::*memories*) :category))
          (format nil "the answer's discovery as a gotcha: ~s"
                  (mapcar (lambda (m) (getf m :content)) nodecode-openltm::*memories*))))
    ;; ops noise and a bare read request are not facts
    (cell-publish "nodecode-openltm"
                  (cell-frame-op "turn.input_committed" :turn "t2"
                                 :payload (nlk:json-object "role" "user"
                                                            "message" "Async delegation batch complete for later.")))
    (cell-publish "nodecode-openltm" (cell-frame-op "turn.completed" :turn "t2"))
    (cell-publish "nodecode-openltm"
                  (cell-frame-op "turn.input_committed" :turn "t3"
                                 :payload (nlk:json-object "role" "user"
                                                            "message" "Please call openltm_context now and report the stats.")))
    (cell-publish "nodecode-openltm" (cell-frame-op "turn.completed" :turn "t3"))
    (is (= 2 (length nodecode-openltm::*memories*)) "neither was stored")
    (is (probe-file (nodecode-openltm::memory-file)) "and the two that were are on disk")))

;;; --- the command ---------------------------------------------------------------

(deftest openltm-cell-command-answers ()
  (with-openltm-running ()
    (openltm-tool "openltm_learn" "content" "The board file is rendered from the log.")
    (let ((state (cell-entry "nodecode-openltm" "openltm" nil)))
      (is (search "OpenLTM memory: 1 mem" state) state))
    (let ((found (cell-entry "nodecode-openltm" "openltm" "board file")))
      (is (search "#1 [pattern]" found) found))
    (let ((empty (cell-entry "nodecode-openltm" "openltm" "kubernetes")))
      (is (search "No memory matched" empty) empty))))
