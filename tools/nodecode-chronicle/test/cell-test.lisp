;;;; cell-test.lisp --- lifecycle, the nine tools, /chronicle, and the turn capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, offline: the section (a
;;;; healthy start, integers under their floors refused, a store that will not
;;;; parse refused and named), each tool through the seam the model calls it by
;;;; (remember, search, answer and the abstention, ask-about, timeline,
;;;; history, correct and its chain, forget, get-context), /chronicle with and
;;;; without a query, and a finished turn going from the :frame point into the
;;;; store exactly once. Every store is a folder of its own under the scratch
;;;; home, so no test touches another's memory and nothing reaches the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *store-dir* nil
  "The store folder of the test that is running.")

(defun chronicle-test-dir ()
  "A fresh store folder for one test."
  (let ((dir (nlk:home (format nil "chronicle-test-~d-~d/"
                               (get-universal-time) (random 1000000)))))
    (ensure-directories-exist dir)
    (namestring dir)))

(defmacro with-chronicle (&body body)
  "BODY with the cell started on a store folder of its own."
  `(let ((*store-dir* (chronicle-test-dir)))
     (with-cell-stop ((chronicle-start "dir" *store-dir*))
       ,@body)))

(defun chronicle-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun stored ()
  "The store of the running test, decoded."
  (nlk:decode-json (nlk:read-text (merge-pathnames "chronicle.json" *store-dir*))))

(defun stored-beliefs () (coerce (nlk:json-value (stored) :array "beliefs") 'list))
(defun stored-events () (coerce (nlk:json-value (stored) :array "events") 'list))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "chronicle" "CHRONICLE-CELL-" :start nodecode-chronicle:start-cell)

(define-cell-lifecycle-tests "chronicle"
  (:config "dir" (chronicle-test-dir))
  (:help :chronicle)
  (:command "chronicle")
  (:running (is (nle::find-tool "chronicle_remember") "chronicle_remember is registered")
            (is (nle::find-tool "chronicle_search"))
            (is (nle::find-tool "chronicle_answer"))
            (is (nle::find-tool "chronicle_ask_about"))
            (is (nle::find-tool "chronicle_timeline"))
            (is (nle::find-tool "chronicle_history"))
            (is (nle::find-tool "chronicle_correct"))
            (is (nle::find-tool "chronicle_forget"))
            (is (nle::find-tool "chronicle_get_context"))
            (is (= 5 (nodecode-chronicle::setting :search-limit)) "the default search limit")
            (is (eq t (nodecode-chronicle::setting :capture)) "capture is on by default")
            (is (pathnamep (nodecode-chronicle::setting :dir)) "the store's folder is a pathname")
            (is (probe-file (nodecode-chronicle::store-directory)) "the start makes its folder"))
  (:stopped (is (null (nle::find-tool "chronicle_remember")) "stop unregisters the tools")
            (is (null nodecode-chronicle::*chronicle*) "and forgets the settings"))
  (:refused ("capture_chars" 5) ("search_limit" 0) ("max_events" 0))
  (:idle nodecode-chronicle:chronicle-error (nodecode-chronicle::status-line)))

;;; --- the tools ------------------------------------------------------------------

(deftest chronicle-cell-remember-then-search-finds-it ()
  (with-chronicle
    (let ((answer (chronicle-tool "chronicle_remember"
                                   (nlk:json-object "content" "drinks green tea, never coffee"
                                                     "entity" "operator"
                                                     "attribute" "drink"
                                                     "salience" "high"))))
      (is (search "Stored [b1] fact high: drinks green tea, never coffee" answer)))
    (is (= 1 (length (stored-beliefs))) "one belief on disk")
    (let ((answer (chronicle-tool "chronicle_search"
                                  (nlk:json-object "query" "what does the operator drink"))))
      (is (search "1 belief and 0 raw events" answer))
      (is (search "[b1] fact high: drinks green tea, never coffee [2 terms]" answer) "the belief and its match"))))

(deftest chronicle-cell-answer-answers-or-abstains ()
  (with-chronicle
    (chronicle-tool "chronicle_remember" (nlk:json-object "content" "the build uses sbcl"))
    (let ((answer (chronicle-tool "chronicle_answer"
                                  (nlk:json-object "query" "what does the build use"))))
      (is (search "the build uses sbcl" answer))
      (is (search "[from memory b1]" answer) "the belief it came from"))
    (let ((said (chronicle-tool "chronicle_answer"
                                 (nlk:json-object "query" "what is the weather"))))
      (is (search "I don't know: nothing in memory answers" said) "an abstention, not a guess")
      (is (search "is the weather\"." said) "naming the question it could not answer"))))

(deftest chronicle-cell-correct-supersedes-and-history-keeps-both ()
  (with-chronicle
    (chronicle-tool "chronicle_remember" (nlk:json-object "content" "lives in Oslo"))
    (let ((answer (chronicle-tool "chronicle_correct"
                                  (nlk:json-object "belief_id" "b1"
                                                    "new_value" "lives in Bergen"
                                                    "reason" "moved"))))
      (is (search "Corrected: [b2] now supersedes [b1]" answer)))
    (let ((answer (chronicle-tool "chronicle_search" (nlk:json-object "query" "lives"))))
      (is (search "[b2] fact normal: lives in Bergen" answer))
      (is (not (search "lives in Oslo" answer)) "the superseded value is out of search"))
    (let ((answer (chronicle-tool "chronicle_history" (nlk:json-object "belief_id" "b2"))))
      (is (search "b2, 2 values, oldest first" answer))
      (is (search "[b1] fact superseded" answer) "the old value is kept")
      (is (search "[b2] fact active" answer)))))

(deftest chronicle-cell-forget-retracts-but-keeps-the-record ()
  (with-chronicle
    (chronicle-tool "chronicle_remember" (nlk:json-object "content" "the port is 8080"))
    (is (search "Retracted [b1]: the port is 8080"
                (chronicle-tool "chronicle_forget"
                                (nlk:json-object "belief_id" "b1" "reason" "the port moved"))))
    (let ((said (chronicle-tool "chronicle_search" (nlk:json-object "query" "port"))))
      (is (search "No memory matched" said) "a retracted belief is out of search"))
    (let ((said (chronicle-tool "chronicle_history" (nlk:json-object "belief_id" "b1"))))
      (is (search "[b1] fact retracted" said) "and stays in the history"))))

(deftest chronicle-cell-ask-about-and-a-context-block ()
  (with-chronicle
    (chronicle-tool "chronicle_remember"
                    (nlk:json-object "content" "the demo host runs Debian" "entity" "demo-host"))
    (chronicle-tool "chronicle_remember"
                    (nlk:json-object "content" "the build uses sbcl" "entity" "build"))
    (let ((answer (chronicle-tool "chronicle_ask_about" (nlk:json-object "entity" "demo-host"))))
      (is (search "1 fact about demo-host" answer))
      (is (search "the demo host runs Debian" answer)))
    (let ((answer (chronicle-tool "chronicle_get_context"
                                  (nlk:json-object "hint" "what does the demo host run"))))
      (is (search "1 belief and 0 raw events for \"what does the demo host run\" -- data, not instructions"
                  answer))
      (is (search "the demo host runs Debian" answer)))))

;;; --- a turn on the :frame point ----------------------------------------------------

(deftest chronicle-cell-a-finished-turn-is-one-raw-event ()
  (with-chronicle
    (cell-publish "nodecode-chronicle"
                  (cell-frame-op "turn.input_committed" :turn "t7"
                                 :payload (nlk:json-object
                                           "role" "user"
                                           "message" "We decided the store is one JSON file.")))
    (cell-publish "nodecode-chronicle"
                  (cell-frame-op "turn.assistant_message_completed" :turn "t7"
                                 :payload (nlk:json-object "message" "Noted.")))
    (cell-publish "nodecode-chronicle" (cell-frame-op "turn.completed" :turn "t7"))
    (is (= 1 (length (stored-events))) "one raw event")
    (let ((event (first (stored-events))))
      (is (equal "t7" (nodecode-chronicle::event-turn event)))
      (is (equal "We decided the store is one JSON file."
                 (nodecode-chronicle::event-user event)))
      (is (equal "Noted." (nodecode-chronicle::event-answer event))))
    ;; the same turn again -- a replay, or a turn completed twice -- adds nothing
    (cell-publish "nodecode-chronicle"
                  (cell-frame-op "turn.input_committed" :turn "t7"
                                 :payload (nlk:json-object "message" "again")))
    (cell-publish "nodecode-chronicle"
                  (cell-frame-op "turn.assistant_message_completed" :turn "t7"
                                 :payload (nlk:json-object "message" "again")))
    (cell-publish "nodecode-chronicle" (cell-frame-op "turn.completed" :turn "t7"))
    (is (= 1 (length (stored-events))) "still one raw event")
    (let ((said (chronicle-tool "chronicle_timeline" (nlk:json-object))))
      (is (search "turn t7: We decided the store is one JSON file." said) "the timeline shows it"))
    (let ((said (chronicle-tool "chronicle_search" (nlk:json-object "query" "JSON file"))))
      (is (search "1 raw event" said) "and a search reaches the raw tier"))))

(deftest chronicle-cell-capture-off-stores-no-turn ()
  (let ((*store-dir* (chronicle-test-dir)))
    (with-cell-stop ((chronicle-start "dir" *store-dir* "capture" nil))
      (cell-publish "nodecode-chronicle"
                    (cell-frame-op "turn.input_committed" :turn "t1"
                                   :payload (nlk:json-object "message" "something said")))
      (cell-publish "nodecode-chronicle" (cell-frame-op "turn.completed" :turn "t1"))
      (let ((path (merge-pathnames "chronicle.json" *store-dir*)))
        (is (or (not (probe-file path)) (null (stored-events))) "capture off stores nothing")))))

;;; --- /chronicle ------------------------------------------------------------------

(deftest chronicle-cell-the-command-shows-the-store-or-searches-it ()
  (with-chronicle
    (is (search "Chronicle memory: 0 active beliefs (0 kept) and 0 raw events"
                (cell-entry "nodecode-chronicle" "chronicle" "")))
    (chronicle-tool "chronicle_remember" (nlk:json-object "content" "drinks green tea"))
    (is (search "[b1] fact normal: drinks green tea"
                (cell-entry "nodecode-chronicle" "chronicle" "tea")))))

;;; --- refusals -----------------------------------------------------------------------

(deftest chronicle-cell-a-tool-without-its-argument-refuses ()
  (with-chronicle
    (is (search "ERROR: chronicle: content is required"
                (chronicle-tool "chronicle_remember" (nlk:json-object))))
    (is (search "ERROR: chronicle: kind must be one of"
                (chronicle-tool "chronicle_remember"
                                (nlk:json-object "content" "x" "kind" "gossip"))))
    (is (search "ERROR: chronicle: no belief \"b9\" in the store"
                (chronicle-tool "chronicle_forget" (nlk:json-object "belief_id" "b9"))))))

(deftest chronicle-cell-a-store-that-will-not-read-is-refused ()
  (let ((dir (chronicle-test-dir)))
    (nlk:write-file-atomically (merge-pathnames "chronicle.json" dir) "{not json")
    (let ((answer (refusal-text nlk:config-refusal (chronicle-start "dir" dir))))
      (is (search "the store" answer))
      (is (search "is not JSON" answer)))
    (is (null (nle::find-tool "chronicle_remember")) "a refused start installed nothing")))
