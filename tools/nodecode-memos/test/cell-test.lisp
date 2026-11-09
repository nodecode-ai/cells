;;;; cell-test.lisp --- lifecycle, the six tools, /memos, and facts outliving a restart.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test runs over a file in a temporary directory, so no test touches
;;;; the home's memories and nothing here dials anything: the store is one
;;;; file, and what a tool answers is what the file's lines fold to.

(in-package #:nodecode.test)

;;; --- the slice and its fixtures ------------------------------------------------

(define-test-slice "memos" "MEMOS-CELL-" :start nodecode-memos:start-cell)

(defvar *memos-file* nil
  "The file the running test's memories live in, under a temporary directory.")

(defmacro with-memos-home (&body body)
  "BODY with the cell's file under a fresh 0700 directory, removed on unwind."
  `(with-temp-directory (dir "memos")
     (let ((*memos-file* (namestring (merge-pathnames "memories.jsonl" dir))))
       ,@body)))

(defun memos-tool (name &rest pairs)
  "Run the registered tool NAME on PAIRS, as dispatch does: through its contract."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun memos-id (answer)
  "The memory id ANSWER names inside its first brackets."
  (let ((open (search "[" answer)))
    (and open (subseq answer (1+ open) (position #\] answer :start open)))))

(defun memos-lines ()
  "The fact file's lines."
  (let ((path (nodecode-memos:store-path)))
    (if (probe-file path) (uiop:read-file-lines path) '())))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "memos"
  (:fixture with-memos-home)
  (:config "file" *memos-file* "limit" 10)
  (:help :memos)
  (:command "memos")
  (:running (is (nle::find-tool "memos_store") "memos_store is registered")
            (is (nle::find-tool "memos_search") "and memos_search")
            (is (nle::find-tool "memos_context_pack") "and memos_context_pack")
            (is (nle::find-tool "memos_supersede") "and memos_supersede")
            (is (nle::find-tool "memos_forget") "and memos_forget")
            (is (nle::find-tool "memos_history") "and memos_history")
            (is (= 10 (nodecode-memos::setting :limit)) "a search answers ten unless it says otherwise")
            (is (= 0 (nodecode-memos::setting :standing)) "no fact rides a prompt by default")
            (is (search "memories.jsonl" (namestring (nodecode-memos::store-path))) "the test's own file"))
  (:stopped (is (null (nle::find-tool "memos_store")) "stop unregisters the tools")
            (is (null (nle::find-tool "memos_history"))) )
  (:refused ("limit" 0) ("standing" -1))
  (:idle nodecode-memos:memos-error (nodecode-memos::run-command "" nil)))

;;; --- keeping and finding ------------------------------------------------------------

(deftest memos-cell-a-kept-fact-is-found-by-its-words ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file*))
      (let* ((stored (memos-tool "memos_store"
                                  "text" "the operator deploys with kamal, never kubernetes"
                                  "memory_type" "preference" "namespace" "deploy"
                                  "tags" #("deploy")))
             (id (memos-id stored)))
        (is (search "remembered [" stored) "a store answers the new id")
        (is (search "preference/deploy" stored) "with its type and namespace")
        (is (equal 1 (length (memos-lines))) "one fact is one line")
        (is (search "kamal" (first (memos-lines))) "the line holds the fact's own words")
        (is-carrying (text (memos-tool "memos_search" "query" "how do we deploy"))
          ("1 memory for" "a question's words find the fact")
          ("kamal" "its own words are in the answer")
          ("preference/deploy" "with where it was kept"))
        (is (search id (memos-tool "memos_search" "query" "how do we deploy")) "the id a correction takes")
        (is (equal "no memory matches \"zebra\""
                   (memos-tool "memos_search" "query" "zebra")) "nothing stored answers nothing")
        (is (search "1 memory" (memos-tool "memos_search" "query" "deploy" "namespace" "deploy")) "the namespace it was kept in answers")
        (is (search "no memory matches"
                    (memos-tool "memos_search" "query" "deploy" "namespace" "other")) "and another namespace does not")
        (is (search "no memory matches" (memos-tool "memos_search" "query" "deploy" "memory_type" "entity")) "nor another type")))))

(deftest memos-cell-a-correction-supersedes-and-the-history-keeps-both ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file*))
      (let* ((old (memos-id (memos-tool "memos_store"
                                         "text" "the staging database is cockroachdb" "namespace" "shop")))
             (corrected (memos-tool "memos_supersede" "memory_id" old
                                     "text" "the staging database is postgres"))
             (new (memos-id corrected)))
        (is (search old corrected) "the correction names the fact it replaces")
        (is (search new corrected) "and the fact it keeps")
        (is (equal 3 (length (memos-lines))) "a correction is two facts, never a rewrite")
        (is (equal "no memory matches \"cockroachdb\""
                   (memos-tool "memos_search" "query" "cockroachdb")) "the old wording no longer answers")
        (is (search "postgres" (memos-tool "memos_search" "query" "postgres")) "the correction does")
        (let ((story (memos-tool "memos_history" "memory_id" old)))
          (is (search "cockroachdb" story) "the history reads the old sentence")
          (is (search "superseded by" story) "and says what replaced it")
          (is (search "postgres" story) "and the correction's words"))
        (is (search "already superseded" (memos-tool "memos_supersede" "memory_id" old "text" "x")) "a second correction of the same fact refuses")))))

(deftest memos-cell-forget-stops-the-answer-and-keeps-the-line ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file*))
      (let* ((id (memos-id (memos-tool "memos_store" "text" "the office wifi password is hunter2")))
             (forgotten (memos-tool "memos_forget" "memory_id" id)))
        (is (search (format nil "forgot [~a]" id) forgotten) "forget answers the id it took")
        (is (equal "no memory matches \"wifi\"" (memos-tool "memos_search" "query" "wifi")) "and it no longer answers")
        (is (search "forgotten" (memos-tool "memos_history" "memory_id" id)) "the history says so")
        (is (equal 2 (length (memos-lines))) "the file still holds the fact")
        (is (search "no memory" (memos-tool "memos_forget" "memory_id" "mem:nosuchid")) "an id nothing kept refuses")
        (is (search "forgotten already" (memos-tool "memos_forget" "memory_id" id)) "and a second forget of one refuses")))))

(deftest memos-cell-a-pack-fits-its-budget-and-counts-what-did-not ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file*))
      (memos-tool "memos_store" "text" "the deploy runs from the laptop with kamal")
      (memos-tool "memos_store" "text" "the deploy is one command in the justfile")
      (memos-tool "memos_store" "text" "the deploy writes a log under /tmp on the laptop")
      (let ((whole (memos-tool "memos_context_pack" "query" "deploy" "tokens" 2000))
            (small (memos-tool "memos_context_pack" "query" "deploy" "tokens" 20)))
        (is (search "3 memories for \"deploy\"" whole) "a wide budget packs every hit")
        (is (not (search "did not fit" whole)) "and leaves nothing out")
        (is (search "1 memory for \"deploy\"" small) "a narrow budget keeps the best hit")
        (is (search "2 more did not fit" small) "and says how many it left out")
        (is (search "20-token budget" small) "over the budget it was given")
        (is (search "the deploy runs from the laptop with kamal" whole) "the best hit's own words")))))

(deftest memos-cell-the-boundary-refuses-a-call-it-does-not-satisfy ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file*))
      (is (signals-error nlk:contract-refusal (memos-tool "memos_store")) "a store without its fact is refused before the body")
      (is (signals-error nlk:contract-refusal
                         (memos-tool "memos_store" "text" "x" "memory_type" "hunch")) "and a type outside the five")
      (is-carrying (text (memos-tool "memos_store" "text" "   "))
        ("ERROR: memos:" "an empty fact is the body's refusal")
        ("needs words" "and says what it wanted"))
      (is (equal '() (memos-lines)) "neither refusal wrote a line")
      (is (search "ERROR: memos:" (memos-tool "memos_history" "memory_id" "mem:nosuchid")) "a tool that refuses answers the model, never crashes the turn"))))

;;; --- data outlives the cell ----------------------------------------------------

(deftest memos-cell-facts-outlive-a-restart ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file*))
      (memos-tool "memos_store" "text" "the release branch is cut on the first monday"))
    (with-cell-stop ((memos-start "file" *memos-file*))
      (is (search "first monday" (memos-tool "memos_search" "query" "when is the release branch cut")) "a fact kept before the restart answers after it"))))

;;; --- standing, and /memos ----------------------------------------------------------

(deftest memos-cell-standing-facts-ride-the-prompt-when-asked ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file* "standing" 400))
      (memos-tool "memos_store" "text" "the operator answers to Sam and works in the Oslo office")
      (let ((section (cell-section "s1" "memos")))
        (is (search "Oslo office" section) "the newest fact stands in every request")
        (is (search "memos_search" section) "and names how to find more")))
    (with-cell-stop ((memos-start "file" *memos-file*))
      (is (null (cell-section "s1" "memos")) "standing 0 keeps the facts for the tools alone"))))

(deftest memos-cell-the-command-shows-the-store-remembers-and-packs ()
  (with-memos-home
    (with-cell-stop ((memos-start "file" *memos-file*))
      (let ((state (cell-entry "nodecode-memos" "memos" ""))
            (dialog nil))
        (setf dialog (nth-value 1 (cell-entry "nodecode-memos" "memos" "")))
        (is (search "MemOS memory: 0 live facts" state) "an empty store says so")
        (is (search "memories.jsonl" state) "and where the facts are")
        (is (search "/memos store" (nlk:encode-json-object dialog)) "a row remembers one fact")
        (is (search "remembered [" (cell-entry "nodecode-memos" "memos" "store the api lives at api.example.test")) "store TEXT keeps it")
        (is (search "api.example.test" (cell-entry "nodecode-memos" "memos" "recent")) "recent lists it")
        (is (search "1 memory for \"where is the api\"" (cell-entry "nodecode-memos" "memos" "where is the api")) "anything else is a query, packed")
        (is (search "1 live fact" (cell-entry "nodecode-memos" "memos" "")) "and the state counts it")))))
