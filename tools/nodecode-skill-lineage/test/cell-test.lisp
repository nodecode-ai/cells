;;;; cell-test.lisp --- the record, its queries, and the chain under them.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here reaches a network or the operator's chain: every test binds a
;;;; folder of its own, so a write in one test is invisible to the next. What is
;;;; proved is what the cell promises -- a strong edge where text was shared, a
;;;; URL that leads to the versions it produced, an operative token with no
;;;; origin reported rather than smoothed over, a tampered line that verifies as
;;;; BROKEN, and a head hash that tells truncation from an intact chain.

(in-package #:nodecode.test)

(define-test-slice "skill-lineage" "LINEAGE-CELL-" :start nodecode-skill-lineage:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "skill-lineage"
  (:config "strong_min" 3)
  (:help :lineage)
  (:command "lineage")
  (:refused ("strong_min" 0))
  (:idle lineage:skill-lineage-error (lineage:status)))

;;; --- a folder of its own, and the two texts the tests share ----------------

(defparameter +page+
  "the release notes say the cache must be warmed before the first request and the retry loop spins forever when the cache is stale"
  "A page a session read, as a source.")

(defparameter +after+
  (format nil "~a~%Always warm the cache before the first request: the retry loop spins forever on a stale cache."
          +page+)
  "The skill body a write produced: part of it came from +PAGE+.")

(defun lineage-test-dir ()
  "A fresh chain folder for one test."
  (let ((dir (merge-pathnames (format nil "lineage-test/~a/" (nlk:make-durable-id "run"))
                               (nlk:home ""))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-lineage ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS and recording into a folder of its own."
  `(let ((nodecode-skill-lineage::*data-dir* (lineage-test-dir))
         (nodecode-skill-lineage::*key* nil))
     (with-cell-stop ((skill-lineage-start ,@pairs))
       ,@body)))

;;; --- the record ------------------------------------------------------------

(deftest lineage-cell-a-shared-body-is-a-strong-edge ()
  (with-lineage ()
    (is (search "empty - nothing recorded yet" (lineage:status)) "a fresh chain says so")
    (let ((answer (lineage:record "deploy" :after +after+ :sources (list +page+))))
      (is (search "recorded skill deploy" answer) "the write is recorded")
      (is (search "strong" answer) "and the page it drew on is a strong edge"))
    (let ((traced (lineage:trace "deploy")))
      (is (search "skill deploy" traced) "the version is the one traced")
      (is (search "[strong" traced) "its origin is the strong edge")
      (is (search "src:ext:" traced) "named as a source node"))
    (is (search "INTACT" (lineage:verify)) "the chain verifies")
    (is (search "2 entries" (lineage:status)) "one source and one version are the chain")
    (is (search "1 skill version" (lineage:status)) "and one version is what it holds")
    (is (search "seq 2 hash" (lineage:head)) "the head is the newest line")))

(deftest lineage-cell-a-url-leads-to-the-versions-it-produced ()
  (with-lineage ()
    (lineage:record "deploy" :after +after+
                     :sources (list (format nil "~a~%see https://example.com/warm" +page+)))
    (let ((found (lineage:descendants "https://example.com/warm")))
      (is (search "LIVE skill deploy" found) "the URL reaches the live version")
      (is (search "strong" found) "through the edge it made"))
    (is (search "LIVE skill deploy" (lineage:descendants "deploy")) "a skill name reaches its versions too")
    (is (search "nothing recorded"
                (refusal-text lineage:skill-lineage-error
                               (lineage:descendants "https://nowhere.example.org/x"))) "an unknown target says so")))

(deftest lineage-cell-an-operative-token-with-no-origin-is-reported ()
  (with-lineage ()
    (is (search "no skill versions with untraceable" (lineage:report)) "an empty record flags nothing")
    (let ((answer (lineage:record "notes"
                                  :after "Run curl https://elsewhere.example.net/x to fetch it."
                                  :sources (list +page+))))
      (is (search "orphan" answer) "the URL no source carries is an orphan"))
    (let ((reported (lineage:report)))
      (is (search "skill notes" reported) "the version is reported")
      (is (search "no traceable origin" reported) "with the reason")
      (is (search "(lineage:trace" reported) "and how to look further"))))

(deftest lineage-cell-a-version-is-recorded-once ()
  (with-lineage ()
    (lineage:record "deploy" :after +after+ :sources (list +page+))
    (is (search "nothing written" (lineage:record "deploy" :after +after+ :sources (list +page+))) "a body already recorded is not written twice")
    (is (search "2 entries" (lineage:status)) "the second call added nothing")
    (is (search "no skill name"
                (refusal-text lineage:skill-lineage-error
                               (lineage:record "Not A Name" :after "x"))) "a name that is not one refuses")))

;;; --- the chain ------------------------------------------------------------

(deftest lineage-cell-a-tampered-line-does-not-verify ()
  (with-lineage ()
    (lineage:record "deploy" :after +after+ :sources (list +page+))
    (is (search "INTACT" (lineage:verify)) "before the tamper")
    (let* ((path (nodecode-skill-lineage::chain-path))
           (text (nlk:read-text path))
           (at (search "version" text)))
      (unless at (error "the fixture chain has no version line"))
      (nlk:write-file-atomically
       path (concatenate 'string (subseq text 0 at) "Version" (subseq text (+ at 7))))
      (is (search "BROKEN" (lineage:verify)) "the edited line no longer hashes to its entry"))))

(deftest lineage-cell-a-head-hash-tells-truncation-from-an-intact-chain ()
  (with-lineage ()
    (lineage:record "deploy" :after +after+ :sources (list +page+))
    (let* ((answer (lineage:head))
           (hash (subseq answer (length "seq 2 hash "))))
      (is (search "given head found" (lineage:verify :head hash)) "a head recorded elsewhere is found in the chain")
      (is (search "BROKEN" (lineage:verify :head (make-string 64 :initial-element #\a))) "a head the chain does not carry means it was truncated or rewritten"))))

;;; --- the tools and the command ---------------------------------------------

(deftest lineage-cell-the-tools-answer ()
  (with-lineage ()
    (let ((args (nodecode-skill-lineage::decode-lineage-write
                 (nlk:json-object "skill" "deploy" "after" +after+ "sources" (vector +page+)))))
      (is (search "recorded skill deploy" (nodecode-skill-lineage::record-tool args)) "the record tool writes what it was handed"))
    (let ((args (nodecode-skill-lineage::decode-lineage-query
                 (nlk:json-object "action" "trace" "skill" "deploy"))))
      (is (search "skill deploy" (nodecode-skill-lineage::query-tool args)) "the query tool traces a skill"))
    (let ((args (nodecode-skill-lineage::decode-lineage-query
                 (nlk:json-object "action" "verify"))))
      (is (search "INTACT" (nodecode-skill-lineage::query-tool args)) "and verifies the chain"))))

(deftest lineage-cell-the-command-answers-each-verb ()
  (with-lineage ()
    (lineage:record "deploy" :after +after+ :sources (list +page+))
    (is (search "chain:" (cell-entry "nodecode-skill-lineage" "lineage" "" "lineage-test")) "typed alone, it says what is recorded")
    (is (search "skill deploy" (cell-entry "nodecode-skill-lineage" "lineage" "trace deploy" "lineage-test")) "trace names the version")
    (is (search "INTACT" (cell-entry "nodecode-skill-lineage" "lineage" "verify" "lineage-test")) "verify walks the chain")
    (is (search "not one of its verbs"
                (cell-entry "nodecode-skill-lineage" "lineage" "bogus" "lineage-test")) "an unknown verb names itself")))

(deftest lineage-cell-a-torn-tail-is-pointed-at-and-written-past ()
  (with-lineage ()
    (lineage:record "deploy" :after +after+ :sources (list +page+))
    (let ((path (nodecode-skill-lineage::chain-path)))
      (with-open-file (out path :direction :output :if-exists :append
                                :external-format :utf-8)
        (write-string "9	half a line with no terminator" out))
      (is (search "BROKEN at seq 3" (lineage:verify)) "the damaged line is named")
      (is (search "seq 2 hash" (lineage:head)) "the head is the last entry that parses")
      (lineage:record "notes" :after "a second skill body, written after the tear"
                      :sources (list +page+))
      (is (search "3 entries" (lineage:status)) "the write after it lands")
      (is (search "BROKEN at seq 3" (lineage:verify)) "and the damaged line stays where it can be read"))))
