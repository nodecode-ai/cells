;;;; cell-test.lisp --- the lifecycle, the nine verbs, the file, /factledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops and its settings refuse when
;;;; they are out of shape; the factledger tool answers every action through
;;;; the seam the model calls it by; a new value closes the row it supersedes
;;;; and the closed row stays as history, readable as of a date; a fact
;;;; without a source is a refusal; the file is the original's TSV, header
;;;; and all; a compiled view stays inside its budget; search, stale, export,
;;;; stats and the integrity check read the same file; and /factledger
;;;; answers its verbs. Every call runs against a ledger in the test's own
;;;; home, so nothing here reads the operator's facts.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(define-test-slice "factledger" "FACTLEDGER-CELL-" :start nodecode-factledger:start-cell)

(defun factledger-file ()
  "The ledger this slice runs on: a file of the test's own, never the operator's."
  (namestring (nlk:home "factledger-test/facts.tsv")))

(defun factledger-forget ()
  (let ((path (factledger-file)))
    (when (probe-file path) (delete-file path))))

(defun factledger-tool (arguments)
  "Run the registered factledger tool with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool "factledger")))
    (assert entry () "no tool named factledger")
    (funcall (third entry) (nle::tool-body-arguments entry arguments))))

(defun factledger-answer (arguments)
  "The tool's answer to ARGUMENTS, decoded."
  (cell-json (factledger-tool arguments)))

(defun factledger-add (subject predicate value source &rest pairs)
  "Add one fact to the market scope through the tool. => the decoded answer."
  (factledger-answer
   (apply #'nlk:make-json-object
          (append (list "action" "add" "scope" "market" "subject" subject
                        "predicate" predicate "value" value "source" source)
                  pairs))))

(defun factledger-facts (arguments)
  "The rows ACTION answers, as a list."
  (coerce (nlk:json-value (factledger-answer arguments) :array "data") 'list))

(defun factledger-header-line ()
  "The ledger file's header: the ten columns, in order, tab-separated."
  (with-output-to-string (out)
    (loop for name in nodecode-factledger:+fields+
          for first = t then nil
          do (unless first (write-char #\Tab out))
             (write-string name out))))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "factledger"
  (:config "path" (factledger-file))
  (:help :factledger)
  (:command "factledger")
  (:running (is (nle::find-tool "factledger") "the factledger tool is registered")
            (is (string= (factledger-file)
                         (nodecode-factledger::ledger-file)) "the ledger is the file the section names"))
  (:stopped (is (null (nle::find-tool "factledger")) "stop unregisters the tool"))
  (:refused ("stale_days" 0) ("order" "random"))
  (:idle nodecode-factledger:factledger-error (nodecode-factledger::run-command "" "s1")))

;;; --- the tool -----------------------------------------------------------------

(deftest factledger-cell-a-fact-is-superseded-with-its-history ()
  (factledger-forget)
  (with-cell-stop ((factledger-start "path" (factledger-file)))
    (let ((first (factledger-add "competitor.x" "price" "10" "price list 2026-01"
                                  "valid_from" "2026-01-01")))
      (is (string= "added" (nlk:json-value first :string "data" "status")) "the first value is added")
      (is (string= "f00001" (nlk:json-value first :string "data" "id")) "it takes the first id")
      (is (string= "stated" (nlk:json-value first :string "data" "row" "confidence")) "confidence defaults to stated"))
    (let ((again (factledger-add "competitor.x" "price" "10" "price list 2026-02"
                                 "valid_from" "2026-02-01")))
      (is (string= "unchanged" (nlk:json-value again :string "data" "status")) "the same value again changes nothing")
      (is (string= "f00001" (nlk:json-value again :string "data" "id")) "and keeps the one row"))
    (let ((second (factledger-add "competitor.x" "price" "12" "price list 2026-06"
                                  "confidence" "derived" "valid_from" "2026-06-01")))
      (is (string= "closed" (nlk:json-value second :string "data" "status")) "a new value closes the row it supersedes")
      (is (string= "f00002" (nlk:json-value second :string "data" "id")) "as a row of its own")
      (is (equal '("f00001")
                 (coerce (nlk:json-value second :array "data" "closed") 'list)) "and says which row it closed"))
    (let ((then (factledger-facts (nlk:json-object "action" "show" "scope" "market"
                                                    "as_of" "2026-03-01")))
          (now (factledger-facts (nlk:json-object "action" "show" "scope" "market"
                                                   "as_of" "2026-07-01"))))
      (is (equal '("10") (mapcar (lambda (row) (nlk:json-value row :string "value")) then)) "as of March the old value still holds, alone")
      (is (equal '("12") (mapcar (lambda (row) (nlk:json-value row :string "value")) now)) "and as of July only the new one does"))
    (let ((history (factledger-facts (nlk:json-object "action" "history"
                                                       "subject" "competitor.x"))))
      (is (= 2 (length history)) "the history keeps both rows")
      (is (string= "2026-06-01" (nlk:json-value (first history) :string "valid_to")) "the closed row keeps the date it closed")
      (is (string= "f00002" (nlk:json-value (first history) :string "superseded_by")) "and names its successor")
      (is (string= "price list 2026-01" (nlk:json-value (first history) :string "source")) "and the source it was written with"))
    (is-carrying (text (factledger-tool (nlk:json-object "action" "add" "scope" "market"
                                                          "subject" "competitor.x"
                                                          "predicate" "price" "value" "13")))
                 ("ERROR:" "a fact without a source is a refusal")
                 ("source" "and it names what is missing"))
    (is (signals-error nlk:contract-refusal
          (factledger-tool (nlk:json-object "action" "add" "scope" "market"
                                             "subject" "competitor.x" "predicate" "price"
                                             "value" "13" "source" "guess"
                                             "confidence" "maybe"))) "the contract refuses a confidence outside its three before the verb runs")
    (is (signals-error nodecode-factledger:factledger-error
          (nodecode-factledger:add-fact "market" "competitor.x" "price" "13" "guess"
                                         :confidence "maybe")) "and the verb refuses it too, for eval and the command")
    (is (signals-error nlk:contract-refusal
          (factledger-tool (nlk:json-object "scope" "market"))) "a call that names no action is refused before any verb runs")
    (let ((check (factledger-answer (nlk:json-object "action" "check"))))
      (is (eq t (nlk:json-value check :boolean "data" "healthy")) "the ledger the history was written to is healthy")
      (is (= 0 (length (coerce (nlk:json-value check :array "data" "problems") 'list))) "with no problem to name"))))

(deftest factledger-cell-search-stale-export-and-totals-read-one-file ()
  (factledger-forget)
  (with-cell-stop ((factledger-start "path" (factledger-file)))
    (factledger-add "competitor.x" "price" "10" "price list" "valid_from" "2020-01-01")
    (factledger-answer (nlk:json-object "action" "add" "scope" "legal" "subject" "case.y"
                                         "predicate" "status" "value" "open"
                                         "source" "filing 2020-02" "valid_from" "2020-02-01"))
    (is (equal '("competitor.x")
               (mapcar (lambda (row) (nlk:json-value row :string "subject"))
                       (factledger-facts (nlk:json-object "action" "search"
                                                           "term" "competitor")))) "search finds a row by its subject")
    (is (equal '("case.y")
               (mapcar (lambda (row) (nlk:json-value row :string "subject"))
                       (factledger-facts (nlk:json-object "action" "search" "term" "filing")))) "and by its source")
    (let ((stale (factledger-facts (nlk:json-object "action" "stale" "days" 30))))
      (is (= 2 (length stale)) "both facts are old enough to re-check")
      (is (plusp (nlk:json-value (first stale) :integer "age_days")) "and their age is counted")
      (is (string= "competitor.x" (nlk:json-value (first stale) :string "row" "subject")) "the oldest first"))
    (is (= 2 (length (factledger-facts (nlk:json-object "action" "export")))) "export hands over the active rows")
    (is (equal '("case.y")
               (mapcar (lambda (row) (nlk:json-value row :string "subject"))
                       (factledger-facts (nlk:json-object "action" "export" "scopes" "legal")))) "a scope narrows it")
    (let ((stats (factledger-answer (nlk:json-object "action" "stats"))))
      (is (= 2 (nlk:json-value stats :integer "data" "total")) "the totals count every row")
      (is (= 2 (nlk:json-value stats :integer "data" "active")) "both rows hold today")
      (is (= 0 (nlk:json-value stats :integer "data" "closed")) "none is closed")
      (is (equal '("legal" "market")
                 (coerce (nlk:json-value stats :array "data" "scopes") 'list)) "and the scopes are named"))))

(deftest factledger-cell-a-compiled-view-stays-inside-its-budget ()
  (factledger-forget)
  (with-cell-stop ((factledger-start "path" (factledger-file)))
    (factledger-add "competitor.x" "price" "one" "list" "valid_from" "2026-01-01")
    (factledger-add "competitor.x" "restock" "two" "list" "confidence" "derived"
                    "valid_from" "2026-02-01")
    (factledger-add "competitor.x" "vendor" "three" "list" "confidence" "assumed"
                    "valid_from" "2026-03-01")
    (is-carrying (view (nlk:json-value (factledger-answer
                                        (nlk:json-object "action" "compile" "budget" 45))
                                       :string "data" "view"))
                 ("price = one" "the firmest fact is kept first")
                 (:absent "restock = two" "and the budget leaves the next one out")
                 ("(+2 more" "counting what the view did not carry")
                 ("factledger show" "and saying where the rest is"))
    (let ((whole (nlk:json-value (factledger-answer (nlk:json-object "action" "compile"))
                                  :string "data" "view")))
      (is (search "price = one" whole) "a view with room carries the stated fact")
      (is (< (search "price = one" whole) (search "vendor = three" whole)) "stated first, the unstated last")
      (is (search "restock = two" whole) "and every fact in between"))))

(deftest factledger-cell-the-ledger-is-the-originals-tsv ()
  (factledger-forget)
  (with-cell-stop ((factledger-start "path" (factledger-file)))
    (factledger-add "competitor.x" "price" "10" "price list" "valid_from" "2026-01-01")
    (factledger-add "competitor.x" "price" "12" "price list" "valid_from" "2026-06-01")
    (let* ((text (uiop:read-file-string (factledger-file)))
           (lines (nlk:lines text)))
      (is (string= (factledger-header-line) (first lines)) "the header names the original's ten columns")
      (is (= 3 (length lines)) "one line a row, the closed row kept")
      (is (search "2026-06-01" (second lines)) "the closed row carries the date it closed")
      (is (search "f00002" (second lines)) "and the row that superseded it")
      (is (search "f00002" (third lines)) "which the new row is"))
    (let ((history (factledger-facts (nlk:json-object "action" "history"
                                                       "subject" "competitor.x"))))
      (is (= 2 (length history)) "a later call reads the file back whole"))))

;;; --- /factledger ---------------------------------------------------------------

(deftest factledger-cell-the-command-reads-and-records ()
  (factledger-forget)
  (with-cell-stop ((factledger-start "path" (factledger-file)))
    (multiple-value-bind (text dialog)
        (cell-entry "nodecode-factledger" "factledger" "" "s1")
      (is (search "factledger: 0 facts" text) "bare /factledger counts an empty ledger")
      (is dialog "and carries a panel of the ledger's verbs"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger"
                                   "add market | competitor.x | price | 10 | price list" "s1"))
                 ("recorded f00001" "add takes five parts split by |"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger" "show" "s1"))
                 ("price = 10" "show reads what holds")
                 ("price list" "with its source"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger"
                                   "add market | competitor.x | price | 12 | price list 2" "s1"))
                 ("closing f00001" "a new value says which row it closed"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger" "history competitor.x" "s1"))
                 ("f00001" "history names the closed row")
                 ("f00002" "and the row that holds"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger" "stats" "s1"))
                 ("2 facts, 1 active, 1 closed" "stats counts every row and the one holding"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger" "check" "s1"))
                 ("healthy" "the ledger checks clean"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger"
                                   "add market | case.y" "s1"))
                 ("five parts split by" "an add of the wrong shape refuses and says the shape"))
    (is-carrying (text (cell-entry "nodecode-factledger" "factledger" "nonsense" "s1"))
                 ("names no /factledger verb" "and a verb it does not know names the ones it does"))))
