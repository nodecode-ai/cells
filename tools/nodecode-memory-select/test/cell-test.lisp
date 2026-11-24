;;;; cell-test.lisp --- lifecycle, the ranking, the budget, the reasons, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and the decision it makes:
;;;; that a start installs the tool and a stop takes it back; that a memory
;;;; answering the query is kept and one sharing no word with it is dropped
;;;; for that reason; that the budget, the item count, the scope, the expiry,
;;;; a duplicate id and a near-duplicate text each stop one memory and say
;;;; which; that freshness breaks a tie and an old memory loses one; that a
;;;; key in a memory answers as a marker; that the tool answers JSON over the
;;;; wire; and that a memory without an id, or a query without a request,
;;;; refuses by name.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "memory-select"
  (:config "max_results" 10)
  (:help :memory-select)
  (:running
   (is (nle::find-tool "memory_select") "memory_select is registered")
   (is (= 10 (nodecode-memory-select::setting :max-results)) "the section's max_results is the one in force"))
  (:stopped
   (is (null (nle::find-tool "memory_select")) "stop unregisters the tool"))
  (:refused
   ("max_results" 0)
   ("max_tokens" 0)
   ("max_results" "many"))
  (:idle nodecode-memory-select:memory-select-error
   (memory-select:select "anything" '())))

;;; --- what is kept, and why -------------------------------------------------------

(deftest memory-select-cell-keeps-the-memory-that-answers-the-query ()
  (with-memory-select ()
    (let ((selection (memory-select:select
                      "where does the app keep its data"
                      (list (memory-select-memory "db" "The app keeps its data in PostgreSQL"
                                                  "importance" 0.7)
                            (memory-select-memory "ui" "The settings page uses a dark theme")))))
      (is (equal '("db") (memory-select-kept selection)) "the database memory is the one kept")
      (is (equal "low_relevance" (memory-select-dropped selection "ui")) "the unrelated one is dropped, and says why")
      (is (search "relevance=" (first (memory-select:selected-memory-reasons
                                       (first (memory-select:selection-selected selection))))) "a kept memory says what it was scored on")
      (is (plusp (memory-select:selection-total-tokens selection)) "and what it costs in tokens"))))

(deftest memory-select-cell-a-plist-from-eval-selects-as-a-tool-call-does ()
  (with-memory-select ()
    (let ((selection (memory-select:select "database"
                                           (list (list :id "a" :text "the database is PostgreSQL")))))
      (is (equal '("a") (memory-select-kept selection)) "a plist reads like a decoded object")
      (is (search "relevance=1.00" (first (memory-select:selected-memory-reasons
                                            (first (memory-select:selection-selected selection))))) "every query word is in the memory"))))

(deftest memory-select-cell-freshness-breaks-a-tie ()
  (with-memory-select ()
    (let ((selection (memory-select:select
                      "database"
                      (list (memory-select-memory "old" "the database is old" "created_at" "2020-01-01T00:00:00Z")
                            (memory-select-memory "new" "the database is new" "created_at" "2026-11-01T00:00:00Z")))))
      (is (equal '("new" "old") (memory-select-kept selection)) "the newer of two equally relevant memories goes first"))))

;;; --- what is stopped, and why ----------------------------------------------------

(deftest memory-select-cell-the-budget-stops-the-list ()
  (with-memory-select ()
    (let ((selection (memory-select:select
                      "database"
                      (list (memory-select-memory "a" "database one")
                            (memory-select-memory "b" "database two"))
                      :max-tokens 4)))
      (is (equal '("a") (memory-select-kept selection)) "the first memory fits and the second does not")
      (is (<= (memory-select:selection-total-tokens selection) 4) "the kept list stays inside the budget")
      (is (equal "budget" (memory-select-dropped selection "b")) "the one left behind says the budget stopped it"))))

(deftest memory-select-cell-the-item-count-stops-the-list ()
  (with-memory-select ()
    (let ((selection (memory-select:select
                      "database"
                      (list (memory-select-memory "a" "database one")
                            (memory-select-memory "b" "database two")
                            (memory-select-memory "c" "database three"))
                      :max-memories 2)))
      (is (= 2 (length (memory-select-kept selection))) "two memories are kept")
      (is (equal "max_items" (memory-select-dropped selection "c")) "the third says the count stopped it"))))

(deftest memory-select-cell-scope-expiry-and-a-repeat-are-each-named ()
  (with-memory-select ()
    (let ((selection (memory-select:select
                      "database"
                      (list (memory-select-memory "a" "database one")
                            (memory-select-memory "elsewhere" "database two" "scope" "another-user")
                            (memory-select-memory "stale" "database three" "expires_at" "2020-01-01T00:00:00Z")
                            (memory-select-memory "a" "database again")
                            (memory-select-memory "lateaug" "database four"
                                                  "created_at" "2026-08-01T00:00:00Z"))
                      :now (encode-universal-time 0 0 12 1 11 2026 0))))
      (is (equal "out_of_scope" (memory-select-dropped selection "elsewhere")) "another scope never selects")
      (is (equal "expired" (memory-select-dropped selection "stale")) "an expired memory is dropped, and says when")
      (is (equal "duplicate_input" (memory-select-dropped selection "a")) "the second memory with the same id is dropped")
      (is (member "lateaug" (memory-select-kept selection) :test #'string=) "an old memory that answers the query is still kept"))))

(deftest memory-select-cell-near-duplicates-keep-the-first ()
  (with-memory-select ()
    (let ((selection (memory-select:select
                      "database host"
                      (list (memory-select-memory "one" "the database runs on the build host" "importance" 0.9)
                            (memory-select-memory "two" "the database runs on the build host again")))))
      (is (equal '("one") (memory-select-kept selection)) "the higher-scoring of two near-identical memories is kept")
      (is (equal "duplicate_of:one" (memory-select-dropped selection "two")) "and the other names the one it repeats"))))

(deftest memory-select-cell-a-contradiction-is-marked-not-dropped ()
  (with-memory-select ()
    (let* ((selection (memory-select:select
                        "which database"
                        (list (memory-select-memory "sql" "the database is MySQL")
                              (memory-select-memory "pg" "the database is PostgreSQL" "importance" 0.9))))
           (reasons (format nil "~{~a~^ ~}"
                            (loop for memory in (memory-select:selection-selected selection)
                                  append (memory-select:selected-memory-reasons memory)))))
      (is (equal '("pg" "sql") (memory-select-kept selection)) "both memories are kept, the more important first")
      (is (search "possible_conflict:" reasons) "and one of them says the two name different databases"))))

(deftest memory-select-cell-a-key-in-a-memory-answers-as-a-marker ()
  (with-memory-select ()
    (let* ((selection (memory-select:select
                        "api key"
                        (list (memory-select-memory "k" "the api key is sk-ant-api03-abcdefghijklmnop"))))
           (memory (first (memory-select:selection-selected selection))))
      (is (search "[REDACTED_API_KEY]" (memory-select:selected-memory-text memory)) "the key answers as a marker")
      (is (null (search "abcdefghijklmnop" (memory-select:selected-memory-text memory))) "and the key itself does not")
      (is (search "[REDACTED_API_KEY]" (format nil "~a" (memory-select:to-json selection))) "the tool's own answer carries the marker"))))

;;; --- the boundary ---------------------------------------------------------------

(deftest memory-select-cell-the-tool-answers-one-json-object ()
  (with-memory-select ()
    (let* ((text (memory-select-tool-call
                  "{\"query\":\"database\",\"memories\":[{\"id\":\"a\",\"text\":\"the database is PostgreSQL\"}]}"))
           (answer (nlk:decode-json text)))
      (is (hash-table-p answer) "the answer is one JSON object")
      (is (search "\"id\":\"a\"" text) "the kept memory is in it")
      (is (search "\"total_tokens\":" text) "with the tokens it costs")
      (is (search "\"dropped\":[]" text) "and nothing dropped"))))

(deftest memory-select-cell-a-query-is-required-at-the-boundary ()
  (is (signals-error nlk:contract-refusal
                      (nlk:contract-decode (nlk:contract 'nodecode-memory-select::memory-selection-request)
                                           (nlk:make-json-object "memories" (vector)))) "a request without a query is refused before the tool body runs"))

(deftest memory-select-cell-a-memory-without-an-id-refuses-by-name ()
  (with-memory-select ()
    (let ((condition (signals-error nodecode-memory-select:memory-select-error
                         (memory-select:select "database" (list (nlk:make-json-object "text" "no id here"))))))
      (is (search "has no id" (princ-to-string condition)) "the refusal says what the memory needs"))))
