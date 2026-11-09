;;;; cell-test.lisp --- a card's contract, its proof, its units, its budget, the command.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's wiring over a real board file and a real
;;;; shell: that a card opens only on a complete contract and a gap is refused
;;;; by name; that a draft cannot be proved; that a proof decides the card and
;;;; every run is kept; that a unit is marked only with evidence; that a
;;;; hardline command is refused before it can run unattended; that a budget
;;;; stops a proof; that the board and one card report why they stand where
;;;; they do; and that /crew answers the board and its rows.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "crew"
  (:help :crew)
  (:command "crew")
  (:running (is (equal nodecode-crew::+primer+ (nle:help :crew)))
            (is (= 300 (nodecode-crew::setting :proof-seconds)) "a proof runs five minutes at most")
            (is (= 0 (nodecode-crew::setting :budget-tokens)) "no budget unless a card or the section names one")
            (is (equal "board" (nodecode-crew::setting :board))))
  (:refused ("proof_seconds" 0) ("budget_tokens" -1))
  (:idle crew:crew-error (crew:board) (crew:diagnose) (crew:proof-check "make") (crew:verdict "crew-1")))

;;; --- the contract ------------------------------------------------------------------

(deftest crew-cell-a-card-opens-only-on-a-complete-contract ()
  (with-crew (:board "contract")
    (is-carrying (answer (refusal-text crew:crew-error
                                       (crew:plan :title "Ship it" :goal "the page"
                                                  :artifact "site/index.html"
                                                  :done-when "it loads at /")))
                 "the contract is missing proof")
    (is (null (getf (crew-board) :cards)) "a refused plan writes no card")
    (is-carrying (answer (refusal-text crew:crew-error (crew:plan :title " " :goal "g")))
                 "the contract is missing")
    (let ((id (crew-new)))
      (is (equal "open" (nodecode-crew::open-state (crew-card id))) "a complete card is open")
      (is-carrying (answer (crew:contract id)) "is a complete card" "true"))))

(deftest crew-cell-a-draft-cannot-be-proved-and-the-intake-fills-it ()
  (with-crew (:board "draft")
    (is-carrying (answer (nodecode-crew::draft "Half a card")) "opened" "draft")
    (let ((id (crew-last-id)))
      (is (equal "draft" (nodecode-crew::open-state (crew-card id))) "a card with no contract is a draft")
      (is-carrying (answer (refusal-text crew:crew-error (crew:verdict id)))
                   "is a draft" "there is no proof to run")
      (is-carrying (answer (crew:contract id :goal "the marker" :artifact "marker"
                                          :done-when "it is there" :proof "true"))
                   "is a complete card")
      (is (equal "open" (nodecode-crew::open-state (crew-card id)))))))

;;; --- the proof ---------------------------------------------------------------------

(deftest crew-cell-the-proof-decides-the-card-and-every-run-is-kept ()
  (with-crew (:board "proof")
    (crew-untouch "crew-test-marker")
    (let ((id (crew-new :title "Make the marker" :proof "test -f crew-test-marker")))
      (is-carrying (answer (crew:verdict id)) "FAIL" "exited 1")
      (is (equal "open" (nodecode-crew::open-state (crew-card id))) "a failed proof leaves it open")
      (crew-touch "crew-test-marker")
      (is-carrying (answer (crew:verdict id)) "PASS" "exited 0")
      (is (equal "passed" (nodecode-crew::open-state (crew-card id))) "a proof that exits 0 passes it")
      (is (= 2 (length (crew-runs id))) "every run is kept")
      (is (equal '("FAIL" "PASS") (mapcar (lambda (run) (getf run :verdict)) (crew-runs id))) "the log reads in the order they ran")
      (is (equal 1 (getf (first (crew-runs id)) :status)) "a failed run keeps its exit status")
      (crew-untouch "crew-test-marker"))))

;;; --- the units ---------------------------------------------------------------------

(deftest crew-cell-a-unit-is-marked-only-with-evidence ()
  (with-crew (:board "units")
    (let ((id (crew-new)))
      (is-carrying (answer (crew:progress id)) "has no unit of work yet" "crew:progress")
      (is-carrying (answer (refusal-text crew:crew-error
                                         (crew:progress id :name "the tests" :result "pass")))
                   "marked with evidence")
      (is-carrying (answer (refusal-text crew:crew-error
                                         (crew:progress id :name "the tests" :result "done"
                                                        :evidence "it ran")))
                   "pass" "fail")
      (is-carrying (answer (crew:progress id :name "the tests" :result "pass"
                                          :evidence "make test printed 12 passing"))
                   "the tests" "passed" "12 passing")
      (is-carrying (answer (crew:progress id :name "the page" :result "fail"
                                          :evidence "404 at /"))
                   "the page" "failed")
      (is-carrying (answer (crew:progress id)) "1 of 2 units passed" "1 failed"))))

;;; --- the floors and the budget -------------------------------------------------------

(deftest crew-cell-a-hardline-proof-is-refused-before-it-can-run ()
  (with-crew (:board "hardline")
    (is-carrying (answer (crew:proof-check "make test")) "ok" "no hardline command")
    (is-carrying (answer (crew:proof-check "rm -rf / --no-preserve-root"))
                 "hardline" "wipes the root")
    (is-carrying (answer (refusal-text crew:crew-error
                                       (crew:plan :title "Wipe it" :goal "gone"
                                                  :artifact "the disk" :done-when "it is empty"
                                                  :proof "mkfs.ext4 /dev/sda1")))
                 "hardline" "formats a filesystem")
    (is (null (getf (crew-board) :cards)) "a hardline proof writes no card")))

(deftest crew-cell-a-spent-budget-refuses-the-next-proof ()
  (with-crew (:board "budget")
    (let ((id (crew-new :proof "true" :budget 100)))
      (is-carrying (answer (crew:verdict id)) "PASS")
      (let ((second (crew-new :title "another card" :proof "true" :budget 100)))
        (crew-spend second 150)
        (is-carrying (answer (refusal-text crew:crew-error (crew:verdict second)))
                     "spent its budget" "150 of 100 tokens")
        (is (null (crew-runs second)) "and records no run")))))

;;; --- the report ----------------------------------------------------------------------

(deftest crew-cell-the-board-and-one-card-say-why-they-stand-there ()
  (with-crew (:board "report")
    (let ((passed (crew-new :title "Ship the page"))
          (draft (progn (nodecode-crew::draft "Still vague") (crew-last-id))))
      (crew:verdict passed)
      (is-carrying (answer (crew:diagnose passed))
                   "passed" "its proof `true` exited 0" "Ship the page")
      (is-carrying (answer (crew:diagnose draft))
                   "a draft" "goal, artifact, done when, proof")
      (is-carrying (answer (crew:diagnose)) "crew board report" "2 cards" "Ship the page")
      (is-carrying (answer (crew:board)) "crew board report" "passed" "Still vague")
      (is-carrying (answer (refusal-text crew:crew-error (crew:diagnose "crew-99")))
                   "no card" "crew-1"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest crew-cell-the-command-answers-the-board-and-its-rows ()
  (with-crew (:board "slash")
    (multiple-value-bind (answer dialog) (nodecode-crew::run-slash "" nil)
      (is-carrying answer "crew board slash" "no card on it")
      (is dialog "a bare /crew offers its rows"))
    (multiple-value-bind (answer dialog) (nodecode-crew::run-slash "open Write the README" nil)
      (is-carrying answer "opened" "as a draft")
      (is (null dialog) "an option answers text alone"))
    (let ((id (crew-last-id)))
      (is-carrying (answer (crew:contract id :goal "the README" :artifact "README.md"
                                         :done-when "it is there" :proof "true"))
                   "is a complete card")
      (is-carrying (answer (nodecode-crew::run-slash (format nil "prove ~a" id) nil))
                   "PASS" "exited 0")
      (is-carrying (answer (nodecode-crew::run-slash (format nil "diagnose ~a" id) nil))
                   "passed" "Write the README")
      (is-carrying (answer (nodecode-crew::run-slash (format nil "drop ~a" id) nil))
                   "dropped")
      (is (equal "dropped" (nodecode-crew::open-state (crew-card id))) "a dropped card keeps its history")
      (is-carrying (answer (nodecode-crew::run-slash "nonsense" nil))
                   "no /crew option" "open, prove, diagnose or drop"))))
