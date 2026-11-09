;;;; support.lisp --- crew test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Crew tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CREW-CELL-
;;;; name prefix; RUN-CREW-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed: a card is a real file under the scratch
;;;; home and a proof is a real shell command, run through NLK:RUN-BOUNDED.

(in-package #:nodecode.test)

(define-test-slice "crew" "CREW-CELL-" :start nodecode-crew:start-cell)

(defmacro with-crew ((&key (board "test")) &body body)
  "BODY with the cell started on a board of its own, so one test's cards are
not another's."
  `(with-cell-stop ((crew-start "board" ,board))
     ,@body))

(defun crew-answer (value)
  "VALUE as the text an answer or a refusal prints."
  (if (stringp value) value (princ-to-string value)))

(defun crew-board ()
  "The board as it is on disk."
  (nodecode-crew::load-board))

(defun crew-card (id)
  "The card named ID on the board, or NIL."
  (find id (getf (crew-board) :cards) :key (lambda (card) (getf card :id)) :test #'equal))

(defun crew-last-id ()
  "The id of the newest card on the board."
  (getf (first (getf (crew-board) :cards)) :id))

(defun crew-new (&key (title "a card") (goal "the goal it delivers") (artifact "artifact.txt")
                       (done-when "the artifact is there") (proof "true") budget)
  "Open a card through the verb and answer its id."
  (crew:plan :title title :goal goal :artifact artifact :done-when done-when
             :proof proof :budget budget)
  (crew-last-id))

(defun crew-touch (name)
  "Make the file NAME in the home, so a proof can see it; answer its path."
  (let ((path (nlk:home name)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-line "ok" out))
    path))

(defun crew-untouch (name)
  "Remove the file NAME from the home when it is there."
  (let ((path (nlk:home name)))
    (when (probe-file path) (delete-file path))))

(defun crew-spend (id tokens)
  "Leave card ID as a turn that spent TOKENS would, so a budget can be proved."
  (let* ((board (crew-board))
         (card (find id (getf board :cards) :key (lambda (c) (getf c :id)) :test #'equal)))
    (when card
      (setf (getf card :spent) tokens)
      (nodecode-crew::save-board board))
    card))

(defun crew-runs (id)
  "Card ID's verdict log, oldest first."
  (getf (crew-card id) :runs))
