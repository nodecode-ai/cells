;;;; cell.lisp --- the section, the five tools, /mock-review, the board route.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A review board for UI mockups. The model writes a round: one or more
;;;; decisions, each a question with the options it weighed, and a mock page
;;;; for an option it wants seen. The cell serves the round as a page on the
;;;; gateway's own port; the operator marks each decision Approve, Change or
;;;; Skip, may pick the option and say why, and presses Send. The marks are
;;;; stored with the round and answered, to the model, as one markdown
;;;; digest: the same five tools the original plugin gives a model.
;;;;
;;;;   mock_review_create      a review, round 1
;;;;   mock_review_add_round   the next round, same link
;;;;   mock_review_feedback    a round's marks as markdown
;;;;   mock_review_ack         say a round is handled
;;;;   mock_review_list        the open reviews
;;;;
;;;; What the original does beyond this -- public hosting, a wake webhook,
;;;; tailnet links, pins on a live page, design profiles, headless-browser
;;;; checks, passwords -- needs a service or a runtime this cell has no part
;;;; of; NOTICE says which and why.

(in-package #:nodecode-mock-review)

(defparameter *primer*
  "Mock review boards for UI work: the model puts the options it weighed on a
page, the operator marks each decision Approve, Change or Skip (and may pick an
option and say why), and Send brings the marks back as one digest.

  mock_review_create      title, brief and the round's decisions; answers the
                           review id and the board's URL to hand over.
  mock_review_add_round   the next round of the same review, same link.
  mock_review_feedback    a round's marks as markdown (the newest by default).
  mock_review_ack          say a round is handled, so nobody waits on it.
  mock_review_list        the open reviews, with their round and state.

A decision is one question and the two to six options it weighed; an option
may carry a mock, a whole small HTML page, which the board frames at its own
sandboxed origin. (mockreview:feedback \"r-1\") reads one by hand, and
/mock-review lists them in a session. A refusal is ERROR:
MOCK-REVIEW-ERROR and says what to set."
  "What (help :mock-review) answers while the cell runs.")

;;; --- a round's decisions -------------------------------------------------

(nlk:define-contract mock-review-option
  (:doc "One option a decision weighed.")
  ("id" :string :default "" :doc "short id the marks name: a, b, c; assigned in order when left out")
  ("label" :text :required t :doc "the option as the operator reads it, one line")
  ("mock" :string :default ""
          :doc "a whole small HTML page showing this option, at most 200 KB; frames with scripts on, same-origin off"))

(nlk:define-contract mock-review-decision
  (:doc "One decision put to the operator: a question, the options weighed, and which one you recommend.")
  ("id" :string :default "" :doc "short id the marks name: d1, d2; assigned in order when left out")
  ("question" :text :required t :doc "what is being decided, as the operator would ask it")
  ("options" :array :required t :of mock-review-option :doc "the options, two to six, in the order to show them")
  ("recommended" :string :default "" :doc "the id of the option you recommend, or empty"))

(nlk:define-contract mock-review-create-call
  (:doc "Create a mock review board (round 1) for the operator to mark up. Answers the review id and the board's URL to hand over; asks nothing.")
  ("title" :text :required t :doc "what the review is of, one line")
  ("brief" :string :default "" :doc "a sentence or two on what the round is for, shown at the top of the board")
  ("decisions" :array :required t :of mock-review-decision :doc "the decisions this round puts, in the order to show them"))

(nlk:define-contract mock-review-round-call
  (:doc "Add the next round to an open review: the same link, the new decisions, the earlier rounds kept.")
  ("review_id" :text :required t :doc "the review the round belongs to")
  ("decisions" :array :required t :of mock-review-decision :doc "the decisions this round puts"))

(nlk:define-contract mock-review-lookup
  (:doc "A review, and the round of it.")
  ("review_id" :text :required t :doc "the review's id")
  ("round" :integer :default 0 :slot round-number :doc "which round: 1, 2, ...; 0 (the default) is the newest"))

(nlk:define-contract mock-review-list-call
  (:doc "List the mock reviews of this home, open ones only unless include_closed says otherwise.")
  ("include_closed" :boolean :default nil :doc "include the reviews that were closed"))

(defparameter +option-letters+ "abcdef" "The ids of the options a decision did not name.")

(defparameter +mock-chars+ 200000 "The longest mock page kept: a mockup, not a site.")

(defparameter +marks+ '("approve" "change" "skip") "The three marks a decision takes.")

(defun decision-json (record &aux (options (coerce (mock-review-decision-options record) 'list)))
  "One decision as the board reads it: ids filled in, the options in order."
  (let ((ids (loop for option in options for n from 0
                   for id = (mock-review-option-id option)
                   collect (if (plusp (length id)) id (string (char +option-letters+ (min n 5))))))
        (recommended (mock-review-decision-recommended record)))
    (nlk:json-object
     "id" (mock-review-decision-id record)
     "question" (mock-review-decision-question record)
     "recommended" (if (member recommended ids :test #'equal) recommended "")
     "options" (coerce (loop for option in options for id in ids
                              collect (nlk:json-object
                                       "id" id
                                       "label" (mock-review-option-label option)
                                       "mock" (mock-review-option-mock option)))
                        'vector))))

(defun decisions-json (records)
  "RECORDS as the round stores them: two to six options a decision, ids no two alike."
  (unless records (fail "a round puts at least one decision"))
  (let ((out '()) (named '()) (n 0))
    (dolist (record records)
      (incf n)
      (let* ((decision (decision-json record))
             (options (nlk:json-array decision "options"))
             (question (nlk:json-value decision :string "question")))
        (when (< (length options) 2)
          (fail "decision ~d (~a) has ~d option~:p: a decision needs two to six"
                n question (length options)))
        (when (> (length options) 6)
          (fail "decision ~d (~a) has ~d options: a decision takes at most six"
                n question (length options)))
        (loop for option across options
              when (> (length (nlk:json-value option :string "mock")) +mock-chars+)
                do (fail "decision ~d's option ~a carries a mock over ~d characters: serve a mockup, not a site"
                         n (nlk:json-value option :string "id") +mock-chars+))
        (let ((id (nlk:json-value decision :string "id")))
          (when (plusp (length id))
            (when (member id named :test #'equal)
              (fail "two decisions are both called ~a" id))
            (push id named)))
        (push decision out)))
    (loop for decision in (reverse out) for i from 1
          unless (plusp (length (nlk:json-value decision :string "id")))
            do (let ((id (format nil "d~d" i)))
                 (loop while (member id named :test #'equal)
                       do (setf id (format nil "d~d~d" i (random 1000))))
                 (push id named)
                 (setf (gethash "id" decision) id)))
    (reverse out)))

;;; --- the marks come back ---------------------------------------------------

(defun text-or (value)
  (if (stringp value) value ""))

(defun normalize-feedback (data round raw)
  "RAW, the page's submission, cleaned against ROUND: unknown decisions and
options dropped, notes capped, an empty one refused."
  (declare (ignore data))
  (unless (hash-table-p raw) (fail "a submission must be a JSON object"))
  (let ((note (clean-block (text-or (nlk:json-value raw :string "note")) (setting :note-chars)))
        (sent (nlk:json-value raw :any "decisions"))
        (kept (make-hash-table :test #'equal))
        (anything nil))
    (dolist (decision (decisions-of round))
      (let* ((id (nlk:json-value decision :string "id"))
             (one (and (hash-table-p sent) (nlk:json-value sent :any id))))
        (when (hash-table-p one)
          (let* ((mark (text-or (nlk:json-value one :string "mark")))
                 (choice (text-or (nlk:json-value one :string "choice")))
                 (why (clean-block (text-or (nlk:json-value one :string "note")) (setting :note-chars)))
                 (known (loop for option across (nlk:json-array decision "options")
                               collect (nlk:json-value option :string "id"))))
            (unless (member mark +marks+ :test #'equal) (setf mark ""))
            (unless (member choice known :test #'equal) (setf choice ""))
            (when (or (plusp (length mark)) (plusp (length choice)) (plusp (length why)))
              (setf anything t)
              (when (and (equal mark "change") (setting :require-note-on-change) (zerop (length why)))
                (fail "the decision ~a is marked Change with no note: say what to change" id))
              (setf (gethash id kept)
                    (nlk:json-object "mark" mark "choice" choice "note" why)))))))
    (unless (or anything (plusp (length note)))
      (fail "Nothing to send yet — mark a decision, pick an option or write a note."))
    (nlk:json-object "at" (now-iso) "note" note "decisions" kept)))

(defun quoted (text)
  (with-output-to-string (out)
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (format out "> ~a~%" line))))

(defun feedback-markdown (data round)
  "ROUND's marks as the model reads them: a heading, then one block a decision."
  (let ((feedback (nlk:json-value round :any "feedback"))
        (id (nlk:json-value data :string "id")))
    (with-output-to-string (out)
      (format out "# Mock review feedback — ~a, round ~a~2%" (nlk:json-value data :string "title")
              (nlk:json-value round :integer "n"))
      (format out "Review `~a` · ~a~2%" id
              (if (hash-table-p feedback) (nlk:json-value feedback :string "at") "no answer yet"))
      (if (hash-table-p feedback)
          (let ((sent (nlk:json-value feedback :any "decisions")))
            (dolist (decision (decisions-of round))
              (let* ((decision-id (nlk:json-value decision :string "id"))
                     (one (and (hash-table-p sent) (nlk:json-value sent :any decision-id))))
                (format out "## ~a — ~a~%" decision-id (nlk:json-value decision :string "question"))
                (if (hash-table-p one)
                    (let ((mark (nlk:json-value one :string "mark"))
                          (choice (nlk:json-value one :string "choice")))
                      (format out "**~a**~@[ · go with ~a~]~2%"
                              (if (plusp (length mark)) (string-upcase mark) "no mark")
                              (and (plusp (length choice)) (string-upcase choice)))
                      (let ((why (nlk:json-value one :string "note")))
                        (when (and why (plusp (length why)))
                          (format out "~a~%" (quoted why))))
                      (format out "~%"))
                    (format out "not marked~2%"))))
            (let ((note (nlk:json-value feedback :string "note")))
              (when (and note (plusp (length note)))
                (format out "## Anything else~2%~a~%" note))))
          (format out "No feedback yet. The board is at ~a~%" (review-url id))))))

;;; --- the verbs ------------------------------------------------------------

(defun review-url (id)
  "ID's board on this image's gateway port."
  (let ((base (nle:gateway-endpoint)))
    (format nil "~a/mock-review/~a/" (or base "http://127.0.0.1:PORT") id)))

(define-verb create (title brief decisions)
  "Open a review at round 1 from TITLE, BRIEF and DECISIONS (contract records)."
  (let ((data (make-review (clean-line title 120) (clean-block brief 2000) (decisions-json decisions))))
    (format nil "Review ~a created, round 1, ~d decision~:p.~%Board: ~a~%Hand the board link to the operator; mock_review_feedback ~a answers their marks when they press Send."
            (nlk:json-value data :string "id") (length decisions)
            (review-url (nlk:json-value data :string "id"))
            (nlk:json-value data :string "id"))))

(define-verb add-round (review-id decisions)
  "Add the next round to the open review REVIEW-ID."
  (let* ((data (load-review review-id))
         (json (decisions-json decisions))
         (round (progn (unless (review-open-p data)
                          (fail "review ~a is closed: open a new one" review-id))
                       (add-round-to data json))))
    (format nil "Review ~a, round ~a, ~d decision~:p.~%Board: ~a"
            review-id (nlk:json-value round :integer "n") (length decisions) (review-url review-id))))

(define-verb feedback (review-id &optional round-number)
  "ROUND-NUMBER's marks, the newest when NIL: one markdown digest."
  (let* ((data (load-review review-id))
         (round (round-by-number data (and round-number (plusp round-number) round-number))))
    (feedback-markdown data round)))

(define-verb ack (review-id &optional round-number)
  "Say ROUND-NUMBER (the newest when NIL) is handled."
  (let* ((data (load-review review-id))
         (round (round-by-number data (and round-number (plusp round-number) round-number))))
    (unless (hash-table-p (nlk:json-value round :any "feedback"))
      (fail "round ~a of ~a has no feedback to acknowledge"
            (nlk:json-value round :integer "n") review-id))
    (setf (gethash "acked_at" round) (or (nlk:json-value round :string "acked_at") (now-iso)))
    (save-review data)
    (format nil "Round ~a of ~a acknowledged." (nlk:json-value round :integer "n") review-id)))

(define-verb list-reviews (&optional include-closed)
  "One line a review, newest first."
  (let ((rows '()))
    (dolist (id (review-ids))
      (let ((data (ignore-errors (load-review id))))
        (when (and (hash-table-p data) (or include-closed (review-open-p data)))
          (let* ((round (newest-round data))
                 (answered (hash-table-p (nlk:json-value round :any "feedback"))))
            (push (format nil "~a  ~a  round ~a  ~a  ~s~%~a~%"
                          id
                          (nlk:json-value data :string "status")
                          (nlk:json-value round :integer "n")
                          (cond ((nlk:json-value round :string "acked_at") "acked")
                                (answered "marks in, unread")
                                (t "waiting"))
                          (nlk:json-value data :string "title")
                          (review-url id))
                  rows)))))
    (if rows (format nil "~{~a~}" rows)
        (if include-closed "No reviews here." "No open reviews here."))))

(defun close-review-verb (review-id)
  "Close REVIEW-ID: off the list, and (mock_review_list) hides it unless asked."
  (close-review (load-review review-id))
  (format nil "Review ~a closed." review-id))

;;; --- the tools ------------------------------------------------------------

(defun create-tool (call)
  (create (mock-review-create-call-title call)
          (mock-review-create-call-brief call)
          (coerce (mock-review-create-call-decisions call) 'list)))

(defun add-round-tool (call)
  (add-round (mock-review-round-call-review-id call)
             (coerce (mock-review-round-call-decisions call) 'list)))

(defun feedback-tool (call)
  (feedback (mock-review-lookup-review-id call) (mock-review-lookup-round-number call)))

(defun ack-tool (call)
  (ack (mock-review-lookup-review-id call) (mock-review-lookup-round-number call)))

(defun list-tool (call)
  (list-reviews (mock-review-list-call-include-closed call)))

;;; --- the slash command and the manual -------------------------------------

(defun run-command (args session-id)
  "One /mock-review invocation => text: the open reviews, or one review's marks."
  (declare (ignore session-id))
  (let ((word (nlk:trimmed (or args ""))))
    (if (plusp (length word))
        (feedback word nil)
        (list-reviews nil))))

(nle:define-cell mock-review
  (:section ("mock-review")
    (:guide "nothing is required; directory keeps the reviews under the home, require_note_on_change makes every Change say why, note_chars caps a note")
    ("directory" :directory :default "mock-reviews/"
                 :doc "where the reviews and their boards are kept, under the home")
    ("require_note_on_change" :boolean :default nil
                               :doc "block Send until every decision marked Change carries a note")
    ("note_chars" :integer :default 2000 :min 100
                   :doc "the longest note kept from a reviewer"))
  (:tool "mock_review_create" (nlk:contract 'mock-review-create-call) #'create-tool)
  (:tool "mock_review_add_round" (nlk:contract 'mock-review-round-call) #'add-round-tool)
  (:tool "mock_review_feedback" (nlk:contract 'mock-review-lookup) #'feedback-tool)
  (:tool "mock_review_ack" (nlk:contract 'mock-review-lookup) #'ack-tool)
  (:tool "mock_review_list" (nlk:contract 'mock-review-list-call) #'list-tool)
  (:command "mock-review" 'run-command
            :description "Mock reviews: list them, or one review's marks"
            :argument-hint "REVIEW_ID")
  (:help :mock-review "Mock review boards: a round of decisions, marked on a page and read back as one digest" *primer*)
  (:route "/mock-review/" #'serve-board :public t)
  (:start (lambda ()
            (let ((directory (setting :directory)))
              (ensure-directories-exist (merge-pathnames "probe" directory))
              (nle:notice (format nil "mock-review: boards open at ~a/mock-review/<id>/"
                                   (or (nle:gateway-endpoint) "this gateway's address"))
                          :key "nodecode-mock-review")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-mock-review")))))))
