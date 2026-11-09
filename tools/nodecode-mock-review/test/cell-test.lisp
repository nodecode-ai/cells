;;;; cell-test.lisp --- the lifecycle, a round on the page, the marks back.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(define-test-slice "mock-review" "MOCK-REVIEW-CELL-" :start nodecode-mock-review:start-cell)

;;; --- what the tests build ----------------------------------------------------

(defmacro with-fresh-reviews ((&rest pairs) &body body)
  "BODY with the cell started on its own empty reviews folder: PAIRS are section members set over it."
  (let ((directory (gensym "REVIEWS")))
    `(let ((,directory (uiop:ensure-directory-pathname
                        (format nil "~~a/nodecode-mock-review-~~a/"
                                (uiop:temporary-directory) (random 1000000)))))
       (unwind-protect
            (with-cell-stop ((mock-review-start "directory" (namestring ,directory) ,@pairs))
              ,@body)
         (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore)))))

(defun mock-review-round-json (&optional (second-label "B — the calmer one"))
  "A round as a model's tool call carries one: one decision, two options, a mock each."
  (nlk:decode-json
   (format nil "{\"decisions\":[{\"question\":\"Which pricing page?\",\"recommended\":\"b\",~@
                 \"options\":[{\"label\":\"A — bold\",\"mock\":\"<h1>A</h1>\"},~@
                 {\"label\":~s,\"mock\":\"<h1>B</h1>\"}]}]}"
           second-label)))

(defun mock-review-create (title &optional (round (mock-review-round-json)))
  "What mock_review_create answers for TITLE, through the tool's own body."
  (nodecode-mock-review::create-tool
   (nlk:contract-decode (nlk:contract 'nodecode-mock-review::mock-review-create-call)
                         (nlk:json-object "title" title "brief" "Two ways to price it."
                                           "decisions" (nlk:json-array round "decisions")))))

(defun mock-review-id (said)
  (second (uiop:split-string said :separator " ")))

(defun mock-review-env (path &key method body)
  (list :path-info path
        :request-method (or method :get)
        :query-string nil
        :content-length (and body (length body))
        :raw-body (and body (make-string-input-stream body))))

(defun mock-review-answer (path &key method body)
  "The board's answer to PATH: (values STATUS BODY-TEXT)."
  (let* ((answer (nodecode-mock-review::serve-board (mock-review-env path :method method :body body)))
         (sent (third answer)))
    (values (first answer)
            (cond ((stringp sent) sent)
                  ((listp sent) (format nil "~{~a~}" sent))
                  (t (princ-to-string sent))))))

(defun mock-review-data (id)
  (nodecode-mock-review::load-review id))

(defun mock-review-token (id)
  (nlk:json-value (mock-review-data id) :string "token"))

(defun mock-review-submit-path (id)
  (format nil "/mock-review/~a/submit" id))

(defun mock-review-submission (id &key (mark "approve") (choice "b")
                                    (note "B, but tighter type") (token :own) (anything t))
  "A reviewer's Send, as the page posts it against review ID."
  (let* ((round (nodecode-mock-review::newest-round (mock-review-data id)))
         (decision (first (nodecode-mock-review::decisions-of round)))
         (decision-id (nlk:json-value decision :string "id"))
         (feedback (nlk:json-object "note" (if anything "nothing else" ""))))
    (let ((decisions (nlk:json-object)))
      (when anything
        (setf (gethash decision-id decisions)
              (nlk:json-object "mark" mark "choice" choice "note" note)))
      (setf (gethash "decisions" feedback) decisions))
    (nlk:encode-json-object
     (nlk:json-object "token" (if (eq token :own) (mock-review-token id) (or token ""))
                      "round" (nlk:json-value round :integer "n")
                      "feedback" feedback))))

(defun mock-review-command-output (&optional (args ""))
  (let* ((entry (nle::find-registered-command "mock-review"))
         (run (and entry (nle::slash-command-headless entry))))
    (if run (funcall run args nil) (nodecode-mock-review::run-command args nil))))

(defun mock-review-mock-path (id)
  "The address of the first decision's first option's mock."
  (let* ((round (nodecode-mock-review::newest-round (mock-review-data id)))
         (decision (first (nodecode-mock-review::decisions-of round)))
         (option (aref (nlk:json-array decision "options") 0)))
    (format nil "/mock-review/~a/m/~a.html" id
            (nodecode-mock-review::option-key decision option))))

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "mock-review"
  (:command "mock-review")
  (:help :mock-review)
  (:running (is (search "mock_review_create" (nle:help :mock-review)) "the manual names the tools"))
  (:refused ("note_chars" 0))
  (:idle nodecode-mock-review:mock-review-error (mockreview:list-reviews)))

;;; --- a round on the board -----------------------------------------------------

(deftest mock-review-cell-a-review-puts-its-decisions-on-a-page (with-fresh-reviews ()
  (let* ((said (mock-review-create "Pricing page"))
         (id (mock-review-id said))
         (data (mock-review-data id))
         (round (nodecode-mock-review::newest-round data))
         (page (nodecode-mock-review::board-page data round))
         (decision (first (nodecode-mock-review::decisions-of round)))
         (option (aref (nlk:json-array decision "options") 0))
         (mock (nodecode-mock-review::serve-mock
                data (nodecode-mock-review::option-key decision option))))
    (is (search "created, round 1, 1 decision" said) "the verb says what it made")
    (is (search (format nil "/mock-review/~a/" id) said) "and where the board is")
    (is (search "Which pricing page?" page) "the question is on the page")
    (is (search "A — bold" page) "so is an option's label")
    (is (search "<iframe" page) "a mock is framed")
    (is (search "(recommended)" page) "the recommendation shows")
    (is (search (mock-review-token id) page) "and the review's submit token is in it")
    (multiple-value-bind (status) (mock-review-answer (format nil "/mock-review/~a/" id))
      (is (= 200 status) "the route answers the board"))
    (is (= 200 (first mock)) "a mock answers at its own address")
    (is (equal "<h1>A</h1>" (format nil "~{~a~}" (third mock))) "as its own document"))))

(deftest mock-review-cell-a-send-comes-back-as-one-digest (with-fresh-reviews ()
  (let* ((said (mock-review-create "Pricing page"))
         (id (mock-review-id said)))
    (multiple-value-bind (status answer)
        (mock-review-answer (mock-review-submit-path id)
                             :method :post :body (mock-review-submission id))
      (is (= 200 status) answer)
      (is (search "\"ok\":true" answer) "the page is told it landed"))
    (let ((digest (mockreview:feedback id nil)))
      (is (search "# Mock review feedback — Pricing page, round 1" digest) "a titled digest")
      (is (search "**APPROVE** · go with B" digest) "the mark and the pick")
      (is (search "> B, but tighter type" digest) "the note, quoted")
      (is (search "## Anything else" digest) "and the general note"))
    (is (search id (mockreview:list-reviews)) "the review is listed")
    (is (search "marks in, unread" (mockreview:list-reviews)) "with its marks waiting")
    (is (search "acknowledged" (mockreview:ack id)) "and an ack is answered")
    (is (search id (mock-review-command-output)) "/mock-review lists it")
    (is (search "## d1" (mockreview:feedback id 1)) "a round can be asked for by number"))))

(deftest mock-review-cell-a-send-needs-its-token-and-something-to-say (with-fresh-reviews ()
  (let* ((said (mock-review-create "Pricing page"))
         (id (mock-review-id said))
         (path (mock-review-submit-path id)))
    (multiple-value-bind (status answer)
        (mock-review-answer path :method :post :body (mock-review-submission id :token "not-it"))
      (is (= 400 status))
      (is (search "not the one this board handed out" answer) "a foreign token is refused"))
    (multiple-value-bind (status answer)
        (mock-review-answer path :method :post :body (mock-review-submission id :anything nil))
      (is (= 400 status))
      (is (search "Nothing to send yet" answer) "so is an empty Send"))
    (multiple-value-bind (status answer) (mock-review-answer "/mock-review/r-nope/")
      (is (= 404 status))
      (is (search "no review" answer) "a review that is not there says so")))))

(deftest mock-review-cell-a-change-must-say-why-when-the-section-says-so
    (with-fresh-reviews ("require_note_on_change" t)
  (let* ((said (mock-review-create "Pricing page"))
         (id (mock-review-id said)))
    (multiple-value-bind (status answer)
        (mock-review-answer (mock-review-submit-path id)
                             :method :post :body (mock-review-submission id :mark "change" :note ""))
      (is (= 400 status))
      (is (search "marked Change with no note" answer) "a Change with no note is refused"))
    (let ((one-option (nlk:decode-json
                        "{\"decisions\":[{\"question\":\"Which?\",\"options\":[{\"label\":\"only\"}]}]}")))
      (is (signals-error nodecode-mock-review:mock-review-error
                         (mock-review-create "One option only" one-option))
          "a decision with one option is refused")
      (is (search "needs two to six"
                  (handler-case (mock-review-create "One option only" one-option)
                    (error (e) (princ-to-string e))))
          "and says how many it wants")))))

(deftest mock-review-cell-a-second-round-keeps-the-first (with-fresh-reviews ()
  (let* ((said (mock-review-create "Pricing page"))
         (id (mock-review-id said)))
    (let ((second (nodecode-mock-review::add-round-tool
                   (nlk:contract-decode
                    (nlk:contract 'nodecode-mock-review::mock-review-round-call)
                    (nlk:json-object "review_id" id
                                      "decisions" (nlk:json-array (mock-review-round-json "B — quieter still") "decisions"))))))
      (is (search "round 2" second) "the next round is numbered"))
    (is (search "round 2" (mockreview:list-reviews)) "the list says which round is current")
    (is (search "round 1" (mockreview:feedback id 1)) "and an older round is still there"))))
