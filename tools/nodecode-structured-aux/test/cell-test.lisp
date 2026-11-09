;;;; cell-test.lisp --- the structured-aux cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json bound as the auth file, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every decision a stubbed
;;;; dex:request (NLK:HTTP's own call): nothing touches the network, the
;;;; environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "structured-aux" "STRUCTURED-AUX-" :start nodecode-structured-aux:start-cell)

(define-cell-lifecycle-tests "structured-aux"
  (:help :structured-aux)
  (:command "structured-aux")
  (:refused ("timeout_seconds" 0) ("blocks_per_call" 1))
  (:idle nodecode-structured-aux:structured-aux-error
         (structured-aux:approve "ls")
         (structured-aux:pick-tool "r" '("a" "b"))
         (structured-aux:compress "some text")))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +aux-approve+
  "{\"model\":\"jev-2026\",\"answers\":{\"verdict\":{\"type\":\"choice\",\"choice\":\"APPROVE\",\"probabilities\":{\"APPROVE\":0.9,\"DENY\":0.05,\"ESCALATE\":0.05},\"confidence\":0.9}},\"usage\":{\"prompt_tokens\":20,\"completion_tokens\":0}}"
  "One approval answered APPROVE.")

(defparameter +aux-tool+
  "{\"model\":\"jev-2026\",\"answers\":{\"tool\":{\"type\":\"choice\",\"choice\":\"read_file\",\"probabilities\":{\"read_file\":0.8,\"delete_file\":0.2},\"confidence\":0.8}},\"usage\":{}}"
  "One tool pick answered read_file.")

(defparameter +aux-keep+
  "{\"model\":\"jev-2026\",\"answers\":{\"block_0\":{\"type\":\"choice\",\"choice\":\"KEEP\",\"confidence\":0.8},\"block_1\":{\"type\":\"choice\",\"choice\":\"DROP\",\"confidence\":0.7},\"block_2\":{\"type\":\"choice\",\"choice\":\"KEEP\",\"confidence\":0.9}},\"usage\":{}}"
  "Three blocks: the first and last survive.")

(defparameter +aux-drop-all+
  "{\"model\":\"jev-2026\",\"answers\":{\"block_0\":{\"type\":\"choice\",\"choice\":\"DROP\"},\"block_1\":{\"type\":\"choice\",\"choice\":\"DROP\"},\"block_2\":{\"type\":\"choice\",\"choice\":\"DROP\"}},\"usage\":{}}"
  "Three blocks the provider would drop all of.")

(defmacro with-aux ((requests &key (answers '(list +aux-approve+)) (statuses '(list 200))
                                     (env '(("OPENROUTER_API_KEY" . "sk-or-env")))
                                     (auth "{}") config)
                    &body forms)
  "FORMS with the cell started on CONFIG, AUTH as the auth file, ENV as the
environment, and the decision route answering ANSWERS with STATUSES in turn (the
last repeated): REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest
first."
  `(with-cell-stop ((structured-aux-start ,@config))
     (with-temp-auth (auth ,auth)
       (let ((nle::*auth-file-path* auth) (,requests '())
             (answers ,answers) (statuses ,statuses))
         (with-stubbed-fdefinitions
             ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
              (dex:request (url &rest args)
                           (setf ,requests (append ,requests
                                                   (list (list (getf args :method) url (getf args :headers)
                                                               (getf args :content)))))
                           (values (if (rest answers) (pop answers) (first answers))
                                   (if (rest statuses) (pop statuses) (first statuses))
                                   (make-hash-table :test 'equal))))
           ,@forms)))))

(defun aux-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun aux-body (request)
  "REQUEST's content as the JSON it is."
  (nlk:decode-json (fourth request)))

(defun aux-near (a b)
  "Whether numbers A and B agree to a millionth."
  (and (realp a) (realp b) (< (abs (- a b)) 1d-6)))

(defun aux-paragraph (number)
  "A paragraph long enough to be a block of its own, naming its place."
  (format nil "paragraph ~d ~a" number (make-string 130 :initial-element #\x)))

(defun aux-three-blocks ()
  (format nil "~a~%~%~a~%~%~a" (aux-paragraph 0) (aux-paragraph 1) (aux-paragraph 2)))

;;; --- the wire ---------------------------------------------------------------------

(deftest structured-aux-cell-posts-a-bounded-choice ()
  (with-aux (requests)
    (structured-aux:approve "rm -rf build" :policy "never touch the repository")
    (destructuring-bind (method url headers content) (first requests)
      (declare (ignore content))
      (is (eq :post method))
      (is (equal "https://openrouter.ai/api/alpha/decisions" url))
      (is (equal "Bearer sk-or-env" (aux-header headers "authorization")))
      (is (equal "application/json" (aux-header headers "content-type")))
      (is (equal "https://nodecode.ai" (aux-header headers "http-referer"))))
    (let* ((body (aux-body (first requests)))
           (state (nlk:json-value body :object "state"))
           (questions (nlk:json-value body :object "questions")))
      (is (equal "rm -rf build" (nlk:json-value state :string "request")))
      (is (equal "never touch the repository" (nlk:json-value state :string "guardian_policy")))
      (is (equal "~typesafe/jev-latest" (nlk:json-value body :string "model")))
      (is (equal "choice" (nlk:json-value questions :string "verdict" "type")))
      (is (nth-value 1 (gethash "ESCALATE" (nlk:json-value questions :object "verdict" "criteria")))
          "all three verdicts are offered")
      (is (search "ACTUAL risk" (nlk:json-value questions :string "verdict" "instructions"))))))

(deftest structured-aux-cell-reads-the-verdict ()
  (with-aux (requests)
    (multiple-value-bind (verdict confidence probabilities model)
        (structured-aux:approve "rm -rf build")
      (is (eq :approve verdict))
      (is (aux-near 0.9 confidence))
      (is (equal '("APPROVE" "DENY" "ESCALATE") (mapcar #'car probabilities)))
      (is (equal "jev-2026" model) "the model that answered"))
    (is (= 1 (length requests)))))

(deftest structured-aux-cell-outside-the-contract-is-not-consent ()
  (with-aux (requests :answers (list "{\"model\":\"jev\",\"answers\":{\"verdict\":{\"type\":\"choice\",\"choice\":\"MAYBE\",\"confidence\":0.4}},\"usage\":{}}"))
    (multiple-value-bind (verdict confidence) (structured-aux:approve "rm -rf /")
      (is (eq :unknown verdict) "a label outside the three is neither consent nor refusal")
      (is (aux-near 0.4 confidence)))))

(deftest structured-aux-cell-refuses-an-answer-that-is-missing ()
  (with-aux (requests :answers (list "{\"model\":\"jev\",\"answers\":{},\"usage\":{}}"))
    (let ((text (refusal-text structured-aux:structured-aux-error (structured-aux:approve "ls"))))
      (is (search "missing an answer" text) text))))

(deftest structured-aux-cell-redacts-before-it-sends ()
  (with-aux (requests)
    (structured-aux:approve "API_KEY=sk-live1234567890 ./deploy")
    (let ((content (fourth (first requests))))
      (is (search "API_KEY=[REDACTED]" content))
      (is (null (search "sk-live1234567890" content)) "the token never leaves"))))

;;; --- the key -------------------------------------------------------------------------

(deftest structured-aux-cell-saved-key-outranks-the-variable ()
  (with-aux (requests :auth "{\"api_keys\":{\"openrouter\":{\"provider\":\"openrouter\",\"key\":\"sk-or-saved\"}}}")
    (structured-aux:approve "ls")
    (is (equal "Bearer sk-or-saved" (aux-header (third (first requests)) "authorization")))))

(deftest structured-aux-cell-without-a-key-says-where-to-make-one ()
  (with-aux (requests :env ())
    (let ((text (refusal-text structured-aux:structured-aux-error (structured-aux:approve "ls"))))
      (is (search "openrouter.ai/settings/keys" text))
      (is (search "OPENROUTER_API_KEY" text)))
    (is (null requests) "nothing was sent")))

(deftest structured-aux-cell-says-a-refusal-and-hides-the-key ()
  (with-aux (requests :answers (list "{\"error\":\"invalid key sk-or-env\"}") :statuses (list 401))
    (let ((text (refusal-text structured-aux:structured-aux-error (structured-aux:approve "ls"))))
      (is (search "HTTP 401" text))
      (is (null (search "sk-or-env" text)) "the key never reaches the model")
      (is (search "[redacted]" text)))))

;;; --- tools and digests ------------------------------------------------------------------

(deftest structured-aux-cell-picks-a-tool-or-refuses-a-short-list ()
  (with-aux (requests :answers (list +aux-tool+))
    (is (equal "read_file" (structured-aux:pick-tool "open the page" '("read_file" "delete_file"))))
    (let ((body (aux-body (first requests))))
      (is (nth-value 1 (gethash "delete_file" (nlk:json-value body :object "state" "available_tools"))))
      (is (nth-value 1 (gethash "read_file" (nlk:json-value body :object "questions" "tool" "criteria"))))))
  (with-aux (requests)
    (is (search "at least two candidate tools"
                (refusal-text structured-aux:structured-aux-error
                               (structured-aux:pick-tool "open the page" '("read_file")))))
    (is (null requests) "nothing was sent")))

(deftest structured-aux-cell-digests-only-what-survived ()
  (with-aux (requests :answers (list +aux-keep+))
    (multiple-value-bind (digest kept total) (structured-aux:compress (aux-three-blocks))
      (is (equal 2 kept))
      (is (equal 3 total))
      (is (search "retained 2/3 blocks" digest))
      (is (search "paragraph 0" digest) "a retained block is verbatim")
      (is (null (search "paragraph 1" digest)) "a dropped one is gone")
      (is (search "paragraph 2" digest))
      (is (search "no text was generated" digest)))
    (is (= 1 (length requests)) "three blocks ride one call")
    (let* ((body (aux-body (first requests)))
           (blocks (nlk:json-array body "state" "blocks")))
      (is (= 3 (length blocks)))
      (is (equal "choice" (nlk:json-value body :string "questions" "block_0" "type"))))))

(deftest structured-aux-cell-keeps-the-last-block-when-nothing-survives ()
  (with-aux (requests :answers (list +aux-drop-all+))
    (multiple-value-bind (digest kept) (structured-aux:compress (aux-three-blocks))
      (is (equal 0 kept))
      (is (search "retained 1/3 blocks" digest) "an empty digest would read as a failure")
      (is (search "paragraph 2" digest) "so the newest block is kept")
      (is (null (search "paragraph 0" digest))))))

;;; --- the command ----------------------------------------------------------------------

(deftest structured-aux-cell-status-says-where-the-key-comes-from ()
  (with-aux (requests)
    (is (search "key from OPENROUTER_API_KEY"
                (cell-entry "nodecode-structured-aux" "structured-aux" "status")))
    (is (search "/api/alpha/decisions" (cell-entry "nodecode-structured-aux" "structured-aux" "")))
    (is (search "nope is not one of its choices"
                (cell-entry "nodecode-structured-aux" "structured-aux" "nope")))
    (is (null requests) "status asks no one"))
  (with-aux (requests :env ())
    (is (search "no key" (cell-entry "nodecode-structured-aux" "structured-aux" "status")))))

(deftest structured-aux-cell-decides-any-bounded-choice ()
  (with-aux (requests
             :answers (list "{\"model\":\"jev\",\"answers\":{\"kind\":{\"type\":\"choice\",\"choice\":\"refactor\",\"probabilities\":{\"fix\":0.2,\"refactor\":0.8},\"confidence\":0.8}},\"usage\":{\"prompt_tokens\":9,\"completion_tokens\":0}}"))
    (multiple-value-bind (answers model usage)
        (structured-aux:decide "the patch renames a public function"
                               '(:kind (:instructions "What kind of change is this?"
                                       :criteria (("fix" . nil)
                                                  ("refactor" . "restructures without changing behaviour")))))
      (is (equal "refactor" (getf (getf answers :kind) :choice)))
      (is (aux-near 0.8 (getf (getf answers :kind) :confidence)))
      (is (equal "jev" model))
      (is (equal 9 (nlk:json-value usage :integer "prompt_tokens"))))
    (let* ((body (aux-body (first requests)))
           (criteria (nlk:json-value body :object "questions" "kind" "criteria")))
      (is (eq :null (gethash "fix" criteria)) "a rubric of NIL is sent as null")
      (is (equal "choice" (nlk:json-value body :string "questions" "kind" "type"))))))
