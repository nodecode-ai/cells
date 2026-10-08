;;;; cell-test.lisp --- the typesafe cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json bound as the auth file, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every exchange with TypeSafe a
;;;; stubbed dex:request (NLK:HTTP's own call): nothing touches the network,
;;;; the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "typesafe" "TYPESAFE-CELL-" :start nodecode-typesafe:start-cell)

(define-cell-lifecycle-tests "typesafe"
  (:help :typesafe)
  (:command "typesafe")
  (:refused ("model" 5) ("base_url" #(1)))
  (:idle typesafe:typesafe-error
         (typesafe:judge "x" '(:a (:type "bool" :instructions "y")))
         (typesafe:models)))

;;; --- fixtures -------------------------------------------------------------------

(defun ts-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defparameter +ts-answer+
  "{\"model\":\"jev-20260901\",\"answers\":{\"risky\":{\"type\":\"noul\",\"noul\":0.82},\"kind\":{\"type\":\"choice\",\"choice\":\"fix\",\"probabilities\":{\"fix\":0.7,\"feature\":0.2,\"refactor\":0.1},\"confidence\":0.61},\"size\":{\"type\":\"score\",\"score\":1.4,\"probabilities\":{\"0\":0.1,\"1\":0.45,\"2\":0.35,\"3\":0.1},\"confidence\":0.4}},\"usage\":{\"input_tokens\":212,\"output_tokens\":0}}"
  "System One's answer to the three questions +TS-QUESTIONS+ asks.")

(defparameter +ts-a-answer+
  "{\"model\":\"jev\",\"answers\":{\"a\":{\"type\":\"noul\",\"noul\":0.5}},\"usage\":{}}"
  "System One's answer to one bool question, a.")

(defparameter +ts-questions+
  '(:risky (:type "bool" :instructions "Does this change risk breaking callers?"
            :criteria (:true "an API changes" :false "internal only"))
    :kind (:type :choice :instructions "What kind of change is this?"
           :criteria (:fix nil :feature nil :refactor "restructures without changing behaviour"))
    :size (:type "score" :instructions "How large is the change?"
           :criteria ("trivial" "small" "medium" "large")))
  "One question of each kind, as a model writes them in eval.")

(defmacro with-typesafe ((requests &key (answers '(list +ts-answer+)) (statuses '(list 200))
                                         (env '(("TYPESAFE_API_KEY" . "ts-env"))) (auth "{}") config)
                         &body forms)
  "FORMS with the cell started on CONFIG, AUTH as the auth file, ENV as the
environment, and TypeSafe answering ANSWERS with STATUSES in turn (the last
repeated): REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest first."
  `(with-cell-stop ((typesafe-start ,@config))
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

(defun ts-near (a b)
  "Whether numbers A and B agree to a millionth: JSON reads floats in the
reader's default format."
  (and (realp a) (realp b) (< (abs (- a b)) 1d-6)))

(defun ts-body (request)
  "The decoded JSON body of REQUEST."
  (nlk:decode-json (fourth request)))

;;; --- a judgment -----------------------------------------------------------------------

(deftest typesafe-cell-posts-the-system-one-request ()
  (with-typesafe (requests)
    (typesafe:judge "The patch renames a public function." +ts-questions+)
    (destructuring-bind (method url headers content) (first requests)
      (declare (ignore content))
      (is (eq :post method))
      (is (equal "https://api.typesafe.ai/v1/systemone" url))
      (is (equal "Bearer ts-env" (ts-header headers "authorization")))
      (is (equal "application/json" (ts-header headers "accept"))))
    (let* ((body (ts-body (first requests)))
           (questions (nlk:json-value body :object "questions")))
      (is (equal "The patch renames a public function." (nlk:json-value body :string "state")))
      (is (equal "jev-latest" (nlk:json-value body :string "model")) "the bundled default model")
      (is (equal "noul" (nlk:json-value questions :string "risky" "type")) "bool is the wire's noul")
      (is (equal "internal only" (nlk:json-value questions :string "risky" "criteria" "false")))
      (is (equal "choice" (nlk:json-value questions :string "kind" "type")) "a keyword type reads lowercase")
      (is (eq :null (gethash "fix" (nlk:json-value questions :object "kind" "criteria")))
          "a label with no rubric is null, not false")
      (is (equalp #("trivial" "small" "medium" "large") (nlk:json-value questions :array "size" "criteria")))
      (is (search "\"state\":" (fourth (first requests))) "the body leads with the state"))))

(deftest typesafe-cell-answers-each-question ()
  (with-typesafe (requests)
    (multiple-value-bind (answers model usage) (typesafe:judge "a diff" +ts-questions+)
      (is (equal '(:risky :kind :size) (loop for (id) on answers by #'cddr collect id)) "in the order asked")
      (is (eq :bool (getf (getf answers :risky) :type)) "noul comes back as bool")
      (is (ts-near 0.82 (getf (getf answers :risky) :bool)))
      (let ((kind (getf answers :kind)))
        (is (equal "fix" (getf kind :choice)))
        (is (equal '("fix" "feature" "refactor") (mapcar #'car (getf kind :probabilities))) "in the wire's order")
        (is (ts-near 0.7 (cdr (first (getf kind :probabilities)))))
        (is (ts-near 0.61 (getf kind :confidence))))
      (is (ts-near 1.4 (getf (getf answers :size) :score)))
      (is (equal "typesafe/jev-20260901" model) "the model that answered")
      (is (equal '(:input-tokens 212 :output-tokens 0) usage)))))

(deftest typesafe-cell-takes-json-and-structured-state ()
  (with-typesafe (requests)
    (typesafe:judge '(:diff "rename foo to bar" :tests 3)
                    "{\"risky\":{\"type\":\"bool\",\"instructions\":\"Risky?\"},\"kind\":{\"type\":\"choice\",\"instructions\":\"Kind?\",\"criteria\":{\"Fix\":null,\"Feature\":\"adds\"}},\"size\":{\"type\":\"score\",\"instructions\":\"Size?\",\"criteria\":[\"s\",\"l\"]}}")
    (let ((body (ts-body (first requests))))
      (is (equal "rename foo to bar" (nlk:json-value body :string "state" "diff")) "a plist state is an object")
      (is (= 3 (nlk:json-value body :integer "state" "tests")))
      (is (nth-value 1 (gethash "Fix" (nlk:json-value body :object "questions" "kind" "criteria")))
          "a JSON object keeps its labels as written")
      (is (null (nth-value 1 (gethash "criteria" (nlk:json-value body :object "questions" "risky"))))
          "a bool with no criteria sends none"))))

(deftest typesafe-cell-refuses-questions-that-make-no-judgment ()
  (with-typesafe (requests)
    (flet ((refused (state questions)
             (refusal-text typesafe:typesafe-error (typesafe:judge state questions))))
      (is (search "at least two options" (refused "x" '(:a (:type "choice" :instructions "?" :criteria (:only nil))))))
      (is (search "at least two levels" (refused "x" '(:a (:type "score" :instructions "?" :criteria ("one"))))))
      (is (search "type must be" (refused "x" '(:a (:type "rank" :instructions "?")))))
      (is (search "non-empty string instructions" (refused "x" '(:a (:type "bool" :instructions "")))))
      (is (search "state must not be empty" (refused "" '(:a (:type "bool" :instructions "?")))))
      (is (search "keyed by question id" (refused "x" 42))))
    (is (null requests) "and nothing was sent")))

;;; --- the key, the base, the model -----------------------------------------------------------

(deftest typesafe-cell-saved-key-outranks-the-variable ()
  (with-typesafe (requests :auth "{\"api_keys\":{\"typesafe\":{\"provider\":\"typesafe\",\"key\":\"ts-saved\"}}}"
                           :answers (list +ts-a-answer+))
    (typesafe:judge "x" '(:a (:type "bool" :instructions "?")) )
    (is (equal "Bearer ts-saved" (ts-header (third (first requests)) "authorization")))))

(deftest typesafe-cell-without-a-key-says-where-to-make-one ()
  (with-typesafe (requests :env ())
    (let ((text (refusal-text typesafe:typesafe-error (typesafe:judge "x" '(:a (:type "bool" :instructions "?"))))))
      (is (search "console.typesafe.ai" text))
      (is (search "TYPESAFE_API_KEY" text)))
    (is (null requests))))

(deftest typesafe-cell-base-and-model-follow-the-section-then-the-environment ()
  (with-typesafe (requests :config ("base_url" "https://gateway.example/" "model" "jev-next")
                           :answers (list +ts-a-answer+))
    (typesafe:judge "x" '(:a (:type "bool" :instructions "?")))
    (is (equal "https://gateway.example/v1/systemone" (second (first requests))))
    (is (equal "jev-next" (nlk:json-value (ts-body (first requests)) :string "model"))))
  (with-typesafe (requests :env (("TYPESAFE_API_KEY" . "k") ("TYPESAFE_BASE_URL" . "https://env.example")
                                 ("TYPESAFE_DEFAULT_MODEL" . "jev-env"))
                           :answers (list +ts-a-answer+))
    (typesafe:judge "x" '(:a (:type "bool" :instructions "?")) :model "jev-asked")
    (is (equal "https://env.example/v1/systemone" (second (first requests))))
    (is (equal "jev-asked" (nlk:json-value (ts-body (first requests)) :string "model")) "a call's model wins")))

;;; --- failures -----------------------------------------------------------------------------

(deftest typesafe-cell-tries-a-transient-failure-again ()
  (with-typesafe (requests :answers (list "{\"error\":\"busy\"}" +ts-answer+) :statuses (list 503 200))
    (is (getf (typesafe:judge "x" +ts-questions+) :risky) "the second attempt answers")
    (is (= 2 (length requests)))))

(deftest typesafe-cell-says-a-refusal-and-hides-the-key ()
  (with-typesafe (requests :answers (list "{\"detail\":\"invalid key ts-env\"}") :statuses (list 401)
                           :env (("TYPESAFE_API_KEY" . "ts-env")))
    (let ((text (refusal-text typesafe:typesafe-error (typesafe:judge "x" +ts-questions+))))
      (is (search "typesafe/jev-latest API error (401)" text))
      (is (null (search "ts-env" text)) "the key never reaches the model")
      (is (search "[redacted]" text)))
    (is (= 1 (length requests)) "a 401 is not tried again")))

(deftest typesafe-cell-refuses-an-answer-missing-a-question ()
  (with-typesafe (requests :answers (list "{\"model\":\"jev\",\"answers\":{\"risky\":{\"type\":\"noul\",\"noul\":0.5}},\"usage\":{}}"))
    (let ((text (refusal-text typesafe:typesafe-error (typesafe:judge "x" +ts-questions+))))
      (is (search "missing a \"choice\" answer for question \"kind\"" text) text))))

;;; --- the listing and the command --------------------------------------------------------------

(deftest typesafe-cell-lists-the-judges ()
  (with-typesafe (requests :answers (list "{\"models\":[{\"name\":\"jev-latest\",\"description\":\"The current judge\",\"release_date\":\"2026-09-01\"}]}"))
    (is (equal "jev-latest (2026-09-01) - The current judge" (typesafe:models)))
    (destructuring-bind (method url headers content) (first requests)
      (declare (ignore headers content))
      (is (eq :get method))
      (is (equal "https://api.typesafe.ai/v1/models" url)))))

(deftest typesafe-cell-status-says-where-the-key-comes-from ()
  (with-typesafe (requests)
    (is (search "key from TYPESAFE_API_KEY" (cell-entry "nodecode-typesafe" "typesafe" "status")))
    (is (search "model jev-latest" (cell-entry "nodecode-typesafe" "typesafe" "")))
    (is (null requests) "status asks no one"))
  (with-typesafe (requests :env ())
    (is (search "no key" (cell-entry "nodecode-typesafe" "typesafe" "status")))))
