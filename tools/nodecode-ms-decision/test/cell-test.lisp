;;;; cell-test.lisp --- the ms-decision cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json bound as the auth file, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every exchange a stubbed
;;;; DEX:REQUEST (NLK:HTTP's own call): nothing touches the network, the
;;;; environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "ms-decision" "MS-DECISION-CELL-" :start nodecode-ms-decision:start-cell)

(define-cell-lifecycle-tests "ms-decision"
  (:hooks :tool)
  (:help :ms-decision)
  (:command "ms-decision")
  (:refused ("gate_mode" "maybe") ("timeout_seconds" 0)))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +md-answer+
  "{\"answers\":{\"q\":{\"type\":\"noul\",\"noul\":0.82}},\"usage\":{\"input_tokens\":40,\"output_tokens\":1}}"
  "System One's answer to one yes/no question.")

(defparameter +md-choice-answer+
  "{\"answers\":{\"q\":{\"type\":\"choice\",\"choice\":\"infra\",\"probabilities\":{\"infra\":0.7,\"app\":0.3},\"confidence\":0.61}},\"usage\":{}}"
  "System One's answer to one choice question.")

(defmacro with-ms-decision ((requests &key (answers '(list +md-answer+)) (statuses '(list 200))
                                         (env '(list (cons "AZURE_FOUNDRY_API_KEY" "az-key")))
                                         (auth "{}")
                                         (config '("base_url" "https://right.openai.azure.com")))
                              &body forms)
  "FORMS with the cell started on CONFIG, AUTH as the auth file, ENV as the
environment, and the endpoint answering ANSWERS with STATUSES in turn (the last
repeated): REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest first."
  `(with-cell-stop ((ms-decision-start ,@config))
     (with-temp-auth (auth ,auth)
       (let ((nle::*auth-file-path* auth) (,requests '())
             (answers ,answers) (statuses ,statuses))
         (with-stubbed-fdefinitions
             ((nle::credential-env (name) (cdr (assoc name ,env :test #'equal)))
              (dex:request (url &rest args)
                           (setf ,requests (append ,requests
                                                   (list (list (getf args :method) url (getf args :headers)
                                                               (getf args :content)))))
                           (values (if (rest answers) (pop answers) (first answers))
                                   (if (rest statuses) (pop statuses) (first statuses))
                                   (make-hash-table :test 'equal))))
           ,@forms)))))

(defun md-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun md-body (request)
  "The decoded JSON body of REQUEST."
  (nlk:decode-json (fourth request)))

(defun md-near (a b)
  "Whether numbers A and B agree to a millionth."
  (and (realp a) (realp b) (< (abs (- a b)) 1d-6)))

(defun md-error (text)
  "The error TEXT, a body of a call that decided nothing, names."
  (nlk:json-value (nlk:decode-json text) :string "error"))

;;; --- one decision -------------------------------------------------------------------

(deftest ms-decision-cell-posts-a-typed-question ()
  (with-ms-decision (requests)
    (ms-decision:noul "the build is red" "Is the build broken?")
    (destructuring-bind (method url headers content) (first requests)
      (declare (ignore content))
      (is (eq :post method))
      (is (equal "https://right.openai.azure.com/providers/microsoft/v1/systemone" url) "System One hangs off the resource host")
      (is (equal "az-key" (md-header headers "api-key")) "the Foundry key rides the api-key header")
      (is (equal "application/json" (md-header headers "content-type"))))
    (let ((body (md-body (first requests))))
      (is (equal "ms-decision-1" (nlk:json-value body :string "model")) "the default deployment")
      (is (equal "the build is red" (nlk:json-value body :string "state")))
      (is (equal "noul" (nlk:json-value body :string "questions" "q" "type")))
      (is (equal "Is the build broken?" (nlk:json-value body :string "questions" "q" "instructions")))
      (is (equal "true" (nlk:json-value body :string "questions" "q" "criteria" "true"))))))

(deftest ms-decision-cell-keeps-the-distribution ()
  (with-ms-decision (requests :answers (list +md-choice-answer+))
    (let* ((text (ms-decision:choice "a failing test in the queue" "Which team owns it?" '("infra" "app")))
           (body (nlk:decode-json text)))
      (is (equal "infra" (nlk:json-value body :string "answers" "q" "choice")))
      (is (md-near 0.7 (nlk:json-value body :number "answers" "q" "probabilities" "infra")) "every option's probability is carried back")
      (is (md-near 0.61 (nlk:json-value body :number "answers" "q" "confidence"))))
    (let ((body (md-body (first requests))))
      (is (equal "infra" (nlk:json-value body :string "questions" "q" "criteria" "infra")) "with no criteria sent, each label is its own description"))))

(deftest ms-decision-cell-score-sends-its-levels ()
  (with-ms-decision (requests)
    (ms-decision:score "the disk is 96% full" "How severe is it?" '("low" "medium" "high"))
    (let ((body (md-body (first requests))))
      (is (equal "score" (nlk:json-value body :string "questions" "q" "type")))
      (is (equalp #("low" "medium" "high")
                  (nlk:json-value body :array "questions" "q" "criteria")) "the levels go lowest first, as a JSON array"))))

(deftest ms-decision-cell-decide-asks-several-in-one-call ()
  (with-ms-decision (requests)
    (ms-decision:decide "a deploy failed twice"
                         (nlk:json-object
                          "urgent" (nlk:json-object "type" "noul" "instructions" "Is it urgent?")
                          "owner" (nlk:json-object "type" "choice" "instructions" "Who owns it?"
                                                    "criteria" (nlk:json-object "infra" "platform" "app" "product"))))
    (is (= 1 (length requests)) "one call, several questions")
    (let ((questions (nlk:json-value (md-body (first requests)) :object "questions")))
      (is (equal "noul" (nlk:json-value questions :string "urgent" "type")))
      (is (equal "choice" (nlk:json-value questions :string "owner" "type")))
      (is (equal "platform" (nlk:json-value questions :string "owner" "criteria" "infra"))))))

;;; --- where it goes, and with what -------------------------------------------------------

(deftest ms-decision-cell-sends-nothing-without-an-endpoint ()
  (with-ms-decision (requests :config ("base_url" ""))
    (let ((text (ms-decision:noul "x" "y?")))
      (is (search "AZURE_FOUNDRY_BASE_URL" (md-error text)) "the error names the setting"))
    (is (null requests))))

(deftest ms-decision-cell-refuses-a-host-that-is-not-a-resource ()
  (with-ms-decision (requests :config ("base_url" "https://evil.example.com"))
    (is (search "base_url" (md-error (ms-decision:noul "x" "y?"))))
    (is (null requests) "a domain the plugin never allowed carries no key and no state"))
  (with-ms-decision (requests :config ("base_url" "http://right.openai.azure.com"))
    (is (null requests) "https only"))
  (with-ms-decision (requests :config ("base_url" "https://right.openai.azure.com:444"))
    (is (null requests) "no port")))

(deftest ms-decision-cell-sends-nothing-without-a-key ()
  (with-ms-decision (requests :env '())
    (let ((text (ms-decision:noul "x" "y?")))
      (is (search "AZURE_FOUNDRY_API_KEY" (md-error text))))
    (is (null requests))))

(deftest ms-decision-cell-saved-key-outranks-the-variable ()
  (with-ms-decision (requests :auth "{\"api_keys\":{\"ms-decision\":{\"provider\":\"ms-decision\",\"key\":\"az-saved\"}}}")
    (ms-decision:noul "x" "y?")
    (is (equal "az-saved" (md-header (third (first requests)) "api-key")))))

(deftest ms-decision-cell-the-section-and-the-environment-name-the-model ()
  (with-ms-decision (requests :config ("base_url" "https://right.openai.azure.com" "model" "decision-next"))
    (ms-decision:noul "x" "y?")
    (is (equal "decision-next" (nlk:json-value (md-body (first requests)) :string "model"))))
  (with-ms-decision (requests :env (list (cons "AZURE_FOUNDRY_API_KEY" "az-key")
                                         (cons "MS_DECISION_MODEL" "decision-env")))
    (ms-decision:noul "x" "y?")
    (is (equal "decision-env" (nlk:json-value (md-body (first requests)) :string "model")))))

(deftest ms-decision-cell-says-an-http-refusal-without-echoing-the-body ()
  (with-ms-decision (requests :answers (list "{\"error\":\"bad key az-key\"}") :statuses (list 401))
    (let ((text (ms-decision:noul "x" "y?")))
      (is (equal "HTTP 401" (md-error text)))
      (is (null (search "az-key" text)) "the key never reaches the model"))))

;;; --- the command --------------------------------------------------------------------

(deftest ms-decision-cell-status-says-where-the-key-comes-from ()
  (with-ms-decision (requests)
    (is (search "key from AZURE_FOUNDRY_API_KEY" (cell-entry "nodecode-ms-decision" "ms-decision" "status")))
    (is (search "model ms-decision-1" (cell-entry "nodecode-ms-decision" "ms-decision" "")))
    (is (search "gate off" (cell-entry "nodecode-ms-decision" "ms-decision" "")))
    (is (search "nope is not one of its choices" (cell-entry "nodecode-ms-decision" "ms-decision" "nope")))
    (is (null requests) "status asks no one"))
  (with-ms-decision (requests :env '())
    (is (search "no key" (cell-entry "nodecode-ms-decision" "ms-decision" "status"))))
  (with-ms-decision (requests)
    (is (search "gate on (approve)" (cell-entry "nodecode-ms-decision" "ms-decision" "gate on")))
    (is (search "gate off" (cell-entry "nodecode-ms-decision" "ms-decision" "gate off")))))

;;; --- the gate -------------------------------------------------------------------------

(defun md-gate-verdict (arguments)
  "The installed :tool hook's answer for one call: :ALLOWED, or the refusal."
  (funcall (cell-hook "nodecode-ms-decision" :tool)
           (list :name "eval" :arguments arguments :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun call-with-md-ask-operator (answer why thunk)
  "THUNK with the core's ASK-OPERATOR answering ANSWER and WHY, put back after."
  (let* ((symbol (intern "ASK-OPERATOR" '#:nodecode.evolved))
         (saved (and (fboundp symbol) (fdefinition symbol))))
    (setf (fdefinition symbol) (lambda (title &rest keys)
                                  (declare (ignore title keys))
                                  (values answer why)))
    (unwind-protect (funcall thunk)
      (if saved (setf (fdefinition symbol) saved) (fmakunbound symbol)))))

(deftest ms-decision-cell-gate-is-off-unless-asked ()
  (with-ms-decision (requests)
    (is (eq :allowed (md-gate-verdict (nlk:json-object "form" "(sh \"rm -rf /\")"))) "off, every call runs as it would")
    (is (null requests) "and nothing is asked of the endpoint")))

(deftest ms-decision-cell-gate-blocks-a-low-score ()
  (with-ms-decision (requests :config ("base_url" "https://right.openai.azure.com"
                                                "gate" t "gate_mode" "block")
                              :answers (list "{\"answers\":{\"q\":{\"type\":\"noul\",\"noul\":0.1}}}"))
    (let ((verdict (md-gate-verdict (nlk:json-object "form" "(sh \"rm -rf /\")"))))
      (is (stringp verdict) "a call scored under the threshold is refused")
      (is (search "refused by nodecode-ms-decision" verdict))
      (is (search "under the 0.50 threshold" verdict)))
    (let ((body (md-body (first requests))))
      (is (equal "Tool call: eval; operation: sh" (nlk:json-value body :string "state")) "only a fixed label and an allowlisted word leave the machine")
      (is (null (search "rm -rf" (fourth (first requests)))) "the form's own text never does"))))

(deftest ms-decision-cell-gate-asks-and-runs-what-the-operator-allows ()
  (with-ms-decision (requests :config ("base_url" "https://right.openai.azure.com" "gate" t)
                              :answers (list "{\"answers\":{\"q\":{\"type\":\"noul\",\"noul\":0.9}}}"))
    (call-with-md-ask-operator "Allow once" :answered
      (lambda ()
        (is (eq :allowed (md-gate-verdict (nlk:json-object "form" "(sh \"ls\")"))) "Allow once runs the call")))
    (call-with-md-ask-operator "Deny" :answered
      (lambda ()
        (let ((verdict (md-gate-verdict (nlk:json-object "form" "(sh \"ls\")"))))
          (is (stringp verdict) "Deny refuses it")
          (is (search "chose Deny" verdict)))))
    (call-with-md-ask-operator nil :nobody
      (lambda ()
        (is (stringp (md-gate-verdict (nlk:json-object "form" "(sh \"ls\")"))) "nobody to ask is a refusal too")))))

(deftest ms-decision-cell-gate-watches-only-its-own-tools ()
  (with-ms-decision (requests :config ("base_url" "https://right.openai.azure.com"
                                                "gate" t "gate_mode" "block"))
    (is (eq :allowed (funcall (cell-hook "nodecode-ms-decision" :tool)
                               (list :name "read_file" :arguments (nlk:json-object) :call-id "c")
                               (lambda (op) (declare (ignore op)) :allowed))) "a tool the gate does not watch runs")
    (is (null requests)))
  (with-ms-decision (requests :config ("base_url" "https://right.openai.azure.com"
                                                "gate" t "gate_mode" "block" "watch" #("web_search"))
                              :answers (list "{\"answers\":{\"q\":{\"type\":\"noul\",\"noul\":0.2}}}"))
    (is (stringp (funcall (cell-hook "nodecode-ms-decision" :tool)
                           (list :name "web_search" :arguments (nlk:json-object "query" "x") :call-id "c")
                           (lambda (op) (declare (ignore op)) :allowed))) "a tool the section adds is watched")
    (is (equal "Tool call: other_tool; operation: unspecified"
               (nlk:json-value (md-body (first requests)) :string "state")))))

