;;;; wire.lisp --- the decision provider, the redactor, and the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nerve asks one typed question of a decision model and reads the answer
;;;; as a label with probabilities. One wire carries all of it: POST
;;;; {state, model, questions} to a System One endpoint, answered
;;;; {model, answers, usage}. Three hosted roots speak it (TypeSafe's own,
;;;; OpenRouter's decisions alpha, OpenCode Zen), and a base_url of the
;;;; operator's own does too; the section says which.

(in-package #:nodecode-nerve)

;;; --- failure ------------------------------------------------------------------
;;
;; NERVE-ERROR and FAIL are the peripheral's own: the gateway renders a
;; refused start from them, and every verb answers one condition.

(nlk:define-peripheral nerve :not-running t)

;;; --- who answers ----------------------------------------------------------------

(defparameter +providers+
  '(("typesafe" :base "https://api.typesafe.ai" :path "/v1/systemone"
     :model "jev-1.14.0" :env "TYPESAFE_API_KEY" :label "TypeSafe")
    ("openrouter" :base "https://openrouter.ai" :path "/api/alpha/decisions"
     :model "typesafe/jev-1.14" :env "OPENROUTER_API_KEY" :label "OpenRouter")
    ("opencode" :base "https://opencode.ai" :path "/zen/v1/systemone"
     :model "jev-1.13" :env "OPENCODE_API_KEY" :label "OpenCode Zen"))
  "The three roots that speak the decisions wire, as the original shipped them.")

(defparameter +local-path+ "/v1/systemone"
  "The path a base_url of the operator's own is asked on, as the original's
generic System One backend reads a server on this machine or their network.")

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun own-base-p ()
  "Whether base_url names a root other than the provider's own: a server of
the operator's, which need not take a key."
  (plusp (length (or (nonblank (setting :base-url)) ""))))

(defun provider-key ()
  "The provider named outright, or the one a key in the environment picks:
TypeSafe when only its key is set, OpenRouter otherwise (as the original)."
  (let ((named (string-downcase (or (nonblank (setting :provider)) "auto"))))
    (if (equal named "auto")
        (if (nle::credential-env "TYPESAFE_API_KEY") "typesafe" "openrouter")
        named)))

(defun provider-row ()
  "The row +PROVIDERS+ holds for the provider in force; an unknown name fails."
  (let ((key (provider-key)))
    (or (assoc key +providers+ :test #'equal)
        (fail "nerve: ~a is not one of the decision providers (typesafe, openrouter, opencode)"
              key))))

(defun field (name)
  "PROVIDER-ROW's NAME member."
  (getf (rest (provider-row)) name))

(defun base-url ()
  "The root a request goes to: the section's base_url when set, else the
provider's own; no trailing slash."
  (string-right-trim "/" (or (nonblank (setting :base-url)) (field :base))))

(defun endpoint ()
  "The URL one decision is asked on."
  (format nil "~a~a" (base-url) (if (own-base-p) +local-path+ (field :path))))

(defun model ()
  "The model label the request carries: the section's, else the root's own."
  (or (nonblank (setting :model)) (field :model)))

(defun label ()
  "The backend this cell names in what it answers: provider/model."
  (format nil "~a/~a" (provider-key) (model)))

(defun api-key ()
  "The key one request sends: the section's own, else the provider's
environment variable; NIL when neither holds one."
  (or (nonblank (setting :api-key)) (nle::credential-env (field :env))))

(defun require-key ()
  "The key; :LOCAL at a root of the operator's own with none; else the
refusal naming the variable to set."
  (or (api-key)
      (when (own-base-p) :local)
      (fail "nerve: no ~a key -- set ~a, name one in the nerve section, or point base_url at a System One server of your own"
            (field :label) (field :env))))

(defun timeout-seconds ()
  "How long one request may take."
  (max 1 (or (setting :timeout-seconds) 10)))

;;; --- what leaves this machine -------------------------------------------------------

(defparameter +secret-keys+
  '("api_key" "apikey" "authorization" "auth" "cookie" "password" "passwd" "secret"
    "token" "access_token" "refresh_token" "private_key" "client_secret" "credential"
    "credentials" "aws_access_key_id" "aws_secret_access_key" "session_token")
  "A member named one of these is replaced whole, as the original reads them.")

(defparameter +secret-suffixes+
  '("_token" "_secret" "_password" "_api_key" "_access_key" "_private_key"
    "_credential" "_credentials")
  "A member whose name ends with one of these is replaced whole.")

(defparameter +secret-patterns+
  (list "(?i)(bearer\\s+)[A-Za-z0-9._~+/=-]{8,}"
        "\\bgh[pousr]_[A-Za-z0-9]{20,}\\b"
        "\\bsk-[A-Za-z0-9_-]{16,}\\b"
        "\\bxox[baprs]-[A-Za-z0-9-]{10,}\\b"
        "\\b(?:AKIA|ASIA)[A-Z0-9]{16}\\b"
        "\\beyJ[A-Za-z0-9_-]{8,}\\.[A-Za-z0-9_-]{8,}\\.[A-Za-z0-9_-]{8,}\\b"
        "(?i)([?&](?:api[_-]?key|access[_-]?token|token|secret|password)=)[^&#\\s]+"
        "(?i)\\b([A-Z0-9_.-]*(?:API[_-]?KEY|TOKEN|SECRET|PASSWORD)\\s*[=:]\\s*)[^\\s,;]+")
  "Tokens a state may carry as text, as the original scrubs them; the one
group each pattern keeps is what stays beside [REDACTED].")

(defun secret-key-p (key)
  "Whether a member named KEY is replaced whole."
  (let ((name (string-downcase (string key))))
    (or (member name +secret-keys+ :test #'equal)
        (some (lambda (suffix) (uiop:string-suffix-p suffix name)) +secret-suffixes+))))

(defun scrub-text (text)
  "TEXT with every token +SECRET-PATTERNS+ names replaced: the bearer word or
the key's name stays, so the answer still reads as what was there."
  (reduce (lambda (text pattern)
            (cl-ppcre:regex-replace-all pattern text "(?:\\1)[REDACTED]"))
          +secret-patterns+ :initial-value text))

(defun redacted (value)
  "VALUE with credentials replaced: a member named like a secret whole, and a
token inside any string. Everything else is copied as it stands."
  (cond ((hash-table-p value)
         (let ((out (nlk:json-object)))
           (maphash (lambda (key item)
                      (setf (gethash key out)
                            (if (secret-key-p key) "[REDACTED]" (redacted item))))
                    value)
           out))
        ((and (consp value) (keywordp (first value)))
         (loop for (key item) on value by #'cddr
               append (list key (if (secret-key-p key) "[REDACTED]" (redacted item)))))
        ((consp value) (mapcar #'redacted value))
        ((and (vectorp value) (not (stringp value))) (map 'vector #'redacted value))
        ((stringp value) (scrub-text value))
        (t value)))

;;; --- one exchange ------------------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub DEX:REQUEST under it."
  (apply #'nlk:http method url arguments))

(defun headers (key)
  "What every request carries: the bearer KEY when there is one."
  `(,@(when (and key (not (eq key :local)))
        `(("authorization" . ,(format nil "Bearer ~a" key))))
    ("accept" . "application/json")
    ("content-type" . "application/json")
    ("user-agent" . "nodecode-nerve/0.1.0")))

(defun body-text-of (body)
  "BODY, an nlk:http answer, as text."
  (cond ((null body) "")
        ((stringp body) body)
        (t (or (ignore-errors (nlk:body-text body)) (format nil "~a" body)))))

(defun exchange (url key content)
  "One POST of CONTENT to URL with KEY, answered as the decoded JSON object;
a transport failure or a non-2xx answer is a NERVE-ERROR naming the backend."
  (multiple-value-bind (body status)
      (handler-case (http :post url :headers (headers key) :content content
                          :timeout (timeout-seconds) :connect-timeout (timeout-seconds))
        (error (condition) (values nil (format nil "~a" condition))))
    (cond ((stringp status)
           (fail "~a could not be reached: ~a" (label) status))
          ((not (and (integerp status) (<= 200 status 299)))
           (fail "~a error (~a): ~a" (label) status
                 (nlk:clip (nlk:one-line (body-text-of body)) 300 :ellipsis "...")))
          (t (handler-case (nlk:decode-json (body-text-of body))
               (error () (fail "~a answered with something that is not JSON" (label))))))))

(defun question-object (type instructions criteria)
  "One typed question as the wire carries it: a bool is the wire's `noul'."
  (apply #'nlk:make-json-object
         "type" (if (equal type "bool") "noul" (string-downcase (string type)))
         "instructions" instructions
         (when criteria (list "criteria" criteria))))

(defun criteria-object (pairs)
  "PAIRS, a list of (LABEL . RUBRIC), as the JSON object the wire wants."
  (let ((object (nlk:json-object)))
    (dolist (pair pairs object)
      (setf (gethash (car pair) object) (cdr pair)))))

;;; --- what this start has counted ---------------------------------------------------------

(defvar *calls* 0 "Decision calls made since this start.")
(defvar *verdicts* (make-hash-table :test 'equal) "How many answers of each value.")
(defvar *reviews* 0 "Tool calls the gate consulted the decision model about.")
(defvar *refusals* 0 "Tool calls the gate refused.")

(defun count-verdict (value)
  "One more call, and one more answer of VALUE."
  (incf *calls*)
  (when value (incf (gethash value *verdicts* 0))))

(defun reset-stats ()
  "Forget what the last start counted."
  (setf *calls* 0 *reviews* 0 *refusals* 0 *verdicts* (make-hash-table :test 'equal)))

;;; --- the wire ------------------------------------------------------------------------------

(defun system-one (state questions)
  "One decisions call: the redacted STATE, QUESTIONS a JSON object, the
provider's model. => (values ANSWERS MODEL USAGE REQUEST-ID LATENCY-MS)."
  (let* ((key (require-key))
         (payload (nlk:json-object "state" (redacted state)
                                    "model" (model)
                                    "questions" questions))
         (started (get-internal-real-time))
         (answer (exchange (endpoint) key (nlk:encode-json-object payload)))
         (answers (and (hash-table-p answer) (nlk:json-value answer :object "answers")))
         (latency (round (* 1000 (- (get-internal-real-time) started))
                         internal-time-units-per-second)))
    (unless answers
      (fail "~a answered without an answers object" (label)))
    (values answers
            (or (nlk:json-value answer :text "model") (model))
            (nlk:json-value answer :object "usage")
            (nlk:json-value answer :text "id")
            latency)))

(defun answer-for (answers name)
  "The answer ANSWERS carries for the question NAME, or a refusal naming it."
  (let ((answer (and (hash-table-p answers) (gethash name answers))))
    (or (and (hash-table-p answer) answer)
        (fail "~a answered without an answer for ~a" (label) name))))

(defun distribution (object)
  "OBJECT, a JSON object of label to probability, as an alist (LABEL . P) in
the server's own order."
  (let ((pairs '()))
    (when (hash-table-p object)
      (maphash (lambda (label value)
                 (push (cons label (or (ignore-errors (coerce value 'double-float)) 0d0))
                       pairs))
               object))
    (nreverse pairs)))

(defun probability-of (probabilities label)
  "The probability PROBABILITIES gives LABEL, or 0."
  (or (cdr (assoc label probabilities :test #'equal)) 0d0))

(defun choice-of (state instructions choices criteria &optional (question "decision"))
  "Ask one choice question and read the answer: the label, its confidence,
the whole distribution. => a plist naming all of it."
  (let ((labels (remove-duplicates
                 (remove "" (mapcar (lambda (choice) (nlk:trimmed (string choice))) choices)
                         :test #'equal)
                 :test #'equal)))
    (when (< (length labels) 2)
      (fail "nerve: at least two distinct choices are needed, got ~a" (length labels)))
    (let ((questions
            (nlk:make-json-object
             question (question-object
                       "choice" instructions
                       (criteria-object
                        (mapcar (lambda (choice)
                                  (cons choice (cdr (assoc choice criteria :test #'equal))))
                                labels))))))
      (multiple-value-bind (answers model usage request-id latency)
          (system-one state questions)
        (let* ((answer (answer-for answers question))
               (value (or (nlk:json-value answer :text "choice") ""))
               (probabilities (distribution (nlk:json-value answer :object "probabilities")))
               (confidence (or (nlk:json-value answer :number "confidence")
                               (probability-of probabilities value))))
          (unless (plusp (length value))
            (fail "~a answered without a choice" (label)))
          (unless (member value labels :test #'equal)
            (fail "~a answered with an out-of-contract choice: ~a" (label) value))
          (count-verdict value)
          (list :value value
                :confidence (coerce confidence 'double-float)
                :probabilities probabilities
                :model model
                :provider (provider-key)
                :usage usage
                :request-id (or request-id "")
                :latency-ms latency))))))

(defun decide (state instructions choices &key criteria)
  "Ask how to act: CHOICES, two or more labels, CRITERIA an alist of
(label . rubric) saying when each holds.
=> (:value LABEL :confidence P :probabilities (LABEL . P) ... :model M ...)."
  (choice-of state instructions choices criteria))

(defun rank (state instructions items)
  "Order ITEMS, an alist of (LABEL . rubric), by the probability the model
gives each.
=> (:value BEST :confidence P :ranking ((LABEL . P) ...) :model M ...)."
  (when (or (not (listp items)) (< (length items) 2))
    (fail "nerve: rank needs at least two candidates, as (label . rubric) pairs"))
  (let ((result (choice-of state instructions (mapcar #'car items) items)))
    (list :value (getf result :value)
          :confidence (getf result :confidence)
          :ranking (sort (copy-list (getf result :probabilities))
                          (lambda (a b) (> (cdr a) (cdr b))))
          :model (getf result :model)
          :provider (getf result :provider)
          :usage (getf result :usage))))

(defparameter +verify-choices+
  '(("PASS" . "Evidence satisfies the requested outcome with no material unresolved issue.")
    ("RETRY" . "The approach is valid but execution should be tried again with the same plan.")
    ("REPLAN" . "The current approach is insufficient; change the plan before trying again.")
    ("ESCALATE" . "Human judgment, permission, missing information, or an external dependency is required."))
  "The ladder the original's verify asks, and what each rung means.")

(defun verify (state instructions)
  "Ask whether the work is done: PASS, RETRY, REPLAN or ESCALATE, the
evidence in STATE. => the plist DECIDE answers."
  (choice-of state instructions
             (mapcar #'car +verify-choices+) +verify-choices+))

(defun questions-object (plist)
  "PLIST, (NAME (:type ... :instructions ... :criteria ...) ...), as the JSON
object the wire carries; a criteria plist becomes an object, a list a vector."
  (let ((object (nlk:json-object)))
    (loop for (name spec) on plist by #'cddr
          do (setf (gethash (string-downcase (string name)) object)
                   (question-object
                    (or (getf spec :type) "noul")
                    (getf spec :instructions)
                    (let ((criteria (getf spec :criteria)))
                      (typecase criteria
                        (null nil)
                        (hash-table criteria)
                        (list (if (keywordp (first criteria))
                                  (criteria-object
                                   (loop for (label rubric) on criteria by #'cddr
                                         collect (cons (string-downcase (string label)) rubric)))
                                  (coerce criteria 'vector)))
                        (t criteria))))))
    object))

(defun assess (state questions)
  "Ask QUESTIONS as the wire carries them: a plist of (NAME (:type ...
:instructions ... :criteria ...)), a JSON object, or that object's text.
=> (values ANSWERS MODEL USAGE)."
  (let ((object (cond ((hash-table-p questions) questions)
                       ((stringp questions)
                        (handler-case (nlk:decode-json questions)
                          (error () (fail "nerve: the questions are not JSON"))))
                       (t (questions-object questions)))))
    (unless (and (hash-table-p object) (plusp (hash-table-count object)))
      (fail "nerve: assess needs at least one question"))
    (maphash (lambda (name question)
               (let ((type (and (hash-table-p question)
                                (string-downcase (or (nlk:json-value question :text "type") "")))))
                 (unless (member type '("noul" "bool" "choice" "score") :test #'equal)
                   (fail "nerve: question ~a has no usable type" name))
                 (unless (nlk:json-value question :text "instructions")
                   (fail "nerve: question ~a needs instructions" name))))
             object)
    (let ((answers (system-one state object)))
      (maphash (lambda (name answer)
                 (count-verdict (if (hash-table-p answer)
                                    (or (nlk:json-value answer :text "choice") "noul")
                                    (string-downcase (string name)))))
               answers)
      answers)))

(defun stats ()
  "What this start counted: calls, answers by value, gate reviews and
refusals, and where it asks."
  (let ((verdicts '()))
    (maphash (lambda (value count) (push (cons value count) verdicts)) *verdicts*)
    (list :calls *calls*
          :verdicts (sort verdicts #'string< :key #'car)
          :reviews *reviews*
          :refusals *refusals*
          :provider (provider-key)
          :model (model)
          :base-url (base-url))))
