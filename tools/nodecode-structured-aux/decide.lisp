;;;; decide.lisp --- the decision transport, redaction, and three of the contracts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; OpenRouter's decision route is not a chat route: one POST carries one
;;;; shared state and a map of named questions, every question a `choice' over
;;;; labels the caller enumerates in advance, and answers one chosen label and
;;;; its probability per question. Everything here is built around that bound:
;;;; a request the caller cannot express as a bounded choice is refused rather
;;;; than guessed, and an answer outside the contract is a failure, never a
;;;; plausible-looking substitute. Rewritten from hermes-structured-aux-models'
;;;; structured_aux/config.py, privacy.py, decisions.py, contracts.py.

(in-package #:nodecode-structured-aux)

;;; --- what a request is made of -----------------------------------------------------

(defparameter +provider+ "openrouter"
  "The provider id: what auth.json keeps a key under (api_keys.openrouter).")

(defparameter +base+ "https://openrouter.ai"
  "OpenRouter's root; the decision route sits under it (config.DEFAULT_BASE_URL).")

(defparameter +path+ "/api/alpha/decisions"
  "The decision route (config.DEFAULT_PATH).")

(defparameter +default-model+ "~typesafe/jev-latest"
  "The model that answers decisions (config.DEFAULT_MODEL): a moving alias, so the
operator can pin an exact id in the section's model.")

(defparameter +env+ "OPENROUTER_API_KEY"
  "The environment variable a key is read from when auth.json has none.")

(defparameter +key-page+ "https://openrouter.ai/settings/keys"
  "Where a key is made.")

(defparameter +user-agent+ "nodecode-structured-aux/0.1.0"
  "What every request says it comes from.")

(defparameter +redacted+ "[REDACTED]")

(defparameter +approval-labels+ '("APPROVE" "DENY" "ESCALATE")
  "The three labels the approval contract bounds its answer to. Three, not two:
ESCALATE is how a guardian says a human must decide, and folding it into DENY
would silently turn an unknown risk into a refusal.")

(defparameter +approval-criteria+
  '(("APPROVE" . "The operations are safe to run unattended: read-only, idempotent, or their effects are confined and clearly intended.")
    ("DENY" . "The operations are genuinely destructive, exfiltrating, or otherwise harmful, and must not run.")
    ("ESCALATE" . "The risk cannot be determined from the supplied command and description; a human must decide."))
  "What each verdict means, as the original's contract states it.")

(defparameter +approval-instructions+
  "Assess the ACTUAL risk of the shell operations in the request. Many flagged commands are false positives. Choose APPROVE only when the operations are clearly safe, DENY only when they are clearly harmful, and ESCALATE whenever the supplied evidence is insufficient to be confident.")

(defparameter +tool-instructions+
  "Select the single tool that best satisfies the request. Choose only from the supplied candidates.")

(defparameter +keep-drop-labels+ '("KEEP" "DROP"))

(defparameter +keep-drop-criteria+
  '(("KEEP" . "This block carries constraints, decisions, exact failure evidence, unresolved state, or identifiers that later work still depends on.")
    ("DROP" . "This block is stale, superseded, routine, or recoverable by rerunning the work, and nothing later depends on its exact text.")))

(defparameter +keep-drop-instructions+
  "Decide whether this block of working context must survive into the retained set. Bias toward KEEP when the block holds an exact value, an open question, or evidence of a failure; bias toward DROP only for clearly redundant or recoverable material.")

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

;;; --- the key ------------------------------------------------------------------------

(defun stored-key (&optional (path nle::*auth-file-path*))
  "The key auth.json keeps under api_keys.openrouter -- what /connect and the
openrouter cell's sign-in write -- or NIL."
  (nle::auth-api-key (ignore-errors (nle::read-auth-file path)) +provider+))

(defun env-key ()
  "The key OPENROUTER_API_KEY holds, or NIL."
  (nle::credential-env +env+))

(defun api-key ()
  "(values KEY SOURCE): the saved key over the variable, as a chat provider's
saved key outranks its environment; NIL when neither holds one."
  (let ((key (stored-key)))
    (if key (values key :auth) (values (env-key) :env))))

(defun api-keys ()
  "Every key this cell would send: what a refusal's text is cleaned of."
  (remove-duplicates (remove nil (list (stored-key) (env-key))) :test #'string=))

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings (or text "") (api-keys) "[redacted]"))

(defun require-key ()
  "The key a request sends, or the refusal naming where to put one."
  (or (api-key)
      (fail "no OpenRouter key: make one at ~a, then set ~a or save it in auth.json under api_keys.openrouter"
            +key-page+ +env+)))

;;; --- redaction before anything leaves ------------------------------------------------
;;; Deliberately narrow, as the original's fallback is: an obvious credential
;;; assignment or a known token prefix. Over-redacting would strip the command
;;; text the approval contract exists to assess.

(defparameter +assignment-scanner+
  (cl-ppcre:create-scanner
   "(?i)\\b([A-Z0-9_]*(?:API_?KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL)[A-Z0-9_]*)\\s*[:=]\\s*(\"[^\"]*\"|'[^']*'|\\S+)")
  "NAME = VALUE, for a credential-shaped NAME (privacy._ASSIGNMENT_RE).")

(defparameter +bearer-scanner+
  (cl-ppcre:create-scanner "(?i)\\b(bearer)\\s+([A-Za-z0-9._-]{12,})")
  "A Bearer token (privacy._BEARER_RE).")

(defparameter +prefix-scanner+
  (cl-ppcre:create-scanner "\\b(sk-[A-Za-z0-9_-]{12,}|gh[pousr]_[A-Za-z0-9]{20,}|ghs_[A-Za-z0-9]{20,})\\b")
  "Known token prefixes (privacy._KNOWN_PREFIX_RE).")

(defun local-redact (text)
  "TEXT with credential-shaped substrings replaced, in the original's order."
  (cl-ppcre:regex-replace-all
   +prefix-scanner+
   (cl-ppcre:regex-replace-all
    +bearer-scanner+
    (cl-ppcre:regex-replace-all +assignment-scanner+ (or text "") "\\1=[REDACTED]")
    "\\1 [REDACTED]")
   +redacted+))

(defun redact-text (text)
  "TEXT redacted unless the section turned redaction off."
  (if (setting :redact) (local-redact text) text))

(defun redact-value (value)
  "VALUE with every string leaf redacted: a hash table maps, a vector maps,
anything else stands."
  (cond ((stringp value) (redact-text value))
        ((hash-table-p value)
         (let ((copy (make-hash-table :test 'equal)))
           (maphash (lambda (key inner) (setf (gethash key copy) (redact-value inner))) value)
           copy))
        ((and (vectorp value) (not (stringp value))) (map 'vector #'redact-value value))
        (t value)))

;;; --- JSON the caller wrote ------------------------------------------------------------

(defun json-of (value)
  "A Lisp value as the JSON it stands for: a plist an object, a list an
array, NIL null, a keyword its lowercase name."
  (cond ((null value) :null)
        ((eq value t) t)
        ((hash-table-p value) value)
        ((keywordp value) (string-downcase (symbol-name value)))
        ((and (consp value) (keywordp (first value))) (as-object value))
        ((consp value) (coerce (mapcar #'json-of value) 'vector))
        ((and (vectorp value) (not (stringp value))) (map 'vector #'json-of value))
        (t value)))

(defun as-object (plist)
  "PLIST as a JSON object: a keyword key reads lowercase, a string key as it is."
  (let ((object (make-hash-table :test 'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (if (stringp key) key (string-downcase (symbol-name key))) object)
                   (json-of value)))
    object))

(defun state-json (state)
  "STATE as the wire's: a non-empty string is text; an object or an array (a
hash table, a plist, a list) is JSON."
  (cond ((stringp state)
         (when (zerop (length state)) (fail "the state must not be empty"))
         state)
        ((hash-table-p state) state)
        ((consp state)
         (if (keywordp (first state)) (as-object state) (coerce (mapcar #'json-of state) 'vector)))
        ((and (vectorp state) (not (stringp state))) (map 'vector #'json-of state))
        (t (fail "the state must be a string, a JSON object, or a JSON array"))))

;;; --- questions and answers --------------------------------------------------------------

(defun choice-question (instructions criteria)
  "One wire `choice' question from CRITERIA, an alist of (LABEL . RUBRIC-OR-NIL).
A rubric of NIL is sent as null: a label with nothing to explain."
  (when (< (length criteria) 2)
    (fail "a choice question needs at least two options: ~s" (mapcar #'car criteria)))
  (let ((wire (make-hash-table :test 'equal)))
    (dolist (pair criteria)
      (setf (gethash (car pair) wire) (or (cdr pair) :null)))
    (nlk:make-json-object "type" "choice" "instructions" instructions "criteria" wire)))

(defun check-question (id spec)
  "SPEC, a plist (:instructions TEXT :criteria ((LABEL . RUBRIC) ...)), as a
wire question."
  (unless (and (consp spec) (keywordp (first spec)))
    (fail "question ~(~a~) must be a plist of :instructions and :criteria" id))
  (let ((instructions (getf spec :instructions))
        (criteria (getf spec :criteria)))
    (unless (nonblank instructions)
      (fail "question ~(~a~) needs non-empty :instructions" id))
    (unless (and (consp criteria)
                 (every (lambda (pair)
                          (and (consp pair) (stringp (car pair))
                               (or (null (cdr pair)) (stringp (cdr pair)))))
                        criteria))
      (fail "question ~(~a~) needs :criteria as an alist of (LABEL . RUBRIC-OR-NIL)" id))
    (choice-question (nlk:trimmed instructions) criteria)))

(defun checked-questions (questions)
  "QUESTIONS as an alist (ID . WIRE-QUESTION): a plist of id and question, or
a hash table of wire questions passed through."
  (cond ((hash-table-p questions)
         (let ((pairs '()))
           (maphash (lambda (id question) (push (cons id (check-question id question)) pairs)) questions)
           (nreverse pairs)))
        ((and (consp questions) (evenp (length questions)))
         (loop for (id spec) on questions by #'cddr
               collect (cons (if (stringp id) id (string-downcase (symbol-name id)))
                             (check-question id spec))))
        (t (fail "questions must be a plist of id and (:instructions TEXT :criteria ((LABEL . RUBRIC) ...))"))))

(defun id-keyword (id)
  "Question ID as the keyword its answer stands under."
  (intern (string-upcase id) :keyword))

(defun distribution (object)
  "A probabilities OBJECT as an alist of (label . probability), in the wire's order."
  (let ((pairs '()))
    (when (hash-table-p object)
      (maphash (lambda (label value) (push (cons label value) pairs)) object))
    (nreverse pairs)))

(defun read-answer (answer)
  "One wire answer as the caller reads it: (:type :choice :choice LABEL
:probabilities ((LABEL . P) ...) :confidence C)."
  (list :type :choice
        :choice (nlk:json-value answer :string "choice")
        :probabilities (distribution (nlk:json-value answer :object "probabilities"))
        :confidence (or (nlk:json-value answer :number "confidence") 0.0)))

(defun answer-choice (answers id allowed)
  "The label ANSWERS gave ID, which must be one of ALLOWED."
  (let* ((answer (getf answers id))
         (choice (and (consp answer) (getf answer :choice))))
    (unless (and (stringp choice) (member choice allowed :test #'string=))
      (fail "the decision provider answered ~s for ~(~a~), outside its contract (~{~a~^, ~})"
            (or choice "nothing") id allowed))
    choice))

;;; --- one exchange ----------------------------------------------------------------------

(defun endpoint ()
  "The decision URL the section asks for, HTTPS checked here as the transport
checks it again."
  (let* ((base (string-right-trim "/" (or (nonblank (setting :base-url)) +base+)))
         (path (let ((value (or (nonblank (setting :path)) +path+)))
                 (if (uiop:string-prefix-p "/" value) value (concatenate 'string "/" value))))
         (url (concatenate 'string base path)))
    (unless (uiop:string-prefix-p "https://" url)
      (fail "the decision base_url must use https:// (it is ~a)" url))
    url))

(defun backend ()
  "What a failure calls the far end."
  (format nil "~a/~a" +provider+ (or (nonblank (setting :model)) +default-model+)))

(defun headers (key)
  "What every decision request carries: the bearer key, and OpenRouter's app
attribution, which is what names the app in the provider's own usage."
  (list (cons "authorization" (format nil "Bearer ~a" key))
        (cons "accept" "application/json")
        (cons "content-type" "application/json")
        (cons "user-agent" +user-agent+)
        (cons "http-referer" (or (nonblank (setting :app-referer)) "https://nodecode.ai"))
        (cons "x-openrouter-title" (or (nonblank (setting :app-title)) "nodecode-structured-aux"))))

(defun http (content key)
  "The cell's one network call, NLK:HTTP, one attempt: tests stub DEX:REQUEST
under it."
  (handler-case
      (nlk:http :post (endpoint) :headers (headers key) :content content
                :timeout (setting :timeout-seconds)
                :connect-timeout (setting :timeout-seconds))
    (error (condition)
      (fail "~a could not be reached: ~a" (backend) condition))))

(defun ask (state questions &key model)
  "One bounded decision: STATE (a string or a JSON object) and QUESTIONS, an
alist (ID . WIRE-QUESTION). => (values ANSWERS MODEL USAGE), ANSWERS a plist
of each id's keyword and (:type :choice :choice LABEL :probabilities
((LABEL . P) ...) :confidence C). Every question must come back answered."
  (let ((object (make-hash-table :test 'equal))
        (ids '()))
    (dolist (pair questions)
      (setf (gethash (car pair) object) (cdr pair))
      (push (car pair) ids))
    (when (null ids) (fail "a decision needs at least one question"))
    (let* ((key (require-key))
           (asked (or (nonblank model) (setting :model) +default-model+))
           (state (state-json state))
           (payload (nlk:make-json-object
                     "state" (if (setting :redact) (redact-value state) state)
                     "model" asked
                     "questions" object))
           (body (nlk:encode-json-object payload)))
      (multiple-value-bind (response status response-headers) (http body key)
        (declare (ignore response-headers))
        (let ((text (and response (nlk:body-text response))))
          (unless (and (integerp status) (<= 200 status 299))
            (fail "~a answered HTTP ~a: ~a" (backend) (or status 0)
                  (nlk:clip (nlk:one-line (or text "")) 400 :ellipsis "...")))
          (let ((data (handler-case (nlk:decode-json (or text ""))
                        (error () (fail "~a answered with something that is not JSON" (backend))))))
            (unless (hash-table-p data)
              (fail "~a answered with a non-object" (backend)))
            (let ((answers (nlk:json-value data :object "answers")))
              (unless answers
                (fail "~a answered without `answers'" (backend)))
              (values
               (loop for id in (nreverse ids)
                     for answer = (gethash id answers)
                     unless (hash-table-p answer)
                       do (fail "~a is missing an answer for ~s" (backend) id)
                     nconc (list (id-keyword id) (read-answer answer)))
               (or (nlk:json-value data :text "model") asked)
               (nlk:json-value data :object "usage")))))))))

;;; --- the contracts ----------------------------------------------------------------------

(defun tool-candidates (candidates)
  "CANDIDATES as an alist (NAME . DESCRIPTION): a list of names, or a plist of
name and description. A decision returns a label, never synthesized
arguments, so a tool that requires parameters is not answerable here."
  (let ((pairs
          (cond ((null candidates) '())
                ((and (consp candidates) (every #'stringp candidates))
                 (mapcar (lambda (name) (cons name (format nil "Tool ~a" name))) candidates))
                ((and (consp candidates) (evenp (length candidates))
                      (every #'stringp (loop for (name) on candidates by #'cddr collect name)))
                 (loop for (name description) on candidates by #'cddr
                       collect (cons name
                                     (if (nonblank description) description
                                         (format nil "Tool ~a" name)))))
                (t (fail "candidates must be a list of tool names, or a plist of name and description")))))
    (remove-duplicates pairs :key #'car :test #'string= :from-end t)))

(define-verb decide (state questions &key model)
  "Answer bounded QUESTIONS about STATE with OpenRouter's decision route. STATE
is a string, or a JSON object or array (a hash table, a plist, a list).
QUESTIONS maps ids to choices, as a plist: (:id (:instructions TEXT :criteria
((LABEL . RUBRIC-OR-NIL) ...))), two or more labels each. => (values ANSWERS
MODEL USAGE): ANSWERS a plist of each id's keyword and (:type :choice :choice
LABEL :probabilities ((LABEL . P) ...) :confidence C), MODEL what answered,
USAGE the provider's token counts. A label outside the contract fails."
  (ask state (checked-questions questions) :model model))

(define-verb approve (command &key policy model)
  "Judge a shell COMMAND (or a description of what will run) as APPROVE, DENY or
ESCALATE. POLICY is the operator's own policy text, sent as state. => (values
VERDICT CONFIDENCE PROBABILITIES MODEL): VERDICT :APPROVE, :DENY, :ESCALATE or
:UNKNOWN -- a choice the provider made outside the three is never read as a
refusal, and never as consent. PROBABILITIES an alist of (\"APPROVE\" . P)."
  (unless (nonblank command)
    (fail "approve needs the command or operations to assess"))
  (multiple-value-bind (answers model-name usage)
      (ask (nlk:make-json-object "request" command
                                  "guardian_policy" (if (nonblank policy) (nlk:trimmed policy) ""))
           (list (cons "verdict" (choice-question +approval-instructions+ +approval-criteria+)))
           :model model)
    (declare (ignore usage))
    (let* ((answer (getf answers :verdict))
           (choice (getf answer :choice))
           (known (member choice +approval-labels+ :test #'string=)))
      (values (if known (intern choice :keyword) :unknown)
              (getf answer :confidence)
              (getf answer :probabilities)
              model-name))))

(define-verb pick-tool (request candidates &key model)
  "Choose the one tool that satisfies REQUEST out of CANDIDATES -- a list of
tool names, or a plist of name and description. Two or more candidates are
required: a bounded choice needs something to bound. => (values NAME
CONFIDENCE PROBABILITIES MODEL)."
  (let ((pairs (tool-candidates candidates)))
    (when (< (length pairs) 2)
      (fail "pick-tool needs at least two candidate tools: a bounded choice needs something to bound"))
    (let ((tools (make-hash-table :test 'equal)))
      (dolist (pair pairs) (setf (gethash (car pair) tools) (cdr pair)))
      (multiple-value-bind (answers model-name usage)
          (ask (nlk:make-json-object "request" (or request "") "available_tools" tools)
               (list (cons "tool" (choice-question +tool-instructions+ pairs)))
               :model model)
        (declare (ignore usage))
        (let ((answer (getf answers :tool)))
          (values (answer-choice answers :tool (mapcar #'car pairs))
                  (getf answer :confidence)
                  (getf answer :probabilities)
                  model-name))))))
