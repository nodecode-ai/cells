;;;; judge.lisp --- TypeSafe's System One: typed questions over a state.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/typesafe.kdl and providers/typesafe.kdl, ai/src/judgment/typesafe.ts
;;;; (TypeSafeJudge: the request, its retries, the answer check) and types.ts
;;;; (the question and answer shapes), coding-agent/src/eval/
;;;; judgment-bridge.ts (how a caller's questions are checked before they
;;;; go, `bool' for the wire's `noul'), catalog/src/discovery/typesafe.ts (the
;;;; model cards), and the bundled row of catalog/src/models.json, which
;;;; models.json in this folder carries.
;;;;
;;;; TypeSafe is not a chat model. One POST /v1/systemone asks a map of named
;;;; questions about one state -- a text, or a JSON object or array -- and
;;;; answers each with a distribution:
;;;;
;;;;   {"state": ..., "model": "jev-latest",
;;;;    "questions": {"<id>": {"type": "choice"|"noul"|"score",
;;;;                           "instructions": "...", "criteria": ...}}}
;;;;   => {"model": "...", "answers": {"<id>": {"type": ..., ...}},
;;;;       "usage": {"input_tokens", "output_tokens", "cost"?}}
;;;;
;;;; a choice its most probable option and every option's probability, a
;;;; noul (yes/no) the probability of yes, a score the probability-weighted
;;;; level. Every question sees the same state and is answered on its own, so
;;;; independent questions go in one call. A bearer key from the TypeSafe
;;;; console; a 408, a 429, a 5xx or a dropped connection is tried again, at
;;;; most three times, waiting what Retry-After says (at most 5 s), else
;;;; 0.5 s, then 1 s.

(in-package #:nodecode-typesafe)

(defparameter +base+ "https://api.typesafe.ai"
  "TypeSafe's public API root (TYPESAFE_DEFAULT_BASE_URL).")

(defparameter +route+ "/v1/systemone"
  "The judgment route under the base (JUDGMENT_ROUTES.typesafe).")

(defparameter +env+ "TYPESAFE_API_KEY"
  "The environment variable a key is read from.")

(defparameter +key-page+ "https://console.typesafe.ai/"
  "Where a key is made.")

(defparameter +timeout+ 10
  "Seconds one attempt may take (DEFAULT_TIMEOUT_MS).")

(defparameter +attempts+ 3
  "How many times one judgment is tried (MAX_ATTEMPTS).")

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string (asdf:system-relative-pathname "nodecode-typesafe" "models.json")))
  "omp's bundled TypeSafe row: the default model and its price.")

(defun default-model ()
  "The bundled row's model: jev-latest."
  (or (nlk:json-value (and (plusp (length +models+)) (aref +models+ 0)) :text "id") "jev-latest"))

;;; --- where, which, with what -----------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun base-url ()
  "The section's base_url, else TYPESAFE_BASE_URL, else the public root; no trailing slash."
  (string-right-trim "/" (or (nonblank (setting :base-url))
                             (nle::credential-env "TYPESAFE_BASE_URL")
                             +base+)))

(defun model ()
  "The section's model, else TYPESAFE_DEFAULT_MODEL, else jev-latest."
  (or (nonblank (setting :model))
      (nle::credential-env "TYPESAFE_DEFAULT_MODEL")
      (default-model)))

(defun stored-key (&optional (path nle::*auth-file-path*))
  "The key auth.json keeps under api_keys.typesafe, or NIL; a store that
cannot be read keeps none."
  (nle::auth-api-key (ignore-errors (nle::read-auth-file path)) +provider+))

(defun api-key (&optional (path nle::*auth-file-path*))
  "(values KEY SOURCE): the saved key over TYPESAFE_API_KEY, as a chat
provider's saved key outranks its variable; NIL when neither holds one."
  (alexandria:if-let (key (stored-key path))
    (values key :auth)
    (alexandria:when-let (key (nle::credential-env +env+))
      (values key :env))))

(defun api-keys (&optional (path nle::*auth-file-path*))
  "Every key this cell would send, saved or in the environment: what a
refusal's text is cleaned of."
  (remove-duplicates (remove nil (list (stored-key path) (nle::credential-env +env+))) :test #'string=))

(defun require-key ()
  "The key a request sends, or the refusal naming where to put one."
  (or (api-key)
      (fail "no TypeSafe key: make one at ~a, then set ~a or save it in auth.json under api_keys.typesafe"
            +key-page+ +env+)))

;;; --- one exchange ---------------------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub DEX:REQUEST under it."
  (apply #'nlk:http method url arguments))

(defun headers (key)
  "What every TypeSafe request carries."
  `(("authorization" . ,(format nil "Bearer ~a" key))
    ("accept" . "application/json")
    ("content-type" . "application/json")))

(defun retry-after-seconds (response-headers)
  "The integral seconds RESPONSE-HEADERS' Retry-After names, or NIL."
  (let ((value (nlk:json-value response-headers :any "retry-after")))
    (typecase value
      (integer value)
      (string (ignore-errors (parse-integer value :junk-allowed t))))))

(defun backoff (attempt response-headers)
  "Seconds to wait before attempt ATTEMPT+1: the server's hint, capped at 5,
else 0.5 doubled per attempt, capped at 5 (backoffMs)."
  (let ((hinted (retry-after-seconds response-headers)))
    (if hinted
        (min hinted 5)
        (min (* 0.5 (expt 2 attempt)) 5))))

(defun label ()
  "The backend a judgment names: typesafe/<model>."
  (format nil "~a/~a" +provider+ (model)))

(defun exchange (method path key &key content)
  "One request to TypeSafe, tried again on a transient failure: the decoded
answer of the first 2xx, or a failure naming the status and what came back."
  (let ((url (format nil "~a~a" (base-url) path)))
    (loop for attempt from 0
          do (multiple-value-bind (body status response-headers)
                 (handler-case (http method url :headers (headers key) :content content
                                                :timeout +timeout+ :connect-timeout +timeout+)
                   (error (condition)
                     (when (>= (1+ attempt) +attempts+)
                       (fail "~a could not be reached: ~a" (label) condition))
                     (values nil nil nil)))
               (let ((text (and body (nlk:body-text body))))
                 (cond ((null status) (sleep (backoff attempt nil)))
                       ((<= 200 status 299)
                        (return (handler-case (nlk:decode-json text)
                                  (error () (fail "~a answered with something that is not JSON" (label))))))
                       ((and (or (= status 408) (= status 429) (>= status 500))
                             (< (1+ attempt) +attempts+))
                        (sleep (backoff attempt response-headers)))
                       (t (fail "~a API error (~d): ~a" (label) status
                                (nlk:clip (nlk:one-line (or text "")) 400 :ellipsis "…")))))))))

;;; --- the questions ------------------------------------------------------------------------
;;; A caller hands them as Lisp or as JSON: a JSON object (a hash table or its
;;; text), or a plist of id and question, a question a plist of :type
;;; :instructions :criteria. They are checked here, before anything goes, as
;;; the eval bridge checks them (parseQuestions), and become the wire's
;;; objects: `bool' is `noul' there.

(defun key-name (key)
  "KEY, a string or a symbol, as the string a JSON object keys it by."
  (if (stringp key) key (string-downcase (symbol-name key))))

(defun plist-p (value)
  "Whether VALUE is a non-empty plist keyed by keywords: a list of strings
is an array, never an object."
  (and (consp value) (evenp (length value))
       (loop for (key) on value by #'cddr always (keywordp key))))

(defun as-object (value)
  "VALUE as a JSON object: itself when a hash table, a plist's pairs, a JSON
text decoded; NIL when it is none of these."
  (cond ((hash-table-p value) value)
        ((plist-p value)
         (apply #'nlk:make-json-object (loop for (key inner) on value by #'cddr
                                             collect (key-name key) collect inner)))
        ((and (stringp value) (uiop:string-prefix-p "{" (nlk:trimmed value)))
         (let ((decoded (ignore-errors (nlk:decode-json value))))
           (and (hash-table-p decoded) decoded)))))

(defun json-of (value)
  "A Lisp value as the JSON it stands for: a plist an object, a list an
array, NIL null; a hash table, a vector, a string, a number as they are."
  (cond ((null value) :null)
        ((eq value t) t)
        ((hash-table-p value)
         (let ((object (make-hash-table :test 'equal)))
           (maphash (lambda (key inner) (setf (gethash key object) (json-of inner))) value)
           object))
        ((plist-p value)
         (apply #'nlk:make-json-object (loop for (key inner) on value by #'cddr
                                             collect (key-name key) collect (json-of inner))))
        ((and (consp value) (not (stringp value))) (map 'vector #'json-of value))
        ((and (vectorp value) (not (stringp value))) (map 'vector #'json-of value))
        ((keywordp value) (key-name value))
        (t value)))

(defun refuse (format-control &rest arguments)
  "The refusal of arguments that do not make a judgment."
  (fail "judge received invalid arguments: ~?" format-control arguments))

(defun check-state (state)
  "STATE as the wire's: a non-empty string is text; an object or an array
(or a plist or list standing for one) is JSON."
  (cond ((stringp state)
         (when (zerop (length state)) (refuse "state must not be empty"))
         state)
        ((or (hash-table-p state) (consp state) (and (vectorp state) (not (stringp state))))
         (json-of state))
        (t (refuse "state must be a string, a JSON object, or a JSON array"))))

(defun instructions (id question)
  (let ((value (nlk:json-value question :text "instructions")))
    (or value (refuse "question ~s needs non-empty string instructions" id))))

(defun check-choice (id question)
  (let ((instructions (instructions id question))
        (criteria (as-object (gethash "criteria" question)))
        (wire (make-hash-table :test 'equal)))
    (unless criteria (refuse "choice question ~s needs criteria: { label: rubric | null }" id))
    (maphash (lambda (label rubric)
               (unless (or (stringp rubric) (null rubric) (eq rubric :null))
                 (refuse "choice question ~s criteria ~s must be a string or null" id label))
               (setf (gethash label wire) (if (stringp rubric) rubric :null)))
             criteria)
    (when (< (hash-table-count wire) 2) (refuse "choice question ~s needs at least two options" id))
    (nlk:json-object "type" "choice" "instructions" instructions "criteria" wire)))

(defun check-bool (id question)
  (let ((instructions (instructions id question))
        (raw (gethash "criteria" question)))
    (if (or (null raw) (eq raw :null))
        (nlk:json-object "type" "noul" "instructions" instructions)
        (let ((criteria (as-object raw))
              (wire (make-hash-table :test 'equal)))
          (unless criteria
            (refuse "bool question ~s criteria must be { true?: string, false?: string }" id))
          (dolist (side '("true" "false"))
            (multiple-value-bind (description present) (gethash side criteria)
              (when present
                (unless (stringp description)
                  (refuse "bool question ~s criteria.~a must be a string" id side))
                (setf (gethash side wire) description))))
          (nlk:json-object "type" "noul" "instructions" instructions "criteria" wire)))))

(defun check-score (id question)
  (let* ((instructions (instructions id question))
         (raw (gethash "criteria" question))
         (levels (and (or (consp raw) (and (vectorp raw) (not (stringp raw)))) (coerce raw 'list))))
    (unless (and levels (every #'stringp levels))
      (refuse "score question ~s needs criteria: [lowest, ..., highest] level descriptions" id))
    (when (< (length levels) 2) (refuse "score question ~s needs at least two levels" id))
    (nlk:json-object "type" "score" "instructions" instructions "criteria" (coerce levels 'vector))))

(defun check-questions (questions)
  "QUESTIONS as the wire's questions object, each checked; => (values OBJECT IDS),
IDS in the order given."
  (let ((object (as-object questions))
        (wire (make-hash-table :test 'equal))
        (ids '()))
    (unless object (refuse "questions must be an object keyed by question id"))
    (maphash (lambda (id raw)
               (let ((question (as-object raw)))
                 (unless question (refuse "question ~s must be an object" id))
                 (let ((type (key-name (or (gethash "type" question) ""))))
                   (setf (gethash id wire)
                         (cond ((equal type "choice") (check-choice id question))
                               ((equal type "bool") (check-bool id question))
                               ((equal type "score") (check-score id question))
                               (t (refuse "question ~s type must be \"choice\", \"bool\", or \"score\"" id))))
                   (push id ids))))
             object)
    (when (zerop (hash-table-count wire)) (refuse "questions must contain at least one question"))
    (values wire (nreverse ids))))

;;; --- the answers ------------------------------------------------------------------------

(defun id-keyword (id)
  "Question ID as the keyword its answer stands under."
  (intern (string-upcase id) :keyword))

(defun distribution (object)
  "A probabilities OBJECT as an alist of (label . probability), in the wire's order."
  (let ((pairs '()))
    (when (hash-table-p object)
      (maphash (lambda (label value) (push (cons label value) pairs)) object))
    (nreverse pairs)))

(defun cell-answer (answer)
  "One wire ANSWER as the caller reads it: a plist, the wire's noul as bool."
  (let ((type (nlk:json-value answer :string "type")))
    (cond ((equal type "noul") (list :type :bool :bool (nlk:json-value answer :number "noul")))
          ((equal type "choice")
           (list :type :choice
                 :choice (nlk:json-value answer :string "choice")
                 :probabilities (distribution (nlk:json-value answer :object "probabilities"))
                 :confidence (nlk:json-value answer :number "confidence")))
          (t (list :type :score
                   :score (nlk:json-value answer :number "score")
                   :probabilities (distribution (nlk:json-value answer :object "probabilities"))
                   :confidence (nlk:json-value answer :number "confidence"))))))

;;; --- the verbs ---------------------------------------------------------------------------

(define-verb judge (state questions &key model)
  "Answer typed QUESTIONS about STATE with TypeSafe's System One. STATE is a
string, or a JSON object or array (a hash table, a plist, a list). QUESTIONS
maps ids to questions, as a plist or a JSON object: (:id (:type \"choice\"
:instructions TEXT :criteria (:label RUBRIC-OR-NIL ...))), (:type \"bool\"
:instructions TEXT [:criteria (:true TEXT :false TEXT)]), (:type \"score\"
:instructions TEXT :criteria (LOWEST ... HIGHEST)). => (values ANSWERS MODEL
USAGE): ANSWERS a plist of each id's keyword and its answer -- (:type :choice
:choice LABEL :probabilities ((LABEL . P) ...) :confidence C), (:type :bool
:bool P-YES), (:type :score :score LEVEL :probabilities ((\"0\" . P) ...)
:confidence C) -- MODEL the backend that answered, USAGE (:input-tokens N
:output-tokens N [:cost USD])."
  (let ((state (check-state state)))
    (multiple-value-bind (wire ids) (check-questions questions)
      (let* ((key (require-key))
             (model (or (nonblank model) (model)))
             (answer (exchange :post +route+ key
                               :content (nlk:encode-json-object
                                         (nlk:json-object "state" state "model" model "questions" wire))))
             (answers (nlk:json-value answer :object "answers")))
        (values
         (loop for id in ids
               for want = (nlk:json-value (gethash id wire) :string "type")
               for got = (nlk:json-value answers :object id)
               unless (equal want (nlk:json-value got :string "type"))
                 do (fail "~a/~a response is missing a ~s answer for question ~s" +provider+ model want id)
               nconc (list (id-keyword id) (cell-answer got)))
         (format nil "~a/~a" +provider+ (or (nlk:json-value answer :text "model") model))
         (let ((usage (nlk:json-value answer :object "usage")))
           (list* :input-tokens (or (nlk:json-value usage :integer "input_tokens") 0)
                  :output-tokens (or (nlk:json-value usage :integer "output_tokens") 0)
                  (alexandria:when-let (cost (nlk:json-value usage :number "cost"))
                    (list :cost cost)))))))))

(define-verb models ()
  "The judgment models this TypeSafe key may use, one line each: its name,
its release date and what it is for (GET /v1/models)."
  (let* ((answer (exchange :get "/v1/models" (require-key)))
         (cards (nlk:json-array answer "models")))
    (if (zerop (length cards))
        "TypeSafe lists no models for this key"
        (format nil "~{~a~^~%~}"
                (loop for card across cards
                      for name = (nlk:json-value card :text "name")
                      when name
                        collect (format nil "~a~@[ (~a)~]~@[ - ~a~]" name
                                        (nlk:json-value card :text "release_date")
                                        (nonblank (nlk:json-value card :string "description"))))))))
