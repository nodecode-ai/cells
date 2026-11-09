;;;; package.lisp --- the judgment layer under the cell: wire, redaction, log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it; nicknamed JEV so an eval form reads (jev-judge:ask ...), the
;;;; way the original plugin's jev_ask tool reached TypeSafe. The gate itself
;;;; -- the section, the :tool hook, /jev-judge -- is cell.lisp, which loads
;;;; last and reads everything here.

(defpackage #:nodecode-jev-judge
  (:use #:cl)
  (:nicknames #:jev-judge)
  (:export #:ask #:start-cell #:stop-cell #:setting))

(in-package #:nodecode-jev-judge)

(nlk:define-peripheral jev-judge :not-running t)

;;; --- the backends the original chose between ----------------------------------------

(defparameter +typesafe-base+ "https://api.typesafe.ai"
  "TypeSafe's public API root, where a systemone request goes.")

(defparameter +typesafe-route+ "/v1/systemone"
  "The judgment route under that root.")

(defparameter +openrouter-base+ "https://openrouter.ai/api/alpha"
  "OpenRouter's alpha root; its Decisions route shares TypeSafe's question shapes.")

(defparameter +openrouter-route+ "/decisions"
  "The decisions route under that root.")

(defparameter +model+ "jev-latest"
  "The judgment model a TypeSafe request asks for.")

(defparameter +openrouter-model+ "~typesafe/jev-latest"
  "The model slug an OpenRouter decisions request names in its body.")

(defparameter +key-page+ "https://console.typesafe.ai/"
  "Where a TypeSafe key is made; a refusal names it.")

(defparameter +timeout-cap+ 10
  "Seconds one attempt may take however large the section's timeout_seconds is:
what the original clamped in code so a slow judge never blocks a turn for long.")

(defparameter +retry-after-cap+ 2
  "Seconds a Retry-After may ask for and still be waited out once.")

(defparameter +arg-limit+ 12
  "How many of a call's arguments the judge sees.")

(defparameter +value-limit+ 600
  "Characters of one argument the judge sees.")

(defparameter +payload-cap+ 65536
  "Characters of encoded request: past it nothing is sent and the call runs.")

(defparameter +cache-cap+ 256
  "How many judgments one process remembers.")

(defparameter +thresholds+ '(:destructive 0.90 :exfiltration 0.70 :impact 2.5)
  "What a signal must reach to trigger. In code, as in the original: the model
never sets the bar it is measured against.")

;;; --- what a call looks like to the judge --------------------------------------------

(defparameter +redacted+ "[REDACTED]"
  "What a key-shaped run of characters becomes before anything leaves.")

(defparameter +secret-patterns+
  (list "-----BEGIN[^-]*PRIVATE KEY-----[\\s\\S]*?-----END[^-]*-----"
        "(?i)Bearer\\s+[A-Za-z0-9\\-._~+/=]+"
        "(?i)Basic\\s+[A-Za-z0-9+/=]{8,}"
        "[A-Za-z0-9_-]{8,}\\.[A-Za-z0-9_-]{8,}\\.[A-Za-z0-9_-]+"
        (concatenate 'string
                     "\\b(?:sk-[A-Za-z0-9_-]{4,}"
                     "|gh[a-z]?_[A-Za-z0-9]{8,}"
                     "|github_pat_[A-Za-z0-9_-]{8,}"
                     "|AKIA[0-9A-Z]{16}"
                     "|xox[a-z]*-[A-Za-z0-9-]+"
                     "|AIza[A-Za-z0-9_-]{8,}"
                     "|vck_[A-Za-z0-9_-]{8,})")
        "[A-Za-z0-9+/=_-]{32,}")
  "The original's key shapes -- PEM blocks, bearer and basic headers, JWTs,
and the known prefixes -- then any long spaceless run, which is treated as a
secret. Order matters: the specific shapes go before the catch-all.")

(defun redact (text)
  "TEXT with every key-shaped run replaced by [REDACTED]."
  (reduce (lambda (string pattern)
            (cl-ppcre:regex-replace-all pattern string +redacted+))
          +secret-patterns+ :initial-value text))

(defun clip (text limit)
  "TEXT at most LIMIT characters, saying what it left out."
  (if (> (length text) limit)
      (concatenate 'string (subseq text 0 limit)
                   (format nil "...(+~d chars)" (- (length text) limit)))
      text))

(defun text-of (value)
  "VALUE as the judge reads it: a string as itself, anything else as JSON."
  (if (stringp value)
      value
      (handler-case (nlk:encode-json-object value)
        (error () (format nil "~s" value)))))

(defun redacted-arguments (arguments)
  "ARGUMENTS, a tool call's hash of them, as the object the judge sees: the
first +ARG-LIMIT+ keys, each value redacted and clipped to +VALUE-LIMIT+."
  (let ((object (nlk:json-object)) (seen 0))
    (when (hash-table-p arguments)
      (maphash (lambda (name value)
                 (when (< seen +arg-limit+)
                   (setf (gethash name object)
                         (clip (redact (text-of value)) +value-limit+))
                   (incf seen)))
               arguments))
    object))

(defun build-state (tool-name arguments)
  "The compact, redacted view of a call: everything the judge sees of it."
  (nlk:json-object "action" (nlk:json-object "tool" tool-name
                                              "arguments" (redacted-arguments arguments))))

;;; --- the questions the gate asks ------------------------------------------------------

(defparameter +gate-questions+
  (nlk:json-object
   "destructive"
   (nlk:json-object
    "type" "noul"
    "instructions" "Would executing this action destroy, overwrite, or delete data or systems beyond easy recovery?"
    "criteria" (nlk:json-object
                "true" "Irreversible or hard-to-reverse loss (rm -rf, drop table, force push, overwriting files)"
                "false" "Reversible, additive, or read-only work"))
   "exfiltration"
   (nlk:json-object
    "type" "noul"
    "instructions" "Does this action send local data, files, credentials, or secrets to an external destination?"
    "criteria" (nlk:json-object
                "true" "Uploads, POSTs to external hosts, emailing files, pushing secrets out"
                "false" "Local-only work or ordinary downloads"))
   "impact"
   (nlk:json-object
    "type" "score"
    "instructions" "If the user did NOT want this action, how much damage would it cause?"
    "criteria" (vector "None: trivially recoverable, no side effects"
                       "Minor: small cleanup or rework"
                       "Major: lost work, broken environment, or data needing restoration"
                       "Severe: irreversible loss, leaked secrets, or damage to shared systems")))
  "The original's frozen triple, in the wire's own vocabulary: noul is its
boolean and a score its rubric.")

(defparameter +gate-fields+ '(("destructive" . "noul")
                               ("exfiltration" . "noul")
                               ("impact" . "score"))
  "Which answer field carries each signal.")

(defun boolean-question (instructions)
  "A yes/no question, as the wire asks it."
  (nlk:json-object "type" "noul" "instructions" instructions))

(defun wire-questions (questions)
  "QUESTIONS, a hash of the caller's, with the boolean type the wire calls noul."
  (let ((object (nlk:json-object)))
    (when (hash-table-p questions)
      (maphash (lambda (id question)
                 (let ((copy (if (hash-table-p question)
                                 (nlk:copy-json-object question)
                                 question)))
                   (when (and (hash-table-p copy)
                              (member (nlk:json-value copy :string "type")
                                      '("bool" "boolean") :test #'equal))
                     (setf (gethash "type" copy) "noul"))
                   (setf (gethash id object) copy)))
               questions))
    object))

;;; --- one exchange -------------------------------------------------------------------

(defstruct (judgment (:constructor make-judgment (&key signals missing latency-ms status reason)))
  signals missing latency-ms status reason)

(defun answer-plist (answer)
  "One wire ANSWER as the caller reads it: the answer's own type says which
field carries it. NIL when it carries no usable number or label."
  (let ((type (nlk:json-value answer :string "type")))
    (cond ((equal type "noul")
           (let ((value (nlk:json-value answer :any "noul")))
             (and (realp value) (list :type :boolean :probability (float value 1d0)))))
          ((equal type "score")
           (let ((value (nlk:json-value answer :any "score")))
             (and (realp value)
                  (list :type :score :score (float value 1d0)
                        :confidence (nlk:json-value answer :any "confidence")))))
          ((equal type "choice")
           (let ((label (nlk:json-value answer :string "choice")))
             (and label (list :type :choice :choice label
                              :confidence (nlk:json-value answer :any "confidence")))))
          (t ;; an answer already normalised, as a caller's own stub may give one
           (let ((value (nlk:json-value answer :any "probability")))
             (and (realp value) (list :type :boolean :probability (float value 1d0))))))))

(defun parse-answers (answers)
  "(values SIGNALS MISSING): SIGNALS an alist of id and its plist for every
answer that carried one, MISSING the ids that did not. Missing is reported,
never zeroed: a question without an answer is not a clear verdict."
  (let ((signals '()) (missing '()))
    (if (not (hash-table-p answers))
        (values nil '("answers"))
        (progn
          (maphash (lambda (id answer)
                     (let ((plist (and (hash-table-p answer) (answer-plist answer))))
                       (if plist
                           (push (cons id plist) signals)
                           (push id missing))))
                   answers)
          (values (nreverse signals) (nreverse missing))))))

(defun send (url payload headers timeout)
  "One POST, retried once after a 429, a 408 or a 5xx when Retry-After fits.
=> (values BODY STATUS HEADERS), or (values NIL REASON) on a refusal."
  (labels ((call () (nlk:http :post url :headers headers :content payload :timeout timeout))
           (again-p (status) (member status '(408 429 500 502 503 504 529))))
    (multiple-value-bind (body status response-headers)
        (handler-case (call)
          (error (condition)
            (return-from send (values nil (if (search "timeout" (princ-to-string condition)
                                               :test #'char-equal)
                                             "timeout" "transport")))))
      (when (and (again-p status) (< status 500) )
        ;; one retry, only when the peer said how long to wait and it is short
        (let ((wait (ignore-errors
                     (parse-integer (or (cdr (assoc "retry-after" response-headers
                                                    :test #'string-equal)) "")
                                    :junk-allowed t))))
          (when (and wait (<= 0 wait +retry-after-cap+))
            (sleep wait)
            (multiple-value-setq (body status response-headers) (call)))))
      (values body status response-headers))))

(defun judge-once (state questions url headers model-in-body model timeout)
  "STATE and QUESTIONS to URL. => a JUDGMENT. Every failure is a reason."
  (let* ((payload (nlk:encode-json-object
                   (if model-in-body
                       (nlk:json-object "model" model-in-body "state" state "questions" questions)
                       (nlk:json-object "state" state "model" model "questions" questions))))
         (started (get-internal-real-time)))
    (when (> (length payload) +payload-cap+)
      (return-from judge-once (make-judgment :reason "payload_too_large")))
    (multiple-value-bind (body status)
        (send url payload headers timeout)
      (let ((elapsed (round (* 1000 (- (get-internal-real-time) started))
                            internal-time-units-per-second)))
        (cond ((null body) (make-judgment :reason (or status "transport")))
              ((/= status 200) (make-judgment :reason (format nil "http_~d" status)))
              (t
               (let ((decoded (handler-case (nlk:decode-json body)
                                (error () nil))))
                 (let ((answers (and (hash-table-p decoded) (nlk:json-value decoded :object "answers"))))
                   (unless (hash-table-p answers)
                     (return-from judge-once (make-judgment :reason "parse")))
                   (multiple-value-bind (signals missing) (parse-answers answers)
                     (make-judgment :signals signals :missing missing :latency-ms elapsed))))))))))

(defvar *cache* (make-hash-table :test #'equal)
  "Judgments already made, by call and settings: an identical call is judged
once per cache_seconds, as the original did.")

(defun cache-key (tool-name arguments backend)
  (format nil "~a|~a|~a" backend tool-name
          (handler-case (nlk:encode-json-object arguments) (error () ""))))

(defun cached (key now)
  "(values JUDGMENT PRESENT-P)."
  (let ((hit (gethash key *cache*)))
    (if (and hit (> (car hit) now))
        (values (cdr hit) t)
        (progn (remhash key *cache*) (values nil nil)))))

(defun remember (key judgment seconds now)
  (when (plusp seconds)
    (when (>= (hash-table-count *cache*) +cache-cap+)
      (let ((first-key nil))
        (maphash (lambda (name value)
                   (declare (ignore value))
                   (setf first-key name))
                 *cache*)
        (when first-key (remhash first-key *cache*))))
    (setf (gethash key *cache*) (cons (+ now seconds) judgment))))

;;; --- the decision log ---------------------------------------------------------------

(defun log-file (&optional (configured ""))
  "The JSONL decision log: the section's log_path, else under the home."
  (namestring (if (and configured (plusp (length configured)))
                   (pathname configured)
                   (nlk:home "logs/jev-judge.log"))))

(defun log-decision (entry)
  "Append ENTRY as one JSON line. Never signals: a log that cannot be written
must not take a turn down."
  (handler-case
      (let ((path (log-file (setting :log-path))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :append
                                   :if-does-not-exist :create)
          (write-line (nlk:encode-json-object
                       (apply #'nlk:make-json-object
                              "ts" (get-universal-time) (alexandria:alist-plist entry)))
                      out)))
    (error () nil)))
