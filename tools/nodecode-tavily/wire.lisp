;;;; wire.lisp --- the one network seam, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through TAVILY-POST, so a test stubs the
;;;; one name and never touches the network. Tavily's API answers HTTP status
;;;; as a value; anything but a 2xx is TAVILY-ERROR with the service's own
;;;; message in it, which is what the model repairs from.

(in-package #:nodecode-tavily)

(defparameter +base-url+ "https://api.tavily.com"
  "Tavily's API, where every tool goes unless the section names another.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names inline or by variable, else TAVILY_API_KEY, as
the plugin's packaged connection reads it."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "TAVILY_API_KEY"))))

(defun check-config ()
  "Refuse a section that could reach Tavily with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "tavily: this cell needs a key: set TAVILY_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the tavily section; a key is made at "
                                 "app.tavily.com"))))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +base-url+)))

(defun auth-headers ()
  "The plugin's own authentication: a bearer key, and the attribution header
both of its connection examples keep."
  (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
        (cons "x-client-name" "nodecode tavily cell")))

(defun bolt (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL VALUE: a
Tavily request carries only what the call asked for."
  (loop for (key value) on pairs by #'cddr
        when value append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

(defun strings (vector)
  "VECTOR — a contract's array of strings — as a JSON array, or NIL when empty."
  (when (and vector (plusp (length vector)))
    (coerce (remove-if-not #'stringp (coerce vector 'list)) 'vector)))

(defun decode-body (answer)
  "ANSWER — an HTTP body — as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value (nlk:json-value body :object "detail") :text "error")
        (nlk:json-value body :text "detail")
        (nlk:json-value body :text "message")
        (nlk:json-value body :text "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun post (path body &key timeout)
  "POST BODY — a JSON object — to PATH on the configured root => the decoded
answer. A transport failure or a refused status signals TAVILY-ERROR."
  (let* ((url (concatenate 'string (root) path))
         (limit (or timeout (setting :timeout-seconds)))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post url
                    :headers (cons '("content-type" . "application/json") (auth-headers))
                    :content (nlk:encode-json-object body)
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "tavily ~a: ~a" path (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300)) (decode-body answer))
          ((and (integerp status) (= status 401))
           (fail "tavily ~a: HTTP 401, the key was refused; check api_key in the tavily section" path))
          ((and (integerp status) (= status 432))
           (fail "tavily ~a: HTTP 432, this account is out of credits" path))
          ((and (integerp status) (= status 429))
           (fail "tavily ~a: HTTP 429, rate or credit limit; do not retry at once" path))
          (t (fail "tavily ~a: HTTP ~d~@[: ~a~]" path status (body-error-text answer))))))

(defun object-text (object key)
  "KEY of the JSON OBJECT, as text, or NIL."
  (and (hash-table-p object) (nlk:json-value object :text key)))

(defun objects-of (body)
  "BODY's result objects: the \"results\" array, as a list, dropping any
entry that is not an object."
  (let ((results (nlk:json-value body :array "results")))
    (and results (remove-if-not #'hash-table-p (coerce results 'list)))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))
