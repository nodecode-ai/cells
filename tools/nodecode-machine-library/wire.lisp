;;;; wire.lisp --- the one network seam, the key, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through REQUEST, so a test stubs the one
;;;; name and never touches the network. The API answers an HTTP status as a
;;;; value; anything but a 2xx is MACHINE-LIBRARY-ERROR with the service's
;;;; own message in it, which is what the model repairs from. A 404 is not an
;;;; error here: it says the API holds no document for that url, which the
;;;; extract tool answers one url at a time.

(in-package #:nodecode-machine-library)

(defparameter +base-url+ "https://api.machinelibrary.ai"
  "Machine Library's API, where every call goes unless the section names another.")

(defparameter +site-url+ "https://machinelibrary.ai"
  "The public site: where a document's page and a key are.")

(defparameter +version+ "0.1.0"
  "This cell's version, carried in the user agent the original also sent one of.")

(defparameter +max-search-limit+ 20
  "The most documents one search answers with: the API's own ceiling.")

(defparameter +unreserved+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
  "The characters a url keeps as they are; every other one is written %XX.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names inline or by variable, else MACHINELIBRARY_API_KEY,
the variable the original's README told its users to export."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "MACHINELIBRARY_API_KEY"))))

(defun check-config ()
  "Refuse a section that would call Machine Library without a key, or with a
count the API cannot answer; name the member the operator repairs."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "machine-library: this cell needs a key: set MACHINELIBRARY_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the machine-library section; a key is made at "
                                 "https://machinelibrary.ai/keys")))
  (when (> (setting :max-results) +max-search-limit+)
    (error 'nlk:config-refusal
           :detail (format nil "machine-library: max_results is at most ~d; no search answers with more"
                           +max-search-limit+))))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +base-url+)))

(defun site ()
  "The public site this cell links to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :site-url) ""))))
    (if (plusp (length given)) given +site-url+)))

(defun auth-headers ()
  "The headers every request carries: the key where the API reads it, and the
attribution header the original sent, so its usage stays one shape on the
service's side."
  (list (cons "x-api-key" (or (api-key) ""))
        (cons "x-request-source" "api")
        (cons "accept" "application/json")
        (cons "user-agent" (format nil "nodecode-machine-library/~a" +version+))))

;;; --- urls --------------------------------------------------------------------

(defun percent-encode (text)
  "TEXT as a url path segment: every character but the unreserved ones written
%XX over its UTF-8 octets, a / included, as the original encoded a DOI."
  (with-output-to-string (stream)
    (loop for character across (or text "")
          for code = (char-code character)
          do (cond ((and (<= code 127) (find character +unreserved+))
                    (write-char character stream))
                   ((<= code 127)
                    (format stream "%~2,'0X" code))
                   (t (loop for octet across (sb-ext:string-to-octets
                                               (string character) :external-format :utf-8)
                            do (format stream "%~2,'0X" octet)))))))

(defun percent-decode (text)
  "TEXT with its %XX escapes read back as characters; a malformed escape is
kept as it was written."
  (with-output-to-string (stream)
    (let ((index 0)
          (end (length text)))
      (loop while (< index end)
            for character = (char text index)
            do (if (and (char= character #\%)
                        (<= (+ index 3) end)
                        (digit-char-p (char text (1+ index)) 16)
                        (digit-char-p (char text (+ index 2)) 16))
                   (progn (write-char (code-char (parse-integer text :start (1+ index)
                                                                :end (+ index 3) :radix 16))
                                      stream)
                          (incf index 3))
                   (progn (write-char character stream)
                          (incf index)))))))

(defun split-on (text character)
  "TEXT's parts between CHARACTER, in order."
  (loop with start = 0
        for position = (position character text :start start)
        collect (subseq text start position)
        while position
        do (setf start (1+ position))))

(defun encode-path (text)
  "TEXT as a url path: each /-separated part percent-encoded, the slashes kept."
  (format nil "~{~a~^/~}" (mapcar #'percent-encode (split-on (or text "") #\/))))

;;; --- the exchange -------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER --- an HTTP body --- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (nlk:trimmed answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :text "detail")
        (nlk:json-value body :text "error")
        (nlk:json-value (nlk:json-value body :object "detail") :text "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun request (method path &key content timeout)
  "METHOD PATH on the configured root => the decoded answer, or NIL when the
API holds no document for it (HTTP 404). A transport failure, or any other
refused status, signals MACHINE-LIBRARY-ERROR."
  (let* ((url (concatenate 'string (root) path))
         (limit (or timeout (setting :timeout-seconds)))
         (headers (if content
                      (cons (cons "content-type" "application/json") (auth-headers))
                      (auth-headers)))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (apply #'nlk:http method url
                 :headers headers
                 :timeout limit
                 :connect-timeout (min 10 limit)
                 (and content (list :content content))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "machine-library ~a: ~a" path (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300)) (decode-body answer))
          ((and (integerp status) (= status 404)) nil)
          ((and (integerp status) (= status 401))
           (fail "machine-library ~a: HTTP 401, the key was refused; check api_key in the machine-library section"
                 path))
          ((and (integerp status) (= status 402))
           (fail "machine-library ~a: HTTP 402, this account's balance is spent; top up at ~a/keys"
                 path (site)))
          ((and (integerp status) (= status 429))
           (fail "machine-library ~a: HTTP 429, the rate limit; wait before asking again" path))
          (t (fail "machine-library ~a: HTTP ~d~@[: ~a~]" path status (body-error-text answer))))))

;;; --- reading answers ----------------------------------------------------------

(defun json-object-of (value)
  "VALUE when it is a decoded JSON object, else NIL."
  (and (hash-table-p value) value))

(defun object-text (object key)
  "KEY of the JSON OBJECT, as text, or NIL."
  (nlk:json-value object :text key))

(defun object-array (object key)
  "KEY of the JSON OBJECT as a list of its elements, or NIL."
  (let ((value (nlk:json-value object :array key)))
    (and value (coerce value 'list))))

(defun objects-of (object key)
  "The objects of OBJECT's KEY array, dropping any entry that is not one."
  (remove-if-not #'hash-table-p (object-array object key)))

(defun blank-p (value)
  "True when VALUE is NIL or the empty string."
  (or (null value) (and (stringp value) (zerop (length value)))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left out."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun collapse-space (text)
  "TEXT with every run of whitespace as one space, and none at either end."
  (let ((out (make-string-output-stream))
        (pending nil)
        (written nil))
    (loop for character across (or text "")
          do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                 (setf pending written)
                 (progn (when pending (write-char #\Space out))
                        (setf pending nil)
                        (setf written t)
                        (write-char character out))))
    (get-output-stream-string out)))
