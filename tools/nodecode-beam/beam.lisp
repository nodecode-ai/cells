;;;; beam.lisp --- Beam's HTTP API and the `beam' CLI, as the tools reach them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from Beam-Network/beam-hermes (see NOTICE): beam_hermes/api.py,
;;;; which reads /auth/me, /transfers and /transfers/<id> with an X-Api-Key
;;;; header, and beam_hermes/rooms.py, which drives the installed `beam' CLI
;;;; with --json --no-interactive and reads the JSON it answers.
;;;;
;;;; Two seams, and nothing else dials: *GETENV* is every read of the
;;;; environment, *RUN* every run of the CLI. A test replaces both.

(in-package #:nodecode-beam)

;;; --- the words ------------------------------------------------------------------

(defparameter +list-statuses+ '("pending" "in_progress" "completed" "failed" "cancelled")
  "What a transfer list may be narrowed to, as the original's LIST_STATUSES is.")

(defparameter +order-by+ '("created_at" "completed_at")
  "The orders a transfer list takes.")

(defparameter +room-states+ '("active" "closed" "all")
  "Which Rooms a list shows.")

(defparameter +channel-kinds+ '("message" "stream" "datagram" "request-reply" "object" "media")
  "The kinds a Room channel is created as.")

(defparameter +grant-actions+ '("discover" "subscribe" "publish" "manage")
  "What a channel grant may allow.")

(defparameter +transfer-fields+
  '("id" "status" "total_bytes" "bytes_transferred" "error_message"
    "created_at" "started_at" "completed_at")
  "The transfer fields the model is shown: the original's TRANSFER_FIELDS, its
internal metadata left out.")

;;; --- the seams ------------------------------------------------------------------

(defvar *getenv* #'uiop:getenv
  "The one place the environment is read: (NAME) => its value, or NIL.
Tests replace it; nothing else reads the environment.")

;;; --- the key --------------------------------------------------------------------

(defun api-key ()
  "The Beam API key the section's variable holds, or a refusal naming it."
  (let* ((name (setting :api-key-env))
         (value (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (or (funcall *getenv* name) ""))))
    (when (zerop (length value))
      (fail "~a is not set: make a Beam API key at ~a and put it in that variable."
            name (setting :console-url)))
    value))

;;; --- the API ---------------------------------------------------------------------

(defun url-encode (text)
  "TEXT percent-encoded, its unreserved characters kept."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets (princ-to-string text) :external-format :utf-8)
          for character = (code-char byte)
          do (if (or (alphanumericp character) (find character "-._~"))
                 (write-char character out)
                 (format out "%~2,'0X" byte)))))

(defun query-string (parameters)
  "PARAMETERS, (NAME . VALUE) pairs, as a query string; a NIL value is left out."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in parameters
                when (and value (not (equal value "")))
                  collect (format nil "~a=~a" name (url-encode value)))))

(defun decode-body (body)
  "BODY, JSON text, as a value; an object is answered as one however odd."
  (let ((value (and (stringp body) (ignore-errors (nlk:decode-json body)))))
    (or value (make-hash-table :test #'equal))))

(defun call-api (path &key (method :get) parameters)
  "One Beam API exchange => the decoded body. Anything but HTTP 200 is
refused with BEAM-ERROR, in the words the original's api.py uses."
  (let* ((key (api-key))
         (url (format nil "~a~a~@[?~a~]"
                       (string-right-trim "/" (setting :api-url))
                       path
                       (and parameters (query-string parameters)))))
    (multiple-value-bind (body status)
        (handler-case
            (nlk:http method url
                      :headers (list (cons "X-Api-Key" key)
                                     (cons "Accept" "application/json"))
                      :timeout (setting :timeout-seconds))
          (error (condition)
            (fail "Could not reach the Beam API at ~a: ~a."
                  (setting :api-url) (type-of condition))))
      (cond ((eql status 200) (decode-body body))
            ((member status '(401 403))
             (fail "Beam refused the API key (HTTP ~d): check it, or make a new one at ~a."
                   status (setting :console-url)))
            ((eql status 404)
             (fail "Beam has nothing with that ID in this organization (HTTP 404)."))
            (t (fail "Beam answered HTTP ~d~@[: ~a~]." status
                     (and (stringp body) (subseq body 0 (min 300 (length body))))))))))

;;; --- what a transfer reads as -------------------------------------------------------

(defun field (object name)
  "OBJECT's NAME, whatever its type, or NIL."
  (and (hash-table-p object) (gethash name object)))

(defun text-field (object name)
  "OBJECT's NAME as a string, or NIL."
  (let ((value (field object name)))
    (and (not (null value)) (princ-to-string value))))

(defun bytes-text (value)
  "VALUE bytes as a short line, or NIL when VALUE is not a number."
  (when (numberp value)
    (cond ((>= value (expt 10 9)) (format nil "~,2f GB" (/ value 1d9)))
          ((>= value (expt 10 6)) (format nil "~,1f MB" (/ value 1d6)))
          ((>= value (expt 10 3)) (format nil "~,1f kB" (/ value 1d3)))
          (t (format nil "~d B" value)))))

(defun transfer-line (transfer)
  "One transfer as one line: its id, its status, its bytes, its error, its date."
  (let ((done (bytes-text (field transfer "bytes_transferred")))
        (total (bytes-text (field transfer "total_bytes"))))
    (format nil "~a  ~a~@[  ~a~]~@[ of ~a~]~@[  ~a~]~@[  ~a~]"
            (or (text-field transfer "id") "?")
            (or (text-field transfer "status") "?")
            done total
            (text-field transfer "error_message")
            (text-field transfer "created_at"))))

(defun percentage (transfer)
  "How far TRANSFER has gone, or NIL when it says nothing."
  (let ((done (field transfer "bytes_transferred"))
        (total (field transfer "total_bytes")))
    (when (and (numberp done) (numberp total) (plusp total))
      (format nil "~,1f%" (* 100 (/ done total))))))

;;; --- the beam CLI ------------------------------------------------------------------

(defun cli-program ()
  "The beam CLI the section names, else `beam' on PATH."
  (let ((named (setting :beam-cli)))
    (if (and (stringp named) (plusp (length named))) named "beam")))

(defun resolve-program (name)
  "NAME as a path: itself when it carries one, else the first of that name on PATH."
  (if (or (zerop (length name)) (find #\/ name) (find #\\ name))
      name
      (or (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                    :separator '(#\: #\;))
                 for path = (merge-pathnames name
                                              (uiop:ensure-directory-pathname
                                               (if (plusp (length directory)) directory ".")))
                 when (probe-file path) return (uiop:native-namestring path))
          name)))

(defun run-beam-cli (arguments input)
  "ARGUMENTS run through the beam CLI with INPUT on its standard input
=> (VALUES STDOUT STDERR EXIT-CODE)."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program (cons (resolve-program (cli-program)) arguments)
                            :input (or input "")
                            :output :string
                            :error-output :string
                            :timeout (setting :cli-timeout-seconds)
                            :ignore-error-status t)
        (values (or out "") (or err "") (or code 0)))
    (serious-condition (condition)
      (values "" (princ-to-string condition) -1))))

(defvar *run* #'run-beam-cli
  "The one place the beam CLI runs: (ARGUMENTS INPUT) => (VALUES STDOUT STDERR
EXIT-CODE). Tests replace it; nothing else runs a program.")

;;; --- reading what the CLI answered ---------------------------------------------------

(defun render-value (value &optional (indent 0))
  "VALUE as indented text: an object one key a line, an array one item a line."
  (let ((pad (make-string indent :initial-element #\Space)))
    (cond ((hash-table-p value)
           (if (zerop (hash-table-count value))
               "{}"
               (format nil "~{~a~^~%~}"
                       (loop for key being the hash-keys of value using (hash-value item)
                             collect (format nil "~a~a: ~a" pad key
                                             (render-value item (+ indent 2)))))))
          ((and (vectorp value) (not (stringp value)))
           (if (zerop (length value))
               "[]"
               (format nil "~{~a~^~%~}"
                       (loop for item across value
                             collect (format nil "~a- ~a" pad
                                             (render-value item (+ indent 2)))))))
          ((null value) "null")
          (t (princ-to-string value)))))

(defun render-output (text)
  "TEXT, the CLI's --json answer, as text a model reads: JSON rendered, or
TEXT itself when it is not JSON."
  (let ((value (and (stringp text)
                    (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
                    (ignore-errors (nlk:decode-json text)))))
    (if value (render-value value) (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))))
