;;;; api.lisp --- the key, the endpoint, and what Fish Audio answers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One key (the section's, else the variable it names), one endpoint root
;;;; (https unless the operator says otherwise for a stand-in), and one
;;;; exchange that answers every status as a value. Nothing here holds a key
;;;; or a host: both are read at each call, so a rotation needs no restart.

(in-package #:nodecode-fish-audio)

;;; --- octets and text ----------------------------------------------------------

(defun octets-of (data)
  "DATA as octets: NLK:HTTP answers a string when the peer sent text."
  (etypecase data
    (string (map '(vector (unsigned-byte 8)) #'char-code data))
    ((array (unsigned-byte 8) (*)) data)
    ((array t (*)) data)
    (null (make-array 0 :element-type '(unsigned-byte 8)))))

(defun body-text (data)
  "DATA, text or octets, as a string: what an error body is read out of."
  (if (or (stringp data) (null data))
      (or data "")
      (octets-text data)))

(defun octets-text (data)
  (map 'string #'code-char data))

(defun trimmed-text (value)
  (and value (nlk:trimmed value)))

;;; --- the key -------------------------------------------------------------------

(defun api-key ()
  "The key a call is billed to: the section's, else the variable api_key_env
names. A refusal here costs nothing, so it is raised before any request."
  (let ((key (trimmed-text (setting :api-key))))
    (when (zerop (length key))
      (setf key (trimmed-text (funcall *getenv* (setting :api-key-env)))))
    (when (or (null key) (zerop (length key)))
      (fail "no Fish Audio key: paste one into the fish-audio section's api_key, or set the variable ~a. ~
             A key is free to create at https://fish.audio/app/api-keys. Nothing was sent and nothing was billed."
            (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the Fish Audio key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was billed."))
    key))

(defun checked-base-url (text)
  "TEXT as the endpoint root, without its trailing slash, or a CONFIG-REFUSAL.
Plain http is allowed only for a loopback host: anywhere else it would send
the key in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "fish-audio.api_base_url is empty; it is https://api.fish.audio unless a stand-in is wanted"))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest host (plusp (length host)))
        (nlk:config-error "fish-audio.api_base_url is ~s; it must be an https URL, such as https://api.fish.audio" url))
      (when (and plain (not (member (string-downcase host) '("localhost" "127.0.0.1" "::1" "[::1]")
                                     :test #'string=)))
        (nlk:config-error "fish-audio.api_base_url is ~s; plain http would send the key in clear, so it is allowed only for a localhost stand-in" url))
      url)))

;;; --- the exchange ---------------------------------------------------------------

(defun percent-encoded (text)
  "TEXT with everything but RFC 3986's unreserved characters escaped, as a
query value has it."
  (with-output-to-string (out)
    (loop for octet across (nlk:utf-8-octets (or text ""))
          do (let ((char (code-char octet)))
               (if (or (alphanumericp char) (find char "-._~" :test #'char=))
                   (write-char char out)
                   (format out "%~2,'0X" octet))))))

(defun query-url (path params)
  "PATH under the section's endpoint root, PARAMS as its query string in the
order given."
  (let ((url (format nil "~a~a" (setting :api-base-url) path)))
    (if (null params)
        url
        (format nil "~a?~{~a~^&~}"
                url
                (loop for (name . value) in params
                      when (and value (not (and (stringp value) (zerop (length value)))))
                        collect (format nil "~a=~a" name (percent-encoded (princ-to-string value))))))))

(defun request (method path &key key model params content content-type binary
                                     (timeout 60) (max-bytes (* 64 1024 1024)))
  "(values BODY STATUS HEADERS TRUNCATED): one exchange with Fish Audio, BODY
text unless BINARY. A transport failure becomes this cell's condition, so the
model reads one sentence instead of a Lisp backtrace."
  (handler-case
      (nlk:http method (query-url path params)
                :headers (append (list (cons "authorization" (format nil "Bearer ~a" key)))
                                 (when model (list (cons "model" model)))
                                 (when content-type (list (cons "content-type" content-type))))
                :content content
                :binary binary
                :timeout timeout
                :max-bytes max-bytes)
    (error (condition)
      (declare (ignore condition))
      (fail "the request to Fish Audio did not complete: ~a is unreachable or too slow. Try again shortly; ~
             nothing was billed twice by this cell."
            (setting :api-base-url)))))

(defun fish-error-text (status body)
  "Fish Audio's own words for a failed STATUS, the code it names, and the fix
the status itself implies."
  (let* ((text (nlk:trimmed (body-text body)))
         (payload (ignore-errors (nlk:decode-json text)))
         (message (or (nlk:json-value payload :string "message")
                      (nlk:json-value payload :string "detail")
                      (if (plusp (length text))
                          (subseq text 0 (min 200 (length text)))
                          "no body")))
         (code (nlk:json-value payload :string "code")))
    (format nil "Fish Audio returned HTTP ~d: ~a~@[ (code ~a)~]~a"
            status message code
            (or (cdr (assoc status '((400 . " -- Fish Audio rejected the request; check the voice id, model and text")
                                     (401 . " -- the key was refused; check it at https://fish.audio/app/api-keys")
                                     (402 . " -- the API credits are used up; top up at https://fish.audio/app/developers/billing (app plan credits are separate)")
                                     (403 . " -- this key may not use that voice or model")
                                     (404 . " -- Fish Audio has no such voice on this account")
                                     (413 . " -- the request is too large; use less text or a smaller file")
                                     (415 . " -- Fish Audio does not read that audio format")
                                     (429 . " -- too many requests at once; try again shortly"))))
                (when (and (integerp status) (>= status 500))
                  " -- Fish Audio is unavailable; try again later")))))

(defun ensure-ok (status body)
  "Signal this cell's condition with Fish Audio's own sentence unless STATUS
is a success."
  (unless (and (integerp status) (<= 200 status 299))
    (fail "~a" (fish-error-text status body))))

(defun json-answer (method path &key key model params content content-type timeout)
  "(values PAYLOAD STATUS BODY): a JSON exchange decoded; PAYLOAD is NIL when
the body is not JSON, and BODY is what the caller quotes back."
  (multiple-value-bind (body status) (request method path :key key :model model
                                                       :params params :content content
                                                       :content-type content-type
                                                       :timeout (or timeout 60))
    (values (ignore-errors (nlk:decode-json (body-text body))) status (body-text body))))

;;; --- what came back, and where it goes -------------------------------------------

(defparameter +magic+
  '(("mp3" . #(255)) ("wav" . #(82 73 70 70)) ("ogg" . #(79 103 103 83)))
  "The first bytes each container starts with: an MP3 frame, RIFF, OggS.")

(defun audio-p (octets extension)
  "Whether OCTETS begin like the container EXTENSION names. Fish Audio's
content type is not trusted: a 200 carrying a JSON error is what this catches."
  (let* ((magic (cdr (assoc extension +magic+ :test #'string=)))
         (n (length octets)))
    (and magic (>= n (length magic))
         (loop for i below (length magic) always (= (aref octets i) (aref magic i)))
         (if (string= extension "ogg")
             (and (search (map '(vector (unsigned-byte 8)) #'char-code "OpusHead") octets
                          :end2 (min n 512)) t)
             t))))

(defun slug (text)
  "TEXT as a file name's stem: lowercase letters, digits and single dashes, at
most 40 of them, never empty."
  (let ((chars '()) (dash t) (n 0))
    (loop for char across (string-downcase (or text ""))
          while (< n 40)
          do (cond ((and (<= 32 (char-code char) 126) (alphanumericp char))
                    (push char chars) (setf dash nil) (incf n))
                   ((not dash) (push #\- chars) (setf dash t) (incf n))))
    (let ((out (string-right-trim "-" (coerce (nreverse chars) 'string))))
      (if (zerop (length out)) "speech" out))))

(defun datestamp ()
  "The local time as 20261108_101500, the way a file name carries it."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

(defun file-id ()
  "A short random stem, so two files made in the same second never collide."
  (format nil "~8,'0x" (random (expt 16 8))))

(defun save-bytes (octets extension label)
  "Write OCTETS under the section's directory, one file that never overwrites
another, and answer its path."
  (let* ((name (format nil "fish_~a_~a_~a.~a" (slug label) (datestamp) (file-id) extension))
         (path (merge-pathnames name (setting :directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :error
                               :if-does-not-exist :create
                               :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    path))

(defun checked-input (text &key (max-bytes +max-upload-bytes+))
  "(values PATH KIND BYTES): a local audio file the operator named, checked
before a billed request names it. A path is not a place to guess: an absent
file, an unreadable one, a kind Fish Audio does not take and one over the cap
each refuse here."
  (let* ((name (trimmed-text text))
         (path (and name (plusp (length name)) (probe-file (pathname name)))))
    (unless path
      (fail "file_path is ~s, and no readable file is there. Pass an absolute path, or one relative to the folder Nodecode runs in."
            (or text "empty")))
    (let* ((kind (string-downcase (or (pathname-type path) "")))
           (mime (cdr (assoc kind +audio-kinds+ :test #'string=)))
           (bytes (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
      (unless mime
        (fail "~a is a .~a file; Fish Audio transcribes ~{~a~^, ~}"
              (file-namestring path) kind (mapcar #'car +audio-kinds+)))
      (when (> bytes max-bytes)
        (fail "~a is ~a, over the ~a Fish Audio takes for one upload"
              (file-namestring path) (nlk:size-text bytes) (nlk:size-text max-bytes)))
      (values path kind mime bytes))))
