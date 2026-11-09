;;;; client.lisp --- the HTTP side: Tripo's v3 envelope, uploads, polling and downloads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's client.py, in Lisp: one call checks the envelope
;;;; ({code, message, suggestion, data}) and answers DATA; a local file
;;;; becomes a file_token through POST /files; a finished task's signed URLs
;;;; are streamed to disk with the API key left off the request.

(in-package #:nodecode-tripo)

;;; --- names -------------------------------------------------------------------

(defparameter +root+ "https://openapi.tripo3d.ai/v3"
  "Tripo's v3 API, where the cell goes unless base_url names another.")

(defparameter +user-agent+ "nodecode-tripo/0.1"
  "What every request says it is; the original sent hermes-tripo/0.4.0.")

(defparameter +terminal+ '("success" "failed" "cancelled" "banned" "expired")
  "A task in one of these states will not change again (client.py:19).")

(defparameter +max-upload-bytes+ (* 50 1024 1024)
  "The largest local file the cell uploads; past it, a public URL is the way.")

(defparameter +image-types+ '("png" "jpg" "jpeg" "webp")
  "What Tripo takes as an image input.")

(defparameter +model-types+ '("glb" "gltf" "fbx" "obj" "stl")
  "What Tripo takes as a model input.")

;;; --- the section, read back ------------------------------------------------------

(defun api-key ()
  "The key the section names, inline or by environment variable, else TRIPO_API_KEY."
  (or (let ((inline (setting :api-key))) (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "TRIPO_API_KEY")))

(defun local-http-p (url)
  "Whether URL is plain http to this machine, the way a local test server is (client.py:91)."
  (and (uiop:string-prefix-p "http://" url)
       (some (lambda (host) (uiop:string-prefix-p (concatenate 'string "http://" host) url))
             '("127.0.0.1" "localhost" "[::1]"))))

(defun root ()
  "The API root this cell talks to, without a trailing slash; https, or plain http
on this machine, as the original's base_url check insists (client.py:96)."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (let ((root (if (plusp (length given)) given +root+)))
      (unless (or (uiop:string-prefix-p "https://" root) (local-http-p root))
        (fail "base_url must be an https URL (or http://127.0.0.1 for a local test server), got ~a" root))
      root)))

(defun timeout ()
  (setting :timeout-seconds))

(defun endpoint (path)
  (concatenate 'string (root) path))

(defun auth-headers ()
  "The key as a bearer token: the one place it leaves the process (client.py:128)."
  (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
        (cons "user-agent" +user-agent+)))

;;; --- the envelope ---------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun hint (code status)
  "What a Tripo error code means for the operator, as the original's _hint does (client.py:104)."
  (cond ((or (member code '(1000 1001)) (eql status 401))
         "; the key looks wrong: check \"api_key\" (or TRIPO_API_KEY) in the tripo section")
        ((eql code 2010) "; the account is out of credits, top up at platform.tripo3d.ai")
        ((or (member code '(2000 1007)) (eql status 429))
         "; Tripo is rate limiting or its concurrency cap is full: wait for running tasks")
        ((eql code 2008) "; Tripo's content policy refused the input: change the prompt or image")
        (t "")))

(defun call (method path &key body)
  "METHOD PATH on the configured root, BODY its JSON object => the envelope's DATA.
A refused status, a non-zero code or a transport failure signals TRIPO-ERROR."
  (let ((payload nil)
        (status nil))
    (handler-case
        (multiple-value-setq (payload status)
          (nlk:http method (endpoint path)
                    :headers (cons '("content-type" . "application/json") (auth-headers))
                    :content (and body (nlk:encode-json-object body))
                    :timeout (timeout)
                    :connect-timeout (min 10 (timeout))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string method))
              (nle:transport-failure-label condition (endpoint path)))))
    (let* ((answer (decode-body payload))
           (code (nlk:json-value answer :integer "code")))
      (cond ((and (integerp status) (< status 300) (eql code 0))
             (nlk:json-value answer :any "data"))
            (t
             (fail "~a ~a: HTTP ~a~@[ - ~a~]~@[ (~a)~]~a"
                   (string-upcase (string method)) path status
                   (nlk:json-value answer :string "message")
                   (nlk:json-value answer :string "suggestion")
                   (hint code status)))))))

(defun get-task (task-id)
  "One task's record."
  (let ((data (call :get (format nil "/tasks/~a" task-id))))
    (when (hash-table-p data) data)))

(defun wait-for-task (task-id seconds)
  "TASK-ID's record, polled until it is terminal or SECONDS have passed (client.py:249)."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second)))
        (delay 2))
    (loop
      (let ((data (get-task task-id)))
        (cond ((member (nlk:json-value data :string "status") +terminal+ :test #'equal) (return data))
              ((<= deadline (get-internal-real-time)) (return data))
              (t (sleep delay)
                 (setf delay (min (* delay 1.5) 10))))))))

(defun balance ()
  (let ((data (call :get "/account/balance")))
    (if (hash-table-p data) data (nlk:json-object))))

(defun usage ()
  (let ((data (call :get "/account/usage")))
    (if (and (vectorp data) (not (stringp data))) data #())))

;;; --- uploads --------------------------------------------------------------------

(defun file-extension (path)
  (string-downcase (or (pathname-type path) "")))

(defun upload (path kind)
  "POST PATH as a multipart file and answer its file_token (client.py:204).
KIND is \"image\" or \"model\": what the endpoint takes."
  (let* ((path (pathname path))
         (allowed (if (equal kind "image") +image-types+ +model-types+)))
    (unless (probe-file path)
      (fail "no such file: ~a" (namestring path)))
    (unless (member (file-extension path) allowed :test #'equal)
      (fail "~a: expected one of ~{~a~^, ~}" (file-namestring path) allowed))
    (let ((size (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
      (when (zerop size) (fail "~a is empty" (file-namestring path)))
      (when (> size +max-upload-bytes+)
        (fail "~a is ~d MB; upload it somewhere public and pass the URL"
              (file-namestring path) (floor size (* 1024 1024)))))
    (let ((answer nil)
          (status nil))
      (handler-case
          (multiple-value-setq (answer status)
            (nlk:http :post (endpoint "/files")
                      :headers (auth-headers)
                      :content (list (cons "file" path))
                      :timeout 300
                      :connect-timeout 15))
        ((or error sb-sys:deadline-timeout) (condition)
          (fail "upload of ~a: ~a" (file-namestring path)
                (nle:transport-failure-label condition (endpoint "/files")))))
      (let* ((body (decode-body answer))
             (token (nlk:json-value body :string "data" "file_token")))
        (or token
            (fail "upload of ~a: HTTP ~a~@[ - ~a~]" (file-namestring path) status
                  (nlk:json-value body :string "message")))))))

(defun upload-marker (path kind)
  "A local file a request body would upload, replaced by its token before the create."
  (cons :upload (list path kind)))

(defun resolve-uploads (value)
  "VALUE with every upload marker replaced by the file_token it uploaded to."
  (cond ((and (consp value) (eq :upload (car value)))
         (upload (second value) (third value)))
        ((hash-table-p value)
         (loop for key being the hash-keys of value using (hash-value inner)
               do (setf (gethash key value) (resolve-uploads inner)))
         value)
        ((and (vectorp value) (not (stringp value)))
         (loop for index below (length value)
               do (setf (aref value index) (resolve-uploads (aref value index))))
         value)
        (t value)))

(defun read-octets (stream)
  "Everything STREAM still says, as one octet vector."
  (let ((into (make-array 65536 :element-type '(unsigned-byte 8)))
        (all (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for count = (read-sequence into stream)
          while (plusp count)
          do (loop for index below count
                   do (vector-push-extend (aref into index) all)))
    (coerce all '(simple-array (unsigned-byte 8) (*)))))

;;; --- downloads --------------------------------------------------------------------


(defun fetch (url target)
  "Stream a signed output URL to TARGET; answer the byte count (client.py:268)."
  (unless (or (uiop:string-prefix-p "https://" url) (local-http-p url))
    (fail "refusing a non-https download URL: ~a" (subseq url 0 (min 80 (length url)))))
  (let ((body nil)
        (status nil))
    (handler-case
        (multiple-value-setq (body status)
          (nlk:http :get url :headers (list (cons "user-agent" +user-agent+))
                    :binary t :timeout 300 :connect-timeout 15))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "download failed: ~a" (nle:transport-failure-label condition url))))
    (unless (and (integerp status) (< status 300))
      (fail "download failed with HTTP ~a (output URLs expire: fetch the task again for fresh ones)"
            status))
    (ensure-directories-exist target)
    (with-open-file (out target :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
      (write-sequence (cond ((streamp body) (read-octets body))
                            ((stringp body) (babel:string-to-octets body))
                            (t body))
                      out))
    (with-open-file (in target :element-type '(unsigned-byte 8)) (file-length in))))
