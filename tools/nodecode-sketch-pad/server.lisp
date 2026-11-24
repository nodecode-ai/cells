;;;; server.lisp --- the pad page on loopback, and the save it posts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three routes on 127.0.0.1, every one of them local:
;;;;   GET  /       the page (pad.html)
;;;;   POST /save   {"png_base64": "..."} => the kept drawing, as JSON
;;;;   GET  /last    the newest drawing, as JSON, or 404
;;;; The port is the section's, 17319 unless it says otherwise; a port already
;;;; taken makes the pad fall back to one the system picks, and says so in a
;;;; notice, so two profiles' pads can run at once.

(in-package #:nodecode-sketch-pad)

(defparameter +max-body-bytes+ 4000000
  "The largest request body the save route reads: a 3 MB PNG in base64.")

(defvar *pad-port* nil
  "The port the pad answers on, or NIL while it is not served.")

(defun pad-port ()
  "The port the pad answers on while it serves, or NIL."
  *pad-port*)

(defun pad-url ()
  "Where the operator opens the pad."
  (running-settings)
  (format nil "http://127.0.0.1:~d/" (or *pad-port* (setting :port))))

;;; --- responses ---------------------------------------------------------------

(defun text-response (status text &optional (type "text/plain; charset=utf-8"))
  (list status (list :content-type type :cache-control "no-store") (list text)))

(defun json-response (status object)
  (text-response status (nlk:encode-json-object object)
                 "application/json; charset=utf-8"))

(defun refusal (status text)
  (json-response status (nlk:json-object "ok" :false "error" text)))

(defun request-path (env)
  (let ((uri (or (getf env :request-uri) "")))
    (subseq uri 0 (or (position #\? uri) (length uri)))))

;;; --- the body ---------------------------------------------------------------

(defun read-body-json (env)
  "ENV's body decoded as JSON => the object, or NIL and the refusal."
  (let* ((length (or (getf env :content-length) 0))
         (stream (getf env :raw-body)))
    (cond
      ((> length +max-body-bytes+)
       (values nil 413 "the sketch is over the 3 MB the save route reads"))
      ((or (zerop length) (null stream))
       (values nil 400 "no body: send {\"png_base64\": ...}"))
      (t
       (let ((octets (make-array length :element-type '(unsigned-byte 8))))
         (if (/= length (read-sequence octets stream))
             (values nil 400 "the body ended early")
             (nlk:with-handlers ((error (condition)
                                   (values nil 400 (format nil "invalid JSON: ~a" condition))))
               (values (nlk:decode-json
                        (sb-ext:octets-to-string octets :external-format :utf-8))))))))))

(defun decode-base64 (text)
  "TEXT, a standard base64 string or a data: URI, as octets."
  (let* ((text (nlk:trimmed (or text "")))
         (comma (position #\, text))
         (body (if (and comma (search ";base64" text :end2 comma))
                   (subseq text (1+ comma))
                   text))
         (clean (remove-if (lambda (char)
                              (member char '(#\Space #\Tab #\Return #\Newline)))
                            body)))
    (when (zerop (length clean))
      (fail "png_base64 is empty"))
    ;; A canvas writes standard base64; a URL-safe string is accepted too.
    (nlk:with-handlers ((error ()
                          (nlk:with-handlers ((error () (fail "png_base64 is not valid base64")))
                            (cl-base64:base64-string-to-usb8-array clean :uri t))))
      (cl-base64:base64-string-to-usb8-array clean :uri nil))))

;;; --- the three routes -----------------------------------------------------

(defun sketch-json (info)
  (apply #'nlk:make-json-object
         "ok" t
         (loop for (key value) on info by #'cddr
               append (list (string-downcase (symbol-name key)) value))))

(defun save-route (env)
  (multiple-value-bind (object status text) (read-body-json env)
    (if (null object)
        (refusal status text)
        (nlk:with-handlers ((sketch-pad-error (condition)
                              (refusal 400 (sketch-pad-error-detail condition)))
                            (error (condition)
                              (refusal 500 (princ-to-string condition))))
          (let ((info (save-sketch (decode-base64
                                     (nlk:json-value object :string "png_base64")))))
            ;; The model reads this in the notices block of its next request,
            ;; so it knows a drawing exists without being told.
            (nle:notice (format nil "sketch-pad: a drawing is saved at ~a (~a). Look at it with (look ~s) when the operator asks."
                                (getf info :path) (nlk:size-text (getf info :bytes))
                                (getf info :path))
                        :key "nodecode-sketch-pad")
            (json-response 200 (sketch-json info)))))))

(defun last-route ()
  (nlk:with-handlers ((sketch-pad-error (condition)
                        (refusal 400 (sketch-pad-error-detail condition))))
    (let ((info (last-sketch)))
      (if (null info)
          (json-response 404 (nlk:json-object "ok" :false "error" "no sketch yet"))
          (json-response 200 (sketch-json info))))))

(defun pad-app (env)
  "The clack application: the page, the save, the last drawing."
  (let ((method (getf env :request-method))
        (path (request-path env)))
    (cond
      ((and (eq method :get) (member path '("/" "/index.html") :test #'string=))
       (text-response 200 (page-html) "text/html; charset=utf-8"))
      ((and (eq method :get) (string= path "/last")) (last-route))
      ((and (eq method :post) (string= path "/save")) (save-route env))
      (t (refusal 404 (format nil "no route ~a ~a" method path))))))

;;; --- serving it -------------------------------------------------------------

(defun start-pad ()
  "Serve the pad on the section's port => the port it answers on. A port
someone else holds falls back to one the system picks, and says so."
  (let ((wanted (setting :port))
        (handler nil) (port nil))
    (handler-case
        (multiple-value-setq (handler port) (nle:serve-local #'pad-app :port wanted))
      (usocket:address-in-use-error ()
        (multiple-value-setq (handler port) (nle:serve-local #'pad-app :port 0))
        (nle:notice (format nil "sketch-pad: port ~d is taken; the pad answers on ~a instead."
                            wanted (format nil "http://127.0.0.1:~d/" port))
                    :level :warning :key "nodecode-sketch-pad-port")))
    (setf *pad-port* port)
    (nle:on-stop (lambda ()
                   (setf *pad-port* nil)
                   (nle:stop-clack-handler handler)
                   (nle:notice nil :key "nodecode-sketch-pad-port")))
    port))
