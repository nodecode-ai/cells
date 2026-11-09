;;;; wire.lisp --- the signature, the one request, the answer as lines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original had no code to copy: its skills documented the REST API
;;;; and left the calling to whatever agent read them. This file is that
;;;; documentation made executable -- HMAC-SHA256 over METHOD + PATH + BODY +
;;;; TIMESTAMP, the three headers, the same host -- and nothing else. The
;;;; SHA-256 is the image's own (ironclad, the one crypto system the serving
;;;; image names); HMAC around it is twelve lines here rather than another
;;;; dependency.
;;;;
;;;; Nothing here touches a file: the only way out is HTTP, the one seam a
;;;; test stubs.

(in-package #:nodecode-sigrex)

;;; --- HMAC-SHA256, lowercase hex -----------------------------------------------

(defparameter +block-length+ 64
  "SHA-256's block size in octets, HMAC's inner and outer padding.")

(defparameter +hex-digits+ "0123456789abcdef"
  "The lowercase hexadecimal the signature is written in.")

(defun octets (text)
  "TEXT as its UTF-8 octets."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun sha256 (sequence)
  "The thirty-two octets of SHA-256 over SEQUENCE."
  (ironclad:digest-sequence :sha256 sequence))

(defun padded (block byte)
  "+BLOCK-LENGTH+ octets, every one BYTE."
  (make-array +block-length+ :element-type '(unsigned-byte 8) :initial-element byte))

(defun xored (left right)
  "The octets of LEFT xor RIGHT."
  (let ((out (make-array (length left) :element-type '(unsigned-byte 8))))
    (loop for index below (length left)
          do (setf (aref out index) (logxor (aref left index) (aref right index))))
    out))

(defun hmac-sha256 (key text)
  "The thirty-two octets of HMAC-SHA256 over TEXT, keyed by KEY; both strings."
  (let* ((key (octets key))
         (key (if (> (length key) +block-length+) (sha256 key) key))
         (inner (padded +block-length+ 0))
         (outer (padded +block-length+ 0)))
    (replace inner key)
    (replace outer key)
    (let* ((message (octets text))
           (first-pass (make-array (+ +block-length+ (length message))
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0))
           (tail (padded +block-length+ 0)))
      (replace first-pass (xored inner (padded +block-length+ #x36)))
      (replace first-pass message :start1 +block-length+)
      (replace tail (xored outer (padded +block-length+ #x5c)))
      (sha256 (concatenate '(vector (unsigned-byte 8)) tail (sha256 first-pass))))))

(defun hex (octets)
  "OCTETS as lowercase hexadecimal, as the original's client wrote the signature."
  (with-output-to-string (out)
    ;; ~X writes upper case in Common Lisp; the API takes lower case.
    (loop for byte across octets
          do (format out "~(~2,'0X~)" byte))))

(defun unix-time ()
  "The UNIX time in seconds, the timestamp a request signs with."
  (- (get-universal-time) 2208988800))

(defun signature (secret method path body timestamp)
  "The signature over one request, as the skills wrote the algorithm: the
uppercase method, the full path with its query string, the exact body text
for POST and PUT (empty otherwise), and the timestamp, in that order."
  (hex (hmac-sha256 secret (concatenate 'string method path body timestamp))))

;;; --- what a request carries ------------------------------------------------

(defparameter +methods+ '("GET" "POST" "PUT" "DELETE")
  "The verbs the REST API documents; anything else is the caller's mistake.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun method-of (value)
  "VALUE as one of +METHODS+, or a refusal naming them."
  (let ((method (string-upcase (or (nonblank value) "GET"))))
    (or (find method +methods+ :test #'string=)
        (fail "method is ~s: the Sigrex API takes ~{~a~^, ~}" value +methods+))))

(defun api-path (path)
  "PATH made absolute under /api/v2, which is part of the signed path."
  (let ((path (or (nonblank path)
                  (fail "sigrex_api needs a path, e.g. /api/v2/strategy/code"))))
    (unless (char= (char path 0) #\/)
      (setf path (concatenate 'string "/" path)))
    (if (and (>= (length path) 7) (string= "/api/v2" path :end2 7))
        path
        (concatenate 'string "/api/v2" path))))

(defun base-url ()
  "The API host the section names, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

(defun hook-base-url ()
  "The hook host the section names, without a trailing slash."
  (string-right-trim "/" (setting :hook-url)))

;;; --- the one seam ------------------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS), every status a value."
  (nlk:http method url
            :headers (append (list (cons "accept" "application/json")) headers)
            :content content
            :timeout (or timeout (setting :timeout-seconds))))

(defun signed-headers (method path body timestamp)
  "The three headers the API authenticates with, and the body's own when
one rides it."
  (let ((headers (list (cons "api-key" (setting :api-key))
                       (cons "timestamp" timestamp)
                       (cons "signature" (signature (setting :api-secret)
                                                     method path (or body "") timestamp)))))
    (if body (append headers (list (cons "content-type" "application/json"))) headers)))

(defun request (method path &key body (base (base-url)))
  "One request to the Sigrex API, signed: METHOD and PATH as the API spells
them, BODY a JSON object or NIL. => (values BODY STATUS), every status a
value, and the body text the peer sent."
  (let* ((method (method-of method))
         (path (api-path path))
         (payload (and body (nlk:encode-json-object body)))
         (timestamp (princ-to-string (unix-time))))
    (http (intern method :keyword)
          (concatenate 'string base path)
          :headers (signed-headers method path payload timestamp)
          :content payload)))

;;; --- the answer as lines --------------------------------------------------------

(defparameter +value-chars+ 400
  "How much of one string value is shown before it is cut; code and prompts
are long.")

(defparameter +answer-chars+ 6000
  "How much of one answer the model reads, the whole of a long list being
more than a context wants.")

(defun cut (text limit)
  "TEXT at most LIMIT characters, with a note when it was cut."
  (if (<= (length text) limit)
      text
      (format nil "~a [~d more characters]" (subseq text 0 limit) (- (length text) limit))))

(defun single-line (value)
  "VALUE as one trimmed line, its newlines folded to spaces."
  (let ((text (nlk:trimmed (princ-to-string (or value "")))))
    (map 'string (lambda (character)
                   (if (member character '(#\Newline #\Return #\Tab)) #\Space character))
         text)))

(defun keys-of (table)
  "TABLE's keys, sorted so two runs of the same answer read the same."
  (sort (loop for key being the hash-keys of table collect key) #'string<))

(defun rendered (value indent)
  "VALUE as indented lines: an object one member a line, an array one item a
line, a string as itself."
  (cond
    ((hash-table-p value)
     (if (zerop (hash-table-count value))
         "{}"
         (with-output-to-string (out)
           (dolist (key (keys-of value))
             (format out "~%~a~a: ~a" (make-string indent :initial-element #\Space) key
                     (rendered (gethash key value) (+ indent 2)))))))
    ((and (vectorp value) (not (stringp value)))
     (if (zerop (length value))
         "[]"
         (with-output-to-string (out)
           (loop for item across value
                 for index from 1
                 do (format out "~%~a~d. ~a" (make-string indent :initial-element #\Space)
                            index (rendered item (+ indent 3)))))))
    ((null value) "null")
    ((stringp value) (cut (single-line value) +value-chars+))
    (t (princ-to-string value))))

(defun message-of (payload)
  "PAYLOAD's own sentence about what went wrong: the API's \"message\" on an
authentication failure, \"error\" on a route error."
  (when (hash-table-p payload)
    (or (nlk:json-value payload :string "message")
        (let ((error (nlk:json-value payload :string "error")))
          (and error (single-line error)))
        (let ((error (nlk:json-value payload :object "error")))
          (and error (nlk:json-value error :string "message"))))))

(defun payload-of (body)
  "BODY decoded as the API's JSON, or NIL when it is not JSON at all."
  (let ((payload (ignore-errors (nlk:decode-json (or body "")))))
    (if (or (hash-table-p payload) (and (vectorp payload) (not (stringp payload))))
        payload
        nil)))

(defun answer (label body status)
  "What one request answers the caller: the status, then the body as lines."
  (let* ((payload (payload-of body))
         (text (if payload (rendered payload 0) (single-line (or body ""))))
         (text (cut text +answer-chars+)))
    (if (>= status 400)
        (fail "sigrex: ~a answered HTTP ~d~@[ - ~a~]~@[~%~a~]"
              label status (and payload (message-of payload))
              (and (plusp (length text)) text))
        (format nil "sigrex: ~a -> HTTP ~d~@[~%~a~]"
                label status (and (plusp (length text)) text)))))
