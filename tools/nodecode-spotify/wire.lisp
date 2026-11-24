;;;; wire.lisp --- what every Spotify call needs: a path, a query, JSON on disk,
;;;; and the small encodings (percent, base64url, SHA-256) OAuth's PKCE asks for.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-spotify)

;;; --- time --------------------------------------------------------------------

(defvar *now* nil
  "Unix seconds, or NIL to read the clock. A test binds it so an expiry is a fact.")

(defun now-seconds () (or *now* (nlk:unix-now)))

;;; --- text --------------------------------------------------------------------

(defun trimmed (text)
  "TEXT without its surrounding blanks, or NIL."
  (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun nonblank (text what)
  "TEXT trimmed, or a refusal naming WHAT."
  (let ((text (trimmed text)))
    (if (and text (plusp (length text))) text (fail "~a is required" what))))

(defun url-encode (text)
  "TEXT's UTF-8 octets, every byte outside RFC 3986's unreserved set escaped."
  (with-output-to-string (out)
    (loop for byte across (nlk:utf-8-octets (princ-to-string text))
          do (if (or (<= 65 byte 90) (<= 97 byte 122) (<= 48 byte 57)
                     (member byte '(45 46 95 126)))
                 (write-char (code-char byte) out)
                 (format out "%~2,'0X" byte)))))

(defun query-string (params)
  "PARAMS, an alist of (NAME . VALUE), as a query string; a NIL or empty
VALUE is left out, which is how Spotify reads the default."
  (let ((pairs (loop for (name . value) in params
                     when (and value (not (equal value "")))
                       collect (format nil "~a=~a" (url-encode (string-downcase (string name)))
                                       (url-encode value)))))
    (when pairs (format nil "~{~a~^&~}" pairs))))

(defun with-query (url params)
  "URL with PARAMS appended when any of them is set."
  (let ((query (query-string params)))
    (if query (format nil "~a?~a" url query) url)))

;;; --- base64url and SHA-256, for PKCE --------------------------------------------

(defparameter +base64url-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "RFC 4648's URL-safe alphabet: base64 with - and _, and no padding.")

(defun base64url (octets)
  "OCTETS as unpadded base64url -- what a PKCE verifier and challenge are."
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 3
          for left = (- (length octets) i)
          for triple = (+ (ash (aref octets i) 16)
                          (ash (if (> left 1) (aref octets (1+ i)) 0) 8)
                          (if (> left 2) (aref octets (+ i 2)) 0))
          do (write-char (char +base64url-alphabet+ (ldb (byte 6 18) triple)) out)
             (write-char (char +base64url-alphabet+ (ldb (byte 6 12) triple)) out)
             (when (> left 1) (write-char (char +base64url-alphabet+ (ldb (byte 6 6) triple)) out))
             (when (> left 2) (write-char (char +base64url-alphabet+ (ldb (byte 6 0) triple)) out)))))

(defun sha256-octets (text)
  "TEXT's SHA-256 digest, 32 octets."
  (let* ((hex (subseq (nlk:sha256-text text) 7))
         (octets (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length octets)
          do (setf (aref octets i)
                   (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))
    octets))

(defun pkce-verifier ()
  "A fresh PKCE code verifier: 32 random octets, 43 characters of base64url."
  (base64url (nlk:random-bytes 32)))

(defun pkce-challenge (verifier)
  "A verifier's S256 challenge."
  (base64url (sha256-octets verifier)))

;;; --- JSON on disk -------------------------------------------------------------

(defun json-object-of (&rest pairs)
  "A JSON object from PAIRS, leaving out a member whose value is NIL."
  (apply #'nlk:make-json-object
         (loop for (name value) on pairs by #'cddr when value append (list name value))))

(defun read-json-file (path)
  "PATH decoded, or NIL when it is absent or is not JSON."
  (when (and path (probe-file path))
    (handler-case (nlk:decode-json (uiop:read-file-string path))
      (error () nil))))

(defun write-json-file (path object)
  "OBJECT as JSON at PATH, owner-readable only, replaced whole."
  (ensure-directories-exist path)
  (let ((text (nlk:encode-json-object object)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string text out))
    #+sbcl (ignore-errors (sb-posix:chmod (namestring path) #o600))
    path))

;;; --- what Spotify says when it says no ------------------------------------------

(defun error-detail (body)
  "BODY, Spotify's error document, as one line: its error message or the raw text."
  (let* ((decoded (and (stringp body)
                        (handler-case (nlk:decode-json body) (error () nil))))
         (message (and decoded (or (nlk:json-value decoded :string "error" "message")
                                   (nlk:json-value decoded :string "error")
                                   (nlk:json-value decoded :string "error_description")))))
    (or (trimmed message)
        (let ((text (trimmed body))) (if (and text (plusp (length text))) text nil))
        "no detail")))

(defun friendly-error (status detail path)
  "Spotify's STATUS and DETAIL in words a model can act on, PATH the endpoint."
  (let ((playback (and path (<= 10 (length path)) (string= path "/me/player" :end2 10))))
    (cond ((= status 401) "Spotify authentication failed or expired: run /spotify login again.")
          ((= status 403) (if playback
                               (concatenate 'string
                                            "Spotify rejected this playback request: playback control usually "
                                            "needs a Premium account and an active Spotify Connect device.")
                               (concatenate 'string
                                            "Spotify rejected the request: the signed-in account may lack "
                                            "the scope for it, or permission for this item.")))
          ((= status 404) (if playback
                               "Spotify found no active playback device for this request."
                               "Spotify could not find that resource."))
          ((= status 429) "Spotify rate limit exceeded: wait a moment and try again.")
          (t detail))))
