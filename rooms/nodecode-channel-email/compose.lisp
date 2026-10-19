;;;; compose.lisp --- one reply, written as RFC 5322 text.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure. The message is 7-bit clean -- headers RFC 2047 encoded, the body and
;;;; each file base64 -- so no relay ever rewrites it, and its lines end CRLF.
;;;; An answer with HTML carries both forms as multipart/alternative: the
;;;; plain text first, the HTML last, as RFC 2046 orders the richer one.

(in-package #:nodecode-channel-email)

(defvar *mail-random* (make-random-state t)
  "Seeded at load: SBCL's default state repeats across boots, and a
Message-ID must not.")

(defparameter +crlf+ (coerce '(#\Return #\Newline) 'string))

(defun new-message-id (domain)
  "A fresh <...@DOMAIN> Message-ID."
  (format nil "<nc.~(~36r~).~(~36r~)@~a>" (get-universal-time) (random (expt 2 64) *mail-random*) domain))

(defun rfc5322-date (&optional (time (get-universal-time)))
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time time 0)
    (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d +0000"
            (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            d (nth (1- mo) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            y h m s)))

(defun ascii-p (text)
  (every (lambda (c) (<= 32 (char-code c) 126)) text))

(defun utf8 (text)
  (babel:string-to-octets text :encoding :utf-8))

(defun encode-header (text)
  "TEXT as a header value: itself when printable ASCII, else RFC 2047 words,
each under 76 characters and never splitting a character."
  (if (ascii-p text)
      text
      (let ((words '()) (chunk (make-string-output-stream)) (bytes 0))
        (flet ((flush ()
                 (let ((piece (get-output-stream-string chunk)))
                   (when (plusp (length piece))
                     (push (format nil "=?UTF-8?B?~a?=" (cl-base64:usb8-array-to-base64-string (utf8 piece)))
                           words)))
                 (setf bytes 0)))
          (loop for c across text
                for size = (length (utf8 (string c)))
                do (when (> (+ bytes size) 45) (flush))
                   (write-char c chunk)
                   (incf bytes size))
          (flush))
        (format nil (concatenate 'string "~{~a~^" +crlf+ " ~}") (nreverse words)))))

(defun mailbox (address &optional name)
  "ADDRESS with its display NAME, as a From or To value."
  (cond ((or (null name) (string= name "")) address)
        ((not (ascii-p name)) (format nil "~a <~a>" (encode-header name) address))
        ((find-if (lambda (c) (find c "()<>@,;:\\\".[]")) name)
         (format nil "\"~a\" <~a>" (ppcre:regex-replace-all "([\"\\\\])" name "\\\\\\1") address))
        (t (format nil "~a <~a>" name address))))

(defun reply-subject (subject)
  "SUBJECT as a reply's: one Re: ahead of it, however many it had."
  (let ((bare (trim (ppcre:regex-replace "(?i)^\\s*((re|aw|antw|sv|vs|ref)\\s*(\\[\\d+\\])?\\s*:\\s*)+" (or subject "") ""))))
    (if (string= bare "") "Re: your message" (format nil "Re: ~a" bare))))

(defun base64-lines (octets)
  "OCTETS as base64 in 76-character CRLF lines."
  (let ((flat (cl-base64:usb8-array-to-base64-string (coerce octets '(vector (unsigned-byte 8))))))
    (with-output-to-string (out)
      (loop for start from 0 below (length flat) by 76
            do (write-string flat out :start start :end (min (length flat) (+ start 76)))
               (write-string +crlf+ out)))))

(defun crlf-text (text)
  "TEXT with every line ended CRLF."
  (format nil (concatenate 'string "~{~a~^" +crlf+ "~}") (split-lines text)))

(defun filename-param (filename)
  (if (ascii-p filename)
      (format nil "filename=\"~a\"" (remove #\" filename))
      (format nil "filename*=utf-8''~{~a~}"
              (map 'list (lambda (b) (if (or (<= 48 b 57) (<= 65 b 90) (<= 97 b 122) (find (code-char b) ".-_"))
                                         (string (code-char b))
                                         (format nil "%~2,'0X" b)))
                   (utf8 filename)))))

(defun new-boundary ()
  (format nil "=_nc_~(~36r~)" (random (expt 36 14) *mail-random*)))

(defun compose-message (&key from from-name to to-name subject text html message-id
                             in-reply-to references attachments (date (get-universal-time)))
  "One outgoing message as CRLF text: FROM (and FROM-NAME) to TO (and
TO-NAME), SUBJECT, the plain TEXT and, when given, the HTML document that
says the same (the two as multipart/alternative), threaded under IN-REPLY-TO
and REFERENCES (a list of <...> ids), each of ATTACHMENTS -- (:filename
:media-type :octets) -- a part of its own. Marked Auto-Submitted:
auto-replied, so another responder never answers it back (RFC 3834)."
  (let ((boundary (new-boundary))
        (alternative (new-boundary)))
    (with-output-to-string (out)
      (labels ((line (control &rest args)
                 (apply #'format out control args)
                 (write-string +crlf+ out))
               (text-part (type words)
                 (line "Content-Type: ~a; charset=utf-8" type)
                 (line "Content-Transfer-Encoding: base64")
                 (line "")
                 (write-string (base64-lines (utf8 (crlf-text words))) out))
               (body-part ()
                 (cond ((null html) (text-part "text/plain" (or text "")))
                       (t (line "Content-Type: multipart/alternative; boundary=\"~a\"" alternative)
                          (line "")
                          (line "--~a" alternative)
                          (text-part "text/plain" (or text ""))
                          (line "--~a" alternative)
                          (text-part "text/html" html)
                          (line "--~a--" alternative)))))
        (line "From: ~a" (mailbox from from-name))
        (line "To: ~a" (mailbox to to-name))
        (line "Subject: ~a" (encode-header (or subject "")))
        (line "Date: ~a" (rfc5322-date date))
        (line "Message-ID: ~a" message-id)
        (when in-reply-to (line "In-Reply-To: ~a" in-reply-to))
        (when references
          (line "References: ~a" (format nil (concatenate 'string "~{~a~^" +crlf+ " ~}") references)))
        (line "Auto-Submitted: auto-replied")
        (line "MIME-Version: 1.0")
        (cond ((null attachments) (body-part))
              (t
               (line "Content-Type: multipart/mixed; boundary=\"~a\"" boundary)
               (line "")
               (line "This is a multipart message in MIME format.")
               (line "--~a" boundary)
               (body-part)
               (dolist (file attachments)
                 (line "--~a" boundary)
                 (line "Content-Type: ~a" (or (getf file :media-type) "application/octet-stream"))
                 (line "Content-Disposition: attachment; ~a" (filename-param (or (getf file :filename) "file")))
                 (line "Content-Transfer-Encoding: base64")
                 (line "")
                 (write-string (base64-lines (getf file :octets)) out))
               (line "--~a--" boundary)))))))
