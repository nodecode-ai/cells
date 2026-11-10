;;;; wire.lisp --- the one network seam: Twilio's REST API and the TwiML document.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything this cell sends leaves through TWILIO-REQUEST, so a test stubs
;;;; the one name and never touches the network. The original used the twilio
;;;; python client and its bridge; this rewrite speaks the same Calls resource
;;;; over HTTP and hands Twilio a TwiML document, which is all a scripted call
;;;; needs: the disclosure, the message, an optional recording, goodbye.

(in-package #:nodecode-phone-call)

(defparameter +api-root+ "https://api.twilio.com/2010-04-01/Accounts/"
  "Twilio's REST root; the account SID is the next path segment.")

(defun account-sid () (nonempty (ignore-errors (setting :account-sid))))
(defun auth-token () (nonempty (ignore-errors (setting :auth-token))))
(defun from-number () (nonempty (ignore-errors (setting :from-number))))

(defun twilio-configured-p ()
  "Are all three things a real call needs named in the section?"
  (and (account-sid) (auth-token) (from-number)))

(defun timeout-seconds ()
  "One Twilio request's ceiling."
  (or (ignore-errors (setting :timeout-seconds)) 20))

;;; --- encoding -----------------------------------------------------------------

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "RFC 4648's alphabet: what a Basic authorization header is built from.")

(defun base64 (text)
  "TEXT (ASCII) as base64, padded as the header wants it."
  (let* ((octets (sb-ext:string-to-octets text :external-format :ascii))
         (out (make-string-output-stream)))
    (loop for i from 0 below (length octets) by 3
          do (let* ((b0 (aref octets i))
                    (b1 (if (> (+ i 1) (1- (length octets))) 0 (aref octets (+ i 1))))
                    (b2 (if (> (+ i 2) (1- (length octets))) 0 (aref octets (+ i 2))))
                    (n (+ (ash b0 16) (ash b1 8) b2)))
               (write-char (char +base64-alphabet+ (ldb (byte 6 18) n)) out)
               (write-char (char +base64-alphabet+ (ldb (byte 6 12) n)) out)
               (write-char (if (> (+ i 2) (1- (length octets)))
                               #\=
                               (char +base64-alphabet+ (ldb (byte 6 6) n)))
                           out)
               (write-char (if (> (+ i 1) (1- (length octets)))
                               #\=
                               (char +base64-alphabet+ (ldb (byte 6 0) n)))
                           out)))
    (get-output-stream-string out)))

(defun url-encode (text)
  "TEXT percent-encoded as a form value."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (or text "") :external-format :utf-8)
          do (let ((ch (code-char octet)))
               (if (or (alphanumericp ch) (member ch '(#\- #\_ #\. #\~)))
                   (write-char ch out)
                   (format out "%~2,'0x" octet))))))

(defun form-encode (pairs)
  "PAIRS — (name . value) — as an application/x-www-form-urlencoded body."
  (format nil "~{~a~^&~}"
          (mapcar (lambda (pair)
                    (format nil "~a=~a" (url-encode (car pair)) (url-encode (cdr pair))))
                  pairs)))

(defun decode-body (answer)
  "ANSWER — an HTTP body — decoded; an empty body reads as an empty object."
  (cond ((hash-table-p answer) answer)
        ((and (stringp answer)
              (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer))))
         (handler-case (nlk:decode-json answer) (error () (nlk:json-object))))
        (t (nlk:json-object))))

(defun body-text (answer)
  "What a refused Twilio answer says, in one secret-free line."
  (let ((body (decode-body answer)))
    (or (nlk:json-value body :text "message")
        (nlk:json-value body :text "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

;;; --- the calls resource ---------------------------------------------------------

(defun twilio-request (method path &optional params)
  "METHOD PATH — relative to the account — with PARAMS as a form body.
=> (values body status). A transport failure signals PHONE-CALL-ERROR."
  (let* ((sid (account-sid))
         (url (concatenate 'string +api-root+ (or sid "") "/" path))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (list (cons "authorization"
                                         (format nil "Basic ~a"
                                                 (base64 (format nil "~a:~a"
                                                         (or sid "") (or (auth-token) "")))))
                                   (cons "content-type" "application/x-www-form-urlencoded"))
                    :content (if params (form-encode params) "")
                    :timeout (timeout-seconds)
                    :connect-timeout (min 10 (timeout-seconds))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "phone-call: could not reach Twilio: ~a"
              (nle:transport-failure-label condition url))))
    (values (decode-body answer) status)))

(defun create-call (call twiml)
  "POST the call to Twilio => (values SID error). The TwiML rides the request,
so Twilio needs no public URL of ours."
  (multiple-value-bind (body status)
      (twilio-request :post "Calls.json"
                      (list (cons "To" (getf call :number))
                            (cons "From" (from-number))
                            (cons "Twiml" twiml)
                            (cons "TimeLimit"
                                  (princ-to-string (+ 15 (getf call :max-duration-s))))))
    (if (and (integerp status) (< status 300))
        (let ((sid (nlk:json-value body :text "sid")))
          (if (nonempty sid)
              (values sid nil)
              (values nil "Twilio answered no call SID")))
        (values nil (format nil "Twilio answered HTTP ~a~@[: ~a~]" status (body-text body))))))

(defun fetch-call (provider-sid)
  "GET one call resource => (values PLIST error)."
  (multiple-value-bind (body status)
      (twilio-request :get (format nil "Calls/~a.json" provider-sid))
    (if (and (integerp status) (< status 300))
        (values (list :status (nlk:json-value body :text "status")
                      :duration (nlk:json-value body :text "duration")
                      :price (nlk:json-value body :text "price"))
                nil)
        (values nil (format nil "Twilio answered HTTP ~a~@[: ~a~]" status (body-text body))))))

(defun fetch-recording (provider-sid)
  "The first recording of a finished call, as a URL, or NIL."
  (handler-case
      (multiple-value-bind (body status)
          (twilio-request :get (format nil "Calls/~a/Recordings.json" provider-sid))
        (when (and (integerp status) (< status 300))
          (let* ((recordings (nlk:json-value body :array "recordings"))
                 (first (and recordings (plusp (length recordings)) (aref recordings 0)))
                 (url (and first (nlk:json-value first :text "uri"))))
            (when (nonempty url)
              (concatenate 'string "https://api.twilio.com"
                           (subseq url 0 (min (length url) (or (search ".json" url) (length url))))
                           ".mp3")))))
    (error () nil)))

;;; --- the document Twilio reads --------------------------------------------------

(defun xml-escape (text)
  "TEXT safe inside an XML text node or an attribute."
  (with-output-to-string (out)
    (loop for ch across (or text "")
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&apos;" out))
               (t (write-char ch out))))))

(defun say (text)
  "One SAY verb in the section's voice and language."
  (format nil "<Say voice=\"~a\" language=\"~a\">~a</Say>"
          (xml-escape (or (nonempty (ignore-errors (setting :voice))) "Polly.Joanna"))
          (xml-escape (or (nonempty (ignore-errors (setting :language))) "en"))
          (xml-escape text)))

(defun twiml-for (call)
  "The document a call speaks: the disclosure, the message, an optional
recording of the answer, and goodbye."
  (with-output-to-string (out)
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response>" out)
    (write-string (say (opening-line (getf call :record))) out)
    (write-string "<Pause length=\"1\"/>" out)
    (write-string (say (call-message call)) out)
    (let ((callback (nonempty (ignore-errors (setting :callback-number)))))
      (when callback
        (write-string (say (format nil "You can reach us back on ~a." callback)) out)))
    (when (getf call :record)
      (format out "<Record maxLength=\"~d\" playBeep=\"false\" trim=\"trim-silence\"/>"
              (getf call :max-duration-s)))
    (write-string (say +goodbye+) out)
    (write-string "<Hangup/></Response>" out)))
