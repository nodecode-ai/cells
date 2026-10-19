;;;; wire.lisp --- IMAP in and SMTP out, over TLS.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The few commands the channel needs and no more: IMAP opens the mailbox
;;;; read-only (EXAMINE) and fetches with BODY.PEEK, so a shared inbox keeps
;;;; every flag a person set, waits in IDLE for the server to say new mail
;;;; came, and files a copy of what the bot sent with APPEND; SMTP
;;;; authenticates and sends one message. TLS is
;;;; required and verified: implicit on 993/465, STARTTLS elsewhere, and a
;;;; server that offers no STARTTLS never sees the password. No error text
;;;; carries a credential.

(in-package #:nodecode-channel-email)

(defstruct (wire (:copier nil) (:constructor %make-wire))
  socket stream host (tag 0))

(defun tls-stream (stream host)
  (cl+ssl:make-ssl-client-stream stream :hostname host :verify :required))

(defun wire-connect (host port &key (tls t) (timeout 30))
  "A connection to HOST:PORT, TLS from the first byte when TLS, every read
bounded by TIMEOUT seconds."
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8) :timeout timeout)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (ignore-errors (usocket:socket-close socket)))))
      (setf (usocket:socket-option socket :receive-timeout) timeout)
      (%make-wire :socket socket :host host
                  :stream (if tls
                              (tls-stream (usocket:socket-stream socket) host)
                              (usocket:socket-stream socket))))))

(defun wire-starttls (wire)
  (setf (wire-stream wire) (tls-stream (usocket:socket-stream (wire-socket wire)) (wire-host wire))))

(defun wire-close (wire)
  (when wire
    (ignore-errors (close (wire-stream wire) :abort t))
    (ignore-errors (usocket:socket-close (wire-socket wire)))))

(defun wire-read-line (wire)
  "One line from WIRE, without its CRLF, one character per octet."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (stream (wire-stream wire)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte)
                    (if (zerop (length out))
                        (error "~a closed the connection" (wire-host wire))
                        (return)))
                   ((= byte 10) (return))
                   (t (vector-push-extend byte out))))
    (when (and (plusp (length out)) (= 13 (aref out (1- (length out)))))
      (decf (fill-pointer out)))
    (octets-latin1 out)))

(defun wire-write (wire text)
  (write-sequence (utf8 text) (wire-stream wire))
  (finish-output (wire-stream wire)))

;;; --- IMAP -----------------------------------------------------------------------

(defun imap-quote (text)
  (format nil "\"~a\"" (ppcre:regex-replace-all "([\"\\\\])" text "\\\\\\1")))

(defun imap-read-response (wire)
  "One response with its literals: its lines and octet vectors, in order."
  (let ((segments '()))
    (loop
      (let* ((line (wire-read-line wire))
             (size (ppcre:register-groups-bind ((#'parse-integer n)) ("\\{(\\d+)\\}$" line) n)))
        (push line segments)
        (unless size (return (nreverse segments)))
        (let* ((octets (make-array size :element-type '(unsigned-byte 8)))
               (got (read-sequence octets (wire-stream wire))))
          (when (< got size)
            (error "~a closed the connection inside a ~d-octet literal" (wire-host wire) size))
          (push octets segments))))))

(defun imap-tag (wire)
  (format nil "N~d" (incf (wire-tag wire))))

(defun tagged-p (line tag)
  (uiop:string-prefix-p (concatenate 'string tag " ") line))

(defun imap-collect (wire tag what)
  "Read through TAG's tagged answer; WHAT names the command in an error
(never the command line, which may carry the password). => (values the
untagged responses, in order, each a list of lines and literals; the tagged
status, OK and its text)."
  (let ((responses '()))
    (loop
      (let* ((segments (imap-read-response wire))
             (line (first segments)))
        (if (tagged-p line tag)
            (let ((status (subseq line (1+ (length tag)))))
              (unless (uiop:string-prefix-p "OK" (string-upcase status))
                (error "IMAP ~a refused: ~a" what status))
              (return (values (nreverse responses) status)))
            (push segments responses))))))

(defun imap-continuation (wire tag what)
  "Read until the server's + go-ahead for TAG's literal or idle; its tagged
answer instead is a refusal."
  (loop
    (let ((line (first (imap-read-response wire))))
      (cond ((uiop:string-prefix-p "+" line) (return line))
            ((tagged-p line tag)
             (error "IMAP ~a refused: ~a" what (subseq line (1+ (length tag)))))))))

(defun imap-command (wire what control &rest arguments)
  "Send one tagged command and read through its tagged answer, as
IMAP-COLLECT."
  (let ((tag (imap-tag wire)))
    (wire-write wire (format nil "~a ~?~a" tag control arguments +crlf+))
    (imap-collect wire tag what)))

(defun imap-capabilities (wire)
  "What the logged-in server can do, as upcased words (IDLE, UIDPLUS, ...)."
  (loop for segments in (imap-command wire "CAPABILITY" "CAPABILITY")
        for line = (trim (first segments))
        when (ppcre:scan "(?i)^\\* CAPABILITY " line)
          append (mapcar #'string-upcase (rest (rest (ppcre:split "\\s+" line))))))

(defun imap-open (host port user password &key (timeout 30))
  "A logged-in IMAP connection to HOST:PORT."
  (let ((wire (wire-connect host port :tls (/= port 143) :timeout timeout)))
    (handler-bind ((error (lambda (condition) (declare (ignore condition)) (wire-close wire))))
      (let ((greeting (wire-read-line wire)))
        (unless (ppcre:scan "(?i)^\\* (OK|PREAUTH)" greeting)
          (error "IMAP ~a greeted: ~a" host greeting)))
      (when (= port 143)
        (imap-command wire "STARTTLS" "STARTTLS")
        (wire-starttls wire))
      (imap-command wire "LOGIN" "LOGIN ~a ~a" (imap-quote user) (imap-quote password))
      wire)))

(defun imap-close (wire)
  (when wire
    (ignore-errors (imap-command wire "LOGOUT" "LOGOUT"))
    (wire-close wire)))

(defun response-number (responses pattern)
  (loop for segments in responses
        for found = (ppcre:register-groups-bind ((#'parse-integer n)) (pattern (first segments)) n)
        when found return found))

(defun imap-examine (wire mailbox)
  "Open MAILBOX read-only: => (values UIDVALIDITY UIDNEXT)."
  (let ((responses (imap-command wire "EXAMINE" "EXAMINE ~a" (imap-quote mailbox))))
    (values (response-number responses "(?i)\\[UIDVALIDITY (\\d+)\\]")
            (response-number responses "(?i)\\[UIDNEXT (\\d+)\\]"))))

(defun imap-search-new (wire after-uid &optional recipient)
  "The UIDs above AFTER-UID, ascending -- those naming RECIPIENT in To or Cc
when it is given."
  (let ((responses (if recipient
                       (imap-command wire "SEARCH" "UID SEARCH UID ~d:* OR TO ~a CC ~a"
                                     (1+ after-uid) (imap-quote recipient) (imap-quote recipient))
                       (imap-command wire "SEARCH" "UID SEARCH UID ~d:*" (1+ after-uid)))))
    (sort (loop for segments in responses
                for line = (first segments)
                when (ppcre:scan "(?i)^\\* SEARCH" line)
                  append (loop for token in (rest (rest (ppcre:split "\\s+" line)))
                               for uid = (parse-integer token :junk-allowed t)
                               when (and uid (> uid after-uid)) collect uid))
          #'<)))

(defun internal-date (text)
  "An IMAP INTERNALDATE, 19-Oct-2026 18:00:00 +0200, as a universal time."
  (ppcre:register-groups-bind ((#'parse-integer d) month (#'parse-integer y h mi s) sign
                               (#'parse-integer zh zm))
      ("(\\d{1,2})-(\\w{3})-(\\d{4}) (\\d\\d):(\\d\\d):(\\d\\d) ([+-])(\\d\\d)(\\d\\d)" text)
    (let ((mo (1+ (or (position month '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                :test #'string-equal)
                      0))))
      (- (encode-universal-time s mi h d mo y 0)
         (* (if (string= sign "-") -1 1) (+ (* 3600 zh) (* 60 zm)))))))

(defun imap-fetch (wire uid)
  "Message UID without touching its flags: => (values OCTETS ARRIVED SIZE),
ARRIVED its INTERNALDATE as a universal time."
  (let ((responses (imap-command wire "FETCH" "UID FETCH ~d (UID RFC822.SIZE INTERNALDATE BODY.PEEK[])" uid)))
    (dolist (segments responses (values nil nil nil))
      (let ((head (first segments)))
        (when (and (ppcre:scan "(?i)^\\* \\d+ FETCH" head)
                   (eql uid (ppcre:register-groups-bind ((#'parse-integer n)) ("(?i)UID (\\d+)" head) n)))
          (return (values (find-if (lambda (s) (typep s '(vector (unsigned-byte 8)))) segments)
                          (ppcre:register-groups-bind (date) ("(?i)INTERNALDATE \"([^\"]+)\"" head)
                            (internal-date date))
                          (ppcre:register-groups-bind ((#'parse-integer n)) ("(?i)RFC822\\.SIZE (\\d+)" head) n))))))))

(defun imap-sizes (wire uids)
  "For each of UIDS, (uid size arrived), read without fetching a body."
  (when uids
    (loop for segments in (imap-command wire "FETCH" "UID FETCH ~{~d~^,~} (UID RFC822.SIZE INTERNALDATE)" uids)
          for head = (first segments)
          for uid = (ppcre:register-groups-bind ((#'parse-integer n)) ("(?i)UID (\\d+)" head) n)
          when uid
            collect (list uid
                          (ppcre:register-groups-bind ((#'parse-integer n)) ("(?i)RFC822\\.SIZE (\\d+)" head) n)
                          (ppcre:register-groups-bind (date) ("(?i)INTERNALDATE \"([^\"]+)\"" head)
                            (internal-date date))))))

(defparameter +idle-check-seconds+ 0.25
  "How often a wait in IDLE looks for the server's word or a stop: local
only, nothing crosses the wire.")

(defun imap-idle (wire seconds stop-p)
  "Wait in IDLE (RFC 2177) on the selected mailbox until the server says a
message arrived, SECONDS pass or STOP-P answers true, then end the wait.
=> :new, :quiet or :stop."
  (let ((tag (imap-tag wire))
        (deadline (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))
        (outcome nil))
    (wire-write wire (format nil "~a IDLE~a" tag +crlf+))
    (imap-continuation wire tag "IDLE")
    (loop until outcome
          do (cond ((funcall stop-p) (setf outcome :stop))
                   ((>= (get-internal-real-time) deadline) (setf outcome :quiet))
                   ((listen (wire-stream wire))
                    (let ((line (wire-read-line wire)))
                      (cond ((ppcre:scan "(?i)^\\* \\d+ EXISTS" line) (setf outcome :new))
                            ((ppcre:scan "(?i)^\\* BYE" line)
                             (error "~a ended the session: ~a" (wire-host wire) line)))))
                   (t (sleep +idle-check-seconds+))))
    (wire-write wire (format nil "DONE~a" +crlf+))
    (imap-collect wire tag "IDLE")
    outcome))

(defun list-mailbox-name (segments)
  "The mailbox one LIST response names: quoted, an atom, or a literal."
  (let ((literal (find-if (lambda (s) (typep s '(vector (unsigned-byte 8)))) segments)))
    (if literal
        (octets-latin1 literal)
        (ppcre:register-groups-bind (quoted atom)
            ("^\\* LIST \\([^)]*\\) (?:\"(?:[^\"\\\\]|\\\\.)*\"|NIL) (?:\"((?:[^\"\\\\]|\\\\.)*)\"|(\\S+))\\s*$"
             (trim (first segments)))
          (if quoted (ppcre:regex-replace-all "\\\\(.)" quoted "\\1") atom)))))

(defun imap-special-mailbox (wire use)
  "The mailbox the server marks with the special-use attribute USE (RFC
6154), such as \\Sent, or NIL."
  (loop for segments in (imap-command wire "LIST" "LIST \"\" \"*\"")
        for attributes = (ppcre:register-groups-bind (words) ("(?i)^\\* LIST \\(([^)]*)\\)" (first segments))
                           (ppcre:split "\\s+" words))
        when (member use attributes :test #'string-equal)
          return (list-mailbox-name segments)))

(defun imap-append (wire mailbox data &key (flags '("\\Seen")))
  "File DATA, a composed message, in MAILBOX with FLAGS. => the copy's UID
when the server names it (UIDPLUS), else T."
  (let ((octets (utf8 data))
        (tag (imap-tag wire)))
    (wire-write wire (format nil "~a APPEND ~a (~{~a~^ ~}) {~d}~a"
                             tag (imap-quote mailbox) flags (length octets) +crlf+))
    (imap-continuation wire tag "APPEND")
    (write-sequence octets (wire-stream wire))
    (wire-write wire +crlf+)
    (let ((status (nth-value 1 (imap-collect wire tag "APPEND"))))
      (or (ppcre:register-groups-bind ((#'parse-integer uid)) ("(?i)\\[APPENDUID \\d+ (\\d+)\\]" status) uid)
          t))))

;;; --- SMTP -----------------------------------------------------------------------

(defun smtp-reply (wire)
  "One (possibly multi-line) reply: => (values CODE TEXT)."
  (let ((lines '()) (code nil))
    (loop
      (let ((line (wire-read-line wire)))
        (setf code (parse-integer line :end (min 3 (length line)) :junk-allowed t))
        (push (if (> (length line) 4) (subseq line 4) "") lines)
        (when (or (< (length line) 4) (char/= (char line 3) #\-))
          (return (values code (format nil "~{~a~^ ~}" (nreverse lines)))))))))

(defun smtp-expect (wire codes what)
  (multiple-value-bind (code text) (smtp-reply wire)
    (unless (member code codes)
      (error "SMTP ~a refused: ~a ~a" what code text))
    (values code text)))

(defun dot-stuff (data)
  "DATA as an SMTP DATA body: CRLF lines, a leading dot doubled."
  (with-output-to-string (out)
    (dolist (line (split-lines data))
      (when (and (plusp (length line)) (char= (char line 0) #\.))
        (write-char #\. out))
      (write-string line out)
      (write-string +crlf+ out))))

(defun smtp-send (host port user password from recipients data &key (timeout 60) (helo "nodecode.local"))
  "Send DATA, a composed message, from FROM to RECIPIENTS through HOST:PORT
as USER. => the server's acceptance text (it often names the queue id)."
  (let ((wire (wire-connect host port :tls (= port 465) :timeout timeout)))
    (unwind-protect
         (flet ((say (control &rest arguments)
                  (wire-write wire (format nil "~?~a" control arguments +crlf+))))
           (smtp-expect wire '(220) "greeting")
           (say "EHLO ~a" helo)
           (let ((features (nth-value 1 (smtp-expect wire '(250) "EHLO"))))
             (unless (= port 465)
               (unless (search "STARTTLS" (string-upcase features))
                 (error "SMTP ~a offers no STARTTLS: the password is never sent in the clear" host))
               (say "STARTTLS")
               (smtp-expect wire '(220) "STARTTLS")
               (wire-starttls wire)
               (say "EHLO ~a" helo)
               (smtp-expect wire '(250) "EHLO")))
           (say "AUTH PLAIN ~a" (cl-base64:usb8-array-to-base64-string
                                 (utf8 (format nil "~c~a~c~a" (code-char 0) user (code-char 0) password))))
           (smtp-expect wire '(235) "AUTH")
           (say "MAIL FROM:<~a>" from)
           (smtp-expect wire '(250) "MAIL FROM")
           (dolist (recipient recipients)
             (say "RCPT TO:<~a>" recipient)
             (smtp-expect wire '(250 251) "RCPT TO"))
           (say "DATA")
           (smtp-expect wire '(354) "DATA")
           (wire-write wire (dot-stuff data))
           (say ".")
           (prog1 (nth-value 1 (smtp-expect wire '(250) "the message"))
             (ignore-errors (say "QUIT") (smtp-reply wire))))
      (wire-close wire))))
