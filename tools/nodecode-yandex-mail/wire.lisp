;;;; wire.lisp --- IMAP and SMTP, spoken over the sockets nodecode loads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The few commands the tools need and no more: IMAP logs in, lists and
;;;; searches mailboxes, fetches with BODY.PEEK so reading never sets a flag,
;;;; stores flags, copies, expunges, and files a copy of what was sent; SMTP
;;;; authenticates with the same app password and sends one message. TLS is
;;;; required and verified - implicit on 993 and 465, STARTTLS elsewhere - so
;;;; the password never crosses the wire in the clear.
;;;;
;;;; Everything above the transport is one shape: a segment list per server
;;;; response, lines and literal octet vectors in the order they arrived. The
;;;; transport is a generic function away from the socket, which is how the
;;;; tests drive a scripted mailbox with no network at all.

(in-package #:nodecode-yandex-mail)

(defparameter +crlf+ (format nil "~c~c" #\Return #\Linefeed)
  "What every IMAP and SMTP line ends with.")

;;; --- bytes and text -------------------------------------------------------------

(defun utf8 (text)
  "TEXT as UTF-8 octets."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun octets-line (octets)
  "OCTETS, one character per octet: the shape an IMAP line arrives in."
  (let ((out (make-string (length octets))))
    (loop for i below (length octets)
          do (setf (char out i) (code-char (aref octets i))))
    out))

(defun protocol-bytes (text)
  "TEXT as the bytes of a protocol line: UTF-8, one octet per character of
an octet string the server sent being already such a string."
  (utf8 text))

;;; --- the transport --------------------------------------------------------------

(defstruct (transport (:copier nil) (:constructor make-transport (stream socket host)))
  "One open connection: its stream, the socket under it, and the host it
reached, which a TLS handshake names."
  stream socket host (tag 0))

(defvar *tag-counter* 0
  "The IMAP tag of the last command; it only has to be unique within one
connection, and a counter over every connection is.")

(defun transport-name (transport)
  "What to call the peer in a refusal: the host, when this is a connection
this file opened."
  (if (typep transport 'transport) (transport-host transport) "the mail server"))

(defvar *open-transport* nil
  "The function that opens one connection: (HOST PORT TLS TIMEOUT) => a TRANSPORT.
Bound to %OPEN-TRANSPORT at load; the tests bind a scripted one instead.")

(defgeneric transport-write (transport text)
  (:documentation "Send TEXT - a line, CRLF already on it - and flush it."))
(defgeneric transport-read-line (transport)
  (:documentation "One line without its CRLF, or NIL at end of stream."))
(defgeneric transport-read-octets (transport size)
  (:documentation "Exactly SIZE octets, as an octet vector."))
(defgeneric transport-starttls (transport)
  (:documentation "Wrap the connection in TLS, verifying the host's certificate."))
(defgeneric transport-close (transport)
  (:documentation "Close it, quietly, whatever state it is in."))

(defun %open-transport (host port tls timeout)
  "A connection to HOST:PORT, TLS from the first byte when TLS, every read
bounded by TIMEOUT seconds."
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)
                                        :timeout (max 1 (or timeout 30)))))
    (handler-case
        (progn
          (setf (usocket:socket-option socket :receive-timeout) (max 1 (or timeout 30)))
          (make-transport (if tls
                              (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket)
                                                             :hostname host :verify :required)
                              (usocket:socket-stream socket))
                          socket host))
      (error (condition)
        (ignore-errors (usocket:socket-close socket))
        (error condition)))))

(setf *open-transport* #'%open-transport)

(defmethod transport-write ((transport transport) text)
  (let ((stream (transport-stream transport)))
    (write-sequence (if (stringp text) (protocol-bytes text) text) stream)
    (finish-output stream))
  text)

(defmethod transport-read-line ((transport transport))
  (let ((stream (transport-stream transport))
        (out (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte)
                    (when (zerop (length out)) (return nil)))
                   ((= byte 10) (return))
                   (t (vector-push-extend byte out))))
    (when (and (plusp (length out)) (= 13 (aref out (1- (length out)))))
      (decf (fill-pointer out)))
    (octets-line out)))

(defmethod transport-read-octets ((transport transport) size)
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (unless (= size (read-sequence buffer (transport-stream transport)))
      (fail "~a closed the connection inside a ~d-octet literal"
                        (transport-name transport) size))
    buffer))

(defmethod transport-starttls ((transport transport))
  (setf (transport-stream transport)
        (cl+ssl:make-ssl-client-stream (usocket:socket-stream (transport-socket transport))
                                       :hostname (transport-name transport)
                                       :verify :required))
  transport)

(defmethod transport-close ((transport transport))
  (when transport
    (ignore-errors (close (transport-stream transport) :abort t))
    (ignore-errors (usocket:socket-close (transport-socket transport))))
  nil)

;;; --- IMAP framing ----------------------------------------------------------------

(defun tagged-p (line tag)
  (and (stringp line) (uiop:string-prefix-p (concatenate 'string tag " ") line)))

(defun literal-size (line)
  "The octet count LINE announces with a trailing {N}, or NIL."
  (when (and (stringp line)
             (plusp (length line))
             (char= (char line (1- (length line))) #\}))
    (let ((open (position #\{ line :from-end t)))
      (when open
        (let ((digits (subseq line (1+ open) (1- (length line)))))
          (when (and (plusp (length digits)) (every #'digit-char-p digits))
            (parse-integer digits)))))))

(defun imap-read-response (transport)
  "One response with its literals: its lines and octet vectors, in order."
  (let ((segments '()))
    (loop
      (let* ((line (transport-read-line transport))
             (size (progn (unless line
                            (fail "~a closed the connection"
                                               (transport-name transport)))
                          (literal-size line))))
        (push line segments)
        (unless size (return (nreverse segments)))
        (push (transport-read-octets transport size) segments)))))

(defun imap-collect (transport tag what)
  "Read through TAG's tagged answer. => (values the untagged responses, in
order; the tagged status line). WHAT names the command in a refusal, never
the command line, which may carry the password."
  (let ((responses '()))
    (loop
      (let* ((segments (imap-read-response transport))
             (line (first segments)))
        (if (tagged-p line tag)
            (let ((status (subseq line (1+ (length tag)))))
              (unless (uiop:string-prefix-p "OK" (string-upcase status))
                (fail "IMAP ~a refused: ~a" what status))
              (return (values (nreverse responses) status)))
            (push segments responses))))))

(defun imap-tagged (transport what control &rest arguments)
  "Send one tagged command and read through its tagged answer."
  (let ((tag (format nil "A~d" (incf *tag-counter*))))
    (transport-write transport (format nil "~a ~?~a" tag control arguments +crlf+))
    (imap-collect transport tag what)))

(defun imap-command (transport what control &rest arguments)
  (apply #'imap-tagged transport what control arguments))

(defun imap-quote (text)
  "TEXT as an IMAP quoted string."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across (princ-to-string text)
          do (when (member character '(#\" #\\))
               (write-char #\\ out))
             (write-char character out))
    (write-char #\" out)))

(defun imap-response-line (responses prefix)
  "RESPONSES' first line that starts with the upcased PREFIX, or NIL."
  (loop for segments in responses
        for line = (first segments)
        when (and (stringp line) (uiop:string-prefix-p prefix (string-upcase line)))
          return line))

;;; --- modified UTF-7, the encoding IMAP mailbox names use (RFC 3501 5.1.3) ---------

(defun utf16be->string (octets)
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 2
          for unit = (logior (ash (aref octets i) 8) (aref octets (1+ i)))
          do (cond ((and (<= #xD800 unit) (<= unit #xDBFF) (< (+ i 3) (length octets)))
                    (let ((low (logior (ash (aref octets (+ i 2)) 8) (aref octets (+ i 3)))))
                      (write-char (code-char (+ #x10000
                                                 (ash (- unit #xD800) 10)
                                                 (- low #xDC00)))
                                  out)
                      (incf i 2)))
                   (t (write-char (code-char unit) out))))))

(defun string->utf16be (text)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((put (unit)
             (vector-push-extend (ldb (byte 8 8) unit) out)
             (vector-push-extend (ldb (byte 8 0) unit) out)))
      (loop for character across text
            for code = (char-code character)
            do (if (< code #x10000)
                   (put code)
                   (let ((offset (- code #x10000)))
                     (put (+ #xD800 (ash offset -10)))
                     (put (+ #xDC00 (logand offset #x3FF)))))))
    out))

(defun utf7-decode (name)
  "NAME as text: the &...- runs decoded, everything else as it stands."
  (with-output-to-string (out)
    (let ((i 0) (n (length name)))
      (loop while (< i n)
            do (let ((character (char name i)))
                 (if (char/= character #\&)
                     (progn (write-char character out) (incf i))
                     (let ((end (position #\- name :start (1+ i))))
                       (cond ((null end) (write-char #\& out) (setf i n))
                             ((= end (1+ i)) (write-char #\& out) (setf i (1+ end)))
                             (t (let* ((body (substitute #\/ #\, (subseq name (1+ i) end)))
                                       (padding (mod (- 4 (mod (length body) 4)) 4))
                                       (padded (concatenate 'string body
                                                            (make-string padding
                                                                         :initial-element #\=))))
                                  (handler-case
                                      (write-string (utf16be->string
                                                     (cl-base64:base64-string-to-usb8-array padded))
                                                    out)
                                    (error () (write-string (subseq name i (1+ end)) out)))
                                  (setf i (1+ end))))))))))))

(defun utf7-encode (name)
  "NAME as an IMAP mailbox name: ASCII as it stands, the rest in &...- runs."
  (with-output-to-string (out)
    (let ((pending '()))
      (flet ((flush ()
               (when pending
                 (let* ((octets (string->utf16be (coerce (nreverse pending) 'string)))
                        (b64 (cl-base64:usb8-array-to-base64-string octets))
                        (trimmed (string-right-trim "=" b64)))
                   (write-char #\& out)
                   (write-string (substitute #\, #\/ trimmed) out)
                   (write-char #\- out))
                 (setf pending '()))))
        (loop for character across name
              do (cond ((char= character #\&)
                        (flush)
                        (write-string "&-" out))
                       ((and (<= 32 (char-code character)) (<= (char-code character) 126)
                             (char/= character #\&))
                        (flush)
                        (write-char character out))
                       (t (push character pending))))
        (flush))))
  )

(defun imap-mailbox (name)
  "NAME as an IMAP argument: a quoted string in modified UTF-7."
  (imap-quote (utf7-encode name)))

;;; --- IMAP, the commands the tools use ---------------------------------------------

(defun imap-open (settings)
  "A logged-in IMAP connection, from the SETTINGS plist the cell runs on."
  (let* ((host (getf settings :imap-host))
         (port (getf settings :imap-port))
         (timeout (getf settings :timeout))
         (transport (funcall *open-transport* host port (getf settings :imap-tls) timeout)))
    (handler-case
        (progn
          (let ((greeting (transport-read-line transport)))
            (unless (and greeting (imap-greeting-p greeting))
              (fail "~a greeted with ~s, which is not an IMAP greeting"
                                host greeting))
            (when (and (not (getf settings :imap-tls)) (getf settings :starttls))
              (imap-command transport "STARTTLS" "STARTTLS")
              (transport-starttls transport)))
          (imap-command transport "LOGIN" "LOGIN ~a ~a"
                         (imap-quote (getf settings :login))
                         (imap-quote (getf settings :password)))
          transport)
      (yandex-mail-error (condition) (transport-close transport) (error condition))
      (error (condition) (transport-close transport) (error condition)))))

(defun imap-greeting-p (line)
  (let ((up (string-upcase line)))
    (or (uiop:string-prefix-p "* OK" up)
        (uiop:string-prefix-p "* PREAUTH" up))))

(defun imap-close (transport)
  (when transport
    (ignore-errors (imap-tagged transport "LOGOUT" "LOGOUT"))
    (transport-close transport)))

(defun imap-capabilities (transport)
  "The capabilities the server names, upcased."
  (let ((line (imap-response-line (imap-command transport "CAPABILITY" "CAPABILITY")
                                  "* CAPABILITY")))
    (when line
      (mapcar #'string-upcase (cdr (cdr (split-words line)))))))

(defun split-words (text)
  "TEXT split on runs of whitespace, empties dropped."
  (let ((words '()) (start nil))
    (loop for i below (length text)
          for space = (member (char text i) '(#\Space #\Tab #\Return #\Linefeed))
          do (cond (space (when start (push (subseq text start i) words) (setf start nil)))
                   ((null start) (setf start i))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun imap-list (transport)
  "Every mailbox: a plist (:name :flags :delimiter) each."
  (loop for segments in (imap-command transport "LIST" "LIST \"\" \"*\"")
        for line = (first segments)
        for entry = (parse-list-response line segments)
        when entry collect entry))

(defun unquote-imap (text)
  "TEXT, an IMAP quoted string or NIL, without its quotes and escapes."
  (if (and (> (length text) 1) (char= (char text 0) #\"))
      (with-output-to-string (out)
        (loop with i = 1
              while (< i (length text))
              do (let ((character (char text i)))
                   (cond ((char= character #\") (return))
                         ((char= character #\\)
                          (when (< (1+ i) (length text))
                            (write-char (char text (1+ i)) out))
                          (incf i 2))
                         (t (write-char character out) (incf i))))))
      text))

(defun parse-list-response (line segments)
  "One LIST response as a plist, or NIL when the line is not one.
The NAME is decoded from modified UTF-7; a literal name arrives as octets."
  (when (and (stringp line) (uiop:string-prefix-p "* LIST" (string-upcase line)))
    (let* ((open (position #\( line))
           (close (and open (position #\) line :start open))))
      (when (and open close)
        (let* ((flags (split-words (subseq line (1+ open) close)))
               (rest-text (string-trim '(#\Space #\Tab) (subseq line (1+ close))))
               (literal (find-if (lambda (s) (typep s '(vector (unsigned-byte 8)))) segments))
               (delimiter nil) (name nil))
          ;; The delimiter, then the name: either may be a quoted string, an
          ;; atom, or (the name) a literal the server sent separately.
          (let* ((first-quote (position #\" rest-text))
                 (after (and first-quote (position #\" rest-text :start (1+ first-quote)))))
            (cond ((and first-quote after)
                   (setf delimiter (unquote-imap (subseq rest-text first-quote (1+ after))))
                   (let* ((tail (string-trim '(#\Space #\Tab) (subseq rest-text (1+ after))))
                          (q (position #\" tail))
                          (e (and q (position #\" tail :start (1+ q)))))
                     (setf name (cond (e (unquote-imap (subseq tail q (1+ e))))
                                      ((and literal q) (utf7-decode (octets->string literal)))
                                      (t (string-trim '(#\Space #\Tab) tail))))))
                  (t (setf name (string-trim '(#\Space #\Tab) rest-text))))
            (when (equal delimiter "NIL") (setf delimiter nil))
            (when name
              (list :name (if literal (utf7-decode (octets->string literal)) (utf7-decode name))
                    :flags flags
                    :delimiter delimiter))))))))

(defun imap-status (transport mailbox)
  "Its message count and unseen count, as (values MESSAGES UNSEEN), or NILs."
  (let ((line (imap-response-line (imap-command transport "STATUS" "STATUS ~a (MESSAGES UNSEEN)"
                                                 (imap-mailbox mailbox))
                                  "* STATUS")))
    (when line
      (values (number-after line "MESSAGES") (number-after line "UNSEEN")))))

(defun number-after (line word)
  "The integer after WORD in LINE, or NIL."
  (let ((at (search (concatenate 'string word " ") (string-upcase line))))
    (when at
      (let ((rest (string-trim '(#\Space #\Tab #\))
                                (subseq line (+ at (length word))))))
        (parse-integer rest :junk-allowed t)))))

(defun imap-examine (transport mailbox)
  "Open MAILBOX read-only; => (values UIDVALIDITY UIDNEXT)."
  (let ((responses (imap-command transport "EXAMINE" "EXAMINE ~a" (imap-mailbox mailbox))))
    (values (bracketed-number responses "UIDVALIDITY")
            (bracketed-number responses "UIDNEXT"))))

(defun bracketed-number (responses word)
  (loop for segments in responses
        for at = (search (concatenate 'string "[" word " ")
                         (string-upcase (first segments)))
        when at collect (parse-integer (subseq (first segments) (+ at (length word) 2))
                                       :junk-allowed t)
          into found
        finally (return (first found))))

(defun imap-search (transport criteria)
  "The UIDs matching CRITERIA, a string already built, ascending."
  (let ((line (imap-response-line (imap-command transport "SEARCH" "UID SEARCH ~a" criteria)
                                  "* SEARCH")))
    (sort (if line
              (loop for word in (cddr (split-words line))
                    for uid = (parse-integer word :junk-allowed t)
                    when uid collect uid)
              '())
          #'<)))

(defun imap-fetch-raw (transport folder uid)
  "Message UID of FOLDER without touching its flags: its octets, its FLAGS,
its size and its INTERNALDATE, as four values."
  (imap-examine transport folder)
  (let ((responses (imap-command transport "FETCH"
                                  "UID FETCH ~d (UID FLAGS RFC822.SIZE INTERNALDATE BODY.PEEK[])"
                                  uid)))
    (dolist (segments responses (values nil nil nil nil))
      (let ((head (first segments)))
        (when (and (stringp head)
                   (uiop:string-prefix-p "* " head)
                   (search "FETCH" (string-upcase head))
                   (eql uid (number-after head "UID")))
          (return (values (find-if (lambda (s) (typep s '(vector (unsigned-byte 8)))) segments)
                          (flags-in head)
                          (number-after head "RFC822.SIZE")
                          (quoted-after head "INTERNALDATE"))))))))

(defun flags-in (line)
  "The flag names in LINE's FLAGS (...), upcased, or NIL."
  (let* ((up (string-upcase line))
         (at (search "FLAGS (" up)))
    (when at
      (let ((open (+ at 6))
            (close (position #\) up :start at)))
        (when close
          (split-words (subseq line (1+ open) close)))))))

(defun quoted-after (line word)
  "The first quoted string after WORD in LINE, or NIL."
  (let ((at (search (concatenate 'string word " \"") (string-upcase line))))
    (when at
      (let* ((open (+ at (length word) 2))
             (close (position #\" line :start open)))
        (when close (subseq line open close))))))

(defun imap-fetch-headers (transport uids fields)
  "For each of UIDS: a plist of (:uid :flags :size :internaldate :headers),
HEADERS the requested header block as text."
  (when uids
    (let ((responses (imap-command
                      transport "FETCH"
                      "UID FETCH ~{~d~^,~} (UID FLAGS RFC822.SIZE INTERNALDATE BODY.PEEK[HEADER.FIELDS (~{~a~^ ~})])"
                      uids fields)))
      (loop for segments in responses
            for head = (first segments)
            for uid = (and (stringp head) (consp (cdr segments)) (number-after head "UID"))
            when (and uid (search "FETCH" (string-upcase head)))
              collect (list :uid uid
                            :flags (flags-in head)
                            :size (number-after head "RFC822.SIZE")
                            :internaldate (quoted-after head "INTERNALDATE")
                            :headers (let ((literal (find-if (lambda (s)
                                                                (typep s '(vector (unsigned-byte 8))))
                                                              segments)))
                                       (and literal (octets->string literal))))))))

(defun octets->string (octets)
  "OCTETS as text: UTF-8 when they are, one character per octet when not."
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
    (error () (octets-line octets))))

(defun imap-store (transport folder uids &key add remove)
  "Add and remove flags on UIDS of FOLDER, silently."
  (when (or add remove)
    (imap-examine transport folder)
    (when add
      (imap-command transport "STORE" "UID STORE ~{~d~^,~} +FLAGS.SILENT (~{~a~^ ~})" uids add))
    (when remove
      (imap-command transport "STORE" "UID STORE ~{~d~^,~} -FLAGS.SILENT (~{~a~^ ~})" uids remove))
    t))

(defun imap-copy (transport folder uids destination)
  "Copy UIDS of FOLDER into DESTINATION."
  (imap-examine transport folder)
  (imap-command transport "COPY" "UID COPY ~{~d~^,~} ~a" uids (imap-mailbox destination))
  t)

(defun imap-expunge (transport folder uids &key (uidplus nil) (sweep nil))
  "Remove the \\Deleted messages of FOLDER: only UIDS when the server honours
UIDPLUS, the whole folder when SWEEP."
  (imap-examine transport folder)
  (cond (uidplus (imap-command transport "EXPUNGE" "UID EXPUNGE ~{~d~^,~}" uids))
        (sweep (imap-command transport "EXPUNGE" "EXPUNGE")))
  uids)

;;; --- SMTP ------------------------------------------------------------------------

(defun smtp-reply (transport)
  "One (possibly multi-line) reply: => (values CODE TEXT)."
  (let ((lines '()) (code nil))
    (loop
      (let ((line (transport-read-line transport)))
        (unless line (fail "the mail server closed the connection"))
        (setf code (parse-integer line :end (min 3 (length line)) :junk-allowed t))
        (push (if (> (length line) 4) (subseq line 4) "") lines)
        (when (or (< (length line) 4) (char/= (char line 3) #\-))
          (return (values code (format nil "~{~a~^ ~}" (nreverse lines)))))))))

(defun smtp-expect (transport codes what)
  (multiple-value-bind (code text) (smtp-reply transport)
    (unless (member code codes)
      (fail "SMTP ~a refused: ~a ~a" what code text))
    text))

(defun dot-stuff (data)
  "DATA as an SMTP DATA body: CRLF lines, a leading dot doubled."
  (with-output-to-string (out)
    (loop for line in (split-lines data)
          do (when (and (plusp (length line)) (char= (char line 0) #\.))
               (write-char #\. out))
             (write-string line out)
             (write-string +crlf+ out))))

(defun split-lines (text)
  "TEXT's lines, CRLF or LF, without the terminators."
  (let ((lines '()) (start 0))
    (loop for i below (length text)
          when (char= (char text i) #\Newline)
            do (let ((end (if (and (plusp i) (char= (char text (1- i)) #\Return)) (1- i) i)))
                 (push (subseq text start end) lines)
                 (setf start (1+ i))))
    (when (< start (length text)) (push (subseq text start) lines))
    (nreverse lines)))

(defun smtp-send (settings from recipients data)
  "Send DATA, a composed message, from FROM to RECIPIENTS, through the SMTP
host in SETTINGS, as the account's login. => the server's acceptance text."
  (let* ((host (getf settings :smtp-host))
         (port (getf settings :smtp-port))
         (implicit (getf settings :smtp-tls))
         (transport (funcall *open-transport* host port implicit (getf settings :timeout))))
    (unwind-protect
         (flet ((say (control &rest arguments)
                  (transport-write transport (format nil "~?~a" control arguments +crlf+))))
           (smtp-expect transport '(220) "greeting")
           (say "EHLO ~a" (getf settings :helo))
           (let* ((features (smtp-expect transport '(250) "EHLO"))
                  (supported (string-upcase features)))
             (unless implicit
               (unless (search "STARTTLS" supported)
                 (fail "~a offers no STARTTLS: the password is never sent in the clear"
                                    host))
               (say "STARTTLS")
               (smtp-expect transport '(220) "STARTTLS")
               (transport-starttls transport)
               (say "EHLO ~a" (getf settings :helo))
               (smtp-expect transport '(250) "EHLO"))
             (if (search "AUTH PLAIN" supported)
                 (progn
                   (say "AUTH PLAIN ~a"
                        (cl-base64:usb8-array-to-base64-string
                         (utf8 (format nil "~c~a~c~a" (code-char 0)
                                       (getf settings :login) (code-char 0)
                                       (getf settings :password)))))
                   (smtp-expect transport '(235) "AUTH"))
                 (progn
                   (say "AUTH LOGIN")
                   (smtp-expect transport '(334) "AUTH LOGIN")
                   (say "~a" (cl-base64:usb8-array-to-base64-string
                              (utf8 (getf settings :login))))
                   (smtp-expect transport '(334) "AUTH LOGIN user")
                   (say "~a" (cl-base64:usb8-array-to-base64-string
                              (utf8 (getf settings :password))))
                   (smtp-expect transport '(235) "AUTH LOGIN password"))))
           (say "MAIL FROM:<~a>" from)
           (smtp-expect transport '(250) "MAIL FROM")
           (dolist (recipient recipients)
             (say "RCPT TO:<~a>" recipient)
             (smtp-expect transport '(250 251) "RCPT TO"))
           (say "DATA")
           (smtp-expect transport '(354) "DATA")
           (transport-write transport (dot-stuff data))
           (say ".")
           (prog1 (smtp-expect transport '(250) "the message")
             (ignore-errors (say "QUIT") (smtp-reply transport))))
      (transport-close transport))))
