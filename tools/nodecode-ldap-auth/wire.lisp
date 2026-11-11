;;;; wire.lisp --- the connection to the directory, and the three things it is asked.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A password is checked one way: bind as the user with it and see whether the
;;;; directory says yes. Around that bind the original plugin did four things this
;;;; keeps: it refused an empty password before any bind (LDAP reads one as an
;;;; anonymous bind, which always succeeds), it escaped the username before it
;;;; went into a DN or a filter, it opened a fresh connection per attempt with a
;;;; timeout, and it never chased a referral - so credentials are never replayed
;;;; to a host the directory names.
;;;;
;;;; One seam is a variable, *OPENER*: a test binds it and the whole cell runs
;;;; against a scripted directory with no socket in sight.

(in-package #:nodecode-ldap-auth)

;;; --- the seam ----------------------------------------------------------------

(defstruct (transport (:constructor make-transport (send read &key (close (lambda ()))
                                                          upgrade (tls nil))))
  "One open conversation with a directory: SEND octets, READ one message's
octets, CLOSE it, and UPGRADE it to TLS in place (NIL when it cannot)."
  (send (lambda (octets) (declare (ignore octets))) :read-only t)
  (read (lambda () nil) :read-only t)
  (close (lambda ()) :read-only t)
  (upgrade nil :read-only t)
  (tls nil :read-only t))

(defvar *opener* (lambda (settings) (open-directory settings))
  "SETTINGS => a TRANSPORT. The one seam: a test binds it to a scripted
directory and the cell's verbs run with nothing dialled.")

(defstruct (endpoint (:constructor make-endpoint (host port ldaps)))
  host port ldaps)

(defun endpoint-of (url)
  "URL as an ENDPOINT: host, port, and whether the session is TLS throughout.
A missing port is 636 for ldaps:// and 389 for ldap://; a path is ignored."
  (let* ((ldaps (starts-with-p "ldaps://" url))
         (rest (cond (ldaps (subseq url 8))
                     ((starts-with-p "ldap://" url) (subseq url 7))
                     (t url)))
         (slash (position #\/ rest))
         (hostport (if slash (subseq rest 0 slash) rest))
         (colon (position #\: hostport :from-end t)))
    (if colon
        (make-endpoint (subseq hostport 0 colon)
                       (or (ignore-errors (parse-integer hostport :start (1+ colon) :junk-allowed t))
                           (if ldaps 636 389))
                       ldaps)
        (make-endpoint hostport (if ldaps 636 389) ldaps))))

(defun short-error (condition)
  "CONDITION's text on one line: what a caller can read."
  (let* ((text (princ-to-string condition))
         (end (or (position #\Newline text) (length text))))
    (subseq text 0 end)))

(defun read-element (stream)
  "The next BER element on STREAM, whole, as octets."
  (flet ((octet () (or (read-byte stream nil nil)
                        (fail "the directory closed the connection mid-answer"))))
    (let* ((tag (octet))
           (first (octet))
           (length (if (< first #x80)
                       first
                       (loop with n = 0
                             repeat (logand first #x7f)
                             do (setf n (logior (ash n 8) (octet)))
                             finally (return n))))
           (body (make-array length :element-type '(unsigned-byte 8))))
      (loop for i below length do (setf (aref body i) (octet)))
      (concat (octet-vector (list tag)) (octet-vector (ber-length length)) body))))

(defun open-directory (settings)
  "SETTINGS => a TRANSPORT to its directory: usocket, with cl+ssl for ldaps://
or the StartTLS upgrade."
  (let* ((endpoint (endpoint-of (getf settings :server-url)))
         (host (endpoint-host endpoint))
         (port (endpoint-port endpoint))
         (timeout (getf settings :timeout-seconds 5))
         (ca-file (getf settings :ca-certs-file ""))
         (socket (handler-case
                     (usocket:socket-connect host port
                                              :element-type '(unsigned-byte 8)
                                              :nodelay t :timeout timeout)
                   (error (cause)
                     (fail "~a:~d did not answer: ~a" host port (short-error cause)))))
         (stream (usocket:socket-stream socket)))
    (labels ((send (octets)
               (handler-case (progn (write-sequence octets stream) (finish-output stream))
                 (error (cause) (fail "~a:~d stopped answering: ~a" host port (short-error cause)))))
             (read-one ()
               (handler-case (read-element stream)
                 (error (cause) (fail "~a:~d stopped answering: ~a" host port (short-error cause)))))
             (shut ()
               (ignore-errors (close stream))
               (ignore-errors (usocket:socket-close socket)))
             (upgrade ()
               (handler-case
                   (setf stream (apply #'cl+ssl:make-ssl-client-stream stream
                                       :hostname host :verify :required
                                       (when (and (stringp ca-file) (plusp (length ca-file)))
                                         (list :ca-file ca-file))))
                 (error (cause)
                   (fail "the TLS handshake with ~a:~d failed: ~a" host port (short-error cause))))
               (make-transport #'send #'read-one :close #'shut :upgrade #'upgrade :tls t)))
      (when (endpoint-ldaps endpoint)
        (upgrade))
      (make-transport #'send #'read-one :close #'shut :upgrade #'upgrade
                      :tls (endpoint-ldaps endpoint)))))

;;; --- one conversation ----------------------------------------------------------

(defstruct (session (:constructor make-session (transport)))
  (transport nil :read-only t)
  (id 1 :type integer))

(defun next-id (session)
  (prog1 (session-id session) (incf (session-id session))))

(defun next-message (session)
  "The next reply, parsed; a reply that is cut short is a failure, not a denial."
  (let ((octets (funcall (transport-read (session-transport session)))))
    (unless octets
      (fail "the directory's answer ended early"))
    (parse-message octets)))

(defun bind-on (session dn password)
  "Bind as DN with PASSWORD on SESSION => the result code (0 accepted, 49
rejected, anything else is the directory's own result)."
  (let ((transport (session-transport session)))
    (funcall (transport-send transport) (bind-request (next-id session) dn password))
    (let ((message (next-message session)))
      (unless (eq :result (getf (getf message :body) :kind))
        (fail "the directory answered a bind with something else"))
      (getf (getf message :body) :code))))

(defun search-on (session &key base scope filter attributes)
  "Search on SESSION => (values ENTRIES CODE); ENTRIES are parsed entries."
  (let ((transport (session-transport session))
        (entries '()))
    (funcall (transport-send transport)
             (search-request (next-id session) :base base :scope scope
                                                   :filter filter :attributes attributes))
    (loop
      (let* ((message (next-message session))
             (body (getf message :body)))
        (case (getf body :kind)
          (:entry (push body entries))
          (:result (return (values (nreverse entries) (getf body :code))))
          (t nil))))))

(defun unbind-on (session)
  "Say goodbye and close; a failure here is never a failed login."
  (let ((transport (session-transport session)))
    (ignore-errors (funcall (transport-send transport) (unbind-request (next-id session))))
    (ignore-errors (funcall (transport-close transport)))))

(defun open-session (settings)
  "A dialled session, StartTLS done when the settings ask for it."
  (let ((transport (funcall *opener* settings)))
    (when (and (getf settings :start-tls) (not (transport-tls transport)))
      (setf transport (start-tls transport)))
    (make-session transport)))

(defun start-tls (transport)
  "Upgrade TRANSPORT in place => the upgraded one, or a failure."
  (let ((session (make-session transport)))
    (funcall (transport-send transport) (starttls-request (next-id session)))
    (let* ((message (next-message session))
           (body (getf message :body))
           (code (getf body :code)))
      (unless (eql code +result-success+)
        (ignore-errors (funcall (transport-close transport)))
        (fail "the directory refused StartTLS (result ~d~@[~]: ~a~])"
              code (plusp (length (or (getf body :diagnostic) "")))
              (getf body :diagnostic)))
      (let ((upgrade (transport-upgrade transport)))
        (unless upgrade
          (fail "this directory connection cannot be upgraded to TLS"))
        (funcall upgrade)))))

(defmacro with-session ((var settings) &body body)
  "BODY over an open SESSION, unbound and closed on the way out."
  `(let ((,var (open-session ,settings)))
     (unwind-protect (progn ,@body)
       (unbind-on ,var))))

;;; --- what the directory is asked ----------------------------------------------

(defun rejected-p (code)
  (eql code +result-invalid-credentials+))

(defun service-bind (session settings)
  "Bind as the service account (or anonymously when none is named); a rejection
here is the operator's problem, not the user's."
  (let* ((dn (getf settings :bind-dn ""))
         (code (bind-on session dn (getf settings :bind-password ""))))
    (unless (eql code +result-success+)
      (fail "the directory rejected the service-account bind ~@[(uid ~a) ~](result ~d): check bind_dn and bind_password"
            (and (plusp (length dn)) dn) code))))

(defun find-entry (settings session username)
  "The one entry USERNAME's filter names, or NIL. Two matches reject the
login: a filter that is not unique must never let a bind against any of
them succeed."
  (let* ((filter (getf settings :user-search-filter "(uid={username})"))
         (escaped (escape-filter username))
         (text (substitute-username filter escaped)))
    (multiple-value-bind (entries code)
        (search-on session
                   :base (getf settings :user-search-base)
                   :scope +subtree+
                   :filter (parse-filter text)
                   :attributes (list (getf settings :email-attribute "mail")
                                     (getf settings :display-name-attribute "cn")))
      (unless (member code (list +result-success+ +result-no-such-object+) :test #'=)
        (fail "the user search under ~a failed (result ~d): check user_search_base and the service account's read access"
              (getf settings :user-search-base) code))
      (and (= 1 (length entries)) (first entries)))))

(defun substitute-username (template username)
  "TEMPLATE with every {username} replaced by the already-escaped USERNAME."
  (with-output-to-string (out)
    (loop with at = 0
          for start = (search "{username}" template :start2 at)
          do (cond (start
                    (write-string template out :start at :end start)
                    (write-string username out)
                    (setf at (+ start (length "{username}"))))
                   (t (write-string template out :start at) (return))))))

(defun parse-filter (text)
  "TEXT, one filter from the section, as the filter form ENCODE-FILTER writes.
The subset a login needs: present, equality, and and/or/not around them."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (inner (if (and (plusp (length text))
                         (char= (char text 0) #\()
                         (char= (char text (1- (length text))) #\)))
                    (subseq text 1 (1- (length text)))
                    text)))
    (parse-filter-inner inner)))

(defun parse-filter-inner (text)
  (when (zerop (length text))
    (fail "the user_search_filter is empty"))
  (cond
    ((char= (char text 0) #\&) (cons :and (parse-filter-list text 1)))
    ((char= (char text 0) #\|) (cons :or (parse-filter-list text 1)))
    ((char= (char text 0) #\!)
     (list :not (parse-filter (first (parse-filter-list text 1)))))
    ((char= (char text 0) #\=)
     (list :equal (subseq text 1) "")) ; not a filter a login writes
    (t
     (let ((equals (position #\= text)))
       (cond ((null equals) (list :present text))
             ((and (> equals 0) (char= (char text (1- equals)) #\~))
              (list :equal (subseq text 0 (1- equals)) (unescape-filter (subseq text (1+ equals)))))
             ((and (> equals 0) (char= (char text (1- equals)) #\*))
              (list :present (subseq text 0 (1- equals))))
             (t (list :equal (subseq text 0 equals) (unescape-filter (subseq text (1+ equals))))))))))

(defun parse-filter-list (text at)
  "The parenthesised filters that follow TEXT's leading operator."
  (let ((forms '())
        (at at))
    (loop while (< at (length text))
          do (let ((open (position #\( text :start at)))
               (unless open (return))
               (let ((close (matching-paren text open)))
                 (push (parse-filter (subseq text open (1+ close))) forms)
                 (setf at (1+ close)))))
    (nreverse forms)))

(defun matching-paren (text open)
  "The index of the parenthesis that closes the one at OPEN, honouring the
\\28 and \\29 escapes a filter value may carry."
  (let ((depth 0))
    (loop for at from open below (length text)
          for char = (char text at)
          do (cond ((char= char #\() (incf depth))
                   ((char= char #\))
                    (decf depth)
                    (when (zerop depth) (return at)))))))

(defun unescape-filter (value)
  "VALUE with the RFC 4515 hex escapes read back."
  (with-output-to-string (out)
    (loop with at = 0
          while (< at (length value))
          do (let ((char (char value at)))
               (if (and (char= char #\\) (< (+ at 2) (length value)))
                   (let ((code (ignore-errors (parse-integer value :start (1+ at) :end (+ at 3) :radix 16))))
                     (if code (write-char (code-char code) out) (write-char char out))
                     (incf at 3))
                   (progn (write-char char out) (incf at)))))))

(defun group-filter (dn username)
  "The membership probe the original built: the three common group schemas
in one filter, each value escaped."
  (list :or (list :equal "member" (escape-filter dn))
          (list :equal "uniqueMember" (escape-filter dn))
          (list :equal "memberUid" (escape-filter username))))

(defun in-group-p (session settings dn username)
  "Whether the freshly-bound user's own connection can read the group entry
and finds them in it."
  (multiple-value-bind (entries code)
      (search-on session :base (getf settings :require-group) :scope +base-object+
                         :filter (group-filter dn username) :attributes '())
    (and (eql code +result-success+) (plusp (length entries)))))

(defun still-present-p (settings dn)
  "Whether DN is still in the directory: the refresh probe search mode makes."
  (if (zerop (length (or dn "")))
      nil
      (with-session (session settings)
        (service-bind session settings)
        (multiple-value-bind (entries code)
            (search-on session :base dn :scope +base-object+
                               :filter '(:present "objectClass") :attributes '())
          (and (eql code +result-success+) (plusp (length entries)))))))

(defun identity-of (settings entry username)
  "ENTRY as the identity a session carries: DN, mail and display name."
  (list :dn (getf entry :dn)
        :email (attribute-value entry (getf settings :email-attribute "mail"))
        :display (let ((name (attribute-value entry (getf settings :display-name-attribute "cn"))))
                   (if (plusp (length name)) name username))))

(defun verify-credentials (settings username password)
  "Check USERNAME and PASSWORD with an LDAP bind => the identity plist, or NIL
when the directory said no. A directory that cannot be reached, a service
account the directory rejects, or a group check that fails to run is a
failure (LDAP-AUTH-ERROR), never a denial."
  (when (or (not (stringp username)) (zerop (length (string-trim " " username)))
            (not (stringp password)) (zerop (length (string-trim " " password))))
    ;; An empty password is an anonymous bind on an LDAP server, which always
    ;; succeeds: reject it before anything reaches the wire.
    (return-from verify-credentials nil))
  (let ((username (string-trim " " username)))
    (if (getf settings :user-dn-template)
        (verify-direct settings username password)
        (verify-searched settings username password))))

(defun verify-direct (settings username password)
  (let ((dn (substitute-username (getf settings :user-dn-template) (escape-dn username))))
    (with-session (session settings)
      (let ((code (bind-on session dn password)))
        (cond ((eql code +result-success+)
               (if (getf settings :require-group)
                   (and (in-group-p session settings dn username)
                        (list :dn dn :email "" :display username))
                   (list :dn dn :email "" :display username)))
              ((rejected-p code) nil)
              (t (fail "the directory answered the bind with result ~d" code)))))))

(defun verify-searched (settings username password)
  (multiple-value-bind (dn entry)
      (with-session (session settings)
        (service-bind session settings)
        (let ((entry (find-entry settings session username)))
          (values (and entry (getf entry :dn)) entry)))
    (unless dn
      ;; Timing pad: an unknown user costs one bind too, so the two answers
      ;; cannot be told apart by how long they took.
      (with-session (session settings)
        (bind-on session "uid=probe,dc=invalid" password))
      (return-from verify-searched nil))
    (with-session (session settings)
      (let ((code (bind-on session dn password)))
        (cond ((eql code +result-success+)
               (if (getf settings :require-group)
                   (and (in-group-p session settings dn username)
                        (identity-of settings entry username))
                   (identity-of settings entry username)))
              ((rejected-p code) nil)
              (t (fail "the directory answered the bind with result ~d" code)))))))

(defun lookup-account (settings username)
  "The directory's own record of USERNAME, with no password: what /ldap-auth
<username> answers, and what a caller checks a login against. Search mode only."
  (unless (getf settings :user-search-base)
    (fail "a lookup needs search-then-bind: set user_search_base and user_search_filter in the ldap-auth section (the direct-bind mode knows no username to search for)"))
  (let ((username (string-trim " " (or username ""))))
    (when (zerop (length username))
      (fail "name a username to look up"))
    (with-session (session settings)
      (service-bind session settings)
      (let ((entry (find-entry settings session username)))
        (unless entry
          (return-from lookup-account nil))
        (when (getf settings :require-group)
          (setf entry (append entry (list :in-group (in-group-p session settings (getf entry :dn) username)))))
        (append (identity-of settings entry username) (list :entry entry))))))
