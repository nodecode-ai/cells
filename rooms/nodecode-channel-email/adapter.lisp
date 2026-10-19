;;;; adapter.lisp --- the email platform, its executor, and the polling lap.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One email thread is one room: the first message names it, every reply in
;;;; it continues the same conversation, and the answer goes back as a reply
;;;; email threaded under the ask (In-Reply-To, References, Re: subject).
;;;; Email has no live card, no typing and no edits: the kit's status card
;;;; and its edits plan to nothing, so a turn sends exactly its answer.
;;;;
;;;; The book remembers what threading needs across restarts -- each thread's
;;;; correspondent, subject and References, each message id the kit holds,
;;;; and how far the mailbox was read -- in one file under the home.

(in-package #:nodecode-channel-email)

(defparameter +email-text-limit+ 100000
  "An answer is one email: the limit is only a guard against a runaway one.")

(defparameter +references-kept+ 12
  "How many ids a reply's References carries: the root, then the latest.")

;;; --- the book -------------------------------------------------------------------

(defstruct (mail-book (:copier nil) (:constructor %make-mail-book))
  (lock (bt2:make-lock :name "channel-email-book"))
  (path nil)
  ;; channel -> (:to ADDRESS :name NAME :subject SUBJECT :references (ID ...) :last ID)
  (threads (make-hash-table :test 'equal))
  ;; the kit's message id -> <Message-ID>
  (ids (make-hash-table :test 'equal))
  ;; <Message-ID> -> channel, for every message the book has seen or sent
  (rooms (make-hash-table :test 'equal))
  (uidvalidity nil)
  (last-uid nil))

(defun hash-alist (table)
  (loop for key being the hash-keys of table using (hash-value value) collect (cons key value)))

(defun fill-hash (table alist)
  (loop for (key . value) in alist do (setf (gethash key table) value)))

(defun make-mail-book (&key path)
  "A book kept at PATH (read back when it exists), or only in memory."
  (let ((book (%make-mail-book :path path)))
    (when (and path (probe-file path))
      (handler-case
          (let ((data (with-open-file (in path :external-format :utf-8)
                        (with-standard-io-syntax
                          (let ((*read-eval* nil)) (read in))))))
            (fill-hash (mail-book-threads book) (getf data :threads))
            (fill-hash (mail-book-ids book) (getf data :ids))
            (fill-hash (mail-book-rooms book) (getf data :rooms))
            (setf (mail-book-uidvalidity book) (getf data :uidvalidity)
                  (mail-book-last-uid book) (getf data :last-uid)))
        (error (condition)
          (warn "email: the book at ~a is unreadable (~a); starting a fresh one" path condition))))
    book))

(defmacro with-book ((book) &body body)
  `(bt2:with-lock-held ((mail-book-lock ,book)) ,@body))

(defun save-book (book)
  "Write BOOK whole to its path: a temp file renamed over the old."
  (nlk:when-let (path (mail-book-path book))
    (let ((data (with-book (book)
                  (list :uidvalidity (mail-book-uidvalidity book)
                        :last-uid (mail-book-last-uid book)
                        :threads (hash-alist (mail-book-threads book))
                        :ids (hash-alist (mail-book-ids book))
                        :rooms (hash-alist (mail-book-rooms book)))))
          (temp (make-pathname :type "tmp" :defaults path)))
      (ensure-directories-exist path)
      (with-open-file (out temp :direction :output :if-exists :supersede :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-readably* nil)) (prin1 data out))))
      (rename-file temp path))))

(defun book-thread (book channel)
  (with-book (book) (gethash channel (mail-book-threads book))))

(defun note-message (book channel message-id)
  "Record MESSAGE-ID as CHANNEL's, under its kit id. => the kit id."
  (let ((id (mail-id message-id)))
    (with-book (book)
      (setf (gethash id (mail-book-ids book)) message-id
            (gethash (string-downcase message-id) (mail-book-rooms book)) channel))
    id))

(defun book-room (book message-id)
  (with-book (book) (gethash (string-downcase message-id) (mail-book-rooms book))))

(defun book-message-id (book id)
  (and id (with-book (book) (gethash id (mail-book-ids book)))))

(defun kept-references (ids)
  "IDS without repeats, cut to the root and the latest."
  (let ((ids (remove-duplicates ids :test #'string-equal :from-end t)))
    (if (<= (length ids) +references-kept+)
        ids
        (cons (first ids) (last ids (1- +references-kept+))))))

;;; --- plans and the executor -----------------------------------------------------

(defun noop-plan (label timeout-seconds &optional body)
  "A plan that reaches nothing: what a card, an edit or a delete is in email."
  (rest-plan "NOOP" "none" label nil timeout-seconds
             (list* :id (format nil "n~(~36r~)" (random (expt 2 40) *mail-random*)) body)))

(defun file-octets (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun media-type-of (pathname)
  (let ((type (string-downcase (or (pathname-type pathname) ""))))
    (or (cdr (assoc type '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
                           ("gif" . "image/gif") ("webp" . "image/webp") ("svg" . "image/svg+xml")
                           ("pdf" . "application/pdf") ("txt" . "text/plain") ("md" . "text/markdown")
                           ("csv" . "text/csv") ("json" . "application/json") ("html" . "text/html")
                           ("zip" . "application/zip") ("mp4" . "video/mp4") ("ogg" . "audio/ogg")
                           ("mp3" . "audio/mpeg") ("wav" . "audio/wav") ("eml" . "message/rfc822"))
                    :test #'string=))
        "application/octet-stream")))

(defun file-attachment (file)
  "FILE -- a pathname, a namestring, or a plist naming :path -- as an attachment."
  (let ((pathname (pathname (if (and (consp file) (getf file :path)) (getf file :path) file))))
    (list :filename (file-namestring pathname)
          :media-type (media-type-of pathname)
          :octets (file-octets pathname))))

(defun send-plan (book identity channel text &key reply-to attachments timeout-seconds (label "email_send"))
  "The reply in CHANNEL's thread carrying TEXT and ATTACHMENTS, threaded under
REPLY-TO (a kit message id) or the thread's latest message. IDENTITY is the
bot's (:address :name)."
  (let ((thread (book-thread book channel)))
    (if (null thread)
        (rest-plan "FAIL" "none" label nil timeout-seconds
                   (list :error (format nil "no email thread is known for ~a" channel)))
        (let* ((address (getf identity :address))
               (message-id (new-message-id (domain-of address)))
               (parent (or (book-message-id book reply-to) (getf thread :last)))
               (references (kept-references (append (getf thread :references) (and parent (list parent)))))
               (data (compose-message :from address :from-name (getf identity :name)
                                      :to (getf thread :to) :to-name (getf thread :name)
                                      :subject (reply-subject (getf thread :subject))
                                      :text text :message-id message-id
                                      :in-reply-to parent :references references
                                      :attachments attachments))
               (id (note-message book channel message-id)))
          (with-book (book)
            (setf (getf (gethash channel (mail-book-threads book)) :references)
                  (kept-references (append references (list message-id)))))
          (save-book book)
          (rest-plan "SEND" "smtp" label nil timeout-seconds
                     (list :id id :message-id message-id :to (getf thread :to)
                           :subject (reply-subject (getf thread :subject)) :text text
                           :attachments (mapcar (lambda (file) (getf file :filename)) attachments)
                           :data data))))))

(defun message-plan (book identity target chunk &key reply-to card panel files timeout-seconds
                     &allow-other-keys)
  "The kit's post: a card or a command panel plans to nothing (email has no
message to keep editing), anything else is a reply email."
  (if (or card panel)
      (noop-plan "email_card" timeout-seconds)
      (send-plan book identity (getf target :channel-id) (text-chunk-text chunk)
                 :reply-to reply-to :timeout-seconds timeout-seconds
                 :attachments (mapcar #'file-attachment files))))

(defun file-plan (book identity target pathname &key content reply-to timeout-seconds &allow-other-keys)
  (send-plan book identity (getf target :channel-id) (or content "")
             :reply-to reply-to :timeout-seconds timeout-seconds :label "email_file"
             :attachments (list (file-attachment pathname))))

(defun make-email-executor (&key host port username password from)
  "The executor that sends each SEND plan over SMTP; NOOP plans succeed
without a sound, FAIL plans fail with their reason."
  (make-plan-executor
   :run (lambda (plan)
          (let ((method (request-plan-method plan))
                (body (request-plan-body plan)))
            (cond ((string= method "NOOP") (make-execution :ok-p t :status 200 :body body))
                  ((string= method "SEND")
                   (handler-case
                       (let ((accepted (smtp-send host port username password from
                                                  (list (getf body :to)) (getf body :data))))
                         (make-execution :ok-p t :status 250 :attempts 1
                                         :body (list* :accepted accepted body)))
                     (error (condition)
                       (make-execution :ok-p nil :status 0 :attempts 1
                                       :error (princ-to-string condition)))))
                  (t (make-execution :ok-p nil :status 404 :error (getf body :error))))))))

;;; --- the platform ---------------------------------------------------------------

(defparameter +email-api-primer+
  "Email is open to you as the bot, from eval:
  (nce:send-mail :to \"person@example.com\" :subject \"...\" :text \"...\" &key attachments) => (:status N :body VALUE)
sends one NEW email from the bot's address (ATTACHMENTS a list of file paths) -- an outward effect: send only what was asked. Each line you read is one email: the sender's quoted earlier messages are cut from it, and its files arrive as attachments. The adapter sends your final answer as the reply in the ask's own thread: do not send it again yourself."
  "What the lane contract says about reaching email itself.")

(defun email-where-text (candidate address)
  "Where the conversation is, for the lane contract."
  (format nil "Email: a thread with ~a~@[ (~a)~]~@[, subject \"~a\"~]. The bot's address is ~a; each answer is sent as one reply email in this thread, so write it as an email body: no greeting card, no status chatter."
          (source-field candidate "user_id")
          (source-field candidate "user_name")
          (source-field candidate "subject")
          address))

(defun email-message-id (body)
  (and (consp body) (getf body :id)))

(defun email-platform (&key book identity)
  "The email PLATFORM over BOOK, sending as IDENTITY (:address :name)."
  (make-platform
   :id "email"
   :name "Email"
   :noun "email thread"
   :owner-label "email address"
   :session-prefix "email"
   :contract-section "email-channel"
   :text-limit +email-text-limit+
   ;; Email shows no typing: the beat plans to nothing, and seldom.
   :typing-refresh-ms 600000
   :plan-typing (lambda (target &key timeout-seconds &allow-other-keys)
                  (declare (ignore target))
                  (noop-plan "email_typing" timeout-seconds))
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan book identity target chunk keys))
   :plan-edit (lambda (target message-id text &key timeout-seconds &allow-other-keys)
                (declare (ignore target text))
                (noop-plan "email_edit" timeout-seconds (list :id message-id)))
   :plan-delete (lambda (target message-id &key timeout-seconds &allow-other-keys)
                  (declare (ignore target))
                  (noop-plan "email_delete" timeout-seconds (list :id message-id)))
   :plan-file (lambda (target pathname &rest keys) (apply #'file-plan book identity target pathname keys))
   :message-id-of #'email-message-id
   :address-of (lambda (target message-id) (declare (ignore target)) message-id)
   :strip-mention #'identity
   :where-text (lambda (candidate) (email-where-text candidate (getf identity :address)))
   :api-primer +email-api-primer+
   :seams-primer "  nce:email-handle-message (adapter raw) -- every message the poll reads, as raw octets, before any gate: return fast."))

;;; --- inbound --------------------------------------------------------------------

(defstruct (email-adapter (:copier nil) (:constructor %make-email-adapter))
  host book executor identity allowed
  (require-auth t) auth-server
  ;; the IMAP side
  imap-host imap-port username password mailbox
  (poll-seconds 30) (catch-up-minutes 60) (max-bytes (* 25 1024 1024))
  ;; the test seam: a function of no arguments answering the raw messages that
  ;; arrived since its last call, read instead of IMAP
  inbox)

(defvar *email-adapter* nil
  "The running email adapter, for SEND-MAIL.")

(defun mail-candidate (mail &key channel id reply-to first-p)
  "MAIL as the kit's candidate in room CHANNEL under kit message ID."
  (multiple-value-bind (address name) (from-address mail)
    (let* ((subject (header-text (header mail "subject")))
           (body (strip-quoted-reply (mail-text mail)))
           (text (if (and first-p subject (plusp (length (trim subject))))
                     (format nil "Subject: ~a~%~%~a" (trim subject) body)
                     body)))
      (nlk:json-object
       "text" text
       "attachments" (coerce (loop for file in (mail-attachments mail)
                                   for n from 1
                                   collect (let ((octets (getf file :octets)))
                                             (nlk:json-object
                                              "id" (format nil "~af~d" id n)
                                              "download" (format nil "email:~a" (getf file :filename))
                                              "media_type" (getf file :media-type)
                                              "filename" (getf file :filename)
                                              "size" (length octets)
                                              "fetch" (lambda () octets))))
                             'vector)
       "source" (nlk:json-object
                 "platform" "email"
                 "chat_kind" "channel"
                 "channel_id" channel
                 "user_id" address
                 "user_name" (or name address)
                 "message_id" id
                 "reply_to_message_id" reply-to
                 "subject" subject
                 "addressed" t
                 "is_bot" nil)))))

(defun place-mail (book mail sender)
  "MAIL's room: the room of the newest message it answers that the book knows
and that is SENDER's own thread, else a fresh room for SENDER and its root."
  (or (loop for message-id in (reverse (append (message-ids (header mail "references"))
                                               (message-ids (header mail "in-reply-to"))))
            for channel = (book-room book message-id)
            when (and channel (equal sender (getf (book-thread book channel) :to)))
              return channel)
      (thread-channel sender (thread-root mail))))

(defun email-handle-message (adapter raw)
  "One raw message the poll read: gated, placed, recorded, and handed to the
kit. => :handed, or a string naming why it was dropped."
  (let* ((mail (parse-message raw))
         (book (email-adapter-book adapter))
         (self (getf (email-adapter-identity adapter) :address))
         (sender (from-address mail))
         (message-id (own-message-id mail)))
    (flet ((drop (reason)
             (warn "email: not answering ~a from ~a: ~a" message-id (or sender "nobody") reason)
             (return-from email-handle-message reason)))
      (unless (addressed-to-p mail self) (drop "not addressed to the bot"))
      (nlk:when-let (machine (automated-p mail :self self)) (drop machine))
      (unless (sender-allowed-p sender (email-adapter-allowed adapter)) (drop "sender not allowed"))
      (when (email-adapter-require-auth adapter)
        (multiple-value-bind (ok how) (authenticated-p mail :authserv (email-adapter-auth-server adapter))
          (unless ok (drop (format nil "sender not authenticated (~a)" how)))))
      (when (book-room book message-id) (drop "already read"))
      (let* ((channel (place-mail book mail sender))
             (first-p (null (book-thread book channel)))
             (parent (first (message-ids (header mail "in-reply-to"))))
             (reply-to (and parent (book-room book parent) (mail-id parent)))
             (id (note-message book channel message-id)))
        (with-book (book)
          (let ((thread (gethash channel (mail-book-threads book))))
            (setf (gethash channel (mail-book-threads book))
                  (list :to sender
                        :name (nth-value 1 (from-address mail))
                        :subject (or (getf thread :subject) (header-text (header mail "subject")))
                        :references (kept-references (append (message-ids (header mail "references"))
                                                             (getf thread :references)
                                                             (list message-id)))
                        :last message-id))))
        (save-book book)
        (handle-candidate (email-adapter-host adapter)
                          (mail-candidate mail :channel channel :id id :reply-to reply-to :first-p first-p))
        :handed))))

;;; --- the lap --------------------------------------------------------------------

(defun read-new-mail (adapter wire)
  "Hand the kit every message that arrived since the last read; the first
read of a mailbox only marks where it stands, so nothing old is answered."
  (let ((book (email-adapter-book adapter))
        (self (getf (email-adapter-identity adapter) :address)))
    (multiple-value-bind (validity next) (imap-examine wire (email-adapter-mailbox adapter))
      (let ((next (or next (1+ (reduce #'max (imap-search-new wire 0) :initial-value 0)))))
        (cond
          ((or (null (mail-book-last-uid book)) (not (eql validity (mail-book-uidvalidity book))))
           (with-book (book)
             (setf (mail-book-uidvalidity book) validity
                   (mail-book-last-uid book) (1- next)))
           (save-book book))
          (t
           (let* ((uids (imap-search-new wire (mail-book-last-uid book) self))
                  (facts (imap-sizes wire uids))
                  (oldest (- (get-universal-time) (* 60 (email-adapter-catch-up-minutes adapter)))))
             (dolist (uid uids)
               (destructuring-bind (&optional size arrived) (rest (assoc uid facts))
                 (cond ((and size (> size (email-adapter-max-bytes adapter)))
                        (warn "email: message ~d is ~d bytes, over the limit; not read" uid size))
                       ((and arrived (< arrived oldest))
                        (warn "email: message ~d arrived before the catch-up window; not answered" uid))
                       (t (nlk:when-let (octets (imap-fetch wire uid))
                            (handler-case (email-handle-message adapter octets)
                              (error (condition)
                                (warn "email: message ~d signalled: ~a" uid condition)))))))
               (with-book (book) (setf (mail-book-last-uid book) uid))
               (save-book book)))
           (when (> (1- next) (mail-book-last-uid book))
             (with-book (book) (setf (mail-book-last-uid book) (1- next)))
             (save-book book))))))))

(defun wait-or-stop (seconds stop-p)
  (loop repeat (ceiling (* 2 seconds)) until (funcall stop-p) do (sleep 0.5))
  (funcall stop-p))

(defun email-poll-lap (adapter stop-p)
  "One supervised IMAP session: log in, read what is new every poll_seconds
until the session fails, and come back for another."
  ;; Returns :stop / :fatal / seconds-to-wait per the kit supervisor contract.
  (when (funcall stop-p) (return-from email-poll-lap :stop))
  (nlk:when-let (inbox (email-adapter-inbox adapter))
    (set-channel-status "email" :state :running :connected t :detail nil)
    (loop
      (dolist (raw (funcall inbox))
        (handler-case (email-handle-message adapter raw)
          (error (condition) (warn "email: a message signalled: ~a" condition))))
      (when (wait-or-stop 0.2 stop-p) (return-from email-poll-lap :stop))))
  (let ((wire nil))
    (unwind-protect
         (handler-case
             (progn
               (setf wire (imap-open (email-adapter-imap-host adapter) (email-adapter-imap-port adapter)
                                     (email-adapter-username adapter) (email-adapter-password adapter)))
               (set-channel-status "email" :state :running :connected t :detail nil)
               (loop
                 (read-new-mail adapter wire)
                 (when (wait-or-stop (email-adapter-poll-seconds adapter) stop-p)
                   (return :stop))))
           (error (condition)
             (let ((detail (princ-to-string condition)))
               (cond ((search "LOGIN refused" detail)
                      (set-channel-status "email" :state :degraded :connected nil :detail detail)
                      (warn "email: ~a" detail)
                      :fatal)
                     (t (set-channel-status "email" :connected nil :detail detail)
                        (warn "email: ~a; reconnecting in 30s" detail)
                        30.0)))))
      (imap-close wire))))

;;; --- the model's verb -----------------------------------------------------------

(defun send-mail (&key to subject text attachments)
  "Send one new email from the bot's address to TO: SUBJECT, TEXT, and
ATTACHMENTS (file paths). => (:status N :body VALUE), the shape NCK:CALL
prints."
  (let ((adapter *email-adapter*))
    (unless adapter (error "the email channel is not running"))
    (multiple-value-bind (address) (parse-address to)
      (unless address (error "~s names no email address" to))
      (let* ((identity (email-adapter-identity adapter))
             (message-id (new-message-id (domain-of (getf identity :address))))
             (plan (rest-plan "SEND" "smtp" "email_send_mail" nil 60
                              (list :id (mail-id message-id) :message-id message-id :to address
                                    :subject subject
                                    :data (compose-message :from (getf identity :address)
                                                           :from-name (getf identity :name)
                                                           :to address :subject (or subject "")
                                                           :text text :message-id message-id
                                                           :attachments (mapcar #'file-attachment attachments)))))
             (result (execute-plan (email-adapter-executor adapter) plan)))
        (if (execution-ok-p result)
            (list :status 250 :body (list :sent message-id :to address :accepted (getf (execution-body result) :accepted)))
            (list :status 0 :error (execution-error result)))))))

;;; --- the channel entry ----------------------------------------------------------

(defun base-login (address)
  "ADDRESS without its +tag: the mailbox a plus address delivers to."
  (ppcre:regex-replace "\\+[^@]*@" address "@"))

(defun start-channel (section &key executor inbox)
  "Start the email lane from its channels.email config SECTION."
  ;; Returns a stop thunk. EXECUTOR replaces the SMTP executor and INBOX, a
  ;; function answering the raw messages that arrived since its last call,
  ;; replaces IMAP: the scripted test seams.
  ;;
  ;; Fail-closed: refuses unless allowed_senders (or owner) names someone;
  ;; every message from anyone else is read and dropped before the kit sees
  ;; it, and so is one whose sender the receiving server did not vouch for.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "email")) section))
         (address (string-downcase (or (config-string section "address" nil)
                                       (error "channels.email.address is unset"))))
         (domain (domain-of address))
         (username (config-string section "username" (base-login address)))
         (password (and (not (and executor inbox)) (resolve-channel-secret section "password")))
         (configured (mapcar #'string-downcase (getf settings :allowed-senders)))
         (owners (mapcar #'string-downcase
                         (resolve-owners "email" (getf settings :owner) configured "allowed_senders")))
         (allowed (union configured owners :test #'string=))
         (identity (list :address address :name (config-string section "from_name" "Nodecode")))
         (book (make-mail-book :path (or (nlk:when-let (path (config-string section "state_file" nil))
                                           (pathname (nlk:expand-home path)))
                                         (nlk:home "channels/email-state.sexp"))))
         (sender (or executor
                     (make-email-executor :host (config-string section "smtp_host" (format nil "smtp.~a" domain))
                                          :port (config-integer section "smtp_port" 465 :min 1)
                                          :username username :password password :from address))))
    (require-non-empty-allowlist "email" "allowed_senders" allowed)
    (let* ((host (make-host-from-section
                  section (email-platform :book book :identity identity)
                  :executor sender
                  :owners owners
                  :soul-path (soul-path section)
                  :request-timeout 60
                  :policy (make-inbound-policy :allowed-users allowed
                                               :require-mention nil
                                               :allow-bots :none
                                               :channel-match :exact)))
           (adapter (%make-email-adapter
                     :host host :book book :executor sender :identity identity :allowed allowed
                     :require-auth (config-boolean section "require_auth" t)
                     :auth-server (config-string section "auth_server" nil)
                     :imap-host (config-string section "imap_host" (format nil "imap.~a" domain))
                     :imap-port (config-integer section "imap_port" 993 :min 1)
                     :username username :password password
                     :mailbox (config-string section "mailbox" "INBOX")
                     :poll-seconds (config-integer section "poll_seconds" 30 :min 5)
                     :catch-up-minutes (config-integer section "catch_up_minutes" 60 :min 0)
                     :max-bytes (* 1024 1024 (config-integer section "max_megabytes" 25 :min 1))
                     :inbox inbox)))
      (run-host host "poll" (lambda (stop-p) (email-poll-lap adapter stop-p))
                :on-start (lambda () (setf *email-adapter* adapter))
                :on-stop (lambda ()
                           (when (eq *email-adapter* adapter)
                             (setf *email-adapter* nil)))))))
