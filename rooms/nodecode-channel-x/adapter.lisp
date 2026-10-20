;;;; adapter.lisp --- the X adapter: the platform, the poll lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the status line, the
;;;; answer, the write-back -- is the kit's (host.lisp, room.lisp). This file
;;;; is what X contributes: how a post is spelled (rest.lisp, bound into a
;;;; PLATFORM), where the conversation is, and the poll lap that turns new
;;;; mentions into candidates.
;;;;
;;;; Thread topology (kit rule: no network I/O on a thread that is not ours):
;;;;
;;;;   - channel-x-poll (supervised lap): every mention since the last read,
;;;;     oldest first, through X-HANDLE-MENTION. X has no stream the web
;;;;     client can hold, so the lap is the only way in; the id the next read
;;;;     goes on from is kept on disk after each batch.
;;;;   - the host's :FRAME fold and channel-x-deliver pool: the kit's.
;;;;
;;;; The first start reads where the timeline stands and answers nothing said
;;;; before it; a later start goes on from the kept id, so a mention made
;;;; while the organism was down is answered when it comes back.

(in-package #:nodecode-channel-x)

(nlk:access (host channel-host) (bridge bridge) (book x-book)
            (result execution) (chunk text-chunk) (plan request-plan))

(defstruct (x-adapter (:copier nil) (:constructor %make-x-adapter))
  (host nil)
  (bridge nil)
  (book nil)
  (bot-handle "" :type string)
  (bot-id "" :type string)
  ;; The newest mention id answered through, and the file it is kept in.
  (cursor nil :type (or null integer))
  (cursor-path nil)
  (poll-seconds 60 :type real))

(nlk:access (adapter x-adapter))

(defmethod print-object ((adapter x-adapter) stream)
  (print-unreadable-object (adapter stream :type t :identity t)
    (format stream "@~a" (x-adapter-bot-handle adapter))))

(defvar *x-adapter* nil
  "The live X adapter, or NIL when the lane is stopped.")

(defvar *poll-floor* 15
  "The fewest seconds between two reads of the mentions: X watches how often
an account reads. Tests bind it lower.")

(defvar *cursor-state-directory* nil
  "Where the mention id is kept; NIL is the home's channels/x/. Tests bind a
scratch folder.")

;;; --- what X contributes to the host ------------------------------------------

(defparameter +x-api-primer+
  "X is open to you as the bot's own account, over a logged-in web session, from eval:
  (ncx:request \"mentions\" :since \"0\" :limit 10) reads the newest mentions; (ncx:request \"tweet\" :id \"<id>\") reads one post; (ncx:request \"reply\" :text \"...\" :reply_to \"<id>\" :image \"<path>\") posts a reply -- an outward effect: post only what was asked. (ncx:request \"delete\" :id \"<id>\") deletes one of the bot's own posts. A post is named by its id, the m<id> handle on each line, and a person by their @handle. X draws no Markdown and allows 280 characters; answers are posted as plain text, and a long answer goes on as numbered replies under the ask. The adapter posts your final answer: do not post it again yourself."
  "What the lane contract says about reaching X itself.")

(defun x-where-text (candidate)
  "Where the conversation is, for the lane contract."
  (format nil "a public X conversation ~a. The bot answers as @~a, in reply under the ask. ~
Answers are posted as plain text, 280 characters to a post, a long one as numbered replies."
          (or (source-field candidate "channel_id") "unknown")
          (if *x-adapter* (x-adapter-bot-handle *x-adapter*) "the bot")))

(defun x-platform (&key book bot-handle)
  "The X PLATFORM over BOOK: replies through the bridge, 270 characters to a
post, no card (X cannot edit one), pictures, numbered parts, no reactions and
no typing (X shows neither)."
  (make-platform
   :id "x"
   :name "X"
   :noun "conversation"
   :owner-label "X account id"
   :session-prefix "x"
   :contract-section "x-channel"
   :text-limit +x-text-limit+
   :typing-refresh-ms 60000
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan book target chunk keys))
   :plan-edit (lambda (target id text &rest keys) (apply #'edit-plan book target id text keys))
   :plan-delete (lambda (target id &rest keys) (apply #'delete-plan book target id keys))
   :plan-typing (lambda (target &rest keys) (apply #'typing-plan book target keys))
   :plan-file (lambda (target path &rest keys) (apply #'file-plan book target path keys))
   :message-id-of #'x-message-id
   :strip-mention (lambda (text) (nlk:trimmed (ppcre:regex-replace
                                             (format nil "(?i)(^|[^\\w@])@~a(?!\\w)"
                                                     (ppcre:quote-meta-chars (or bot-handle "")))
                                             (or text "") "\\1")))
   :answer-body (lambda (text footer) (declare (ignore footer)) text)
   :where-text #'x-where-text
   :choices nil
   :api-primer +x-api-primer+
   :seams-primer "  ncx:x-handle-mention (adapter tweet) -- every mention the lap reads (asks and replies under the bot's posts), on the lap thread: return fast. tweet is the bridge's JSON object (\"id\", \"text\", \"author_id\", \"author\", \"in_reply_to\", \"conversation_id\")."))

;;; --- the lap learns ----------------------------------------------------------------

(defun lap-call (adapter plan)
  (execute-plan (host-executor adapter.host) plan))

(defun cursor-path (bot-handle)
  "The file the mention id last answered through is kept in."
  (let ((name (format nil "~a.cursor" (normalize-x-handle bot-handle))))
    (if *cursor-state-directory*
        (merge-pathnames name *cursor-state-directory*)
        (nlk:home (format nil "channels/x/~a" name)))))

(defun keep-cursor (adapter)
  (when (and adapter.cursor-path adapter.cursor)
    (handler-case (nlk:write-file-atomically adapter.cursor-path (princ-to-string adapter.cursor))
      (error (condition) (warn "x: the mention id was not kept: ~a" condition)))))

(defun tweet-id (tweet)
  "TWEET's id as an integer, 0 when it has none."
  (let ((id (nlk:json-value tweet :string "id")))
    (if (and id (every #'digit-char-p id)) (parse-integer id) 0)))

(defun tweet-objects (result)
  "The tweets a mentions answer carries, oldest first."
  (nlk:json-array result.body "tweets"))

(defun x-parent-bot-p (adapter)
  "Whether a tweet id names one of the bot's own posts, asked of X once per
id and kept in the book: a reply under the bot's post is said to it even when
its words carry no @handle."
  (lambda (id)
    (or (bot-tweet-p adapter.book id)
        (multiple-value-bind (known found) (gethash id (x-book-mine adapter.book))
          (if found
              known
              (let ((known
                      (let ((result (lap-call adapter (bridge-plan "tweet" "read_post" 30 "id" id))))
                        (and result.ok-p
                             (equal adapter.bot-id (nlk:json-value result.body :string "tweet" "author_id"))
                             t))))
                (book-note (x-book-mine adapter.book) id known)
                known))))))

(defun x-handle-mention (adapter tweet)
  "One TWEET the lap read: a mention of the bot, or a reply under one of its
posts, handed to the host as the shared candidate. => the candidate, or NIL."
  ;; Exported as the seam a layer advises -- (hook 'ncx:x-handle-mention key fn)
  ;; sees every mention the lap reads -- with the lap's own rule: return fast.
  (let ((candidate (x-mention-candidate tweet
                                        :bot-id adapter.bot-id
                                        :bot-handle adapter.bot-handle
                                        :bot-tweet-p (x-parent-bot-p adapter))))
    (when candidate
      (handle-candidate adapter.host candidate)
      candidate)))

(defparameter +batch+ 40
  "Mentions one read takes; a full batch is read on from at once.")

(defun x-poll-lap (adapter stop-p)
  "One read of the mentions since the last."
  ;; Returns the supervisor verdict: seconds to wait before the next lap, or
  ;; :FATAL for a session X refuses, since no wait mends that.
  (when (funcall stop-p) (return-from x-poll-lap :stop))
  (let* ((first-p (null adapter.cursor))
         (result (lap-call adapter (mentions-plan (or adapter.cursor 0) :limit +batch+))))
    (when (funcall stop-p) (return-from x-poll-lap :stop))
    (cond
      ((member result.status '(401 403))
       (set-channel-status "x" :state :degraded :connected nil
                                :detail "X refused the session cookie: sign the bot's account in again and refresh token_file")
       :fatal)
      ((not result.ok-p)
       (set-channel-status "x" :connected nil :detail result.error)
       (warn "x read failed: ~a" result.error)
       30.0)
      (t
       (set-channel-status "x" :state :running :connected t)
       (let ((tweets (tweet-objects result)))
         (if first-p
             (setf adapter.cursor (loop for tweet across tweets maximize (tweet-id tweet)))
             (loop for tweet across tweets
                   ;; One boundary per tweet, outside the seam, so advice that
                   ;; signals costs that mention and not the read.
                   do (handler-case (x-handle-mention adapter tweet)
                        (error (condition) (warn "x mention handling signalled: ~a" condition)))
                      (setf adapter.cursor (max (or adapter.cursor 0) (tweet-id tweet)))))
         (keep-cursor adapter)
         (if (>= (length tweets) +batch+) 1.0 adapter.poll-seconds))))))

;;; --- the door ----------------------------------------------------------------------

(defun request (op &rest arguments &key &allow-other-keys
                &aux (host (and *x-adapter* (x-adapter-host *x-adapter*))))
  "One bridge OP as the bot's account, ARGUMENTS its members (:reply_to
\"123\" for reply_to), answered as (:status N :body VALUE [:error TEXT])."
  (unless (and host (host-executor host))
    (error "the x channel is not running"))
  (let* ((pairs (loop for (key value) on arguments by #'cddr
                      append (list (string-downcase (symbol-name key)) value)))
         (result (execute-plan (host-executor host)
                               (apply #'bridge-plan op "request" 60 pairs))))
    (list* :status result.status :body result.body
           (and result.error (list :error result.error)))))

(defun section-with-defaults (section)
  "SECTION as the host reads it, with X's own terms: no answer streamed in
edits, which X cannot make, and the card, never posted, ticked seldom."
  (let ((copy (alexandria:copy-hash-table section)))
    (setf (gethash "stream" copy) nil)
    (unless (nth-value 1 (gethash "status_update_ms" copy))
      (setf (gethash "status_update_ms" copy) 60000))
    copy))

(defun account-id (executor handle)
  "The account id of @HANDLE, asked of X through EXECUTOR, or a refusal."
  (let ((result (execute-plan executor (bridge-plan "user_id" "user_id" 60 "handle" handle))))
    (or (and result.ok-p (nlk:json-value result.body :string "user_id"))
        (config-error "channels.x: X knows no account @~a (~a)" handle result.error))))

(defun own-id (executor)
  "The account id the session cookie is signed in as."
  (let ((result (execute-plan executor (bridge-plan "me" "me" 60))))
    (or (and result.ok-p (nlk:json-value result.body :string "user_id"))
        (config-error "channels.x: X did not take the session cookie (~a)" result.error))))

(defun start-channel (section &key executor)
  "Start the X lane from its channels.x config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides the bridge child -- the
  ;; scripted test seam -- and is wrapped in X's terms like the live one.
  ;;
  ;; Fail-closed: refuses unless allowed_users names someone, and unless the
  ;; cookie is signed in as the account handle names, so a lane never answers
  ;; as an account the operator did not mean (his own, above all).
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "x")) section))
         (handle (normalize-x-handle (getf settings :handle)))
         (cookie (resolve-channel-secret section "token"))
         (book (make-x-book :daily-posts (or (getf settings :daily-posts) 50)
                            :max-posts (max 1 (or (getf settings :max-posts) 4))))
         (bridge (unless executor
                   (start-bridge (or (getf settings :bridge-command) +default-bridge-command+) cookie)))
         (started nil))
    (unwind-protect
         (let* ((bot (wrap-x-executor (or executor (bridge-executor bridge)) :book book))
                (self (own-id bot))
                (named (if (plusp (length handle))
                           (account-id bot handle)
                           (config-error "channels.x: handle is not set -- the bot account's @handle"))))
           (unless (equal self named)
             (config-error "channels.x: token_file is signed in as account ~a, not @~a (~a): give the bot its own account's cookie"
                           self handle named))
           (let* ((configured (mapcar (lambda (each) (account-id bot (normalize-x-handle each)))
                                      (getf settings :allowed-users)))
                  (owners (resolve-owners "x" (mapcar (lambda (each) (account-id bot (normalize-x-handle each)))
                                                      (getf settings :owner))
                                          configured "allowed_users"))
                  (allowed (and configured (union configured owners :test #'string=))))
             (require-non-empty-allowlist "x" "allowed_users" allowed)
             (let* ((host (make-host-from-section
                           (section-with-defaults section)
                           (x-platform :book book :bot-handle handle)
                           :executor bot
                           :owners owners
                           :soul-path (soul-path section)
                           :request-timeout 60
                           :policy (x-inbound-policy :allowed-users allowed :bot-handle handle
                                                     :ignored-conversations (getf settings :ignored-conversations))))
                    (path (cursor-path handle))
                    (adapter (%make-x-adapter
                              :host host :bridge bridge :book book :bot-handle handle :bot-id self
                              :cursor (let ((kept (and (probe-file path) (nlk:trimmed (uiop:read-file-string path)))))
                                        (and (plusp (length kept)) (every #'digit-char-p kept) (parse-integer kept)))
                              :cursor-path path
                              :poll-seconds (max *poll-floor* (or (getf settings :poll-seconds) 60))))
                    (stop (run-host host "poll" (lambda (stop-p) (x-poll-lap adapter stop-p))
                                    :on-start (lambda () (setf *x-adapter* adapter))
                                    :on-stop (lambda ()
                                               (when (eq *x-adapter* adapter)
                                                 (setf *x-adapter* nil))))))
               (setf started t)
               (lambda ()
                 (unwind-protect (funcall stop)
                   (stop-bridge bridge))))))
      (unless started (stop-bridge bridge)))))
