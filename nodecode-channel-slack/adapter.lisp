;;;; adapter.lisp --- the Slack adapter: the platform, the socket lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the status line, the
;;;; answer, the write-back -- is the kit's (host.lisp, room.lisp). This file
;;;; is what Slack contributes: how a message, an edit, a delete and a status
;;;; beat are spelled (rest.lisp, bound into a PLATFORM), how a mention is
;;;; written, where the channel is, and the Socket Mode lap that turns
;;;; envelopes into candidates.
;;;;
;;;; Thread topology (kit rule: no network I/O on a thread that is not ours):
;;;;   - the wsd reader callback: parse + enqueue onto the inbound queue.
;;;;   - channel-slack-socket (supervised lap): opens the socket, drains the
;;;;     queue, acknowledges each envelope, admits messages through the host,
;;;;     pings to find a dead link. A person's display name is read here, once
;;;;     per person, after the envelope is acknowledged.
;;;;   - the host's :FRAME fold and channel-slack-deliver pool: the kit's.

(in-package #:nodecode-channel-slack)

(nlk:access (host channel-host) (result execution))

(defparameter +ping-interval-ms+ 15000
  "How often the lap pings Slack.")

(defparameter +dead-link-ms+ 45000
  "How long a socket may go without a frame or a pong before the lap takes
it for dead and opens another.")

(defstruct (slack-adapter (:copier nil) (:constructor %make-slack-adapter))
  (host nil)
  ;; apps.connections.open speaks with the app-level token, everything else
  ;; with the bot's.
  (app-executor nil)
  (bot-token "" :type string)
  (bot-user-id nil :type (or null string))
  (threads (make-thread-book))
  ;; user id -> display name, NIL for one Slack would not name; the lap's.
  (names (make-hash-table :test #'equal))
  ;; channels.slack.debug_reconnects: Slack moves the socket every ~6 minutes.
  (debug-reconnects-p nil :type boolean)
  ;; Liveness, set by the reader's pong callback and read by the lap.
  (heard-ms 0 :type integer))

(nlk:access (adapter slack-adapter))

(defvar *slack-adapter* nil
  "The live Slack adapter, or NIL when the lane is stopped.")

;;; --- what Slack contributes to the host -----------------------------------------------

;;; Data, so an operator's layer can reword it; the ids above it are the
;;; adapter's.
(defparameter +slack-api-primer+
  "Slack's Web API is open to you as the bot, from eval:
  (ncs:request METHOD PATH &key body headers timeout) => (:status N :body VALUE)
PATH is any Web API method: a read is a GET with its arguments in the query, (ncs:request \"GET\" \"/conversations.history?channel=<channel>&limit=20\"); a write is a POST with a keyword plist body, (ncs:request \"POST\" \"/reactions.add\" :body (:channel \"<channel>\" :timestamp \"<ts>\" :name \"thumbsup\")). Objects come back as keyword plists, arrays as vectors. Slack answers a refusal with status 200 too: then :error says what Slack refused and why (missing_scope names the scope the app lacks). A message is named by its channel and its ts -- the m... handle on each line is that ts -- and a person is mentioned as <@U...>. The adapter posts your final answer in the ask's thread: do not post it again yourself."
  "What the lane contract says about reaching Slack itself.")

(defun slack-where-text (candidate bot-user-id &aux (target (channel-target candidate)))
  "Where the channel is and who the bot is, for the lane contract: the
conversation, its kind, the bot's own mention -- whichever are known."
  (format nil "Slack ~a ~a~@[ in workspace ~a~].~@[ The bot is <@~a>.~]~
~:[~; Answers go in the thread under the ask.~]"
          (source-field candidate "chat_kind")
          (or (getf target :channel-id) "unknown")
          (source-field candidate "workspace_id")
          bot-user-id
          (not (dm-channel-p (getf target :channel-id)))))

(defun slack-platform (&key bot-user-id (threads (make-thread-book)))
  "The Slack PLATFORM: Web API plans, 12,000-character markdown blocks, the
<@U...> mention, per-channel message ts (so an address carries the channel),
replies threaded under their ask, a status beat Slack shows for two minutes,
reactions by name, and a slash command answered through its response_url.
THREADS is the book the plans and the lap share."
  ;; Slack has no command menu an app may write (the /nodecode command is the
  ;; manifest's), no thread to open (a reply is the thread), and no file here
  ;; yet: those slots stay empty.
  (make-platform
   :id "slack"
   :name "Slack"
   :noun "channel"
   :owner-label "Slack user id"
   :session-prefix "slack"
   :contract-section "slack-channel"
   :text-limit +slack-text-limit+
   :typing-refresh-ms 20000
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan threads target chunk keys))
   :plan-edit #'edit-plan
   :plan-delete #'delete-plan
   :plan-typing (lambda (target &rest keys) (apply #'typing-plan threads target keys))
   :plan-reaction #'reaction-plans
   :plan-respond #'respond-plan
   :message-id-of #'slack-message-id
   :address-of #'slack-address
   :strip-mention (lambda (text) (slack-strip-mention text bot-user-id))
   :where-text (lambda (candidate) (slack-where-text candidate bot-user-id))
   :api-primer +slack-api-primer+
   :seams-primer "  ncs:slack-handle-event (adapter event &optional team-id) -- every Events API event the app subscribes to, on the socket thread, after Slack has its acknowledgement: return fast. event is the hash table Slack sent (\"type\" names it)."))

;;; --- ingress (socket lap thread) -----------------------------------------------------

(defun slack-user-name (adapter user-id)
  "USER-ID's display name, asked of Slack once per person (users.info), or
NIL when Slack would not say."
  (when user-id
    (multiple-value-bind (name known) (gethash user-id adapter.names)
      (if known
          name
          (let* ((result (execute-plan (host-executor adapter.host)
                                       (rest-plan "GET" (format nil "/users.info?user=~a" user-id)
                                                  "user_info" nil 10)))
                 (user (and result.ok-p (nlk:json-value result.body :object "user"))))
            (setf (gethash user-id adapter.names)
                  (or (nlk:json-value user :text "profile" "display_name")
                      (nlk:json-value user :text "profile" "real_name")
                      (nlk:json-value user :text "name"))))))))

(defun slack-file-fetcher (token url)
  "A thunk that returns the bytes at URL, fetched with the bot TOKEN, or
signals with the whole truth in its message -- never the token."
  ;; Built when the message is admitted, on the lap thread; called when the
  ;; ask's turn is prepared, on the kit's delivery pool, so no download ever
  ;; parks the socket. The ceiling a file may take is the kit's, read off the
  ;; size the entry declares before this runs.
  (lambda ()
    (multiple-value-bind (body status)
        (nlk:with-handlers ((error () (error "the download from Slack failed")))
          (nlk:http :get url :headers (list (cons "authorization" (format nil "Bearer ~a" token)))
                             :binary t :timeout 30))
      (unless (eql 200 status)
        (error "the download from Slack failed with status ~a" status))
      body)))

(defun attach-slack-file-fetchers (adapter candidate)
  "CANDIDATE's attachment entries each gain the thunk that fetches the file
with the bot token."
  (loop for attachment across (nlk:json-array candidate "attachments")
        for url = (nlk:json-value attachment :string "download")
        when url
          do (setf (gethash "fetch" attachment) (slack-file-fetcher adapter.bot-token url)))
  candidate)

(defun slack-handle-event (adapter event &optional team-id)
  "One Events API EVENT, as Slack sent it, on the socket lap: a message
maps to the shared candidate and goes through the host; every other event
falls through."
  ;; Exported as the seam a layer advises -- (hook 'ncs:slack-handle-event key
  ;; fn) sees reactions, joins, whatever the app subscribes to -- with the
  ;; lap's own rule: return fast, this thread is the one that acknowledges.
  ;; Every message in a thread is noted in the thread book first, so a reply
  ;; to it lands in its thread. Returns the candidate admitted, or NIL.
  (let ((channel (nlk:json-value event :string "channel"))
        (threads adapter.threads))
    (note-thread-message threads channel (nlk:json-value event :string "ts")
                         (nlk:json-value event :string "thread_ts"))
    (nlk:when-let (candidate (slack-message-candidate
                              event :team-id team-id
                                    :bot-user-id adapter.bot-user-id
                                    :bot-thread-p (lambda (channel root)
                                                    (bot-thread-p threads channel root))))
      ;; A person's name is asked for only for a line a person wrote.
      (let ((user (source-field candidate "user_id")))
        (unless (or (candidate-bot-p candidate) (equal user adapter.bot-user-id))
          (setf (gethash "user_name" (gethash "source" candidate))
                (slack-user-name adapter user))))
      (handle-candidate adapter.host (attach-slack-file-fetchers adapter candidate))
      candidate)))

(defun candidate-bot-p (candidate)
  (eq t (gethash "is_bot" (gethash "source" candidate))))

(defun slack-handle-envelope (adapter type payload)
  "One acknowledged Socket Mode envelope: an Events API callback goes to
SLACK-HANDLE-EVENT, a slash command runs as the command line it spells, a
button press is relayed to the kit, which acts on it."
  (let ((host adapter.host))
    (cond
      ((equal type "events_api")
       (nlk:when-let (event (nlk:json-value payload :object "event"))
         (slack-handle-event adapter event (nlk:json-value payload :string "team_id"))))
      ((equal type "slash_commands")
       (nlk:when-let (candidate (slack-command-candidate payload))
         (handle-candidate host candidate)
         candidate))
      ((equal type "interactive")
       (nlk:when-let (press (slack-control-press payload))
         (control-pressed host press)
         press)))))

;;; --- the socket lap ----------------------------------------------------------------

(defun open-socket-url (adapter)
  "(values URL FATAL-P DETAIL): a one-use wss URL from apps.connections.open,
or why not -- FATAL-P when the token itself is refused, since no wait mends
that."
  (let* ((result (execute-plan adapter.app-executor
                               (api-plan "apps.connections.open" nil "connections_open"
                                         :timeout-seconds 15)))
         (url (and result.ok-p (nlk:json-value result.body :string "url"))))
    (cond (url (values (if adapter.debug-reconnects-p
                           (format nil "~a&debug_reconnects=true" url)
                           url)
                       nil nil))
          (t (values nil
                     (and (some (lambda (code) (search code (or result.error "")))
                                '("invalid_auth" "not_authed" "not_allowed_token_type"
                                  "token_revoked" "account_inactive"))
                          t)
                     (probe-failure result))))))

(defun slack-socket-lap (adapter stop-p)
  "One supervised Socket Mode connection: open, read until Slack moves the
socket or the link dies, and come back for another."
  ;; Returns :stop / :fatal / seconds-to-wait per the kit supervisor contract.
  (when (funcall stop-p) (return-from slack-socket-lap :stop))
  (multiple-value-bind (url fatal-p detail) (open-socket-url adapter)
    (unless url
      (set-channel-status "slack" :state (if fatal-p :degraded :running)
                                  :connected nil :detail detail)
      (warn "slack: no socket: ~a" detail)
      (return-from slack-socket-lap (if fatal-p :fatal 5.0)))
    (let ((inbound (make-work-queue "channel-slack-inbound" :cap 1024))
          (ws (wsd:make-client url))
          (pings 0)
          (pinged-ms (now-ms)))
      (wsd:on :message ws
              (lambda (message &aux (frame (ignore-errors (nlk:decode-json message))))
                (setf adapter.heard-ms (now-ms))
                (when (hash-table-p frame)
                  (queue-push inbound (list :frame frame)))))
      (wsd:on :close ws
              (lambda (&key code reason)
                (declare (ignore reason))
                (queue-push inbound (list :closed (or code 1000)))))
      (unwind-protect
           (block lap
             (wsd:start-connection ws)
             (setf adapter.heard-ms (now-ms))
             (loop
               (when (funcall stop-p) (return-from lap :stop))
               ;; A socket that answers neither frames nor pings is gone, even
               ;; when no close ever arrives to say so.
               (when (> (- (now-ms) adapter.heard-ms) +dead-link-ms+)
                 (set-channel-status "slack" :connected nil :detail "the socket went quiet")
                 (return-from lap 1.0))
               (when (>= (- (now-ms) pinged-ms) +ping-interval-ms+)
                 (setf pinged-ms (now-ms))
                 (ignore-errors
                  (wsd:send-ping ws (sb-ext:string-to-octets (format nil "~d" (incf pings)))
                                 (lambda () (setf adapter.heard-ms (now-ms))))))
               (multiple-value-bind (item found) (queue-pop inbound 0.5)
                 (when found
                   (ecase (first item)
                     (:closed
                      (set-channel-status "slack" :connected nil)
                      (return-from lap 1.0))
                     (:frame
                      (dolist (action (read-socket-frame (second item)))
                        (ecase (first action)
                          (:hello
                           (set-channel-status "slack" :state :running :connected t :detail nil))
                          (:ack
                           (wsd:send ws (nlk:encode-json-object (ack-frame (second action)))))
                          (:envelope
                           ;; One boundary per envelope, outside the seam, so
                           ;; advice that signals costs that envelope and not
                           ;; the socket.
                           (handler-case (slack-handle-envelope adapter (second action)
                                                                (third action))
                             (error (condition)
                               (warn "slack envelope handling signalled: ~a" condition))))
                          (:reconnect (return-from lap 0.1))
                          (:fatal
                           (set-channel-status "slack" :state :degraded :connected nil
                                                       :detail (second action))
                           (return-from lap :fatal))))))))))
        ;; Runs on the lap thread while wsd's reader may be parked in SSL_read
        ;; on this wss: sever the transport, never WSD:CLOSE-CONNECTION
        ;; (cross-thread SSL_free; see the kit's sever).
        (ignore-errors (sever-ws-transport ws))))))

;;; --- the door ----------------------------------------------------------------------

(defun request (method path &rest arguments &key body headers timeout retry
                &aux (host (and *slack-adapter* (slack-adapter-host *slack-adapter*))))
  "One Slack Web API call as the bot: METHOD PATH, answered as
(:status N :body VALUE [:error TEXT]) -- the shape the eval cell prints
legibly, see NCK:CALL."
  ;; PATH is the method, "/chat.postMessage", with a read's arguments in its
  ;; query string; it joins onto the configured api_base. BODY is a keyword
  ;; plist (a JSON object) or an NLK:JSON-OBJECT. Any method the bot's scopes
  ;; allow; nothing here narrows it. The token stays on the executor and never
  ;; appears in the answer. Slack answers every write with the object it made
  ;; or an ok, so nothing is read back. Blocks on the caller's thread.
  (declare (ignore body headers timeout retry))
  (unless (and host host.executor)
    (error "the slack channel is not running"))
  (apply #'call host.executor method path arguments))

;;; --- the channel entry ---------------------------------------------------------------

(defun start-channel (section &key executor)
  "Start the Slack lane from its channels.slack config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides both live executors -- the
  ;; scripted test seam; it is wrapped in Slack's envelope like the live ones.
  ;;
  ;; Fail-closed: refuses unless at least one of allowed_channels /
  ;; allowed_users is populated. allowed_users says who may talk; owner says
  ;; whose instructions are standing policy (the kit's room, AUTHORITY). An
  ;; owner may always talk: the ids join allowed_users when that list is the
  ;; gate.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "slack")) section))
         (bot-token (resolve-channel-secret section "bot_token"))
         (app-token (resolve-channel-secret section "app_token"))
         (configured-users (getf settings :allowed-users))
         (owners (resolve-owners "slack" (getf settings :owner)
                                 configured-users "allowed_channels"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (api-base (config-string section "api_base" +slack-api-base+))
         (bot (if executor
                  (wrap-slack-executor executor)
                  (make-slack-executor :api-base api-base :token bot-token)))
         (app (if executor bot (make-slack-executor :api-base api-base :token app-token)))
         (threads (make-thread-book))
         (bot-user-id nil))
    (require-non-empty-allowlist "slack"
                                 "allowed_channels" (getf settings :allowed-channels)
                                 "allowed_users" allowed-users)
    ;; The bot's own user id is the mention, the self filter and the thread
    ;; the bot starts. A failed auth.test is loud: every mention-gated channel
    ;; would reject its asks until the lane restarts.
    (let ((me (execute-plan bot (api-plan "auth.test" nil "auth_test" :retry t
                                                                      :timeout-seconds 15))))
      (if (execution-ok-p me)
          (setf bot-user-id (nlk:json-value (execution-body me) :string "user_id"))
          (warn "slack: auth.test failed (~a); mention-gated channels reject every ask ~
                 until the lane restarts" (execution-error me))))
    (let* ((host (make-host-from-section
                  section (slack-platform :bot-user-id bot-user-id :threads threads)
                  :executor bot
                  :owners owners
                  :soul-path (soul-path section)
                  :request-timeout 30
                  ;; The declared members name the policy's own keys; the rest
                  ;; are read here.
                  :policy (apply #'slack-inbound-policy
                                 :allowed-users allowed-users
                                 :allow-bot-authors (config-boolean section "allow_bot_authors" nil)
                                 :bot-user-id bot-user-id
                                 settings)))
           (adapter (%make-slack-adapter
                     :host host
                     :app-executor app
                     :bot-token bot-token
                     :bot-user-id bot-user-id
                     :threads threads
                     :debug-reconnects-p (config-boolean section "debug_reconnects" nil))))
      (run-host host "socket" (lambda (stop-p) (slack-socket-lap adapter stop-p))
                :on-start (lambda () (setf *slack-adapter* adapter))
                :on-stop (lambda ()
                           ;; A stale stop thunk racing a respawned lane must
                           ;; not clear the fresh adapter's binding.
                           (when (eq *slack-adapter* adapter)
                             (setf *slack-adapter* nil)))))))
