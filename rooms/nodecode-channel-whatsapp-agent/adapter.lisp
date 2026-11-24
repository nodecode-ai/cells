;;;; adapter.lisp --- the platform, the poll lap, the channel entry.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-channel-whatsapp-agent)

;; What the kit's records answer: a host's executor, and an execution's
;; outcome -- the two the lap reads.
(nlk:access (host channel-host) (result execution))


(defvar *whatsapp-agent* nil "The adapter the live lap runs, for an eval that asks.")

(defun whatsapp-agent-where-text (candidate)
  "One sentence on where CANDIDATE is: a WhatsApp agent chat, by the
platform's own ids."
  (let ((chat-id (source-field candidate "channel-id"))
        (user (source-field candidate "user-name")))
    (if (and chat-id (plusp (length chat-id)))
        (format nil "a WhatsApp agent chat with ~a (~a)" (or user chat-id) chat-id)
        "a WhatsApp agent chat")))

(defun whatsapp-agent-platform (adapter)
  "The WhatsApp Agent PLATFORM: an answer as WhatsApp shows it -- markdown
converted to its own markup, cut into messages of at most 4000 UTF-16 units --
no threads, no edits (a message cannot be changed
once sent, so a turn's card is posted once and the typing indicator stands
for the turn), no menu, no buttons; a choice is the words the answer already
carries. The last inbound id per chat is what a receipt and the typing
indicator are said on."
  (make-platform
   :id "whatsapp-agent"
   :name "WhatsApp Agent"
   :noun "chat"
   :owner-label "WhatsApp user id"
   :session-prefix "whatsapp-agent"
   :contract-section "whatsapp-agent-chat"
   :text-limit +text-limit+
   :typing-refresh-ms 20000
   :plan-message (lambda (target chunk &rest keys)
                   ;; A chunk of markdown is not yet a message WhatsApp shows: it is
                   ;; converted, then cut again when the conversion outgrew the limit.
                   (send-text-plans (getf target :channel-id) chunk
                                    :reply-to (getf keys :reply-to)))
   :plan-edit (lambda (target id text &rest keys)
                (declare (ignore target text keys))
                (status-plan id :typing t))
   :plan-delete (lambda (target id &rest keys)
                  (declare (ignore target keys))
                  (status-plan id))
   :plan-typing (lambda (target &rest keys)
                  (declare (ignore keys))
                  (status-plan (or (gethash (getf target :channel-id)
                                             (wa-adapter-last-inbound adapter))
                                    "0")
                               :typing t))
   :plan-file (lambda (target pathname &rest keys)
                ;; A file's words ride as its caption: the platform's media
                ;; message holds one caption of its own, of at most 1024 units,
                ;; and none at all for an audio or a sticker.
                (file-plans adapter target pathname
                            :reply-to (getf keys :reply-to)
                            :caption (getf keys :content)))
   :message-id-of #'message-id-of
   :strip-mention nil
   :where-text #'whatsapp-agent-where-text
   :api-primer "  GET   /updates?timeout=N&limit=N[&offset=C] -- the long poll: one answer is {\"messages\": [{id, from, timestamp, type, text:{body}, ...}]}, or 204 when nothing came in.
  POST  /messages {messaging_product:\"whatsapp\", to, type:\"text\", text:{body}, context:{message_id}} -- one message out; messages[0].id in the answer is its id.
  POST  /statuses {message_id, status:\"read\", typing_indicator:{type:\"text\"}} -- a read receipt and the 25-second typing indicator.
  Media: POST /media (upload), GET /media/{id}, DELETE /media/{id}."
   :seams-primer "  ncwa:outbound-messages (body) -- the messages one updates answer carries, which the lap hands the kit as candidates.
  ncwa:media-download (media-id key) -- a file's bytes; ncwa:attach-media-fetchers gives each inbound attachment its thunk.
  ncwa:file-plan (target pathname :host h) -- upload, then the message that carries the file."))

(defun file-plans (adapter target pathname &key reply-to caption)
  "The plans one file is: the message that carries it, and, when the words
cannot ride on it, the whole of them said first -- what an audio or a
sticker never carries, and a caption past 1024 units."
  (multiple-value-bind (on-file text-first) (formatted-caption pathname caption)
    (when (and (wa-adapter-media-enabled adapter) text-first)
      (dolist (plan (send-text-plans (source-field target "channel-id") text-first
                                     :reply-to reply-to))
        (execute-plan (host-executor (wa-adapter-host adapter)) plan)))
    (and (wa-adapter-media-enabled adapter)
         (file-plan target pathname
                    :host (wa-adapter-host adapter)
                    :reply-to (and (null text-first) reply-to)
                    :caption on-file))))

(defun lap (adapter stop-p)
  "One life of the poll: an updates call, each message it brings through the
host once, until the lane stops."
  (let ((executor (host-executor (wa-adapter-host adapter))))
    (loop
      (when (funcall stop-p) (return :stop))
      (let ((result (execute-plan executor
                                  (updates-plan :offset (wa-adapter-offset adapter)
                                                :timeout (wa-adapter-poll-timeout adapter)))))
        (cond
          (result.ok-p
           (dolist (message (outbound-messages result.body))
             (let ((id (nlk:json-value message :string "id"))
                   (sent (nlk:json-value message :integer "timestamp")))
               ;; What the platform kept from before this lane stood, and what it
               ;; has already handed over, are not asks a second time.
               (unless (or (answered-p adapter id) (backlog-p adapter sent))
                 (dolist (candidate (message-candidates message
                                                         :agent-id (wa-adapter-agent-id adapter)))
                   (setf (gethash (source-field candidate "channel-id")
                                  (wa-adapter-last-inbound adapter))
                         (source-field candidate "message-id"))
                   (handle-candidate (wa-adapter-host adapter)
                                     (attach-media-fetchers adapter candidate)))
                 (remember-answered adapter id))))
           (let ((cursor (nlk:json-value result.body :string "cursor")))
             (when (and cursor (plusp (length cursor)))
               (setf (wa-adapter-offset adapter) cursor)
               (setf (wa-adapter-unsaved adapter) t)))
           (when (wa-adapter-unsaved adapter) (save-place adapter)))
          (t (sleep 2)))))))

(defun probe-channel (section &key executor)
  "What this section amounts to, as a text the operator reads; never the
key. The platform lists no chats: an agent chat is a direct message to the
person who created the agent."
  (declare (ignore executor))
  (let ((users (getf (nlk:section-settings
                      (nlk:find-section '("channels" "whatsapp-agent")) section)
                     :allowed-users)))
    (if users
        (format nil "channels.whatsapp-agent: the key is set and ~d user id~p may chat; an agent chat is a direct message to the one who created the agent"
                (length users) (length users))
        "channels.whatsapp-agent: name the agent's creator in allowed_users: the platform delivers their messages and nobody else's")))

(defun start-channel (section &key executor)
  "Start the WhatsApp Agent lane from its channels.whatsapp-agent SECTION.
Returns a stop thunk."
  ;; Fail-closed: the section names who may drive the agent before a socket
  ;; opens.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "whatsapp-agent")) section))
         (key (resolve-channel-secret section "api_key"))
         (configured-users (getf settings :allowed-users))
         (owners (resolve-owners "whatsapp-agent" (getf settings :owner)
                                 configured-users "allowed_users"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (base (config-string section "base_url" +api-base+))
         (poll-timeout (config-integer section "poll_timeout_seconds" 20 :min 0))
         (media-enabled (config-boolean section "media_enabled" t)))
    (require-non-empty-allowlist "whatsapp-agent" "allowed_users" allowed-users
                                 "owner" (getf settings :owner))
    (let* ((executor (or executor (api-executor key :base base)))
           (adapter (read-place
                     (%make-wa-adapter :section section
                                        :key key
                                        :base base
                                        :agent-id (config-string section "agent_id" "")
                                        :media-enabled media-enabled
                                        :poll-timeout poll-timeout)))
           ;; A lane that finds no place of its own keeps the platform's retained
           ;; backlog from before now out of the room: it was not there to answer it.
           (fresh (null (wa-adapter-started adapter)))
           (host (make-host-from-section
                  section (whatsapp-agent-platform adapter)
                  :executor executor
                  :owners owners
                  :soul-path (soul-path section)
                  :request-timeout 30
                  :policy (apply #'make-inbound-policy
                                 :allowed-users allowed-users
                                 :dm-policy :enabled
                                 :group-policy :disabled
                                 :self-id (or (config-string section "agent_id") "")
                                 :channel-match :exact
                                 :allow-other-keys t
                                 settings))))
      (setf (wa-adapter-host adapter) host)
      (when fresh
        (setf (wa-adapter-started adapter) (get-universal-time))
        (save-place adapter))
      (run-host host "poll" (lambda (stop-p) (lap adapter stop-p))
                :on-start (lambda () (setf *whatsapp-agent* adapter))
                :on-stop (lambda ()
                           (when (eq *whatsapp-agent* adapter)
                             (setf *whatsapp-agent* nil)))))))

(nlk:define-section ("channels" "whatsapp-agent")
  (:guide "Create an agent on Meta's WhatsApp Agent Platform and copy the API key it is called with. The agent's creator is the one person who can chat with it: their WhatsApp user id (digits, as the platform writes them) goes in allowed_users. One organism polls one key.")
  (:check #'probe-channel)
  (:one-of "api_key_env" "api_key_file")
  ("media_enabled" :boolean :default t :doc "files in and out: an ask's image, voice or document is fetched, and an answer's file is uploaded and sent")
  (:any-of "allowed_users" "owner")
  ("api_key_env" :env :doc "environment variable holding the Agent Platform API key")
  ("api_key_file" :path :doc "a file only you can read, holding the key")
  ("allowed_users" :list :doc "WhatsApp user ids allowed to drive the agent (its creator)")
  ("owner" :list :doc "the operator's WhatsApp user id, whose word is standing policy")
  ("agent_id" :string :doc "the agent's own WhatsApp id; the platform's messages from it are not asks")
  ("poll_timeout_seconds" :integer :default 20 :doc "seconds one long poll waits for an update")
  ("base_url" :string :doc "the API root, for a proxy or a test; Meta's by default")
  ("session" :string :doc "the session this lane's rooms keep their history in")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona"))
