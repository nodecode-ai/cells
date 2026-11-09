;;;; package.lisp --- NODECODE-CHANNEL-WHATSAPP-AGENT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The WhatsApp Agent Platform channel: Meta's /agent/v1 API, where an
;;;; agent created on the platform is the other end of the chat. One API key
;;;; long-polls for updates and posts messages back; the person who created
;;;; the agent is the one who may chat with it. Plans, the poll and the
;;;; candidates are this cell's; rooms, lanes, cards and admission are the
;;;; channel kit's.

(defpackage #:nodecode-channel-whatsapp-agent
  (:documentation "WhatsApp Agent Platform channel adapter for Nodecode.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:ncwa)
  (:export
   #:start-channel #:probe-channel #:whatsapp-agent-platform
   #:+api-base+ #:updates-plan #:send-text-plan #:status-plan #:message-id-of
   #:message-candidates #:outbound-messages #:whatsapp-agent-where-text
   #:attachment-entry #:text-of-message
   #:media-kind #:media-mime #:media-id-p #:media-info #:media-download
   #:media-fetch-thunk #:attach-media-fetchers #:trusted-media-url
   #:upload-plan #:send-media-plan #:file-plan #:execute-plan-or-error
   #:to-whatsapp #:sanitize-text #:utf16-length #:split-formatted #:send-text-plans
   #:formatted-caption #:+text-ceiling+ #:+caption-limit+ #:+text-limit+))
