;;;; state.lisp --- what one lane remembers: the key, the cursor, the last
;;;; message said in each chat.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-channel-whatsapp-agent)

;; One agent chat, polled: the platform's own ids are the whole of it. The
;; key lives here and nowhere a printed record would show it.
(defstruct (wa-adapter (:constructor %make-wa-adapter) (:copier nil)
                        (:print-object print-wa-adapter))
  host
  section
  key
  (base +api-base+)
  (agent-id "")
  (media-enabled t)
  (offset nil)
  (poll-timeout 20)
  ;; The place the poll keeps across restarts, in the store: the platform's
  ;; cursor, the ids already answered, and when the lane first stood.
  (seen nil)
  (started nil)
  (unsaved nil)
  (last-inbound (make-hash-table :test #'equal)))

(defun print-wa-adapter (adapter stream depth)
  "One line, and never the key."
  (declare (ignore depth))
  (format stream "#<WA-ADAPTER ~a>" (or (wa-adapter-base adapter) "")))

;;; --- the place the poll keeps -------------------------------------------------

(defparameter +place-key+ "poll"
  "The store key the poll's place is kept under: an api key's lane is the
whole of it, so one name holds one cursor.")

(defparameter +recent-ids-max+ 512
  "How many answered ids are remembered: the platform redelivers what it
already sent, and one long run must not answer the same words twice.")

(defun read-place (adapter)
  "ADAPTER, with the place it was left at: the cursor, the ids already
answered, when the lane first stood. A lane that never wrote one starts from
the beginning, and skips what the platform kept from before it stood."
  (when (nlk:store-open-p)
    (ignore-errors
      (let ((text (nlk:session-state-get "channel-whatsapp-agent" +place-key+)))
        (when (and text (plusp (length text)))
          (let ((data (nlk:decode-json text)))
            (when (hash-table-p data)
              (setf (wa-adapter-offset adapter) (nlk:json-value data :string "offset")
                    (wa-adapter-started adapter) (nlk:json-value data :integer "started")
                    (wa-adapter-seen adapter)
                    (reverse
                     (loop for id across (or (nlk:json-array data "seen") #())
                           when (and (stringp id) (plusp (length id)))
                             collect id)))))))))
  adapter)

(defun save-place (adapter)
  "Write ADAPTER's place down, when the store is open: a place lost is words
answered twice, never words lost."
  (when (nlk:store-open-p)
    (ignore-errors
      (nlk:session-state-put "channel-whatsapp-agent" +place-key+
                              (nlk:encode-json-object
                               (nlk:json-object
                                "offset" (wa-adapter-offset adapter)
                                "started" (wa-adapter-started adapter)
                                "seen" (coerce (reverse (wa-adapter-seen adapter)) 'vector))))
      (setf (wa-adapter-unsaved adapter) nil)))
  adapter)

(defun answered-p (adapter message-id)
  "Whether MESSAGE-ID has been answered already."
  (and message-id (member message-id (wa-adapter-seen adapter) :test #'equal) t))

(defun remember-answered (adapter message-id)
  "Note MESSAGE-ID as answered, the newest first, the window no longer than
+RECENT-IDS-MAX+."
  (when message-id
    (unless (answered-p adapter message-id)
      (push message-id (wa-adapter-seen adapter))
      (when (> (length (wa-adapter-seen adapter)) +recent-ids-max+)
        (setf (wa-adapter-seen adapter)
              (subseq (wa-adapter-seen adapter) 0 +recent-ids-max+)))
      (setf (wa-adapter-unsaved adapter) t)))
  adapter)

(defun backlog-p (adapter sent-seconds)
  "Whether a message sent at SENT-SECONDS is older than the lane's own first
standing: what the platform kept from before it."
  (and sent-seconds (wa-adapter-started adapter)
       (< sent-seconds (wa-adapter-started adapter))))
