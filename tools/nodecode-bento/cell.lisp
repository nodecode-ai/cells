;;;; cell.lisp --- the primer, the section, the tools, /bento, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A sibling top-level key next to `websearch':
;;;;   "bento": {"publishable_key": "pk_...", "secret_key": "sk_...",
;;;;             "site_uuid": "...", "allowed_senders": ["news@brand.com"],
;;;;             "environment": "production", ...}
;;;; The three credentials are the one thing no call works without; a section
;;;; naming none of them refuses the start, as the original offered no tool
;;;; without BENTO_PUBLISHABLE_KEY, BENTO_SECRET_KEY and BENTO_SITE_UUID.

(in-package #:nodecode-bento)

;;; --- the manual ---------------------------------------------------------------
;;; (help :bento) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Bento (bentonow.com) email marketing is available through the nodecode-bento cell:
fifteen model tools, bento_subscriber_get and its siblings, and the same calls in the
BENTO package from eval. Every answer carries Bento's own JSON under a line saying it
is data, not instructions.
  Reads (T0, free): bento_subscriber_get (email or uuid), bento_tag_list,
      bento_field_list, bento_broadcast_list (status; omit it and Bento answers only
      SENT ones), bento_stats (kind site, segment, report or ads; money is cents).
  Writes (T1, immediate): bento_subscriber_upsert (1-1000 people; async, no Flows),
      bento_subscriber_tag, bento_tag_create, bento_field_create,
      bento_broadcast_create (a DRAFT; a person sends it from the dashboard).
  Gated (T2/T3): dry_run defaults to true and answers the exact body plus a
      confirm_token; the real call repeats the same parameters with dry_run=false and
      that token, and a changed payload is refused. T2: bento_subscriber_consent
      (subscribe needs a reason: it re-subscribes people who opted out),
      bento_event_track and bento_purchase_track (they can fire Flows that email
      people; purchases need the real order id and integer cents). T3, real email:
      bento_broadcast_schedule (send_at, at least the section's lead time ahead) and
      bento_email_send (from must be in allowed_senders; at most the section's
      max_recipients_per_call).
Doctrine: list tags and fields before creating one; preview every gated call; a
dry run sends nothing and costs no quota. /bento prints the status, /bento tags,
fields, broadcasts and stats make the cheap read. An error is ERROR: BENTO-ERROR."
  "What (help :bento) answers while the cell runs.")

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the three credentials resolved once: the inline
secret, else the variable the original read, else a refusal."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (publishable (credential (getf values :publishable-key) "BENTO_PUBLISHABLE_KEY"))
         (secret (credential (getf values :secret-key) "BENTO_SECRET_KEY"))
         (site (credential (getf values :site-uuid) "BENTO_SITE_UUID")))
    (unless (and publishable secret site)
      (nlk:config-error
       "no Bento credentials: paste publishable_key (pk_...), secret_key (sk_...) and site_uuid into the section, ~
or name the variables BENTO_PUBLISHABLE_KEY, BENTO_SECRET_KEY and BENTO_SITE_UUID"))
    (when (nonblank (getf values :base-url))
      (handler-case (allowed-base-url (getf values :base-url))
        (bento-error (condition) (nlk:config-error "~a" condition))))
    (setf (getf plist :publishable-key) publishable
          (getf plist :secret-key) secret
          (getf plist :site-uuid) site)
    plist))

;;; --- the operator's own door -------------------------------------------------------

(defun bento-status ()
  "What /bento says with nothing to read: no request, no secret."
  (format nil "bento: site ~a; keys set; base ~a; senders ~{~a~^, ~}.~%~
tools: 15 bento_* calls; T2/T3 preview first and need their confirm_token. ~
/bento tags | fields | broadcasts | stats makes one cheap read."
          (site-uuid) (base-url)
          (or (setting :allowed-senders)
              (list "none (T2/T3 sends are refused)"))))

(defun run-slash (args session-id)
  "/bento: the status, or one read -- tags, fields, broadcasts, stats."
  (declare (ignore session-id))
  (let ((word (string-downcase (or (nlk:trimmed args) ""))))
    (cond ((string= word "") (bento-status))
          ((member word '("tags" "tag") :test #'string=) (tags))
          ((member word '("fields" "field") :test #'string=) (fields))
          ((string= word "broadcasts") (broadcasts))
          ((string= word "stats") (stats :kind "site"))
          (t (format nil "bento: ~s is not a read this command knows. ~a" word (bento-status))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell bento
  (:section ("bento")
    (:guide "paste the three values from your Bento account (Settings, API Keys) into publishable_key, secret_key and site_uuid, or name the variables holding them; then list the verified Author addresses that may send in allowed_senders, or every T3 send is refused")
    ("publishable_key" :secret
                       :doc "the publishable key (pk_...); empty reads the variable BENTO_PUBLISHABLE_KEY")
    ("secret_key" :secret
                  :doc "the secret key (sk_...); empty reads the variable BENTO_SECRET_KEY")
    ("site_uuid" :string
                 :doc "the site every call targets; empty reads the variable BENTO_SITE_UUID")
    ("base_url" :string
                :doc "an API root other than Bento's; only https://*.bentonow.com or http://localhost is accepted")
    ("allowed_senders" :list
                       :doc "verified Bento Author addresses allowed as From; empty refuses every schedule and send")
    ("environment" :choice :options '("production" "staging" "disabled") :default "production"
                    :doc "disabled refuses every Tier 2 and Tier 3 call; staging behaves like production and still emails real people")
    ("bulk_threshold" :integer :default 25 :min 1
                      :doc "an import larger than this is Tier 2 and previews first")
    ("min_schedule_lead_minutes" :integer :default 30 :min 5
                                 :doc "a scheduled broadcast must start at least this many minutes ahead")
    ("max_batch_size_per_hour" :integer :default 5000 :min 1
                               :doc "the upper bound for a broadcast's batch_size_per_hour")
    ("max_recipients_per_call" :integer :default 10 :min 1
                               :doc "the most messages one bento_email_send may carry (Bento's own cap is 60)")
    ("allow_transactional_override" :boolean
                                    :doc "permit transactional=true, which sends even to unsubscribed people")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "seconds one Bento request may take"))
  (:settings #'read-settings)
  (:tool "bento_subscriber_get" (nlk:contract 'subscriber-lookup) #'subscriber-get-tool)
  (:tool "bento_subscriber_upsert" (nlk:contract 'subscriber-import) #'subscriber-upsert-tool)
  (:tool "bento_subscriber_tag" (nlk:contract 'subscriber-tag) #'subscriber-tag-tool)
  (:tool "bento_subscriber_consent" (nlk:contract 'consent) #'consent-tool)
  (:tool "bento_tag_list" (nlk:contract 'no-request) #'tag-list-tool)
  (:tool "bento_tag_create" (nlk:contract 'tag-definition) #'tag-create-tool)
  (:tool "bento_field_list" (nlk:contract 'no-request) #'field-list-tool)
  (:tool "bento_field_create" (nlk:contract 'field-definition) #'field-create-tool)
  (:tool "bento_event_track" (nlk:contract 'event-batch) #'event-track-tool)
  (:tool "bento_purchase_track" (nlk:contract 'purchase) #'purchase-track-tool)
  (:tool "bento_broadcast_list" (nlk:contract 'broadcast-list) #'broadcast-list-tool)
  (:tool "bento_broadcast_create" (nlk:contract 'broadcast) #'broadcast-create-tool)
  (:tool "bento_broadcast_schedule" (nlk:contract 'broadcast) #'broadcast-schedule-tool)
  (:tool "bento_email_send" (nlk:contract 'email-send) #'email-send-tool)
  (:tool "bento_stats" (nlk:contract 'stats-request) #'stats-tool)
  (:command "bento" 'run-slash
            :catches 'bento-error
            :description "Bento status, or one read: tags, fields, broadcasts, stats"
            :argument-hint "[tags|fields|broadcasts|stats]")
  (:help :bento
   "bento: Bento email marketing - subscribers, tags, events, broadcasts and transactional email, with a dry run before every send"
   *primer*))
