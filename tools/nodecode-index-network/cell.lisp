;;;; cell.lisp --- the section, the nine tools, /index, the manual, the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Index Network is a protocol of intents: a person says what they are
;;;; looking for in one sentence (a signal), shares it in the communities they
;;;; belong to, and other people's agents negotiate whether there is a reason
;;;; for the two humans to meet; what both sides agree to arrives as an
;;;; opportunity the person accepts or rejects. The original Hermes plugin is
;;;; the client half of that, plus a Bun negotiator that runs the turns. This
;;;; cell is the client half: the same REST API, the same reads and writes,
;;;; reached with an Index API key.
;;;;
;;;; Nine tools, the original's own names for the same calls:
;;;;   index_read_intents            the caller's signals, or a community's
;;;;   index_create_intent           one signal, from the caller's own words
;;;;   index_update_intent           rewrite a signal and reprocess it
;;;;   index_list_opportunities      the matches negotiated so far
;;;;   index_update_opportunity      accept or reject one
;;;;   index_read_networks           the communities, or one roster
;;;;   index_join_network            join an open community
;;;;   index_add_intent_to_network   share a signal in a community
;;;;   index_read_docs               the protocol's own guidance
;;;;
;;;; /index reads the same account from the shell, and the verbs in the
;;;; indexnetwork package do it from eval. Nothing here runs while the cell is
;;;; off, and a section without a key answers every verb with one sentence
;;;; naming what to set.

(in-package #:nodecode-index-network)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Index Network is a protocol of intents: a person writes what they are
looking for as one signal, shares it in the communities they belong to, and
other people's agents negotiate whether there is a reason for the two people
to meet. What both sides agree to arrives as an opportunity the person
accepts or rejects.

The tools (the same names the original Hermes plugin uses):
  index_read_intents        the caller's own signals; network_id reads one
      community's; query matches text; limit 1 to 100; page.
  index_create_intent       one signal, from the caller's own words; a
      signal is shared in every community unless network_ids narrows it.
  index_update_intent       rewrite a signal and reprocess it.
  index_list_opportunities the matches negotiated so far, newest first;
      status narrows them.
  index_update_opportunity  accept or reject one, on the owner's approval.
  index_read_networks       the communities the caller belongs to; network_id
      reads one community's roster.
  index_join_network        join an open community.
  index_add_intent_to_network  share a signal in a community.
  index_read_docs           the protocol's canonical guidance, one topic at a
      time: identity-context, signals, communities-networks, opportunities,
      negotiations, workflows.

/INDEX in the shell prints the newest signals, and /index opportunities,
/index networks, /index docs [TOPIC] and /index status read the rest. The
verbs in the indexnetwork package answer a string too: (indexnetwork:signals),
(indexnetwork:opportunities), (indexnetwork:networks), (indexnetwork:docs).

Nothing is written on the caller's behalf that they did not ask for: a signal
is created only when the person says they are looking for something. A
refusal is ERROR: INDEX-NETWORK-ERROR and says what to set or what Index
answered."
  "What (help :index-network) answers while the cell runs.")

;;; --- small checks ----------------------------------------------------------------

(defun bounded (value low high what)
  "VALUE, refused unless it lies between LOW and HIGH (HIGH NIL: no ceiling)."
  (cond ((null value) nil)
        ((not (integerp value)) (fail "~a must be a whole number, not ~s" what value))
        ((< value low) (fail "~a must be at least ~d, not ~d" what low value))
        ((and high (> value high)) (fail "~a must be at most ~d, not ~d" what high value))
        (t value)))

(defun guarded (thunk)
  "THUNK's string; a refusal or a doomed call becomes the tool answer
ERROR: ..., so the model reads what went wrong and the transcript draws the
call failed."
  (handler-case
      (prog1 (funcall thunk)
        (setf *last-error* nil))
    (index-network-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: INDEX-NETWORK-ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: INDEX-NETWORK-ERROR: ~a" condition)))))

;;; --- the verbs, callable from eval and from a tool --------------------------------

(defun signals (&key network-id query archived limit page)
  "The caller's signals, or one community's, as lines."
  (guarded (lambda () (read-signals :network-id network-id :query query
                                     :archived archived
                                     :limit (bounded limit 1 100 "limit")
                                     :page (bounded page 1 nil "page")))))

(defun opportunities (&key status limit)
  "The introductions negotiated for the caller, as lines."
  (guarded (lambda () (read-opportunities :status status
                                           :limit (bounded limit 1 100 "limit")))))

(defun networks (&optional network-id)
  "The communities the caller belongs to, or one community's roster."
  (guarded (lambda () (read-networks network-id))))

(defun docs (&optional topic)
  "The protocol's guidance, all of it or one TOPIC's."
  (guarded (lambda () (read-docs-text topic))))

(defun read-docs-text (topic)
  "The docs, with a topic outside TOPICS refused before the call."
  (when (and topic (plusp (length topic)) (not (member topic +docs-topics+ :test #'string=)))
    (fail "~a is not one of Index's topics: ~{~a~^, ~}" topic +docs-topics+))
  (fetch-docs topic))

;;; --- the model's side of them ---------------------------------------------------

(nlk:define-contract read-intents-call
  (:doc "Read Index Network intents/signals. Use when the person asks what they are looking for, what signals they have, or what members of a community are seeking. With no parameters it reads the caller's own active signals; network_id reads the signals shared in a community the caller belongs to.")
  ("network_id" :text
                :doc "A community's id: read the signals shared there instead of the caller's own.")
  ("query" :text :doc "Text matched against the caller's own signals.")
  ("archived" :boolean :doc "Include the caller's archived signals.")
  ("limit" :integer :doc "Page size, 1 to 100; the section's own when left out.")
  ("page" :integer :doc "1-based page number, used with limit."))

(nlk:define-contract create-intent-call
  (:doc "Create one Index Network signal, from the person's own words, when they say they want to meet, find, hire, fund, collaborate with, or be introduced to someone. The signal is shared in every community they belong to unless network_ids narrows it. Only create a signal they actually asked for.")
  ("description" :text :required t
                 :doc "What the person is looking for, in their own words.")
  ("network_ids" :array :of :string
                  :doc "Community ids to share the signal in; every community when empty."))

(nlk:define-contract update-intent-call
  (:doc "Rewrite one of the caller's own signals and reprocess it. Use when what they are looking for has changed.")
  ("intent_id" :text :required t :doc "The signal's id.")
  ("description" :text :required t :doc "The rewritten signal text."))

(nlk:define-contract read-opportunities-call
  (:doc "Read the opportunities Index has negotiated for the caller: one per pairing both sides' agents found a reason for. Each carries who the counterpart is, its status, and its id.")
  ("status" :text :doc "Only opportunities in this status, e.g. pending or accepted.")
  ("limit" :integer :doc "Page size, 1 to 100; the section's own when left out."))

(nlk:define-contract update-opportunity-call
  (:doc "Move one opportunity to accepted or rejected. Accept only when the owner approves the introduction.")
  ("opportunity_id" :text :required t :doc "The opportunity's id.")
  ("status" :text :required t :options '("accepted" "rejected")
            :doc "accepted to meet the person, rejected to pass."))

(nlk:define-contract read-networks-call
  (:doc "Read the communities the caller belongs to, with their titles and sizes, to resolve a name to its id; with network_id, read one community's roster.")
  ("network_id" :text :doc "A community's id: read its members instead of the list."))

(nlk:define-contract join-network-call
  (:doc "Join an open community on the caller's behalf. Invite-only communities refuse this and need an invitation instead.")
  ("network_id" :text :required t :doc "The community's id."))

(nlk:define-contract share-intent-call
  (:doc "Share one of the caller's signals in a community they belong to. Read index_read_networks first when the id is not known.")
  ("intent_id" :text :required t :doc "The signal's id.")
  ("network_id" :text :required t :doc "The community's id."))

(nlk:define-contract read-docs-call
  (:doc "Read the protocol's canonical guidance, optionally narrowed to one topic: what a signal is, what a community is, how a negotiation runs, and what the person's own identity context holds.")
  ("topic" :text :options '("identity-context" "signals" "communities-networks"
                            "opportunities" "negotiations" "workflows")
           :doc "One topic; every one of them when left out."))

(defun read-intents-tool (call)
  (signals :network-id (read-intents-call-network-id call)
           :query (read-intents-call-query call)
           :archived (read-intents-call-archived call)
           :limit (read-intents-call-limit call)
           :page (read-intents-call-page call)))

(defun create-intent-tool (call)
  (guarded (lambda ()
             (create-signal (create-intent-call-description call)
                            (create-intent-call-network-ids call)))))

(defun update-intent-tool (call)
  (guarded (lambda ()
             (update-signal (update-intent-call-intent-id call)
                            (update-intent-call-description call)))))

(defun read-opportunities-tool (call)
  (opportunities :status (read-opportunities-call-status call)
                 :limit (read-opportunities-call-limit call)))

(defun update-opportunity-tool (call)
  (guarded (lambda ()
             (set-opportunity-status (update-opportunity-call-opportunity-id call)
                                     (update-opportunity-call-status call)))))

(defun read-networks-tool (call)
  (networks (read-networks-call-network-id call)))

(defun join-network-tool (call)
  (guarded (lambda () (join-network (join-network-call-network-id call)))))

(defun share-intent-tool (call)
  (guarded (lambda ()
             (share-in-network (share-intent-call-intent-id call)
                               (share-intent-call-network-id call)))))

(defun read-docs-tool (call)
  (docs (read-docs-call-topic call)))

;;; --- /index ----------------------------------------------------------------------

(defun status-line ()
  "Where this cell points and whether it has a key, without dialing."
  (format nil "index-network: ~a~%~:[no key: set index-network.api_key in /settings, or export INDEX_API_KEY~;key set~]~%verbs: signals [QUERY], opportunities [STATUS], networks, docs [TOPIC], status"
          (api-url) (configured-p)))

(defun run-slash (args session-id)
  "One /index invocation => the answer's text. Bare /index reads the newest
signals; a verb reads the rest; a word that is no verb still answers, with
what the verbs are."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (handler-case
        (cond
          ((zerop (length verb))
           (format nil "~a~%~a" (status-line) (signals :limit 5)))
          ((member verb '("signals" "intents") :test #'string=)
           (signals :query (and (plusp (length rest)) rest) :limit 10))
          ((string= verb "opportunities") (opportunities :status (and (plusp (length rest)) rest) :limit 10))
          ((string= verb "networks") (networks))
          ((string= verb "docs") (docs (and (plusp (length rest)) rest)))
          ((string= verb "status") (status-line))
          (t (format nil "index: ~a is not one of its verbs.~%~a" verb (status-line))))
      (index-network-error (condition) (format nil "index: ~a" condition))
      (error (condition) (format nil "index: ~a" condition)))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell index-network
  (:section ("index-network")
    (:guide "an Index account makes an API key under Settings, then advanced: paste it in api_key, or export INDEX_API_KEY. api_url is the protocol endpoint and app_url where deep links point; both can stay as they are. limit is how many rows a read answers when its call names none.")
    ("api_key" :secret
               :doc "the Index API key; INDEX_API_KEY answers when this is empty")
    ("api_url" :string :default "https://protocol.index.network"
               :doc "the Index protocol endpoint")
    ("app_url" :string :default "https://index.network"
               :doc "where deep links to a signal, an opportunity or a person point")
    ("limit" :integer :default 20 :min 1
             :doc "rows one read answers when its call names none")
    ("timeout" :integer :default 20 :min 1
               :doc "seconds one call to the API may take"))
  (:tool "index_read_intents" (nlk:contract 'read-intents-call) 'read-intents-tool)
  (:tool "index_create_intent" (nlk:contract 'create-intent-call) 'create-intent-tool)
  (:tool "index_update_intent" (nlk:contract 'update-intent-call) 'update-intent-tool)
  (:tool "index_list_opportunities" (nlk:contract 'read-opportunities-call) 'read-opportunities-tool)
  (:tool "index_update_opportunity" (nlk:contract 'update-opportunity-call) 'update-opportunity-tool)
  (:tool "index_read_networks" (nlk:contract 'read-networks-call) 'read-networks-tool)
  (:tool "index_join_network" (nlk:contract 'join-network-call) 'join-network-tool)
  (:tool "index_add_intent_to_network" (nlk:contract 'share-intent-call) 'share-intent-tool)
  (:tool "index_read_docs" (nlk:contract 'read-docs-call) 'read-docs-tool)
  (:command "index" 'run-slash
            :description "Index Network: signals, matches and communities"
            :argument-hint "[signals|opportunities|networks|docs|status] ...")
  (:help :index-network "index-network: signals and introduced matches - index_read_intents, index_list_opportunities, /index" +primer+))
