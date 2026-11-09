;;;; cell.lisp --- the section, the five tools, /anki, the manual, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a top-level key beside the others:
;;;;   "anki": {"anki_url": "http://127.0.0.1:8765", "default_deck": "Default",
;;;;            "default_model": "Basic", "request_timeout": 10, "max_cards": 50,
;;;;            "api_key": "", "api_key_env": "ANKI_BRIDGE_API_KEY"}
;;;; Nothing is required: the loopback default is where AnkiConnect listens, and
;;;; the key is empty unless AnkiConnect itself was told to ask for one.

(in-package #:nodecode-anki)

;;; --- the manual ---------------------------------------------------------------
;;; (help :anki) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Anki is reachable through the nodecode-anki cell, which talks to the AnkiConnect
add-on (code 2055492159) on the operator's own machine. Five tools, one /anki command:
  anki_decks      the decks, what is due, what is new, what was reviewed today.
  anki_add_cards  create notes in a deck, creating the deck if needed: cards for a
                  two-field model such as Basic (front, back), or notes with an
                  explicit fields object for any other model.
  anki_search     Anki's own search syntax (deck:Spanish tag:verbs is:due added:7).
  anki_due        the cards ready to review, front and back as plain text.
  anki_answer     grade a card (again, hard, good or easy) and reschedule it.
Doctrine: show the cards you mean to add before adding them, unless the operator
already asked for them. For a quiz show only the front, wait for the answer, then
grade: missed it is again, got it with effort is hard, got it is good, instant is
easy. anki_answer changes the schedule immediately, so call it only after the
operator has answered. Anki may refuse a duplicate: report it, do not retry.
A tool that cannot reach Anki answers an error member saying what to fix: open Anki
desktop with the add-on installed, then ask for the status again."
  "What (help :anki) answers while the cell runs.")

(defparameter *slash-help*
  (format nil "Usage: /anki [status|due [deck]]~%  status       Check AnkiConnect, deck names and due cards.~%  due [deck]   Show up to five due cards (front only).~%  help         This text.~%Requires Anki desktop with the AnkiConnect add-on (code ~a) running."
          +ankiconnect-addon+)
  "What /anki with no arguments explains.")

;;; --- reading the section --------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed string, or NIL when it is empty."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (text value))))
    (if (plusp (length trimmed)) trimmed nil)))

(defun prefix-p (prefix string)
  "Whether STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun clamp (value low high)
  "VALUE held between LOW and HIGH."
  (max low (min high value)))

(defun read-settings (values table)
  "The declared VALUES, with the endpoint checked and the key read here, once."
  (let* ((raw (and table (gethash "anki_url" table)))
         (given (getf values :anki-url))
         (url (cond ((and raw (or (eq raw :null)
                                  (and (stringp raw) (null (nonblank raw)))))
                     (nlk:config-error
                      "no endpoint: anki_url names where AnkiConnect listens, usually ~a"
                      *default-url*))
                    ((nonblank given) (nonblank given))
                    (t *default-url*)))
         (timeout (or (getf values :request-timeout) 10))
         (max-cards (or (getf values :max-cards) *max-list*))
         (inline (nonblank (getf values :api-key)))
         (env (nonblank (getf values :api-key-env))))
    (unless (or (prefix-p "http://" url) (prefix-p "https://" url))
      (nlk:config-error
       "anki_url must be an http:// or https:// address; AnkiConnect's own is ~a" *default-url*))
    (list :anki-url (string-right-trim "/" url)
          :default-deck (or (nonblank (getf values :default-deck)) "Default")
          :default-model (or (nonblank (getf values :default-model)) "Basic")
          :request-timeout (clamp timeout 1 120)
          :max-cards (clamp max-cards 1 100)
          :api-key (or inline (and env (nonblank (uiop:getenv env)))))))

;;; --- the boundary ----------------------------------------------------------------

(nlk:define-contract anki-card
  (:doc "One card to add, for a two-field note model such as Basic.")
  ("front" :text :required t :doc "the prompt the operator will see")
  ("back" :text :required t :doc "the one fact that answers it")
  ("tags" :array :of :string :doc "tags for this card alone"))

(nlk:define-contract anki-note
  (:doc "One note to add, for a model that is not a two-field front/back model.")
  ("fields" :object :required t :doc "an Anki field name to its value")
  ("tags" :array :of :string :doc "tags for this note alone"))

(nlk:define-contract add-cards-request
  (:doc "Create Anki notes in a deck. Pass cards for Basic-style front/back cards, or notes with an explicit Anki fields object for other note models. The deck is created if it does not exist. Duplicate cards are reported, not forced. Always show the proposed cards to the operator before calling this unless they already asked you to add them.")
  ("deck" :string :doc "target deck; the section's default_deck answers when omitted")
  ("model" :string :doc "Anki note model; the section's default_model answers when omitted")
  ("tags" :array :of :string :doc "tags applied to every note in this call")
  ("cards" :array :of anki-card :doc "front/back cards for a two-field model")
  ("notes" :array :of anki-note :doc "notes with explicit field names, for any other model"))

(nlk:define-contract search-request
  (:doc "Search Anki notes with Anki's own search syntax (for example deck:Spanish tag:verbs, is:due, or added:7). Returns note fields as plain text. Use this to find existing cards before creating duplicates.")
  ("query" :text :required t :doc "an Anki search query")
  ("limit" :integer :default 20 :doc "maximum notes to return (1-50)"))

(nlk:define-contract due-request
  (:doc "Return the cards that are ready to review, with the front and back as plain text. For a quiz, show only the front, wait for the operator's answer, then call anki_answer; do not reveal the back before they answer.")
  ("deck" :string :doc "limit to one deck; omit to review across the whole collection")
  ("include_new" :boolean :default t :doc "include new cards as well as scheduled due cards")
  ("limit" :integer :default 20 :doc "maximum cards to return (1-50)"))

(defparameter +ease-names+ '("again" "hard" "good" "easy")
  "The four self-grades anki_answer takes.")

(nlk:define-contract answer-request
  (:doc "Grade a reviewed card and schedule its next appearance. Use after the operator answers a card returned by anki_due: map their self-grade to again, hard, good, or easy. This changes the card's scheduling in Anki.")
  ("card_id" :integer :required t :doc "an Anki card id from anki_due")
  ("ease" :text :required t :options +ease-names+
   :doc "the operator's grade; again resets the card, easy lengthens the interval most"))

;;; --- the tools ------------------------------------------------------------------

(defmacro guarded (&body body)
  "BODY's JSON text, or the JSON text of the refusal it raised: a tool never
throws at the model, it answers what to fix."
  `(handler-case (progn ,@body)
     (anki-error (condition)
       (nlk:encode-json-object (nlk:json-object "error" (princ-to-string condition))))
     (error (condition)
       (nlk:encode-json-object
        (nlk:json-object "error" (format nil "unexpected Anki error: ~a" condition))))))

(defparameter +decks-schema+
  (list :description
        "Check that Anki is reachable and show the collection's decks, scheduled due count, new-card count, and reviews completed today. Use this first when the operator asks about Anki."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)
                                       "additionalProperties" nil))
  "anki_decks takes no arguments.")

(defun decks-tool (arguments)
  "The collection's decks and counts, the first call a conversation makes."
  (declare (ignore arguments))
  (guarded
    (let ((status (status)))
      (let ((answer (nlk:json-object "success" t "connected" t "url" (endpoint)
                                     "decks" (coerce (getf status :decks) 'vector)
                                     "due" (getf status :due)
                                     "new" (getf status :new))))
        (when (getf status :reviewed)
          (setf (gethash "reviewed_today" answer) (getf status :reviewed)))
        (nlk:encode-json-object answer)))))

(defun tag-list (value)
  "VALUE, a JSON array of tags, as a list; a tag with whitespace is refused."
  (let ((tags (mapcar #'text (as-list value))))
    (dolist (tag tags)
      (when (find-if (lambda (char) (member char '(#\Space #\Tab))) tag)
        (fail "tag ~s contains whitespace; Anki tags are single tokens, so use hyphens or underscores"
              tag)))
    (remove-duplicates tags :test #'string=)))

(defun note-object (deck model fields tags)
  "One note as AnkiConnect's addNotes takes it."
  (nlk:json-object "deckName" deck "modelName" model "fields" fields
                    "tags" (coerce tags 'vector)
                    "options" (nlk:json-object "allowDuplicate" nil)))

(defun cards-to-notes (cards deck model field-names common)
  "CARDS as notes of a two-field MODEL: the first field is the front."
  (mapcar (lambda (card)
            (let ((front (text (anki-card-front card)))
                  (back (text (anki-card-back card))))
              (unless (and (nonblank front) (nonblank back))
                (fail "every card needs a non-empty front and back"))
              (when (< (length field-names) 2)
                (fail "note model `~a` has fewer than two fields; pass notes with explicit field names instead"
                      model))
              (let ((fields (nlk:json-object)))
                (setf (gethash (first field-names) fields) front
                      (gethash (second field-names) fields) back)
                (note-object deck model fields
                             (append common (tag-list (anki-card-tags card)))))))
          cards))

(defun notes-to-notes (notes deck model field-names common)
  "NOTES with explicit field names, checked against MODEL's own."
  (mapcar (lambda (note)
            (let* ((given (fields-of (anki-note-fields note)))
                   (unknown (set-difference (mapcar #'first given) field-names :test #'string=)))
              (when unknown
                (fail "unknown field(s) for model `~a`: ~{~a~^, ~}; valid fields: ~{~a~^, ~}"
                      model unknown field-names))
              (let ((fields (nlk:json-object)))
                (dolist (pair given)
                  (setf (gethash (first pair) fields) (second pair)))
                (note-object deck model fields
                             (append common (tag-list (anki-note-tags note)))))))
          notes))

(defun add-cards-tool (request)
  "Create notes, creating the deck when the collection has none of that name."
  (guarded
    (let* ((deck (or (nonblank (add-cards-request-deck request)) (setting :default-deck)))
           (model (or (nonblank (add-cards-request-model request)) (setting :default-model)))
           (cards (as-list (add-cards-request-cards request)))
           (notes (as-list (add-cards-request-notes request)))
           (common (tag-list (add-cards-request-tags request))))
      (when (eq (and cards t) (and notes t))
        (fail "pass exactly one of cards or notes"))
      (let* ((limit (setting :max-cards))
             (count (+ (length cards) (length notes))))
        (when (> count limit)
          (fail "too many notes in one call (~a); the section's max_cards is ~a" count limit)))
      (let* ((field-names (model-field-names model))
             (prepared (if cards
                           (cards-to-notes cards deck model field-names common)
                           (notes-to-notes notes deck model field-names common))))
        (ensure-deck deck)
        (let* ((added (remove nil (add-notes prepared)))
               (fixed (if added (ignore-errors (ensure-notes-in-deck added deck)) 0))
               (answer (nlk:json-object "success" t "deck" deck "model" model
                                         "requested" (length prepared)
                                         "added" (length added)
                                         "duplicates" (- (length prepared) (length added))
                                         "note_ids" (coerce added 'vector))))
          (when (and fixed (plusp fixed))
            (setf (gethash "deck_fixed" answer) fixed))
          (nlk:encode-json-object answer))))))

(defun note-field-object (note)
  "NOTE's fields as a JSON object of plain text, one member a field."
  (let ((fields (nlk:json-object)))
    (dolist (pair (fields-of (field note "fields")))
      (setf (gethash (first pair) fields) (strip-html (second pair))))
    fields))

(defun search-tool (request)
  "Search the collection with Anki's own query syntax."
  (guarded
    (let* ((query (search-request-query request))
           (limit (clamp (or (search-request-limit request) 20) 1 *max-list*))
           (rows '()))
      (dolist (note (search-notes query :limit limit))
        (push (nlk:json-object "note_id" (field note "noteId")
                               "model" (text (field note "modelName"))
                               "fields" (note-field-object note)
                               "tags" (coerce (mapcar #'text (as-list (field note "tags"))) 'vector)
                               "card_ids" (coerce (as-list (field note "cards")) 'vector))
              rows))
      (setf rows (nreverse rows))
      (nlk:encode-json-object
       (nlk:json-object "success" t "query" query "count" (length rows)
                        "notes" (coerce rows 'vector))))))

(defun due-tool (request)
  "The cards ready to review, front and back as plain text."
  (guarded
    (let* ((deck (or (nonblank (due-request-deck request)) ""))
           (include-new (due-request-include-new request))
           (limit (clamp (or (due-request-limit request) 20) 1 *max-list*))
           (rows (mapcar #'card-object
                         (due-cards deck :include-new (not (null include-new)) :limit limit)))
           (answer (nlk:json-object "success" t
                                     "deck" (if (plusp (length deck)) deck "all")
                                     "count" (length rows)
                                     "cards" (coerce rows 'vector))))
      (when (null rows)
        (setf (gethash "message" answer)
              "No cards are due. Try anki_decks to confirm the deck, anki_search to find cards, or anki_add_cards to add a few."))
      (nlk:encode-json-object answer))))

(defun answer-tool (request)
  "Grade a card and let Anki schedule its next appearance."
  (guarded
    (let* ((card-id (answer-request-card-id request))
           (ease-name (string-downcase (answer-request-ease request)))
           (ease (cdr (assoc ease-name *ease-values* :test #'string=))))
      (unless ease
        (fail "ease must be again, hard, good or easy, not ~a" ease-name))
      (let ((accepted (answer-cards (list (nlk:json-object "cardId" card-id "ease" ease)))))
        (unless (and accepted (eq (first accepted) t))
          (fail "Anki did not accept the grade for card ~a; refresh it with anki_due and try again, the card may have been reviewed elsewhere"
                card-id))
        (let ((detail (first (cards-info (list card-id)))))
          (nlk:encode-json-object
           (nlk:json-object "success" t "answered" t "card_id" card-id
                            "ease" ease "ease_name" ease-name
                            "next_interval" (field detail "interval")
                            "next_due" (field detail "due")
                            "reps" (field detail "reps")
                            "lapses" (field detail "lapses"))))))))

;;; --- /anki --------------------------------------------------------------------

(defun words-of (args)
  "ARGS split on whitespace."
  (uiop:split-string (or args "") :separator '(#\Space #\Tab #\Newline)))

(defun slash-status ()
  "What the collection has, in the words /anki status answers with."
  (let* ((status (status))
         (decks (getf status :decks))
         (shown (if (> (length decks) 12) (subseq decks 0 12) decks))
         (more (if (> (length decks) 12) (format nil " (+~a more)" (- (length decks) 12)) "")))
    (format nil "AnkiConnect is connected at ~a.~%Decks: ~a~a~%Due now: ~a scheduled, ~a new. Reviewed today: ~a.~%Try /anki due [deck], or ask for a quiz."
            (endpoint)
            (if shown (format nil "~{~a~^, ~}" shown) "(none)")
            more
            (getf status :due) (getf status :new)
            (or (getf status :reviewed) "unknown"))))

(defun slash-due (deck)
  "Up to five due cards, fronts only, and how a quiz starts."
  (let ((cards (due-cards deck :limit 5)))
    (if (null cards)
        (format nil "No cards are due~@[ in ~a~]. Add a few with anki_add_cards, or come back later."
                (nonblank deck))
        (with-output-to-string (out)
          (format out "Due~@[ in ~a~] (~a shown):~%" (nonblank deck) (length cards))
          (loop for card in cards
                for index from 1
                for row = (card-object card)
                do (format out "~a. ~a  [card ~a]~%" index (field row "front") (field row "card_id")))
          (format out "Ask for a quiz: one card at a time, then anki_answer grades it.")))))

(defun run-slash (args session-id)
  "One /anki invocation: status, due [deck], or the usage text."
  (declare (ignore session-id))
  (handler-case
      (let* ((parts (words-of args))
             (command (string-downcase (or (first parts) "status"))))
        (cond ((member command '("help" "-h" "--help") :test #'string=) *slash-help*)
              ((string= command "status") (slash-status))
              ((string= command "due") (slash-due (format nil "~{~a~^ ~}" (rest parts))))
              (t (format nil "Unknown /anki subcommand: ~a~%~%~a" command *slash-help*))))
    (anki-error (condition) (format nil "Anki error: ~a" condition))
    (error (condition) (format nil "Unexpected Anki error: ~a" condition))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell anki
  (:section ("anki")
    (:guide "open Anki desktop with the AnkiConnect add-on (code 2055492159) listening on http://127.0.0.1:8765, which is where anki_url points by default; default_deck and default_model are what a call that names none gets, and api_key is only for an AnkiConnect told to ask for one")
    ("anki_url" :string :default "http://127.0.0.1:8765"
     :doc "the AnkiConnect endpoint; keep the loopback default unless Anki runs elsewhere")
    ("default_deck" :string :default "Default"
     :doc "the deck a call that names none adds to, created when it does not exist")
    ("default_model" :string :default "Basic"
     :doc "the note model a call that names none uses")
    ("request_timeout" :integer :default 10 :min 1
     :doc "seconds one AnkiConnect request may take (at most 120)")
    ("max_cards" :integer :default 50 :min 1
     :doc "the most notes or cards one call adds or returns (at most 100)")
    ("api_key" :secret
     :doc "AnkiConnect's own API key, when the add-on is configured to require one")
    ("api_key_env" :env :default "ANKI_BRIDGE_API_KEY"
     :doc "the environment variable holding the API key, read when api_key is empty"))
  (:settings #'read-settings)
  (:tool "anki_decks" +decks-schema+ 'decks-tool)
  (:tool "anki_add_cards" (nlk:contract 'add-cards-request) 'add-cards-tool)
  (:tool "anki_search" (nlk:contract 'search-request) 'search-tool)
  (:tool "anki_due" (nlk:contract 'due-request) 'due-tool)
  (:tool "anki_answer" (nlk:contract 'answer-request) 'answer-tool)
  (:command "anki" 'run-slash
            :catches 'anki-error
            :description "Anki status and due cards"
            :argument-hint "[status|due [deck]]")
  (:help :anki
   "anki: Anki cards, search and a quiz loop through a local AnkiConnect - the anki_* tools, /anki status, /anki due [deck]"
   *primer*))
