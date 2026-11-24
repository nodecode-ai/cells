;;;; cell.lisp --- exact constraints, kept verbatim and put in front of the model every request.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of jusst01/constraint-vault. Memory summarises and compression
;;;; paraphrases, so an operator's "never spend more than 0.05 ETH" comes back
;;;; as "avoid large spends" and an address loses a character. The vault is a
;;;; guard layer above both: a small set of strings in one JSON file, put in
;;;; front of the model byte for byte on every request, and every delivery
;;;; logged with the SHA-256 of the block, so "the constraint was in context"
;;;; is a fact rather than a hope.
;;;;
;;;; The original hooked Hermes's pre_llm_call and re-injected through the
;;;; next tool result after a compaction. This image rebuilds its standing
;;;; harness sections at every request and re-pins them when an eviction moves
;;;; the window's head, so the block rides a section of its own: nothing has to
;;;; detect a compaction, and the text is in front of the model every turn.

(defpackage #:nodecode-constraint-vault
  (:use #:cl))

(in-package #:nodecode-constraint-vault)

(nlk:define-peripheral constraint-vault :not-running t)

;;; --- the words -------------------------------------------------------------------
;;; Data, so a layer can reword them, and so the marker the model reads lives in
;;; one place.

(defparameter *marker* "[Constraint vault — exact values, verbatim]"
  "The line the block opens on: the model reads what follows as the operator's
own words, not as new conversation.")

(defparameter *note*
  "The operator pinned these exactly. They are in force for this whole session: do not paraphrase, round, reinterpret or drop any of them."
  "The line under the marker, in the original's voice.")

(defparameter *empty-line*
  "The constraint vault is empty. Pin one with the constraint_pin tool, or /constraints add <exact text>."
  "What a listing says when nothing is pinned.")

(defparameter +primer+
  "The constraint vault keeps a few facts that must survive exactly — a number,
an address, a deadline, a hard rule — as the operator's own words. A pinned
constraint is delivered verbatim at the head of every request, so a folded or
edited conversation cannot paraphrase it away, and each delivery is written to
the injection log with the SHA-256 of the block that went out.

  constraint_pin   text=...  pins one constraint exactly as stated. Pinning
                   something already in the vault answers its id and pins nothing.
  constraint_list  text=...  every constraint with its id; text filters by
                   a case-insensitive substring.
  constraint_drop  id=...    removes one, by the id a list names.

Pin when the operator states a precise limit or rule that must not be rounded
or dropped; do not pin an ordinary request. The operator types /constraints
to see them, /constraints add <text> to pin one, /constraints drop <id> to
remove one. Caps come from the cell's section: 50 constraints of 2000
characters unless it says otherwise. A constraint is the organism's: every
session reads the same set."
  "What (help :constraints) answers while the cell runs.")

;;; --- where the constraints live ------------------------------------------------------

(defun store-pathname ()
  "The store file the section names, or a refusal saying which setting to fill."
  (running-settings)
  (let ((name (and (store-configured-p) (nlk:trimmed (setting :store-file)))))
    (unless name
      (fail "constraint-vault.store_file is not set: name the JSON file the constraints are kept in, e.g. constraint-vault/vault.json under the home, in /settings (or the constraint-vault section of config.jsonc)"))
    (if (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
        (nlk:expand-home name)
        (nlk:home name))))

(defun store-configured-p ()
  "T when the section says where the constraints live."
  (let ((name (ignore-errors (setting :store-file))))
    (and (stringp name) (plusp (length (nlk:trimmed name))))))

(defun log-pathname (store)
  "STORE's delivery log beside it: <name>-injections.jsonl in the same folder."
  (merge-pathnames (make-pathname :name (format nil "~a-injections" (pathname-name store))
                                   :type "jsonl")
                   store))

;;; --- the store ----------------------------------------------------------------------

(defun read-store ()
  "The store as (:next-id N :constraints LIST), oldest first. A missing or
unreadable file is an empty vault — never a refusal on the request path."
  (let ((path (ignore-errors (store-pathname))))
    (if (and path (probe-file path))
        (let ((object (ignore-errors (nlk:decode-json (nlk:read-text path)))))
          (let ((items (if object (nlk:json-array object "constraints") #())))
            (list :next-id (or (and object (nlk:json-value object :integer "next_id"))
                               (1+ (loop for entry across items
                                          maximize (or (nlk:json-value entry :integer "id") 0))))
                  :constraints (sort (loop for entry across items
                                           for text = (nlk:json-value entry :string "text")
                                           when (and text (plusp (length text)))
                                             collect (list :id (or (nlk:json-value entry :integer "id") 0)
                                                           :text text
                                                           :sha256 (or (nlk:json-value entry :string "sha256") "")
                                                           :pinned-at (or (nlk:json-value entry :string "pinned_at") "")))
                                     #'< :key (lambda (constraint) (getf constraint :id))))))
        (list :next-id 1 :constraints '()))))

(defun read-constraints ()
  "Every pinned constraint, oldest first."
  (getf (read-store) :constraints))

(defun write-store (next-id constraints)
  "NEXT-ID and CONSTRAINTS to the store, atomically: tmp, then rename. => the pathname."
  (let* ((path (store-pathname))
         (tmp (uiop:parse-native-namestring (concatenate 'string (namestring path) ".tmp"))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (nlk:encode-json-object
                     (nlk:make-json-object "format" 1
                                            "next_id" next-id
                                            "constraints" (apply #'vector (mapcar #'constraint-json constraints))))
                    out))
    (rename-file tmp path)
    path))

(defun constraint-json (constraint)
  "CONSTRAINT as the JSON object the store keeps."
  (nlk:make-json-object "id" (getf constraint :id)
                         "text" (getf constraint :text)
                         "sha256" (getf constraint :sha256)
                         "pinned_at" (getf constraint :pinned-at)))

;;; --- the block -----------------------------------------------------------------------

(defun block-text (&optional (constraints (read-constraints)))
  "CONSTRAINTS as the verbatim block, built byte-identically from the store,
or NIL when nothing is pinned."
  (when constraints
    (with-output-to-string (out)
      (format out "~a~%~a~%" *marker* *note*)
      (dolist (constraint constraints)
        (format out "~d. ~a~%" (getf constraint :id) (getf constraint :text))))))

(defun log-injection (session-id block count)
  "Append one delivery record for BLOCK to the log: when, which session, and
the block's SHA-256. Failures are swallowed — the log is not a precondition."
  (handler-case
      (nlk:append-json-line (log-pathname (store-pathname))
                             (nlk:make-json-object "at" (nlk:iso-now)
                                                    "session" (or session-id "default")
                                                    "via" "harness"
                                                    "constraints" count
                                                    "block_sha256" (nlk:sha256-text block)))
    (error () nil)))

(defun deliver (session-id)
  "The block one request build carries, logged, or NIL when there is nothing
to say. Never signals: this runs on the request path."
  (handler-case
      (when (and (setting :inject) (store-configured-p))
        (let ((constraints (read-constraints)))
          (when constraints
            (let ((block (block-text constraints)))
              (when (setting :log-injections)
                (log-injection session-id block (length constraints)))
              block))))
    (error () nil)))

(defun constrain-sections (next session-id)
  "The harness-section read: the block rides a standing section of its own, so
it is in front of the model at every request, and again after an eviction
re-pins the head."
  (let ((sections (funcall next session-id))
        (block (deliver session-id)))
    (if block
        (acons "constraint-vault" block
               (remove "constraint-vault" sections :key #'car :test #'equal))
        sections)))

;;; --- pinning, listing, dropping -------------------------------------------------------

(defun pin (text)
  "Pin TEXT exactly. => the answer the model or the operator reads."
  (let ((text (nlk:trimmed (princ-to-string (or text "")))))
    (when (zerop (length text))
      (fail "pinning needs text: the constraint in the operator's own words"))
    (let ((limit (setting :max-chars))
          (cap (setting :max-constraints)))
      (when (> (length text) limit)
        (fail "constraint too long (~d > ~d characters): pin the exact value, not a paragraph"
              (length text) limit))
      (let* ((store (read-store))
             (constraints (getf store :constraints))
             (same (find text constraints :key (lambda (constraint) (getf constraint :text))
                                         :test #'string=)))
        (if same
            (format nil "Already pinned as #~d: ~a" (getf same :id) (getf same :text))
            (progn
              (when (>= (length constraints) cap)
                (fail "the vault is full (~d constraints): /constraints drop <id> drops one" cap))
              (let* ((id (getf store :next-id))
                     (entry (list :id id
                                   :text text
                                   :sha256 (nlk:sha256-text text)
                                   :pinned-at (nlk:iso-now))))
                (write-store (1+ id) (append constraints (list entry)))
                (format nil "Pinned #~d: ~a~%Every request from now on carries it verbatim, so compaction cannot paraphrase it."
                        id text))))))))

(defun drop (id)
  "Remove the constraint ID. => the answer the caller reads."
  (unless id
    (fail "dropping needs an id: /constraints lists every constraint with its id"))
  (let* ((store (read-store))
         (constraints (getf store :constraints))
         (found (find id constraints :key (lambda (constraint) (getf constraint :id)))))
    (unless found
      (fail "no constraint with id ~s: /constraints lists the ids" id))
    (write-store (getf store :next-id) (remove found constraints))
    (format nil "Dropped #~d: ~a" id (getf found :text))))

(defun listing (&optional text)
  "Every pinned constraint with its id, optionally only those whose text holds
TEXT, or the one line saying the vault is empty."
  (running-settings)
  (store-pathname)
  (let ((constraints (read-constraints)))
    (when (and text (plusp (length (nlk:trimmed text))))
      (setf constraints (remove-if-not (lambda (constraint)
                                         (search (nlk:trimmed text) (getf constraint :text)
                                                 :test #'char-equal))
                                       constraints)))
    (if (null constraints)
        *empty-line*
        (with-output-to-string (out)
          (format out "~d pinned constraint~:p, delivered verbatim on every request:~%"
                  (length constraints))
          (dolist (constraint constraints)
            (format out "  ~d. ~a~%" (getf constraint :id) (getf constraint :text)))))))

(defun parse-id (text)
  "TEXT as a constraint id, or a refusal saying what an id looks like."
  (let ((id (ignore-errors (parse-integer (nlk:trimmed (or text "")) :junk-allowed nil))))
    (or id (fail "~s is no constraint id: /constraints lists them with their ids"
                 (nlk:trimmed (or text ""))))))

;;; --- the model's way in ---------------------------------------------------------------

(nlk:define-contract pin-call
  (:doc "Pin an exact operator constraint — a number, an address, a deadline, a hard rule — into the constraint vault, in the operator's own words. It is delivered verbatim at the head of every request, so a folded or compressed conversation cannot paraphrase or round it. Use it when the operator states a precise limit or rule that must keep applying; do not pin an ordinary request.")
  ("text" :text :required t
          :doc "the constraint exactly as the operator stated it"))

(nlk:define-contract list-call
  (:doc "List the constraints pinned in the vault, with their ids.")
  ("text" :text
          :doc "only the constraints whose text holds this, case-insensitive"))

(nlk:define-contract drop-call
  (:doc "Remove a pinned constraint from the vault by its id, as constraint_list names it.")
  ("id" :integer :required t
        :doc "the id of the constraint to remove"))

(defun pin-tool (call)
  "The constraint_pin tool: CALL's text, pinned."
  (pin (pin-call-text call)))

(defun list-tool (call)
  "The constraint_list tool: every constraint, or those matching a filter."
  (listing (list-call-text call)))

(defun drop-tool (call)
  "The constraint_drop tool: CALL's id, dropped."
  (drop (drop-call-id call)))

;;; --- the operator's way in -------------------------------------------------------------

(defun run-slash (args session-id)
  "One /constraints invocation: no argument lists, 'add TEXT' pins, 'drop ID'
drops, 'list [TEXT]' lists."
  (declare (ignore session-id))
  (let* ((args (nlk:trimmed (or args "")))
         (lower (string-downcase args)))
    (cond ((zerop (length args)) (listing))
          ((uiop:string-prefix-p "add " args) (pin (subseq args 4)))
          ((uiop:string-prefix-p "drop " args) (drop (parse-id (subseq args 5))))
          ((uiop:string-prefix-p "rm " args) (drop (parse-id (subseq args 3))))
          ((uiop:string-prefix-p "list" lower) (listing (nlk:trimmed (subseq args 4))))
          (t "Usage: /constraints | /constraints add <exact text> | /constraints drop <id>"))))

;;; --- the declaration ------------------------------------------------------------------

(nle:define-cell constraint-vault
  (:section ("constraint-vault")
    (:guide "store_file is the JSON file the constraints are kept in (constraint-vault/vault.json under the home is the usual answer); max_constraints caps how many may be pinned, max_chars how long one may be; inject puts the block in front of the model on every request, and log_injections writes each delivery to a log beside the store; - /cells shows whether the folder started")
    ("store_file" :path :required t
     :doc "the JSON file the constraints are kept in, e.g. constraint-vault/vault.json under the home; the tools and /constraints refuse until it is set")
    ("max_constraints" :integer :default 50 :min 1
                       :doc "how many constraints may be pinned at once")
    ("max_chars" :integer :default 2000 :min 1
                 :doc "the most one constraint's text may carry")
    ("inject" :boolean :default t
              :doc "whether the pinned constraints are put in front of the model on every request")
    ("log_injections" :boolean :default t
                      :doc "whether each delivery is appended to the store's -injections.jsonl with the block's SHA-256"))
  (:tool "constraint_pin" (nlk:contract 'pin-call) #'pin-tool)
  (:tool "constraint_list" (nlk:contract 'list-call) #'list-tool)
  (:tool "constraint_drop" (nlk:contract 'drop-call) #'drop-tool)
  (:hook 'nle::read-harness-sections #'constrain-sections)
  (:command "constraints" 'run-slash
            :catches 'constraint-vault-error
            :description "Exact constraints: list them, pin one, or drop one")
  (:help :constraints
         "constraints: exact operator constraints, delivered verbatim on every request; /constraints"
         +primer+))
