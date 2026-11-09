;;;; cell.lisp --- six verbs over the bd (Beads) CLI, and /beads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; bd is Steve Yegge's issue tracker for agents (github.com/steveyegge/beads):
;;;; a git-adjacent store of beads -- tasks, blockers, claims -- read and
;;;; written by one CLI. This folder is the Hermes plugin jacobhausler/hermes-beads
;;;; rewritten as a Nodecode cell, and it keeps the plugin's six tools:
;;;;
;;;;   beads_smoke     the installed bd, the store identity, a bounded frontier
;;;;   beads_frontier  the ready rows, epics excluded, optionally label-scoped
;;;;   beads_show      one bead verbatim
;;;;   beads_claim     claim for one actor, read back -- never steal
;;;;   beads_update    a guarded metadata write over an allowlisted field group
;;;;   beads_comment   an append-only comment
;;;;
;;;; What the plugin did and this does: every call is one fixed argv list in
;;;; the store's own directory with an explicit actor, a google-id-style flag
;;;; gate that refuses before spawn, and bd's exit codes kept honest. What it
;;;; left out: the Hermes Desktop pane (an Electron surface, not a plugin
;;;; module here), and the unguarded description/title replacement verbs,
;;;; which the plugin itself shipped disabled.

(in-package #:nodecode-beads)

;;; --- the words ----------------------------------------------------------------------

(defparameter *primer*
  "beads is the agent-facing half of bd, a shared work graph on disk: one
store directory (the one holding .beads/) per project, read and written by
the bd CLI. Every tool needs that store named from somewhere -- the beads
section's `store`, or the call's own `workspace`.
  beads_frontier  what is ready now (blocked and claimed rows out, epics out)
  beads_show      one bead as bd sees it
  beads_claim     take a bead for an actor, then read it back
  beads_update    priority, notes, due, estimate, external_ref or defer under
                  the --if-assignee/--if-status guard pair
  beads_comment   append a comment
  beads_smoke     which bd, which store, how much is ready
A claim that loses names the holder; a guard that no longer holds writes
nothing and says so; a failed call is a named error, never an empty list.
Closing a bead stays with plain `bd close' -- the escape hatch is the product.
  (beads:frontier \"/work/project\")
  (beads:show \"/work/project\" \"bd-3\")
  (beads:claim \"/work/project\" \"bd-3\" :actor \"ada\")
/beads [LABEL] is the operator's own look at the frontier."
  "What (help :beads) answers while the cell runs.")

;;; --- what a call names ---------------------------------------------------------------

(defun text-or-nil (value)
  "VALUE trimmed when it is text with something in it, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun the-store (workspace)
  "The store this call names: WORKSPACE, else the beads section's `store`."
  (or (text-or-nil workspace)
      (text-or-nil (setting :store))
      (fail "name a store: pass workspace (the absolute directory holding .beads/), or set the beads section's store")))

(defun the-actor (actor)
  "The actor this call is recorded under: ACTOR, else the section's `actor`."
  (or (text-or-nil actor)
      (text-or-nil (setting :actor))
      (fail "name an actor: claims and writes are recorded under one, and the bead's read-back is checked against it")))

(defun the-label (label)
  "LABEL when it is text with something in it, else NIL."
  (text-or-nil label))

(defun the-bead (bead)
  "The bead id this call names."
  (or (text-or-nil bead)
      (fail "name a bead: its id, as bd prints it (bd-3, bd-a1b2)")))

;;; --- reading a row --------------------------------------------------------------------

(defun row-number (row key)
  "ROW's KEY when it is an integer."
  (nlk:json-value row :integer key))

(defun row-list (row key)
  "ROW's KEY when it is an array, as a list."
  (let ((value (nlk:json-value row :array key)))
    (and value (coerce value 'list))))

(defun row-line (row)
  "One frontier row: id, status, priority and title, then its holder."
  (format nil "  ~a  ~a~@[  P~a~]  ~a~@[  @~a~]"
          (or (row-text-field row "id") "?")
          (or (row-text-field row "status") "?")
          (row-number row "priority")
          (or (row-text-field row "title") "")
          (row-text-field row "assignee")))

(defun bead-block (row)
  "One bead as lines: the fields a caller acts on, and the counts that say
why it is or is not workable."
  (with-output-to-string (out)
    (format out "~a  ~a~@[  P~a~]~@[  type ~a~]~@[  @~a~]~%"
            (or (row-text-field row "id") "?")
            (or (row-text-field row "status") "?")
            (row-number row "priority")
            (row-text-field row "issue_type")
            (row-text-field row "assignee"))
    (format out "  title  ~a~%" (or (row-text-field row "title") "(none)"))
    (let ((notes (row-text-field row "notes")))
      (when notes (format out "  notes  ~a~%" (clip notes 600))))
    (let ((description (row-text-field row "description")))
      (when description (format out "  ~a~%" (clip description 1200))))
    (dolist (pair '(("dependencies" . "depends on") ("dependents" . "needed by")
                     ("blocked_by" . "blocked by") ("comments" . "comments")))
      (let ((rows (row-list row (car pair))))
        (when rows (format out "  ~a  ~d~%" (cdr pair) (length rows)))))))

(defun read-bead (store id)
  "STORE's bead ID, re-read, or NIL when bd does not know it."
  (let ((prefix (list "show" id "--json")))
    (multiple-value-bind (parsed exit stdout stderr)
        (bd-run prefix :directory store :readonly t)
      (bd-ok parsed exit prefix stdout stderr)
      (first (rows-of parsed "show")))))

;;; --- claims and guarded writes ---------------------------------------------------------

(defun conflict-holder (text)
  "The holder a refused claim named in TEXT, or NIL when the refusal was
something else."
  (let ((at (search "already claimed by " text)))
    (when at
      (let* ((rest (subseq text (+ at 19)))
             (end (position-if (lambda (c) (member c '(#\Newline #\Return #\.))) rest)))
        (text-or-nil (if end (subseq rest 0 end) rest))))))

(defparameter *field-flags*
  '(("priority" . "--priority") ("notes" . "--notes") ("due" . "--due")
    ("estimate" . "--estimate") ("external_ref" . "--external-ref")
    ("defer" . "--defer"))
  "The guarded field group: the only keys beads_update writes. description and
title are replacement content with no native guard, and the plugin shipped
those verbs disabled.")

(defun field-value-text (key value)
  "VALUE as the text bd is handed for KEY, or a refusal when it is not a scalar."
  (typecase value
    (string value)
    (integer (princ-to-string value))
    (real (princ-to-string value))
    (t (fail "~a must be text or a number, not ~s" key value))))

(defun field-tokens (fields)
  "FIELDS as the argv tokens after --json, and the (KEY . TEXT) pairs they
were built from, for the read-back check. Keys outside the guarded group are
refused instead of dropped."
  (let ((pairs '()))
    (when (or (null fields) (and (hash-table-p fields) (zerop (hash-table-count fields))))
      (fail "fields is empty: name at least one of ~{~a~^, ~}" (mapcar #'car *field-flags*)))
    (maphash (lambda (key value)
               (let ((flag (cdr (assoc key *field-flags* :test #'string=))))
                 (unless flag
                   (fail "~s is not in the guarded field group (~{~a~^, ~}); description and title are replacement content, edited with bd itself"
                         key (mapcar #'car *field-flags*)))
                 (push (cons key (list flag (field-value-text key value))) pairs)))
             fields)
    (setf pairs (sort pairs #'string< :key #'car))
    (values (loop for (key . tokens) in pairs append tokens) pairs)))

(defun row-value-text (row key)
  "ROW's KEY as text, for comparing a read-back against what was written."
  (let ((value (gethash key row)))
    (typecase value
      (string (string-trim '(#\Space #\Tab #\Newline) value))
      (integer (princ-to-string value))
      (real (princ-to-string value))
      (t (and value (princ-to-string value))))))

;;; --- the verbs --------------------------------------------------------------------------

(define-verb smoke (workspace &key label)
  "Which bd is installed, which store this is, and how much is ready in it.
The plugin's beads_smoke: a receipt, not a guess."
  (let* ((store (the-store workspace))
         (label (the-label label))
         (version-prefix '("version"))
         (version (multiple-value-bind (parsed exit stdout stderr)
                      (bd-run version-prefix :directory store :expect-json nil)
                    (declare (ignore parsed))
                    (bd-ok t exit version-prefix stdout stderr)
                    (clip stdout 200)))
         (info-prefix '("info" "--json"))
         (info (multiple-value-bind (parsed exit stdout stderr)
                   (bd-run info-prefix :directory store :readonly t)
                 (bd-ok parsed exit info-prefix stdout stderr))))
    (with-output-to-string (out)
      (format out "bd ~a~%" version)
      (format out "store  ~a~@[  (~a)~]~%"
              store (nlk:json-value info :string "database_path"))
      (let ((count (nlk:json-value info :integer "issue_count")))
        (when count (format out "beads  ~d~%" count)))
      (format out "~a" (frontier workspace :label label)))))

(define-verb frontier (workspace &key label)
  "What is ready now: open, unblocked, unclaimed beads, epics excluded (they
never dispatch). The plugin's beads_frontier, bounded by the section's
limit and bd's own --max-rows."
  (let* ((store (the-store workspace))
         (label (the-label label))
         (prefix (append (list "ready" "--json" "--exclude-type=epic"
                               "-n" (princ-to-string (setting :limit))
                               "--max-rows" (princ-to-string (setting :max-rows)))
                         (when label (list "--label" label)))))
    (multiple-value-bind (parsed exit stdout stderr)
        (bd-run prefix :directory store :readonly t)
      (bd-ok parsed exit prefix stdout stderr)
      (let ((rows (rows-of parsed "ready")))
        (if rows
            (format nil "~d ready~@[ labelled ~a~] in ~a~%~{~a~%~}"
                    (length rows) label store (mapcar #'row-line rows))
            (format nil "nothing ready~@[ labelled ~a~] in ~a: every open bead is blocked, claimed or an epic~%"
                    label store))))))

(define-verb show (workspace bead)
  "One bead as bd sees it: status, priority, holder, notes, and the counts of
what it waits on. The plugin's beads_show."
  (let* ((store (the-store workspace))
         (id (the-bead bead))
         (row (read-bead store id)))
    (if row
        (bead-block row)
        (format nil "bd knows no bead ~a in ~a" id store))))

(define-verb claim (workspace bead &key actor)
  "Claim a bead for one actor, then read it back and check the read: the
assignee is the actor and the status is in_progress. A claim someone else
holds is refused with their name -- never stolen, never retried."
  (let* ((store (the-store workspace))
         (id (the-bead bead))
         (who (the-actor actor))
         (prefix (list "update" id "--claim" "--json")))
    (multiple-value-bind (parsed exit stdout stderr)
        (bd-run prefix :directory store :actor who :expect-json nil)
      (declare (ignore parsed))
      (unless (eql exit 0)
        (let ((holder (conflict-holder (concatenate 'string stderr " " stdout))))
          (if holder
              (fail "~a is already claimed by ~a: not stolen and not retried -- read it (beads_show) and take another, or let the holder finish"
                    id holder)
              (bd-ok nil exit prefix stdout stderr))))
      (let ((row (read-bead store id)))
        (unless (and row
                     (equal who (row-text-field row "assignee"))
                     (equal "in_progress" (row-text-field row "status")))
          (fail "~a: the claim ran but the read-back says assignee=~s status=~s -- that is not ownership"
                id (and row (row-text-field row "assignee"))
                (and row (row-text-field row "status"))))
        (format nil "claimed ~a as ~a; read back and verified (status in_progress)~@[~%  ~a~]"
                id who (row-text-field row "title"))))))

(define-verb update (workspace bead &key actor if-assignee if-status fields)
  "One guarded metadata write: priority, notes, due, estimate, external_ref or
defer, under both --if-assignee and --if-status, then a read-back that must
agree. A guard that no longer holds writes nothing and says STALE. The
plugin's beads_update."
  (let* ((store (the-store workspace))
         (id (the-bead bead))
         (who (the-actor actor))
         (if-assignee (text-or-nil if-assignee))
         (if-status (text-or-nil if-status)))
    (unless (and if-assignee if-status)
      (fail "a guarded write needs both if_assignee and if_status -- without the guard pair nothing is written"))
    (multiple-value-bind (tokens pairs) (field-tokens fields)
      (let ((prefix (append (list "update" id
                                  "--if-assignee" if-assignee
                                  "--if-status" if-status
                                  "--json")
                            tokens)))
        (multiple-value-bind (parsed exit stdout stderr)
            (bd-run prefix :directory store :actor who :expect-json nil)
          (declare (ignore parsed))
          (when (eql exit 13)
            (let ((row (read-bead store id)))
              (fail "STALE on ~a: the guard (assignee ~a, status ~a) no longer held; nothing was written and the same guard is not retried -- the bead is now ~a under ~a"
                    id if-assignee if-status
                    (or (and row (row-text-field row "status")) "?")
                    (or (and row (row-text-field row "assignee")) "nobody"))))
          (bd-ok nil exit prefix stdout stderr)
          (let ((row (read-bead store id)))
            (unless row (fail "~a: the update ran but the bead could not be read back" id))
            (dolist (pair pairs)
              (let ((want (second (cdr pair))) (got (row-value-text row (car pair))))
                (unless (equal want got)
                  (fail "~a: the read-back disagrees on ~a -- wrote ~s, bd shows ~s"
                        id (car pair) want got))))
            (format nil "updated ~a: ~{~a~^, ~}; read back and verified~@[ (status ~a)~]"
                    id (loop for (key . tokens) in pairs
                             collect (format nil "~a=~a" key (second tokens)))
                    (row-text-field row "status"))))))))

(define-verb comment (workspace bead &key actor text)
  "Append a comment to a bead. Append-only: bd has no comment edit or delete.
The plugin's beads_comment."
  (let* ((store (the-store workspace))
         (id (the-bead bead))
         (who (the-actor actor))
         (text (text-or-nil text)))
    (unless text (fail "a comment needs text"))
    (let ((prefix (list "comments" "add" id "--" text)))
      (multiple-value-bind (parsed exit stdout stderr)
          (bd-run prefix :directory store :actor who :expect-json nil)
        (declare (ignore parsed))
        (bd-ok nil exit prefix stdout stderr)
        (format nil "commented on ~a as ~a (append-only)" id who)))))

;;; --- the boundaries -----------------------------------------------------------------------

(defparameter *workspace-doc*
  "the absolute store directory: the one holding .beads/. Empty uses the beads section's store."
  "One wording for the member every tool carries.")

(nlk:define-contract beads-smoke
  (:doc "Smoke-check one bd store: the installed bd version, the store identity and a bounded ready frontier (epics excluded). Use it first in a project to prove the store answers.")
  ("workspace" :string :default "" :doc *workspace-doc*)
  ("label" :string :default "" :doc "only beads carrying this label; empty for all"))

(nlk:define-contract beads-frontier
  (:doc "The scoped ready frontier of one bd store: open, unblocked, unclaimed beads, epics excluded. Call it before choosing what to work on, and again after a bead closes.")
  ("workspace" :string :default "" :doc *workspace-doc*)
  ("label" :string :default "" :doc "only beads carrying this label; empty for all"))

(nlk:define-contract beads-show
  (:doc "One bead as bd sees it: status, priority, holder, notes and what it waits on.")
  ("workspace" :string :default "" :doc *workspace-doc*)
  ("bead" :text :required t :doc "the bead id, as bd prints it"))

(nlk:define-contract beads-claim
  (:doc "Claim a bead for the actor you are, and read it back: the assignee must be you and the status in_progress. A bead another actor holds is refused with their name -- never stolen, never retried.")
  ("workspace" :string :default "" :doc *workspace-doc*)
  ("bead" :text :required t :doc "the bead id to claim")
  ("actor" :string :default "" :doc "the actor the claim is recorded under; empty uses the section's actor"))

(nlk:define-contract beads-update
  (:doc "A guarded metadata write over the allowed field group -- priority, notes, due, estimate, external_ref, defer and nothing else (status, title and description are not written here) -- under the required if_assignee/if_status guard pair. A guard that no longer holds writes nothing and answers STALE.")
  ("workspace" :string :default "" :doc *workspace-doc*)
  ("bead" :text :required t :doc "the bead id to update")
  ("actor" :string :default "" :doc "the actor the write is recorded under; empty uses the section's actor")
  ("if_assignee" :text :required t :doc "the assignee the bead must still have, or nothing is written")
  ("if_status" :text :required t :doc "the status the bead must still have, or nothing is written")
  ("fields" :object :required t :doc "the fields to write: priority, notes, due, estimate, external_ref or defer"))

(nlk:define-contract beads-comment
  (:doc "Append a comment to a bead. Append-only: bd has no comment edit or delete.")
  ("workspace" :string :default "" :doc *workspace-doc*)
  ("bead" :text :required t :doc "the bead id to comment on")
  ("actor" :string :default "" :doc "the actor the comment is recorded under; empty uses the section's actor")
  ("text" :text :required t :doc "the comment, one paragraph"))

(defun smoke-tool (call)
  "beads_smoke: the receipt the plugin answered."
  (smoke (beads-smoke-workspace call) :label (beads-smoke-label call)))

(defun frontier-tool (call)
  "beads_frontier: the ready rows."
  (frontier (beads-frontier-workspace call) :label (beads-frontier-label call)))

(defun show-tool (call)
  "beads_show: one bead."
  (show (beads-show-workspace call) (beads-show-bead call)))

(defun claim-tool (call)
  "beads_claim: claimed, or refused with the holder's name."
  (claim (beads-claim-workspace call) (beads-claim-bead call)
         :actor (beads-claim-actor call)))

(defun update-tool (call)
  "beads_update: the guarded write and its read-back."
  (update (beads-update-workspace call) (beads-update-bead call)
          :actor (beads-update-actor call)
          :if-assignee (beads-update-if-assignee call)
          :if-status (beads-update-if-status call)
          :fields (beads-update-fields call)))

(defun comment-tool (call)
  "beads_comment: the appended comment."
  (comment (beads-comment-workspace call) (beads-comment-bead call)
           :actor (beads-comment-actor call) :text (beads-comment-text call)))

;;; --- /beads ------------------------------------------------------------------------------

(defun run-slash (args session-id)
  "/beads [LABEL]: the operator's own look at the ready frontier."
  (declare (ignore session-id))
  (let ((label (text-or-nil args)))
    (if label
        (frontier nil :label label)
        (frontier nil))))

;;; --- the declaration ----------------------------------------------------------------------

(nle:define-cell beads
  (:section ("beads")
    (:guide "install the bd CLI once (brew install beads, or a release tarball; bd 1.3 or newer) and run `bd init` in a project, which makes the store; `bd` is its name on PATH or a path to it; store is the directory the tools use when a call names none; actor is the name claims and writes are recorded under")
    ("bd" :string :default "bd"
          :doc "the bd program: its name on PATH, or a path to it")
    ("store" :string :default ""
            :doc "the store directory the tools use when a call names none (the one holding .beads/)")
    ("actor" :string :default ""
            :doc "the actor claims and writes are recorded under when a call names none")
    ("limit" :integer :default 100 :min 1
             :doc "how many ready beads one frontier answers")
    ("max_rows" :integer :default 1000 :min 1
                :doc "bd's own --max-rows bound: past it bd stops (exit 2) rather than answer a partial list")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "one bd call's ceiling"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((program (getf values :bd)))
                 (unless (program-path program)
                   (nlk:config-error
                    "beads.bd is ~s: no such program on PATH -- install the bd CLI (brew install beads, or a release tarball from github.com/steveyegge/beads/releases), or give its path here"
                    program))
                 (let ((store (getf values :store)))
                   (when (and store (plusp (length store)))
                     (let ((problem (store-problems store)))
                       (when problem
                         (nlk:config-error "beads.store is ~s: ~a" store problem)))))
                 values)))
  (:tool "beads_smoke" (nlk:contract 'beads-smoke) 'smoke-tool)
  (:tool "beads_frontier" (nlk:contract 'beads-frontier) 'frontier-tool)
  (:tool "beads_show" (nlk:contract 'beads-show) 'show-tool)
  (:tool "beads_claim" (nlk:contract 'beads-claim) 'claim-tool)
  (:tool "beads_update" (nlk:contract 'beads-update) 'update-tool)
  (:tool "beads_comment" (nlk:contract 'beads-comment) 'comment-tool)
  (:command "beads" 'run-slash
            :catches 'beads-error
            :description "The ready beads in your store"
            :argument-hint "LABEL")
  (:help :beads "beads: read, claim and update a bd task graph - (beads:frontier \"/work/project\")" *primer*))
