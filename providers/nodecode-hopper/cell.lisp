;;;; cell.lisp --- the cell: Hopper's list among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but hopper, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog's hopper row is this section's
;;;;                          base, the chat lane's package, and exactly the
;;;;                          ids models.txt names, so /connect offers it,
;;;;                          /models lists it and a turn resolves its lane
;;;;                          and address the way it does any catalog
;;;;                          provider's
;;;;   LIST-PROVIDER-MODELS   the picker's own listing for hopper is that
;;;;                          same file, so it never widens to OpenRouter's
;;;;                          whole roster; asked with :key it asks OpenRouter
;;;;                          itself, which is /connect's key check
;;;;   :CREDENTIAL            OPENROUTER_API_KEY, else no key: never the chat
;;;;                          family's default variable; a key /connect saved
;;;;                          answers before this point does
;;;;
;;;;   /hopper list | add ID... | remove ID... | reset | file
;;;;
;;;; Config, a sibling top-level key:
;;;;   "hopper": {"base_url": "https://openrouter.ai/api/v1",
;;;;              "models_file": "~/.nodecode/hopper/models.txt"}
;;;; A vetoed section ("enabled": false) installs nothing, and the provider
;;;; is then whatever the catalog alone makes of it.

(in-package #:nodecode-hopper)

(defparameter +primer+
  "hopper: your own list of OpenRouter model ids, offered as a provider
beside OpenRouter's whole roster, so the picker holds the models you chose.
  /hopper list             what the picker offers now
  /hopper add ID...        add model ids (provider/model, as OpenRouter
      spells them, `:free' and routing suffixes included)
  /hopper remove ID...     take them out
  /hopper reset            back to the one model the list starts with
  /hopper file             the file the list lives in
The file is one model id a line, `#` a comment; models_file in the hopper
section names another, and a line that is not provider/model is skipped.
The key is OPENROUTER_API_KEY, or one /connect saved. (help :hopper) has
this while the cell runs."
  "What (help :hopper) answers while the cell runs.")

;;; --- the catalog --------------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Hopper's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Hopper's row, made once per
catalog the core reads, and again after the list changes."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row base))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so the next read builds it again: a list that
changed is a row that changed."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Start clean, and leave nothing of the last start behind."
  (forget-catalog)
  (nle:on-stop #'forget-catalog))

;;; --- the list's words ---------------------------------------------------------------

(defun list-text ()
  "The list as the operator reads it: every id, or how to add one."
  (let ((ids (curated-models)))
    (if ids
        (format nil "~d model~:p in ~a:~%~{~a~%~}" (length ids) (namestring (models-file)) ids)
        (format nil "no models: add one with /hopper add ID (file ~a)"
                (namestring (models-file))))))

(defun add-text (ids)
  "IDs added to the list => what changed, and the list as it now stands."
  (let ((current (curated-models)) (added '()) (bad '()))
    (dolist (id ids)
      (cond ((not (model-id-p id)) (push id bad))
            ((not (member id current :test #'equal)) (push id added))))
    (setf added (nreverse added) bad (nreverse bad))
    (when added
      (write-models (append current added))
      (forget-catalog))
    (with-output-to-string (out)
      (when bad
        (format out "not an OpenRouter model id (expected provider/model): ~{~a~^, ~}~%" bad))
      (when added
        (format out "Added ~d model~:p: ~{~a~^, ~}~%" (length added) added))
      (write-string (list-text) out))))

(defun remove-text (ids)
  "IDS taken out of the list => what changed, and the list as it now stands."
  (let* ((current (curated-models))
         (targets (remove-duplicates (remove-if-not #'model-id-p ids) :test #'equal))
         (kept (set-difference current targets :test #'equal))
         (gone (set-difference current kept :test #'equal)))
    (when gone
      (write-models kept)
      (forget-catalog))
    (with-output-to-string (out)
      (when gone
        (format out "Removed ~d model~:p: ~{~a~^, ~}~%" (length gone) gone))
      (write-string (list-text) out))))

(defun reset-text ()
  "The default list written back => what the list now stands at."
  (write-models +default-models+)
  (forget-catalog)
  (format nil "Back to the default list.~%~a" (list-text)))

(defun run-command (args session-id)
  "One /hopper invocation: typed alone or `list', the list; add, remove,
reset and file as the words say."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (or args "") :separator '(#\Space #\Tab)))
         (verb (string-downcase (or (first words) ""))))
    (cond ((member verb '("" "list") :test #'equal) (list-text))
          ((equal verb "add") (add-text (rest words)))
          ((equal verb "remove") (remove-text (rest words)))
          ((equal verb "reset") (reset-text))
          ((equal verb "file") (namestring (models-file)))
          (t (format nil "not one of its choices: list, add ID..., remove ID..., reset, file.~%~a"
                     (list-text))))))

(nle:define-cell hopper
  (:section ("hopper")
    (:guide "hopper offers the OpenRouter model ids your list names; add them with /hopper add, or edit models_file; the key is OPENROUTER_API_KEY or one /connect saved; base_url is where OpenRouter is served")
    ("base_url" :string :default +base+
     :doc "the OpenRouter API base the chat lane appends /chat/completions to")
    ("models_file" :string :default ""
     :doc "the file the list lives in, one OpenRouter model id a line; empty is hopper/models.txt in the organism's home"))
  (:help :hopper "hopper: the OpenRouter models you curate yourself - (/hopper add ID)" +primer+)
  (:command "hopper" #'run-command
            :description "Hopper: the OpenRouter models you choose yourself"
            :argument-hint "list | add ID... | remove ID... | reset | file")
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook :credential #'credential))
