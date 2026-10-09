;;;; seams.lisp --- the seams this folder writes through, and what is already here.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An import never writes a destination itself. Every piece lands through
;;;; the seam that already owns it -- NLE:CONFIG-SET for the shared config,
;;;; the auth writer for a key, a folder's own verb for what a folder owns
;;;; -- so an imported fact is indistinguishable from one the operator set,
;;;; and nothing here has to know a file format it does not own.
;;;;
;;;; The other half is the question every conflict rule asks: does this
;;;; organism already hold the fact? An import that clobbers what is here
;;;; is worse than one that skips, so the answer is read from the same
;;;; surfaces the organism reads.

(in-package #:nodecode-import-kit)

;;; --- what this organism already holds ---------------------------------------

(defun config-present-p (&rest path)
  "Whether the shared config already carries a member at PATH."
  (nth-value 1 (apply #'nlk:json-value (nle:read-shared-config) :any path)))

(defun auth-key-present-p (id)
  "Whether auth.json already holds a key for ID."
  (and (nle::auth-api-key (ignore-errors (nle::read-auth-file nle::*auth-file-path*)) id) t))

;;; --- the folders an item stands on -------------------------------------------

(defvar *ensure-cell* 'ensure-cell
  "The function that makes a shipped folder present, loaded and started;
a test stubs it and installs the folder itself.")

(defun ensure-cell (name &key fresh)
  "The shipped folder NAME installed and loaded in this process with the
folders it stands on, and every peripheral among them started -- for a
channel adapter, a library with no start of its own, that is its kit."
  ;; FRESH restarts a peripheral already started, so a section written just
  ;; now is the one it reads. => NAME's record.
  (let ((names (nlk:offer-closure (list name) (nlk:shipped-cells))))
    (unless (member name names :test #'string=)
      (fail "this build ships no ~a folder" name))
    (nlk:when-let (absent (remove-if #'nlk:find-cell names))
      (dolist (folder absent) (nlk:install-cell folder))
      (apply #'nlk:load-cell absent))
    (dolist (folder names)
      (let ((record (or (nlk:find-cell folder) (fail "~a did not load" folder))))
        (when (nlk:cell-failures record)
          (fail "~a failed to load: ~a" folder (cdr (first (nlk:cell-failures record)))))
        (when (eq (nlk:cell-kind record) :peripheral)
          (when (or fresh (not (nlk:cell-started-p record)))
            (nle:restart-cells folder))
          (unless (nlk:cell-started-p record)
            (fail "~a did not start~@[: ~a~]" folder (nlk:cell-refusal record))))))
    (nlk:find-cell name)))

(defun need-cell (name &key fresh)
  (funcall *ensure-cell* name :fresh fresh))
