;;;; cell.lisp --- the checkup: what /doctor leaves to a look that costs nothing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nodecode's own /doctor says what this organism is missing -- the model
;;;; pin, the cell folders that failed, the store, the disk, every declared
;;;; section, the default provider's key, a release waiting -- and it is the
;;;; first thing to run. This cell is the other half of the same question,
;;;; which is the half hermes-guide answers for Hermes: of the sections a
;;;; cell declared, which one is still half filled and what member it wants;
;;;; of the folders this boot loaded, which one refused to start and with
;;;; what words; and what the organism's parts are standing up as notices
;;;; right now. Every finding carries its fix, because a finding without one
;;;; is a riddle.
;;;;
;;;; Read-only. Each check reads state another owner already holds --
;;;; (nlk:sections) and the shared config, (nlk:cells) and the folder,
;;;; (nlk:standing-notices) -- so nothing here is cached and nothing is
;;;; written. /checkup prints the report and offers the checks as rows;
;;;; (checkup:scan) answers the same text through eval; the model calls
;;;; run_checkup.

(defpackage #:nodecode-checkup
  (:use #:cl)
  (:nicknames #:checkup)
  (:export #:scan #:finding #:finding-scope #:finding-level #:finding-subject
           #:finding-what #:finding-remedy #:*scopes* #:checkup-error #:checkup-error-detail))

(in-package #:nodecode-checkup)

(nlk:define-peripheral checkup :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "The checkup is the half of a health check the built-in /doctor leaves out,
through the nodecode-checkup cell: Lisp functions in the checkup: package,
called through eval, each answering a string; a refusal is ERROR: CHECKUP-ERROR.
  (checkup:scan)           every check, in the order checkup.scopes lists them
  (checkup:scan \"cells\")   one check
The checks are `sections' (a config section a cell declared that is still half
filled, and the member it wants), `cells' (a folder that loaded and refused at
start, or a link whose folder is gone) and `notices' (what the organism's parts
are standing up as notices right now). Every finding names its fix. Read-only:
nothing here writes the config or the store, and /doctor still reads the model
pin, the store, the disk and the keys. The operator runs the same report as
/checkup, whose panel offers one row per check."
  "What (help :checkup) answers while the cell runs.")

;;; --- findings ---------------------------------------------------------------

(defstruct (finding (:constructor finding (scope level subject what &key remedy))
                    (:copier nil))
  "One thing worth telling the operator, and what to do about it."
  ;; LEVEL is :fix (it is broken now), :warn (it will be) or :info (worth
  ;; knowing, nothing to do). SCOPE is the check that found it, so a report
  ;; and a scope read the same word.
  (scope "" :type string)
  (level :warn)
  (subject "" :type string)
  (what "" :type string)
  (remedy nil))

(defparameter *scopes* '("sections" "cells" "notices")
  "Every check this cell knows, in reading order: what a bare /checkup runs.")

(defun checked-scopes (names)
  "NAMES as the scopes a report runs: NIL, \"all\" or an empty list is
checkup.scopes; a name the cell does not know is a refusal naming the set."
  (let ((names (remove nil (if (stringp names) (list names) names))))
    (cond ((or (null names) (equal names '("all")))
           (or (ignore-errors (setting :scopes)) *scopes*))
          (t (dolist (name names)
               (unless (member name *scopes* :test #'string=)
                 (fail "~s is no check of this cell: ~{~a~^, ~}" name *scopes*)))
             names))))

;;; --- the three checks -----------------------------------------------------------

(defun section-findings ()
  "Every section a cell declared that is enabled and still half filled, with
the member it wants: the same words /settings shows."
  (let ((config (ignore-errors (nle:read-shared-config))))
    (loop for section in (nlk:sections)
          for path = (nlk:section-path section)
          for key = (nlk:section-key path)
          for table = (ignore-errors (nlk:section-table config path))
          when (ignore-errors (nlk:section-needs-setup-p section config))
            collect (finding "sections" :warn key
                             (or (first (ignore-errors
                                         (nlk:section-problems section table)))
                                 "still needed")
                             :remedy (format nil "fill ~a in /settings, then (restart-cells)"
                                              key)))))

(defun cell-findings ()
  "Folders this boot loaded and that refused to start, and links whose
folder is gone -- two ways a cell leaves without a word."
  (append
   (loop for record in (or (ignore-errors (nlk:cells)) '())
         for name = (or (ignore-errors (nlk:cell-name record)) "a cell folder")
         for refusal = (ignore-errors (nlk:cell-refusal record))
         when (and refusal (stringp refusal) (plusp (length refusal)))
           collect (finding "cells" :fix name
                            (format nil "refused at start: ~a" (nlk:one-line refusal))
                            :remedy "fix that section of config.jsonc, then (restart-cells)"))
   (loop for link in (or (ignore-errors (nlk:broken-cell-links)) '())
         collect (finding "cells" :fix (princ-to-string link)
                          "the folder this link points at is gone"
                          :remedy "delete the link, or install the cell again from the hub"))))

(defun notice-findings ()
  "What the organism's parts are standing up as notices right now: a channel
that never connected, a cell that refused, an update waiting."
  (loop for (key text level) in (or (ignore-errors (nlk:standing-notices)) '())
        collect (finding "notices" (if (eq level :error) :fix :warn)
                         (princ-to-string (or key "notice"))
                         (nlk:one-line (princ-to-string text))
                         :remedy "fix what it names; the part clears it when it can")))

(defun scope-findings (scope)
  "SCOPE's findings. A check that cannot read its owner answers none rather
than failing the report: a checkup that dies on a broken home is useless on
exactly the home that needs it."
  (cond ((string= scope "sections") (section-findings))
        ((string= scope "cells") (cell-findings))
        ((string= scope "notices") (notice-findings))
        (t (fail "~s is no check of this cell: ~{~a~^, ~}" scope *scopes*))))

(defun collect-findings (scopes)
  (loop for scope in scopes append (scope-findings scope)))

;;; --- the report -----------------------------------------------------------------

(defparameter *finding-chars* 300
  "How much of a finding's text one line carries; the remedy line says it shorter.")

(defun format-report (findings scopes &key (cap 20))
  "FINDINGS over SCOPES as the report /checkup prints: one line each, its
fix under it, and the words that say what is left to /doctor."
  (let ((scopes (or scopes *scopes*)))
    (if (null findings)
        (format nil "checkup: nothing to fix across ~{~a~^, ~}. /doctor reads the rest."
                scopes)
        (with-output-to-string (out)
          (format out "checkup: ~d finding~:p across ~{~a~^, ~}~@[ (the first ~d shown)~]"
                  (length findings) scopes
                  (and (> (length findings) cap) cap))
          (dolist (item (if (<= (length findings) cap) findings (subseq findings 0 cap)))
            (format out "~%  [~(~a~)] ~a · ~a — ~a~@[~%        -> ~a~]"
                    (finding-level item)
                    (finding-scope item)
                    (finding-subject item)
                    (nlk:clip (nlk:one-line (finding-what item)) *finding-chars*)
                    (finding-remedy item)))))))

(defun scan-scopes (&optional scopes)
  "Run SCOPES -- a name, a list of names, or NIL for checkup.scopes -- and
answer the report as one string."
  (let* ((scopes (checked-scopes scopes))
         (found (collect-findings scopes)))
    (format-report found scopes :cap (or (ignore-errors (setting :max-findings)) 20))))

(define-verb scan (&optional scopes)
  "Run the checks SCOPES names, or checkup.scopes when NIL. => the report."
  (scan-scopes scopes))

;;; --- the tool --------------------------------------------------------------------

(nlk:define-contract checkup-request
  (:doc "Run the checkup: the half of a health check that /doctor leaves out. Read-only, nothing is changed. Answers one line per finding, with the fix under it.")
  ("scope" :string :required t :options '("all" "sections" "cells" "notices")
           :doc "which check to run: all of them, or one; `sections' is a declared config section still half filled, `cells' a folder that refused at start, `notices' what the organism's parts say is wrong")
  ("all_findings" :boolean :default nil
                  :doc "list every finding, not just the first checkup.max_findings"))

(defun run-checkup (request)
  "The model's run_checkup: REQUEST's scope as the report's text."
  (let* ((scope (checkup-request-scope request))
         (scopes (checked-scopes (and scope (list scope))))
         (found (collect-findings scopes)))
    (format-report found scopes
                   :cap (if (checkup-request-all-findings request)
                            most-positive-fixnum
                            (or (ignore-errors (setting :max-findings)) 20)))))

;;; --- /checkup ----------------------------------------------------------------------

(defparameter *row-detail*
  '(("sections" . "a declared section still half filled, and the member it wants")
    ("cells" . "a folder that refused at start, or a link to a folder gone")
    ("notices" . "what the organism's parts say is wrong right now"))
  "What each check looks at, as the panel's row says it.")

(defun panel (text)
  "TEXT with one row per check, so the panel is the report's table of contents."
  (nle:list-dialog
   :title "Checkup" :summary text
   :rows (loop for scope in (or (ignore-errors (setting :scopes)) *scopes*)
               collect (nle:run-row (format nil "Check ~a" scope)
                                    (format nil "/checkup ~a" scope)
                                    :detail (cdr (assoc scope *row-detail* :test #'string=))))))

(defun run-slash (args session-id)
  "/checkup typed alone: the whole report and one row per check; /checkup
SCOPE: that check alone. => the answer's text, and the panel when bare."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (plusp (length text))
        (scan-scopes (list (string-downcase text)))
        (let ((answer (scan-scopes)))
          (values answer (panel answer))))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell checkup
  (:section ("checkup")
    (:guide "nothing here is required: scopes lists the checks a bare /checkup runs, max_findings how long the report may be before it says the rest is left out")
    ("scopes" :list :default '("sections" "cells" "notices")
              :doc "the checks a bare /checkup and checkup:scan run, in order: sections, cells, notices")
    ("max_findings" :integer :default 20 :min 1
                    :doc "how many findings one report carries before it says how many were left out"))
  (:tool "run_checkup" (nlk:contract 'checkup-request) 'run-checkup)
  (:help :checkup "checkup:scan and run_checkup: what /doctor leaves out, with the fix" +primer+)
  (:command "checkup" 'run-slash
            :catches 'checkup-error
            :argument-hint "all | sections | cells | notices"
            :description "What /doctor leaves out: sections half filled, cells that refused, notices standing"))
