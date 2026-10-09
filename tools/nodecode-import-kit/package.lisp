;;;; package.lisp --- the IMPORT KIT package: vocabulary, settings, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed NIK so a EVAL form reads (nik:scan). Every
;;;; exported verb returns a STRING -- the eval snippet prints the value with
;;;; ~S, so a string reads as text -- and every refusal is IMPORT-ERROR,
;;;; which the eval snippet renders as "ERROR: IMPORT-ERROR: <detail>".

(defpackage #:nodecode-import-kit
  (:use #:cl)
  (:nicknames #:nik)
  (:shadow #:import #:search)
  (:export
   ;; model- and operator-facing verbs
   #:scan #:import #:worlds
   ;; the one condition
   #:import-error
   ;; the plan, for the wizard, a tool, or a test
   #:make-plan #:apply-plan #:report-text #:plan-json
   ;; the facts a home yields, for a test and the unmapped report
   #:read-facts
   ;; the format readers
   #:read-yaml #:read-toml #:read-env-file))

(in-package #:nodecode-import-kit)

(nlk:define-peripheral import :not-running "the import cell is not running")

(nlk:define-error yaml-unreadable (import-error) ()
  ;; The reader recovers per top-level section — damage in one subtree must
  ;; not lose the rest of somebody's config — but recovery is only honest
  ;; where the reader still knows what it is looking at. An anchor, an alias,
  ;; a tag or a directive means values elsewhere in the document may depend on
  ;; a construct this reader does not follow, so reading on would be guessing
  ;; at other people's settings. Those are refused by name, whole, the way
  ;; they always were.
  (:documentation "A YAML refusal that costs the WHOLE document rather than
one of its sections.") (:signal fail-document))

(defun hidden-name-p (name)
  (and (stringp name) (plusp (length name)) (char= (char name 0) #\.)))

(defun hidden-directory-p (directory)
  (hidden-name-p (nlk:folder-name directory)))
