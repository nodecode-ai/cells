;;;; package.lisp --- the TLS-POSTURE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, nicknamed TLS so an eval form reads
;;;; (tls:audit "/"). The tool body is the exported verb; the audit itself is
;;;; usable from eval on the same values the model is shown.

(defpackage #:nodecode-tls-posture
  (:use #:cl)
  (:nicknames #:tls)
  (:export #:start-cell
           #:stop-cell
           #:audit
           #:tls-audit
           #:cert #:cert-subject #:cert-issuer #:cert-not-before #:cert-not-after
           #:cert-key-type #:cert-key-bits #:cert-sig-algo #:cert-self-signed-p
           #:decode-certificate #:pem-certificates
           #:tls-posture-error #:tls-posture-error-detail))

(in-package #:nodecode-tls-posture)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. The condition below is what a surface refuses with when the
;;; cell is not running or the tree cannot be audited, so every refusal is
;;; one sentence rather than an unhandled error in the operator's face.
(nlk:define-peripheral tls-posture :not-running t)

(nlk:define-error tls-posture-error (error)
  (detail)
  (:documentation "A TLS-posture refusal: a tree that is not there, a path
that escapes the audit root, a run nothing could be read from.")
  (:report "~a" detail))
