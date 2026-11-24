;;;; package.lisp --- NODECODE-PROTONMAIL: Proton Mail as tools, without Bridge.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-protonmail
  (:use #:cl)
  (:nicknames #:pmail)
  (:export #:start-cell #:stop-cell
           #:proton-mail-error
           #:srp-proofs #:bcrypt-hash #:expand-hash #:key-passphrase
           #:html-to-text
           #:folders #:inbox #:read-message #:mark-message
           #:trash-messages #:save-attachment #:send-message))

(in-package #:nodecode-protonmail)

(nlk:define-peripheral protonmail :not-running t)
