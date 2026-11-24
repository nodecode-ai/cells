;;;; package.lisp --- NODECODE-YANDEX-MAIL: Yandex Mail as eight tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed YMAIL so an eval form reads
;;;; (ymail:list-folders ...). The verbs are tools the model calls; a person
;;;; can call the same functions from eval.

(defpackage #:nodecode-yandex-mail
  (:use #:cl)
  (:nicknames #:ymail)
  (:export #:start-cell #:stop-cell
           ;; the eight verbs
           #:list-folders #:search-messages #:read-message #:save-attachment
           #:mark-message #:move-message #:delete-message #:send-message
           #:yandex-mail-error #:yandex-mail-error-detail
           ;; the wire and the parser, which the tests drive directly
           #:imap-open #:imap-close #:imap-command #:imap-quote
           #:smtp-send #:compose-message
           #:parse-message #:part-text #:part-attachments #:decode-words
           #:page-text #:rfc5322-date #:safe-filename))

(in-package #:nodecode-yandex-mail)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. YANDEX-MAIL-ERROR (signalled by FAIL) comes from here: every
;;; refusal starts as one, and a tool answers it as the call's failure.
(nlk:define-peripheral yandex-mail :not-running t)
