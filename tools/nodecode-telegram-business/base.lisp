;;;; base.lisp --- the constants every file shares, and the one refusal.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-telegram-business)

(defparameter +api-base+ "https://api.telegram.org"
  "Where the Bot API lives. One seam: tests stub NLK:HTTP, never this.")

(defparameter +default-persona+
  "You are drafting a short, friendly reply on behalf of the account owner. Match
the tone of a personal message - warm, direct and concise. Do not introduce
yourself as an assistant or an AI. Reply in the same language the customer used."
  "The drafting instruction the original plugin shipped, and the default here.")

(defparameter +callback-prefix+ "bd:"
  "The button data prefix, as the original used.")

(defparameter +max-owner-quote+ 1500
  "How much of a customer message the owner's DM quotes back.")

(defparameter +answerable+ '("pending" "awaiting_edit")
  "Draft statuses that may still be delivered.")

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (telegram-business-error (condition)
       (nle:failure (format nil "ERROR: ~a" (telegram-business-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Telegram Business: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))
