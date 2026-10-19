;;;; fake-mail.lisp --- raw messages as a mail server delivers them, and an inbox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The adapter's INBOX seam stands where IMAP is: each poll drains what a
;;;; test pushed. The scripted executor stands where SMTP is and keeps every
;;;; plan it was handed, newest first.

(in-package #:nodecode.test)

(define-test-slice "email" "CHANNEL-EMAIL-" :recipe "email")

(defparameter +email-bot+ "bot+nc@example.com")

(defun email-raw (&key (from "Kim <kim@example.com>") (to +email-bot+) (subject "Hello")
                       (message-id "<m1@example.com>") in-reply-to references (body "hi bot")
                       (auth "mx.example.com; dkim=pass header.d=example.com; dmarc=pass header.from=example.com")
                       extra)
  "One raw message as the receiving server hands it on: its stamp on top,
CRLF lines."
  (let ((crlf (coerce '(#\Return #\Newline) 'string)))
    (format nil (concatenate 'string "~{~a" crlf "~}")
            (append (and auth (list (format nil "Authentication-Results: ~a" auth)))
                    (list (format nil "From: ~a" from)
                          (format nil "To: ~a" to)
                          (format nil "Subject: ~a" subject)
                          (format nil "Message-ID: ~a" message-id))
                    (and in-reply-to (list (format nil "In-Reply-To: ~a" in-reply-to)))
                    (and references (list (format nil "References: ~a" references)))
                    extra
                    (list "Content-Type: text/plain; charset=utf-8" "")
                    (uiop:split-string body :separator '(#\Newline))))))

(defstruct (fake-inbox (:copier nil))
  (lock (bt2:make-lock :name "fake-inbox"))
  (queue '())
  (sent '()))

(defun inbox-push (inbox raw)
  (bt2:with-lock-held ((fake-inbox-lock inbox))
    (setf (fake-inbox-queue inbox) (append (fake-inbox-queue inbox) (list raw)))))

(defun inbox-drain (inbox)
  (bt2:with-lock-held ((fake-inbox-lock inbox))
    (prog1 (fake-inbox-queue inbox) (setf (fake-inbox-queue inbox) '()))))

(defun inbox-executor (inbox)
  "The scripted SMTP side: every plan kept, every one answered as sent."
  (nck:make-plan-executor
   :run (lambda (plan)
          (bt2:with-lock-held ((fake-inbox-lock inbox))
            (push plan (fake-inbox-sent inbox)))
          (nck:make-execution :ok-p t :status 250 :body (nck:request-plan-body plan)))))

(defun email-sends (inbox)
  "The SEND plans' bodies, oldest first."
  (bt2:with-lock-held ((fake-inbox-lock inbox))
    (reverse (loop for plan in (fake-inbox-sent inbox)
                   when (equal "SEND" (nck:request-plan-method plan))
                     collect (nck:request-plan-body plan)))))

(defun email-section (token-path soul-path state-path &rest members)
  (let ((section (nlk:json-object
                  "address" +email-bot+
                  "password_file" (namestring token-path)
                  "allowed_senders" (vector "kim@example.com")
                  "owner" (vector "kim@example.com")
                  "soul_file" (namestring soul-path)
                  "state_file" (namestring state-path))))
    (loop for (key value) on members by #'cddr
          do (setf (gethash key section) value))
    section))

(defmacro with-email-lane ((inbox &rest members) &body body)
  "Run BODY against an email lane whose mailbox is INBOX, a FAKE-INBOX; the
provider answers `email reply'."
  (let ((state (gensym "STATE")))
    `(let* ((,inbox (make-fake-inbox))
            (,state (merge-pathnames (format nil "nce-state-~36r.sexp" (random (expt 2 40)))
                                     (uiop:temporary-directory))))
       (unwind-protect
            (with-adapter-lane (:reply "email reply" :token "app-password"
                                :start (lambda (section &key executor)
                                         (nce:start-channel section :executor executor
                                                                    :inbox (lambda () (inbox-drain ,inbox))))
                                :bindings ((executor (inbox-executor ,inbox)))
                                :section (email-section token-path soul-path ,state ,@members)
                                :plans nil
                                :after ((is (adapter-threads-stopped-p "channel-email"))))
              ,@body)
         (ignore-errors (delete-file ,state))))))
