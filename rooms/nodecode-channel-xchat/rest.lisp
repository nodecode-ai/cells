;;;; rest.lisp --- the conversation book and the plans.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call to api.x.com is a plan the kit's executor runs: the
;;;; conversation list, one conversation's events, a ciphertext message, a
;;;; read receipt, the token refresh. The book keeps what survives a restart:
;;;; how far each conversation was read and the access token the last refresh
;;;; left.

(in-package #:nodecode-channel-xchat)

(nlk:access (result execution) (plan request-plan))

(defparameter +api-base+ "https://api.x.com"
  "X's API. The one host this cell ever dials.")

(defparameter +cursor-file+ "cursors.sexp")

;;; --- the book -------------------------------------------------------------------

(defstruct (xchat-book (:copier nil) (:constructor %make-xchat-book))
  (lock (sb-thread:make-mutex :name "xchat book"))
  ;; conversation id -> the newest sequence id read
  (cursors (make-hash-table :test 'equal))
  ;; the access token, and the refresh token it came with
  (token nil)
  (refresh-token nil)
  (path nil))

(defun make-xchat-book (&key path token refresh-token)
  "A book at PATH (read back when it exists)."
  (let ((book (%make-xchat-book :token token :refresh-token refresh-token)))
    (setf (xchat-book-path book) path)
    (when (and path (probe-file path))
      (handler-case
          (with-open-file (in path :external-format :utf-8)
            (let ((data (with-standard-io-syntax
                          (let ((*read-eval* nil)) (read in)))))
              (loop for (conversation . sequence) in (getf data :cursors)
                    do (setf (gethash conversation (xchat-book-cursors book)) sequence))
              (setf (xchat-book-token book) (or (getf data :token) (xchat-book-token book))
                    (xchat-book-refresh-token book) (or (getf data :refresh-token)
                                                        (xchat-book-refresh-token book)))))
        (error (condition)
          (warn "xchat: the book at ~a is unreadable (~a); starting a fresh one" path condition))))
    book))

(defun keep-cursors (book)
  "Write BOOK's cursors and tokens to its path, atomically."
  (when (xchat-book-path book)
    (let ((data (sb-thread:with-mutex ((xchat-book-lock book))
                  (list :cursors (loop for key being the hash-keys of (xchat-book-cursors book)
                                         using (hash-value value)
                                       collect (cons key value))
                        :token (xchat-book-token book)
                        :refresh-token (xchat-book-refresh-token book))))
          (temp (make-pathname :type "tmp" :defaults (xchat-book-path book))))
      (ensure-directories-exist (xchat-book-path book))
      (with-open-file (out temp :direction :output :if-exists :supersede
                                :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-readably* nil)) (prin1 data out))))
      (rename-file temp (xchat-book-path book)))))

(defun xchat-cursor (book conversation-id)
  (sb-thread:with-mutex ((xchat-book-lock book))
    (gethash conversation-id (xchat-book-cursors book))))

(defun (setf xchat-cursor) (sequence book conversation-id)
  (sb-thread:with-mutex ((xchat-book-lock book))
    (setf (gethash conversation-id (xchat-book-cursors book)) sequence)))
