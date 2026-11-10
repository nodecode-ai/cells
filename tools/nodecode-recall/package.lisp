;;;; package.lisp --- the package, the index's structures and its one variable.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-recall
  (:use #:cl)
  (:nicknames #:recall)
  (:shadow #:search)
  (:export #:start-cell #:stop-cell #:setting #:*recall* #:recall-error
           #:index-files #:search #:open-document #:stats-object #:forget-files
           #:ambient-block #:guard-path #:index-path #:+manual+
           #:+index-schema+ #:+search-schema+ #:+open-schema+ #:+stats-schema+
           #:+forget-schema+))

(in-package #:nodecode-recall)

;; The condition, FAIL, *RECALL*, RUNNING-SETTINGS and the running settings
;; refusal; DEFINE-CELL below derives START-CELL and STOP-CELL.
(nlk:define-peripheral recall :not-running t)

;;; --- the index -----------------------------------------------------------
;; One file per indexed document, its chunks kept whole so a search can quote
;; the line it matched. The postings and the chunk lengths are derived from
;; the files at load and never written: the file on disk is the corpus, the
;; tables are what one process reads it into.

(defstruct (remembered (:constructor make-remembered (path mtime size ext skipped chunks)))
  "One indexed file: its path, the stamp it was read at, and its chunks."
  path mtime size ext skipped chunks)

(defstruct (recall-index (:constructor %make-recall-index))
  "The corpus one process holds: files by path, the term postings and the
per-chunk lengths a query scores against."
  (files (make-hash-table :test #'equal))
  (meta (make-hash-table :test #'equal))
  (postings (make-hash-table :test #'equal))
  (lengths (make-hash-table :test #'equal))
  (chunks 0)
  (tokens 0))

(defvar *index* nil
  "The loaded index, or NIL before the first read and after STOP-CELL.")

(defvar *index-lock* (bt2:make-recursive-lock :name "recall index")
  "One writer at a time: an ambient hint reads while a refresh writes.")

(defvar *searched* (make-hash-table :test #'equal)
  "Session id -> the paths an ambient hint already showed it, newest bounded.")

(defparameter *index-file-name* "recall/index.sexp"
  "Where the index lives under the organism's home.")
(defparameter *open-char-cap* 24000
  "Characters RECALL_OPEN returns unless the call asks for another window.")

(defun index-path ()
  "The index file this home keeps."
  (nlk:home *index-file-name*))
