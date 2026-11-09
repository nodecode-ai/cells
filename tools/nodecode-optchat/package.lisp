;;;; package.lisp --- the NODECODE-OPTCHAT package and the live state the files share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed OPTCHAT so an eval form reads (optchat:zoom 12 4),
;;;; used qualified: its verbs are the cell's own words, not Lisp's.
;;;;
;;;; The memory itself is on disk, under the home: main/YYYY-MM-DD.jsonl is
;;;; the append-only log of every message, tree/YYYY-MM-DD.jsonl the summary
;;;; nodes built from it. What lives here is the warm copy -- the log's
;;;; messages, the node table, the view -- loaded at start and kept in step
;;;; with the frames of every turn. It is emptied at every stop.

(defpackage #:nodecode-optchat
  (:use #:cl)
  (:nicknames #:optchat)
  (:export
   ;; model-facing and operator vocabulary (the primer in cell.lisp)
   #:zoom
   #:message-date
   #:status
   ;; how many messages this home remembers
   #:remembered
   ;; the one condition
   #:optchat-error))

(in-package #:nodecode-optchat)

(nlk:define-peripheral optchat :not-running t :lock "optchat" :recursive t)

;;; --- the log's message ----------------------------------------------------------
;;; index in *LOG* is the message id: the log is append-only, so position is
;;; identity, and the "i" a line carries is written for readers of the file.

(defstruct (entry (:constructor make-entry (kind text date session)))
  "One logged message: KIND is user, talk, tool, echo or note; TEXT its words."
  (kind "note" :type string :read-only t)
  (text "" :type string :read-only t)
  (date "" :type string :read-only t)
  (session "" :type string :read-only t))

(defparameter +kinds+ '("user" "talk" "tool" "echo" "note")
  "The kinds a line may carry, as the original's store.py has them.")

(defparameter +cap-chars+ 30000
  "The most of one tool result the log keeps, head and tail, as provider.py's CAP.")

(defparameter +unbuilt+ "(not summarized yet: zoom it)"
  "What a view line says before its node is built, as view.py has it.")

(defparameter +node-default+ 512
  "The target size of one summary line in characters (the spec's NODE).")

(defparameter +budget-default+ 16000
  "Characters the whole view may take in a request (the original's view_budget_chars).")

;;; --- the live copy ---------------------------------------------------------------

(defvar *entries* (make-array 0 :adjustable t :fill-pointer 0)
  "The log, oldest first: message id is the index.")

(defvar *nodes* (make-hash-table :test 'equal)
  "The summary tree: (LEVEL . INDEX) -> the node's one-line text.")

(defvar *parts* '()
  "The view: a list of (LEVEL . INDEX) tiles over the whole log, oldest first.")

(defvar *folded* 0
  "How many messages the view has taken in; equals (LENGTH *ENTRIES*) between requests.")

(defvar *render* nil
  "The rendered <chat> block, or NIL once the view moved.")

(defvar *dirty* t
  "T when the view must fold or fit again before it is read.")

(defvar *worker* nil
  "The summary worker while it runs, or NIL.")

(defvar *use-worker* t
  "Whether a start runs the summary worker; a test drives PUMP-ONCE by hand.")

(defvar *last* nil
  "One line saying what the cell last did, or why it could not, or NIL.")

(defvar *directory* nil
  "The folder this start reads and writes, under the home: (SETTING :STORE-DIR).")

(defun clear-state ()
  "Empty every live table and drop the view and the worker handle."
  (with-optchat-lock
    (setf *entries* (make-array 0 :adjustable t :fill-pointer 0)
          *nodes* (make-hash-table :test 'equal)
          *parts* '()
          *folded* 0
          *render* nil
          *dirty* t
          *worker* nil
          *directory* nil
          *last* nil)))

(defvar *budget* +budget-default+
  "Characters the whole view may take in one request; (setting :view-budget-chars) at start.")

(defvar *node-size* +node-default+
  "The most characters one summary line may take; (setting :node-bytes) at start.")

(defvar *compaction* t
  "Whether this start runs the summarizer; (setting :compact-enabled) at start.")
