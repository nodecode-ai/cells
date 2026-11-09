;;;; tree.lisp --- the binary summary tree (the original's tree.py).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; node(l, i) covers messages [i*2^l, (i+1)*2^l). Level 0 is one message;
;;;; level l > 0 merges its two children (l-1, 2i) and (l-1, 2i+1). The tree
;;;; is purely binary: every parent comes from exactly its two children.
;;;;
;;;; A node is named id+n to a reader -- id is its first message, n = 2^l how
;;;; many messages it covers. The view shows 2184+8 and the agent calls
;;;; (optchat:zoom 2184 8) to open it into its two halves.
;;;;
;;;; Layout: <home>/optchat/tree/YYYY-MM-DD.jsonl, {"l": 0, "i": 42, "text":
;;;; "...", "date": "..."}. The tree is a cache in principle, but it costs
;;;; model calls to rebuild, so it is stored and never recomputed.

(in-package #:nodecode-optchat)

(defun node-key (level index)
  "The one key a node (LEVEL, INDEX) is filed under."
  (cons level index))

(defun node-text (level index)
  "The text of node (LEVEL, INDEX), or NIL while it is not built."
  (gethash (node-key level index) *nodes*))

(defun node-built-p (level index)
  "Whether node (LEVEL, INDEX) is built."
  (nth-value 1 (gethash (node-key level index) *nodes*)))

(defun node-count ()
  "How many nodes the tree holds."
  (hash-table-count *nodes*))

(defun set-node (level index text)
  "Store one built node: appended and flushed, never recomputed once stored."
  (with-optchat-lock
    (let ((path (merge-pathnames (format nil "~a.jsonl" (today-name)) (day-folder "tree"))))
      (append-line path (nlk:encode-json-object
                         (nlk:make-json-object "l" level "i" index
                                                "text" text "date" (iso-now))))
      (setf (gethash (node-key level index) *nodes*) text
            *render* nil)
      text)))

(defun load-tree ()
  "Read every node file into *NODES*; answers the count."
  (with-optchat-lock
    (setf *nodes* (make-hash-table :test 'equal))
    (dolist (path (day-files (day-folder "tree")))
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((object (ignore-errors (nlk:decode-json line))))
                   (when (hash-table-p object)
                     (let ((level (nlk:json-value object :integer "l"))
                           (index (nlk:json-value object :integer "i"))
                           (text (nlk:json-value object :string "text")))
                       (when (and level index text)
                         (setf (gethash (node-key level index) *nodes*) text))))))))
    (hash-table-count *nodes*)))

;;; --- structure --------------------------------------------------------------------

(defun level-span (level)
  "How many messages a node at LEVEL covers."
  (ash 1 level))

(defun node-address (level index)
  "(values ID N) for node (LEVEL, INDEX): its first message and its span."
  (values (* index (level-span level)) (level-span level)))

(defun address-level (n)
  "The level of a line covering N messages (1, 2, 4, 8, ...)."
  (1- (integer-length n)))

(defun node-readable-p (id n total)
  "Whether ID+N names a node of a log of TOTAL messages."
  (and (plusp n)
       (zerop (logand n (1- n)))
       (zerop (mod id n))
       (<= (+ id n) total)))

;;; --- zoom -------------------------------------------------------------------------

(defun zoom (id n)
  "Open the memory line ID+N into the two lines of N/2 messages it was made
from; N = 1 gives message ID in full. Answers text either way, so a line
that is not built says so instead of failing the call."
  (with-optchat-lock
    (let ((total (length *entries*)))
      (cond
        ((not (node-readable-p id n total))
         (format nil "No line ~d+~d." id n))
        ((= n 1)
         (let ((entry (entry-at id)))
           (if (null entry)
               (format nil "No line ~d+~d." id n)
               (format nil "~d+0|~a: ~a" id (entry-kind entry) (entry-text entry)))))
        (t
         (let* ((level (address-level n))
                (index (floor id n))
                (left (1- level))
                (a (node-text left (* 2 index)))
                (b (node-text left (1+ (* 2 index)))))
           (if (or (null a) (null b))
               (format nil "No line ~d+~d." id n)
               (multiple-value-bind (aid an) (node-address left (* 2 index))
                 (multiple-value-bind (bid bn) (node-address left (1+ (* 2 index)))
                   (format nil "~d+~d|~a~%~d+~d|~a"
                           aid an (flatten a) bid bn (flatten b)))))))))))

(defun message-date (id)
  "The date and time message ID was written, or \"\" when the log has none."
  (let ((entry (entry-at id)))
    (if entry (entry-date entry) "")))
