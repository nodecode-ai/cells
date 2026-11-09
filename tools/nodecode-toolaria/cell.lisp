;;;; cell.lisp --- Toolaria: spill oversized tool results, fetch slices back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Sahil-SS9/Toolaria (MIT). The original is a Hermes
;;;; plugin with a transform_tool_result hook: a tool result over a threshold
;;;; is written to a content-addressed blob store, and the model gets a short
;;;; excerpt plus a handle it can fetch slices of. This is the same half, on
;;;; the core's own seam: one hook on the :TOOL point sees every tool result
;;;; text before it enters the context, one tool (rescuer_fetch) reads a blob
;;;; back by slice, one command (/rescuer) reports the store.
;;;;
;;;; Retrieval modes: outline, search, grep, range, stat, full. What the
;;;; rewrite leaves out is at the tail of this file's own comment: no
;;;; embeddings, no pass-by-reference middleware, no encryption.

(defpackage #:nodecode-toolaria
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:rescue-tool-result #:rescuer-fetch #:fetch #:rescue-text
           #:status #:blob-id-of #:store-dir #:valid-id-p))

(in-package #:nodecode-toolaria)

(nlk:define-peripheral toolaria :not-running t)

;;; --- the store ----------------------------------------------------------------

(defun home-expanded (path)
  "PATH with a leading ~ replaced by the home directory, as a string."
  (if (and (> (length path) 1) (char= #\~ (char path 0)) (char= #\/ (char path 1)))
      (namestring (merge-pathnames (subseq path 2) (user-homedir-pathname)))
      path))

(defun store-dir ()
  "The folder this cell's blobs live in: store_path plus blobs/."
  (uiop:ensure-directory-pathname
   (merge-pathnames "blobs/" (home-expanded (setting :store-path)))))

(defun blob-path (id) (merge-pathnames (format nil "~a.txt" id) (store-dir)))

(defun meta-path (id) (merge-pathnames (format nil "~a.meta" id) (store-dir)))

(defun blob-id-of (text)
  "TEXT's handle: the first 12 hex characters of its SHA-256."
  (subseq (nlk:sha256-text text) 7 19))

(defun valid-id-p (id)
  "ID is a handle this store could have written: 12 lowercase hex characters."
  (and (stringp id)
       (= 12 (length id))
       (every (lambda (c) (find c "0123456789abcdef")) id)))

(defun now-string ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun put-blob (text tool)
  "Write TEXT under its handle unless it is already there; answer the handle."
  (let* ((id (blob-id-of text))
         (path (blob-path id)))
    (ensure-directories-exist path)
    (unless (probe-file path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
        (write-string text out)))
    (with-open-file (out (meta-path id) :direction :output :if-exists :supersede
                                           :if-does-not-exist :create
                                           :external-format :utf-8)
      (format out "tool=~a at=~a~%" (or tool "") (now-string)))
    id))

(defun get-blob (id)
  "TEXT of blob ID, or NIL when it is not there."
  (let ((path (blob-path id)))
    (when (probe-file path)
      (handler-case (uiop:read-file-string path :external-format :utf-8)
        (error () nil)))))

(defun blob-tool (id)
  "The tool the blob was last rescued from, or NIL."
  (let ((path (meta-path id)))
    (when (probe-file path)
      (let ((line (car (lines-of (uiop:read-file-string path)))))
        (when (and line (uiop:string-prefix-p "tool=" line))
          (subseq line 5 (or (search " at=" line) (length line))))))))

(defun blob-ids ()
  "Every handle in the store, newest first."
  (let ((files (and (probe-file (store-dir))
                    (directory (merge-pathnames "*.txt" (store-dir))))))
    (mapcar (lambda (path) (pathname-name path))
            (sort files #'> :key (lambda (path) (or (ignore-errors (file-write-date path)) 0))))))

(defun sweep ()
  "Delete blobs older than ttl_hours. Answers how many went."
  (let ((cutoff (- (get-universal-time) (* 3600 (setting :ttl-hours))))
        (gone 0))
    (dolist (id (blob-ids))
      (let ((path (blob-path id)))
        (when (and (probe-file path) (< (or (ignore-errors (file-write-date path)) 0) cutoff))
          (ignore-errors (delete-file path))
          (when (probe-file (meta-path id)) (ignore-errors (delete-file (meta-path id))))
          (incf gone))))
    gone))

;;; --- the excerpt and the handle --------------------------------------------------

(defun lines-of (text)
  "TEXT split at newlines, terminators dropped."
  (let ((lines '()) (start 0))
    (loop for at = (position #\Newline text :start start)
          while at
          do (push (subseq text start at) lines)
             (setf start (1+ at)))
    (push (subseq text start) lines)
    (nreverse lines)))

(defun cap-chars (text limit)
  "TEXT cut to LIMIT characters => (values TEXT TRUNCATED-P)."
  (if (<= (length text) limit)
      (values text nil)
      (values (concatenate 'string (subseq text 0 (max 0 (- limit 24)))
                           "\n[... truncated ...]")
              t)))

(defun excerpt-of (text)
  "The head and tail of TEXT, within excerpt_max_chars => (values TEXT TRUNCATED-P)."
  (let* ((lines (lines-of text))
         (n (length lines))
         (head (setting :head-lines))
         (tail (setting :tail-lines)))
    (if (<= n (+ head tail 2))
        (cap-chars text (setting :excerpt-max-chars))
        (cap-chars (format nil "~{~a~^~%~}~%~%[... ~:d lines omitted ...]~%~%~{~a~^~%~}"
                            (subseq lines 0 head) (- n head tail)
                            (subseq lines (- n tail)))
                   (setting :excerpt-max-chars)))))

(defparameter +handle-opens+ "[Toolaria: tool result rescued."
  "How every handle line opens, which is also how a rescued text is recognized.")

(defun rescue-text (text tool)
  "Store TEXT, and answer the excerpt plus handle the model reads instead."
  (let* ((id (put-blob text tool))
         (n (length (lines-of text))))
    (multiple-value-bind (excerpt truncated) (excerpt-of text)
      (format nil "~a~%~a~%~a~%~a"
              (format nil "[Toolaria: tool result rescued. tool=~a; at=~a; size=~:d chars; lines=~:d; blob=~a]"
                      (or tool "") (now-string) (length text) n id)
              (if truncated
                  (format nil "Preview (budget ~:d chars, truncated); this is a preview, NOT the full output:"
                          (setting :excerpt-max-chars))
                  (format nil "Preview (first ~d / last ~d lines); this is a preview, NOT the full output:"
                          (setting :head-lines) (setting :tail-lines)))
              excerpt
              (format nil "Retrieve more with rescuer_fetch(id=\"~a\", mode=...): outline | search(query) | grep(pattern) | range(start,count) | stat | full"
                      id)))))

;;; --- the hook --------------------------------------------------------------------

(defun excluded-p (tool)
  "TOOL is one the section keeps out of the store."
  (member (string-downcase (princ-to-string tool))
          (mapcar (lambda (name) (string-downcase (princ-to-string name)))
                  (coerce (setting :keep-out) 'list))
          :test #'equal))

(defun rescue-tool-result (op next)
  "The :TOOL hook: run the call, then spill its text when it is too big.
OP is (:name TOOL-NAME :arguments HASH :call-id ID). An error of its own
never fails the call: the text the tool answered goes through untouched."
  (let* ((tool (or (getf op :name) ""))
         (text (funcall next op)))
    (if (and (stringp text)
             (plusp (length text))
             (not (excluded-p tool))
             (not (uiop:string-prefix-p +handle-opens+ text))
             (> (length text) (setting :max-result-chars)))
        (handler-case (rescue-text text tool)
          (error () text))
        text)))

;;; --- the tool --------------------------------------------------------------------

(defparameter +modes+ '("outline" "search" "grep" "range" "stat" "full")
  "Every mode rescuer_fetch answers.")

(nlk:define-contract fetch-call
  (:doc "Fetch slices of a rescued oversized tool result: outline (structure), search (by meaning), grep (regex), range (lines), stat (what it is), full (everything).")
  ("id" :text :required t
   :doc "the blob handle from the rescue line, 12 hex characters")
  ("mode" :text :default "stat" :options +modes+
   :doc "outline, search, grep, range, stat or full; stat when left out")
  ("start" :integer :default 0
   :doc "the first line range returns, counting from 0")
  ("count" :integer :default 40 :slot want
   :doc "how many lines range returns, and how wide grep's answers are")
  ("pattern" :text :default ""
   :doc "the regex grep matches, one line at a time")
  ("query" :text :default ""
   :doc "what search looks for, in plain words"))

(defun clip (text limit)
  (if (> (length text) limit) (concatenate 'string (subseq text 0 limit) "...") text))

(defun outline-text (text id)
  (let* ((lines (lines-of text))
         (heads '())
         (blank t))
    (let ((index 0))
      (dolist (line lines)
        (cond ((every (lambda (c) (find c " \t\r")) line) (setf blank t))
              (blank (when (< (length heads) 40)
                       (push (format nil "  ~:d: ~a" index (clip (string-trim " \t\r" line) 120)) heads))
                     (setf blank nil)))
        (incf index)))
    (format nil "blob ~a: ~:d chars, ~:d lines~%~@[~a~%~]blocks (~:d shown):~%~{~a~^~%~}"
            id (length text) (length lines)
            (let ((tool (blob-tool id))) (and tool (plusp (length tool)) (format nil "from ~a" tool)))
            (length heads) (nreverse heads))))

(defun grep-text (text id pattern want)
  (when (zerop (length pattern))
    (return-from grep-text "Error: grep needs pattern"))
  (let ((hits '()) (index 0) (seen 0))
    (dolist (line (lines-of text))
      (let ((hay (clip line 2000)))
        (when (handler-case (cl-ppcre:scan pattern hay) (error () nil))
          (incf seen)
          (when (< (length hits) (max 1 want))
            (push (format nil "  ~:d: ~a" index (clip line 240)) hits))))
      (incf index))
    (if (null hits)
        (format nil "blob ~a: no line matches ~s" id pattern)
        (format nil "blob ~a: ~:d matching line~:p~@[, first ~d shown~]~%~{~a~^~%~}"
                id seen (max 1 want) (nreverse hits)))))

(defun search-text (text id query want)
  (let* ((terms (remove "" (uiop:split-string (string-downcase query)
                                               :separator '(#\Space #\Tab #\Newline #\, #\.))))
         (scored '())
         (index 0))
    (when (null terms) (return-from search-text "Error: search needs query"))
    (dolist (line (lines-of text))
      (let* ((hay (string-downcase (clip line 2000)))
             (score (count-if (lambda (term) (search term hay)) terms)))
        (when (plusp score) (push (cons score (format nil "  ~:d: ~a" index (clip line 240))) scored)))
      (incf index))
    (let ((top (subseq (sort scored #'> :key #'car) 0 (min (length scored) (max 1 want)))))
      (if (null top)
          (format nil "blob ~a: nothing matches ~s" id query)
          (format nil "blob ~a: best ~d line~:p for ~s~%~{~a~^~%~}"
                  id (length top) query (mapcar #'cdr top))))))

(defun range-text (text id start want)
  (let* ((lines (lines-of text))
         (from (max 0 start))
         (to (min (length lines) (+ from (max 1 want)))))
    (if (>= from (length lines))
        (format nil "blob ~a has ~:d lines; line ~:d is past the end" id (length lines) from)
        (multiple-value-bind (body truncated)
            (cap-chars (format nil "~{~a~^~%~}" (subseq lines from to)) (setting :fetch-max-chars))
          (format nil "blob ~a: lines ~:d-~:d of ~:d~@[ (cut at ~:d chars)~]~%~a"
                  id from (1- to) (length lines) (and truncated (setting :fetch-max-chars)) body)))))

(defun full-text (text id)
  (if (> (length text) (setting :full-fetch-max-chars))
      (format nil "blob ~a is ~:d chars, over full_fetch_max_chars (~:d); use mode=range or mode=outline"
              id (length text) (setting :full-fetch-max-chars))
      text))

(defun stat-text (text id)
  (format nil "blob ~a: ~:d chars, ~:d lines~@[; from ~a~]~%; modes: outline | search(query=...) | grep(pattern=...) | range(start=..., count=...) | stat | full"
          id (length text) (length (lines-of text))
          (let ((tool (blob-tool id))) (and tool (plusp (length tool)) tool))))

(defun fetch (id &key (mode "stat") (start 0) (want 40) (pattern "") (query ""))
  "ID's content, by MODE. Answers TEXT: an error line when the blob is gone."
  (let ((id (string-downcase (or id ""))))
    (cond ((not (valid-id-p id))
           (format nil "Error: invalid blob id ~s (expected 12 hex characters)" id))
          (t (let ((text (get-blob id)))
               (if (null text)
                   (format nil "Error: no rescued blob ~a; it was swept after ~d hours or never existed"
                           id (setting :ttl-hours))
                   (handler-case
                       (cond ((equal mode "stat") (stat-text text id))
                             ((equal mode "outline") (outline-text text id))
                             ((equal mode "grep") (grep-text text id pattern want))
                             ((equal mode "search") (search-text text id query want))
                             ((equal mode "range") (range-text text id start want))
                             ((equal mode "full") (full-text text id))
                             (t (format nil "Error: unknown mode ~s; use one of ~{~a~^, ~}"
                                        mode +modes+)))
                     (error (condition) (format nil "Error: ~a" condition)))))))))

(defun rescuer-fetch (call)
  "The rescuer_fetch tool body: one decoded FETCH-CALL in, text out."
  (fetch (fetch-call-id call)
         :mode (fetch-call-mode call)
         :start (fetch-call-start call)
         :want (fetch-call-want call)
         :pattern (fetch-call-pattern call)
         :query (fetch-call-query call)))

;;; --- /rescuer, the manual -----------------------------------------------------

(defun status (&optional args)
  "Answer the store's line: how many blobs, how much, where, and the switches."
  (declare (ignore args))
  (let* ((ids (blob-ids))
         (bytes (loop for id in ids
                      sum (or (ignore-errors (with-open-file (in (blob-path id)) (file-length in)))
                              0))))
    (format nil "Toolaria: ~:d blob~:p, ~,1f KB in ~a~%; rescues results over ~:d chars; ~d head + ~d tail lines, excerpt budget ~:d chars; ttl ~d h~%; keep out: ~{~a~^, ~}"
            (length ids) (/ bytes 1024.0) (namestring (store-dir))
            (setting :max-result-chars) (setting :head-lines) (setting :tail-lines)
            (setting :excerpt-max-chars) (setting :ttl-hours)
            (coerce (setting :keep-out) 'list))))

(defparameter +primer+
  "Toolaria spills oversized tool results to disk. A result over max_result_chars
comes back as a short preview plus one handle line ending blob=<id>; the full
text stays on disk.

(rescuer_fetch :id \"<id>\" :mode \"range\" :start 0 :count 40)  lines.
(rescuer_fetch :id \"<id>\" :mode \"grep\" :pattern \"failed\")     regex, with line numbers.
(rescuer_fetch :id \"<id>\" :mode \"search\" :query \"payment errors\")  the lines that
    answer a question, ranked by the words they share with it.
(rescuer_fetch :id \"<id>\" :mode \"outline\")  structure: blocks and their first lines.
(rescuer_fetch :id \"<id>\" :mode \"stat\")     size, line count, modes.
(rescuer_fetch :id \"<id>\" :mode \"full\")     everything, under full_fetch_max_chars.

Read the preview first; fetch only the slices you need. /rescuer shows the
store: how many blobs, how much disk, where, and which tools are kept out."
  "What (help :toolaria) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell toolaria
  (:section ("toolaria")
    (:guide "nothing here is required: a result over max_result_chars is spilled to store_path and fetched back by slice; keep_out names tools never spilled")
    ("max_result_chars" :integer :default 12000 :min 200
     :doc "a tool result longer than this is rescued")
    ("head_lines" :integer :default 40 :min 0
     :doc "lines of the result the preview keeps from its start")
    ("tail_lines" :integer :default 15 :min 0
     :doc "lines the preview keeps from its end")
    ("excerpt_max_chars" :integer :default 8000 :min 200
     :doc "the hard cap on the preview, marker included")
    ("fetch_max_chars" :integer :default 4000 :min 200
     :doc "the cap on one range or grep answer")
    ("full_fetch_max_chars" :integer :default 50000 :min 1000
     :doc "full mode refuses a blob over this; range is the way in")
    ("store_path" :string :default "~/.nodecode/toolaria"
     :doc "where the rescued blobs live; ~ is your home directory")
    ("ttl_hours" :integer :default 72 :min 1
     :doc "how long a blob stays; older ones are swept at start")
    ("keep_out" :list :default '("rescuer_fetch" "eval")
     :doc "tools whose results are never spilled, by name"))
  (:tool "rescuer_fetch" (nlk:contract 'fetch-call) 'rescuer-fetch)
  (:help :toolaria "Toolaria: oversized tool results spilled to disk, fetched back by slice"
         +primer+)
  (:hook :tool #'rescue-tool-result)
  (:command "rescuer" 'status
            :description "Toolaria: the rescued blobs, their size, and the switches")
  (:start (lambda ()
            (ensure-directories-exist (merge-pathnames "b" (store-dir)))
            (sweep))))
