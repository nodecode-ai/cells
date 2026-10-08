;;;; verbs.lisp --- what the model calls, and the diagnostics a write earns.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every verb answers a string, stamps its call on the result fact before
;;;; it acts (so a refusal is stamped too), and waits at most +VERB-SECONDS+:
;;;; an eval yields to the background at ten, and a verb that would outlast
;;;; that says "try again" instead. Output follows omp: a diagnostic is
;;;; `path:line:col [severity] [source] message (code)', errors first; a
;;;; location is `path:line:col' and the line it points at. Paths read
;;;; relative to the session's directory when they are inside it.
;;;;
;;;; The write path is two hooks. NOTE-WRITE, around the core's one atomic
;;;; writer, records a written file that some server serves, under the
;;;; session the write was for; the eval that wrote it runs on a worker
;;;; thread of its own, so the record is a table, not a binding.
;;;; DIAGNOSE-WRITES, on the :tool point, drains the session's record after
;;;; an eval and appends what the servers say about those files, inside
;;;; wait_ms. An eval that went to the background leaves the record for the
;;;; eval that reads its end.

(in-package #:nodecode-lsp)

(defparameter +verb-seconds+ 8
  "The longest a verb waits, inside the eval's ten-second yield window.")

(defparameter +block-diagnostics+ 50
  "The most diagnostics the block after a write lists.")

(defparameter +block-characters+ 4000
  "The most characters the block after a write takes.")

(defparameter +block-files+ 10
  "The most written files one eval's block checks.")

(defparameter +list-limit+ 100
  "The most lines a verb's list answers with.")

;;; --- paths -------------------------------------------------------------------

(defun session-bound ()
  "The running turn's repository root, with its trailing slash, or NIL."
  (let* ((session (getf (nle:turn) :session-id))
         (root (and session (ignore-errors (nlk:session-project-root session)))))
    (and root (concatenate 'string (string-right-trim "/" (native root)) "/"))))

(defun session-directory ()
  "The running turn's session directory, with its trailing slash, or NIL."
  (let ((directory (ignore-errors (nle::ambient-session-directory))))
    (when directory
      (let ((found (probe-file (uiop:ensure-directory-pathname directory))))
        (and found (native found))))))

(defun display-path (path &optional (base (session-directory)))
  "PATH relative to BASE when it is inside it, else whole."
  (if (and base (uiop:string-prefix-p base path))
      (subseq path (length base))
      path))

(defun target-path (path)
  "PATH, as the model wrote it, resolved to an existing file's native path."
  (unless (and (or (stringp path) (pathnamep path)) (plusp (length (native path))))
    (fail "a path must be a string naming a file, not ~s" path))
  (let* ((resolved (nle::eval-snippet-path path))
         (file (probe-file resolved)))
    (cond ((null file) (fail "~a does not exist" (native resolved)))
          ((uiop:directory-pathname-p file) (fail "~a is a directory; name a file" (native file)))
          (t (native file)))))

;;; --- servers for a file ---------------------------------------------------------

(defun servers-for (path)
  "The servers PATH gets, started when they are not running.
=> (values SERVERS PASSED), PASSED why the others were passed over."
  (multiple-value-bind (chosen passed)
      (file-servers path (getf (running-settings) :servers) (session-bound))
    (values (loop for (spec root argv) in chosen collect (server-for spec root argv))
            passed)))

(defun no-server-text (path passed)
  (if passed
      (format nil "no language server for ~a: ~{~a~^; ~}" (file-name path) passed)
      (format nil "no language server is configured for ~a files; lsp.servers in ~
                   ~~/.nodecode/config.jsonc adds one"
              (let ((extension (file-extension path)))
                (if (string= extension "") (file-name path) (format nil ".~a" extension))))))

(defun ready (server deadline)
  "SERVER once it is running, or a refusal that says why it is not."
  (case (await-state server deadline)
    (:ready (touch server))
    (:starting (fail "~a is still starting in ~a; try again in a few seconds"
                     (srv-name server) (srv-root server)))
    (t (fail "~a is not running: ~a" (srv-name server) (or (srv-failure server) "stopped")))))

(defun primary (path deadline)
  "The server that answers questions about PATH: the first non-linter."
  (multiple-value-bind (servers passed) (servers-for path)
    (unless servers
      (fail "~a" (no-server-text path passed)))
    (ready (first servers) deadline)))

(defun ask (server path method params deadline)
  "SERVER's answer to METHOD about PATH, every open document synced first;
NIL for null. Refuses while it is still indexing, and on any failure."
  (refresh-documents server path)
  (sync-document server path)
  (unless (await-loaded server deadline)
    (fail "~a is still indexing ~a; try again in a few seconds" (srv-name server) (srv-root server)))
  (multiple-value-bind (result state detail)
      (call (srv-conn server) method params :seconds (max 0.1 (seconds-left deadline)))
    (ecase state
      (:ok (if (eq result :null) nil result))
      (:timeout (fail "~a did not answer ~a within ~d s; it may still be indexing, try again"
                      (srv-name server) method +verb-seconds+))
      (:error (fail "~a refused ~a: ~a" (srv-name server) method detail))
      (:closed (fail "~a stopped while answering; ~a" (srv-name server)
                     (or (srv-failure server) "see (lsp:status)"))))))

(defun symbol-params (server path symbol line)
  "The textDocument/position params of SYMBOL in PATH, as SERVER counts."
  (let ((text (or (read-file path) (fail "cannot read ~a" path))))
    (unless (stringp symbol)
      (fail "symbol must be a string, not ~s" symbol))
    (multiple-value-bind (line-index index) (locate-symbol text symbol :line line)
      (declare (ignore line-index))
      (nlk:json-object "textDocument" (text-document path)
                       "position" (lsp-position text (line-starts text) index (srv-encoding server))))))

;;; --- formatting ------------------------------------------------------------------

(defun file-lines (cache path)
  "(TEXT . STARTS) of PATH, read once per CACHE, or NIL."
  (multiple-value-bind (entry present) (gethash path cache)
    (if present
        entry
        (setf (gethash path cache)
              (let ((text (read-file path))) (and text (cons text (line-starts text))))))))

(defun location-line (server cache path line character &key (context t))
  "`path:line:col' for the LSP position, with the line's text under CONTEXT."
  (let ((file (file-lines cache path)))
    (format nil "~a:~d:~d~@[  ~a~]" (display-path path) (1+ line)
            (if file
                (position-column (car file) (cdr file) line character (srv-encoding server))
                (1+ character))
            (and context file (string-trim '(#\Space #\Tab) (line-text (car file) (cdr file) line))))))

(defun locations (result)
  "RESULT -- a Location, Locations or LocationLinks -- as ((PATH LINE CHARACTER)...)."
  (loop for item in (cond ((null result) '())
                          ((hash-table-p result) (list result))
                          ((vectorp result) (coerce result 'list)))
        for path = (uri-path (or (nlk:json-value item :string "uri")
                                 (nlk:json-value item :string "targetUri")))
        for range = (or (nlk:json-value item :object "targetSelectionRange")
                        (nlk:json-value item :object "range")
                        (nlk:json-value item :object "targetRange"))
        when (and path range)
          collect (list path
                        (or (nlk:json-value range :integer "start" "line") 0)
                        (or (nlk:json-value range :integer "start" "character") 0))))

(defun capped (lines &optional (limit +list-limit+) more)
  "LINES joined, at most LIMIT of them, a last line counting the rest."
  (format nil "~{~a~^~%~}~@[~%... ~d more~@[; ~a~]~]"
          (subseq lines 0 (min limit (length lines)))
          (and (> (length lines) limit) (- (length lines) limit)) more))

(defparameter +severities+ #("error" "warning" "info" "hint"))

(defun severity (diagnostic)
  "DIAGNOSTIC's severity, 1 (error) to 4 (hint); none given is an error."
  (let ((severity (nlk:json-value diagnostic :integer "severity")))
    (if (and severity (<= 1 severity 4)) severity 1)))

(defun clean-message (message first-line)
  "MESSAGE without the further-information lines and bare URLs some servers
add; its first line alone under FIRST-LINE."
  (let ((lines (remove-if (lambda (line)
                            (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                              (or (uiop:string-prefix-p "for further information visit" trimmed)
                                  (ppcre:scan "^https?://\\S*$" trimmed))))
                          (uiop:split-string (or message "") :separator '(#\Newline)))))
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (if first-line
                     (or (find-if (lambda (line) (plusp (length (string-trim " " line)))) lines) "")
                     (format nil "~{~a~^~%~}" lines)))))

(defun diagnostic-line (shown diagnostic &key first-line)
  "One DIAGNOSTIC as omp writes it, for the file SHOWN as it is."
  (let* ((start (nlk:json-value diagnostic :object "range" "start"))
         (code (gethash "code" diagnostic))
         (source (nlk:json-value diagnostic :text "source")))
    (format nil "~a:~d:~d [~a] ~@[[~a] ~]~a~@[ (~a)~]"
            shown
            (1+ (or (nlk:json-value start :integer "line") 0))
            (1+ (or (nlk:json-value start :integer "character") 0))
            (aref +severities+ (1- (severity diagnostic)))
            source
            (clean-message (nlk:json-value diagnostic :string "message") first-line)
            (and (or (stringp code) (integerp code)) (not (equal code "")) code))))

(defun sorted-unique (items)
  "ITEMS, diagnostics from any number of servers, without repeats (same range
and message), by severity, then position, then message."
  (flet ((point (d key what) (or (nlk:json-value d :integer "range" key what) 0)))
    (let ((seen (make-hash-table :test 'equal)))
      (sort (remove-if (lambda (d)
                         (let ((key (list (point d "start" "line") (point d "start" "character")
                                          (point d "end" "line") (point d "end" "character")
                                          (nlk:json-value d :string "message"))))
                           (prog1 (gethash key seen) (setf (gethash key seen) t))))
                       (coerce items 'list))
            (lambda (a b)
              (let ((ka (list (severity a) (point a "start" "line") (point a "start" "character")))
                    (kb (list (severity b) (point b "start" "line") (point b "start" "character"))))
                (loop for x in ka for y in kb
                      do (cond ((< x y) (return t)) ((> x y) (return nil)))
                      finally (return (string< (or (nlk:json-value a :string "message") "")
                                               (or (nlk:json-value b :string "message") ""))))))))))

(defun counts-text (diagnostics)
  "`1 error, 2 warnings' for DIAGNOSTICS; NIL for none."
  (let ((counts (make-array 4 :initial-element 0)))
    (dolist (d diagnostics) (incf (aref counts (1- (severity d)))))
    (let ((parts (loop for count across counts
                       for name in '("error" "warning" "info" "hint")
                       when (plusp count)
                         collect (format nil "~d ~a~a" count name
                                         (if (and (/= count 1) (string/= name "info")) "s" "")))))
      (and parts (format nil "~{~a~^, ~}" parts)))))

(defun diagnostics-call (files &optional (base (session-directory)))
  "The (lsp:diagnostics ...) form that asks about FILES again."
  (let ((shown (mapcar (lambda (file) (display-path file base)) files)))
    (if (rest shown)
        (format nil "(lsp:diagnostics '~s)" shown)
        (format nil "(lsp:diagnostics ~s)" (first shown)))))

(defun job-groups (jobs)
  "JOBS by file, in the order the files came: ((PATH ITEMS PENDING FAILED
ANSWERED)...), ITEMS every server's diagnostics, PENDING and FAILED the
servers that did not answer, ANSWERED whether one did."
  (let ((groups '()))
    (dolist (job jobs)
      (let ((group (or (assoc (job-path job) groups :test #'string=)
                       (car (push (list (job-path job) '() '() '() nil) groups)))))
        (case (job-phase job)
          (:done (setf (second group) (append (second group) (coerce (job-items job) 'list))
                       (fifth group) t))
          ((:start :wait) (push (job-server job) (third group)))
          (:failed (push (job-server job) (fourth group))))))
    (nreverse groups)))

(defun file-report (group single)
  "One file's GROUP (JOB-GROUPS) as the diagnostics verb answers it: OK, or
its count and its diagnostics; the file named first unless SINGLE."
  (destructuring-bind (path items pending failed answered) group
    (let* ((shown (display-path path))
           (sorted (sorted-unique items))
           (counts (counts-text sorted))
           (name (if single "" (format nil "~a: " shown))))
      (format nil "~{~a~^~%~}"
              (append
               (cond (counts (list (format nil "~a~a:" name counts)
                                   (capped (mapcar (lambda (d) (diagnostic-line shown d)) sorted))))
                     (answered (list (format nil "~aOK" name))))
               (mapcar (lambda (server)
                         (format nil "lsp: ~a is still checking ~a; ask again in a moment"
                                 (srv-name server) shown))
                       pending)
               (mapcar (lambda (server)
                         (format nil "lsp: ~a is not running: ~a" (srv-name server)
                                 (or (srv-failure server) "stopped")))
                       failed))))))

;;; --- the verbs ---------------------------------------------------------------------

(defmacro stamped ((verb family source) &body body)
  "BODY as the verb VERB of FAMILY about SOURCE: the call stamped on the
result fact, then the cell's refusal while it is not running. A verb of no
FAMILY is recorded but names no activity: a transcript calls it a snippet."
  `(progn
     (nle:receipt "calls" (list* :verb ,verb
                                 (append (and ,family (list :family ,family))
                                         (let ((source ,source))
                                           (and (or (stringp source) (pathnamep source))
                                                (list :source (native source)))))))
     (running-settings)
     ,@body))

(defun diagnostics (paths)
  "What the language servers say about the file PATHS names, or each of a list."
  ;; Re-reads every file from disk first. One file: `OK', or a count line
  ;; and the diagnostics, errors first. Several: a section per file.
  (stamped ("lsp:diagnostics" "read" (if (listp paths) (first paths) paths))
    (let* ((paths (mapcar #'target-path (if (listp paths) paths (list paths))))
           (notes '())
           (pairs (loop for path in paths
                        nconc (multiple-value-bind (servers passed) (servers-for path)
                                (unless servers
                                  (push (format nil "~a: ~a" (display-path path) (no-server-text path passed))
                                        notes))
                                (loop for server in servers collect (cons server path)))))
           (groups (job-groups (collect-diagnostics pairs (deadline-after +verb-seconds+)))))
      (format nil "~{~a~^~%~}"
              (append (nreverse notes)
                      (loop for group in groups
                            collect (file-report group (= 1 (length paths)))))))))

(defun definition (path symbol &key line)
  "Where SYMBOL, as it appears in the file PATH, is defined."
  ;; SYMBOL is found in the file by name, on a word boundary when it is a
  ;; bare identifier: its first mention, `parse#2' its second, :line N on
  ;; that line.
  (stamped ("lsp:definition" "read" path)
    (let* ((path (target-path path))
           (deadline (deadline-after +verb-seconds+))
           (server (primary path deadline))
           (found (locations (ask server path "textDocument/definition"
                                  (symbol-params server path symbol line) deadline)))
           (cache (make-hash-table :test 'equal)))
      (if found
          (capped (loop for (file at character) in found
                        collect (location-line server cache file at character)))
          (format nil "~a found no definition of ~a" (srv-name server) symbol)))))

(defun references (path symbol &key line)
  "Every reference to SYMBOL, as it appears in the file PATH, its declaration included."
  (stamped ("lsp:references" "read" path)
    (let* ((path (target-path path))
           (deadline (deadline-after +verb-seconds+))
           (server (primary path deadline))
           (params (symbol-params server path symbol line))
           (cache (make-hash-table :test 'equal)))
      (setf (gethash "context" params) (nlk:json-object "includeDeclaration" t))
      (let ((found (locations (ask server path "textDocument/references" params deadline))))
        (if found
            (format nil "~d reference~:p:~%~a" (length found)
                    (capped (loop for (file at character) in found
                                  collect (location-line server cache file at character))))
            (format nil "~a found no references to ~a" (srv-name server) symbol))))))

(defun hover-text (contents)
  "Hover CONTENTS -- markup, a marked string, or a list of them -- as text."
  (cond ((stringp contents) contents)
        ((hash-table-p contents) (or (nlk:json-value contents :string "value") ""))
        ((vectorp contents)
         (format nil "~{~a~^~%~%~}" (remove "" (map 'list #'hover-text contents) :test #'string=)))
        (t "")))

(defun hover (path symbol &key line)
  "What the server says SYMBOL, as it appears in the file PATH, is: its type, its docs."
  (stamped ("lsp:hover" "read" path)
    (let* ((path (target-path path))
           (deadline (deadline-after +verb-seconds+))
           (server (primary path deadline))
           (result (ask server path "textDocument/hover" (symbol-params server path symbol line) deadline))
           (text (string-trim '(#\Space #\Newline) (hover-text (nlk:json-value result :any "contents")))))
      (if (plusp (length text))
          text
          (format nil "~a has nothing to say about ~a" (srv-name server) symbol)))))

(defparameter +symbol-kinds+
  #("file" "module" "namespace" "package" "class" "method" "property" "field" "constructor"
    "enum" "interface" "function" "variable" "constant" "string" "number" "boolean" "array"
    "object" "key" "null" "enum member" "struct" "event" "operator" "type parameter"))

(defun kind-name (kind)
  (if (and (integerp kind) (<= 1 kind 26)) (aref +symbol-kinds+ (1- kind)) "symbol"))

(defun outline (symbols depth)
  "Document SYMBOLS, a DocumentSymbol tree, one line each, children indented."
  (loop for symbol across symbols
        nconc (cons (format nil "~a~a (~a) :~d" (make-string (* 2 depth) :initial-element #\Space)
                            (nlk:json-value symbol :string "name")
                            (kind-name (nlk:json-value symbol :integer "kind"))
                            (1+ (or (nlk:json-value symbol :integer "selectionRange" "start" "line")
                                    (nlk:json-value symbol :integer "range" "start" "line") 0)))
                    (outline (nlk:json-array symbol "children") (1+ depth)))))

(defun symbol-information (server cache symbols)
  "SymbolInformation SYMBOLS, one line each: name, kind, container, where."
  (loop for symbol across symbols
        for location = (first (locations (nlk:json-value symbol :object "location")))
        collect (format nil "~a (~a)~@[ in ~a~]~@[  ~a~]"
                        (nlk:json-value symbol :string "name")
                        (kind-name (nlk:json-value symbol :integer "kind"))
                        (nlk:json-value symbol :text "containerName")
                        (and location
                             (destructuring-bind (file at character) location
                               (location-line server cache file at character :context nil))))))

(defun symbols (path &key query)
  "The symbols the file PATH defines, as an outline; with QUERY, the symbols
matching it anywhere in PATH's project."
  (stamped ("lsp:symbols" "read" path)
    (let* ((path (target-path path))
           (deadline (deadline-after +verb-seconds+))
           (server (primary path deadline))
           (cache (make-hash-table :test 'equal)))
      (if query
          (let ((found (ask server path "workspace/symbol"
                            (nlk:json-object "query" (princ-to-string query)) deadline)))
            (if (plusp (length found))
                (capped (symbol-information server cache found))
                (format nil "~a found no symbol matching ~a" (srv-name server) query)))
          (let ((found (ask server path "textDocument/documentSymbol"
                            (nlk:json-object "textDocument" (text-document path)) deadline)))
            (cond ((zerop (length found))
                   (format nil "~a found no symbols in ~a" (srv-name server) (display-path path)))
                  ((nlk:json-value (aref found 0) :object "location")
                   (capped (symbol-information server cache found)))
                  (t (capped (outline found 0)))))))))

(defun rename (path symbol new-name &key line (apply t))
  "Rename SYMBOL, as it appears in the file PATH, to NEW-NAME everywhere the
server knows it; :apply nil only says what would change."
  (stamped ("lsp:rename" "edit" path)
    (unless (and (stringp new-name) (plusp (length new-name)))
      (fail "new-name must be a non-empty string"))
    (let* ((path (target-path path))
           (deadline (deadline-after +verb-seconds+))
           (server (primary path deadline))
           (params (symbol-params server path symbol line)))
      (setf (gethash "newName" params) new-name)
      (let* ((edit (or (ask server path "textDocument/rename" params deadline)
                       (fail "~a cannot rename ~a there" (srv-name server) symbol)))
             (files (workspace-edit-files edit))
             (total (reduce #'+ files :key (lambda (file) (length (cdr file))))))
        (when (zerop total)
          (fail "~a found nothing to rename for ~a" (srv-name server) symbol))
        (if apply
            (let ((applied (apply-workspace-edit edit (srv-encoding server))))
              (dolist (file applied)
                (ignore-errors (sync-document server (car file))))
              (format nil "renamed ~a to ~a: ~d edit~:p in ~d file~:p (~{~a~^, ~})"
                      symbol new-name total (length applied)
                      (loop for (file . count) in applied
                            collect (format nil "~a ~d" (display-path file) count))))
            (format nil "renaming ~a to ~a would make ~d edit~:p in ~d file~:p:~%~{~a~^~%~}~%~
                         call again without :apply nil to make them"
                    symbol new-name total (length files)
                    (loop for (file . edits) in files
                          collect (format nil "  ~a: line~p ~{~d~^, ~}" (display-path file)
                                          (length edits)
                                          (sort (remove-duplicates
                                                 (map 'list (lambda (edit)
                                                              (1+ (or (nlk:json-value edit :integer "range" "start" "line") 0)))
                                                      edits))
                                                #'<)))))))))

(defun status ()
  "Every language server the cell holds: name, root, state, pid, open files."
  (stamped ("lsp:status" nil nil)
    (let ((servers (bt2:with-lock-held (*servers-lock*) (copy-list *servers*))))
      (if (null servers)
          "no language servers are running; one starts when a file it serves is written or asked about"
          (format nil "~{~a~^~%~}"
                  (loop for server in (reverse servers)
                        collect (format nil "~a in ~a: ~(~a~)~@[, pid ~d~]~@[, ~d open~]~@[: ~a~]~@[; log ~a~]"
                                        (srv-name server) (srv-root server) (srv-state server)
                                        (and (srv-conn server) (conn-pid (srv-conn server)))
                                        (let ((count (hash-table-count (srv-documents server))))
                                          (and (plusp count) count))
                                        (and (eq :failed (srv-state server)) (srv-failure server))
                                        (and (srv-log server) (native (srv-log server))))))))))

(defun restart (&optional name)
  "Stop every language server, or those named NAME; each starts again on its next use."
  (stamped ("lsp:restart" nil name)
    (let ((stopping (bt2:with-lock-held (*servers-lock*)
                      (let ((matching (remove-if-not (lambda (server)
                                                       (or (null name) (equal name (srv-name server))))
                                                     *servers*)))
                        (setf *servers* (set-difference *servers* matching))
                        matching))))
      (if stopping
          (format nil "stopped ~d server~:p (~{~a~^, ~}); each starts again on its next use"
                  (stop-servers stopping) (mapcar #'srv-name stopping))
          (format nil "no ~:[~;~:*~a ~]server is running" name)))))

(defun camel (keyword)
  "A :kebab-case KEYWORD as the camelCase key LSP spells it."
  (let ((parts (uiop:split-string (string-downcase (symbol-name keyword)) :separator "-")))
    (format nil "~a~{~:(~a~)~}" (first parts) (rest parts))))

(defun request-params (params)
  "PARAMS as JSON: a JSON string decoded, a table as it is, a keyword plist
with camelCase keys made from its kebab-case ones."
  (labels ((walk (value)
             (cond ((and (consp value) (keywordp (car value)))
                    (let ((object (nlk:make-json-object)))
                      (loop for (key inner) on value by #'cddr
                            do (setf (gethash (camel key) object) (walk inner)))
                      object))
                   ((consp value) (map 'vector #'walk value))
                   (t value))))
    (cond ((null params) nil)
          ((stringp params) (or (ignore-errors (nlk:decode-json params))
                                (fail "params is not JSON: ~a" (nlk:one-line params :cap 80))))
          (t (walk params)))))

(defun request (server method &key params path)
  "Send METHOD with PARAMS to the running SERVER, by name, and answer its
reply as JSON; with PATH, the server that serves that file, started if need be."
  (stamped ("lsp:request" nil (or path method))
    (let* ((deadline (deadline-after +verb-seconds+))
           (target (if path
                       (let ((path (target-path path)))
                         (or (find server (servers-for path) :key #'srv-name :test #'equal)
                             (fail "~a does not serve ~a" server (display-path path))))
                       (or (bt2:with-lock-held (*servers-lock*)
                             (find-if (lambda (s) (and (equal server (srv-name s)) (eq :ready (srv-state s))))
                                      *servers*))
                           (fail "no ~a is running; pass :path a file it serves to start it" server)))))
      (ready target deadline)
      (multiple-value-bind (result state detail)
          (call (srv-conn target) method (request-params params) :seconds (seconds-left deadline))
        (ecase state
          (:ok (nlk:pretty-json (if (eq result :null) nil result)))
          (:timeout (fail "~a did not answer ~a within ~d s" server method +verb-seconds+))
          (:error (fail "~a refused ~a: ~a" server method detail))
          (:closed (fail "~a stopped while answering" server)))))))

;;; --- the write path -------------------------------------------------------------------

(defvar *written* (make-hash-table :test 'equal)
  "Session id -> the files written in it that a server serves, oldest first,
not yet diagnosed.")

(defvar *written-lock* (bt2:make-lock :name "lsp written"))

(defun note-write (next path text)
  "WRITE-FILE-TEXT advice: once the write has landed, record PATH under the
session it was for when a server serves it. Never fails the write."
  (multiple-value-prog1 (funcall next path text)
    (ignore-errors
     (let ((session (getf (nle:turn) :session-id))
           (settings *lsp*))
       (when (and session settings (getf settings :diagnostics-on-write))
         (let ((file (native (probe-file path))))
           (when (candidate-specs file (getf settings :servers))
             (bt2:with-lock-held (*written-lock*)
               (let ((files (gethash session *written*)))
                 (unless (member file files :test #'string=)
                   (setf (gethash session *written*) (append files (list file)))))))))))))

(defun take-written (session)
  "SESSION's recorded files, the record emptied."
  (bt2:with-lock-held (*written-lock*)
    (prog1 (gethash session *written*)
      (remhash session *written*))))

(defun forget-written ()
  (bt2:with-lock-held (*written-lock*)
    (clrhash *written*)))

(defun backgrounded-p ()
  "Whether the eval call just made went to the background."
  (let ((metadata nle::*tool-result-metadata*))
    (and (hash-table-p metadata) (gethash "background_shell" metadata) t)))

(defun write-block (session)
  "What the servers say about SESSION's recorded files, within wait_ms, as
the block an eval's result gains: errors and warnings, capped; NIL when
there is nothing to say."
  (let* ((files (or (take-written session) (return-from write-block nil)))
         (checked (subseq files 0 (min +block-files+ (length files))))
         (base (session-directory))
         (pairs (loop for path in checked
                      nconc (loop for server in (ignore-errors (servers-for path))
                                  collect (cons server path))))
         (groups (job-groups (collect-diagnostics pairs (deadline-after (/ (setting :wait-ms) 1000)))))
         (shown '()) (notes '()) (total 0))
    (loop for (path items pending failed) in groups
          for file = (display-path path base)
          for serious = (remove-if (lambda (d) (> (severity d) 2)) (sorted-unique items))
          do (incf total (length serious))
             (dolist (d serious)
               (push (diagnostic-line file d :first-line t) shown))
             (dolist (server pending)
               (push (format nil "lsp: ~a still checking ~a; ~a for the result"
                             (srv-name server) file (diagnostics-call (list path) base))
                     notes))
             (dolist (server failed)
               (unless (srv-reported server)
                 (setf (srv-reported server) t)
                 (push (format nil "lsp: ~a is not running: ~a" (srv-name server)
                               (or (srv-failure server) "stopped"))
                       notes))))
    (when (> (length files) +block-files+)
      (push (format nil "lsp: ~d more written file~:p not checked; (lsp:diagnostics PATH) checks one"
                    (- (length files) +block-files+))
            notes))
    (setf shown (nreverse shown) notes (nreverse notes))
    (when (or shown notes)
      (let* ((serious (loop for (path items) in groups
                            nconc (remove-if (lambda (d) (> (severity d) 2)) (sorted-unique items))))
             (header (and shown (format nil "LSP diagnostics (~a):" (counts-text serious))))
             (lines '())
             (size (length (or header ""))))
        (loop for line in (subseq shown 0 (min +block-diagnostics+ (length shown)))
              while (< (+ size (length line) 1) +block-characters+)
              do (push line lines)
                 (incf size (1+ (length line))))
        (setf lines (nreverse lines))
        (format nil "~@[~a~]~{~%~a~}~:[~*~*~;~%... ~d more; ~a~]~:[~;~%~]~{~a~^~%~}"
                header lines
                (< (length lines) total) (- total (length lines)) (diagnostics-call checked base)
                (and header notes) notes)))))

(defun diagnose-writes (op next)
  "The :TOOL advice: after an eval, the diagnostics of the files it wrote,
appended to its result. Fails open: the eval's own text is never lost."
  (let ((text (funcall next op))
        (session (getf (nle:turn) :session-id)))
    (if (and (equal "eval" (getf op :name)) (stringp text) session *lsp*
             (not (backgrounded-p)))
        (handler-case
            (let ((block (write-block session)))
              (if block
                  (format nil "~a~&~%~a" text block)
                  text))
          (nlk:turn-cancelled-condition (condition) (error condition))
          (error () text))
        text)))
