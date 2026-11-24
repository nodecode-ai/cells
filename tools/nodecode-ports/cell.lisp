;;;; cell.lisp --- who holds a port: one question, one answer, a table's worth of rows.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The question the original plugin answers is "who holds this port": a
;;;; bind failure names a port and nothing else, and the answer -- the PID,
;;;; the image, its path -- is one call here instead of two commands whose
;;;; output is localized and has to be joined by hand. The rows come from
;;;; TABLE.LISP; this file is the filter over them, the JSON a model reads,
;;;; the lines /ports prints, and the manual.
;;;;
;;;; Field for field the answer is the one hermes-windows-ports returned:
;;;; ok, matched, listeners, total_endpoints, entries (protocol, address,
;;;; port, state, pid, process, path, remote_address, remote_port),
;;;; by_process and notes -- so a caller written against the original reads
;;;; this one unchanged.

(in-package #:nodecode-ports)

(defparameter *path-chars* 220
  "How much of an image's path one entry carries; a longer one is clipped.")

(defparameter *summary-processes* 10
  "How many processes the answer's by_process holds: the four that hold the
most rows are the ones worth seeing.")

;;; --- the call a model makes --------------------------------------------------------

(nlk:define-contract port-lookup
  (:doc "Answer 'who holds this port': the local TCP and UDP endpoints, the
owning PID, its image name and path, and who else is on it. Read it for a
bind failure or an EADDRINUSE error, for 'is something already listening on
3000', to find a stale gateway or backend that survived a crash, or to see
which process owns an established connection. Read-only: nothing is opened,
bound or connected, and the machine's own tables are all that are read.")
  ("port"     :integer :doc "One local port to match, 0 to 65535.")
  ("ports"    :array :of :integer :doc "Several local ports to match, with port.")
  ("process"  :text :doc "Case-insensitive part of the image name or path,
e.g. 'node', 'gateway', 'python'.")
  ("protocol" :text :options '("tcp" "udp" "both") :doc "Which tables to read; both by default.")
  ("state"    :text :doc "'listen' for servers, 'established' for live
connections, any other TCP state by name, or 'all' (the default). A UDP row
reports 'bound'.")
  ("include_ipv6" :boolean :default t :doc "Read the IPv6 tables too; yes by default.")
  ("limit"    :integer :doc "The most endpoints to return; 100 by default, 500 at most."))

;; The declaration above interns its record and readers where it is read; this
;; keeps the rest of the file in this cell's package whatever it left current.
(in-package #:nodecode-ports)

(defun wanted-ports (call)
  "The local ports CALL names, from PORT and PORTS."
  (remove-if-not (lambda (port) (and (integerp port) (<= 0 port 65535)))
                 (append (let ((port (port-lookup-port call)))
                           (if port (list port) '()))
                         (coerce (or (port-lookup-ports call) '()) 'list))))

(defun wanted-limit (call)
  "The most endpoints CALL asks for, the cell's own default, within 1 and 500."
  (let ((asked (or (port-lookup-limit call) (setting :limit))))
    (max 1 (min 500 (if (integerp asked) asked (setting :limit))))))

(defun state-matches-p (row wanted)
  "Whether ROW's state is the one WANTED names; 'all' names every one."
  (let ((state (or (getf row :state) "")))
    (cond ((or (null wanted) (string= wanted "") (string= wanted "all")) t)
          ((member wanted '("listen" "listening") :test #'string=)
           (string= state "listen"))
          (t (string= state wanted)))))

(defun process-matches-p (row wanted)
  "Whether ROW's image or path holds WANTED, case-insensitively."
  (or (null wanted) (string= wanted "")
      (not (null (search wanted (string-downcase
                                  (format nil "~a ~a" (or (getf row :process) "")
                                          (or (getf row :path) ""))))))))

(defun entry-of (row show-paths)
  "ROW as the entry the answer carries."
  (let ((path (and show-paths (getf row :path))))
    (nlk:make-json-object
     "protocol" (getf row :protocol)
     "address" (getf row :address)
     "port" (getf row :port)
     "state" (getf row :state)
     "pid" (getf row :pid)
     "process" (getf row :process)
     "path" (and path (if (> (length path) *path-chars*)
                          (concatenate 'string (subseq path 0 *path-chars*) "...")
                          path))
     "remote_address" (getf row :remote-address)
     "remote_port" (getf row :remote-port))))

(defun by-process-object (taken)
  "TAKEN's rows counted per image, the busiest first."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (row taken)
      (let ((key (or (getf row :process) (format nil "pid ~a" (getf row :pid)))))
        (setf (gethash key counts) (1+ (or (gethash key counts) 0)))))
    (let ((pairs '()))
      (maphash (lambda (key count) (push (cons key count) pairs)) counts)
      (setf pairs (sort pairs #'> :key #'cdr))
      (apply #'nlk:make-json-object
             (loop for (key . count) in (subseq pairs 0 (min *summary-processes* (length pairs)))
                   append (list key count))))))

(defun lookup (call)
  "CALL's rows, the entries it returns, the rows they came from, the limit
and whether the limit cut the table. => 5 values."
  (let* ((asked (string-downcase (or (port-lookup-protocol call) "both")))
         (protocol (cond ((string= asked "tcp") :tcp)
                         ((string= asked "udp") :udp)
                         ((string= asked "both") :both)
                         (t (fail "protocol is tcp, udp or both, not ~s" asked))))
         (ports (wanted-ports call))
         (wanted (string-downcase (nlk:trimmed (or (port-lookup-state call) "all"))))
         (wanted-process (string-downcase (nlk:trimmed (or (port-lookup-process call) ""))))
         (limit (wanted-limit call))
         (show-paths (setting :show-paths))
         (table (rows :protocol protocol :include-ipv6 (port-lookup-include-ipv6 call)))
         (taken '())
         (entries '())
         (full nil))
    (dolist (row table)
      (when (and (or (null ports) (member (getf row :port) ports))
                 (state-matches-p row wanted)
                 (process-matches-p row wanted-process))
        (push row taken)
        (push (entry-of row show-paths) entries)
        (when (>= (length entries) limit)
          (setf full t)
          (return))))
    (values table (nreverse entries) (nreverse taken) limit full)))

(defun note-list (table entries limit full)
  "The answer's notes: what matched, what the limit cut, what to try next."
  (let ((notes (list (format nil "~d endpoint~:p matched out of ~d in the table"
                              (length entries) (length table)))))
    (when full
      (setf notes (append notes (list (format nil "stopped at limit=~d; raise it or filter by port or process" limit)))))
    (when (null entries)
      (setf notes (append notes (list "no endpoint matched; drop the state filter to see established connections, or check the port list"))))
    notes))

(defun answer (call)
  "CALL's answer as the JSON string a model reads, its refusal included."
  (handler-case
      (multiple-value-bind (table entries taken limit full) (lookup call)
        (nlk:encode-json-object
         (nlk:make-json-object
          "ok" t
          "matched" (length entries)
          "listeners" (count-if (lambda (row) (string= (or (getf row :state) "") "listen"))
                                 taken)
          "total_endpoints" (length table)
          "entries" (coerce entries 'vector)
          "by_process" (by-process-object taken)
          "notes" (coerce (note-list table entries limit full) 'vector))))
    (ports-error (condition)
      (nlk:encode-json-object
       (nlk:make-json-object "ok" nil "error" (ports-error-detail condition))))))

;;; --- the same question from the operator ------------------------------------------

(defun call-for (argument)
  "The call /ports' ARGUMENT asks for: a number is a port, a word like
'listen' or 'established' is a state, and any other word is a process."
  (let ((port nil) (state nil) (words '()))
    (dolist (word (remove "" (uiop:split-string argument :separator '(#\Space #\Tab))
                          :test #'string=))
      (cond ((every #'digit-char-p word)
             (let ((value (ignore-errors (parse-integer word))))
               (when (and value (<= 0 value 65535)) (setf port value))))
            ((member word '("listen" "listening" "established" "bound" "all"
                            "time_wait" "close_wait") :test #'string=)
             (setf state word))
            (t (push word words))))
    (nlk:contract-decode
     (nlk:contract 'port-lookup)
     (apply #'nlk:make-json-object
            (append (when port (list "port" port))
                    (when words (list "process" (format nil "~{~a~^ ~}" (nreverse words))))
                    (when state (list "state" state))
                    (list "include_ipv6" t))))))

(defun line-of (row paths)
  "One row as a line /ports prints."
  (format nil "~5a ~39a ~12a  pid ~6@a  ~a~@[  ~a~]"
          (or (getf row :protocol) "")
          (format nil "~a:~a" (getf row :address) (getf row :port))
          (or (getf row :state) "")
          (getf row :pid)
          (or (getf row :process) "?")
          (and paths (getf row :path))))

(defun slash-ports (args session-id)
  "One /ports invocation: the endpoints the machine holds, ARGS filtered."
  (declare (ignore session-id))
  (let ((argument (string-downcase (nlk:trimmed (or args "")))))
    (multiple-value-bind (table entries taken limit full)
        (lookup (call-for argument))
      (declare (ignore table limit))
      (cond ((null taken)
             (format nil "ports~@[: ~a~]: nothing matched.~%~a~%try /ports (every endpoint), ~
/ports listen, /ports 3000, or /ports node"
                     (and (plusp (length argument)) argument)
                     (car (note-list '() entries (setting :limit) full))))
            (t (format nil "~d endpoint~:p~@[ with more behind them than one page; raise the ~
limit in /settings~]~@[ for ~a~]:~%~{~a~%~}"
                       (length taken) full (and (plusp (length argument)) argument)
                       (mapcar (lambda (row) (line-of row (setting :show-paths))) taken)))))))

;;; --- the manual -------------------------------------------------------------------

(defparameter +primer+
  "Who holds a local port, read-only: the win_port_owners tool, or
(ports:owners ...) through eval, answers this machine's local TCP and UDP
endpoints with the owning PID, its image name and its path, so a bind
failure or an EADDRINUSE error reads as one process instead of two commands.
Windows reads iphlpapi, Linux reads /proc and macOS reads lsof; nothing is
opened, bound or connected, and no child process runs on Windows or Linux.
  (ports:owners 3000)                     the endpoint holding port 3000
  (ports:owners :process \"node\")           whatever node holds
  (ports:owners :state \"listen\")           every listener
  (ports:owners :ports '(3000 8080))       two ports at once
/ports 3000, /ports node and /ports listen print the same rows as lines.
The answer's fields are ok, matched, listeners, total_endpoints, entries
(protocol, address, port, state, pid, process, path, remote_address,
remote_port), by_process and notes. A process whose image cannot be read
keeps its PID and reports a null process rather than failing the call."
  "What (help :ports) answers while the cell runs.")

(defun owners (&rest arguments)
  "The endpoints holding ARGUMENTS -- a port, or the pairs (ports:owners
:port 3000 :state \"listen\" :process \"node\" :protocol \"tcp\"
:include_ipv6 nil :limit 20) -- as the JSON string the tool answers."
  (running-settings)
  (flet ((key-name (key) (string-downcase (string key))))
    (answer (nlk:contract-decode
             (nlk:contract 'port-lookup)
             (apply #'nlk:make-json-object
                    (if (and arguments (not (keywordp (first arguments))))
                        (list "port" (first arguments))
                        (loop for (key value) on arguments by #'cddr
                              append (list (key-name key) value))))))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell ports
  (:section ("ports")
    (:guide "nothing here is required; limit caps one answer and show_paths keeps image paths out of it")
    ("limit" :integer :default 100 :min 1
             :doc "the most endpoints one answer carries; a call may lower or raise it to 500")
    ("show_paths" :boolean :default t
                  :doc "whether an answer carries each process's image path"))
  (:tool "win_port_owners" (nlk:contract 'port-lookup) #'answer)
  (:help :ports "ports: who holds a local port - (ports:owners 3000); /ports PORT|PROCESS" +primer+)
  (:command "ports" 'slash-ports
            :catches 'ports-error
            :description "Who holds a local port: the endpoints, PIDs and processes"
            :argument-hint "PORT|PROCESS|listen"))
