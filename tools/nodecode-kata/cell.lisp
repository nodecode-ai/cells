;;;; cell.lisp --- kata: the routines a session repeats, compiled into one call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A routine is a run of tool calls that the same work produces every time:
;;;; read a file, then edit it, then run the tests. This cell watches every
;;;; dispatch at the :TOOL point, keeps the calls it saw with their arguments
;;;; masked, finds the windows of tool names that repeat in separate
;;;; sessions, and generalises each step's arguments into constants and named
;;;; parameters. A chosen window is a compiled routine -- kata/routines.json
;;;; under the home -- and running one dispatches its steps in order in one
;;;; call, with the parameters the caller fills in.
;;;;
;;;; Nothing here reaches the network, spawns a process or reads a file
;;;; outside the home; the record lives in this image and the routines in one
;;;; JSON file of the cell's own.

(defpackage #:nodecode-kata
  (:use #:cl)
  (:nicknames #:kata)
  (:export #:kata-error #:history #:mined #:show #:routines #:compile-routine
           #:forget-routine #:run-routine #:clear-history #:clear-routines
           #:decode-kata-call))

(in-package #:nodecode-kata)

(nlk:define-peripheral kata :not-running t)

;;; --- the words -------------------------------------------------------------

(defparameter +tool-name+ "kata"
  "The one tool this cell registers.")

(defparameter +actions+ '("list" "show" "compile" "run" "forget" "clear")
  "What an ACTION of the tool or a /kata verb names.")

(defparameter +summary+
  "kata: the tool routines a session repeats, mined and compiled into one call"
  "One line, on the manual and in every request.")

(defparameter +primer+
  "A routine is a run of tool calls the same work produces every time (read a
file, edit it, run the tests). The nodecode-kata cell watches every tool call
of every turn; nothing needs to be done to record one. What it has seen:
  (kata:history)      the recent calls, newest first, arguments masked
  (kata:mined)        the routines the record repeats: id, tools, support,
                      the mined steps with their constants and parameters
  (kata:routines)     the compiled ones
  (kata:compile-routine \"ID\")  keep the mined routine ID as kata_<tools>,
                      or NAME it with a second argument
  (kata:run-routine \"kata_x\" '((\"path\" . \"/a/b\")))  run its steps now
A step's arguments are the values seen first: one that never varied is a
constant, one that varied is a parameter, and a run refuses until every
parameter has a value. /kata lists, and /kata compile ID, /kata show NAME,
/kata run NAME path=/a/b, /kata forget NAME and /kata clear do the same from
the composer. A routine is a shortcut, not a guard: running one calls its
tools exactly as written, side effects included."
  "What (help :kata) answers while the cell runs.")

;;; --- the running section ---------------------------------------------------

(defun config (key &optional default)
  "One running setting, or DEFAULT whichever cell is not started."
  (if *kata* (getf *kata* key default) default))

;;; --- the record ------------------------------------------------------------

(defvar *call-lock* (bt2:make-lock :name "kata-calls")
  "The :TOOL point runs on the turn worker; the record is read from the shell's.")

(defvar *calls* '()
  "Newest first: (:session :tool :arguments ((KEY . TEXT) ...) :result TEXT :at SECONDS).")

(defun secret-key-p (key)
  "Whether an argument KEY names a credential, so its value is not kept."
  (let ((key (string-downcase (string key))))
    (some (lambda (needle) (search needle key))
          '("key" "token" "secret" "password" "passwd" "credential" "auth" "bearer"))))

(defun secret-looking-p (text)
  "Whether TEXT looks like a credential: long, unbroken, mixed case and digits."
  (and (stringp text)
       (>= (length text) 32)
       (not (find #\Space text))
       (some #'upper-case-p text)
       (some #'digit-p text)))

(defun value-text (value)
  "VALUE as the short text the record keeps."
  (cond ((stringp value) value)
        ((null value) "null")
        ((eq value t) "true")
        (t (nlk:encode-json-object value))))

(defun stored-arguments (arguments)
  "ARGUMENTS, the decoded call's table, as a sorted alist of clipped text,
a credential's value replaced by [set]."
  (when (hash-table-p arguments)
    (let (pairs)
      (maphash (lambda (key value)
                 (let ((text (value-text value)))
                   (push (cons key (if (or (secret-key-p key) (secret-looking-p text))
                                       "[set]"
                                       (nlk:clip text 200)))
                         pairs)))
               arguments)
      (sort pairs #'string< :key #'car))))

(defun remember-call (tool arguments result)
  "Keep one successful dispatch, if recording is on and the tool is not ours."
  (when (and (config :record t)
             (stringp tool)
             (not (string= tool +tool-name+)))
    (let ((call (list :session (or nlk:*scribe-session-id* "operator")
                       :tool tool
                       :arguments (stored-arguments arguments)
                       :result (nlk:clip (or result "") 300)
                       :at (get-universal-time)))
          (limit (config :history 200)))
      (bt2:with-lock-held (*call-lock*)
        (push call *calls*)
        (when (> (length *calls*) limit)
          (setf *calls* (subseq *calls* 0 limit))))))
  tool)

(defun record-call (op next)
  "The :TOOL point: run the call, then remember it. A call that signals is
not a step of any routine, and its own error is the call's."
  (let ((text (funcall next op)))
    (nlk:with-handlers ((error () nil))
      (remember-call (getf op :name) (getf op :arguments) text))
    text))

(defun history (&optional (limit 20))
  "The recorded calls, newest first, one line each: the tool, the argument
names and the first line of its answer."
  (with-output-to-string (out)
    (if (null *calls*)
        (format out "kata: no call recorded yet")
        (progn
          (format out "kata: ~d call~:p recorded, ~d kept~%" (length *calls*) (length *calls*))
          (dolist (call (subseq *calls* 0 (min limit (length *calls*))))
            (format out "  ~a(~{~a~^, ~}) -> ~a~%"
                    (getf call :tool)
                    (mapcar #'car (getf call :arguments))
                    (nlk:first-line (getf call :result))))))))

(defun clear-history ()
  "Empty the record; the compiled routines stay."
  (bt2:with-lock-held (*call-lock*) (setf *calls* '()))
  (format nil "kata: the record is empty"))

;;; --- mining ------------------------------------------------------------------

(defparameter *session-gap* 1800
  "Seconds between two calls of one session that end a run.")

(defun tools-of (calls)
  (mapcar (lambda (call) (getf call :tool)) calls))

(defun signature (calls)
  (format nil "~{~a~^|~}" (tools-of calls)))

(defun session-support (occurrences)
  "How many separate sessions OCCURRENCES, a list of windows, came from."
  (length (remove-duplicates (mapcar (lambda (window) (getf (first window) :session)) occurrences)
                              :test #'string=)))

(defun runs ()
  "The record as runs: per session in time order, split where two calls are
further apart than *SESSION-GAP*; single calls and one-call runs are dropped."
  (let ((by-session (make-hash-table :test #'equal))
        (out '()))
    (bt2:with-lock-held (*call-lock*)
      (dolist (call (reverse *calls*))
        (push call (gethash (getf call :session) by-session))))
    (maphash (lambda (session calls)
               (declare (ignore session))
               (let ((run '()))
                 (flet ((close-run ()
                          (when (>= (length run) 2) (push (nreverse run) out))
                          (setf run '())))
                   (dolist (call (nreverse calls))
                     (when (and run (> (- (getf call :at) (getf (first run) :at)) *session-gap*))
                       (close-run))
                     (push call run))
                   (close-run))))
             by-session)
    out))

(defun windows (runs max-steps)
  "SIGNATURE -> the windows of 2..MAX-STEPS calls in RUNS carrying it."
  (let ((found (make-hash-table :test #'equal)))
    (dolist (run runs)
      (loop for n from 2 to (min max-steps (length run))
            do (loop for start from 0 to (- (length run) n)
                     for window = (subseq run start (+ start n))
                     do (pushnew window (gethash (signature window) found) :test #'equal))))
    found))

(defun dominated-p (occurrences frequent)
  "Whether a window of OCCURRENCES is a prefix or a suffix of a window one
call longer that repeats at least as often: the miner proposes whole
routines, never their fragments."
  (let ((tools (tools-of (first occurrences)))
        (mine (session-support occurrences)))
    (and (> (length tools) 2)
         (some (lambda (sub)
                 (let ((other (gethash (format nil "~{~a~^|~}" sub) frequent)))
                   (and other (>= (session-support other) mine))))
               (list (butlast tools) (rest tools))))))

(defun signature-id (tools)
  "The stable id of the routine TOOLS names: FNV-1a of its signature, 8 hex."
  (let ((hash 2166136261))
    (loop for char across (format nil "~{~a~^|~}" tools)
          do (setf hash (logand (* (logxor hash (char-code char)) 16777619) #xFFFFFFFF)))
    (format nil "~(~8,'0x~)" hash)))

(defun param-name (key)
  "KEY as a parameter name: lowercase, anything but a letter or digit is _."
  (let ((name (string-downcase
               (substitute #\_ #\Space
                           (remove-if-not (lambda (char) (or (alphanumericp char) (char= char #\_)))
                                          (substitute #\_ #\Space key))))))
    (if (zerop (length name)) "arg" name)))

(defun routine-name (tools)
  "The name a routine of TOOLS is kept under: kata_ and the first word of
each tool name, runs of one word collapsed, at most 56 characters."
  (let ((parts '()))
    (dolist (tool tools)
      (let* ((flat (remove-if-not #'alphanumericp (string-downcase tool)))
             (short (or (first (uiop:split-string flat :separator '(#\_))) "step")))
        (unless (equal short (first parts)) (push short parts))))
    (nlk:clip (format nil "kata_~{~a~^_~}" (reverse parts)) 56)))

(defun common-keys (arg-lists)
  "The argument names every alist in ARG-LISTS carries, in the first one's order."
  (reduce (lambda (keys alist)
            (remove-if-not (lambda (key) (assoc key alist :test #'string=)) keys))
          (rest arg-lists)
          :initial-value (mapcar #'car (first arg-lists))))

(defun build-template (occurrences)
  "OCCURRENCES of one window, generalised: each step's argument is a
constant when it never varied and a parameter otherwise.
=> (VALUES STEPS PARAMS), STEPS a list of (TOOL ((KEY KIND VALUE) ...))."
  (let ((steps '())
        (params '()))
    (dotimes (index (length (first occurrences)))
      (let* ((calls (mapcar (lambda (occurrence) (nth index occurrence)) occurrences))
             (tool (getf (first calls) :tool))
             (arg-lists (mapcar (lambda (call) (copy-alist (getf call :arguments))) calls))
             (args '()))
        (dolist (key (common-keys arg-lists))
          (let ((values (mapcar (lambda (alist) (cdr (assoc key alist :test #'string=))) arg-lists)))
            (if (every (lambda (value) (string= value (first values))) values)
                (push (list key :const (first values)) args)
                (let ((name (param-name key)))
                  (unless (member name params :test #'string=) (setf params (append params (list name))))
                  (push (list key :param name) args)))))
        (push (list tool (nreverse args)) steps)))
    (values (nreverse steps) params)))

(defun fits-p (steps occurrence)
  "Whether a call of OCCURRENCE is what STEPS would render: every constant
matches, a parameter takes whatever it was."
  (every (lambda (step call)
           (and (string= (first step) (getf call :tool))
                (every (lambda (arg)
                         (destructuring-bind (key kind value) arg
                           (let ((seen (cdr (assoc key (getf call :arguments) :test #'string=))))
                             (or (eq kind :param) (and seen (string= seen value))))))
                       (second step))))
         steps
         occurrence))

(defun fit-rate (steps occurrences)
  "How many of OCCURRENCES the template built from all of them reproduces."
  (if occurrences
      (/ (count-if (lambda (occurrence) (fits-p steps occurrence)) occurrences)
         (coerce (length occurrences) 'double-float))
      0d0))

(defun mined (&aux (frequent (make-hash-table :test #'equal)))
  "The routines the record repeats, best first. Each is a plist of :ID,
:TOOLS, :NAME, :SUPPORT, :OCCURRENCES, :FIT, :STEPS and :PARAMS."
  (let ((found (windows (runs) (config :max-steps 6)))
        (candidates '()))
    (maphash (lambda (key occurrences)
               (when (>= (session-support occurrences) (config :min-support 2))
                 (setf (gethash key frequent) occurrences)))
             found)
    (maphash (lambda (key occurrences)
               (unless (dominated-p occurrences frequent)
                 (push occurrences candidates)))
             frequent)
    (setf candidates (sort candidates (lambda (a b)
                                        (let ((sa (session-support a)) (sb (session-support b)))
                                          (if (= sa sb)
                                              (> (length a) (length b))
                                              (> sa sb))))))
    (loop for occurrences in candidates
          for tools = (tools-of (first occurrences))
          collect (multiple-value-bind (steps params) (build-template occurrences)
                    (list :id (signature-id tools)
                          :tools tools
                          :name (routine-name tools)
                          :support (session-support occurrences)
                          :occurrences (length occurrences)
                          :fit (fit-rate steps occurrences)
                          :steps steps
                          :params params)))))

(defun candidate (id)
  "The mined routine ID names, or a refusal."
  (or (find id (mined) :key (lambda (routine) (getf routine :id)) :test #'string=)
      (fail "no mined routine ~a: /kata lists the ids the record repeats" id)))

;;; --- the routines ------------------------------------------------------------

(defvar *routines* '()
  "The compiled routines, in the order they were kept, each a plist of
:NAME :TOOLS :SUPPORT :FIT :PARAMS :STEPS.")

(defvar *routines-file* nil
  "Where the compiled routines are kept; NIL reads (nlk:home \"kata/routines.json\").")

(defun routines-file ()
  (or *routines-file* (namestring (nlk:home "kata/routines.json"))))

(defun routine-object (routine)
  (nlk:json-object
   "name" (getf routine :name)
   "tools" (coerce (getf routine :tools) 'vector)
   "support" (getf routine :support 0)
   "fit" (format nil "~,2f" (getf routine :fit 1d0))
   "params" (coerce (getf routine :params) 'vector)
   "steps" (coerce (loop for (tool args) in (getf routine :steps)
                          collect (nlk:json-object
                                   "tool" tool
                                   "args" (coerce (loop for (key kind value) in args
                                                         collect (vector key
                                                                         (string-downcase (symbol-name kind))
                                                                         value))
                                                  'vector)))
                    'vector)))

(defun json-strings (object key)
  "OBJECT's KEY as a list of strings, empty when absent."
  (coerce (or (nlk:json-array object key) #()) 'list))

(defun object-routine (object)
  "One routine read back from OBJECT."
  (list :name (nlk:json-value object :string "name")
        :tools (json-strings object "tools")
        :support (or (nlk:json-value object :integer "support") 0)
        :fit (or (ignore-errors (read-from-string (or (nlk:json-value object :string "fit") "1.00"))) 1d0)
        :params (json-strings object "params")
        :steps (loop for step across (or (nlk:json-array object "steps") #())
                     collect (list (nlk:json-value step :string "tool")
                                   (loop for arg across (or (nlk:json-array step "args") #())
                                         collect (list (string (aref arg 0))
                                                       (if (string= (aref arg 1) "param") :param :const)
                                                       (string (aref arg 2))))))))

(defun save-routines ()
  "The compiled routines to the cell's own file, atomically."
  (nlk:write-file-atomically
   (routines-file)
   (nlk:encode-json-object
    (nlk:json-object "routines" (coerce (mapcar #'routine-object *routines*) 'vector))))
  *routines*)

(defun load-routines ()
  "The routines kept in the cell's own file, in the order they stand there."
  (let ((text (nlk:read-text (routines-file))))
    (if (null text)
        '()
        (handler-case
            (loop for object across (nlk:json-array (shasht:read-json text) "routines")
                  collect (object-routine object))
          (error (condition)
            (fail "the routines file ~a does not read: ~a"
                  (routines-file) (nlk:one-line (princ-to-string condition))))))))

(defun routines ()
  "The compiled routines, in the order they were kept."
  *routines*)

(defun find-routine (name &aux (name (string-downcase (string name))))
  (or (find name *routines* :key (lambda (routine) (getf routine :name)) :test #'string=)
      (fail "no compiled routine ~a: /kata lists the ones there are" name)))

(defun compile-routine (id &optional name)
  "Keep the mined routine ID as a compiled one, under NAME or its own."
  (let* ((routine (candidate id))
         (final (or (and name (plusp (length name)) (string-downcase name))
                    (getf routine :name)))
         (kept (list :name final
                     :tools (getf routine :tools)
                     :support (getf routine :support)
                     :fit (format nil "~,2f" (getf routine :fit 1d0))
                     :params (getf routine :params)
                     :steps (getf routine :steps))))
    (setf *routines* (append (remove final *routines*
                                     :key (lambda (routine) (getf routine :name))
                                     :test #'string=)
                             (list kept)))
    (save-routines)
    (format nil "kata: kept ~a -- ~{~a~^, then ~}; to run it: (kata:run-routine ~s ~s)~
                 ~@[ -- parameters: ~{~a~^, ~}~]"
            final (getf kept :tools) final
            (loop for param in (getf kept :params) collect (cons param ""))
            (getf kept :params))))

(defun forget-routine (name)
  "Take the compiled routine NAME off the cell and its file."
  (let ((routine (find-routine name)))
    (setf *routines* (remove routine *routines*))
    (save-routines)
    (format nil "kata: forgot ~a" (getf routine :name))))

(defun clear-routines ()
  (setf *routines* '())
  (save-routines)
  (format nil "kata: no compiled routines are left"))

;;; --- running one ------------------------------------------------------------

(defun dispatch-to-registry (tool arguments)
  "Call the registered tool TOOL with ARGUMENTS, the table its body takes."
  (when (string= tool +tool-name+)
    (fail "a routine cannot call ~a: the tool is running it" +tool-name+))
  (let ((entry (nle::find-tool tool)))
    (unless entry
      (fail "the tool ~a a routine names is not registered here" tool))
    (funcall (third entry) (nle::tool-body-arguments entry arguments))))

(defvar *dispatch* 'dispatch-to-registry
  "What a routine's step is dispatched through: the registry, or a test's stub.")

(defun render-arguments (args values)
  "One step's ARGS, a ((KEY KIND VALUE) ...) template, into the table its
tool's body takes: constants as recorded, parameters from VALUES."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (arg args)
      (destructuring-bind (key kind value) arg
        (if (eq kind :const)
            (setf (gethash key table) value)
            (multiple-value-bind (bound found) (gethash value values)
              (unless found
                (fail "this routine needs ~a: give it as ~a=VALUE" value value))
              (setf (gethash key table) bound)))))
    table))

(defun param-table (params &optional (pairs '()))
  "PARAMS named, filled from PAIRS, one (NAME . VALUE) a pair."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair pairs table)
      (when (and (consp pair) (member (car pair) params :test #'string=))
        (setf (gethash (car pair) table) (cdr pair))))))

(defun missing-params (params values)
  (remove-if (lambda (param) (nth-value 1 (gethash param values))) params))

(defun run-routine (name &optional (values (make-hash-table :test #'equal)))
  "Run the compiled routine NAME with VALUES, a table of parameter values,
and answer what each step answered."
  (let* ((routine (find-routine name))
         (missing (missing-params (getf routine :params) values)))
    (when missing
      (fail "~a needs ~{~a~^, ~}: give each as name=value" (getf routine :name) missing))
    (with-output-to-string (out)
      (format out "kata: ~a, ~d step~:p~%" (getf routine :name) (length (getf routine :steps)))
      (loop for (tool args) in (getf routine :steps)
            for index from 1
            do (format out "~d. ~a(" index tool)
               (let ((arguments (render-arguments args values)))
                 (format out "~{~a~^, ~})" (sort (loop for key being the hash-keys of arguments
                                                       collect key)
                                                 #'string<))
                 (handler-case
                     (let ((answer (funcall *dispatch* tool arguments)))
                       (format out " -> ~a~%" (nlk:first-line (or answer ""))))
                   (error (condition)
                     (format out " -> ERROR: ~a~%" (nlk:one-line (princ-to-string condition)))
                     (return))))))))

;;; --- what the tool and /kata answer ------------------------------------------

(defun candidates-text ()
  "The mined routines as the one block /kata list and the tool answer."
  (let ((candidates (mined)))
    (with-output-to-string (out)
      (if (null candidates)
          (format out "kata: no routine repeats yet -- ~d call~:p recorded, ~
                       a routine needs the same window in ~d separate sessions"
                  (length *calls*) (config :min-support 2))
          (progn
            (format out "kata: ~d routine~:p the record repeats~%" (length candidates))
            (dolist (routine candidates)
              (format out "  ~a  ~a: ~{~a~^, then ~} -- ~d session~:p, ~d seen, fit ~,2f~%"
                      (getf routine :id) (getf routine :name) (getf routine :tools)
                      (getf routine :support) (getf routine :occurrences)
                      (getf routine :fit)))
            (format out "compile one with (kata:compile-routine \"ID\") or /kata compile ID"))))))

(defun routines-text ()
  (with-output-to-string (out)
    (if (null *routines*)
        (format out "kata: nothing compiled yet -- /kata lists the candidates")
        (progn
          (format out "kata: ~d compiled routine~:p~%" (length *routines*))
          (dolist (routine *routines*)
            (format out "  ~a: ~{~a~^, then ~}~@[ -- parameters ~{~a~^, ~}~]~%"
                    (getf routine :name) (getf routine :tools) (getf routine :params)))))))

(defun show (id-or-name)
  "One mined or compiled routine's steps, as (kata:show \"x\") answers."
  (let* ((routine (or (find id-or-name (mined) :key (lambda (item) (getf item :id))
                                 :test #'string=)
                      (find-routine id-or-name)))
         (steps (getf routine :steps)))
    (with-output-to-string (out)
      (format out "~a~@[ (~a)~]: ~{~a~^, then ~}~%"
              (getf routine :name) (getf routine :id) (getf routine :tools))
      (loop for (tool args) in steps
            for index from 1
            do (format out "~d. ~a~@[~{ ~a~}~]~%" index tool
                       (loop for (key kind value) in args
                             collect (if (eq kind :const)
                                         (format nil "~a=~s" key value)
                                         (format nil "~a={{~a}}" key value)))))
      (when (getf routine :params)
        (format out "parameters: ~{~a~^, ~}~%" (getf routine :params))))))

(defun execute (action &optional routine params)
  "The tool's and /kata's one verb table: ACTION, a ROUTINE and its PARAMS."
  (let ((action (string-downcase (or action "list"))))
    (cond ((member action '("list" "") :test #'string=)
           (concatenate 'string (candidates-text) (routines-text)))
          ((string= action "show")
           (if (plusp (length (or routine "")))
               (show routine)
               "kata: show needs a routine id or name"))
          ((string= action "compile")
           (if (plusp (length (or routine "")))
               (compile-routine routine)
               "kata: compile needs the id /kata list shows"))
          ((string= action "forget")
           (if (plusp (length (or routine "")))
               (forget-routine routine)
               "kata: forget needs a compiled routine's name"))
          ((string= action "run")
           (if (plusp (length (or routine "")))
               (handler-case (run-routine routine (or params (make-hash-table :test #'equal)))
                 (kata-error (condition) (format nil "kata: ~a" condition)))
               "kata: run needs a compiled routine's name"))
          ((string= action "clear")
           (format nil "~a~%~a" (clear-history) (clear-routines)))
          (t (format nil "kata: no action ~a; one of ~{~a~^, ~}" action +actions+)))))

;;; --- the boundary ------------------------------------------------------------

(nlk:define-contract kata-call
  (:doc "The tool routines this session repeats: list the candidates, show one's steps, compile one into a named routine, run a compiled one with its parameters, forget one. Use it when the same run of tool calls has to be made again and again.")
  ("action" :text :required t :options +actions+
   :doc "list the candidates and the compiled ones; show one's steps; compile a candidate id; run a compiled routine; forget a compiled routine; clear the record and the compiled ones")
  ("routine" :text :default ""
   :doc "a candidate id from list, or a compiled routine's name")
  ("params" :object
   :doc "a routine's parameters by name, for run: {\"path\": \"/a/b\"}"))

(defun kata-tool (call)
  "The kata tool: the one verb table over the record and the routines."
  (execute (kata-call-action call)
           (kata-call-routine call)
           (kata-call-params call)))

(defun slash-params (name words)
  "The NAME= VALUE pairs in WORDS, as the table the routine NAME takes."
  (let ((pairs (loop for word in words
                     for at = (position #\= word)
                     when at
                       collect (cons (subseq word 0 at) (subseq word (1+ at))))))
    (param-table (or (ignore-errors (getf (find-routine name) :params)) '()) pairs)))

(defun run-slash (args session-id)
  "One /kata invocation: the verb ARGS names, the rest its arguments."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (verb (first words))
         (routine (second words)))
    (execute (or verb "list")
             (or routine "")
             (and routine (slash-params routine (cddr words))))))

;;; --- the cell -----------------------------------------------------------------

(defun install ()
  "Load the routines kept in the cell's own file; the record starts empty."
  (setf *calls* '()
        *routines* (load-routines))
  (nle:on-stop (lambda () (setf *calls* '() *routines* '())))
  (when *routines*
    (nle:notice (format nil "kata: ~d compiled routine~:p ready -- /kata lists them"
                        (length *routines*))
                :key "nodecode-kata")
    (nle:on-stop (lambda () (nle:notice nil :key "nodecode-kata")))))

(nle:define-cell kata
  (:section ("kata")
    (:guide "nothing is required; record watches the calls, min_support and max_steps shape what counts as a routine")
    ("record" :boolean :default t
     :doc "remember the tool calls a turn runs, arguments masked")
    ("history" :integer :default 200 :min 10
     :doc "how many recent calls the record keeps")
    ("max_steps" :integer :default 6 :min 2
     :doc "the longest routine mined, in tool calls")
    ("min_support" :integer :default 2 :min 2
     :doc "separate sessions a routine must repeat in"))
  (:start #'install)
  (:hook :tool #'record-call)
  (:tool +tool-name+ (nlk:contract 'kata-call) 'kata-tool)
  (:help :kata +summary+ +primer+)
  (:command "kata" #'run-slash
            :description "The routines a session repeats: the candidates, compile, show, run, forget"
            :argument-hint "list | show ID | compile ID [NAME] | run NAME key=value | forget NAME | clear"))
