;;;; cell.lisp --- a turn forks itself: its independent tasks run at once, each
;;;; in a fork of the session that holds its whole context.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (fork:run "task" ... &key seconds from check keep) forks the calling
;;;; session at the turn running it, once per task, puts each task to its fork
;;;; and waits for every fork's turn to end. A fork is an anchored child
;;;; (NLK:CREATE-SESSION :PARENT :ANCHOR-TURN-ID): its history composes the
;;;; parent's by reference through the running turn, and the create carries the
;;;; parent's model, so its first request extends the parent's cached prefix and
;;;; it pays for its own work alone. That history ends in the eval call waiting
;;;; on it, which the fold answers `no result recorded (interrupted)': the
;;;; task's words say why, and that a fork does not fork.
;;;;
;;;; Going back is the same verb, never a second one:
;;;;
;;;; :FROM an earlier turn -- -N for N turns back, or its id -- anchors the
;;;; forks there: they hold what the session knew when that turn ended, and
;;;; nothing after it.
;;;;
;;;; :FROM a generation G, or an (OLD NEW) pair, with :CHECK is the way back
;;;; through the generations this home sealed. The check -- a form answering
;;;; true, or a shell command exiting 0 -- is put to a few generations at once,
;;;; each laid into a throwaway home of its own (NLK:BUILD-GENERATION) and
;;;; asked in a fresh process of today's binary (NLK:TRIAL), so neither the
;;;; live image nor the operator's home is touched; the search halts at the
;;;; step where the check turned false, and says what moved in it.
;;;;
;;;; :KEEP NAME with :CHECK races versions of NAME. Each fork writes its own as
;;;; FORK-<n>/NAME; each version and what stands are put to the check in a
;;;; fresh process laid at the newest generation; those that pass are timed on
;;;; the image's own calls of NAME -- each live call served by one of them, the
;;;; call itself the clock -- and the winner goes in NAME's place HELD
;;;; (NLK:HOLD-DEFINITIONS): written to the layer after one clean call made
;;;; outside an eval, put back if that call signals, and (revert 'NAME) puts
;;;; the old one back by hand until then. Beside a way back that found its
;;;; step, NAME as the last good generation's layer had it races too.
;;;;
;;;; A fork runs in the operator's own image, so it defines only names of its
;;;; own: a form defining or forgetting a name that already answers, or one a
;;;; sibling claimed, is refused before it runs (NLK:DEFINITION-ESCROW), and
;;;; nothing a fork defines is written to the layer (NLK:RECORD-DEFINITIONS).
;;;; What a fork defined stays live; the layer gets what the parent keeps.
;;;;
;;;; The wait rides the :FRAME point: a fork's turn ending pokes its waiter,
;;;; and the log (NLK:TURN-TERMINAL-KIND) says how it ended. A fork's turn
;;;; runs under the call's seconds (NLE:TURN-BUDGET): past them its tools are
;;;; refused and it answers where it stands, and one still running
;;;; *GRACE-SECONDS* later is cancelled. The caller unwinding -- an eval
;;;; interrupt, its own turn stopped -- cancels every fork still running.
;;;;
;;;; Forks are sessions like any other: /sessions draws them under the session
;;;; they came off, and their turns are in the log. They are never named (a
;;;; model call each) and never reflected on (their source is `fork').

(in-package #:nodecode-fork)

;;; --- the manual ---------------------------------------------------------------
;;; (help :fork) answers it while the cell runs; every request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "Forking this turn, through the nodecode-fork cell: one verb, called through
eval. A refusal is ERROR: FORK-ERROR with the reason.
  (fork:run \"task\" ... &key seconds from check keep)
 fork this session at the turn running it, once per task (at most 4), and
 wait for them all. Each fork holds this whole context, works its one task on
 this session's model, and answers. => one plist a task, in order: (:task
 :session :status :answer :ran) -- STATUS :completed, :failed, :cancelled or
 :timeout, ANSWER the fork's last words, RAN the first line of each eval it
 ran. SECONDS bounds each fork's turn (fork.seconds, 900, unless given): past
 it the fork's tools are refused and it answers where it stands.
  :from -N (or a turn id)
 fork N turns back instead of here: the forks hold what this session knew
 when that turn ended, and nothing after it.
  :from G (or (OLD NEW)) :check CHECK, tasks optional
 go back through the generations this home sealed, G to the newest, to the
 step where CHECK turned false. CHECK is a form answering true, e.g.
 (fboundp 'nle::help), or a shell command exiting 0 in this session's
 folder, e.g. \"test -f README.md\". Each generation is laid in a throwaway
 home and asked in a fresh process of today's binary, 4 at once (about a
 minute a round, 6 rounds over a thousand generations); nothing live is
 touched. => (:bisect (:verdict V :range (OLD NEW) :good G :bad G :changes
 LINES :rounds N :tested ROWS :says WORDS) :forks RESULTS): V :found;
 :clean when the newest passes, so the break is outside what a generation
 records; :program when the oldest fails too, so no generation explains it;
 :unanswered when an end could not be asked; :unsteady when the step's two
 generations, asked again, answer otherwise (a check reading what a boot
 varies). Tasks are forked after the search, told what it found.
  :keep NAME :check CHECK
 race versions of NAME, a plain function: fork n writes its version as
 fork-n/NAME, one defun taking every call NAME takes. Each version and what
 stands are put to CHECK in a fresh process; the ones that pass are timed on
 this image's own calls of NAME, one version serving each call; the fastest,
 if 5% faster than what stands (or passing where what stands fails), goes
 live HELD -- written to the layer after one clean call outside an eval,
 (revert 'NAME) putting the old one back until then. Beside :from G, NAME as
 the last good generation had it races too. => (:forks RESULTS :race (:keep
 NAME :standing ROW :versions ROWS :winner LABEL :says WORDS)).
A fork defines only names of its own -- one that already answers is refused --
and nothing it defines is written to the layer: the parent keeps what it chose.
When to fork: work that splits into branches that do not need each other --
a probe beside a measurement, one check over three folders -- or one probe
whose output would bury this context. When not to: a plain linear task, one
read, steps that each need the step before; do those here. Forks run at once
in the same folder, so give each a task that touches nothing another one does.
A fork cannot fork, and its answer comes back here, never to a person. A fork
pays for its own work, not this context again: it starts from this session's
cached prefix. A wait past the eval's yield goes on in the background, and its
value arrives when the last fork is done."
  "What (help :fork) answers while the cell runs.")

;;; --- the words -------------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *task-input*
  "[fork ~d of ~d] You are a fork of this session, made at the eval call above, which is your parent's and is still waiting on you -- that is why its result reads interrupted. Do not run it again: a fork cannot fork. Your one task:~%~a~%~:[~;Other forks are working on the other tasks at this moment, in the same folder: change nothing another task could touch. ~]Make no posts, messages or commits unless the task says to. Define only names of your own: you run in the operator's own image, so a form that defines or forgets a name that already answers is refused, and nothing you define is written to the layer -- the parent keeps what it chooses. Your answer goes back to that eval call, not to a person: lead with the result, then only the facts that back it."
  "A fork's input, over its number, how many forks the call made, its task,
and whether it has siblings.")

(defparameter *earlier-input*
  "[fork ~d of ~d] You are a fork of this session, taken back at an earlier turn of it: you hold what this session knew when that turn ended, and nothing after it happened in your record. A fork cannot fork. Your one task:~%~a~%~:[~;Other forks are working on the other tasks at this moment, in the same folder: change nothing another task could touch. ~]Make no posts, messages or commits unless the task says to. Define only names of your own: you run in the operator's own image, so a form that defines or forgets a name that already answers is refused, and nothing you define is written to the layer -- the parent keeps what it chooses. Your answer goes back to the call that made you, not to a person: lead with the result, then only the facts that back it."
  "A fork's input when its call took it back to an earlier turn, over the same
four.")

(defparameter *keep-input*
  "~%~%This call races versions of ~a. Write yours as ~a: one defun taking every call ~a takes today~@[ -- its lambda list is ~a~] -- with any helper inside it in flet or labels, and leave ~a itself alone. The parent puts your version to this check in a fresh process: ~a. The versions that pass are timed on this image's own calls of ~a, and the fastest goes live if it is 5% faster than what stands. Answer with what your version does differently and why it is faster, and nothing else."
  "What a :KEEP call adds to each task, over the name raced, the fork's own
name for its version, the name, its lambda list, the name, the check and the
name.")

(defparameter *bisect-input*
  "~%~%Before you were made, this call went back through the generations this home sealed: ~a"
  "What a way back adds to each task: what it found, in words.")

;;; --- the forks -------------------------------------------------------------------

(defparameter +most+ 4 "The forks one call may make.")

(defparameter +width+ 4
  "How many generations a way back asks at once, each a fresh process booting.")

(defparameter *grace-seconds* 60
  "How long past its budget a fork's turn may still run before it is cancelled.")

(defparameter *probe-seconds* 300
  "How long one fresh process may take to answer a check.")

(defparameter +turn-ends+ '("turn.completed" "turn.failed" "turn.cancelled"))

(defvar *waiting* (make-hash-table :test #'equal :synchronized t)
  "The forks a call is waiting on, by session id: (:SEMAPHORE S :SECONDS N
:ACTS FORMS), ACTS the definitions the fork evaluated, newest first.")

(defvar *owners* (make-hash-table :test #'equal :synchronized t)
  "While a call runs, each name its forks define: the name -> the fork's session.")

(defvar *made* (make-hash-table :test #'equal :synchronized t)
  "Each name a fork has defined in this image -> T: live and never kept, so no
eval's window counts it as that eval's own unkept change (UNKEPT-HOOK).")

(defun fork-name (turn n)
  "The session id of TURN's Nth fork: fork-<turn>-<n>, numbered further when
taken."
  (nlk:unused-name (format nil "fork-~a-~d" turn n) #'nlk:session-exists-p))

(defun fork-p (session)
  "Whether SESSION is a fork this cell made."
  (and (stringp session) (uiop:string-prefix-p "fork-turn-" session) t))

(defun caller ()
  "(values SESSION TURN): the session whose eval is calling and the id of the
turn running it, or a refusal."
  (let ((session nlk:*scribe-session-id*)
        (turn nle::*current-durable-turn*))
    (cond ((fork-p session)
           (fail "a fork cannot fork: work your task here, and say in your answer what you would have forked"))
          ((not (and session turn (equal session (nlk:durable-turn-session-id turn))))
           (fail "fork:run forks the turn running it: call it from a session's eval, inside a turn"))
          (t (values session (nlk:durable-turn-turn-id turn))))))

(defun turn-from-p (from)
  "Whether FROM names an earlier turn: -N, or a turn's id."
  (or (stringp from) (and (integerp from) (minusp from))))

(defun keepable (name)
  "NAME as the symbol a race keeps, or a refusal: a plain function of this
image, outside a locked package, with no advice on it and no hold."
  (let* ((symbol (if (stringp name) (find-symbol (string-upcase name) '#:nodecode.evolved) name))
         (package (and (symbolp symbol) (symbol-package symbol)))
         (advice (and (symbolp symbol) (cdr (assoc symbol nlk::*hooks* :test #'equal)))))
    (cond ((not (and symbol (symbolp symbol) (fboundp symbol)))
           (fail ":keep names a function of this image, and ~s is none" name))
          ((or (macro-function symbol) (special-operator-p symbol))
           (fail ":keep races calls, and ~s is a macro" symbol))
          ((typep (fdefinition symbol) 'generic-function)
           (fail ":keep races one definition, and ~s is a generic function with its methods: race a plain defun" symbol))
          ((and package (sb-ext:package-locked-p package))
           (fail ":keep races what this image may change, and ~s is in the locked package ~a" symbol (package-name package)))
          (advice
           (fail ":keep serves the calls of a name past any advice on it, and ~s carries ~{~a~^, ~}: race a name with none"
                 symbol (mapcar #'car advice)))
          ((find (symbol-name symbol) (nlk:held-definitions) :key (lambda (entry) (getf entry :name)) :test #'string=)
           (fail "~s is HELD, waiting on its first clean call: let one pass, or (revert '~(~s~)), first" symbol symbol))
          (t symbol))))

(defun parse (arguments)
  "(values TASKS SECONDS FROM CHECK KEEP) of fork:run's ARGUMENTS: the tasks,
strings, then :SECONDS N, :FROM X, :CHECK C and :KEEP NAME; or a refusal."
  (let* ((at (or (position-if-not #'stringp arguments) (length arguments)))
         (tasks (subseq arguments 0 at))
         (options (nthcdr at arguments)))
    (unless (and (evenp (length options))
                 (loop for (key) on options by #'cddr
                       always (member key '(:seconds :from :check :keep))))
      (fail "fork:run takes the tasks, strings, then :seconds N, :from X, :check C and :keep NAME; it was given ~s" options))
    (let* ((seconds (getf options :seconds (setting :seconds)))
           (from (getf options :from))
           (check (getf options :check))
           (keep (getf options :keep))
           (back (and from (not (turn-from-p from)))))
      (cond ((> (length tasks) +most+)
             (fail "fork:run makes at most ~d forks a call; it was given ~d tasks" +most+ (length tasks)))
            ((some (lambda (task) (zerop (length (nlk:trimmed task)))) tasks)
             (fail "a task is words, and one was blank"))
            ((not (and (integerp seconds) (plusp seconds)))
             (fail ":seconds is a whole number of seconds above zero, not ~s" seconds))
            ((and from (not (or (turn-from-p from)
                                (and (integerp from) (plusp from))
                                (and (consp from) (= 2 (length from))
                                     (every (lambda (n) (and (integerp n) (plusp n))) from)))))
             (fail ":from is an earlier turn -- -N for N turns back, or its id -- or a generation G, or an (old new) pair of them; not ~s" from))
            ((and (or back keep) (null check))
             (fail "going back through generations and racing a name are judged by a check: give :check, a form answering true or a shell command exiting 0"))
            ((and check (not (or back keep)))
             (fail ":check judges a way back or a race: give :from G (a generation, or an (old new) pair) or :keep NAME beside it"))
            ((and (null tasks) (not back))
             (fail "fork:run needs a task: (fork:run \"task\" ...)~:[~; -- a race needs versions, and each is a fork's~]" keep))
            (t (values tasks seconds from check (and keep (keepable keep))))))))

(defun earlier-turn (session turn from)
  "FROM -- -N, or a turn's id -- as a turn of SESSION before TURN, or a refusal."
  (let ((parents (nlk::session-composed-parents session)))
    (if (integerp from)
        (let ((at turn))
          (dotimes (back (- from) at)
            (setf at (gethash at parents))
            (unless at
              (fail ":from ~d goes back past this session's first turn: ~d turn~:p came before this one" from back))))
        (cond ((equal from turn)
               (fail ":from ~a is the turn running this call; leave :from out to fork here" from))
              ((not (nth-value 1 (gethash from parents)))
               (fail ":from ~s is not a turn of this session" from))
              (t from)))))

(defun start (session anchor n words task seconds rival)
  "Fork SESSION at ANCHOR as its Nth fork and put WORDS to it; RIVAL, when the
call races a name, is the name this fork writes its version under, claimed
for it before it runs. => the fork: (:TASK :N :SESSION :RIVAL :TURN), or,
when its turn could not start, :STATUS :FAILED and why."
  (let ((child (fork-name anchor n)))
    (nlk:create-session :id child :cwd (nlk:find-session-cwd session)
                        :parent session :anchor-turn-id anchor)
    ;; Waited on before the submit: its turn's budget is read as it opens.
    (setf (gethash child *waiting*)
          (list :semaphore (bt2:make-semaphore :name child) :seconds seconds :acts '()))
    (when rival (setf (gethash rival *owners*) child))
    (handler-case
        (list :task task :n n :session child :rival rival
              :turn (nlk:active-input-admission-turn-id
                     (nle:submit child words :command-id (format nil "fork:~a" child)
                                 :source "fork" :source-id session)))
      (error (condition)
        (list :task task :n n :session child :rival rival :status :failed
              :answer (format nil "its turn could not start: ~a" condition))))))

(defun release (session)
  "Every name SESSION claimed, unclaimed."
  (sb-ext:with-locked-hash-table (*owners*)
    (let ((names (loop for name being the hash-keys of *owners* using (hash-value owner)
                       when (equal owner session) collect name)))
      (dolist (name names) (remhash name *owners*)))))

(defun fork-tasks (session anchor tasks words seconds rivals)
  "Fork SESSION at ANCHOR once per TASK, put each its WORDS, and wait for
every fork's turn to end. => (values RESULTS FORKS): RESULTS as fork:run
answers them, FORKS each (:N :SESSION :RIVAL :ACTS), what it defined."
  (let ((forks '()))
    ;; However this call ends, no fork outlives the wait for it, and no name
    ;; stays claimed by one.
    (nlk:with-cleanup ((dolist (fork forks)
                         (let ((child (getf fork :session))
                               (started (getf fork :turn)))
                           (remhash child *waiting*)
                           (release child)
                           (when (and started (not (nlk:turn-terminal-kind started)))
                             (ignore-errors
                              (nlk:request-cancel-turn child started "the call waiting on it stopped"))))))
      (loop for task in tasks
            for text in words
            for n from 1
            do (push (start session anchor n text task seconds (nth (1- n) rivals)) forks))
      (setf forks (reverse forks))
      (let ((deadline (+ (get-internal-real-time)
                         (* (+ seconds *grace-seconds*) internal-time-units-per-second))))
        (values (loop for fork in forks
                      ;; one that never started is already as answered
                      collect (result fork (and (not (getf fork :status))
                                                (await-fork fork deadline seconds))))
                (loop for fork in forks
                      collect (list :n (getf fork :n) :session (getf fork :session)
                                    :rival (getf fork :rival)
                                    :acts (getf (gethash (getf fork :session) *waiting*) :acts))))))))

(defun await-fork (fork deadline seconds)
  "Wait for FORK's turn to end, or for DEADLINE (internal real time) to pass.
=> the turn's terminal kind, or :TIMEOUT once it has been asked to stop."
  (let ((child (getf fork :session))
        (turn (getf fork :turn)))
    (loop
      (let ((kind (nlk:turn-terminal-kind turn)))
        (when kind (return kind)))
      (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
        (when (<= left 0)
          (nlk:request-cancel-turn child turn (format nil "the fork ran past its ~d seconds" seconds))
          ;; its last words land with its ending
          (nlk:wait-until (lambda () (nlk:turn-terminal-kind turn)) (nlk:deadline-after 10) 0.1)
          (return :timeout))
        ;; A frame pokes at the ending; the bound stands under a frame not seen.
        (bt2:wait-on-semaphore (getf (gethash child *waiting*) :semaphore)
                               :timeout (float (min left 30)))))))

(defun ran (child turn)
  "The first line of each eval CHILD's TURN ran -- its note -- in order, or the
name of any other tool it called."
  (loop for payload in (nlk:events :session-id child :turn-id turn
                                   :kind nlk::+kind-tool-call-started+ :as :payloads)
        for form = (nlk:json-value (ignore-errors
                                    (nlk:decode-json (nlk:json-value payload :string "arguments")))
                                   :string "form")
        collect (if (and form (plusp (length (nlk:trimmed form))))
                    (nlk:clip (nlk:trimmed (first (nlk:lines (nlk:trimmed form)))) 120)
                    (or (nlk:json-value payload :string "tool-name") "a tool"))))

(defun ending (child turn)
  "What CHILD's TURN said as it failed or was cancelled, or NIL."
  (nlk:json-value (nlk:events :session-id child :turn-id turn
                              :kind '("turn.failed" "turn.cancelled") :as :payload)
                  :string "detail"))

(defun result (fork kind)
  "FORK as fork:run answers it, its turn having ended as KIND; one whose turn
never started, as it stands."
  (let ((child (getf fork :session))
        (turn (getf fork :turn)))
    (if (getf fork :status)
        (list :task (getf fork :task) :session child :status (getf fork :status)
              :answer (getf fork :answer) :ran '())
        (list :task (getf fork :task) :session child
              :status (cond ((eq kind :timeout) :timeout)
                            ((equal kind "turn.completed") :completed)
                            ((equal kind "turn.failed") :failed)
                            (t :cancelled))
              :answer (or (nlk:turn-answer child turn) (ending child turn))
              :ran (ran child turn)))))

;;; --- what a fork may define ------------------------------------------------------
;;; A fork runs in the operator's own image: what it evaluates is live there at
;;; once. So it writes only names of its own -- displacing one the image or a
;;; sibling stands on would break both -- and the layer is the parent's.

(defun act-name (act)
  "The name ACT, a defining or forgetting form, defines or forgets: a symbol
or a (SETF symbol) list, or NIL."
  (let ((raw (second act)))
    (cond ((and (consp raw) (eq (first raw) 'quote)) (second raw))
          ((and (consp raw) (eq (first raw) 'setf)) raw)
          ((consp raw) (and (symbolp (first raw)) (first raw)))
          (t raw))))

(defun standing-name-p (name)
  "Whether NAME already answers in this image: a function, a variable or a
class."
  (typecase name
    (null nil)
    (keyword nil)
    (symbol (and (or (fboundp name) (boundp name) (find-class name nil)) t))
    (cons (and (ignore-errors (fboundp name)) t))
    (t nil)))

(defun escrow-hook (next form &rest more)
  "Advice on NLK:DEFINITION-ESCROW, which runs before a form evaluates: a fork's
form defines and forgets only names of its own -- one that already answers in
the image, or that a sibling claimed, is refused and the form never runs.
Every other session's form is NEXT's alone."
  (let ((session nlk:*scribe-session-id*))
    (when (fork-p session)
      (let ((claimed '()))
        (nlk::walk-toplevel-acts
         form
         (lambda (act)
           (let* ((name (act-name act))
                  (owner (and name (gethash name *owners*))))
             (cond ((or (null name) (equal owner session)))
                   (owner
                    (fail "~(~s~) is another fork's: give yours a name of its own. The form was not evaluated." name))
                   ((standing-name-p name)
                    (fail "~(~s~) already answers in the operator's image, which every fork runs in, so a fork may not define or forget it: give yours a name of its own. The form was not evaluated." name))
                   ((nlk:definition-key act) (push name claimed))))))
        (dolist (name claimed) (setf (gethash name *owners*) session))))
    (apply next form more)))

(defun record-hook (next form form-string end &rest keys
                    &key (session-id nlk:*scribe-session-id*) &allow-other-keys)
  "Advice on NLK:RECORD-DEFINITIONS: what a fork defines stays live in the image
and is never written to the layer -- the parent keeps what it chooses -- and
is noted for the call waiting on the fork. Every other session's is NEXT's."
  (if (not (fork-p session-id))
      (apply next form form-string end keys)
      (let ((acts '()))
        (nlk::walk-toplevel-acts form (lambda (act) (when (nlk:definition-key act) (push act acts))))
        (dolist (act acts) (setf (gethash (act-name act) *made*) t))
        (when (gethash session-id *waiting*)
          (setf (getf (gethash session-id *waiting*) :acts)
                (append acts (getf (gethash session-id *waiting*) :acts))))
        (when acts
          (let ((names (let ((*package* (find-package '#:nodecode.evolved)) (*print-case* :downcase))
                         (mapcar (lambda (act) (prin1-to-string (act-name act))) (reverse acts)))))
            (format t "~&[fork: ~{~a~^, ~} ~:[is~;are~] live in this image and not kept: the parent keeps what it chooses]~%"
                    names (rest names))))
        nil)))

(defun unkept-hook (next before form kept)
  "Advice on NLK:REPORT-UNKEPT-CHANGES, which tells an eval what its form changed
that the layer will not carry: a fork's eval is told nothing -- what a fork
defines is never kept, and RECORD-HOOK says so -- and in any other eval, a name
a fork defined while its window was open is no change of that eval's form."
  (cond ((fork-p nlk:*scribe-session-id*) nil)
        ((and before (plusp (hash-table-count *made*)))
         ;; the window takes each such name as it stands now: unchanged
         (let ((snapshot (car before)))
           (dolist (name (sb-ext:with-locked-hash-table (*made*)
                           (loop for name being the hash-keys of *made* collect name)))
             (let ((symbol (if (consp name) (second name) name)))
               (when (symbolp symbol)
                 (loop for (kind object) on (nlk::definition-objects symbol) by #'cddr
                       when (and object (eq (eq kind :setf) (consp name)))
                         do (setf (gethash (cons kind symbol) snapshot) object)))))
           (funcall next before form kept)))
        (t (funcall next before form kept))))

;;; --- a check, in a fresh process -------------------------------------------------

(defvar *ask* 'ask-at-once
  "How checks are put to fresh processes: (lambda (JOBS DIRECTORY)) => a row a
job, as ASK-AT-ONCE answers them. A test's stub.")

(defun check-said (check)
  "CHECK as the words that name it."
  (let ((*package* (find-package '#:nodecode.evolved)) (*print-case* :downcase) (*print-pretty* t))
    (if (stringp check)
        (format nil "the shell command ~s exits 0" check)
        (format nil "~s answers true" check))))

(defun check-form (check directory &optional version)
  "What a fresh process answers for CHECK, VERSION -- a DEFUN -- evaluated first:
1 when it passes, 0 when it does not, -1 when it signals. A string is a shell
command, run in DIRECTORY, that passes by exiting 0."
  `(handler-case
       (progn ,@(and version (list version))
              ,(if (stringp check)
                   `(if (eql 0 (nth-value 2 (nlk:run-bounded (list "sh" "-c" ,check)
                                                             :seconds 120 :directory ,directory)))
                        1 0)
                   `(if ,check 1 0)))
     (error () -1)))

(defun form-text (form)
  "FORM as text that reads back the same in any package: every symbol written
with its own."
  (with-standard-io-syntax
    (let ((*package* (find-package '#:keyword))
          (*print-circle* t))
      (prin1-to-string form))))

(defun answer-words (answer)
  "Why a fresh process's ANSWER to a check is no pass, in words."
  (case (getf answer :score)
    (0 "the check answered false")
    (-1 "the check signalled")
    (t (format nil "the check could not be asked: ~a" (or (getf answer :detail) "no answer")))))

(defun since (began)
  "Seconds since BEGAN, an internal real time."
  (/ (float (- (get-internal-real-time) began)) internal-time-units-per-second))

(defun in-scratch (function)
  "FUNCTION called on a fresh directory under this home's tmp/, deleted after."
  (let ((directory (uiop:ensure-directory-pathname
                    (nlk:home-path (format nil "tmp/fork-~36r/" (random (expt 36 10) (make-random-state t)))))))
    (ensure-directories-exist directory)
    (unwind-protect (funcall function directory)
      (ignore-errors (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defun ask-at-once (jobs directory)
  "Each JOB, (GENERATION TEXT): GENERATION laid into a throwaway home of its own
under DIRECTORY, one after another, then every TEXT asked at once, each in a
fresh process of this organism under its home (NLK:TRIAL). => a row a job, in
order: (:SCORE S :DETAIL D :NOTES N :SECONDS T), S the number the process
answered, or NIL when it answered none."
  (let* ((seconds *probe-seconds*)
         (homes (loop for (generation) in jobs
                      for k from 1
                      collect (merge-pathnames (format nil "~d-g~d/" k generation) directory)))
         (laid (loop for (generation) in jobs
                     for home in homes
                     collect (handler-case
                                 (list :notes (nth-value 1 (nlk:build-generation
                                                            generation (merge-pathnames "cells/" home))))
                               (error (condition)
                                 (list :failed (format nil "generation ~d could not be laid: ~a" generation condition))))))
         (threads (loop for (generation text) in jobs
                        for home in homes
                        for lay in laid
                        collect (let ((home home) (text text) (lay lay))
                                  (bt2:make-thread
                                   (lambda ()
                                     (if (getf lay :failed)
                                         (list :score nil :detail (getf lay :failed) :seconds 0.0)
                                         (let ((began (get-internal-real-time)))
                                           (handler-case
                                               (multiple-value-bind (score detail)
                                                   (nlk:trial text :home home :seconds seconds)
                                                 (list :score score :detail detail :seconds (since began)))
                                             (error (condition)
                                               (list :score nil :detail (princ-to-string condition)
                                                     :seconds (since began)))))))
                                   :name (format nil "fork-trial-g~d" generation))))))
    (unwind-protect
         (loop for thread in threads
               for lay in laid
               collect (append (bt2:join-thread thread) (list :notes (getf lay :notes))))
      (dolist (home homes)
        (ignore-errors (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))))

;;; --- the way back ----------------------------------------------------------------

(defun spread (items width)
  "At most WIDTH of ITEMS, evenly spaced across them."
  (let ((count (length items)))
    (if (<= count width)
        items
        (loop for k from 1 to width collect (nth (floor (* k count) (1+ width)) items)))))

(defun bisect (numbers ask &optional (width +width+))
  "Where NUMBERS -- generations, ascending, two at least -- turn from passing to
failing. ASK takes some of them and answers a row each, asked at once:
(:SCORE S ...), S 1 passing, 0 or -1 failing, NIL unanswered. Both ends go
first, then up to WIDTH numbers between the newest pass and the oldest
failure, a round at a time, until no unasked one lies between; one that
answers nothing is stepped over, never read as the break. The step found is
asked again, both its ends: a fresh boot that answers otherwise the second
time names no step. => (:VERDICT V :GOOD G :BAD B :ROUNDS R :TESTED ROWS
:AGAIN ROWS): V :FOUND, :UNSTEADY when the step's second answers differ,
:CLEAN when the newest passes, :PROGRAM when the oldest fails too,
:UNANSWERED when an end answered nothing."
  (let ((all (coerce numbers 'vector))
        (rows (make-hash-table))
        (rounds 0))
    (labels ((ask (indices)
               (incf rounds)
               (loop for index in indices
                     for row in (funcall ask (mapcar (lambda (index) (aref all index)) indices))
                     do (setf (gethash index rows) (list* :number (aref all index) row))))
             (score (index) (getf (gethash index rows) :score))
             (passed (index) (eql 1 (score index)))
             (failed (index) (member (score index) '(0 -1)))
             (answer (verdict good bad)
               (list :verdict verdict
                     :good (and good (aref all good)) :bad (and bad (aref all bad))
                     :rounds rounds
                     :tested (sort (loop for row being the hash-values of rows collect row)
                                   #'< :key (lambda (row) (getf row :number))))))
      (let ((good 0) (bad (1- (length all))))
        (ask (list good bad))
        (cond ((passed bad) (answer :clean bad nil))
              ((failed good) (answer :program nil good))
              ((not (and (passed good) (failed bad))) (answer :unanswered nil nil))
              (t (loop for between = (loop for index from (1+ good) below bad
                                           unless (nth-value 1 (gethash index rows)) collect index)
                       while between
                       do (ask (spread between width))
                          (let ((failure (loop for index from (1+ good) below bad
                                               when (failed index) return index)))
                            (when failure (setf bad failure)))
                          (let ((pass (loop for index downfrom (1- bad) above good
                                            when (passed index) return index)))
                            (when pass (setf good pass))))
                 (incf rounds)
                 (let ((again (loop for index in (list good bad)
                                    for row in (funcall ask (list (aref all good) (aref all bad)))
                                    collect (list* :number (aref all index) row))))
                   (append (answer (if (and (eql 1 (getf (first again) :score))
                                            (member (getf (second again) :score) '(0 -1)))
                                       :found
                                       :unsteady)
                                   good bad)
                           (list :again again)))))))))

(defun manifest (number)
  "Generation NUMBER's manifest."
  (find number (nlk:manifests) :key #'nlk:manifest-number))

(defun generations (from)
  "The generations FROM spans, ascending: G to the newest, or an (OLD NEW) pair;
a refusal when they are fewer than two."
  (let* ((sealed (sort (mapcar #'nlk:manifest-number (nlk:manifests)) #'<))
         (old (if (consp from) (reduce #'min from) from))
         (new (if (consp from) (reduce #'max from) (car (last sealed))))
         (numbers (remove-if-not (lambda (number) (and new (<= old number new))) sealed)))
    (when (< (length numbers) 2)
      (fail "a way back needs two sealed generations to tell apart, and ~s spans ~d of this home's ~d"
            from (length numbers) (length sealed)))
    numbers))

(defun change-line (change)
  "One change between two generations, as a line."
  (flet ((short (revision) (and (stringp revision) (subseq revision 0 (min 7 (length revision)))))
         (subject (commit) (format nil "~a ~a" (getf commit :sha) (nlk:clip (getf commit :subject) 90))))
    (let ((row (getf change :row))
          (before (getf change :before)))
      (case (getf change :change)
        (:binary (format nil "the binary ~a -> ~a" (getf change :from) (getf change :to)))
        (:added (format nil "~a added" (getf row :name)))
        (:removed (format nil "~a taken away" (getf row :name)))
        (t (format nil "~a ~:[~a -> ~a~;~2*edited~]~:[~; (uncommitted edits)~]~@[: ~{~a~^; ~}~]"
                   (getf row :name)
                   (equal (getf before :revision) (getf row :revision))
                   (short (getf before :revision)) (short (getf row :revision))
                   (getf row :dirty)
                   (mapcar #'subject (getf change :keeps))))))))

(defun bisect-report (numbers found check)
  "FOUND, what BISECT answered over NUMBERS, as the :BISECT report."
  (let* ((verdict (getf found :verdict))
         (good (getf found :good))
         (bad (getf found :bad))
         (tested (getf found :tested))
         (changes (and (eq verdict :found)
                       (mapcar #'change-line (nlk:generation-changes (manifest good) (manifest bad)))))
         (silent (loop for row in tested unless (getf row :score) collect (getf row :number)))
         (ends (intersection silent (list (first numbers) (car (last numbers)))))
         (said (check-said check)))
    (list :verdict verdict :range (list (first numbers) (car (last numbers)))
          :good good :bad bad :changes changes :rounds (getf found :rounds) :tested tested
          :again (getf found :again)
          :says (ecase verdict
                  (:found
                   (format nil "~a: it held at generation ~d, and had gone by ~d, both asked twice~@[ (~{~d~^, ~} between could not be asked)~]. ~:[No folder moved in that step, so the break is outside what a generation records.~;What moved: ~:*~{~a~^; ~}.~]"
                           said good bad
                           (remove-if-not (lambda (number) (< good number bad)) silent)
                           changes))
                  (:unsteady
                   (format nil "~a held at generation ~d and had gone by ~d, but asked again a fresh boot of them answered ~{~a~^ and ~}: the check does not answer the same way twice, so no step is named. A check that reads what a boot evaluates -- a layer variable fetched at load, the network -- answers that way; check what a boot cannot vary."
                           said good bad
                           (mapcar (lambda (row) (format nil "~d: ~a" (getf row :number) (getf row :score)))
                                   (getf found :again))))
                  (:clean
                   (format nil "~a holds at the newest generation, ~d: nothing this home sealed explains the break, so it lies outside what a generation records -- the box, a secret, the world the work runs in."
                           said good))
                  (:program
                   (format nil "~a fails as far back as generation ~d, the oldest asked: every generation ran on today's binary, so no change this home sealed explains it, and the program itself is to blame. Rebuilding old binaries is not built yet."
                           said bad))
                  (:unanswered
                   (format nil "generation~p ~{~d~^ and ~} answered nothing (~a), and a way back needs both ends to answer: give a range whose ends boot."
                           (length ends) ends
                           (getf (find (first ends) tested :key (lambda (row) (getf row :number))) :detail)))))))

(defun way-back (from check session)
  "The way back FROM spans -- G to the newest, or an (OLD NEW) pair -- judged by
CHECK in SESSION's folder. => the :BISECT report."
  (let* ((numbers (generations from))
         (text (form-text (check-form check (nlk:find-session-cwd session))))
         (found (in-scratch
                 (lambda (scratch)
                   (bisect numbers
                           (lambda (batch)
                             (funcall *ask* (mapcar (lambda (number) (list number text)) batch) scratch)))))))
    (bisect-report numbers found check)))

;;; --- the race --------------------------------------------------------------------

(defparameter +race-calls+ 20 "The live calls each entrant of a race is timed on.")

(defparameter +race-least+ 8 "The timed calls below which an entrant's time is not read.")

(defparameter *race-seconds* 300 "How long a race listens for live calls.")

(defparameter +race-faster+ 0.05 "How much faster a version has to be to displace what stands.")

(defvar *serving* nil
  "True on a thread inside a call a race serves: a call made within it is what
stands' own, untimed.")

(defun rival-name (n keep)
  "The name fork N writes its version of KEEP under: FORK-<n>/KEEP, in KEEP's
package."
  (intern (format nil "FORK-~d/~a" n (symbol-name keep)) (symbol-package keep)))

(defun keep-words (keep rival check)
  "What a :KEEP call adds to the task of the fork writing RIVAL."
  (let ((*package* (find-package '#:nodecode.evolved))
        (*print-case* :downcase))
    (let ((name (prin1-to-string keep))
          (lambda-list (ignore-errors (sb-kernel:%fun-lambda-list (fdefinition keep)))))
      (format nil *keep-input* name (prin1-to-string rival) name
              (and (consp lambda-list) (prin1-to-string lambda-list))
              name (check-said check) name))))

(defun fork-version (fork keep)
  "What FORK wrote of KEEP -- its newest DEFUN of its own name for it, that name
KEEP again throughout -- as a version, or NIL when it wrote none."
  (let* ((rival (getf fork :rival))
         (act (find-if (lambda (act) (and (eq rival (act-name act))
                                          (string= "DEFUN" (symbol-name (first act)))))
                       (getf fork :acts))))
    (and act (list :label (format nil "fork ~d" (getf fork :n))
                   :source (getf fork :session)
                   :form (subst keep rival act)))))

(defun generation-version (generation keep)
  "KEEP as GENERATION's layer had it, as a version -- read from that
generation's copy of the layer file that defines KEEP today -- or NIL when
KEEP is no layer definition, or that file had none."
  (let* ((folder (format nil "~a/" (nlk::layer-name)))
         (path (first (first (nlk:definition-location keep))))
         (relative (and path (uiop:subpathp path (merge-pathnames folder nlk::*cells-directory*)))))
    (when relative
      (in-scratch
       (lambda (scratch)
         (let ((cells (merge-pathnames "cells/" scratch)))
           (nlk:build-generation generation cells)
           (let* ((file (merge-pathnames relative (merge-pathnames folder cells)))
                  (hit (and (probe-file file)
                            (first (nlk::file-definitions file :name (symbol-name keep) :read-eval t))))
                  (form (and hit
                             (ignore-errors
                              (destructuring-bind (key start end package) hit
                                (declare (ignore key))
                                (with-standard-io-syntax
                                  (let ((*package* package) (*read-eval* t))
                                    (read-from-string (uiop:read-file-string file) t nil
                                                      :start start :end end))))))))
             (and (consp form) (string= "DEFUN" (symbol-name (first form))) (eq keep (second form))
                  (list :label (format nil "generation ~d" generation)
                        :source (format nil "generation ~d's layer" generation)
                        :form form)))))))))

(defun versions (keep forks bisect)
  "What races in KEEP's place: each fork's version, then, beside a way back that
found its step, KEEP as the last good generation's layer had it."
  (append (remove nil (mapcar (lambda (fork) (fork-version fork keep)) forks))
          (and (eq :found (getf bisect :verdict))
               (remove nil (list (generation-version (getf bisect :good) keep))))))

(defun misfit (form standing)
  "Why FORM cannot stand in for STANDING -- it is no DEFUN, or it does not take
every call STANDING's lambda list takes -- or NIL."
  (let ((old (ignore-errors (sb-kernel:%fun-lambda-list standing)))
        (new (and (consp form) (third form))))
    (cond ((not (and (consp form) (string= "DEFUN" (symbol-name (first form))) (listp new)))
           "it is not a defun")
          ((and (listp old) (not (nlk::takes-every-call-p new old)))
           (let ((*package* (or (symbol-package (second form)) *package*)) (*print-case* :downcase))
             (format nil "it takes ~s where every call of it today takes ~s" new old))))))

(defun version-function (form keep)
  "FORM, a DEFUN of KEEP, made a function bound to no name -- its own calls of
KEEP its own -- or NIL when it does not compile clean in this image."
  (destructuring-bind (lambda-list &rest body) (cddr form)
    (handler-case
        (multiple-value-bind (function warned failed)
            (handler-bind ((warning #'muffle-warning) (sb-ext:compiler-note #'muffle-warning))
              (compile nil `(lambda (&rest arguments)
                              (labels ((,keep ,lambda-list ,@body))
                                (apply #',keep arguments)))))
          (declare (ignore warned))
          (and (not failed) function))
      (error () nil))))

(defun median (times)
  "The middle of TIMES, or NIL for none."
  (let ((sorted (sort (copy-list times) #'<))
        (count (length times)))
    (cond ((zerop count) nil)
          ((oddp count) (nth (floor count 2) sorted))
          (t (/ (+ (nth (1- (floor count 2)) sorted) (nth (floor count 2) sorted)) 2)))))

(defun nanoseconds ()
  "The monotonic clock, in nanoseconds: fine enough to time one call, which
GET-INTERNAL-REAL-TIME is not -- SBCL reads Linux's coarse clock for it, one
kernel tick (1 ms here), so a 100 us call timed with it reads 0."
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))

(defun race-live (keep entrants)
  "Serve KEEP's live calls among ENTRANTS -- (:LABEL L :FUNCTION F), or
(:LABEL L :STANDING T) for what stands -- one entrant a call in turn, each
call timed, until each has +RACE-CALLS+ calls or *RACE-SECONDS* pass. A call
made inside a served one is what stands' own, untimed; a version that
signals is out, and that call is served again by what stands. => a record an
entrant: (:LABEL :STANDING :CALLS :MEDIAN-US :BROKEN)."
  (let* ((lock (bt2:make-lock :name "fork-race"))
         (records (mapcar (lambda (entrant) (list* :calls 0 :times '() :broken nil entrant)) entrants))
         (ring (coerce records 'vector))
         (tick 0)
         (deadline (+ (get-internal-real-time) (* *race-seconds* internal-time-units-per-second))))
    (labels ((open-p (record)
               (and (not (getf record :broken)) (< (getf record :calls) +race-calls+)))
             (pick ()
               (bt2:with-lock-held (lock)
                 (loop repeat (length ring)
                       for record = (aref ring (mod (incf tick) (length ring)))
                       when (open-p record) return record)))
             (timed (record thunk)
               (let ((began (nanoseconds)))
                 (multiple-value-prog1 (let ((*serving* t)) (funcall thunk))
                   (let ((micros (/ (- (nanoseconds) began) 1000.0)))
                     (bt2:with-lock-held (lock)
                       (when (< (getf record :calls) +race-calls+)
                         (incf (getf record :calls))
                         (push micros (getf record :times))))))))
             (serve (next arguments)
               (let ((record (and (not *serving*) (pick))))
                 (cond ((null record) (apply next arguments))
                       ((getf record :standing) (timed record (lambda () (apply next arguments))))
                       (t (let ((broken (block version
                                          (handler-bind
                                              ((error (lambda (condition)
                                                        ;; Esc unwinding a turn is no verdict on the version
                                                        (unless (typep condition 'nlk::turn-cancelled-condition)
                                                          (return-from version condition)))))
                                            (return-from serve
                                              (timed record (lambda () (apply (getf record :function) arguments))))))))
                            (bt2:with-lock-held (lock)
                              (setf (getf record :broken)
                                    (handler-case (princ-to-string broken)
                                      (error () (princ-to-string (type-of broken))))))
                            (apply next arguments)))))))
      (nlk:hook keep "fork-race" (lambda (next &rest arguments) (serve next arguments)))
      (unwind-protect
           (loop until (or (>= (get-internal-real-time) deadline) (notany #'open-p records))
                 do (sleep 0.1))
        (nlk:unhook keep "fork-race"))
      (bt2:with-lock-held (lock)
        (loop for record in records
              collect (list :label (getf record :label) :standing (getf record :standing)
                            :calls (getf record :calls) :median-us (median (getf record :times))
                            :broken (getf record :broken)))))))

(defun timed-records (records)
  "RECORDS timed enough to read: never broken, +RACE-LEAST+ calls at least."
  (remove-if (lambda (record) (or (getf record :broken) (< (getf record :calls) +race-least+)))
             records))

(defun fastest (records)
  "The version among RECORDS timed enough to read with the least median, or NIL."
  (first (sort (remove-if (lambda (record) (getf record :standing)) (timed-records records))
               #'< :key (lambda (record) (getf record :median-us)))))

(defun choose (standing-passed passing records)
  "The label of the version that wins, or NIL. What stands having passed the
check, the fastest version timed, if +RACE-FASTER+ faster than it; what stands
having failed it, the fastest version timed, else the first of PASSING that
never signalled."
  (let ((standing (find-if (lambda (record) (getf record :standing)) (timed-records records)))
        (best (fastest records)))
    (cond (standing-passed
           (and best standing
                (< (getf best :median-us) (* (- 1 +race-faster+) (getf standing :median-us)))
                (getf best :label)))
          (best (getf best :label))
          (t (loop for label in passing
                   unless (getf (find label records :key (lambda (record) (getf record :label)) :test #'equal)
                                :broken)
                     return label)))))

(defun swap (keep form)
  "FORM, a DEFUN of KEEP, live in KEEP's place and HELD (NLK:HOLD-DEFINITIONS):
written to the layer after one clean call made outside an eval, the
definition it displaced put back if that call signals."
  (let* ((previous (fdefinition keep))
         (*package* (symbol-package keep))
         (text (with-standard-io-syntax
                 (let ((*package* (symbol-package keep)) (*print-case* :downcase)
                       (*print-pretty* t) (*print-right-margin* 100) (*print-readably* nil))
                   (prin1-to-string form)))))
    (handler-bind ((warning #'muffle-warning) (sb-ext:compiler-note #'muffle-warning))
      (eval form))
    ;; its own words say the name runs on the eval's stack; the race's report says what is true
    (let ((*standard-output* (make-broadcast-stream)))
      (nlk:hold-definitions (list (list (symbol-name keep) keep previous)) form text (length text)
                            :session-id nlk:*scribe-session-id*))
    text))

(defun race-says (keep standing-passed passing records winner)
  "The race's answer, in words."
  (let* ((name (let ((*package* (find-package '#:nodecode.evolved)) (*print-case* :downcase))
                 (prin1-to-string keep)))
         (won (find winner records :key (lambda (record) (getf record :label)) :test #'equal))
         (standing (find-if (lambda (record) (getf record :standing)) records))
         (best (fastest records))
         (broken (loop for record in records when (getf record :broken) collect (getf record :label)))
         (calls (reduce #'+ records :key (lambda (record) (getf record :calls)))))
    (flet ((us (record) (if (and record (getf record :median-us))
                            (format nil "~,1f us" (getf record :median-us))
                            "untimed")))
      (cond
        ((and winner standing-passed)
         (format nil "~a's version is live in ~a's place, HELD: ~a a call against ~a for what stands, over ~d and ~d live calls. One clean call outside an eval writes it to the layer; (revert '~a) puts the old one back until then."
                 winner name (us won) (us standing) (getf won :calls) (getf standing :calls) name))
        (winner
         (format nil "what stands fails the check and ~a's version passes it, so it is live in ~a's place, HELD~@[ (~a a call)~]. One clean call outside an eval writes it to the layer; (revert '~a) puts the old one back until then."
                 winner name (and won (getf won :median-us) (us won)) name))
        ((null passing)
         (format nil "no version passed the check~:[, and what stands fails it too~;~], so ~a stays as it was."
                 standing-passed name))
        ((and best standing (>= (getf standing :calls) +race-least+))
         (format nil "nothing beat what stands by ~d%: it takes ~a a call and the best version, ~a, ~a, so ~a stays as it was."
                 (round (* 100 +race-faster+)) (us standing) (getf best :label) (us best) name))
        (t
         (format nil "~a was called ~d time~:p while the race listened (~d s), too few to time ~d version~:p against what stands, ~d calls each at least~@[; ~{~a~^, ~} signalled on a live call~], so it stays as it was."
                 name calls *race-seconds* (length passing) +race-least+ broken))))))

(defun race (keep versions check session generation)
  "Race VERSIONS -- (:LABEL :SOURCE :FORM), FORM a DEFUN of KEEP -- against what
stands, judged by CHECK: each that takes every call KEEP takes is put to
CHECK in a fresh process laid at GENERATION, beside what stands; those that
pass are timed on KEEP's live calls (RACE-LIVE); the winner goes in KEEP's
place, HELD (SWAP). => the :RACE report."
  (let* ((standing (fdefinition keep))
         (directory (nlk:find-session-cwd session))
         (shaped (loop for version in versions
                       for why = (misfit (getf version :form) standing)
                       collect (if why (list* :out why version) version)))
         (fit (remove-if (lambda (row) (getf row :out)) shaped))
         (answers (and versions
                       (in-scratch
                        (lambda (scratch)
                          (funcall *ask*
                                   (cons (list generation (form-text (check-form check directory)))
                                         (loop for row in fit
                                               collect (list generation
                                                             (form-text (check-form check directory (getf row :form))))))
                                   scratch)))))
         (standing-passed (eql 1 (getf (first answers) :score)))
         (tried (loop for row in fit
                      for answer in (rest answers)
                      collect (if (eql 1 (getf answer :score))
                                  (let ((function (version-function (getf row :form) keep)))
                                    (if function
                                        (list* :function function row)
                                        (list* :out "it passed the check, and does not compile clean in this image" row)))
                                  (list* :out (answer-words answer) row))))
         (passing (loop for row in tried unless (getf row :out) collect (getf row :label)))
         (entrants (append (and standing-passed (list (list :label "what stands" :standing t)))
                           (loop for row in tried
                                 unless (getf row :out)
                                   collect (list :label (getf row :label) :function (getf row :function)))))
         (records (and passing (or standing-passed (rest passing))
                       (race-live keep entrants)))
         (winner (and passing (choose standing-passed passing records)))
         (form (and winner (getf (find winner tried :key (lambda (row) (getf row :label)) :test #'equal)
                                 :form))))
    (when form (swap keep form))
    (list :keep keep
          :standing (let ((record (find-if (lambda (record) (getf record :standing)) records)))
                      (list :passed standing-passed
                            :detail (and answers (not standing-passed) (answer-words (first answers)))
                            :calls (and record (getf record :calls))
                            :median-us (and record (getf record :median-us))))
          :versions (loop for row in (append (remove-if-not (lambda (row) (getf row :out)) shaped) tried)
                          for record = (find (getf row :label) records
                                             :key (lambda (record) (getf record :label)) :test #'equal)
                          collect (list :label (getf row :label) :source (getf row :source)
                                        :passed (and (member (getf row :label) passing :test #'equal) t)
                                        :out (getf row :out)
                                        :calls (and record (getf record :calls))
                                        :median-us (and record (getf record :median-us))
                                        :broken (and record (getf record :broken))))
          :winner (and form winner)
          :says (if versions
                    (race-says keep standing-passed passing records (and form winner))
                    (format nil "no version of ~(~s~) was written to race, so it stays as it was" keep)))))

(defun newest-generation ()
  "The newest generation this home sealed, or a refusal when it sealed none."
  (let ((numbers (mapcar #'nlk:manifest-number (nlk:manifests))))
    (if numbers
        (reduce #'max numbers)
        (fail "this home has sealed no generation to try a version in"))))

;;; --- the verb --------------------------------------------------------------------

(define-verb run (&rest arguments)
  "Fork the calling session at the turn running it, once per task, and wait
for every fork's turn to end -- taken back to an earlier turn (:FROM -N, or
a turn's id), after a way back through the generations (:FROM G, or an (OLD
NEW) pair, with :CHECK), racing the versions of a name the forks write
(:KEEP NAME, with :CHECK), or both. => one plist a task, in order: (:TASK
:SESSION :STATUS :ANSWER :RAN); with a way back or a race, (:BISECT REPORT
:FORKS RESULTS :RACE REPORT), each there when the call made it."
  (multiple-value-bind (tasks seconds from check keep) (parse arguments)
    (multiple-value-bind (session turn) (caller)
      (let* ((anchor (if (turn-from-p from) (earlier-turn session turn from) turn))
             (bisect (and from (not (turn-from-p from)) (way-back from check session)))
             (rivals (and keep (loop for n from 1 to (length tasks) collect (rival-name n keep))))
             (words (loop for task in tasks
                          for n from 1
                          collect (format nil "~?~@[~a~]~@[~a~]"
                                          (if (equal anchor turn) *task-input* *earlier-input*)
                                          (list n (length tasks) task (rest tasks))
                                          (and bisect (format nil *bisect-input* (getf bisect :says)))
                                          (and keep (keep-words keep (nth (1- n) rivals) check))))))
        (if (not (or bisect keep))
            (values (fork-tasks session anchor tasks words seconds nil))
            (unwind-protect
                 (multiple-value-bind (results forks)
                     (and tasks (fork-tasks session anchor tasks words seconds rivals))
                   (append (and bisect (list :bisect bisect))
                           (and tasks (list :forks results))
                           (and keep (list :race (race keep (versions keep forks bisect) check session
                                                       (newest-generation))))))
              ;; the versions raced were the call's own scratch
              (dolist (rival rivals)
                (when (fboundp rival) (fmakunbound rival)))))))))

;;; --- the hooks -------------------------------------------------------------------

(defun observe (op)
  "A turn ended somewhere: wake whoever waits on it, when it was a fork's.
Publishing thread: a lookup and a signal."
  (when (member (nlk:frame-fact op) +turn-ends+ :test #'equal)
    (let ((waiting (gethash (getf op :session-id) *waiting*)))
      (when waiting
        (bt2:signal-semaphore (getf waiting :semaphore))))))

(defun budget-hook (next turn)
  "Advice on NLE:TURN-BUDGET: a fork's turn runs under the seconds its call
gave; every other turn is NEXT's."
  (let ((waiting (gethash (getf turn :session-id) *waiting*)))
    (if waiting
        (list :seconds (getf waiting :seconds))
        (funcall next turn))))

(defun naming-hook (next session-id text)
  "Advice on NLE:NAME-SESSION: a fork is never named, its name a model call.
Every other session is NEXT's."
  (unless (fork-p session-id)
    (funcall next session-id text)))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell fork
  (:section ("fork")
    (:guide "fork:run hands a turn's independent tasks to forks of its session; seconds bounds each fork's turn")
    ("seconds" :integer :default 900 :min 30
               :doc "seconds a fork's turn may run before its tools are refused"))
  (:hook :frame (nle:observer #'observe "fork"))
  (:hook 'nle:turn-budget #'budget-hook)
  (:hook 'nle:name-session #'naming-hook)
  (:hook 'nlk:definition-escrow #'escrow-hook)
  (:hook 'nlk:record-definitions #'record-hook)
  (:hook 'nlk:report-unkept-changes #'unkept-hook)
  (:help :fork "fork: whenever an ask splits into two or more independent investigations that each take several reads, run them at once with (fork:run \"task 1\" \"task 2\" ...) instead of one after another: each of up to 4 forks of this session holds this whole context, and the call answers their results. Also for a probe whose output would bury this context; never for a linear task or a quick read" +primer+))
