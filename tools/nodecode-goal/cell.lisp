;;;; cell.lisp --- a goal a fact, its check after each turn, the next turn until it passes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A goal is a condition a session works toward across turns: the operator
;;;; states when the work counts as done (/goal CONDITION), and the session
;;;; keeps taking turns until a check passes or its turns run out. The check
;;;; is a shell command that exits 0 in the session's folder (--check
;;;; COMMAND), or, without one, a judge: a fork of the session at the turn
;;;; that just ended, told to change nothing, answering MET or NOT MET with
;;;; its evidence. Not met, the session's next turn is admitted with what the
;;;; check said; met, spent, cancelled or failed, the goal ends and a [goal]
;;;; note in the session says so, which a channel lane posts to its room.
;;;;
;;;; Every step is a fact on the session's own log, on the goal's aggregate
;;;; (goal/ID): goal.set, goal.judging (a judge forked), goal.checked (a
;;;; verdict) and goal.ended (met, spent, cancelled, failed, stopped,
;;;; replaced). Where a goal stands is a fold of those facts and the
;;;; session's turns, so a restart loses nothing: the worker reads the log
;;;; and takes the one step each goal owes. A step taken twice admits one
;;;; turn -- every turn the cell starts carries a command id made of the
;;;; goal and its number.
;;;;
;;;; THREAD RULE: the :FRAME hook only pokes the worker. Every store read,
;;;; check, fork and submit runs on the worker thread or a verb's caller.

(in-package #:nodecode-goal)

;;; --- the manual ---------------------------------------------------------------
;;; (help :goal) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter +primer+
  "A goal keeps this session taking turns until a check passes, through the
nodecode-goal cell: Lisp functions in the goal: package, called through eval.
Each answers a string; a refusal is ERROR: GOAL-ERROR with the reason.
  (goal:start \"condition\" &key check turns)   set this session's goal,
      replacing one it had. CHECK, a shell command, makes it done when that
      exits 0 in the session's folder; without one a judge -- a fork of this
      session at the turn that just ended, told to change nothing -- answers
      MET or NOT MET. TURNS caps the turns it may take (goal.turns unless
      given). Set from inside a turn, the check runs when that turn ends.
  (goal:status)   the goal, its check, the turns checked, the last verdict.
  (goal:stop \"why\")   end it when it cannot be met without the operator,
      and say in your answer what you need.
After each turn of the session ends the check runs. Not met with turns left,
the session's next turn starts on its own, its input beginning [goal] and
carrying what the check said: the cell's words, not the operator's. Met,
spent, cancelled (the operator's stop) or failed, the goal ends and a [goal]
note says so. The operator sets one with /goal CONDITION [--check COMMAND]
[--turns N], reads it with /goal and ends it with /goal stop."
  "What (help :goal) answers while the cell runs.")

(defparameter +usage+ "/goal CONDITION [--check COMMAND] [--turns N] | /goal | /goal stop")

;;; --- the words -------------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *kickoff-input*
  "[goal] Work toward this goal until it holds: ~a~%~a after each turn you end; until it passes you are given the next turn, at most ~d in all. If it cannot be met without the operator, say what you need and call (goal:stop \"why\")."
  "The first turn's input, over the condition, the check's phrase and the
turns the goal may take.")

(defparameter *continue-input*
  "[goal] Not done yet: turn ~d of ~d. The goal: ~a~%The check (~a) said:~%~a~%Keep working toward it; the check runs again when this turn ends."
  "A later turn's input, over its number, the turns the goal may take, the
condition, the check's label and what the check said.")

(defparameter *judge-input*
  "[goal check] You are this goal's check, not its worker: decide whether it holds now.~%The goal: ~a~%Read the transcript above and verify what you can yourself -- read the files, run the tests, query the live state. Change nothing: no edits, no commits, no posts, no messages. Then answer with a first line that is exactly MET or NOT MET, and under it at most three lines of evidence; for NOT MET, the specific gap the worker has to close next."
  "A judge's input, over the condition.")

(defparameter *detail-chars* 1500
  "How much of what a check said a fact keeps and the next turn is told.")

(defparameter *condition-chars* 4000
  "The longest condition, in characters.")

(defparameter +aggregate+ "goal")
(defparameter +kind-set+ "goal.set")
(defparameter +kind-judging+ "goal.judging")
(defparameter +kind-checked+ "goal.checked")
(defparameter +kind-ended+ "goal.ended")
(defparameter +turn-ends+ '("turn.completed" "turn.failed" "turn.cancelled"))

(defvar *worker* nil
  "The worker thread while the cell runs (an NLK:WORKER), or NIL.")

(defvar *tick-lock* (bt2:make-lock :name "goal tick")
  "Held across one pass of the worker, so two passes never take one step
twice; the verbs take *LOCK* alone and never wait on a running check.")

(defun poke ()
  "Wake the worker: a turn ended, or a goal was set."
  (nlk:worker-poke *worker*))

;;; --- the facts and their fold ----------------------------------------------------

(defun record (session kind id &rest payload)
  "Append one KIND fact on goal ID's aggregate, in SESSION's log."
  (values (nlk:record-event session kind (list* :goal id payload)
                            :aggregate-type +aggregate+ :aggregate-id id)))

(defun facts (id kind &key (order :oldest) limit)
  "Goal ID's KIND facts' payloads, in ORDER."
  (nlk:events :aggregate-type +aggregate+ :aggregate-id id :kind kind
              :order order :limit limit :as :payloads))

(defun read-goal (session payload position)
  "A goal.set fact as the plist the steps read."
  (let ((object (nlk:decode-json payload)))
    (list :id (nlk:json-value object :string "goal")
          :session session
          :condition (nlk:json-value object :string "condition")
          :check (nlk:json-value object :string "check")
          :turns (or (nlk:json-value object :integer "turns") 1)
          :position position)))

(defun ended (id)
  "Goal ID's goal.ended payload, or NIL while it runs."
  (first (facts id +kind-ended+ :limit 1)))

(defun newest-goal (session)
  "SESSION's newest goal, running or ended, or NIL."
  (let ((row (nlk:events :session-id session :kind +kind-set+ :order :newest :limit 1
                         :as :row :columns '("payload" "log_position"))))
    (and row (read-goal session (first row) (second row)))))

(defun goal-of (session)
  "SESSION's running goal, or NIL."
  (let ((goal (newest-goal session)))
    (and goal (not (ended (getf goal :id))) goal)))

(defun running-goals ()
  "Every running goal whose session is still there."
  (loop for (session payload position) in (nlk:events :kind +kind-set+ :as :rows
                                                      :columns '("session_id" "payload" "log_position"))
        for goal = (read-goal session payload position)
        when (and (not (ended (getf goal :id))) (nlk:session-exists-p session))
          collect goal))

(defun checks (id)
  "Goal ID's verdicts, oldest first."
  (facts id +kind-checked+))

(defun check-number (check) (nlk:json-value check :integer "n"))
(defun check-turn (check) (nlk:json-value check :string "turn"))

(defun last-turn (session after)
  "(TURN-ID KIND PAYLOAD) of SESSION's newest ended turn past the log position
AFTER, PAYLOAD decoded; NIL when none ended since."
  (let ((row (nlk:events :session-id session :kind +turn-ends+ :after after
                         :order :newest :limit 1
                         :as :row :columns '("turn_id" "kind" "payload"))))
    (and row (list (first row) (second row) (ignore-errors (nlk:decode-json (third row)))))))

;;; --- the words a goal is said in -------------------------------------------------

(defun check-label (goal)
  "The check, named: its command quoted, or the judge."
  (if (getf goal :check) (format nil "`~a`" (getf goal :check)) "the judge"))

(defun check-phrase (goal)
  "How the goal is checked, as the first turn is told."
  (if (getf goal :check)
      (format nil "It counts as done when `~a` exits 0 in this session's folder; that runs"
              (getf goal :check))
      "A judge -- a fork of this session that changes nothing -- decides whether it holds"))

(defun tail (text limit)
  "The last LIMIT characters of TEXT, cut at the front."
  (if (> (length text) limit)
      (concatenate 'string "..." (subseq text (- (length text) limit)))
      text))

(defun standing (session)
  "SESSION's goal as it stands, or the last one's end."
  (let ((goal (goal-of session)))
    (if goal
        (let* ((id (getf goal :id))
               (checks (checks id))
               (last (car (last checks)))
               (judging (first (facts id +kind-judging+ :order :newest :limit 1))))
          (format nil "goal: ~a~%check: ~a~%checked ~d of ~d turns~:[~;; the judge is checking~]~@[~%last check: ~a~]"
                  (getf goal :condition)
                  (if (getf goal :check) (format nil "~a exits 0" (check-label goal)) "a judge, a fork of this session")
                  (length checks) (getf goal :turns)
                  (and judging (> (or (check-number judging) 0) (length checks)))
                  (and last (format nil "~a -- ~a" (nlk:json-value last :string "verdict")
                                    (nlk:clip (nlk:one-line (or (nlk:json-value last :string "detail") "")) 300)))))
        (let ((last (newest-goal session)))
          (if last
              (let ((end (ended (getf last :id))))
                (format nil "no goal running; the last, ~s, ended ~a~@[: ~a~]~%~a"
                        (nlk:clip (nlk:one-line (getf last :condition)) 120)
                        (nlk:json-value end :string "reason")
                        (nlk:json-value end :string "detail")
                        +usage+))
              (format nil "no goal on this session: ~a" +usage+))))))

(defun ended-text (goal reason detail)
  "The [goal] note that says GOAL ended for REASON."
  (let ((spent (length (checks (getf goal :id)))))
    (cond ((equal reason "met")
           (format nil "Goal met after ~d turn~:p. The check (~a): ~a" spent (check-label goal) detail))
          ((equal reason "spent")
           (format nil "Goal not met: all ~d turns spent. The last check (~a): ~a"
                   (getf goal :turns) (check-label goal) detail))
          (t (format nil "Goal stopped after ~d checked turn~:p: ~a" spent detail)))))

;;; --- the steps --------------------------------------------------------------------

(defun announce (goal text &aux (session (getf goal :session)))
  "Record TEXT in GOAL's session as one settled [goal] exchange that a room
posts as a note -- only while the session is idle. => the turn id, or NIL."
  (let ((cursor nil) (turn nil))
    (bt2:with-recursive-lock-held (nlk::*store-lock*)
      (unless (nlk:active-turn-p session)
        (setf cursor (ignore-errors (nle::session-cursor session))
              turn (nlk:record-exchange-turn
                    session
                    (format nil "[goal] ~a" (nlk:clip (nlk:one-line (getf goal :condition)) 200))
                    text
                    :command-id (format nil "goal:~a:ended" (getf goal :id))
                    :announce t
                    :source "goal"))))
    (when (and turn cursor)
      (handler-case (nle::publish-turn-facts session cursor)
        (error (condition)
          (warn "goal: the end of ~a was recorded but not published: ~a" (getf goal :id) condition))))
    turn))

(defun finish (goal reason detail &key (by "cell") (announce t) &aux (id (getf goal :id)))
  "End GOAL for REASON, once; ANNOUNCE says so in its session."
  (with-goal-lock
    (unless (ended id)
      (record (getf goal :session) +kind-ended+ id :reason reason :by by :detail detail)
      (when announce
        (announce goal (ended-text goal reason detail))))))

(defun turn-input (goal n)
  "The input GOAL's Nth turn is admitted with."
  (if (= n 1)
      (format nil *kickoff-input* (getf goal :condition) (check-phrase goal) (getf goal :turns))
      (let ((last (first (facts (getf goal :id) +kind-checked+ :order :newest :limit 1))))
        (format nil *continue-input* n (getf goal :turns) (getf goal :condition) (check-label goal)
                (or (and last (nlk:json-value last :string "detail")) "nothing")))))

(defun work (goal n &aux (id (getf goal :id)))
  "Admit GOAL's Nth turn into its session -- once, whoever asks twice."
  (if (> n (getf goal :turns))
      (finish goal "spent" "no turn is left")
      (handler-case
          (nle:submit (getf goal :session) (turn-input goal n)
                      :command-id (format nil "goal:~a:~d" id n) :source "goal" :source-id id)
        (error (condition)
          (finish goal "failed" (format nil "turn ~d could not start: ~a" n condition))))))

(defun conclude (goal turn n met detail &aux (id (getf goal :id)))
  "Record the verdict on GOAL's Nth checked turn TURN, then end the goal or
admit its next turn."
  (with-goal-lock
    (unless (or (ended id) (find n (checks id) :key #'check-number))
      (record (getf goal :session) +kind-checked+ id :n n :turn turn
              :verdict (if met "met" "not met") :by (if (getf goal :check) "command" "judge")
              :detail (nlk:clip detail *detail-chars*))
      (cond (met (finish goal "met" detail))
            ((>= n (getf goal :turns)) (finish goal "spent" detail))
            (t (work goal (1+ n)))))))

(defun run-check (goal &aux (command (getf goal :check)) (seconds (setting :check-seconds))
                            (cwd (nlk:find-session-cwd (getf goal :session))))
  "Run GOAL's check command in its session's folder. => (values MET DETAIL)."
  (if (null cwd)
      (values nil "the session names no folder to run the check in")
      (multiple-value-bind (out err status)
          (handler-case (nlk:run-bounded (list "sh" "-c" command) :seconds seconds :directory cwd)
            (error (condition) (values nil (princ-to-string condition) nil)))
        (let ((said (tail (nlk:trimmed (format nil "~@[~a~]~@[~%~a~]" out err)) *detail-chars*)))
          (values (eql status 0)
                  (format nil "~a~@[~%~a~]"
                          (cond ((eql status 0) "exited 0")
                                ((eq status :timeout) (format nil "ran past ~ds and was killed" seconds))
                                ((integerp status) (format nil "exited ~d" status))
                                (t "could not run"))
                          (and (plusp (length said)) said)))))))

;;; --- the judge --------------------------------------------------------------------

(defun judge-name (id n) (format nil "~a-check-~d" id n))

(defun judge-p (session)
  "Whether SESSION is a judge this cell forked."
  (and (stringp session) (uiop:string-prefix-p "goal-" session) (search "-check-" session) t))

(defun summon (goal turn n &aux (origin (getf goal :session)) (id (getf goal :id))
                                (child (judge-name id n)))
  "Fork GOAL's session at TURN as its Nth judge and put the question to it,
once, whoever asks twice."
  (unless (nlk:session-exists-p child)
    (nlk:create-session :id child :cwd (nlk:find-session-cwd origin) :parent origin
                        :anchor-turn-id turn)
    ;; the origin's model: its cached prefix is the judge's
    (multiple-value-bind (provider model) (nlk:session-model-selection origin)
      (when (or provider model)
        (nlk:record-session-model-selection child :provider provider :model model))))
  (nle:submit child (format nil *judge-input* (getf goal :condition))
              :command-id (format nil "goal:~a:judge:~d" id n) :source "goal" :source-id id))

(defun judge (goal turn n &aux (id (getf goal :id)))
  "Have a judge check GOAL's Nth turn TURN."
  (when (with-goal-lock
          (unless (ended id)
            (record (getf goal :session) +kind-judging+ id :n n :turn turn :judge (judge-name id n))
            t))
    (summon goal turn n)))

(defun word-prefix-p (word text)
  "Whether TEXT begins with WORD as a word of its own."
  (and (uiop:string-prefix-p word text)
       (or (= (length text) (length word)) (not (alphanumericp (char text (length word)))))))

(defun verdict (text)
  "(values MET DETAIL) of a judge's answer TEXT: MET when its first line says
MET, never when it says NOT MET or neither."
  (let* ((trim " *#>`_.:-")
         (lines (remove-if (lambda (line) (string= "" (nlk:trimmed line))) (nlk:lines (or text ""))))
         (head (string-trim trim (or (first lines) "")))
         (upper (string-upcase head)))
    (flet ((detail (word)
             (let ((after (string-trim trim (subseq head (length word)))))
               (let ((said (nlk:trimmed (format nil "~@[~a~%~]~{~a~^~%~}"
                                                (and (plusp (length after)) after) (rest lines)))))
                 (if (string= said "") "no evidence given" said)))))
      (cond ((word-prefix-p "NOT MET" upper) (values nil (detail "NOT MET")))
            ((word-prefix-p "MET" upper) (values t (detail "MET")))
            (t (values nil (format nil "the judge answered no MET or NOT MET line: ~a"
                                   (nlk:clip (nlk:one-line (or text "nothing")) 300))))))))

(defun judge-answer (child)
  "The judge CHILD's final answer, or NIL."
  (let ((payload (first (nlk:events :session-id child :kind "turn.assistant_message_completed"
                                    :order :newest :limit 1 :as :payloads))))
    (and payload (nlk:fact-message-content payload))))

(defun settle (goal judging &aux (n (check-number judging)) (turn (nlk:json-value judging :string "turn"))
                                 (child (judge-name (getf goal :id) n)))
  "The judge in flight, JUDGING its fact: its verdict once its turn ended, or
its question again when it never got it."
  (destructuring-bind (&optional kind payload)
      (nlk:events :session-id child :kind +turn-ends+ :order :newest :limit 1
                  :as :row :columns '("kind" "payload"))
    (cond ((null kind)
           (unless (or (nlk:active-turn-p child)
                       (and (nlk:session-exists-p child)
                            (plusp (nlk:events :session-id child :kind "turn.started" :as :count))))
             (summon goal turn n)))
          ((equal kind "turn.completed")
           (multiple-value-bind (met detail) (verdict (judge-answer child))
             (conclude goal turn n met detail)))
          (t
           (finish goal "failed"
                   (format nil "the judge's turn ~:[failed~@[: ~a~]~;was cancelled~*~]"
                           (equal kind "turn.cancelled")
                           (nlk:json-value (ignore-errors (nlk:decode-json payload)) :string "detail")))))))

;;; --- the worker -------------------------------------------------------------------

(defun check (goal turn n)
  "Check GOAL's Nth turn TURN: its command now, or a judge."
  (if (getf goal :check)
      (multiple-value-bind (met detail) (run-check goal)
        (conclude goal turn n met detail))
      (judge goal turn n)))

(defun advance (goal &aux (session (getf goal :session)) (id (getf goal :id)))
  "Take the one step GOAL owes, from the log as it stands."
  (let* ((checks (checks id))
         (spent (length checks))
         (judging (first (facts id +kind-judging+ :order :newest :limit 1))))
    (cond
      ((and judging (> (or (check-number judging) 0) spent))
       (settle goal judging))
      ((nlk:active-turn-p session) nil)
      (t
       (destructuring-bind (&optional turn kind payload) (last-turn session (getf goal :position))
         (cond ((null turn) (work goal 1))
               ;; checked, not met, turns left: the next turn, if it never landed
               ((find turn checks :key #'check-turn :test #'equal) (work goal (1+ spent)))
               ((equal kind "turn.cancelled") (finish goal "cancelled" "its turn was cancelled"))
               ((equal kind "turn.failed")
                (finish goal "failed" (format nil "its turn failed~@[: ~a~]"
                                              (nlk:json-value payload :string "detail"))))
               ;; a parked steer cut it short: the turn it opens answers for both
               ((and payload (eq t (nlk:json-value payload :boolean "steered"))) nil)
               (t (check goal turn (1+ spent)))))))))

(defun tick ()
  "One pass of the worker: every running goal's next step."
  (when (nlk:store-open-p)
    (bt2:with-lock-held (*tick-lock*)
      (dolist (goal (running-goals))
        (handler-case (advance goal)
          (error (condition) (warn "goal ~a: ~a" (getf goal :id) condition)))))))

(defun observe (op)
  "A turn ended somewhere: wake the worker. Publishing thread: a poke, no I/O."
  (when (member (nlk:frame-fact op) +turn-ends+ :test #'equal)
    (poke)))

(defun budget-hook (next turn)
  "Advice on NLE:TURN-BUDGET: a judge's turn runs under goal.judge_seconds and
goal.judge_calls; every other turn is NEXT's."
  (if (judge-p (getf turn :session-id))
      (list :seconds (setting :judge-seconds) :calls (setting :judge-calls))
      (funcall next turn)))

(defun naming-hook (next session-id text)
  "Advice on NLE:NAME-SESSION: a judge is never named -- a fork a check, and
its name a model call. Every other session is NEXT's."
  (unless (judge-p session-id)
    (funcall next session-id text)))

;;; --- setting and ending -----------------------------------------------------------

(defun blank-char-p (char) (member char '(#\Space #\Tab #\Newline #\Return)))

(defun option-at (name text)
  "Where NAME starts in TEXT as a word of its own, or NIL."
  (loop for start = 0 then (1+ at)
        for at = (search name text :start2 start)
        while at
        when (let ((end (+ at (length name))))
               (and (or (zerop at) (blank-char-p (char text (1- at))))
                    (or (= end (length text)) (blank-char-p (char text end)))))
          return at))

(defun parse-request (text)
  "/goal's argument TEXT as (values CONDITION CHECK TURNS): --turns N
anywhere, --check taking the rest of the line."
  (let ((text (nlk:trimmed (or text ""))) (check nil) (turns nil))
    (nlk:when-let (at (option-at "--turns" text))
      (let* ((from (or (position-if-not #'blank-char-p text :start (+ at 7)) (length text)))
             (to (or (position-if #'blank-char-p text :start from) (length text)))
             (word (subseq text from to)))
        (setf turns (or (ignore-errors (parse-integer word))
                        (fail "--turns takes a whole number, not ~s" word))
              text (nlk:trimmed (concatenate 'string (subseq text 0 at) " " (subseq text to))))))
    (nlk:when-let (at (option-at "--check" text))
      (setf check (nlk:trimmed (subseq text (+ at 7)))
            text (nlk:trimmed (subseq text 0 at)))
      (when (string= check "")
        (fail "--check takes the command that exits 0 once the goal holds")))
    (values text check turns)))

(defun begin (session condition check turns &key (by "operator") kickoff)
  "Set SESSION's goal, ending the one it had; KICKOFF admits its first turn
now. => the new goal's id."
  (let* ((check (let ((text (and check (nlk:trimmed (princ-to-string check)))))
                  (and text (plusp (length text)) text)))
         (condition (let ((text (nlk:trimmed (or condition ""))))
                      (cond ((plusp (length text)) text)
                            (check (format nil "`~a` exits 0" check)))))
         (turns (or turns (setting :turns)))
         (cwd (nlk:find-session-cwd session)))
    (unless condition
      (fail "a goal needs words: ~a" +usage+))
    (when (> (length condition) *condition-chars*)
      (fail "a goal is at most ~d characters and this one is ~d: put the detail in a file and name it"
            *condition-chars* (length condition)))
    (unless (and (integerp turns) (plusp turns))
      (fail "turns is a whole number of at least 1, not ~s" turns))
    (when (and check (not (and cwd (uiop:directory-exists-p (uiop:ensure-directory-pathname cwd)))))
      (fail "a --check runs in the session's folder, and this session names none that exists"))
    (prog1 (with-goal-lock
             (nlk:when-let (old (goal-of session))
               (finish old "replaced" "a new goal replaced it" :by by :announce nil))
             (let ((id (string-downcase (nlk:make-durable-id "goal"))))
               (apply #'record session +kind-set+ id :condition condition :turns turns :by by
                      (and check (list :check check)))
               (when kickoff
                 (work (goal-of session) 1))
               id))
      (poke))))

(defun halt (session reason by)
  "End SESSION's running goal, said to be stopped by BY for REASON."
  (let ((goal (or (goal-of session) (fail "this session has no goal running"))))
    (finish goal "stopped" reason :by by :announce nil)
    goal))

(defun eval-session ()
  "The session whose evaluation is calling, or a refusal."
  (or nlk:*scribe-session-id* (fail "a goal belongs to a session: call this from a session's eval")))

(define-verb start (condition &key check turns)
  "Set this session's goal: it keeps taking turns until CHECK, a shell command,
exits 0 in its folder, or without one until a judge answers MET; TURNS caps
the turns. Set mid-turn, the check runs when the turn ends. => the goal."
  (let ((session (eval-session)))
    (begin session (and condition (princ-to-string condition)) check turns :by "agent")
    (format nil "~a~%the check runs when this turn ends" (standing session))))

(define-verb status ()
  "This session's goal as it stands. => its text."
  (standing (eval-session)))

(define-verb stop (reason)
  "End this session's goal, REASON saying what it is missing. => what ended."
  (let ((reason (nlk:trimmed (princ-to-string (or reason "")))))
    (when (string= reason "")
      (fail "say why: (goal:stop \"what the operator has to give\")"))
    (let ((goal (halt (eval-session) reason "agent")))
      (format nil "goal stopped: ~a~%say in your answer what you need from the operator"
              (getf goal :condition)))))

;;; --- /goal -------------------------------------------------------------------------

(defun run-slash (args session-id)
  "/goal: CONDITION sets the goal and starts its first turn, nothing reads it,
stop ends it. => the answer's text."
  (unless (nlk:store-open-p)
    (fail "no store is open"))
  (unless session-id
    (fail "this shell has no session"))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((string= text "") (standing session-id))
          ((member (string-downcase text) '("stop" "off" "clear") :test #'string=)
           (let ((goal (halt session-id "the operator stopped it" "operator")))
             (format nil "goal stopped: ~a~%a turn already running finishes; stop it as you stop any turn"
                     (getf goal :condition))))
          (t
           (when (nlk:on-standby-p session-id)
             (nlk:materialize-standby-session session-id))
           (multiple-value-bind (condition check turns) (parse-request text)
             (begin session-id condition check turns :by "operator" :kickoff t)
             (format nil "~a~%turn 1 starts now" (standing session-id)))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell goal
  (:section ("goal")
    (:guide "A goal keeps a session taking turns until its check passes: a command that exits 0 in the session's folder, or a judge -- a fork of the session -- that answers MET. turns caps one goal's turns, check_seconds a command's run, judge_seconds and judge_calls a judge's turn")
    ("turns" :integer :default 10 :min 1
             :doc "turns one goal may take when /goal names none")
    ("check_seconds" :integer :default 600 :min 1
                     :doc "how long a check command runs before it is killed and counts as not met")
    ("judge_seconds" :integer :default 600 :min 1
                     :doc "seconds a judge's turn may run")
    ("judge_calls" :integer :default 40 :min 1
                   :doc "tool calls a judge's turn may make"))
  (:hook :frame (nle:observer #'observe "goal"))
  (:hook 'nle:turn-budget #'budget-hook)
  (:hook 'nle:name-session #'naming-hook)
  (:help :goal "goal: /goal CONDITION keeps the session taking turns until a check passes (a command exits 0, or a judge says MET); goal:start, goal:status, goal:stop" +primer+)
  (:command "goal" 'run-slash
            :catches 'goal-error
            :description "Keep working until a check passes: CONDITION sets the goal, --check COMMAND makes the check a command, nothing reads it, stop ends it"
            :argument-hint "CONDITION [--check COMMAND] [--turns N] | stop")
  (:start (lambda ()
            (setf *worker* (nlk:worker-start "goal" #'tick :wake 30 :prime t))
            (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*)))))))
