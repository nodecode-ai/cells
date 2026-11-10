;;;; cell-test.lisp --- the line, the words that move it, the file, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here runs over a real store and a real turn loop, the one
;;;; stub being the provider: that the line rides the first request of a turn
;;;; and no request of a mood that is off; that thanks, praise and a complaint
;;;; move the numbers the way the original's rules say and a quiet message
;;;; moves nothing; that the mood halves its distance to baseline in the
;;;; half-life and mentions a break of an hour or more; that the words follow
;;;; the numbers; that /mood answers, resets and turns the line off; that the
;;;; state survives a restart of the cell; that a damaged file starts over;
;;;; and that a value the section cannot read refuses with nothing installed.

(in-package #:nodecode.test)

(define-test-slice "mood" "MOOD-CELL-"
  :start nodecode-mood:start-cell
  :defaults (("half_life_hours" 6)))

(define-cell-lifecycle-tests "mood"
  (:hooks :round)
  (:help :mood)
  (:command "mood")
  (:running
   (is (= 6 (nodecode-mood::setting :half-life-hours)) "the half-life defaults to six hours")
   (is (eq t (nodecode-mood::setting :quiet)) "and the line keeps its quiet reminder")
   (is (numberp (getf nodecode-mood::*state* :valence)) "the state is read at start"))
  (:refused ("half_life_hours" 0) ("state_file" 42))
  (:idle mood:mood-error (mood:show) (mood:reset) (mood:set-enabled nil)))

;;; --- the fixture -------------------------------------------------------------

(defmacro with-mood ((&rest pairs) &body body)
  "BODY over a temp store with session s1 and the cell started on PAIRS,
stopped on unwind."
  `(with-temp-store ()
     (ensure-durable-session "s1")
     (with-cell-stop ((mood-start ,@pairs))
       ,@body)))

(defun mood-lines (context)
  "The mood lines CONTEXT, one compiled request, carries."
  (loop for message in (coerce (nle::compiled-turn-context-messages context) 'list)
        for text = (nle::content-text (gethash "content" message))
        when (and (stringp text) (search "[Your current inner state" text))
          collect text))

(defun mood-ask (session text)
  "One turn on SESSION saying TEXT; the mood lines its first request carried,
as one string."
  (let ((seen '()))
    (with-provider-stub (context
                         (push (mood-lines context) seen)
                         (wire-message "assistant" :content "ok"))
      (nle:run text :session (nle:intern-session session)
               :command-id (nlk:make-durable-id "command")))
    (format nil "~{~a~}" (car (last seen)))))

(defun mood-value (key)
  "KEY of the mood the cell is running on."
  (getf nodecode-mood::*state* key))

;;; --- the line ------------------------------------------------------------------

(deftest mood-cell-the-line-rides-the-first-request-of-a-turn ()
  (with-mood ()
    (let ((text (mood-ask "s1" "hello there")))
      (is (search "[Your current inner state" text) text)
      (is (search "rapport: friendly" text) "the rapport it starts at")
      (is (search "don't mention it unless asked" text) "with the original's reminder"))))

(deftest mood-cell-thanks-and-praise-move-it-up ()
  (with-mood ()
    (mood-ask "s1" "thanks, that was great work")
    (is (> (mood-value :valence) 0.1) "mood rose from baseline")
    (is (> (mood-value :warmth) 0.5) "and warmth with the thanks")
    (is (> (mood-value :rapport) 0.3) "and rapport, which only creeps up")
    (is (= 1 (mood-value :turns)) "one turn")
    (is (< (mood-value :valence) 0.4) "a single message cannot swing it far")))

(deftest mood-cell-a-complaint-makes-it-more-direct-and-mood-down ()
  (with-mood ()
    (mood-ask "s1" "this is broken and useless, ugh")
    (is (< (mood-value :valence) 0.1) "mood fell")
    (is (> (mood-value :directness) 0.6) "and it leans brief")
    (let ((before (mood-value :valence)))
      (mood-ask "s1" "what is the capital of Norway")
      (is (= before (mood-value :valence)) "a message with no signal moves nothing")
      (is (< (abs (- (mood-value :directness) 0.62)) 0.001) "and leaves the traits alone"))))

(deftest mood-cell-the-words-follow-the-numbers ()
  (with-mood ()
    (let ((state (copy-list nodecode-mood::*defaults*)))
      (setf (getf state :valence) 1.0 (getf state :energy) 1.0 (getf state :rapport) 1.0
            (getf state :warmth) 0.9 (getf state :playfulness) 0.9 (getf state :directness) 0.9)
      (let ((text (nodecode-mood::describe-state state)))
        (is (search "mood very good, lively" text) text)
        (is (search "rapport: close" text) "the rapport word")
        (is (search "they respond best to: warm, playful, brief and direct" text) "the leanings"))
      (let ((state (copy-list nodecode-mood::*defaults*)))
        (setf (getf state :valence) -1.0 (getf state :energy) 0.1 (getf state :playfulness) 0.1
              (getf state :directness) 0.1)
        (let ((text (nodecode-mood::describe-state state)))
          (is (search "mood upset, calm" text) text)
          (is (search "they respond best to: serious, detailed" text) "the other end"))))))

;;; --- the drift, and the gap -----------------------------------------------------

(deftest mood-cell-the-mood-halves-toward-baseline-in-the-half-life ()
  (with-mood ()
    (let ((state (copy-list nodecode-mood::*defaults*)))
      (setf (getf state :valence) 1.0 (getf state :energy) 1.0 (getf state :updated) 1000000)
      (nodecode-mood::decay state (+ 1000000 (* 6 3600)))
      (is (< (abs (- (getf state :valence) 0.55)) 0.001) "half the distance to baseline in six hours")
      (is (< (abs (- (getf state :energy) 0.75)) 0.001) "energy too"))))

(deftest mood-cell-a-break-of-an-hour-is-mentioned-on-the-next-turn ()
  (with-mood ()
    (let ((nodecode-mood::*now* 1000000))
      (is (null (search "since you last talked" (mood-ask "s1" "hello"))) "no gap on the first turn"))
    (let ((nodecode-mood::*now* (+ 1000000 (* 3 3600))))
      (is (search "3 hours since you last talked" (mood-ask "s1" "hello again")) "the gap, in hours"))
    (let ((nodecode-mood::*now* (+ 1000000 (* 3 3600) (* 48 3600))))
      (is (search "2 days since you last talked" (mood-ask "s1" "hello yet again")) "and in days"))))

(deftest mood-cell-the-mood-that-is-off-says-nothing-and-moves-nothing ()
  (with-mood ()
    (mood:set-enabled nil)
    (is (equal "" (mood-ask "s1" "thanks, great")) "no line")
    (is (= 0 (mood-value :turns)) "nothing counted")
    (is (= 0.1 (mood-value :valence)) "and the mood did not move")))

;;; --- the command ---------------------------------------------------------------

(deftest mood-cell-the-command-answers-with-the-numbers ()
  (with-mood ()
    (mood-ask "s1" "thanks")
    (let ((text (cell-entry "nodecode-mood" "mood" "" "s1")))
      (is (search "Mood is on." text) text)
      (is (search "valence 0." text) "the numbers are shown")
      (is (search "warmth 0.5" text) "the traits too")
      (is (search "1 turns" text) "and the count"))))

(deftest mood-cell-the-command-resets-and-turns-the-line-off ()
  (with-mood ()
    (mood-ask "s1" "this is broken, ugh")
    (is (search "Mood injection is now off."
                (cell-entry "nodecode-mood" "mood" "off" "s1")) "off says so")
    (is (search "Mood is off." (cell-entry "nodecode-mood" "mood" "" "s1")) "and it says so")
    (is (search "Mood injection is now on."
                (cell-entry "nodecode-mood" "mood" "on" "s1")) "and on again")
    (is (search "Mood reset to the starting state."
                (cell-entry "nodecode-mood" "mood" "reset" "s1")) "reset answers too")
    (is (= 0.1 (mood-value :valence)) "back to baseline")
    (is (= 0 (mood-value :turns)) "and no turns")
    (is (search "valence" (string-downcase (cell-entry "nodecode-mood" "mood" "nonsense" "s1"))) "an argument it does not know answers the report and the choices")))

;;; --- the file -------------------------------------------------------------------

(deftest mood-cell-the-state-survives-a-restart ()
  (with-temp-store ()
    (ensure-durable-session "s1")
    (let ((stop (mood-start)))
      (mood-ask "s1" "thanks")
      (funcall stop))
    (let ((stop (mood-start)))
      (unwind-protect
           (progn
             (is (= 1 (mood-value :turns)) "the turn is remembered")
             (is (> (mood-value :warmth) 0.5) "and the warmth it earned"))
        (funcall stop)))))

(deftest mood-cell-a-damaged-file-starts-from-the-defaults ()
  (with-temp-store ()
    (ensure-durable-session "s1")
    (let ((path (nlk:home "mood/state.json")))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :external-format :utf-8)
        (write-string "{not json at all" out)))
    (let ((stop (mood-start)))
      (unwind-protect
           (progn
             (is (= 0 (mood-value :turns)) "the defaults")
             (is (= 0.3 (mood-value :rapport)) "with the rapport it starts at"))
        (funcall stop)))))
