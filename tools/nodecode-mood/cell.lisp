;;;; cell.lisp --- the mood, in its own file under the home.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps a mood (upset to happy, calm to lively), a rapport
;;;; level and three slow traits (warmth, playfulness, directness); scores
;;;; each user message against a handful of keyword rules; lets the mood
;;;; drift back toward baseline over a few hours; and appends one short line
;;;; to that message through its PRE_LLM_CALL hook. No model call is made:
;;;; the whole thing costs a few milliseconds and a few dozen tokens.
;;;;
;;;; Nodecode's seam is the :ROUND point, which the first round of every turn
;;;; passes through before its request is built. There the turn's own opening
;;;; words are scored, the state is written to its file, and the line is
;;;; recorded into that very request with NLK:RECORD-TURN-CONTEXT-PROMPT --
;;;; the same place a message would carry it, without touching a system
;;;; prompt that a cache keys on.

(in-package #:nodecode-mood)

;;; --- the numbers the original kept -------------------------------------------

(defparameter *baseline* '(:valence 0.1 :energy 0.5)
  "Where the mood drifts back to: slightly good, steady.")

(defparameter *max-shift* 0.35
  "The most one message moves mood, so a burst of praise cannot swing it.")

(defparameter *trait-min* 0.1
  "Traits never leave this band, however long a conversation runs.")

(defparameter *trait-max* 0.9)

(defparameter *trait-step* 0.02
  "How far one signal moves a trait.")

(defparameter *defaults*
  '(:enabled t
    :valence 0.1 :energy 0.5 :rapport 0.3
    :warmth 0.5 :playfulness 0.4 :directness 0.6
    :turns 0 :last-seen 0 :updated 0)
  "A fresh mood: the original's DEFAULT_STATE, its three traits kept flat
instead of nested. :ENABLED is /mood off in the state, as it was there; a
section's own \"enabled\": false vetoes the cell before it ever starts.")

(defparameter *signals*
  (flet ((rule (pattern &rest effects)
           (cons (cl-ppcre:create-scanner pattern :case-insensitive-mode t) effects)))
    (list
     (rule "\\b(thanks|thank you|thx|ty|cheers|appreciate)\\b"
           :valence 0.1 :rapport 0.02 :warmth 1)
     (rule "\\b(great|awesome|perfect|nice|love it|amazing|brilliant|good job|well done)\\b"
           :valence 0.12 :energy 0.1 :rapport 0.01)
     (rule "\\b(lol|lmao|haha+|hehe)\\b|😂|🤣|😄|😆"
           :valence 0.08 :energy 0.15 :playfulness 1)
     (rule "\\b(wrong|broken|doesn'?t work|not working|useless|stupid|annoying|ugh|wtf)\\b"
           :valence -0.35 :energy 0.1 :directness 1)
     (rule "\\b(too long|shorter|tl;?dr|get to the point|just tell me|be brief)\\b"
           :valence -0.05 :directness 2)
     (rule "\\b(more detail|explain more|elaborate|go deeper|in depth)\\b"
           :directness -1)
     (rule "\\b(stop joking|be serious|not funny)\\b"
           :playfulness -2)
     (rule "\\b(tired|stressed|sad|exhausted|rough day|worried)\\b"
           :energy -0.15 :warmth 1)))
  "The original's SIGNALS: (PATTERN . EFFECTS), a trait's effect in steps of
*TRAIT-STEP* and mood's in its own units. Word boundaries keep \"no\" out of
\"know\", and one message can match several.")

(defparameter *mood-words*
  '((-0.5 . "upset") (-0.15 . "a bit low") (0.15 . "even") (0.5 . "good") (1.0 . "very good")))

(defparameter *energy-words*
  '((0.3 . "calm") (0.7 . "steady") (1.0 . "lively")))

(defparameter *rapport-words*
  '((0.25 . "still getting to know them") (0.5 . "friendly")
    (0.75 . "comfortable") (1.0 . "close")))

(defparameter *framing*
  "[Your current inner state (let it colour your tone quietly; don't mention it unless asked): ~a.]"
  "The line the model reads, the original's wording: it is told to let the
state colour its tone and not to talk about it. QUIET off drops the last half.")

(defparameter *open-framing*
  "[Your current inner state (let it colour your tone quietly): ~a.]")

;;; --- the clock, and the file ------------------------------------------------

(defvar *now* nil
  "A universal time a test pins the clock to, or NIL for the clock's own.")

(defun now ()
  "Now, in universal time, unless a test pinned it."
  (or *now* (get-universal-time)))

(defun half-life ()
  "Hours for the mood to halve its distance to baseline."
  (float (setting :half-life-hours) 1.0))

(defun state-path ()
  "Where the mood lives: the section's state_file, else mood/state.json under
the home, so one profile keeps one mood."
  (let ((given (setting :state-file)))
    (if (and given (plusp (length given)))
        (nlk:expand-home given)
        (nlk:home "mood/state.json"))))

(defun load-state ()
  "The state the file holds over the defaults; the defaults when it is absent,
unreadable, or not the object it should be."
  (let ((state (copy-list *defaults*))
        (path (state-path)))
    (let ((object (and (uiop:file-exists-p path)
                       (ignore-errors (nlk:decode-json (uiop:read-file-string path))))))
      (when (hash-table-p object)
        (dolist (pair '(("valence" . :valence) ("energy" . :energy) ("rapport" . :rapport)))
          (let ((value (nlk:json-value object :number (car pair))))
            (when value (setf (getf state (cdr pair)) (float value 1.0)))))
        (dolist (pair '(("warmth" . :warmth) ("playfulness" . :playfulness)
                        ("directness" . :directness)))
          (let ((value (nlk:json-value object :number "traits" (car pair))))
            (when value (setf (getf state (cdr pair)) (float value 1.0)))))
        (dolist (pair '(("turns" . :turns) ("last_seen" . :last-seen) ("updated" . :updated)))
          (let ((value (nlk:json-value object :integer (car pair))))
            (when value (setf (getf state (cdr pair)) value))))
        (multiple-value-bind (value present) (nlk:json-value object :boolean "enabled")
          (when present (setf (getf state :enabled) value)))))
    state))

(defun save-state (state)
  "Write STATE where a restart reads it, atomically: the same members, the
traits nested as the original nested them, so the file reads as it did there."
  (ignore-errors
    (nlk:write-file-atomically
     (state-path)
     (nlk:encode-json-object
      (nlk:json-object
       "enabled" (if (getf state :enabled) t nil)
       "valence" (float (getf state :valence) 1.0)
       "energy" (float (getf state :energy) 1.0)
       "rapport" (float (getf state :rapport) 1.0)
       "traits" (nlk:json-object "warmth" (float (getf state :warmth) 1.0)
                                  "playfulness" (float (getf state :playfulness) 1.0)
                                  "directness" (float (getf state :directness) 1.0))
       "turns" (getf state :turns)
       "last_seen" (getf state :last-seen)
       "updated" (getf state :updated)))
     :mode :private :directory-mode #o700))
  state)

;;; --- the state as the model reads it ----------------------------------------

(defun clamp (value low high)
  (max low (min high value)))

(defun word (value scale)
  "VALUE as the first word of SCALE it does not exceed, else the last."
  (or (loop for (limit . text) in scale when (<= value limit) return text)
      (cdr (car (last scale)))))

(defun gap-text (seconds)
  "How long it has been since the last turn, or NIL under the hour."
  (when (and seconds (>= seconds 3600))
    (let ((hours (floor seconds 3600)))
      (if (< hours 24)
          (format nil "~d hours since you last talked" hours)
          (format nil "~d days since you last talked" (floor hours 24))))))

(defun describe-state (state &key gap)
  "The state as one clause: mood, energy, rapport, what this person responds
best to, and -- only after a break -- how long it has been."
  (let ((leanings '())
        (parts '()))
    (when (>= (getf state :warmth) 0.6)
      (push "warm" leanings))
    (cond ((>= (getf state :playfulness) 0.6) (push "playful" leanings))
          ((<= (getf state :playfulness) 0.25) (push "serious" leanings)))
    (cond ((>= (getf state :directness) 0.7) (push "brief and direct" leanings))
          ((<= (getf state :directness) 0.35) (push "detailed" leanings)))
    (push (format nil "mood ~a, ~a"
                  (word (getf state :valence) *mood-words*)
                  (word (getf state :energy) *energy-words*))
          parts)
    (push (format nil "rapport: ~a" (word (getf state :rapport) *rapport-words*)) parts)
    (when leanings
      (push (format nil "they respond best to: ~{~a~^, ~}" (nreverse leanings)) parts))
    (let ((since (and gap (gap-text gap))))
      (when since (push since parts)))
    (format nil "~{~a~^; ~}" (nreverse parts))))

(defun line (state &key gap)
  "The one line a request carries: the framing, and the state."
  (format nil (if (setting :quiet) *framing* *open-framing*) (describe-state state :gap gap)))

;;; --- what a message does to it ----------------------------------------------

(defun nudge (state key amount)
  "Move KEY of STATE by AMOUNT: a trait by steps, mood and rapport directly,
each kept inside its own band."
  (case key
    ((:warmth :playfulness :directness)
     (setf (getf state key)
           (clamp (+ (getf state key) (* amount *trait-step*)) *trait-min* *trait-max*)))
    ((:valence) (setf (getf state :valence) (clamp (+ (getf state :valence) amount) -1.0 1.0)))
    ((:energy) (setf (getf state :energy) (clamp (+ (getf state :energy) amount) 0.0 1.0)))
    ((:rapport) (setf (getf state :rapport) (clamp (+ (getf state :rapport) amount) 0.0 1.0))))
  state)

(defun score (state text)
  "Move STATE by every rule TEXT matches, cap what this one turn did to mood,
and count that the person showed up at all."
  (when (and (stringp text) (plusp (length text)))
    (let ((before (list :valence (getf state :valence) :energy (getf state :energy))))
      (dolist (signal *signals*)
        (when (cl-ppcre:scan (car signal) text)
          (loop for (key amount) on (cdr signal) by #'cddr
                do (nudge state key amount))))
      (loop for (key start) on before by #'cddr
            do (setf (getf state key)
                     (clamp (getf state key) (- start *max-shift*) (+ start *max-shift*))))
      (nudge state :rapport 0.003)))
  state)

(defun decay (state now)
  "Pull STATE back toward baseline by how long it has been since NOW, halving
the distance every HALF-LIFE hours."
  (let ((updated (getf state :updated)))
    (when (and updated (plusp updated))
      (let ((keep (expt 0.5 (/ (max 0.0 (/ (float (- now updated) 1.0) 3600.0)) (half-life)))))
        (loop for (key base) on *baseline* by #'cddr
              do (setf (getf state key) (+ base (* (- (getf state key) base) keep)))))))
  state)

;;; --- the :ROUND point --------------------------------------------------------

(defvar *state* nil
  "The mood as the cell runs on it, a plist: the file's members over
*DEFAULTS*, held under the cell's lock and written on every turn.")

(defun turn-key (turn)
  "TURN's id, whether the point handed over the turn or its id."
  (if (stringp turn) turn (nlk:durable-turn-turn-id turn)))

(defun turn-input (turn)
  "The words TURN was admitted with, or NIL."
  (ignore-errors
    (nlk:json-value (nlk:events :turn-id (turn-key turn) :kind nlk::+kind-turn-started+
                                :as :payload)
                    :string "input")))

(defun before-round (op)
  "The :ROUND point: on a turn's first round, score the words it was admitted
with, save the mood, and record the line into the request about to be built.
A mood that is off says nothing and moves nothing."
  (let ((turn (getf op :turn)))
    (when turn
      (with-mood-lock
        (let ((state (or *state* (setf *state* (load-state)))))
          (when (getf state :enabled)
            (let* ((at (now))
                   (gap (and (plusp (getf state :last-seen))
                             (- at (getf state :last-seen))))
                   (first (eql 0 (or (getf op :round) 0))))
              (decay state at)
              (when first
                (score state (turn-input turn))
                (incf (getf state :turns)))
              (setf (getf state :last-seen) at
                    (getf state :updated) at)
              (save-state state)
              (nlk:record-turn-context-prompt
               turn :text (line state :gap (and first gap))))))))))

;;; --- what the operator asks ---------------------------------------------------

(defun report ()
  "One answer: whether the line is on, the line itself, and the numbers."
  (format nil "Mood is ~a.~%~a~%valence ~,2f, energy ~,2f, rapport ~,2f | warmth ~,2f, playfulness ~,2f, directness ~,2f | ~d turns"
          (if (getf *state* :enabled) "on" "off")
          (describe-state *state*)
          (getf *state* :valence) (getf *state* :energy) (getf *state* :rapport)
          (getf *state* :warmth) (getf *state* :playfulness) (getf *state* :directness)
          (getf *state* :turns)))

(define-verb show ()
  "The mood now: the line a request carries, and the numbers behind it."
  (with-mood-lock
    (setf *state* (decay *state* (now)))
    (report)))

(define-verb reset ()
  "Back to the starting state; whether the line is on stays as it was."
  (with-mood-lock
    (let ((enabled (getf *state* :enabled)))
      (setf *state* (copy-list *defaults*))
      (setf (getf *state* :enabled) enabled)
      (save-state *state*)
      "Mood reset to the starting state.")))

(define-verb set-enabled (on)
  "Turn the line on or off: OFF keeps the state and stops the line and the
scoring until it is on again."
  (with-mood-lock
    (setf (getf *state* :enabled) (and on t))
    (save-state *state*)
    (format nil "Mood injection is now ~a." (if on "on" "off"))))

(defun run-slash (args session)
  "/mood: the state and its numbers; reset, on or off as the argument."
  (declare (ignore session))
  (let ((arg (string-downcase (nlk:trimmed (or args "")))))
    (cond ((string= arg "") (show))
          ((string= arg "reset") (reset))
          ((string= arg "off") (set-enabled nil))
          ((string= arg "on") (set-enabled t))
          (t (format nil "~a~%~%mood: reset, on or off." (show))))))

;;; --- the manual and the declaration ------------------------------------------

(defparameter +primer+
  "mood: a small inner state the model colours its tone with - mood, energy, rapport and three slow traits, scored from what the person writes, drifting back toward baseline over hours, and read as one line at the start of every turn. (mood:show), (mood:reset), (mood:set-enabled t) or nil, and /mood, /mood reset, /mood on, /mood off. It needs no key and no model call; the state is a JSON file under the home."
  "The manual entry (help :mood) answers.")

(nle:define-cell mood
  (:section ("mood")
    (:guide "nothing is required; half_life_hours is how fast the mood drifts back to baseline, state_file where it is kept, and quiet drops the line's reminder not to mention it")
    ("half_life_hours" :integer :default 6 :min 1
     :doc "Hours for the mood to halve its distance to baseline; the original's 6.")
    ("state_file" :path
     :doc "Where the mood is kept: <home>/mood/state.json unless named here, one file per profile.")
    ("quiet" :boolean :default t
     :doc "Whether the line asks the model to let the state colour its tone and not mention it; off says only the first half."))
  (:hook :round (nle:observer #'before-round "mood"))
  (:command "mood" #'run-slash
            :catches 'mood-error
            :argument-hint "[reset|on|off]"
            :description "The model's mood and rapport now, or reset it, or turn the line off and on")
  (:help :mood
         "mood: the model's small evolving inner state and rapport, one line a turn; /mood"
         +primer+)
  (:start (lambda ()
            (setf *state* (load-state))
            (nle:on-stop (lambda () (setf *state* nil))))))
