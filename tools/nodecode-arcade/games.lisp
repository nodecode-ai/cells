;;;; games.lisp --- the cabinet: Snake, Breakout and Pong, played a move at a time.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original draws all three on an HTML5 canvas in a dashboard tab: a
;;;; frame every animation tick, glow trails, particles, a chiptune synth, and
;;;; an attract mode with autopilots. Nodecode has no dashboard tab, so here a
;;;; game is a run the conversation advances one move at a time: each move
;;;; answers the whole frame as text, and the run's score goes to the same
;;;; leaderboard the original kept - top ten, arcade initials, plays counted.
;;;;
;;;; Nothing here reads a file, a clock or a network: the pellet, the ball and
;;;; the serve come from the run's own seeded generator, so a run is the same
;;;; run every time it is started in the same order.

(in-package #:nodecode-arcade)

;;; --- the cabinet ------------------------------------------------------------

(defparameter +games+
  '(("snake" "Neon Snake"
     "Eat the pellets, do not bite yourself or the wall. 10 points a pellet."
     "a move is one letter: n, s, e, w.")
    ("breakout" "Neon Breakout"
     "Smash the brick wall, three lives, a new wall every level. 20 points a brick, 250 a level."
     "a move is the paddle column, 1-9: the ball comes down within one column of it.")
    ("pong" "Neon Pong"
     "First to 7 against the machine. 100 points a point, 500 for the win."
     "a move is the zone you cover, 1-9: a serve inside one zone of it is returned."))
  "The cabinet, one row a game: id, label, blurb, and how a move is written.")

(defun game-ids ()
  "The cabinet's game ids."
  (mapcar #'first +games+))

(defun game-row (game)
  "The cabinet's row for GAME, or NIL."
  (find (string-downcase (princ-to-string game)) +games+ :key #'first :test #'string=))

(defun game-label (game)
  "GAME as the cabinet writes its name."
  (let ((row (game-row game)))
    (if row (second row) game)))

;;; --- the run -----------------------------------------------------------------

(defstruct (run (:constructor make-run (game &key score over-p state)))
  "One game in play: which game, the points so far, whether it ended, and the
game's own state, a plist the game's step mutates."
  (game "" :read-only t)
  (score 0)
  (over-p nil)
  (state nil))

(defvar *seed* 20261108
  "The cabinet's generator seed, advanced once a run. A run depends on the
order runs start, never on the clock, so a test's run is a test's run.")

(defun make-rng (seed)
  "A generator SEED starts: one cons, its car the state."
  (list (logand #xffffffff (if (zerop seed) 2463534242 seed))))

(defun rnd (rng n)
  "A number below N from RNG, its state advanced."
  (let ((x (car rng)))
    (setf x (logand #xffffffff (logxor x (ash x 13))))
    (setf x (logand #xffffffff (logxor x (ash x -17))))
    (setf x (logand #xffffffff (logxor x (ash x 5))))
    (setf (car rng) x)
    (mod (ash x -8) n)))

(defun dir-delta (ch)
  "CH as a step: DX, DY and T; NIL, NIL and NIL for a character that is no move."
  (case (char-downcase ch)
    (#\n (values 0 1 t))
    (#\s (values 0 -1 t))
    (#\e (values 1 0 t))
    (#\w (values -1 0 t))
    (t (values nil nil nil))))

(defun digit-of (ch)
  "CH as the column or zone it names, 1-9, or NIL."
  (when (digit-char-p ch)
    (let ((n (digit-char-p ch)))
      (and (<= 1 n 9) n))))

;;; --- Neon Snake ----------------------------------------------------------------

(defun place-pellet (state)
  "A cell of STATE's grid the snake is not on, as (X Y)."
  (let ((w (getf state :w)) (h (getf state :h))
        (body (getf state :body)) (rng (getf state :rng)))
    (loop repeat 200
          do (let ((x (rnd rng w)) (y (rnd rng h)))
               (unless (member (list x y) body :test #'equal)
                 (return-from place-pellet (list x y))))
          finally (return-from place-pellet (list 0 0)))))

(defun new-snake (w h)
  "A new Neon Snake run on a W by H grid."
  (let* ((state (list :w w :h h
                      :body (list (list 3 2) (list 2 2) (list 1 2))
                      :dir #\e :grew 0 :rng (make-rng (incf *seed*)))))
    (setf (getf state :pellet) (place-pellet state))
    (make-run "snake" :state state)))

(defun snake-step (run ch)
  "Advance RUN one move CH; the run ends on a wall or its own body."
  (multiple-value-bind (dx dy known) (dir-delta ch)
    (when known
      (let* ((state (run-state run))
             (body (getf state :body))
             (head (first body))
             (x (+ (first head) dx))
             (y (+ (second head) dy))
             (w (getf state :w)) (h (getf state :h))
             (pellet (getf state :pellet))
             (grow (equal (list x y) pellet))
             (body-of (if grow body (butlast body))))
        (when (or (< x 0) (< y 0) (>= x w) (>= y h)
                  (member (list x y) body-of :test #'equal))
          (setf (run-over-p run) t))
        (unless (run-over-p run)
          (setf (getf state :body) (cons (list x y) body-of))
          (setf (getf state :dir) (char-downcase ch))
          (when grow
            (incf (run-score run) 10)
            (incf (getf state :grew))
            (setf (getf state :pellet) (place-pellet state)))))))
  run)

(defun snake-frame (run)
  "RUN's grid as text: @ the head, # the body, * the pellet."
  (let* ((state (run-state run))
         (w (getf state :w)) (h (getf state :h))
         (body (getf state :body)) (pellet (getf state :pellet)))
    (with-output-to-string (s)
      (loop for y from (1- h) downto 0
            do (loop for x from 0 to (1- w)
                     do (write-char (cond ((equal (list x y) pellet) #\*)
                                          ((equal (list x y) (first body)) #\@)
                                          ((member (list x y) body :test #'equal) #\#)
                                          (t #\.))
                                    s)
                     finally (terpri s))))))

;;; --- Neon Breakout -------------------------------------------------------------

(defparameter *brick-rows* 3
  "Bricks a column of the wall starts a level with.")

(defun new-breakout (&key (cols 9) (lives 3))
  "A new Neon Breakout run: a full wall, LIVES lives, level 1."
  (make-run "breakout"
            :state (list :cols cols :lives lives :level 1 :paddle 5
                         :wall (make-list cols :initial-element *brick-rows*)
                         :rng (make-rng (incf *seed*)))))

(defun breakout-step (run ch)
  "Advance RUN one move CH: the paddle column. Within one column of the serve
is a hit and a brick; further is a life."
  (let ((column (digit-of ch)))
    (when column
      (let* ((state (run-state run))
             (cols (getf state :cols))
             (wall (getf state :wall))
             (serve (1+ (rnd (getf state :rng) cols))))
        (setf (getf state :paddle) (min column cols))
        (setf (getf state :serve) serve)
        (if (<= (abs (- (getf state :paddle) serve)) 1)
            (let ((target (if (plusp (nth (1- serve) wall))
                              (1- serve)
                              (or (position-if #'plusp wall) (1- serve)))))
              (decf (nth target wall))
              (incf (run-score run) 20)
              (when (every #'zerop wall)
                (incf (run-score run) 250)
                (incf (getf state :level))
                (setf (getf state :wall) (make-list cols :initial-element *brick-rows*))))
            (progn
              (decf (getf state :lives))
              (when (<= (getf state :lives) 0)
                (setf (run-over-p run) t)))))))
  run)

(defun breakout-frame (run)
  "RUN's wall, paddle and lives as text."
  (let* ((state (run-state run))
         (wall (getf state :wall))
         (paddle (getf state :paddle))
         (cols (getf state :cols)))
    (with-output-to-string (s)
      (loop for row from *brick-rows* downto 1
            do (loop for col from 1 to cols
                     do (write-char (if (>= (nth (1- col) wall) row) #\# #\.) s)
                     finally (terpri s)))
      (loop for col from 1 to cols
            do (write-char (if (= col paddle) #\= #\-) s)
            finally (terpri s))
      (loop for col from 1 to cols
            do (write-char (digit-char (mod col 10)) s)
            finally (terpri s))
      (format s "lives ~d, level ~d~%" (getf state :lives) (getf state :level))
      (when (getf state :serve)
        (format s "the ball came down on column ~d~%" (getf state :serve))))))

;;; --- Neon Pong ------------------------------------------------------------------

(defun new-pong (&key (target 7) (zones 9))
  "A new Neon Pong run: first to TARGET, a TARGET of ZONES to cover."
  (make-run "pong"
            :state (list :you 0 :machine 0 :target target :zones zones
                         :zone nil :serve nil :rng (make-rng (incf *seed*)))))

(defun pong-step (run ch)
  "Advance RUN one move CH: the zone covered. A serve inside one zone of it
is returned; the run ends at TARGET points, and a win pays 500."
  (let ((zone (digit-of ch)))
    (when zone
      (let* ((state (run-state run))
             (zones (getf state :zones))
             (target (getf state :target))
             (serve (1+ (rnd (getf state :rng) zones))))
        (setf (getf state :zone) (min zone zones))
        (setf (getf state :serve) serve)
        (if (<= (abs (- (getf state :zone) serve)) 1)
            (progn (incf (getf state :you)) (incf (run-score run) 100))
            (incf (getf state :machine)))
        (cond ((>= (getf state :you) target)
               (incf (run-score run) 500)
               (setf (run-over-p run) t))
              ((>= (getf state :machine) target)
               (setf (run-over-p run) t))))))
  run)

(defun pong-frame (run)
  "RUN's score, the target and the last serve as text."
  (let* ((state (run-state run))
         (zones (getf state :zones)))
    (with-output-to-string (s)
      (format s "you ~d - ~d machine (first to ~d)~%"
              (getf state :you) (getf state :machine) (getf state :target))
      (loop for zone from 1 to zones
            do (write-char (if (eql zone (getf state :zone)) #\^ #\-) s)
            finally (terpri s))
      (loop for zone from 1 to zones
            do (write-char (digit-char (mod zone 10)) s)
            finally (terpri s))
      (when (getf state :serve)
        (format s "the serve came down on zone ~d~%" (getf state :serve))))))

;;; --- one run, any game ---------------------------------------------------------------

(defun start-game (game &key (width 16) (height 10))
  "A new run of GAME: snake on WIDTH by HEIGHT, breakout with its wall, pong
with its target. => the run, or an ARCADE-ERROR for a game the cabinet has not."
  (let ((row (game-row game)))
    (unless row
      (fail "~s is no arcade game; the cabinet has ~{~a~^, ~}" game (game-ids)))
    (cond ((string= (first row) "snake") (new-snake width height))
          ((string= (first row) "breakout") (new-breakout))
          (t (new-pong)))))

(defun advance-run (run moves)
  "Advance RUN by every character of MOVES, stopping when the run ends."
  (loop for ch across (or moves "")
        until (run-over-p run)
        do (cond ((string= (run-game run) "snake") (snake-step run ch))
                 ((string= (run-game run) "breakout") (breakout-step run ch))
                 (t (pong-step run ch))))
  run)

(defun run-frame (run)
  "RUN's frame as text, its header first."
  (let ((row (game-row (run-game run))))
    (format nil "~a  score ~d~%~a"
            (if row (second row) (run-game run))
            (run-score run)
            (cond ((string= (run-game run) "snake") (snake-frame run))
                  ((string= (run-game run) "breakout") (breakout-frame run))
                  (t (pong-frame run))))))

(defun run-text (run &key (limit 1200) (recorded nil))
  "RUN's frame and, when it ended, its last line; RECORDED is the leaderboard
answer for the score, when it has one. At most LIMIT characters."
  (let* ((row (game-row (run-game run)))
         (text (with-output-to-string (s)
                 (write-string (run-frame run) s)
                 (when (run-over-p run)
                   (format s "Game over: ~d points at ~a.~%~@[~a~]~%~a"
                           (run-score run)
                           (if row (second row) (run-game run))
                           recorded
                           (fourth (or row (list nil nil nil "Start another with arcade_play."))))))))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) " ... (cut)")
        text)))

(defvar *run* nil
  "The cabinet's one live run, or NIL. One machine, one game at a time, the
way a cabinet stands in a room: whoever plays next plays on it.")

(defun reset-cabinet ()
  "Clear the live run, as a test wants it."
  (setf *run* nil))
