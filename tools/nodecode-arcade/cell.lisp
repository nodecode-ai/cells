;;;; cell.lisp --- the tools, /arcade, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of leoresendedop-sys's hermes-arcade, a Hermes Agent
;;;; plugin that put a neon arcade cabinet in the dashboard: three canvas games,
;;;; persistent top-ten leaderboards with arcade initials, and three tools so
;;;; the agent could answer questions about the scores. Nodecode has no
;;;; dashboard tab, so the cabinet lives in the conversation instead: a run is
;;;; advanced a move at a time by arcade_play or /arcade, and the three tool
;;;; answers the original had - the games, the board, the stats - are here
;;;; unchanged in what they say.

(in-package #:nodecode-arcade)

;;; --- the tools --------------------------------------------------------------------

(nlk:define-contract arcade-games-arguments
  (:doc "The cabinet's games, what each pays and how a move is written. Call
it when a user asks what games there are, what the arcade is, or how to play.")
  ("game" :string :default ""
          :doc "one game to describe: snake, breakout or pong; omit for all three"))

(nlk:define-contract arcade-play-arguments
  (:doc "Play the cabinet's Snake, Breakout or Pong in the conversation: give
the game and a move for each turn, and the answer is the whole frame, the score
and, when the run ends, where the score was filed. Start a game with no moves
to see the board. Call it when the user says play a game, or asks to keep
playing, or wants a break from the work.")
  ("game" :string :required t :options '("snake" "breakout" "pong")
          :doc "the game to play: snake, breakout or pong")
  ("moves" :string :default ""
           :doc "one character a turn: snake takes n, s, e or w; breakout and pong take the column or zone you cover, 1-9")
  ("initials" :string :default ""
              :doc "the 1-3 letters to file the score under, as on an arcade cabinet; defaults to the section's initials"))

(nlk:define-contract arcade-scores-arguments
  (:doc "The cabinet's leaderboards: the top ten a game with initials and
points, and how many times each game has been played. Call it whenever a user
asks about high scores, records, or how well they have played.")
  ("game" :string :default ""
          :doc "one game to read: snake, breakout or pong; omit for all three"))

(nlk:define-contract arcade-stats-arguments
  (:doc "Aggregate cabinet stats: total games played, plays a game, and each
game's champion. Give initials to list every board entry filed under them.")
  ("initials" :string :default ""
              :doc "1-3 arcade initials to collect across every game"))

(defun games-text (rows)
  "ROWS of the cabinet as text a user reads."
  (with-output-to-string (s)
    (dolist (row rows)
      (format s "~a (~a) - ~a~%  a move: ~a~%" (second row) (first row) (third row) (fourth row)))))

(defun scores-text (entry)
  "One game's board ENTRY as text."
  (with-output-to-string (s)
    (format s "~a: ~d play~:p~%" (getf entry :label) (getf entry :plays))
    (let ((scores (getf entry :scores)))
      (if (null scores)
          (write-line "  no scores yet" s)
          (loop for score in scores
                for i from 1
                do (format s "  ~d. ~a ~d~%" i (first score) (second score)))))))

(defun cabinet-text (game)
  "The cabinet's games, GAME alone, or the reason GAME is none of them."
  (let ((game (string-downcase (nlk:trimmed game))))
    (cond ((zerop (length game))
           (format nil "Hermes Arcade, three games in the conversation:~%~a~%Play with arcade_play: the game and a move a turn. The board is arcade_top_scores."
                   (games-text +games+)))
          ((game-row game)
           (games-text (list (game-row game))))
          (t (format nil "arcade: ~s is no arcade game; the cabinet has ~{~a~^, ~}."
                     game (game-ids))))))

(defun run-games (arguments)
  "The arcade_games tool: the cabinet, or one game of it."
  (cabinet-text (arcade-games-arguments-game arguments)))

(defun remember-run (run)
  "Put RUN on the cabinet."
  (setf *run* run))

(defun play-moves (game moves &key initials)
  "Play GAME by MOVES on the cabinet, starting a run when there is none or the
last one ended. => the answer's text."
  (let* ((game (string-downcase (nlk:trimmed game)))
         (row (game-row game)))
    (unless row
      (return-from play-moves
        (format nil "arcade: ~s is no arcade game; the cabinet has ~{~a~^, ~}."
                game (game-ids))))
    (let ((run (if (and *run* (string-equal (run-game *run*) game)
                        (not (run-over-p *run*)))
                   *run*
                   (let ((started (start-game game
                                               :width (setting :snake-width)
                                               :height (setting :snake-height))))
                     (record-play game)
                     (remember-run started)
                     started)))
          (who (or (and initials (plusp (length initials)) (normalize-initials initials))
                   (normalize-initials (setting :initials)))))
      (advance-run run (or moves ""))
      (let ((filed nil))
        (when (run-over-p run)
          (let ((answer (submit-score game who (run-score run)
                                      :keep (setting :keep-scores))))
            (setf filed (format nil "Filed on the board: ~a ~d~@[, rank ~d~]~@[, best ~d~]."
                                 who (run-score run)
                                 (getf answer :rank) (getf answer :best)))))
        (run-text run :limit (setting :answer-chars) :recorded filed)))))

(defun run-play (arguments)
  "The arcade_play tool."
  (play-moves (arcade-play-arguments-game arguments)
              (arcade-play-arguments-moves arguments)
              :initials (arcade-play-arguments-initials arguments)))

(defun top-scores-text (game)
  "The board of GAME, or all three when GAME is empty."
  (let ((game (string-downcase (nlk:trimmed game))))
    (cond ((zerop (length game))
           (with-output-to-string (s)
             (format s "Hermes Arcade leaderboards~2%")
             (dolist (id (game-ids))
               (write-string (scores-text (board id :keep (setting :keep-scores))) s)
               (terpri s))))
          ((game-row game)
           (scores-text (board game :keep (setting :keep-scores))))
          (t (format nil "arcade: ~s is no arcade game; the cabinet has ~{~a~^, ~}."
                     game (game-ids))))))

(defun run-top-scores (arguments)
  "The arcade_top_scores tool: one board or all three."
  (top-scores-text (arcade-scores-arguments-game arguments)))

(defun run-stats (arguments)
  "The arcade_player_stats tool: the cabinet's totals and, for INITIALS, their scores."
  (let* ((initials (nlk:trimmed (arcade-stats-arguments-initials arguments)))
         (stats (summary :initials (and (plusp (length initials)) initials)
                         :keep (setting :keep-scores))))
    (with-output-to-string (s)
      (format s "Hermes Arcade: ~d play~:p in all.~%" (getf stats :total-plays))
      (dolist (row (getf stats :games))
        (let ((champion (getf row :champion)))
          (format s "~a: ~d play~:p~@[, champion ~a ~d~].~%"
                  (getf row :label) (getf row :plays)
                  (and champion (first champion)) (and champion (second champion)))))
      (when (getf stats :player)
        (let ((mine (getf stats :player-scores)))
          (format s "~a on the boards:~%" (getf stats :player))
          (if (null mine)
              (write-line "  nothing filed yet" s)
              (dolist (entry mine)
                (format s "  ~a:" (car entry))
                (dolist (hit (cdr entry))
                  (format s " #~d ~d" (getf hit :rank) (getf hit :score)))
                (terpri s))))))))

;;; --- /arcade ----------------------------------------------------------------------

(defun words-of (text)
  "TEXT split on whitespace."
  (let ((words '())
        (start nil))
    (loop for i from 0 to (length text)
          for ch = (and (< i (length text)) (char text i))
          do (if (and ch (not (member ch '(#\Space #\Tab #\Newline #\Return))))
                 (unless start (setf start i))
                 (when start (push (subseq text start i) words) (setf start nil))))
    (nreverse words)))

(defun run-slash (args session-id)
  "The /arcade command: the cabinet, a board, or a move.
/arcade -> the cabinet and the boards; /arcade snake nneee -> play; /arcade
snake -> start Neon Snake and show its frame."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (words-of text))
         (head (and words (string-downcase (first words)))))
    (cond ((or (null words) (member head '("board" "boards" "scores") :test #'string=))
           (top-scores-text ""))
          ((member head '("games" "cabinet" "help") :test #'string=)
           (run-games (nlk:make-json-object "game" "")))
          ((game-row head)
           (play-moves head
                       (if (cdr words) (format nil "~{~a~}" (cdr words)) "")))
          (t (format nil "arcade: ~s is no arcade game; the cabinet has ~{~a~^, ~}.~%/arcade <game> <moves>: snake takes n, s, e, w; breakout and pong take 1-9."
                     head (game-ids))))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual+
  "arcade: the cabinet in the conversation - Neon Snake, Neon Breakout and Neon
Pong - and the leaderboards the original plugin kept, with scores
  A run is advanced a move at a time. /arcade <game> <moves> plays it; the tool
arcade_play does the same for a model. A move is one character: snake takes n, s,
e or w (10 points a pellet, the run ends on a wall or its own body); breakout takes
the paddle column 1-9 (20 points a brick, 250 a level, three lives, the run ends at
the third miss); pong takes the zone you cover, 1-9 (first to 7, 100 points a
point, 500 for the win). A serve within one column or zone of you is a hit.
  Each move answers the whole frame as text, the score, and - when the run ends -
the rank the score reached. A score is filed under the section's initials, three
letters at most, as an arcade cabinet asks. arcade_games lists the cabinet,
arcade_top_scores reads the top ten a game with its play count, and
arcade_player_stats answers totals, each game's champion, and one player's
entries. The section arcade bounds it: initials (YOU), keep_scores (ten a game),
snake_width and snake_height (16 by 10), answer_chars (1,200 characters of one
frame). The board is one file under the Nodecode home; nothing leaves the machine."
  "What (help :arcade) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell arcade
  (:section ("arcade")
    (:guide "nothing here is required: the cabinet plays on a 16x10 grid, keeps ten scores a game under the initials YOU, and answers at most 1,200 characters of a frame")
    ("initials" :string :default "YOU"
                :doc "the 1-3 letters your scores are filed under, as on an arcade cabinet")
    ("keep_scores" :integer :default 10 :min 1
                   :doc "how many scores a game's board keeps")
    ("snake_width" :integer :default 16 :min 6
                   :doc "Neon Snake's grid, in cells across")
    ("snake_height" :integer :default 10 :min 5
                    :doc "Neon Snake's grid, in cells down")
    ("answer_chars" :integer :default 1200 :min 100
                    :doc "how much of one frame an answer carries"))
  (:tool "arcade_games" (nlk:contract 'arcade-games-arguments) 'run-games)
  (:tool "arcade_play" (nlk:contract 'arcade-play-arguments) 'run-play)
  (:tool "arcade_top_scores" (nlk:contract 'arcade-scores-arguments) 'run-top-scores)
  (:tool "arcade_player_stats" (nlk:contract 'arcade-stats-arguments) 'run-stats)
  (:command "arcade" 'run-slash
            :description "Play Snake, Breakout or Pong, and read the leaderboards"
            :argument-hint "GAME [MOVES]")
  (:help :arcade "arcade: play Neon Snake, Neon Breakout or Neon Pong in the conversation, and read the cabinet's leaderboards - /arcade" +manual+))
