;;;; games-test.lisp --- the games' half: the runs, the frames, the board.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: a run is a run - Snake ends at a wall, Breakout at
;;;; its third miss, Pong at seven - and each answers a frame; a character
;;;; that is no move moves nothing; initials are three letters; and the board
;;;; keeps ten, best first, and answers a score's rank.

(in-package #:nodecode.test)

(deftest arcade-a-snake-run-ends-at-the-wall ()
  (let ((run (nodecode-arcade:start-game "snake")))
    (is (not (nodecode-arcade:run-over-p run)) "a new run is in play")
    (nodecode-arcade:advance-run run (make-string 20 :initial-element #\e))
    (is (nodecode-arcade:run-over-p run) "the snake leaves the grid and the run ends")
    (is (search "Game over" (nodecode-arcade:run-text run)) "and the frame says so")))

(deftest arcade-a-snake-run-has-a-pellet-on-its-grid ()
  (let* ((run (nodecode-arcade:start-game "snake" :width 16 :height 10))
         (state (nodecode-arcade::run-state run))
         (pellet (getf state :pellet)))
    (is (listp pellet) "the pellet is a cell")
    (is (< (first pellet) 16) "inside the grid across")
    (is (< (second pellet) 10) "and down")
    (is (search "@" (nodecode-arcade:run-text run)) "the frame draws the snake's head")))

(deftest arcade-a-character-that-is-no-move-moves-nothing ()
  (let ((run (nodecode-arcade:start-game "snake")))
    (nodecode-arcade:advance-run run "zzz")
    (is (not (nodecode-arcade:run-over-p run)) "the run is still in play")
    (is (= 0 (nodecode-arcade:run-score run)) "and nothing was scored")))

(deftest arcade-breakout-ends-at-the-third-miss ()
  (let ((run (nodecode-arcade:start-game "breakout")))
    (nodecode-arcade:advance-run run (make-string 60 :initial-element #\1))
    (is (nodecode-arcade:run-over-p run) "three misses end the run")
    (is (search "lives" (nodecode-arcade:run-text run)) "the frame counts lives")
    (is (>= (nodecode-arcade:run-score run) 0) "and the score is a score")))

(deftest arcade-pong-plays-to-seven ()
  (let* ((run (nodecode-arcade:start-game "pong"))
         (text (progn (nodecode-arcade:advance-run run (make-string 80 :initial-element #\5))
                      (nodecode-arcade:run-text run))))
    (is (nodecode-arcade:run-over-p run) "someone reaches seven")
    (is (search "machine" text) "the frame says where the match stands")))

(deftest arcade-initials-are-three-letters ()
  (is (string= "LEO" (nodecode-arcade:normalize-initials "leo!")) "letters upcased, punctuation dropped")
  (is (string= "ABC" (nodecode-arcade:normalize-initials "abcdef")) "three at most")
  (is (string= "???" (nodecode-arcade:normalize-initials "")) "nothing left is the arcade's ???"))

(deftest arcade-the-board-keeps-ten-best-first ()
  (with-board-file ((nlk:home "arcade/board-test.txt"))
    (loop for i from 1 to 12
          do (nodecode-arcade:submit-score "snake" "leo" (* i 10)))
    (let* ((entry (nodecode-arcade:board "snake"))
           (scores (getf entry :scores)))
      (is (= 10 (length scores)) "ten on the board")
      (is (= 120 (second (first scores))) "best first")
      (is (= 30 (second (car (last scores)))) "and the twelfth fell off the end")
      (is (= 0 (getf entry :plays)) "a score is not a play"))))

(deftest arcade-a-score-answers-its-rank ()
  (with-board-file ((nlk:home "arcade/rank-test.txt"))
    (is (= 1 (getf (nodecode-arcade:submit-score "pong" "leo" 500) :rank)) "the best is first")
    (is (= 2 (getf (nodecode-arcade:submit-score "pong" "leo" 100) :rank)) "the next is second")
    (is (getf (nodecode-arcade:submit-score "pong" "leo" 100) :made-board) "and a low score still makes the board")))

(deftest arcade-an-unknown-game-refuses ()
  (is (signals-error nodecode-arcade:arcade-error
        (nodecode-arcade:start-game "chess")) "the cabinet has three games")
  (is (signals-error nodecode-arcade:arcade-error
        (nodecode-arcade:board "chess")) "and a board for no other"))
