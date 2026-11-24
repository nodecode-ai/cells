;;;; cell-test.lisp --- the cell's half: the tools, the command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs four tools, /arcade and
;;;; (help :arcade) and takes them all back on stop; a play answers the frame
;;;; and, when the run ends, files the score it reached; the three tool
;;;; answers the original had - the games, the board, the stats - read what a
;;;; play filed; /arcade plays for the operator; a play with no game is
;;;; refused at the boundary; and the section's initials name the score.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "arcade"
  (:config)
  (:help :arcade)
  (:command "arcade")
  (:running
   (is (= 10 (nodecode-arcade::setting :keep-scores)) "the original's ten a board")
   (is (string= "YOU" (nodecode-arcade::setting :initials)) "scores are filed under the section's initials"))
  (:refused ("keep_scores" 0)))

(deftest arcade-cell-the-games-tool-answers-the-cabinet ()
  (with-cabinet ()
    (let ((answer (arcade-tool "arc-call-1" "arcade_games" "{}")))
      (is (search "Neon Snake" answer) "the first game")
      (is (search "Neon Breakout" answer) "the second")
      (is (search "Neon Pong" answer) "the third")
      (is (search "arcade_play" answer) "and how to play it"))
    (is (search "Neon Pong" (arcade-tool "arc-call-2" "arcade_games" "{\"game\": \"pong\"}")) "one game alone answers its own row")
    (is (search "no arcade game" (arcade-tool "arc-call-3" "arcade_games" "{\"game\": \"chess\"}")) "and a game it has not is a line, not a failed call")))

(deftest arcade-cell-a-play-answers-a-frame-and-files-the-score ()
  (with-cabinet ("initials" "LEO")
    (let ((answer (arcade-tool "arc-call-4" "arcade_play" "{\"game\": \"snake\", \"moves\": \"eee\"}")))
      (is (search "Neon Snake" answer) "the frame names the game")
      (is (search "score" answer) "and carries the score"))
    (let ((answer (arcade-tool "arc-call-5" "arcade_play"
                               "{\"game\": \"snake\", \"moves\": \"nnnnnnnnnnnnnnnn\"}")))
      (is (search "Game over" answer) "the run ends at the wall")
      (is (search "Filed on the board: LEO" answer) "and the score is filed under the section's initials"))
    (let ((answer (arcade-tool "arc-call-6" "arcade_top_scores" "{}")))
      (is (search "LEO" answer) "the board the play filed to")
      (is (search "play" answer) "with the play counted"))
    (let ((answer (arcade-tool "arc-call-7" "arcade_player_stats" "{\"initials\": \"LEO\"}")))
      (is (search "in all" answer) "the cabinet's total")
      (is (search "LEO" answer) (format nil "and one player's entries: ~a" answer)))))

(deftest arcade-cell-the-command-answers-the-operator ()
  (with-cabinet ("initials" "LEO")
    (is (search "Neon Pong" (nodecode-arcade:run-slash "" nil)) "bare, it answers the boards")
    (is (search "Neon Pong" (nodecode-arcade:run-slash "pong 555" nil)) "/arcade <game> <moves> plays")
    (is (search "Neon Snake" (nodecode-arcade:run-slash "snake" nil)) "a game alone starts it and shows the frame")
    (is (search "no arcade game" (nodecode-arcade:run-slash "chess" nil)) "and a game it has not comes back with the reason")))

(deftest arcade-cell-a-play-with-no-game-is-refused ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-arcade::arcade-play-arguments)
                             (nlk:make-json-object "moves" "eee"))) "a play with no game is refused by the declaration")
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-arcade::arcade-play-arguments)
                             (nlk:make-json-object "game" "chess"))) "and so is a game outside the three"))
