;;;; store.lisp --- the leaderboard: top ten a game, arcade initials, plays.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps its scores as JSON under the harness's plugin-data
;;;; directory and answers three tool calls from it: the games, the top ten a
;;;; game, and aggregate stats. The same file is read and written here, as the
;;;; same shape of data, at <home>/arcade/scores.txt: a score is (INITIALS
;;;; SCORE AT), a game is (:game ID :plays N :scores (...)). One cabinet, one
;;;; board, no upload: nothing leaves the machine.

(in-package #:nodecode-arcade)

(defparameter *store-file* nil
  "Where the board is written. NIL (the default) means the arcade folder
under the Nodecode home; a test binds it to a file of its own.")

(defun store-path ()
  "The file the board is in."
  (or *store-file* (nlk:home "arcade/scores.txt")))

(defun empty-store ()
  "A board with every game on it and nothing played."
  (loop for id in (game-ids)
        collect (list :game id :plays 0 :scores '())))

(defun normalize-initials (text)
  "TEXT as the initials a board files a score under: letters and digits,
uppercased, three at most, ??? when nothing is left."
  (let* ((raw (remove-if-not #'alphanumericp (princ-to-string (or text ""))))
         (clean (string-upcase raw)))
    (cond ((zerop (length clean)) "???")
          ((> (length clean) 3) (subseq clean 0 3))
          (t clean))))

(defun clean-row (row &key (keep 10))
  "ROW as the board keeps it: every score well formed, best first, KEEP of them."
  (let* ((game (getf row :game))
         (scores (loop for entry in (getf row :scores)
                       when (and (consp entry) (second entry))
                         collect (list (normalize-initials (first entry))
                                       (max 0 (truncate (second entry)))
                                       (or (third entry) 0))))
         (sorted (sort scores #'> :key #'second)))
    (list :game game
          :plays (max 0 (truncate (or (getf row :plays) 0)))
          :scores (if (> (length sorted) keep) (subseq sorted 0 keep) sorted))))

(defun read-store (&key (keep 10))
  "The board as the file has it; a missing or unreadable file is an empty one."
  (let ((path (store-path)))
    (if (not (probe-file path))
        (empty-store)
        (handler-case
            (with-open-file (in path :direction :input :if-does-not-exist nil)
              (let ((*read-eval* nil)
                    (*package* (find-package :keyword)))
                (let ((data (read in nil nil)))
                  (if (and (listp data) data)
                      (loop for id in (game-ids)
                            for row = (find id data :key (lambda (r) (getf r :game))
                                                         :test (lambda (a b) (and a b (string-equal a b))))
                            collect (clean-row (or row (list :game id :plays 0 :scores '()))
                                               :keep keep))
                      (empty-store)))))
          (error () (empty-store))))))

(defun write-store (store)
  "Write STORE, whole, under a temporary name then moved into place."
  (let* ((path (store-path))
         (tmp (merge-pathnames (format nil "~a.tmp" (file-namestring path)) path)))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
      (let ((*print-pretty* t)
            (*print-readably* t)
            (*package* (find-package :keyword)))
        (prin1 store out)
        (terpri out)))
    (rename-file tmp path)
    store))

(defun game-entry (store game)
  "STORE's row for GAME, or NIL."
  (find (string-downcase (princ-to-string game)) store
        :key (lambda (row) (getf row :game))
        :test (lambda (a b) (and a b (string-equal a b)))))

(defun record-play (game)
  "Count one play of GAME, as the original counts it when a run starts."
  (let* ((store (read-store))
         (row (game-entry store game)))
    (when row
      (incf (getf row :plays))
      (write-store store))
    row))

(defun submit-score (game initials score &key (keep 10))
  "File SCORE at INITIALS on GAME's board. => :rank (NIL when it missed the
board), :made-board, and :best, the board's top score."
  (let* ((store (read-store :keep keep))
         (row (game-entry store game)))
    (unless row
      (fail "~s is no arcade game; the cabinet has ~{~a~^, ~}" game (game-ids)))
    (let* ((entry (list (normalize-initials initials)
                        (max 0 (truncate (or score 0)))
                        (get-universal-time)))
           (scores (sort (cons entry (copy-list (getf row :scores))) #'> :key #'second))
           (rank (1+ (position entry scores :test #'equal))))
      (setf (getf row :scores) (if (> (length scores) keep)
                                    (subseq scores 0 keep)
                                    scores))
      (write-store store)
      (list :rank (and (<= rank keep) rank)
            :made-board (<= rank keep)
            :best (second (first (getf row :scores)))))))

(defun board (game &key (keep 10))
  "GAME's board: :label, :plays and the top KEEP scores."
  (let* ((store (read-store :keep keep))
         (row (game-entry store game)))
    (unless row
      (fail "~s is no arcade game; the cabinet has ~{~a~^, ~}" game (game-ids)))
    (list :game (getf row :game)
          :label (game-label (getf row :game))
          :plays (getf row :plays)
          :scores (getf row :scores))))

(defun summary (&key initials (keep 10))
  "The cabinet's aggregate stats: total plays, and a game's plays and
champion; with INITIALS, every board entry filed under them."
  (let* ((store (read-store :keep keep))
         (out (list :total-plays 0 :games '())))
    (dolist (row store)
      (let* ((game (getf row :game))
             (best (first (getf row :scores))))
        (push (list :game game
                    :label (game-label game)
                    :plays (getf row :plays)
                    :champion best)
              (getf out :games))
        (incf (getf out :total-plays) (getf row :plays))))
    (setf (getf out :games) (nreverse (getf out :games)))
    (when (and initials (plusp (length (princ-to-string initials))))
      (let ((wanted (normalize-initials initials))
            (mine '()))
        (dolist (row store)
          (let ((hits (loop for entry in (getf row :scores)
                            for i from 1
                            when (string= (first entry) wanted)
                              collect (list :rank i :score (second entry)))))
            (when hits
              (push (cons (getf row :game) hits) mine))))
        (setf (getf out :player) wanted)
        (setf (getf out :player-scores) (nreverse mine))))
    out))
