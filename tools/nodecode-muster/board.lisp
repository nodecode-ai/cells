;;;; board.lisp --- the board, and the foreman's report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's render_table and digest: the same four
;;;; numbers over the rows - working, stuck, failing on repeat, gone - and the
;;;; same one line per flight, with the action that matches its state.

(in-package #:nodecode-muster)

(defun counts (rows)
  "The four numbers the header carries, read off the rows."
  (let ((run 0) (stall 0) (spin 0) (lost 0) (now (get-universal-time))
        (threshold (setting-int :spin-threshold 3))
        (window (setting-int :spin-window-seconds 1800)))
    (dolist (flight rows)
      (let ((state (getf flight :state)))
        (cond ((equal state "RUNNING") (incf run))
              ((equal state "STALLED") (incf stall))
              ((and (equal state "LOST")
                    (let ((ended (getf flight :ended)))
                      (or (null ended) (<= (- now ended) 86400))))
               (incf lost))
              ((and (spinning-p flight rows now window threshold)) (incf spin)))))
    (list :run run :stall stall :spin spin :lost lost)))

(defun header-line (counts)
  (let ((parts '()))
    (flet ((part (n word) (when (plusp n) (push (format nil "~d ~a" n word) parts))))
      (part (getf counts :run) "working")
      (part (getf counts :stall) "stuck")
      (part (getf counts :spin) "failing on repeat")
      (part (getf counts :lost) "gone"))
    (if parts
        (format nil "muster: ~{~a~^, ~}" (nreverse parts))
        "muster: all quiet - nothing running.")))

(defparameter +marks+
  '(("RUNNING" . ">") ("STALLED" . "!") ("SUCCEEDED" . "+") ("FAILED" . "x")
    ("INTERRUPTED" . "~") ("CANCELLED" . "+") ("LOST" . "-") ("CANCEL_REQUESTED" . "~")))

(defparameter +words+
  '(("RUNNING" . "working") ("STALLED" . "stuck") ("SPINNING" . "failing on repeat")
    ("SUCCEEDED" . "done") ("FAILED" . "failed") ("INTERRUPTED" . "cut short")
    ("CANCELLED" . "stopped") ("LOST" . "gone") ("CANCEL_REQUESTED" . "stopping...")))

(defun state-word (state) (or (cdr (assoc state +words+ :test #'string=)) (string-downcase state)))

(defun ago (seconds)
  (cond ((null seconds) "")
        ((< seconds 90) (format nil "~ds ago" (round seconds)))
        ((< seconds 5400) (format nil "~dm ago" (round seconds 60)))
        (t (format nil "~dh ago" (round seconds 3600)))))

(defun one-line (flight rows now)
  "FLIGHT as the board draws it: a mark, its state, its id, its job, and what
to do about it."
  (let* ((state (getf flight :state))
         (id (getf flight :id))
         (short (subseq id 0 (min 8 (length id))))
         (word (if (and (equal state "FAILED")
                        (spinning-p flight rows now
                                    (setting-int :spin-window-seconds 1800)
                                    (setting-int :spin-threshold 3)))
                   "failing on repeat"
                   (state-word state)))
         (show (setting :show-goals))
         (label (and show (getf flight :label)))
         (lines (list (format nil "~a ~a  \"~a\"~@[  ~a~]"
                               (or (cdr (assoc state +marks+ :test #'string=)) ".")
                               word short label))))
    (flet ((say (text) (push text lines)))
      (let ((age (event-age flight now)))
        (cond
          ((equal state "RUNNING")
           (say (format nil "    busy - ~d tool call~:p~@[, ~a~]"
                         (or (getf flight :calls) 0)
                         (and age (format nil "last heard ~a" (ago age))))))
          ((equal state "STALLED")
           (say (format nil "    silent for ~a with nothing in progress - probably stuck."
                         (or (and age (ago age)) "a while")))
           (say (format nil "    stop it: /muster stop ~a" short)))
          ((equal state "FAILED")
           (when (plusp (or (getf flight :requeues) 0))
             (say (format nil "    tried ~d time~:p and failed every time - something upstream is broken."
                           (1+ (getf flight :requeues)))))
           (say (format nil "    re-run it: /muster note ~a keep going" short)))
          ((equal state "LOST")
           (say "    the image went down before this one finished. found at a start - it needs a re-run."))
          ((equal state "INTERRUPTED")
           (say "    cut short before it answered."))
          ((equal state "CANCELLED")
           (say "    stopped on purpose."))
          ((equal state "CANCEL_REQUESTED")
           (say "    stopping - it finishes its current step, then quits."))
          ((equal state "SUCCEEDED")
           (when (plusp (or (getf flight :requeues) 0))
             (say (format nil "    finished after ~d re-run~:p." (getf flight :requeues))))))
        (when (and (member state '("RUNNING" "STALLED") :test #'string=)
                   (getf flight :turn))
          (say (format nil "    send it a note: /muster note ~a <message>" short))
          (unless (equal state "STALLED")
            (say (format nil "    stop it: /muster stop ~a" short))))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defvar *board-limit* nil
  "How many flights the board lists, bound by the tool's own call; the
section's board_rows otherwise.")

(defun table (&optional (now (get-universal-time)))
  "The board, as /muster and the muster_status tool answer it."
  (let* ((rows (flights))
         (limit (or *board-limit* (setting-int :board-rows 40)))
         (shown (if (> (length rows) limit) (subseq rows 0 limit) rows))
         (counts (counts rows)))
    (with-output-to-string (out)
      (format out "~a~%" (header-line counts))
      (if (null shown)
          (format out "no flights yet: they appear as soon as a subagent runs - (fork \"task\" ...) starts one.~%")
          (dolist (flight shown)
            (format out "~a~%" (one-line flight rows now)))))))

(defun digest (&optional (now (get-universal-time)))
  "The foreman's report: how the fleet stands, what needs a decision, and
nothing else."
  (let* ((rows (flights))
         (counts (counts rows))
         (threshold (setting-int :spin-threshold 3))
         (window (setting-int :spin-window-seconds 1800))
         (parts '()))
    (flet ((part (n word) (when (plusp n) (push (format nil "~d ~a" n word) parts))))
      (part (getf counts :run) "working")
      (part (getf counts :stall) "stuck")
      (part (getf counts :spin) "failing on repeat")
      (part (getf counts :lost) "gone"))
    (with-output-to-string (out)
      (format out "fleet: ~a~%" (if parts (format nil "~{~a~^, ~}" (nreverse parts)) "all quiet"))
      (let ((needs (remove-if-not
                    (lambda (flight)
                      (let ((state (getf flight :state)))
                        (or (member state '("STALLED" "LOST") :test #'string=)
                            (spinning-p flight rows now window threshold))))
                    rows)))
        (format out "~%")
        (if (null needs)
            (format out "nothing needs you right now.~%")
            (progn
              (format out "needs a decision:~%")
              (dolist (flight (if (> (length needs) 5) (subseq needs 0 5) needs))
                (let* ((state (getf flight :state))
                       (id (getf flight :id))
                       (short (subseq id 0 (min 8 (length id))))
                       (label (and (setting :show-goals) (getf flight :label)))
                       (word (cond ((equal state "LOST") "gone")
                                   ((equal state "STALLED") "stuck")
                                   (t "failing on repeat"))))
                  (format out "  - ~a: \"~a\" [~a]~a~%"
                          word (or label "") short
                          (cond ((equal state "LOST") " - re-run it; it will not come back on its own")
                                ((equal state "STALLED") (format nil " - /muster stop ~a" short))
                                (t " - re-run it when the cause is fixed")))))
              (when (> (length needs) 5)
                (format out "  ... and ~d more~%" (- (length needs) 5)))))))))
