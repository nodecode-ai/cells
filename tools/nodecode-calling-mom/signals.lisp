;;;; signals.lisp --- "the worker is stuck", measured rather than declared.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's signals.py, cut down to the two signals a
;;;; tool-call hook can read here:
;;;;
;;;;   loop        the identical tool call three times within the last ten
;;;;               calls: the agent is going in circles.
;;;;   same_result the same answer or the same failure three times in a row,
;;;;               normalized so a changing path, hash or number still counts
;;;;               as the same one (three red test runs with different
;;;;               message numbers are one red test).
;;;;
;;;; The model never judges itself: a weak model cannot tell when it is out
;;;; of its depth, so the plugin watches what it does instead.

(in-package #:nodecode-calling-mom)

(defparameter +path+ "(?<![\\w.])/[\\w./-]+")
(defparameter +hex+ "\\b(?:0x)?[0-9a-f]{6,}\\b")
(defparameter +digits+ "\\d+")
(defparameter +space+ "\\s+")

(defparameter +loop-window+ 10
  "How many calls the loop signal reads back.")

(defvar *path-scanner* (cl-ppcre:create-scanner +path+ :case-insensitive-mode t))
(defvar *hex-scanner* (cl-ppcre:create-scanner +hex+ :case-insensitive-mode t))
(defvar *digits-scanner* (cl-ppcre:create-scanner +digits+))
(defvar *space-scanner* (cl-ppcre:create-scanner +space+))

(defun signature (text)
  "A stable fingerprint of a result: paths, hex and numbers normalized away,
the tail read, case folded."
  (let* ((raw (if (stringp text) text (format nil "~a" text)))
         (tail (if (> (length raw) 400) (subseq raw (- (length raw) 400)) raw)))
    (flet ((sub (scanner replacement string)
             (cl-ppcre:regex-replace-all scanner string replacement)))
      (string-downcase
       (string-trim " "
                    (sub *space-scanner* " "
                         (sub *digits-scanner* "N"
                              (sub *hex-scanner* "<h>"
                                   (sub *path-scanner* "<p>" tail)))))))))

(defstruct (tracker (:constructor make-tracker (&key (loop-limit 3) (same-limit 3))))
  "One run's recent calls: what the signals are measured from."
  (loop-limit 3 :read-only t)
  (same-limit 3 :read-only t)
  (window '() :type list)          ; call keys, newest first
  (last-sig nil)
  (streak 0 :type fixnum)
  (steps 0 :type fixnum))

(defun call-key (tool arguments)
  "The identity of one call: the tool and its arguments, printed."
  (format nil "~a ~a" tool arguments))

(defun tracker-feed (tracker tool arguments result)
  "Record one finished call. => the signals it fired, a list of keywords."
  (let ((fired '()))
    ;; the identical result, again and again: one failure, or one answer.
    (let ((sig (signature result)))
      (setf (tracker-streak tracker)
            (if (and (tracker-last-sig tracker) (string= sig (tracker-last-sig tracker)))
                (1+ (tracker-streak tracker))
                1)
            (tracker-last-sig tracker) sig)
      (when (>= (tracker-streak tracker) (tracker-same-limit tracker))
        (push :same-result fired)
        (setf (tracker-streak tracker) 0 (tracker-last-sig tracker) nil)))
    ;; the identical call, again and again near each other.
    (let* ((key (call-key tool arguments))
           (window (cons key (tracker-window tracker)))
           (window (if (> (length window) +loop-window+)
                       (subseq window 0 +loop-window+)
                       window)))
      (setf (tracker-window tracker) window)
      (when (>= (count key window :test #'string=) (tracker-loop-limit tracker))
        (push :loop fired)
        (setf (tracker-window tracker) '())))
    (incf (tracker-steps tracker))
    (when (>= (tracker-steps tracker) 40)
      (push :steps fired)
      (setf (tracker-steps tracker) 0))
    (nreverse fired)))

(defun tracker-notice (signals)
  "One line naming the signals that fired, for the advisor's user message."
  (format nil "~(~{~a~^, ~}~)" signals))
