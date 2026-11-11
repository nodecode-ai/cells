;;;; shape.lisp --- the shape of one shell command: its rm danger tokens.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A scar is a token set, and the set is drawn from the three words an rm
;;;; command says: rm, recursive, force. The path is not part of it, so
;;;; `rm -rf build' and `rm -fr ./out' are one shape, and a later call that
;;;; carries all three tokens is that shape again.
;;;;
;;;; The command is READ, not grepped. It is cut into the simple commands it
;;;; runs (an unquoted ; & | or newline starts the next), each one's leading
;;;; wrapper words are stepped over (nice, nohup, timeout, xargs, stdbuf,
;;;; doas, env with its options and assignments, and up to two other leading
;;;; words such as git), and a shell -c body is read with the same rules,
;;;; eight shells deep. A command that only prints or tests its arguments
;;;; (echo, printf, cat, grep, ...) ends the reading, so a sentence of file
;;;; names, or an echoed command, is not the shape.
;;;;
;;;; What is not read: python -c, node -e, a script file, command
;;;; substitution, and a -- before -c. A scar is a speed bump, not a
;;;; boundary: a command written in a way this reader does not follow passes.

(in-package #:nodecode-scar)

(defparameter +max-text+ 200000 "A command longer than this is not read.")

(defparameter +max-shell+ 8 "How deep a shell -c body is followed.")

(defparameter +max-peel+ 8 "Leading wrapper words stepped over in one command.")

(defparameter +allowed-tokens+ '("force" "recursive" "rm")
  "The only tokens a scar can hold: the three words an rm command says.")

(defparameter +data-commands+
  '("echo" "printf" "cat" "head" "tail" "less" "more" "grep" "egrep" "fgrep"
    "sed" "awk" "wc" "tr" "sort" "uniq" "true" "false" "test" "[" "cut" "rev"
    "od" "hexdump" "strings" "file" "stat" "date" "basename" "dirname" "read" ":")
  "Words that print or test their arguments. They are not a command prefix.")

(defparameter +wrappers+
  '(("nice" "-n") ("nohup") ("time" "-f" "-o") ("timeout" "-s" "-k")
    ("xargs" "-n" "-i" "-l" "-p" "-s" "-d" "-e" "-a") ("command") ("exec" "-a")
    ("builtin") ("stdbuf" "-i" "-o" "-e") ("ionice" "-c" "-n" "-p") ("setsid")
    ("caffeinate" "-t" "-w") ("chronic") ("doas" "-u" "-c"))
  "Wrapper words that run the next word as a command, each with the flags
that take the next word as their value.")

(defparameter +shells+ '("sh" "bash" "dash" "zsh" "ksh" "ash")
  "The shells whose -c body is read as a command.")

(defparameter +backslash+ (code-char 92) "The shell's escape character.")

(defparameter +double-quote+ (code-char 34) "The shell's double quote.")

(defparameter +single-quote+ (code-char 39) "The shell's single quote.")

;;; --- cutting a line into words ----------------------------------------------

(defun split-segments (text)
  "TEXT cut at an unquoted ; & | or newline: the simple commands it runs.
A quoted separator belongs to the word it sits in."
  (let ((parts '()) (start 0) (i 0) (n (length text)) (quote nil))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((and quote (char= c +backslash+)) (incf i 2))
                     (quote (when (char= c quote) (setf quote nil)) (incf i))
                     ((or (char= c +double-quote+) (char= c +single-quote+))
                      (setf quote c) (incf i))
                     ((or (char= c #\;) (char= c #\|) (char= c #\&) (char= c #\Newline))
                      (push (subseq text start i) parts)
                      (setf start (1+ i))
                      (incf i))
                     (t (incf i)))))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun quote-char-p (c)
  "True when C opens or closes a shell quotation."
  (or (char= c +double-quote+) (char= c +single-quote+)))

(defun space-char-p (c)
  "True when C ends a shell word."
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Return) (char= c #\Newline)))

(defun tokenize (text)
  "TEXT as shell words: quotes and backslashes removed, a word ended at an
unquoted space, tab or return."
  (let ((words '())
        (buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (quote nil) (i 0) (n (length text)))
    (flet ((flush ()
             (when (plusp (fill-pointer buffer))
               (push (coerce (subseq buffer 0) 'string) words)
               (setf (fill-pointer buffer) 0))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((and quote (char= c +backslash+))
                        (when (< (1+ i) n)
                          (vector-push-extend (char text (1+ i)) buffer))
                        (incf i 2))
                       (quote
                        (if (char= c quote)
                            (setf quote nil)
                            (vector-push-extend c buffer))
                        (incf i))
                       ((quote-char-p c) (setf quote c) (incf i))
                       ((char= c +backslash+)
                        (when (< (1+ i) n)
                          (vector-push-extend (char text (1+ i)) buffer))
                        (incf i 2))
                       ((space-char-p c) (flush) (incf i))
                       (t (vector-push-extend c buffer) (incf i)))))
      (flush))
    (nreverse words)))

(defun command-name (word)
  "WORD without its directory, lowercased: /bin/RM is rm."
  (let* ((name (substitute #\/ +backslash+ word))
         (slash (position #\/ name :from-end t))
         (bare (if slash (subseq name (1+ slash)) name)))
    (string-downcase (string-right-trim "/" bare))))

(defun assignment-p (word)
  "True when WORD is NAME=value, the assignment env reads."
  (let ((equals (position #\= word)))
    (and equals (plusp equals)
         (let ((name (subseq word 0 equals)))
           (and (alpha-char-p (char name 0))
                (every (lambda (c) (or (alphanumericp c) (char= c #\_))) name))))))

;;; --- the three tokens ---------------------------------------------------------

(defun rm-tokens (argv)
  "The danger tokens one rm command's words carry: rm, and r and f as they
are spelled on it. The path is not kept."
  (let ((tokens '("rm")))
    (dolist (word (rest argv) tokens)
      (cond ((string= word "--") (return tokens))
            ((and (> (length word) 2) (string= "--" word :end2 2))
             (let* ((body (subseq word 2))
                    (name (subseq body 0 (or (position #\= body) (length body)))))
               (cond ((string= name "recursive") (pushnew "recursive" tokens :test #'equal))
                     ((string= name "force") (pushnew "force" tokens :test #'equal)))))
            ((and (> (length word) 1) (char= (char word 0) #\-)
                  (every #'alpha-char-p (subseq word 1)))
             (when (find #\r (subseq word 1)) (pushnew "recursive" tokens :test #'equal))
             (when (find #\f (subseq word 1)) (pushnew "force" tokens :test #'equal)))))))

(defun exec-rms (argv)
  "The danger tokens of an rm inside find's -exec or -execdir, which is a
command ARGV does not start with."
  (let ((found '()) (n (length argv)))
    (loop for index from 0 below n
          do (when (member (nth index argv) '("-exec" "-execdir") :test #'equal)
               (let ((scan (1+ index)) (done nil))
                 (loop while (and (< scan n) (not done))
                       do (let ((word (nth scan argv)))
                            (cond ((member word '(";" "+") :test #'equal) (setf done t))
                                  ((string= (command-name word) "rm")
                                   (setf found (union found
                                                      (rm-tokens (nthcdr scan argv))
                                                      :test #'equal))
                                   (setf done t))
                                  (t (incf scan))))))))
    found))

(defun shell-script (argv)
  "The body of a shell -c in ARGV, or NIL when ARGV is not that form."
  (when argv
    (let ((name (command-name (first argv))) (index 1))
      (when (and (string= name "busybox") (> (length argv) 1)
                 (member (command-name (second argv)) +shells+ :test #'equal))
        (setf name (command-name (second argv)) index 2))
      (when (member name +shells+ :test #'equal)
        (loop while (< index (length argv))
              do (let ((word (nth index argv)))
                   (cond ((string= word "--") (return nil))
                         ((or (string= word "-c")
                              (and (> (length word) 1)
                                   (char= (char word 0) #\-)
                                   (every #'alpha-char-p (subseq word 1))
                                   (find #\c (subseq word 1))))
                          (return (and (< (1+ index) (length argv))
                                       (nth (1+ index) argv))))
                         ((char= (char word 0) #\-) (incf index))
                         (t (return nil)))))))))

(defparameter +env-value-short+ '("u" "c" "p" "s")
  "env's short options that take the next word as their value.")

(defparameter +env-value-long+ '("--unset" "--chdir" "--argv0" "--split-string"
                                 "--block-signal" "--ignore-signal" "--default-signal")
  "env's long options that take the next word as their value.")

(defun after-env (argv)
  "The command env runs, past its options and assignments; NIL when ARGV is
not env, or env runs nothing."
  (when (and argv (string= (command-name (first argv)) "env"))
    (let ((rest (rest argv)))
      (loop while rest
            do (let ((word (first rest)))
                 (cond ((zerop (length word)) (setf rest (rest rest)))
                       ((assignment-p word) (setf rest (rest rest)))
                       ((string= word "--") (return (rest rest)))
                       ((and (> (length word) 2) (string= "--" word :end2 2))
                        (setf rest (if (or (find #\= word)
                                           (not (member word +env-value-long+ :test #'equal)))
                                       (rest rest)
                                       (cddr rest))))
                       ((and (> (length word) 1) (char= (char word 0) #\-))
                        (setf rest
                              (cond ((member (subseq word 1 2) +env-value-short+ :test #'equal)
                                     (if (> (length word) 2) (rest rest) (cddr rest)))
                                    (t (rest rest)))))
                       (t (return rest))))
            finally (return '())))))

(defun skip-flags (argv value-flags)
  "ARGV past its leading flags and assignments; VALUE-FLAGS take the next
word as their value."
  (loop while argv
        do (let ((word (first argv)))
             (cond ((zerop (length word)) (setf argv (rest argv)))
                   ((assignment-p word) (setf argv (rest argv)))
                   ((string= word "--") (return (rest argv)))
                   ((char/= (char word 0) #\-) (return argv))
                   (t (setf argv (if (member word value-flags :test #'equal)
                                     (cddr argv)
                                     (rest argv))))))
        finally (return argv)))

;;; --- one command, then the whole line -----------------------------------------

(defun segment-tokens (argv depth)
  "The danger tokens one simple command carries: its leading wrapper words
are peeled, and a shell -c body is read at DEPTH + 1."
  (let ((found '()) (generic 0) (steps 0))
    (loop
      (loop while (and argv (assignment-p (first argv))) do (setf argv (rest argv)))
      (when (null argv) (return found))
      (let ((name (command-name (first argv))))
        (cond
          ((member name +data-commands+ :test #'equal) (return found))
          ((string= name "rm") (return (union found (rm-tokens argv) :test #'equal)))
          (t
           (setf found (union found (exec-rms argv) :test #'equal))
           (let ((script (shell-script argv)))
             (when script
               (return (union found (command-tokens script (1+ depth)) :test #'equal))))
           (cond
             ((string= name "env")
              (setf argv (after-env argv))
              (when (null argv) (return found)))
             ((assoc name +wrappers+ :test #'equal)
              (let ((rest (skip-flags (rest argv)
                                       (rest (assoc name +wrappers+ :test #'equal)))))
                (when (string= name "timeout") (setf rest (rest rest)))
                (setf argv rest)))
             (t
              (when (>= generic 2) (return found))
              (incf generic)
              (setf argv (skip-flags (rest argv) '())))))))
      (incf steps)
      (when (> steps +max-peel+) (return found)))))

(defun command-tokens (text &optional (depth 0))
  "The rm danger tokens TEXT carries, sorted: rm, recursive, force. Empty
when it carries none, or is too long, or nests shells too deep to read."
  (if (or (not (stringp text)) (zerop (length text))
          (> (length text) +max-text+) (> depth +max-shell+))
      '()
      (let ((folded (string-downcase text)) (tokens '()))
        (dolist (segment (split-segments folded) (sort tokens #'string<))
          (let ((words (tokenize segment)))
            (when words
              (setf tokens (union tokens (segment-tokens words depth) :test #'equal))))))))

(defun rm-shape-p (text)
  "True when TEXT carries an rm danger token at all."
  (not (null (command-tokens text))))
