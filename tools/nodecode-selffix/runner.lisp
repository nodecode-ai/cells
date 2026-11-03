;;;; runner.lisp --- a repro is one Lisp form, asked in a sealed fresh process of this organism.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A repro is a form that answers 1 while the failure stands and 0 once it is
;;;; gone. It is asked through the organism's own `trial' verb (launch.lisp):
;;;; a fresh process of this binary that loads a folder, reads one form from
;;;; stdin, and prints the number it answers on a marked last line. The folder
;;;; is a generation this home sealed, laid into a throwaway home by
;;;; NLK:BUILD-GENERATION, so the repro runs against the agent's own cells as
;;;; they stood, never against the live image or the live home.
;;;;
;;;; NLK:TRIAL alone is not sealed: its throwaway environment re-roots HOME,
;;;; NODECODE_HOME and XDG over the parent's environment, so the child still
;;;; sees the parent's keys and the network. The runner therefore starts the
;;;; same verb under bwrap: the filesystem read-only, /home /root /run /tmp
;;;; hidden but the throwaway home and the binary's own folder, the
;;;; environment cleared to PATH, LANG and the throwaway roots, and every
;;;; namespace unshared, the network with them. Where bwrap is missing the
;;;; repro is not run at all: an unsealed run is never a pass or a finding.
;;;;
;;;; Locating is the kernel's `fork :from' way back done without a session: the
;;;; same repro asked at older generations, halving, until the step where it
;;;; turned from gone to standing.

(in-package #:nodecode-selffix)

(defparameter *repro-seconds* 120 "How long one sealed repro may run.")

(defparameter *locate-probes* 7
  "Sealed runs a locate may spend: the oldest generation, then 6 halvings, which cover 64.")

(defparameter +score-marker+ "nodecode-trial-score "
  "The prefix of the one stdout line the trial verb answers on (launch.lisp).")

(defvar *ask* 'ask-sealed
  "(funcall *ask* TEXT HOME) => (values SCORE DETAIL); a test binds a stub.")

(defvar *lay* 'lay-generation
  "(funcall *lay* NUMBER HOME) lays generation NUMBER as HOME's cells/; a test binds a stub.")

(defun native (path)
  (string-right-trim "/" (sb-ext:native-namestring (uiop:ensure-directory-pathname path))))

(defun on-path (program)
  "PROGRAM's file on PATH, or NIL."
  (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "") :separator '(#\:))
        for file = (and (plusp (length dir)) (probe-file (format nil "~a/~a" dir program)))
        when file return file))

(defun organism-binary ()
  "This organism's binary, or NIL in a developer image, which runs no trial."
  (first (nlk::organism-command "trial")))

(defun hidden-roots ()
  "The folders a repro must not see wherever they live: this organism's home
and the user's, beside the fixed /home and /root."
  (remove-duplicates (remove nil (list (ignore-errors (native (nlk:home "")))
                                       (let ((home (uiop:getenv "HOME")))
                                         (and home (plusp (length home)) (native home)))))
                     :test #'equal))

(defun sealed-command (binary home &key (hide (hidden-roots)))
  "The argv that runs BINARY's trial verb sealed, with HOME its whole world:
every folder in HIDE, /home, /root, /run and the temporary folders under an
empty tmpfs, then the binary's own folder read-only and HOME writable."
  (let ((root (native home))
        (own (native (uiop:pathname-directory-pathname binary))))
    `("bwrap" "--ro-bind" "/" "/"
              "--tmpfs" "/home" "--tmpfs" "/root" "--tmpfs" "/run" "--tmpfs" "/tmp" "--tmpfs" "/var/tmp"
              ,@(loop for dir in hide append (list "--tmpfs" dir))
              "--ro-bind" ,own ,own "--bind" ,root ,root
              "--dev" "/dev" "--proc" "/proc"
              "--unshare-all" "--die-with-parent" "--new-session" "--clearenv"
              ,@(loop for (name value) on (list "PATH" "/usr/local/bin:/usr/bin:/bin" "LANG" "C.UTF-8"
                                                "HOME" root "NODECODE_HOME" (format nil "~a/" root)
                                                "XDG_CACHE_HOME" (format nil "~a/cache" root)
                                                "XDG_CONFIG_HOME" (format nil "~a/config" root)
                                                "XDG_DATA_HOME" (format nil "~a/data" root))
                      by #'cddr append (list "--setenv" name value))
              ,(sb-ext:native-namestring binary) "trial")))

(defun score-of (output)
  "The number on OUTPUT's last marked line, or NIL."
  (loop for line in (reverse (uiop:split-string output :separator '(#\Newline)))
        for value = (and (uiop:string-prefix-p +score-marker+ line)
                         (ignore-errors
                          (let ((*read-eval* nil))
                            (read-from-string (subseq line (length +score-marker+))))))
        when (realp value) return value))

(defun ask-sealed (text home)
  ;; HOME lies under this organism's home (selffix/scratch/), which the seal
  ;; hides: it is bound back after the tmpfs, alone.
  "TEXT, one form, asked in a sealed fresh process under HOME => (values SCORE DETAIL)."
  (let ((binary (organism-binary)))
    (cond ((null binary) (values nil "no organism binary: a developer image runs no trial"))
          ((null (on-path "bwrap")) (values nil "no bwrap: the repro cannot be sealed, so it is not run"))
          (t (let ((config (merge-pathnames "config.jsonc" home)))
               (unless (probe-file config)
                 (nlk:write-file-atomically config (format nil "{\"update\": {\"mode\": \"off\"}}~%"))))
             (multiple-value-bind (out err status)
                 (nlk:run-bounded (sealed-command binary home) :seconds *repro-seconds* :input (format nil "~a~%" text))
               (let ((score (and (not (eq status :timeout)) (score-of (princ-to-string (or out ""))))))
                 (values score
                         (cond (score "scored")
                               ((eq status :timeout) "killed at the deadline")
                               (t (format nil "exited ~a without a score~@[: ~a~]" status
                                          (and err (plusp (length (princ-to-string err)))
                                               (nlk:one-line (princ-to-string err) :cap 200))))))))))))

(defun lay-generation (number home)
  "HOME's cells/ as generation NUMBER had it, or, when NUMBER is NIL (a home
that has sealed no generation yet), this home's folder as it stands."
  (let ((target (merge-pathnames "cells/" home)))
    (if number
        (nlk:build-generation number target)
        (let ((root (or nlk::*cells-directory* (nlk:home "cells/"))))
          (if (and root (probe-file root))
              (multiple-value-bind (out err status)
                  (nlk:run-bounded (list "cp" "-R" (native root) (native target)) :seconds 120)
                (declare (ignore out))
                (unless (eql status 0)
                  (fail "the folder could not be copied: ~a" (nlk:one-line (princ-to-string (or err "")) :cap 200))))
              (ensure-directories-exist target))))
    target))

(defun generation-numbers ()
  "The generations this home sealed, oldest first."
  (sort (loop for manifest in (nlk:manifests)
              for number = (and (hash-table-p manifest) (gethash "number" manifest))
              when (integerp number) collect number)
        #'<))

(defun guarded (text)
  "TEXT wrapped so a repro that signals answers -1: could not run, never a pass."
  (format nil "(handler-case ~a (error () -1))" text))

(defun ask-at (text number)
  "TEXT asked at generation NUMBER => (values VERDICT DETAIL): :STANDS on 1, :GONE on 0,
:UNDECIDED on 2 (the repro cannot judge), :UNKNOWN when it could not run."
  (let ((home (uiop:ensure-directory-pathname
               (selffix-path (format nil "scratch/~36r/" (random (expt 36 10) (make-random-state t)))))))
    (ensure-directories-exist home)
    (unwind-protect
         (progn (funcall *lay* number home)
                (multiple-value-bind (score detail) (funcall *ask* (guarded text) home)
                  (values (case score (1 :stands) (0 :gone) (2 :undecided) (t :unknown))
                          (if (eql score -1) "the repro signalled" detail))))
      (ignore-errors (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)))))

(defun locate (text numbers)
  "Where between NUMBERS, oldest first, the repro TEXT turned from gone to
standing, if it stands at the newest => a plist (:GONE-AT G :STANDS-AT H),
(:STANDS-SINCE OLDEST) when it stands at every one asked, or NIL when the
search could not tell within *LOCATE-PROBES* runs."
  (when (rest numbers)
    (let ((lo 0) (hi (1- (length numbers))) (probes 0))
      (case (progn (incf probes) (ask-at text (nth lo numbers)))
        (:stands (return-from locate (list :stands-since (nth lo numbers))))
        (:unknown (return-from locate nil)))
      (loop while (> (- hi lo) 1)
            do (when (>= probes *locate-probes*) (return-from locate nil))
               (let ((mid (floor (+ lo hi) 2)))
                 (incf probes)
                 (case (ask-at text (nth mid numbers))
                   (:stands (setf hi mid))
                   (:gone (setf lo mid))
                   (t (return-from locate nil)))))
      (list :gone-at (nth lo numbers) :stands-at (nth hi numbers)))))
