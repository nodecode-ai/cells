;;;; support.lisp --- showreel test runner, the history it cuts, the stubbed ffmpeg.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Showreel tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a SHOWREEL-CELL-
;;;; name prefix; RUN-SHOWREEL-TESTS runs exactly that slice.
;;;;
;;;; The history is real: turns admitted on a real store, tool calls and
;;;; results recorded as the engine records them. The one stub is the
;;;; program: UIOP's runner never runs here, so no ffmpeg is needed and
;;;; nothing is encoded -- the stub writes the file ffmpeg would have.

(in-package #:nodecode.test)

(define-test-slice "showreel" "SHOWREEL-CELL-" :start nodecode-showreel:start-cell)

(defvar *calls* '()
  "Every argv the stub runner was given, newest first.")

(defun showreel-stub-runner (argv &key directory)
  "The stubbed ffmpeg: record ARGV and write the file it names last."
  (declare (ignore directory))
  (push argv *calls*)
  (let ((out (car (last argv))))
    (unless (or (null out) (uiop:string-prefix-p "-" out))
      (ensure-directories-exist out)
      (with-open-file (stream out :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-string "stub" stream))))
  (values "" 0))

(defun showreel-dir ()
  "A folder of this run's own for renders."
  (let ((dir (merge-pathnames (format nil "showreel-~d/" (random 1000000))
                               (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-showreel ((&rest pairs) &body body)
  "BODY over a temp store with session s1, the cell started on PAIRS (its
output_dir always one), and the program stubbed."
  `(with-temp-store ()
     (ensure-durable-session "s1")
     (with-cell-stop ((showreel-start "output_dir" (namestring (showreel-dir)) ,@pairs))
       (let ((nodecode-showreel::*run-program* #'showreel-stub-runner)
             (*calls* '()))
         ,@body))))

(defun showreel-history (&optional (session "s1") &key (ask "Build a snake game") secret)
  "One turn on SESSION as a real session leaves it: the ask, a failing step,
the step that fixed it, the spend and the answer."
  (let ((turn (nlk:admit-turn session (nlk::make-durable-id "command") ask)))
    (nlk:record-tool-call turn "c1" "snippet" "{\"command\": \"npm test\"}")
    (nlk:record-tool-result turn "c1" "snippet"
                            "ERROR: 1 failing - the snake eats its own tail"
                            :duration-ms 120)
    (nlk:record-tool-call turn "c2" "snippet" "{\"command\": \"npm test --fixed\"}")
    (nlk:record-tool-result turn "c2" "snippet" "12 passing" :duration-ms 90)
    (nlk:record-tool-call turn "c3" "write_file" "{\"path\": \"snake.html\"}")
    (nlk:record-tool-result turn "c3" "write_file" "wrote snake.html, 214 lines" :duration-ms 40)
    (nlk:record-turn-usage turn :input-tokens 41000 :output-tokens 6100
                                :provider "test" :model "")
    (nlk:complete-turn turn
                       (nlk:make-json-object
                        "role" "assistant"
                        "content" (format nil "Snake is ready: open snake.html.~@[ (used ~a)~]" secret)))
    session))

(defun showreel-call-tool (&rest pairs)
  "SHOWREEL_RENDER's registered body, called as the dispatcher calls it:
one decoded arguments object."
  (let ((entry (nle::find-tool "showreel_render")))
    (assert entry () "showreel_render is not registered")
    (funcall (third entry)
             (nlk:contract-decode (second entry)
                                   (apply #'nlk:make-json-object pairs)))))

(defun showreel-file (dir type)
  "The one file of TYPE in DIR, or NIL."
  (first (directory (merge-pathnames (make-pathname :name :wild :type type) dir))))

(defun showreel-text (path)
  "PATH read whole."
  (with-open-file (in path :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))
