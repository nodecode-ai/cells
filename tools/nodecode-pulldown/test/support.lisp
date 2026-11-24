;;;; support.lisp --- the pulldown test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; PULLDOWN-CELL- name prefix; RUN-PULLDOWN-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the pulldown program, so that is
;;;; what the tests stub: a special variable they rebind to a Lisp function
;;;; (WITH-PULLDOWN-STUB: fast, and it records every run), or a real /bin/sh
;;;; script that records its argv and prints JSON (PULLDOWN-RECORDING-PROGRAM),
;;;; for the one test that proves the program is really run. No test needs
;;;; pulldown installed, and none touches the network.

(in-package #:nodecode.test)

(define-test-slice "pulldown" "PULLDOWN-CELL-" :start nodecode-pulldown:start-cell
  :defaults (("program" (pulldown-stub-program)) ("cache" nil)))

(defvar *runs* '()
  "The (PROGRAM ARGS TIMEOUT) of every run this test stubbed, newest first.")

(defmacro with-pulldown-stub ((&key (stdout (pulldown-json)) (stderr "") (exit 0)
                                    (timed-out nil)) &body body)
  "BODY with the program stubbed: it answers STDOUT without running anything,
and every run is pushed on *RUNS* as (PROGRAM ARGS TIMEOUT)."
  `(let ((*runs* '())
         (nodecode-pulldown::*run*
           (lambda (program args timeout)
             (push (list program args timeout) *runs*)
             (values ,stdout ,stderr ,exit ,timed-out))))
     ,@body))

(defparameter +address+ "https://example.org/a-bridge"
  "The address the tests read.")

(defparameter +article+
  "# A Bridge in the Fog

The river rose, and the old crossing went with it."
  "The markdown the stub program answers with.")

(defun last-run ()
  "The (PROGRAM ARGS TIMEOUT) of the newest run."
  (first *runs*))

(defun last-args ()
  "The argv of the newest run."
  (second (last-run)))

(defun pulldown-refusal (url)
  "What reading URL refuses with, as the text the model would read."
  (handler-case (pulldown:fetch url)
    (error (condition) (princ-to-string condition))))

(defun pulldown-json (&key (title "A Bridge in the Fog")
                            (content +article+)
                            (ok t)
                            (status 200)
                            (error nil)
                            (from-cache nil)
                            (description "How one town rebuilt its crossing.")
                            (page-type "article")
                            (source "readability")
                            (quality "good")
                            (render-recommended nil)
                            (omit '()))
  "The JSON the pulldown CLI prints with --json-output --meta, as the cell
reads it. OMIT names members left out of the answer."
  (let* ((routing (nlk:make-json-object "page_type" page-type
                                         "source" source
                                         "quality_grade" quality
                                         "render_recommended" render-recommended))
         (meta (nlk:make-json-object "description" description "routing" routing))
         (table (nlk:make-json-object "url" +address+
                                       "status_code" status
                                       "title" title
                                       "content" content
                                       "elapsed_ms" 812
                                       "from_cache" from-cache
                                       "ok" ok
                                       "meta" meta)))
    (when error (setf (gethash "error" table) error))
    (dolist (key omit) (remhash key table))
    (nlk:encode-json-object table)))

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun pulldown-script (path &rest lines)
  "Write LINES to PATH after a /bin/sh line, make it executable, and answer its
native namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line lines) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path))
                     :output nil :error-output nil)
  (uiop:native-namestring path))

(defun pulldown-stub-program ()
  "A program the cell accepts as pulldown: it exits at once, so a start finds
something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (pulldown-script (merge-pathnames "pulldown-stub/pulldown"
                                               (uiop:temporary-directory))
                              "exit 0"))))

(defun pulldown-recording-program (directory &optional (json (pulldown-json)))
  "A pulldown that records the argv it was run with in DIRECTORY/argv and
prints JSON."
  (pulldown-script
   (merge-pathnames "pulldown" directory)
   (format nil "printf '%s\\n' \"$@\" > ~s"
           (namestring (merge-pathnames "argv" directory)))
   (format nil "printf '%s' ~s" json)))
