;;;; support.lisp --- debug-desk test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; debug-desk tests register into the SAME nodecode.test registry (the
;;;; core's DEFTEST, with its hermetic machine-state posture) under a
;;;; DEBUG-DESK-CELL- name prefix; RUN-DEBUG-DESK-TESTS runs exactly that
;;;; slice.
;;;;
;;;; Nothing here needs a network. The digest runs git on a throwaway
;;;; repository the test makes; a failure is a shell answer the test hands
;;;; the cell's own hook; the optional summary asks a Lisp function instead
;;;; of a model. The state file is a scratch file, never the home's.

(in-package #:nodecode.test)

(define-test-slice "debug-desk" "DEBUG-DESK-CELL-" :start nodecode-debug-desk:start-cell)

(defun desk-state-file ()
  "A scratch state file of this run's own."
  (merge-pathnames (format nil "debug-desk-~a/state.json" (gensym "STATE-"))
                   (uiop:temporary-directory)))

(defmacro with-desk (&body body)
  "BODY with the cell started on its defaults and a scratch state file, both
undone after."
  `(let ((nodecode-debug-desk::*state-file* (desk-state-file)))
     (with-cell-stop ((debug-desk-start))
       ,@body)))

(defun desk-answer (text &key (command "make test") (cwd "project"))
  "TEXT handed to the cell's :TOOL hook as a shell call's answer, as the
engine passes it: the answered text is what the call answered."
  (funcall (cell-hook "nodecode-debug-desk" :tool)
           (list :name "snippet"
                 :arguments (nlk:make-json-object "form" command "cwd" cwd)
                 :call-id "call-1")
           (lambda (op) (declare (ignore op)) text)))

(defun desk-call (name &rest pairs)
  "The tool NAME called through its own registered contract with PAIRS."
  (let ((entry (desk-tool-entry name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun desk-tool-entry (name)
  "The (NAME SCHEMA HANDLER) NLE holds for NAME."
  (let ((entry (nle::find-tool name)))
    (unless entry (error "no tool ~a" name))
    entry))

(defun desk-git (folder &rest args)
  "git ARGS in FOLDER, its output trimmed."
  (nlk:trimmed (or (nlk:run-bounded (list* "git" args) :seconds 30
                                      :directory (uiop:native-namestring folder))
                    "")))

(defun desk-repo (&key (name "start") (commits 2))
  "A throwaway git repository with COMMITS commits made now, this run's own.
=> its folder, a pathname."
  (let* ((folder (merge-pathnames (format nil "desk-repo-~a-~a-~a/" (get-universal-time) (random 1000000) (gensym "R"))
                                   (uiop:temporary-directory)))
         (file (merge-pathnames "README" folder)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string name out))
    (desk-git folder "init" "-q" "-b" "main")
    (dotimes (i commits)
      (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
        (format out "line ~d~%" i))
      (desk-git folder "add" "-A")
      (desk-git folder "-c" "user.name=desk" "-c" "user.email=desk@example.invalid"
                 "commit" "-q" "-m" (if (zerop i) "feat: start the desk" "fix: the desk reads git")))
    folder))
