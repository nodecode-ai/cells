;;;; support.lisp --- the command-tray test slice: a stubbed shell, no process.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the same nodecode.test registry every cell's do
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; COMMAND-TRAY-CELL- name prefix; RUN-COMMAND-TRAY-TESTS runs that slice.
;;;;
;;;; The one seam that reaches outside the image is the shell, and it is a
;;;; stub here: TRAY-SHELL calls the cell's own advice around SH with a NEXT
;;;; that answers canned output and an exit status, so a test proves what the
;;;; tray records without running a command, and nothing dials or spawns.

(in-package #:nodecode.test)

(define-test-slice "command-tray" "COMMAND-TRAY-CELL-" :start nodecode-command-tray:start-cell)

(defun tray-advice ()
  "The function the cell put on SH, or NIL while it is stopped."
  (cell-hook "nodecode-command-tray" 'nodecode.evolved:sh))

(defun tray-shell (command output status &key directory)
  "COMMAND run through the cell's advice with a stubbed shell: NEXT answers
OUTPUT and STATUS, and DIRECTORY rides the call as a real (sh ...) names one.
=> (values OUTPUT STATUS), what the advised call answered."
  (let ((advice (tray-advice)))
    (assert advice () "the tray does not advise the shell verb")
    (funcall advice (lambda (&rest args)
                      (declare (ignore args))
                      (values output status))
             command :directory directory)))

(defun tray-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun tray-rows ()
  "The tray's rows, oldest first."
  (command-tray:runs))
