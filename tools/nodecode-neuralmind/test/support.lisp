;;;; support.lisp --- the NeuralMind test slice and its one stubbed program.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-NEURALMIND::*RUN*, the
;;;; function every hook action runs through. WITH-RUN-STUB replaces it, so no
;;;; test needs the neuralmind program installed and none touches the network:
;;;; the stub answers the JSON one hook action prints, built by the same
;;;; encoder the cell decodes. The lifecycle test starts the whole cell.

(in-package #:nodecode.test)

(define-test-slice "neuralmind" "NEURALMIND-CELL-" :start nodecode-neuralmind:start-cell
  :defaults (("command" (neuralmind-stub-program))))

(defvar *calls* '()
  "The (ACTION INPUT DIRECTORY) of every run this test stubbed, newest first.")

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defparameter *project* nil
  "A directory that reads as a built NeuralMind project, made once a run.")

(defun executable-script (path &rest lines)
  "Write LINES to PATH, one per line, make it executable, and answer its
native namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line lines) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path))
                     :output nil :error-output nil)
  (uiop:native-namestring path))

(defun neuralmind-stub-program ()
  "A program the cell accepts as neuralmind: it answers one word, so a start
finds something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (executable-script (merge-pathnames "neuralmind-stub/neuralmind" (uiop:temporary-directory))
                               "cat >/dev/null"
                               "printf '%s' 'stub'"))))

(defun neuralmind-project ()
  "A directory `neuralmind build' has run in: the marker file the plugin and
the cell both read."
  (or *project*
      (let ((directory (merge-pathnames "neuralmind-project/" (uiop:temporary-directory))))
        (ensure-directories-exist (merge-pathnames ".neuralmind/build_status.json" directory))
        (with-open-file (out (merge-pathnames ".neuralmind/build_status.json" directory)
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
          (write-string "{}" out))
        (setf *project* directory))))

(defun neuralmind-unbuilt ()
  "A directory nothing has built."
  (let ((directory (merge-pathnames "neuralmind-unbuilt/" (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defun json-context (text)
  "TEXT as the reply one hook action prints: the block under the key the
plugin read."
  (nlk:encode-json-object
   (nlk:make-json-object
    "hookSpecificOutput" (nlk:make-json-object "additionalContext" text))))

(defmacro with-run-stub ((&key
                            (recap "NeuralMind session recap - the previous session in this project: the discount code was added.")
                            (recall "## NeuralMind associative recall~%Code matching this prompt:~%- shop/pricing.py: add_discount() L18")
                            (edit ""))
                         &body body)
  "BODY with the program stubbed: session-start answers RECAP, prompt-submit
RECALL, edit-activity EDIT, anything else nothing. Every run is pushed on
*CALLS*, and each edit is recorded inline rather than on a thread."
  `(let ((*calls* '())
         (nodecode-neuralmind::*background* #'funcall)
         (nodecode-neuralmind::*run*
           (lambda (action input directory)
             (push (list action input directory) *calls*)
             (let ((text (cond ((equal action "session-start") ,recap)
                               ((equal action "prompt-submit") ,recall)
                               ((equal action "edit-activity") ,edit)
                               (t ""))))
               (if (plusp (length text)) (json-context text) "")))))
     ,@body))

(defun neuralmind-call (action)
  "The newest stubbed run of ACTION, as (ACTION INPUT DIRECTORY), or NIL."
  (find action *calls* :key #'first :test #'equal))

(defun neuralmind-payload (action)
  "ACTION's stubbed run's stdin, decoded, or NIL when it never ran."
  (let ((call (neuralmind-call action)))
    (and call (ignore-errors (nlk:decode-json (second call))))))

;;; --- a session, a turn, and what its requests were told ---------------------

(defun neuralmind-turn (session &key (input "go") (source "cli"))
  "A turn admitted on SESSION the way a shell's turn is: its input is what the
round hook reads as the prompt."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command input
                    :invocation (nlk:default-invocation "turn.start" command
                                                         :source source :id command))))

(defun neuralmind-round (session turn round)
  "Run the registered :ROUND chain as the engine does: OP through it."
  (funcall (cell-hook "nodecode-neuralmind" :round)
           (list :session-id session :turn turn :round round)
           (lambda (op) (declare (ignore op)) nil)))

(defun neuralmind-told (session)
  "Every block SESSION's turns were told, as texts, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt" :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when text collect text))
