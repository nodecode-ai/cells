;;;; support.lisp --- the hyprland test slice, the stubbed desktop, a fake PNG.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hyprland tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a HYPRLAND-CELL-
;;;; name prefix; RUN-HYPRLAND-TESTS runs exactly that slice.
;;;;
;;;; What is stubbed is the compositor, never the cell: *RUN* answers the
;;;; argv hyprctl, grim, wtype, wl-copy and the pointer helper would be
;;;; given, *WHICH* says which programs this machine has, and grim writes a
;;;; real PNG header into a real file. Every filter, the coordinate mapping,
;;;; the target enforcement, the key combination and the answer's shape are
;;;; the cell's own code.

(in-package #:nodecode.test)

(define-test-slice "hyprland" "HYPRLAND-CELL-" :start nodecode-hyprland:start-cell)

(defparameter *capture-dir* "hyprland-test-captures/"
  "Where a test's captures land: under the run's scratch home.")

(defmacro with-hyprland ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS as its `hyprland' section, and stopped
on unwind."
  `(with-cell-stop ((hyprland-start "capture_dir" *capture-dir* ,@pairs))
     ,@body))

(defun hyprland-tool (arguments &rest pairs)
  "The hyprland tool called through the wire as a provider sends it, ARGUMENTS
its JSON body, the cell started on PAIRS. => the decoded answer."
  (nlk:decode-json
   (with-cell-stop ((apply #'hyprland-start "capture_dir" *capture-dir* pairs))
     (princ-to-string
      (nle::message-content (execute-wire-call "call-1" "hyprland" arguments))))))

;;; --- the desktop the cell is shown -----------------------------------------------

(defvar *calls* nil
  "Every argv the stub machine was given, newest first.")

(defvar *inputs* nil
  "Every (PROGRAM . STDIN) the stub machine was given, newest first.")

(defparameter +clients+
  (concatenate 'string
   "[{\"address\": \"0x55a1b2c3\", \"class\": \"firefox\", \"title\": \"Mozilla Firefox\", "
   "\"pid\": 4242, \"at\": [100, 50], \"size\": [800, 600], "
   "\"workspace\": {\"name\": \"1\"}, \"focusHistoryID\": 0}, "
   "{\"address\": \"0x55d4e5f6\", \"class\": \"kitty\", \"title\": \"shell\", "
   "\"pid\": 5150, \"at\": [0, 0], \"size\": [1920, 1080], "
   "\"workspace\": {\"name\": \"2\"}, \"focusHistoryID\": 1}]")
  "Two windows: the focused Firefox at 100,50 and a full-screen terminal behind it.")

(defparameter +monitors+
  "[{\"x\": 0, \"y\": 0, \"width\": 1920, \"height\": 1080, \"name\": \"eDP-1\"}]"
  "One monitor, the layout the cell's extents and origin come from.")

(defun write-png-header (path width height)
  "A file at PATH whose first bytes are a real PNG signature and IHDR size."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (dolist (byte '(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82))
      (write-byte byte out))
    (dolist (number (list width height))
      (write-byte (ldb (byte 8 24) number) out)
      (write-byte (ldb (byte 8 16) number) out)
      (write-byte (ldb (byte 8 8) number) out)
      (write-byte (ldb (byte 8 0) number) out)))
  path)

(defun stub-answer (argv)
  "What the program ARGV names answers on a machine with one monitor."
  (let ((program (file-namestring (first argv))))
    (cond ((string= program "hyprctl")
           (cond ((member "clients" argv :test #'string=) (values +clients+ "" 0))
                 ((member "monitors" argv :test #'string=) (values +monitors+ "" 0))
                 ((member "repl" argv :test #'string=) (values "" "" 0))
                 (t (values "{}" "" 0))))
          ((string= program "grim") (write-png-header (fourth argv) 800 600) (values "" "" 0))
          (t (values "" "" 0)))))

(defmacro with-stub (() &body body)
  "BODY with the cell's two seams answering a machine of one monitor and two
windows, and every argv recorded in *CALLS*."
  `(let ((*calls* '())
         (*inputs* '())
         (nodecode-hyprland:*run* (lambda (argv &key input)
                                     (push (cons (file-namestring (first argv)) input) *inputs*)
                                     (push argv *calls*)
                                     (stub-answer argv)))
         (nodecode-hyprland:*which* (lambda (name)
                                       (unless (string= name "magick")
                                         (format nil "/usr/bin/~a" name)))))
     ,@body))

(defun called (program)
  "The newest argv whose program is PROGRAM, or NIL."
  (find program *calls* :test #'string= :key (lambda (argv) (file-namestring (first argv)))))

(defun arguments-of (program)
  "The arguments, after the program, of the newest call to PROGRAM."
  (let ((argv (called program))) (and argv (mapcar (lambda (argument) (princ-to-string argument))
                                                   (rest argv)))))

;;; --- the answer's fields ---------------------------------------------------------

(defun answer-value (answer type key)
  "KEY of ANSWER read as TYPE."
  (nlk:json-value answer type key))

(defun entries (answer key)
  "KEY's objects of ANSWER, one at a time."
  (coerce (nlk:json-value answer :array key) 'list))

(defun entry-value (entry type key)
  "KEY of one object of an answer read as TYPE."
  (nlk:json-value entry type key))
