;;;; package.lisp --- the package, the words of a mark, and the seams a test stubs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-hud-teach", which is this folder's name.

(defpackage #:nodecode-hud-teach
  (:use #:cl)
  (:nicknames #:hud-teach)
  (:export #:start-cell #:stop-cell #:*hud-teach*
           #:hud-teach-error #:hud-teach-error-detail))

(in-package #:nodecode-hud-teach)

(nlk:define-peripheral hud-teach :not-running t)

;;; --- what a mark may be -----------------------------------------------------------

(defparameter +kinds+ '("circle" "rect" "arrow" "line" "polyline" "label")
  "The shapes a draw call paints. Each one is a picture of a control, not a click on it.")

(defparameter +legend-positions+ '("auto" "top-left" "top-right" "bottom-left" "bottom-right")
  "Where the step card may sit; auto takes the corner the marks cover least.")

(defparameter +colors+
  '(("black" . "#111111") ("blue" . "#0A84FF") ("green" . "#30D158")
    ("orange" . "#FF9F0A") ("purple" . "#BF5AF2") ("red" . "#FF3B30")
    ("white" . "#FFFFFF") ("yellow" . "#FFD60A"))
  "The eight colours a mark may name, and the ink each one is painted in.")

(defparameter +halos+
  '(("black" . "#FFFFFF") ("blue" . "#001428") ("green" . "#00240C")
    ("orange" . "#2A1600") ("purple" . "#1A0028") ("red" . "#2A0000")
    ("white" . "#111111") ("yellow" . "#1A1400"))
  "The one darker (or lighter) shade a badge of each colour is drawn on, so the
number reads on any wallpaper.")

(defun color-hex (name)
  "NAME's ink, red when it names none: validation already refused the rest."
  (or (cdr (assoc name +colors+ :test #'string=)) (cdr (assoc "red" +colors+ :test #'string=))))

(defun halo-hex (name)
  "The shade NAME's badge number sits on."
  (or (cdr (assoc name +halos+ :test #'string=)) (cdr (assoc "red" +halos+ :test #'string=))))

;;; --- what a call may ask for -------------------------------------------------------

(defparameter +max-step+ 12 "The highest step number, on a badge and on the card.")
(defparameter +max-shapes+ 24 "How many marks one draw paints.")
(defparameter +max-polyline-points+ 64 "How many points one polyline walks.")
(defparameter +max-label+ 80 "The longest caption beside a badge.")
(defparameter +max-title+ 80 "The longest lesson title on the card.")
(defparameter +max-step-text+ 120 "The longest line one step of the card carries.")
(defparameter +frame-slack+ 0.05
  "How far outside the screenshot a mark may reach: a badge on a window edge is
fine, a coordinate from another window is not.")
(defparameter +default-ttl+ 180 "Seconds marks stay up unless the call says otherwise.")
(defparameter +max-ttl+ 900 "The longest a call may keep marks up.")
(defparameter +resize-tolerance+ 2
  "Pixels of size change that still count as the window the capture came from.")
(defparameter +stale-after+ 600
  "Seconds past which a capture probably no longer matches the screen.")
(defparameter +heartbeat-max+ 15
  "Seconds a heartbeat may be old and the overlay still count as running.")

;;; --- the seams ---------------------------------------------------------------

(defun run-command (argv)
  "Run ARGV and answer (values STDOUT STDERR STATUS). A program that is not
there is a status of 127, not a condition: every caller reports it as its own
refusal, with the fix for the thing that is missing."
  (handler-case
      (multiple-value-bind (out err status)
          (uiop:run-program argv :output :string :error-output :string
                                 :ignore-error-status t)
        (values (or out "") (or err "") status))
    (error (condition)
      (values "" (princ-to-string condition) 127))))

(defvar *run* #'run-command
  "The one seam that runs a program: (ARGV) => (values STDOUT STDERR STATUS).
A test binds this; nothing else does.")

(defvar *now* (lambda () (get-universal-time))
  "The clock, as universal time. A test binds this.")

(defvar *sleep* (lambda (seconds) (sleep seconds))
  "How the cell waits for the overlay's first heartbeat. A test binds this.")

(defvar *platform* (lambda () (software-type))
  "The system this image runs on, as SBCL reports it. A test binds this to
\"Darwin\" so a Linux test run reaches the same code a Mac does.")

(defun macos-p ()
  "Whether this is a Mac: screencapture, sips and the window list are macOS's."
  (string= "Darwin" (funcall *platform*)))

(defun run (argv)
  "ARGV through *RUN*."
  (funcall *run* argv))

(defun write-value (path value)
  "Write VALUE readably to PATH, so a capture record or a scene the cell wrote
reads back as what it held."
  (nlk:write-file-atomically
   path (with-standard-io-syntax (let ((*print-readably* t)) (prin1-to-string value)))
   :mode #o600 :directory-mode #o700)
  value)

(defun read-value (path)
  "What WRITE-VALUE wrote at PATH, or NIL when it is not there or unreadable."
  (when (probe-file path)
    (ignore-errors (with-standard-io-syntax (with-open-file (in path) (read in))))))
