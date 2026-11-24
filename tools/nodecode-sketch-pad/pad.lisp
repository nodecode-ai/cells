;;;; pad.lisp --- the page the operator draws on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The drawing surface is a browser tab, not a desktop pane: the same
;;;; canvas, the same pen and Clear and Save, served by this cell on
;;;; 127.0.0.1. pad.html is the page itself, read once at the first request
;;;; and kept; nothing in it reaches the network beyond the save POST to this
;;;; process.

(in-package #:nodecode-sketch-pad)

(defparameter *page* nil
  "pad.html as one string, once it has been read.")

(defun page-html ()
  "The pad page."
  (or *page*
      (setf *page* (uiop:read-file-string
                    (asdf:system-relative-pathname "nodecode-sketch-pad" "pad.html")))))
