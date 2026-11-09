;;;; cell.lisp --- the pad page, the sketch it saves, the two tools and /sketch.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The operator draws in a browser tab this cell serves, presses Save, and
;;;; the PNG is kept under the profile's sketches folder; the model then reads
;;;; the path from sketch_pad_last and calls (look PATH) to see it. That is
;;;; aydnOktay's Hermes sketch pad with the desktop pane replaced by a page:
;;;; the drawing surface was the only part of it tied to Hermes Desktop.

(in-package #:nodecode-sketch-pad)

(defparameter +tool-last+ "sketch_pad_last"
  "The tool's name, the original plugin's, so a prompt written for it reads right.")

(defparameter +tool-save+ "sketch_pad_save"
  "The save the desktop pane used to POST to its dashboard route, now a tool as well.")

(defparameter +primer+
  "The sketch pad is a browser tab the operator draws in: nodecode-sketch-pad serves it on
127.0.0.1 and /sketch names the address. They draw with the mouse or a finger, press Save,
and the PNG is kept under this profile's sketches folder -- sketch-<stamp>.png, one per
drawing, and last.png, the newest, replaced atomically.
  sketch_pad_last()  the newest drawing's path, bytes, size and time. Call it when the
      operator says \"the sketch\", \"what did I draw\", \"this shape\", or asks you to look
      again later: then (look PATH) -- PNG is one of the formats look reads. The path stays
      good after the turn ends, unlike an image already in the conversation.
  sketch_pad_save(png_base64)  keep a base64 PNG as a drawing, the same way the page's Save
      does: for a screenshot or an image another tool produced that the operator will talk
      about afterwards. It answers the saved path.
A drawing is only what its pixels show: never describe one you have not looked at, and never
invent detail from the operator's words. The pad is local -- no server is started for it
beyond this cell's own, and nothing is fetched from localhost.
"
  "What (help :sketch) answers while the cell runs.")

;;; --- the tool boundaries -------------------------------------------------------

(nlk:define-contract sketch-request
  (:doc "Keep one drawing: a base64 PNG, exactly as a canvas toDataURL('image/png') gives it or with its data:image/png;base64, prefix. It is saved under this profile's sketches folder beside the pad's own drawings and becomes the newest one, so sketch_pad_last names it afterwards. At most sketch-pad.max_bytes of PNG (800 KB unless the section says otherwise); anything that is not a PNG is refused.")
  ("png_base64" :text :required t
                 :doc "the drawing, base64 PNG; a data:image/png;base64, prefix is accepted"))

(defparameter +last-schema+
  (list :description "The newest sketch-pad drawing as a file: its path, its size in bytes, its width and height, and when it was saved. Call it when the operator points at a sketch, then (look PATH) to see it -- the path stays readable after this turn, so look again in a later turn when they ask about the same drawing. Answers \"no sketch yet\" before the first Save, with the pad's address."
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:make-json-object))))

;;; --- the tool bodies -----------------------------------------------------------

(defun run-last (arguments)
  "sketch_pad_last: where the newest drawing is."
  (declare (ignore arguments))
  (handler-case (last-sketch-text)
    (sketch-pad-error (condition)
      (nle:failure (format nil "ERROR: ~a" (sketch-pad-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

(defun run-save (request)
  "sketch_pad_save: keep a base64 PNG as a drawing."
  (handler-case
      (let ((info (save-sketch (decode-base64 (sketch-request-png-base64 request)))))
        (format nil "saved ~a~%(look ~s) shows it to you."
                (sketch-line info) (getf info :path)))
    (sketch-pad-error (condition)
      (nle:failure (format nil "ERROR: ~a" (sketch-pad-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /sketch -------------------------------------------------------------------

(defun run-slash (args session-id)
  "The pad's address, and what the newest drawing is."
  (declare (ignore session-id))
  (handler-case
      (let ((argument (string-downcase (nlk:trimmed (or args "")))))
        (cond
          ((string= argument "last")
           (last-sketch-text))
          ((zerop (length argument))
           (format nil "Sketch pad: ~a -- draw with the mouse, press Save, then ask about the shape.~%~a"
                   (pad-url) (last-sketch-text)))
          (t
           (format nil "Sketch pad: ~a (open it in a browser tab). /sketch last names the newest drawing.~%~a"
                   (pad-url) (last-sketch-text)))))
    (sketch-pad-error (condition)
      (format nil "ERROR: ~a" (sketch-pad-error-detail condition)))
    (error (condition)
      (format nil "ERROR: ~a" condition))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell sketch-pad
  (:section ("sketch-pad")
    (:guide "nothing here is required: the pad serves itself on 127.0.0.1 and keeps its drawings in the home; port is where it answers, max_bytes how large a PNG it takes, and directory where the drawings go")
    ("port" :integer :default 17319 :min 0
            :doc "the port the pad page answers on, on 127.0.0.1; 0 lets the system pick one, and a port already taken falls back to one it picks, with a notice")
    ("max_bytes" :integer :default 800000 :min 1024
                  :doc "the largest PNG a save takes, in bytes; 800 KB is a large drawing")
    ("directory" :directory :default "sketches/"
                 :doc "where the drawings and last.png are kept, under the home"))
  (:tool "sketch_pad_save" (nlk:contract 'sketch-request) 'run-save)
  (:tool "sketch_pad_last" +last-schema+ 'run-last)
  (:help :sketch "sketch: the operator draws in a browser tab and the PNG is saved where you can look at it"
         +primer+)
  (:command "sketch" 'run-slash
            :description "The sketch pad's address, and the newest drawing"
            :argument-hint "[last]")
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first
            ;; start, even before the first Save.
            (ensure-directories-exist (merge-pathnames "last.png" (sketches-directory)))
            (start-pad))))
