;;;; cell.lisp --- snapcompact: a transcript as bitmap frames, and /snapcompact.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of vimona3ds/hermes-snapcompact (see NOTICE). The original is a
;;;; Hermes context engine: when the context fills, old turns are serialized
;;;; and rendered into dense PNG frames that a vision model reads back at a
;;;; fraction of the text's token cost, with the newest turns kept verbatim.
;;;; Nodecode evicts history instead of compacting it, so here the same
;;;; rendering is what the model calls (the snapcompact tool) and what the
;;;; operator types (/snapcompact PATH): the text you hand it becomes frames
;;;; you look at, and the answer says which frame holds what.
;;;;
;;;; Everything is local and deterministic: no network, no key, no model
;;;; call. A frame is drawn from the 5x7 font beside this file and written as
;;;; a one-bit PNG, so the only requirement is a vision-capable model to read
;;;; it back.

(in-package #:nodecode-snapcompact)

(defconstant +default-columns+ 100 "Characters across a page when nothing says otherwise.")
(defconstant +default-rows+ 60 "Rows down a page when nothing says otherwise.")
(defconstant +default-scale+ 1 "Pixels a glyph pixel when nothing says otherwise.")

;;; --- the sections's values, read once --------------------------------------------

(defun nonblank (text)
  "TEXT when it holds anything but whitespace, else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, with output_dir expanded and made a folder, and the
geometry the section names as integers."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home
            (or (nonblank (getf values :output-dir))
                (nlk:config-error "output_dir is empty: name the folder frames are written to, e.g. .nodecode/snapcompact in the home folder")))))
    plist))

(defun output-folder ()
  "The folder frames are written under, from the section."
  (uiop:ensure-directory-pathname (setting :output-dir)))

;;; --- one rendering ----------------------------------------------------------------

(defun read-archive (path)
  "The text of the file at PATH, refused when it is not a plain readable file."
  (let ((truename (probe-file (nlk:expand-home path))))
    (unless truename
      (fail "the file ~a does not exist" path))
    (unless (and (pathname-name truename) (null (uiop:directory-pathname-p truename)))
      (fail "~a is a folder, not a file" path))
    (handler-case (uiop:read-file-string truename)
      (error (condition)
        (fail "the file ~a cannot be read as UTF-8 text: ~a" path (nlk:one-line (princ-to-string condition)))))))

(defun frame-folder (label)
  "A fresh folder for one rendering, named for the moment and the label."
  (let* ((stamp (format nil "~d-~6,'0d" (get-universal-time) (random 1000000)))
         (stem (or (nonblank label) "frame"))
         (clean (string-downcase
                 (with-output-to-string (out)
                   (loop for character across stem
                         do (write-char (if (or (alphanumericp character)
                                                (member character '(#\- #\_)))
                                            character #\-)
                                         out)))))
         (folder (merge-pathnames (format nil "~a-~a/" clean stamp) (output-folder))))
    (ensure-directories-exist (merge-pathnames "keep" folder))
    folder))

(defun frame-json (frame)
  (nlk:json-object "path" (getf frame :path)
                   "width" (getf frame :width)
                   "height" (getf frame :height)
                   "chars" (getf frame :chars)))

(defun reading-guide (result label)
  "What the model needs to read RESULT's frames back, in the original's
guide's terms: the grid, the order, the line breaks, the line runs."
  (let* ((frames (getf result :frames))
         (count (length frames))
         (chars (getf result :chars))
         (width (getf result :width))
         (height (getf result :height))
         (head (format nil "~@[~a: ~]~d character~:p of transcript rendered as ~d bitmap frame~:p, ~:d by ~:d pixels~:[~;, in reading order, oldest first~]. Read them before continuing: call (look \"PATH\") on each path below, in order.~%"
                        (nonblank label) (or chars 0) count width height (> count 1))))
    (with-output-to-string (out)
      (write-string head out)
      (loop for frame in frames
            for index from 1
            do (format out "  ~d. ~a~%" index (getf frame :path)))
      (format out "Each frame is a grid ~d characters wide and up to ~d rows tall, read left to right then top to bottom, with no word wrap: a line longer than the grid continues on the next row, and a run of spaces is one space. A word may break across rows; read it from the letters around it.~@[~%~d characters of the oldest text were dropped to stay within max_frames.~]~%"
              (getf result :columns) (getf result :rows) (getf result :dropped-chars))
      (format out "The frames carry the same words as text, so rely on them exactly; when an exact earlier detail matters and a frame is unclear, re-derive it from the workspace rather than guess.~%"))))

(defun render-answer (text &key label columns scale)
  "TEXT rendered into frames under the section's folder => the answer object
the tool sends and the command prints."
  (unless (nonblank text)
    (fail "there is no text to render: give the tool text, or a file holding it"))
  (let* ((columns (or columns (setting :columns) +default-columns+))
         (rows (setting :rows))
         (scale (or scale (setting :scale) +default-scale+))
         (max-frames (setting :max-frames))
         (folder (frame-folder label))
         (result (render-frames (clean-text text)
                                :columns columns :rows rows :scale scale
                                :max-frames max-frames
                                :directory folder
                                :stem (or (nonblank label) "frame"))))
    (when (zerop (length (getf result :frames)))
      (fail "nothing was drawn: the text holds no characters"))
    (nlk:json-object
     "frames" (coerce (mapcar #'frame-json (getf result :frames)) 'vector)
     "frame_count" (length (getf result :frames))
     "columns" (getf result :columns)
     "rows" (getf result :rows)
     "scale" (getf result :scale)
     "width" (getf result :width)
     "height" (getf result :height)
     "chars" (getf result :chars)
     "dropped_chars" (getf result :dropped-chars)
     "dropped_frames" (- (getf result :total-pages) (getf result :pages))
     "text_tokens" (ceiling (getf result :chars) 4)
     "image_tokens" (getf result :image-tokens)
     "guide" (reading-guide result (or (nonblank label) nil)))))

(defmacro answering (&body body)
  "BODY's answer, or the call's failure as text, the way a tool is expected
to refuse: a SNAPCOMPACT-ERROR in the tool's own words, anything else named
as the cell's."
  `(handler-case (progn ,@body)
     (snapcompact-error (condition)
       (nle:failure (format nil "ERROR: ~a" (snapcompact-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: snapcompact: ~a"
                            (nlk:one-line (princ-to-string condition)))))))

;;; --- the tool --------------------------------------------------------------------

(defun snapcompact-tool (request)
  "The snapcompact tool: a text, or a file holding one, as bitmap frames."
  (answering
    (let* ((file (nonblank (snapcompact-request-file request)))
           (text (or (and file (read-archive file))
                     (snapcompact-request-text request)))
           (label (or (nonblank (snapcompact-request-label request))
                      (and file (pathname-name (probe-file (nlk:expand-home file)))))))
      (nlk:encode-json-object
       (render-answer text :label label
                          :columns (snapcompact-request-columns request)
                          :scale (snapcompact-request-scale request))))))

;;; --- /snapcompact ----------------------------------------------------------------

(defun snapcompact-command (args session-id)
  "One /snapcompact invocation => the answer's text, and typed alone the
dialog every command answers then: one row that asks for the text."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Give /snapcompact a file path, or a text, and it renders that into bitmap frames a vision model reads back for a fraction of the tokens."
                (nle:list-dialog
                 :title "Snapcompact"
                 :summary "A transcript as dense bitmap frames"
                 :rows (list (nle:ask-row "Render a file" "File" "/snapcompact"
                                           :placeholder "~/notes/transcript.txt" :verb "render"))))
        (handler-case
            (let* ((file (and (probe-file (nlk:expand-home line)) line))
                   (text (if file (read-archive file) line))
                   (label (and file (pathname-name (probe-file (nlk:expand-home file)))))
                   (answer (render-answer text :label label)))
              (values (format nil "~d frame~:p: ~{~a~^, ~}~@[~%and ~a~]"
                              (nlk:json-value answer :integer "frame_count")
                              (loop for frame across (nlk:json-value answer :array "frames")
                                    collect (nlk:json-value frame :string "path"))
                              (nlk:json-value answer :string "guide"))
                      nil))
          (snapcompact-error (condition)
            (values (format nil "ERROR: ~a" (snapcompact-error-detail condition)) nil))
          (error (condition)
            (values (format nil "ERROR: snapcompact: ~a"
                            (nlk:one-line (princ-to-string condition)))
                    nil))))))

;;; --- from eval -------------------------------------------------------------------

(defun frames-for (text &key label columns rows scale)
  "TEXT as bitmap frames, from eval: the answer object the tool sends.
COLUMNS and SCALE override the section; ROWS is what the section says."
  (declare (ignore rows))
  (render-answer text :label label :columns columns :scale scale))

;;; --- the cell --------------------------------------------------------------------

(defparameter *manual*
  "snapcompact renders a long text into dense bitmap PNG frames that a vision-capable model reads back at a fraction of the tokens the same words cost as text: local, deterministic, no model call.
  snapcompact  {text | file, label, columns, scale}
      Writes one or more PNG frames under the section's output_dir and answers their paths, one grid of characters each, with a reading guide. Then call (look \"PATH\") on every frame in order, oldest first: that is what puts the pages in front of you. Pass file (a path) to render a transcript on disk, or text for words you already hold. A page is columns characters wide and rows tall; a line longer than columns continues on the next row, no word wrap, and a run of spaces is one space.
Write text meant to be read back exactly as it is: whole turns, tool calls with their output, the decisions and file paths a later turn depends on. The frames replace the words, so keep what a reader would need and leave out what the workspace can re-derive. Type /snapcompact PATH for the same from the operator's side."
  "What (help :snapcompact) answers while the cell runs.")

(nlk:define-contract snapcompact-request
  (:doc "Render a text into dense bitmap PNG frames a vision-capable model reads back for a fraction of the tokens, and answer the path of each frame with a reading guide. call (look \"PATH\") on every frame in order, oldest first, before you continue: the frames hold the words themselves, so rely on them exactly, and re-derive from the workspace only when a frame is unclear. Give text, or file (a path to a transcript), not both when the file is what you mean.")
  ("text" :text
   :doc "the words to archive: whole turns, tool calls and their output, the decisions and paths a later turn depends on")
  ("file" :string
   :doc "a path to a text file to render instead of text, a leading tilde for the home folder and a relative path for the workspace")
  ("label" :string
   :doc "a short name for this archive, used in the folder and file names and in the reading guide, e.g. build-log")
  ("columns" :integer
   :doc "characters across a page, twenty at the least, when it should differ from the operator's setting; fewer columns means more frames")
  ("scale" :integer
   :doc "pixels a glyph pixel: 1 is densest and cheapest, 2 or 3 for a model that struggles with small text"))

(nle:define-cell snapcompact
  (:section ("snapcompact")
    (:guide "where frames are written and how large a page is; nothing here needs a key, and a vision-capable model is all the rest")
    ("output_dir" :path :required t
     :doc "the folder frames are written under, one subfolder per call, e.g. .nodecode/snapcompact in the home folder")
    ("columns" :integer :default 100 :min 20
     :doc "characters across a page, twenty at the least: the wider the page, the fewer frames and the more pixels a frame carries")
    ("rows" :integer :default 60 :min 5
     :doc "rows down a page, the tallest one is, five at the least: a short last page is drawn short")
    ("scale" :integer :default 1 :min 1
     :doc "pixels a glyph pixel: 1 is the densest and the cheapest in tokens, 2 or 3 when a model misreads small text")
    ("max_frames" :integer :default 80 :min 1
     :doc "the most frames one rendering writes; past it the oldest pages are dropped and counted in the answer"))
  (:settings #'read-settings)
  (:help :snapcompact
         "snapcompact: a transcript as dense bitmap frames a vision model reads back for a fraction of the tokens"
         *manual*)
  (:tool "snapcompact" (nlk:contract 'snapcompact-request) #'snapcompact-tool)
  (:command "snapcompact" 'snapcompact-command
            :description "Render a file or a text as bitmap frames a vision model reads"
            :argument-hint "FILE or TEXT"))
