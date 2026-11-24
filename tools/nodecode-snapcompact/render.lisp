;;;; render.lisp --- text to a grid of glyphs, a page at a time, as PNG files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original rendered through a native library reached over a Bun
;;;; bridge. The grid is simple enough to draw here: a page is COLUMNS
;;;; characters wide and ROWS tall, drawn at SCALE pixels a glyph pixel, so
;;;; one page carries COLUMNS x ROWS characters in one image. A line longer
;;;; than COLUMNS continues on the next row with no word wrap, exactly as the
;;;; original's reading guide tells the model to expect; a tab is a space and
;;;; a run of spaces is one, which is the guide's other rule.

(in-package #:nodecode-snapcompact)

(defconstant +margin+ 8 "Blank pixels kept around a page's grid.")
(defconstant +glyph-width+ 5 "Columns in one glyph.")
(defconstant +glyph-height+ 7 "Rows in one glyph.")
(defconstant +advance+ 6 "Pixels a glyph takes horizontally at scale 1: five and a gap.")
(defconstant +pitch+ 8 "Pixels a line takes vertically at scale 1: seven and a gap.")

(defun clean-text (text &key (collapse t))
  "TEXT as a page draws it: a carriage return dropped, a tab a space, and
with COLLAPSE a run of spaces one space."
  (let ((out (make-string-output-stream)))
    (loop with last-space = nil
          for character across text
          do (case character
               (#\Return nil)
               (#\Tab (unless (and collapse last-space) (write-char #\Space out))
                      (setf last-space t))
               (#\Space (unless (and collapse last-space) (write-char #\Space out))
                        (setf last-space t))
               ((#\Newline)
                (write-char #\Newline out)
                (setf last-space nil))
               (t (write-char character out)
                  (setf last-space nil))))
    (get-output-stream-string out)))

(defun text-rows (text columns)
  "TEXT as the rows a page draws: each line split every COLUMNS characters,
with no word wrap. => a list of strings, none longer than COLUMNS."
  (let ((rows '())
        (line (make-string-output-stream)))
    (labels ((flush-line ()
               (let ((text (get-output-stream-string line)))
                 (if (zerop (length text))
                     (push "" rows)
                     (loop with start = 0
                           while (< start (length text))
                           do (push (subseq text start (min (length text) (+ start columns)))
                                    rows)
                              (incf start columns))))))
      (loop for character across text
            do (if (char= character #\Newline)
                   (flush-line)
                   (write-char character line)))
      (flush-line))
    (nreverse rows)))

(defun paginate (rows per-page)
  "ROWS in pages of PER-PAGE rows, the last one short."
  (loop with pages = '()
        with page = '()
        for row in rows
        do (push row page)
           (when (= (length page) per-page)
             (push (nreverse page) pages)
             (setf page '()))
        finally (progn (when page (push (nreverse page) pages))
                       (return (nreverse pages)))))

(defun page-bitmap (rows columns scale)
  "One frame: ROWS drawn COLUMNS wide, SCALE pixels a glyph pixel, on the
blank page it sits in."
  (let* ((width (+ (* 2 +margin+) (* columns +advance+ scale)))
         (height (+ (* 2 +margin+) (* (max 1 (length rows)) +pitch+ scale)))
         (bitmap (make-blank-bitmap width height)))
    (loop for row in rows
          for line from 0
          do (loop for column from 0 below (min (length row) columns)
                   for character = (char row column)
                   for rows-of-glyph = (glyph-rows character)
                   do (ink-glyph bitmap rows-of-glyph
                                 (+ +margin+ (* column +advance+ scale))
                                 (+ +margin+ (* line +pitch+ scale))
                                 scale)))
    bitmap))

(defun frame-bytes (columns rows scale)
  "One frame's width and height in pixels, without drawing it."
  (values (+ (* 2 +margin+) (* columns +advance+ scale))
          (+ (* 2 +margin+) (* rows +pitch+ scale))))

(defun image-tokens (width height)
  "A frame's cost, roughly, as a vision model bills it: a pixel is 1/750 of
a token."
  (ceiling (* width height) 750))

(defun render-frames (text &key columns rows (scale 2) max-frames directory stem)
  "TEXT as PNG frames under DIRECTORY, named STEM-01.png and up. Pages past
MAX-FRAMES are dropped from the front, the oldest, and counted. => a plist:
:frames, each (:path :width :height :rows :chars), and the totals."
  (let* ((rows-list (text-rows text columns))
         (pages (paginate rows-list rows))
         (dropped (max 0 (- (length pages) max-frames)))
         (kept (nthcdr dropped pages)))
    (let ((frames '())
          (chars 0)
          (dropped-chars 0)
          (tokens 0))
      (loop for page in (subseq pages 0 dropped)
            do (incf dropped-chars (reduce #'+ page :key #'length :initial-value 0)))
      (loop for page in kept
            for index from 1
            for path = (merge-pathnames (format nil "~a-~2,'0d.png" stem index) directory)
            for page-chars = (reduce #'+ page :key #'length :initial-value 0)
            do (multiple-value-bind (width height)
                     (frame-bytes columns (max 1 (length page)) scale)
                 (write-png (page-bitmap page columns scale) path)
                 (incf chars page-chars)
                 (incf tokens (image-tokens width height))
                 (push (list :path (namestring path) :width width :height height
                             :rows (length page) :chars page-chars)
                       frames)))
      (multiple-value-bind (width height) (frame-bytes columns rows scale)
        (list :frames (nreverse frames)
              :columns columns :rows rows :scale scale
              :width width :height height
              :pages (length kept) :total-pages (length pages)
              :chars chars :dropped-chars dropped-chars
              :image-tokens tokens)))))
