;;;; render.lisp --- the plan drawn, read back, and captioned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's render.py and verify.py, in one file: bars start at zero,
;;;; a share band is 100% wide, a line keeps its dates in order, every bar
;;;; carries its number, and the source is printed under the chart. What the
;;;; rewrite keeps of the original's promise is the read-back: every bar's
;;;; drawn length and every printed number are compared with the input before
;;;; the file is answered, and a chart that does not match is not returned.

(in-package #:nodecode-charts)

(defparameter +palette+
  '((0 114 178) (230 159 0) (0 158 115) (204 121 167)
    (86 180 233) (213 94 0) (240 228 66) (153 153 153))
  "Okabe and Ito's color-blind-safe eight, the original's own palette.")

(defparameter +grey+ '(187 187 187) "A bar the highlight left out.")
(defparameter +ink+ '(30 30 30) "Text and axes.")
(defparameter +muted+ '(110 110 110) "The source line and the notes.")
(defparameter +rule+ '(210 210 210) "A baseline, and a legend's box.")

(defstruct drawing
  (plan nil) (canvas nil) (path "") (width 0) (height 0)
  (labels nil) (bars nil) (hidden nil) (font "5x7 dots"))

(defun palette-color (index)
  (nth (mod index (length +palette+)) +palette+))

(defun make-labeler (plan)
  "A function of one value answering the text a bar's number is printed as on
the picture: grouped, rounded, and ASCII, because the canvas sets its own
five-by-seven face and a 万 or an 億 would set as a box. The caption written
under the chart carries the compact form the table's language uses."
  (let ((decimals (plan-decimals plan)))
    (lambda (value) (string-right-trim "." (group-number value decimals)))))

(defun note-label (drawing text value &key (kind "number"))
  "A printed number, kept so the verifier can read it back."
  (push (list text value kind) (drawing-labels drawing))
  drawing)

(defun note-bar (drawing series category length expected kind)
  "A bar's drawn length and the length its value asks for, kept so the
verifier can compare the two."
  (push (list series category length expected kind) (drawing-bars drawing))
  drawing)

;;; --- the frame -------------------------------------------------------------------

(defun scale-of (width) (max 2 (floor width 330)))

(defun draw-head (drawing)
  "The title, the subtitle and the series legend; => the y the plot starts at."
  (let* ((canvas (canvas-of drawing))
         (plan (plan-of drawing))
         (scale (scale-of (drawing-width drawing)))
         (margin (floor (/ (drawing-width drawing) 22)))
         (y margin))
    (when (plusp (length (plan-title plan)))
      (draw-text canvas margin y (plan-title plan) +ink+ :scale (1+ scale))
      (incf y (+ (text-height :scale (1+ scale)) scale)))
    (when (plusp (length (plan-subtitle plan)))
      (draw-text canvas margin y (plan-subtitle plan) +muted+ :scale scale)
      (incf y (+ (text-height :scale scale) scale)))
    (incf y scale)
    (when (> (length (plan-series plan)) 1)
      (let ((x margin))
        (loop for s across (plan-series plan)
              for index from 0
              do (let ((label (cdr (assoc s (plan-series-labels plan) :test #'string=))))
                   (fill-rect canvas x (+ y 2) (* 3 scale) (* 3 scale) (palette-color index))
                   (draw-text canvas (+ x (* 4 scale)) y label +ink+ :scale scale)
                   (incf x (+ (text-width label :scale scale) (* 8 scale))))))
      (incf y (* 9 scale)))
    (incf y scale)
    y))

(defun footer-height (drawing)
  (let ((scale (max 2 (1- (scale-of (drawing-width drawing)))))
        (plan (plan-of drawing)))
    (+ (* (+ 1 (length (plan-notes plan))) (+ (text-height :scale scale) scale))
       (floor (/ (drawing-width drawing) 22)))))

(defun plan-of (drawing) (drawing-plan drawing))
(defun canvas-of (drawing) (drawing-canvas drawing))

(defun draw-footer (drawing)
  "The source line, then every note, under the plot."
  (let* ((canvas (canvas-of drawing))
         (plan (plan-of drawing))
         (scale (max 2 (1- (scale-of (drawing-width drawing)))))
         (margin (floor (/ (drawing-width drawing) 22)))
         (step (+ (text-height :scale scale) scale))
         (y (- (drawing-height drawing) (footer-height drawing))))
    (draw-line canvas margin (1- y) (- (drawing-width drawing) margin) (1- y) +rule+)
    (incf y scale)
    (draw-text canvas margin y (plan-source plan) +muted+ :scale scale)
    (incf y step)
    (dolist (note (plan-notes plan))
      (draw-text canvas margin y note +muted+ :scale scale)
      (incf y step))))

;;; --- horizontal bars ---------------------------------------------------------------

(defun draw-horizontal (drawing)
  "Bars that grow to the right from a label column, sorted as the plan has them."
  (let* ((canvas (canvas-of drawing))
         (plan (plan-of drawing))
         (scale (scale-of (drawing-width drawing)))
         (margin (floor (/ (drawing-width drawing) 22)))
         (labeler (make-labeler plan))
         (categories (plan-categories plan))
         (series (plan-series plan))
         (label-width (min (floor (* (drawing-width drawing) 2) 5)
                           (loop for c across categories maximize (text-width (string c) :scale scale))))
         (numbers (loop for s across series append (remove nil (gethash s (plan-values plan)))))
         (peak (max 1d-9 (loop for v in numbers maximize (abs (coerce v 'double-float)))))
         (top (draw-head drawing))
         (bottom (- (drawing-height drawing) (footer-height drawing) (* 3 scale)))
         (x0 (+ margin label-width (* 4 scale)))
         (value-room (loop for v in numbers maximize (text-width (funcall labeler v) :scale scale)))
         (span (max 1 (- (drawing-width drawing) margin x0 value-room (* 4 scale))))
         (rows (length categories))
         (row-height (floor (- bottom top) (max 1 rows)))
         (bar-height (max 2 (floor (* row-height (if (> (length series) 1) 0.7 0.5)) (length series)))))
    (loop for i below rows
          for category = (aref categories i)
          do (let ((y (+ top (* i row-height))))
               ;; the category label, right-aligned against the plot
               (draw-text canvas (- x0 (* 2 scale)) (+ y (floor (- row-height (text-height :scale scale)) 2))
                          (string category) +ink+ :scale scale :align "right")
               (loop for s across series
                     for index from 0
                     for value = (nth i (gethash s (plan-values plan)))
                     for bar-y = (+ y (floor (- row-height (* bar-height (length series))) 2) (* index bar-height))
                     do (let* ((length (if value (round (* span (/ (abs (coerce value 'double-float)) peak))) 0))
                               (color (if (and (plan-highlight plan)
                                               (not (member (string category) (plan-highlight plan) :test #'string=)))
                                          +grey+
                                          (palette-color index))))
                          (when (plusp length)
                            (fill-rect canvas x0 bar-y length (max 1 (- bar-height 2)) color))
                          (when value
                            (note-bar drawing s (string category) length (round (* span (/ (abs (coerce value (quote double-float))) peak))) "bar-h")
                            (let ((text (funcall labeler value)))
                              (if (<= (+ x0 length (* 2 scale) (text-width text :scale scale))
                                      (- (drawing-width drawing) margin))
                                  (progn
                                    (draw-text canvas (+ x0 length (* 2 scale))
                                               (+ bar-y (max 0 (floor (- bar-height (text-height :scale scale)) 2)))
                                               text +muted+ :scale scale)
                                    (note-label drawing text value :kind "value"))
                                  (push (list s (string category) text) (drawing-hidden drawing)))))))))
    (draw-line canvas x0 (1- top) x0 (1+ bottom) +rule+))
  drawing)

;;; --- vertical bars, stacked and not -------------------------------------------------

(defun draw-vertical (drawing &key stacked)
  "Bars that grow up from a baseline; STACKED puts the series on top of each
other, so the total and its parts are both read off one bar."
  (let* ((canvas (canvas-of drawing))
         (plan (plan-of drawing))
         (scale (scale-of (drawing-width drawing)))
         (margin (floor (/ (drawing-width drawing) 22)))
         (labeler (make-labeler plan))
         (categories (plan-categories plan))
         (series (plan-series plan))
         (top (draw-head drawing))
         (bottom (- (drawing-height drawing) (footer-height drawing) (* 8 scale)))
         (peaks (if stacked
                    (loop for i below (length categories)
                          maximize (loop for s across series sum (or (nth i (gethash s (plan-values plan))) 0d0)))
                    (loop for s across series
                          maximize (loop for v in (gethash s (plan-values plan))
                                         when v maximize (abs (coerce v 'double-float)) else maximize 0d0))))
         (peak (max 1d-9 peaks))
         (x0 (+ margin (text-width (funcall labeler peak) :scale scale) (* 4 scale)))
         (span (max 1 (- (drawing-width drawing) margin x0)))
         (column (floor span (max 1 (length categories))))
         (bar-width (max 2 (floor (* column (if stacked 0.6 0.7)) (length series))))
         (height (max 1 (- bottom top))))
    (draw-line canvas x0 top x0 bottom +rule+)
    (draw-line canvas x0 bottom (drawing-width drawing) bottom +rule+)
    ;; the value axis' own marks, at the peak and the middle
    (loop for fraction in '(0d0 0.5d0 1d0)
          do (let* ((value (* peak fraction))
                    (y (- bottom (round (* height fraction)))))
               (draw-line canvas x0 y (drawing-width drawing) y +rule+)
               (let ((text (funcall labeler value)))
                 (draw-text canvas (- x0 (* scale 2)) y text +muted+ :scale scale :align "right"))))
    (loop for i below (length categories)
          for category = (aref categories i)
          for column-x = (+ x0 (* i column))
          do (let ((bar-x (+ column-x (floor (- column (* bar-width (if stacked 1 (length series)))) 2)))
                   (accumulated 0d0))
               (loop for s across series
                     for index from 0
                     for value = (nth i (gethash s (plan-values plan)))
                     do (cond
                          (stacked
                           (let* ((size (if value (round (* height (/ (coerce value 'double-float) peak))) 0))
                                  (bar-y (- bottom accumulated size)))
                             (when (plusp size)
                               (fill-rect canvas bar-x bar-y bar-width size (palette-color index)))
                             (when value
                               (note-bar drawing s (string category) size (round (* height (/ (abs (coerce value (quote double-float))) peak))) "bar-v"))
                             (incf accumulated size)))
                          (t
                           (let* ((size (if value (round (* height (/ (abs (coerce value 'double-float)) peak))) 0))
                                  (bar-y (- bottom size))
                                  (bar-x (+ column-x (* index bar-width) (floor (- column (* bar-width (length series))) 2))))
                             (when (plusp size)
                               (fill-rect canvas bar-x bar-y (max 1 (1- bar-width)) size (palette-color index)))
                             (when value
                               (note-bar drawing s (string category) size (round (* height (/ (abs (coerce value (quote double-float))) peak))) "bar-v")
                               (let ((text (funcall labeler value)))
                                 (if (<= (text-width text :scale scale) column)
                                     (progn
                                       (draw-text canvas (+ bar-x (floor bar-width 2)) (- bar-y (text-height :scale scale) scale)
                                                  text +muted+ :scale scale :align "center")
                                       (note-label drawing text value :kind "value"))
                                     (push (list s (string category) text) (drawing-hidden drawing)))))))))
               (draw-text canvas (+ column-x (floor column 2)) (+ bottom (* 2 scale)) (string category)
                          +ink+ :scale scale :align "center")))
    (when (plusp (length (plan-y-title plan)))
      (draw-text canvas x0 top (plan-y-title plan) +muted+ :scale scale))
    (draw-text canvas (+ x0 (floor span 2)) (- (drawing-height drawing) (footer-height drawing) (* 3 scale))
               (concatenate 'string (plan-x-title plan) " (" (plan-unit plan) ")") +muted+ :scale scale :align "center"))
  drawing)

;;; --- the share band ----------------------------------------------------------------

(defun draw-share (drawing)
  "One 100% band a row, each part's own percentage printed inside it when it fits."
  (let* ((canvas (canvas-of drawing))
         (plan (plan-of drawing))
         (scale (scale-of (drawing-width drawing)))
         (margin (floor (/ (drawing-width drawing) 22)))
         (categories (plan-categories plan))
         (series (plan-series plan))
         (label-width (min (floor (* (drawing-width drawing) 1) 4)
                           (loop for c across categories maximize (text-width (string c) :scale scale))))
         (top (draw-head drawing))
         (bottom (- (drawing-height drawing) (footer-height drawing) scale))
         (x0 (+ margin label-width (* 3 scale)))
         (span (max 1 (- (drawing-width drawing) margin x0)))
         (row-height (floor (- bottom top) (max 1 (length categories))))
         (band (max 3 (floor (* row-height 0.55)))))
    (loop for i below (length categories)
          for category = (aref categories i)
          for parts = (loop for s across series collect (or (nth i (gethash s (plan-values plan))) 0d0))
          for total = (max 1d-9 (reduce #'+ parts))
          for y = (+ top (* i row-height))
          for x = x0
          do (draw-text canvas (- x0 (* 2 scale)) (+ y (floor (- row-height (text-height :scale scale)) 2))
                        (string category) +ink+ :scale scale :align "right")
             (loop for s across series
                   for index from 0
                   for value in parts
                   for share = (/ (coerce value 'double-float) total)
                   for size = (round (* span share))
                   do (fill-rect canvas x (+ y (floor (- row-height band) 2)) size band
                                  (palette-color index))
                      (let ((text (format nil "~d%" (round (* share 100)))))
                        (when (and (>= size (* 3 (text-width text :scale scale)))
                                   (<= (text-width text :scale scale) size))
                          (draw-text canvas (+ x (floor (- size (text-width text :scale scale)) 2))
                                     (+ y (floor (- row-height (text-height :scale scale)) 2))
                                     text '(255 255 255) :scale scale)
                          (note-label drawing text (* share 100) :kind "share")))
                      (note-bar drawing s (string category) size (round (* span share)) "share")
                      (incf x size)))
    (let ((y (+ top (* (length categories) row-height) scale)))
      (draw-text canvas x0 y (if (string= (plan-locale plan) "ja")
                                  "構成比（%）"
                                  "share (%)")
                 +muted+ :scale scale))
    (draw-line canvas x0 (1- top) x0 (1+ bottom) +rule+))
  drawing)

;;; --- the line -----------------------------------------------------------------------

(defun draw-line-chart (drawing)
  "Points joined in time order; only the series' ends and the value axis are written."
  (let* ((canvas (canvas-of drawing))
         (plan (plan-of drawing))
         (scale (scale-of (drawing-width drawing)))
         (margin (floor (/ (drawing-width drawing) 22)))
         (labeler (make-labeler plan))
         (categories (plan-categories plan))
         (series (plan-series plan))
         (numbers (loop for s across series append (remove nil (gethash s (plan-values plan)))))
         (peak (max 1d-9 (loop for v in numbers maximize (abs (coerce v 'double-float)))))
         (top (+ (draw-head drawing) (* 2 scale)))
         (bottom (- (drawing-height drawing) (footer-height drawing) (* 3 scale)))
         (axis-room (text-width (funcall labeler peak) :scale scale))
         (x0 (+ margin axis-room (* 2 scale)))
         (span (max 1 (- (drawing-width drawing) margin x0)))
         (height (max 1 (- bottom top)))
         (step (if (> (length categories) 1) (/ (coerce span 'double-float) (1- (length categories))) 0d0)))
    (draw-line canvas x0 top x0 bottom +rule+)
    (draw-line canvas x0 bottom (drawing-width drawing) bottom +rule+)
    (loop for fraction in '(0d0 0.5d0 1d0)
          do (let* ((value (* peak fraction))
                    (y (- bottom (round (* height fraction)))))
               (draw-line canvas x0 y (drawing-width drawing) y +rule+)
               (draw-text canvas (- x0 (* scale 2)) y (funcall labeler value) +muted+ :scale scale :align "right")))
    (loop for s across series
          for index from 0
          do (let ((points nil))
               (loop for value in (gethash s (plan-values plan))
                     for i from 0
                     when value
                       do (push (cons (round (+ x0 (* i step)))
                                      (- bottom (round (* height (/ (abs (coerce value 'double-float)) peak)))))
                                points))
               (setf points (nreverse points))
               (loop for (a b) on points
                     while b
                     do (draw-line canvas (car a) (cdr a) (car b) (cdr b) (palette-color index) (1+ scale)))
               (dolist (point points)
                 (fill-rect canvas (- (car point) scale) (- (cdr point) scale) (* 2 scale) (* 2 scale)
                            (palette-color index)))
               ;; the series' name at its last point, and the value there
               (when points
                 (let* ((last (car (last points)))
                        (value (car (last (remove nil (gethash s (plan-values plan))))))
                        (text (funcall labeler (or value 0))))
                   (let ((label-x (+ (car last) (* 2 scale)))
                         (align (if (> (+ (car last) (* 2 scale)
                                          (text-width (cdr (assoc s (plan-series-labels plan)
                                                                  :test #'string=)) :scale scale))
                                     (- (drawing-width drawing) margin))
                                  "right" "left")))
                     (draw-text canvas (if (string= align "right") (- label-x (* 4 scale)) label-x)
                                (+ (cdr last) (* 8 scale) (* index (* 9 scale)))
                                (cdr (assoc s (plan-series-labels plan) :test #'string=))
                                +ink+ :scale scale :align align)
                     (draw-text canvas (if (string= align "right") (- label-x (* 4 scale)) label-x)
                                (+ (cdr last) (* 8 scale) (* (1+ index) (* 9 scale)))
                                text +muted+ :scale scale :align align))
                   (note-label drawing text (or value 0) :kind "value")))))
    (loop for i below (length categories)
          for at = (round (+ x0 (* i step)))
          do (draw-text canvas at (+ bottom (* 2 scale)) (string (aref categories i))
                        +ink+ :scale scale
                        :align (cond ((zerop i) "left")
                                     ((= i (1- (length categories))) "right")
                                     (t "center"))))
    (when (plusp (length (plan-y-title plan)))
      (draw-text canvas x0 top (plan-y-title plan) +muted+ :scale scale)))
  drawing)

;;; --- the file, the check and the caption ----------------------------------------------

(defun output-dir ()
  "Where a chart is written: under the agent's own home, a folder of its own."
  (let ((home (uiop:getenv "NODECODE_HOME")))
    (merge-pathnames (make-pathname :directory '(:relative "charts"))
                      (if (and home (plusp (length home)))
                          (uiop:ensure-directory-pathname home)
                          (merge-pathnames (make-pathname :directory '(:relative ".nodecode"))
                                           (user-homedir-pathname))))))

(defun png-path (title &key directory filename)
  "Where a chart titled TITLE is written; the file name is the title, read
down to letters, digits, a dash and a dot."
  (let* ((stamp (multiple-value-bind (second minute hour day month year)
                      (get-decoded-time)
                    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))
         (digest (format nil "~(~x~)" (logand #xffffffff (sxhash (format nil "~a~a" title (get-universal-time))))))
         (base (string-downcase
                (with-output-to-string (out)
                  (loop for character across (string (or filename ""))
                        do (if (or (alphanumericp character) (find character "._-" :test #'char=))
                               (write-char character out)
                               (write-char #\- out))))))
         (base (string-trim "-." (if (and (>= (length base) 4) (string-suffix-p ".png" base))
                                     (subseq base 0 (- (length base) 4))
                                     base)))
         (name (if (plusp (length base)) (format nil "~a-~a.png" (subseq base 0 (min 60 (length base))) digest)
                   (format nil "chart-~a-~a.png" stamp digest)))
         (directory (or directory (output-dir))))
    (merge-pathnames name (uiop:ensure-directory-pathname directory))))

(defun render-plan (plan path)
  "PLAN drawn into PATH, and the DRAWING recorded for the check."
  (let* ((canvas (make-canvas (plan-width plan) (plan-height plan)))
         (drawing (make-drawing :plan plan :canvas canvas :path (namestring path)
                                 :width (plan-width plan) :height (plan-height plan))))
    (cond ((string= (plan-chart-type plan) "bar")
           (if (string= (plan-orientation plan) "horizontal")
               (draw-horizontal drawing)
               (draw-vertical drawing)))
          ((string= (plan-chart-type plan) "stacked_bar") (draw-vertical drawing :stacked t))
          ((string= (plan-chart-type plan) "share_bar") (draw-share drawing))
          ((string= (plan-chart-type plan) "line") (draw-line-chart drawing))
          (t (draw-vertical drawing)))
    (draw-footer drawing)
    (write-png canvas path)
    (setf (drawing-labels drawing) (nreverse (drawing-labels drawing))
          (drawing-bars drawing) (nreverse (drawing-bars drawing)))
    drawing))

(defun verify-drawing (drawing)
  "Every bar's drawn length and every printed number, read back against the input.
=> a plist with :status and, when it is not \"ok\", :mismatches."
  (let ((mismatches nil) (checked 0))
    (dolist (bar (drawing-bars drawing))
      (destructuring-bind (series category length expected kind) bar
        (declare (ignore kind))
        (incf checked)
        (when (> (abs (- expected length)) 2)
          (push (format nil "~a / ~a: drawn ~d px, expected ~d px" series category length expected)
                mismatches))))
    (dolist (label (drawing-labels drawing))
      (destructuring-bind (text value kind) label
        (declare (ignore kind))
        (incf checked)
        (let ((read-back (parse-label text)))
          (cond ((null read-back)
                 (push (format nil "the number ~s could not be read back" text) mismatches))
                ((> (abs (- read-back (coerce value 'double-float)))
                    (max 0.51d0 (* 1d-2 (abs (coerce value 'double-float)))))
                 (push (format nil "printed ~s, which is ~,3f, for a value of ~,3f" text read-back value)
                       mismatches))))))
    (if mismatches
        (list :status "mismatch" :checked checked :mismatches (subseq mismatches 0 (min 5 (length mismatches))))
        (list :status "ok" :checked checked))))

(defun alt-text (drawing)
  "A caption that says what the chart shows, in the same rounding the chart prints."
  (let* ((plan (plan-of drawing))
         (ja (string= (plan-locale plan) "ja"))
         (labeler (lambda (value) (format-value value (plan-number-style plan)
                                                 (plan-decimals plan))))
         (categories (plan-categories plan))
         (series (plan-series plan))
         (kind (cond ((string= (plan-chart-type plan) "line") (if ja "折れ線グラフ" "Line chart"))
                     ((string= (plan-chart-type plan) "stacked_bar") (if ja "積み上げ棒グラフ" "Stacked bar chart"))
                     ((string= (plan-chart-type plan) "share_bar") (if ja "構成比の帯グラフ" "100% band chart"))
                     ((string= (plan-orientation plan) "horizontal") (if ja "横棒グラフ" "Bar chart"))
                     (t (if ja "棒グラフ" "Bar chart"))))
         (parts nil))
    (cond
      ((string= (plan-chart-type plan) "share_bar")
       (loop for i below (length categories)
             for total = (max 1d-9 (loop for s across series sum (or (nth i (gethash s (plan-values plan))) 0d0)))
             do (push (format nil "~@[~a: ~]~{~a~^, ~}" (string (aref categories i))
                               (loop for s across series
                                     collect (format nil "~a ~d%" (cdr (assoc s (plan-series-labels plan) :test #'string=))
                                                     (round (* 100 (/ (or (nth i (gethash s (plan-values plan))) 0d0) total))))))
                      parts)))
      ((string= (plan-chart-type plan) "line")
       (loop for s across series
             for values = (gethash s (plan-values plan))
             do (let ((first-at (position-if #'identity values))
                      (last-at (position-if #'identity values :from-end t)))
                  (when first-at
                    (push (format nil "~a: ~a to ~a" (cdr (assoc s (plan-series-labels plan) :test #'string=))
                                  (funcall labeler (nth first-at values)) (funcall labeler (nth last-at values)))
                          parts)))))
      (t
       (let* ((totals (loop for i below (length categories)
                             collect (cons (aref categories i)
                                           (loop for s across series
                                                 sum (or (nth i (gethash s (plan-values plan))) 0d0)))))
              (high (reduce (lambda (a b) (if (>= (cdr a) (cdr b)) a b)) totals))
              (low (reduce (lambda (a b) (if (<= (cdr a) (cdr b)) a b)) totals)))
         (push (format nil "~@[~a ~]~a ~a" (if (and ja (> (length series) 1)) "合計" "")
                       (if ja "最大" "largest") (string (car high)))
               parts)
         (push (format nil "~a ~a" (if ja "最小" "smallest") (string (car low))) parts)
         (push (format nil "~a" (funcall labeler (cdr high))) parts))))
    (format nil (if (string= (plan-locale plan) "ja")
                     "~a: ~a~@[ (~a)~]。~{~a~^、~}。~a ~a"
                     "~a: ~a~@[ (~a)~]. ~{~a~^; ~}. ~a ~a")
            kind (plan-title plan)
            (if (plusp (length (plan-unit plan))) (plan-unit plan) nil)
            (mapcar (lambda (part) (string-right-trim "." part)) (reverse parts))
            (if ja "出典:" "Source:")
            (plan-source plan))))

(defun drawing-size (drawing)
  (list (drawing-width drawing) (drawing-height drawing)))
