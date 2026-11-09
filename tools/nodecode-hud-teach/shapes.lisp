;;;; shapes.lisp --- what a draw call may paint, checked before anything is on screen.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A draw is refused in whole rather than half-painted: every shape is
;;;; checked against its kind, its colour, its step number, its caption and the
;;;; capture it claims -- and every point of it against that capture's frame --
;;;; before the overlay script is written. Coordinates are pixels of the
;;;; screenshot, (0,0) at its top left, which is what the model saw in (look).

(in-package #:nodecode-hud-teach)

(defun shape-number (shape key)
  "SHAPE's KEY as a number, or NIL."
  (nlk:json-value shape :number key))

(defun shape-string (shape key)
  "SHAPE's KEY as a string, or NIL."
  (nlk:json-value shape :string key))

(defun shape-list (value)
  "VALUE as a list: a JSON array reads as a vector, a single value as itself,
and a string is never a list of points."
  (cond ((null value) '())
        ((and (vectorp value) (not (stringp value))) (coerce value 'list))
        ((consp value) value)
        (t '())))

(defun step-problem (step)
  "The reason STEP is no badge number, or NIL for 1 to +MAX-STEP+."
  (cond ((null step) nil)
        ((not (integerp step)) (format nil "step must be an integer from 1 to ~d." +max-step+))
        ((or (< step 1) (> step +max-step+))
         (format nil "step must be from 1 to ~d." +max-step+))
        (t nil)))

(defun point-problem (point label)
  "The reason POINT is no (x y.D) pair, or NIL."
  (unless (hash-table-p point)
    (return-from point-problem (format nil "~a must be an object with x and y." label)))
  (unless (and (shape-number point "x") (shape-number point "y"))
    (format nil "~a needs numeric x and y." label)))

(defun validate-shape (index shape)
  "The reason SHAPE (the INDEXth of a draw) cannot be painted, or NIL."
  (let ((prefix (format nil "shapes[~d]" index)))
    (unless (hash-table-p shape)
      (return-from validate-shape (format nil "~a must be an object." prefix)))
    (let ((kind (shape-string shape "kind"))
          (color (or (shape-string shape "color") "red"))
          (label (shape-string shape "label")))
      (unless (member kind +kinds+ :test #'string=)
        (return-from validate-shape (format nil "~a.kind must be one of: ~{~a~^, ~}."
                                            prefix +kinds+)))
      (unless (member color (mapcar #'car +colors+) :test #'string=)
        (return-from validate-shape (format nil "~a.color must be one of: ~{~a~^, ~}."
                                            prefix (mapcar #'car +colors+))))
      (let ((step (step-problem (shape-number shape "step"))))
        (when step (return-from validate-shape (format nil "~a: ~a" prefix step))))
      (when (and label (> (length label) +max-label+))
        (return-from validate-shape (format nil "~a.label must be at most ~d characters."
                                            prefix +max-label+)))
      (cond
        ((string= kind "circle")
         (unless (and (shape-number shape "x") (shape-number shape "y"))
           (return-from validate-shape (format nil "~a circle needs numeric x and y." prefix)))
         (let ((radius (shape-number shape "radius")))
           (unless (and radius (plusp radius))
             (return-from validate-shape (format nil "~a circle needs a positive radius." prefix)))
           nil))
        ((string= kind "rect")
         (dolist (key '("x" "y" "width" "height"))
           (unless (shape-number shape key)
             (return-from validate-shape (format nil "~a rect needs numeric ~a." prefix key))))
         (unless (and (plusp (shape-number shape "width"))
                      (plusp (shape-number shape "height")))
           (return-from validate-shape (format nil "~a rect width and height must be positive." prefix)))
         nil)
        ((member kind '("arrow" "line") :test #'string=)
         (dolist (key '("fromX" "fromY" "toX" "toY"))
           (unless (shape-number shape key)
             (return-from validate-shape (format nil "~a ~a needs numeric ~a." prefix kind key))))
         nil)
        ((string= kind "polyline")
         (let ((points (shape-list (nlk:json-value shape :any "points"))))
           (when (< (length points) 2)
             (return-from validate-shape (format nil "~a polyline needs at least two points." prefix)))
           (when (> (length points) +max-polyline-points+)
             (return-from validate-shape (format nil "~a polyline is capped at ~d points."
                                                 prefix +max-polyline-points+)))
           (loop for point in points
                 for i from 0
                 for problem = (point-problem point (format nil "~a.points[~d]" prefix i))
                 when problem do (return-from validate-shape problem))
           nil))
        (t                       ; label
         (let ((text (shape-string shape "text")))
           (unless (and text (plusp (length (string-trim '(#\Space #\Tab) text))))
             (return-from validate-shape (format nil "~a label needs non-empty text." prefix)))
           (when (> (length text) +max-label+)
             (return-from validate-shape (format nil "~a label text is capped at ~d characters."
                                                 prefix +max-label+)))
           (unless (and (shape-number shape "x") (shape-number shape "y"))
             (return-from validate-shape (format nil "~a label needs numeric x and y." prefix)))
           nil))))))

(defun shape-points (shape)
  "Every screenshot point SHAPE touches, as (X . Y) conses."
  (let ((kind (shape-string shape "kind")))
    (cond
      ((string= kind "circle")
       (let ((x (shape-number shape "x")) (y (shape-number shape "y"))
             (r (shape-number shape "radius")))
         (list (cons (- x r) y) (cons (+ x r) y) (cons x (- y r)) (cons x (+ y r)))))
      ((string= kind "rect")
       (let ((x (shape-number shape "x")) (y (shape-number shape "y"))
             (w (shape-number shape "width")) (h (shape-number shape "height")))
         (list (cons x y) (cons (+ x w) (+ y h)))))
      ((member kind '("arrow" "line") :test #'string=)
       (list (cons (shape-number shape "fromX") (shape-number shape "fromY"))
             (cons (shape-number shape "toX") (shape-number shape "toY"))))
      ((string= kind "polyline")
       (loop for point in (shape-list (nlk:json-value shape :any "points"))
             collect (cons (shape-number point "x") (shape-number point "y"))))
      (t (list (cons (shape-number shape "x") (shape-number shape "y")))))))

(defun check-in-frame (shapes frame-width frame-height)
  "The reason a mark of SHAPES lies well outside the FRAME-WIDTH by
FRAME-HEIGHT capture, or NIL: coordinates from another picture point at
nothing."
  (let ((low-x (* -1 frame-width +frame-slack+))
        (high-x (* frame-width (+ 1 +frame-slack+)))
        (low-y (* -1 frame-height +frame-slack+))
        (high-y (* frame-height (+ 1 +frame-slack+))))
    (loop for shape in shapes
          for i from 0
          do (loop for (x . y) in (shape-points shape)
                   unless (and (<= low-x x high-x) (<= low-y y high-y))
                     do (return-from check-in-frame
                          (format nil "shapes[~d] reaches (~,0f, ~,0f), outside the ~dx~d capture. \
Coordinates are screenshot pixels with (0,0) at the top left of the captured image."
                                  i x y frame-width frame-height))))
    nil))

(defun validate-draw-payload (shapes frame-width frame-height)
  "The reason a draw cannot be painted, or NIL."
  (unless (and (numberp frame-width) (plusp frame-width)
               (numberp frame-height) (plusp frame-height))
    (return-from validate-draw-payload
      "draw needs frame_width and frame_height from the screenshot just captured."))
  (let ((shapes (shape-list shapes)))
    (unless shapes
      (return-from validate-draw-payload "draw needs a non-empty shapes array."))
    (when (> (length shapes) +max-shapes+)
      (return-from validate-draw-payload (format nil "draw is capped at ~d shapes." +max-shapes+)))
    (loop for shape in shapes
          for i from 0
          for problem = (validate-shape i shape)
          when problem do (return-from validate-draw-payload problem))
    (check-in-frame shapes frame-width frame-height)))

(defun normalized-steps (steps)
  "STEPS as ((:N 1 :TEXT ... :DONE T) ...), whether each arrived as a string
or as a {text, done} object."
  (loop for step in (shape-list steps)
        for i from 0
        for object = (hash-table-p step)
        collect (list :n (1+ i)
                      :text (string-trim '(#\Space #\Tab)
                                          (or (and object (shape-string step "text"))
                                              (and (stringp step) step)
                                              ""))
                      :done (and object (nlk:json-value step :boolean "done")))))

(defun lesson-steps (steps)
  "STEPS as normalized, whether they arrive raw from a call or already
normalized by the one caller that draws them."
  (if (or (null steps)
          (and (listp steps) (consp (first steps)) (keywordp (first (first steps)))))
      steps
      (normalized-steps steps)))

(defun validate-lesson (title steps current-step legend)
  "The reason a lesson cannot be drawn, or NIL."
  (when (and title (> (length title) +max-title+))
    (return-from validate-lesson (format nil "title must be at most ~d characters." +max-title+)))
  (let ((steps (lesson-steps steps)))
    (when (> (length steps) +max-step+)
      (return-from validate-lesson (format nil "steps is capped at ~d entries." +max-step+)))
    (loop for step in steps
          when (zerop (length (getf step :text)))
            do (return-from validate-lesson
                 (format nil "steps[~d] needs non-empty text." (1- (getf step :n)))))
    (loop for step in steps
          when (> (length (getf step :text)) +max-step-text+)
            do (return-from validate-lesson
                 (format nil "steps[~d] text is capped at ~d characters."
                         (1- (getf step :n)) +max-step-text+)))
    (when current-step
      (let ((problem (step-problem current-step)))
        (when problem (return-from validate-lesson problem))
        (when (and steps (> current-step (length steps)))
          (return-from validate-lesson
            (format nil "current_step ~d is past the ~d listed steps." current-step (length steps))))))
    (when (and legend (not (member legend +legend-positions+ :test #'string=)))
      (return-from validate-lesson (format nil "legend_position must be one of: ~{~a~^, ~}."
                                           +legend-positions+))))
  nil)

(defun pick-legend-corner (shapes frame-width frame-height &optional (preferred "auto"))
  "The corner the step card covers least of the marks; ties go top-right.
PREFERRED names one and wins when it is one of the four."
  (when (and preferred (member preferred +legend-positions+ :test #'string=)
             (not (string= preferred "auto")))
    (return-from pick-legend-corner preferred))
  (let ((tallies (list (cons "top-right" 0) (cons "top-left" 0)
                       (cons "bottom-right" 0) (cons "bottom-left" 0)))
        (quarter-width (* frame-width 0.45))
        (quarter-height (* frame-height 0.45)))
    (dolist (shape (shape-list shapes))
      (dolist (point (shape-points shape))
        (let* ((x (car point)) (y (cdr point))
               (right (>= x (- frame-width quarter-width)))
               (left (<= x quarter-width))
               (top (<= y quarter-height))
               (bottom (>= y (- frame-height quarter-height))))
          (when (and top right) (incf (cdr (assoc "top-right" tallies :test #'string=))))
          (when (and top left) (incf (cdr (assoc "top-left" tallies :test #'string=))))
          (when (and bottom right) (incf (cdr (assoc "bottom-right" tallies :test #'string=))))
          (when (and bottom left) (incf (cdr (assoc "bottom-left" tallies :test #'string=)))))))
    (let ((best (first tallies)))
      (dolist (cell (rest tallies) (car best))
        (when (< (cdr cell) (cdr best)) (setf best cell))))))
