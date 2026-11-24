;;;; png.lisp --- a pixel canvas, a five-by-seven face, and the PNG file it becomes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original drew with matplotlib, which brought a font stack, a
;;;; dependency install and a 40 MB image with it. The rewrite draws with a
;;;; canvas of its own and writes the PNG itself: an 8-bit RGB image, one
;;;; filter byte a row, deflated through salza2 - which the serving image
;;;; already loads - so a chart is a few hundred kilobytes and the cell needs
;;;; nothing installed. The face is five by seven dots, scaled to whatever the
;;;; preset's width asks for.

(in-package #:nodecode-charts)

;;; --- the face --------------------------------------------------------------------

(defparameter +glyphs+
  '((" " "00000" "00000" "00000" "00000" "00000" "00000" "00000")
    ("0" "01110" "10001" "10011" "10101" "11001" "10001" "01110")
    ("1" "00100" "01100" "00100" "00100" "00100" "00100" "01110")
    ("2" "01110" "10001" "00001" "00010" "00100" "01000" "11111")
    ("3" "11111" "00010" "00100" "00010" "00001" "10001" "01110")
    ("4" "00010" "00110" "01010" "10010" "11111" "00010" "00010")
    ("5" "11111" "10000" "11110" "00001" "00001" "10001" "01110")
    ("6" "00110" "01000" "10000" "11110" "10001" "10001" "01110")
    ("7" "11111" "00001" "00010" "00100" "01000" "01000" "01000")
    ("8" "01110" "10001" "10001" "01110" "10001" "10001" "01110")
    ("9" "01110" "10001" "10001" "01111" "00001" "00010" "01100")
    ("A" "01110" "10001" "10001" "11111" "10001" "10001" "10001")
    ("B" "11110" "10001" "10001" "11110" "10001" "10001" "11110")
    ("C" "01110" "10001" "10000" "10000" "10000" "10001" "01110")
    ("D" "11100" "10010" "10001" "10001" "10001" "10010" "11100")
    ("E" "11111" "10000" "10000" "11110" "10000" "10000" "11111")
    ("F" "11111" "10000" "10000" "11110" "10000" "10000" "10000")
    ("G" "01110" "10001" "10000" "10111" "10001" "10001" "01111")
    ("H" "10001" "10001" "10001" "11111" "10001" "10001" "10001")
    ("I" "01110" "00100" "00100" "00100" "00100" "00100" "01110")
    ("J" "00111" "00010" "00010" "00010" "00010" "10010" "01100")
    ("K" "10001" "10010" "10100" "11000" "10100" "10010" "10001")
    ("L" "10000" "10000" "10000" "10000" "10000" "10000" "11111")
    ("M" "10001" "11011" "10101" "10101" "10001" "10001" "10001")
    ("N" "10001" "10001" "11001" "10101" "10011" "10001" "10001")
    ("O" "01110" "10001" "10001" "10001" "10001" "10001" "01110")
    ("P" "11110" "10001" "10001" "11110" "10000" "10000" "10000")
    ("Q" "01110" "10001" "10001" "10001" "10101" "10010" "01101")
    ("R" "11110" "10001" "10001" "11110" "10100" "10010" "10001")
    ("S" "01111" "10000" "10000" "01110" "00001" "00001" "11110")
    ("T" "11111" "00100" "00100" "00100" "00100" "00100" "00100")
    ("U" "10001" "10001" "10001" "10001" "10001" "10001" "01110")
    ("V" "10001" "10001" "10001" "10001" "10001" "01010" "00100")
    ("W" "10001" "10001" "10001" "10101" "10101" "11011" "10001")
    ("X" "10001" "10001" "01010" "00100" "01010" "10001" "10001")
    ("Y" "10001" "10001" "01010" "00100" "00100" "00100" "00100")
    ("Z" "11111" "00001" "00010" "00100" "01000" "10000" "11111")
    ("." "00000" "00000" "00000" "00000" "00000" "01100" "01100")
    ("," "00000" "00000" "00000" "00000" "01100" "00100" "01000")
    (":" "00000" "01100" "01100" "00000" "01100" "01100" "00000")
    ("-" "00000" "00000" "00000" "11111" "00000" "00000" "00000")
    ("/" "00001" "00010" "00010" "00100" "01000" "01000" "10000")
    ("(" "00010" "00100" "01000" "01000" "01000" "00100" "00010")
    (")" "01000" "00100" "00010" "00010" "00010" "00100" "01000")
    ("%" "11001" "11010" "00010" "00100" "01000" "01011" "10011")
    ("+" "00000" "00100" "00100" "11111" "00100" "00100" "00000")
    ("=" "00000" "00000" "11111" "00000" "11111" "00000" "00000")
    ("'" "00100" "00100" "00000" "00000" "00000" "00000" "00000")
    ("?" "01110" "10001" "00001" "00010" "00100" "00000" "00100")
    ("#" "01010" "01010" "11111" "01010" "11111" "01010" "01010"))
  "A five-by-seven dot face: the digits, the letters, and the punctuation a
chart's labels use. A lower-case letter sets as its capital; a character
outside this table sets as a box, which is how a label in a script the face
has no dots for shows itself rather than vanishing.")

(defparameter *glyph-table*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry +glyphs+ table)
      (setf (gethash (first entry) table) (rest entry))))
  "The face above, by the one character it sets.")

(defparameter +box-glyph+ '("11111" "10001" "10001" "10001" "10001" "10001" "11111")
  "What a character outside the face sets as.")

(defun glyph-rows (character)
  (or (gethash (string character) *glyph-table*)
      (let ((upper (string-upcase (string character))))
        (or (gethash upper *glyph-table*) +box-glyph+))))

;;; --- the canvas ------------------------------------------------------------------

(defstruct (canvas (:constructor make-canvas-raw (width height pixels)))
  (width 0) (height 0) (pixels nil))

(defun make-canvas (width height &optional (color '(255 255 255)))
  "A canvas WIDTH by HEIGHT filled with COLOR."
  (let ((pixels (make-array (* width height 3) :element-type '(unsigned-byte 8) :initial-element 0)))
    (destructuring-bind (r g b) color
      (loop for i from 0 below (* width height)
            do (setf (aref pixels (* 3 i)) r
                     (aref pixels (1+ (* 3 i))) g
                     (aref pixels (+ 2 (* 3 i))) b)))
    (make-canvas-raw width height pixels)))

(defun canvas-pixel (canvas x y color)
  "One pixel; outside the canvas it does nothing, so a shape needs no clipping."
  (when (and (<= 0 x (1- (canvas-width canvas))) (<= 0 y (1- (canvas-height canvas))))
    (let ((base (* 3 (+ x (* y (canvas-width canvas))))))
      (destructuring-bind (r g b) color
        (setf (aref (canvas-pixels canvas) base) (logand r #xFF)
              (aref (canvas-pixels canvas) (1+ base)) (logand g #xFF)
              (aref (canvas-pixels canvas) (+ base 2)) (logand b #xFF))))))

(defun pixel-color (canvas x y)
  "The (R G B) at X, Y, or NIL outside."
  (when (and (<= 0 x (1- (canvas-width canvas))) (<= 0 y (1- (canvas-height canvas))))
    (let ((base (* 3 (+ x (* y (canvas-width canvas))))))
      (list (aref (canvas-pixels canvas) base)
            (aref (canvas-pixels canvas) (1+ base))
            (aref (canvas-pixels canvas) (+ base 2))))))

(defun fill-rect (canvas x y width height color)
  (loop for row from 0 below height
        do (loop for column from 0 below width
                 do (canvas-pixel canvas (+ x column) (+ y row) color)))
  canvas)

(defun stroke-rect (canvas x y width height color &optional (thickness 1))
  (fill-rect canvas x y width thickness color)
  (fill-rect canvas x (- (+ y height) thickness) width thickness color)
  (fill-rect canvas x y thickness height color)
  (fill-rect canvas (- (+ x width) thickness) y thickness height color)
  canvas)

(defun draw-line (canvas x0 y0 x1 y1 color &optional (thickness 1))
  "A straight line, thick enough to read at a phone's width."
  (let ((steps (max 1 (abs (- x1 x0)) (abs (- y1 y0)))))
    (loop for step from 0 to steps
          for x = (round (+ x0 (* step (/ (- x1 x0) steps))))
          for y = (round (+ y0 (* step (/ (- y1 y0) steps))))
          do (if (> thickness 1)
                 (fill-rect canvas (- x (floor thickness 2)) (- y (floor thickness 2))
                            thickness thickness color)
                 (canvas-pixel canvas x y color))))
  canvas)

(defun text-width (text &key (scale 2))
  "How wide TEXT sets at SCALE."
  (if (zerop (length text)) 0 (* (1- (* 6 (length text))) scale)))

(defun draw-text (canvas x y text color &key (scale 2) (align "left"))
  "TEXT at X, Y in COLOR; ALIGN is left, center or right. => its width."
  (let* ((width (text-width text :scale scale))
         (x (cond ((string= align "center") (- x (floor width 2)))
                  ((string= align "right") (- x width))
                  (t x))))
    (loop for character across (string text)
          for column from 0
          do (let ((rows (glyph-rows character)))
               (loop for row in rows
                     for row-index from 0
                     do (loop for dot across row
                              for dot-index from 0
                              when (char= dot #\1)
                                do (fill-rect canvas (+ x (* (+ (* column 6) dot-index) scale))
                                              (+ y (* row-index scale))
                                              scale scale color)))))
    width))

(defun text-height (&key (scale 2))
  (* 7 scale))

;;; --- the file --------------------------------------------------------------------

(defvar *crc-table* (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
                      (loop for n below 256
                            do (let ((c n))
                                 (loop repeat 8
                                       do (setf c (if (logtest c 1)
                                                      (logxor #xEDB88320 (ash c -1))
                                                      (ash c -1))))
                                 (setf (aref table n) (logand c #xFFFFFFFF))))
                      table)
  "The CRC-32 lookup table PNG's chunks are checked with.")

(defun crc32 (octets &optional (start 0) (end (length octets)))
  (let ((crc #xFFFFFFFF))
    (loop for index from start below end
          do (setf crc (logxor (aref *crc-table* (logand (logxor crc (aref octets index)) #xFF))
                               (ash crc -8))))
    (logand (logxor crc #xFFFFFFFF) #xFFFFFFFF)))

(defun push-octet (out byte) (vector-push-extend (logand byte #xFF) out))
(defun push-octets (out octets)
  (loop for byte across octets do (push-octet out byte)))

(defun push-be32 (out value)
  (push-octet out (ldb (byte 8 24) value))
  (push-octet out (ldb (byte 8 16) value))
  (push-octet out (ldb (byte 8 8) value))
  (push-octet out (ldb (byte 8 0) value)))

(defun push-chunk (out type payload)
  "One PNG chunk: its length, its type, its payload, and the CRC over both."
  (push-be32 out (length payload))
  (let ((start (fill-pointer out)))
    (loop for character across (string type) do (push-octet out (char-code character)))
    (push-octets out payload)
    (push-be32 out (crc32 out start (fill-pointer out)))))

(defun canvas-scanlines (canvas)
  "The canvas as PNG scanlines: a filter byte, then R, G, B for every pixel."
  (let* ((width (canvas-width canvas))
         (height (canvas-height canvas))
         (raw (make-array (+ (* height 1) (* height width 3)) :element-type '(unsigned-byte 8)))
         (at 0))
    (loop for y below height
          do (setf (aref raw at) 0)
             (incf at)
             (loop for x below width
                   do (let ((base (* 3 (+ x (* y width)))))
                        (setf (aref raw at) (aref (canvas-pixels canvas) base)
                              (aref raw (1+ at)) (aref (canvas-pixels canvas) (1+ base))
                              (aref raw (+ at 2)) (aref (canvas-pixels canvas) (+ base 2)))
                        (incf at 3))))
    raw))

(defun write-png (canvas path)
  "CANVAS written to PATH as an 8-bit RGB PNG. => the path."
  (let ((raw (canvas-scanlines canvas))
        (out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (byte '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A)) (push-octet out byte))
    (let ((header (make-array 13 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
      (push-be32 header (canvas-width canvas))
      (push-be32 header (canvas-height canvas))
      (push-octet header 8)                     ; bit depth
      (push-octet header 2)                     ; colour type: RGB
      (push-octet header 0) (push-octet header 0) (push-octet header 0)
      (push-chunk out "IHDR" header))
    (push-chunk out "IDAT" (salza2:compress-data raw 'salza2:zlib-compressor))
    (push-chunk out "IEND" (make-array 0 :element-type '(unsigned-byte 8)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede :if-does-not-exist :create)
      (write-sequence out stream))
    path))
