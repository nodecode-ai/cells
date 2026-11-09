;;;; png.lisp --- a pixel canvas, and the PNG file it becomes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The encoder is its own: an 8-bit RGBA image, one filter byte a row, and
;;;; a zlib stream of stored (uncompressed) deflate blocks with the ADLER-32
;;;; the format asks for. A 16x16 sprite is a kilobyte and the tileset under
;;;; a hundred, so the omission of compression costs nothing and the cell
;;;; needs no codec library to load, honouring the original's offline promise.

(in-package #:nodecode-gameforge)

;;; --- a canvas ---------------------------------------------------------------

(defstruct (canvas (:constructor make-canvas-raw (&key width height pixels)))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (pixels nil))

(defun make-canvas (width height)
  "A transparent canvas WIDTH by HEIGHT."
  (make-canvas-raw :width width :height height
                   :pixels (make-array (* width height 4) :element-type '(unsigned-byte 8)
                                                        :initial-element 0)))

(defun canvas-index (canvas x y)
  (* 4 (+ (* (canvas-height canvas) 0) (* y (canvas-width canvas)) x)))

(defun set-pixel (canvas x y color)
  "COLOR is (r g b a), each 0-255; outside the canvas it does nothing, so a
shape can be drawn without clipping at every call."
  (when (and (<= 0 x (1- (canvas-width canvas))) (<= 0 y (1- (canvas-height canvas))))
    (let ((base (* 4 (+ (* y (canvas-width canvas)) x))))
      (destructuring-bind (r g b &optional (a 255)) color
        (setf (aref (canvas-pixels canvas) base) (logand r #xFF)
              (aref (canvas-pixels canvas) (1+ base)) (logand g #xFF)
              (aref (canvas-pixels canvas) (+ base 2)) (logand b #xFF)
              (aref (canvas-pixels canvas) (+ base 3)) (logand a #xFF))))))

(defun pixel-at (canvas x y)
  "(r g b a) at X, Y, or NIL outside."
  (when (and (<= 0 x (1- (canvas-width canvas))) (<= 0 y (1- (canvas-height canvas))))
    (let ((base (* 4 (+ (* y (canvas-width canvas)) x))))
      (list (aref (canvas-pixels canvas) base)
            (aref (canvas-pixels canvas) (1+ base))
            (aref (canvas-pixels canvas) (+ base 2))
            (aref (canvas-pixels canvas) (+ base 3))))))

(defun canvas-blit (dest source x0 y0)
  "SOURCE's opaque pixels onto DEST at X0, Y0."
  (loop for y below (canvas-height source)
        do (loop for x below (canvas-width source)
                 do (let ((color (pixel-at source x y)))
                      (when (and color (plusp (fourth color)))
                        (set-pixel dest (+ x0 x) (+ y0 y) color)))))
  dest)

(defun canvas-scaled (canvas factor)
  "CANVAS with every pixel FACTOR by FACTOR, nearest neighbour: pixel art
stays pixel art."
  (let* ((factor (max 1 (truncate factor)))
         (out (make-canvas (* factor (canvas-width canvas)) (* factor (canvas-height canvas)))))
    (loop for y below (canvas-height canvas)
          do (loop for x below (canvas-width canvas)
                   do (let ((color (pixel-at canvas x y)))
                        (when (and color (plusp (fourth color)))
                          (loop for dy below factor
                                do (loop for dx below factor
                                         do (set-pixel out (+ (* x factor) dx)
                                                       (+ (* y factor) dy) color)))))))
    out))

;;; --- the file ---------------------------------------------------------------

(defvar *crc-table* (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
                      (loop for n below 256
                            do (let ((c n))
                                 (loop repeat 8
                                       do (setf c (if (logtest c 1)
                                                      (logxor #xEDB88320 (ash c -1))
                                                      (ash c -1))))
                                 (setf (aref table n) (logand c #xFFFFFFFF))))
                      table)
  "The CRC-32 (PNG's chunk checksum) lookup table.")

(defun crc32 (octets &optional (start 0) (end (length octets)))
  (let ((crc #xFFFFFFFF))
    (loop for index from start below end
          for byte = (aref octets index)
          do (setf crc (logxor (aref *crc-table* (logand (logxor crc byte) #xFF))
                               (ash crc -8))))
    (logand (logxor crc #xFFFFFFFF) #xFFFFFFFF)))

(defun adler32 (octets)
  (let ((a 1) (b 0))
    (loop for byte across octets
          do (setf a (mod (+ a byte) 65521)
                   b (mod (+ b a) 65521)))
    (+ (ash b 16) a)))

(defun push-chunk (out type payload)
  "One PNG chunk: length, type, PAYLOAD, CRC over type and payload."
  (push-be32 out (length payload))
  (let ((start (fill-pointer out)))
    (push-ascii out type)
    (push-octets out payload)
    (push-be32 out (crc32 out start (fill-pointer out)))))

(defun zlib-stored (raw)
  "RAW as a zlib stream of stored deflate blocks."
  (let ((out (make-octets (+ (length raw) 64))))
    (push-octet out #x78) (push-octet out #x01)   ; deflate, 32K window, no dictionary
    (loop with index = 0
          while (< index (length raw))
          for size = (min 65535 (- (length raw) index))
          for last = (>= (+ index size) (length raw))
          do (push-octet out (if last 1 0))        ; BFINAL, BTYPE 00 = stored
             (push-le16 out size)
             (push-le16 out (logxor size #xFFFF))
             (loop for at from index below (+ index size)
                   do (push-octet out (aref raw at)))
             (incf index size))
    (push-be32 out (adler32 raw))
    out))

(defun png-bytes (canvas)
  "CANVAS as the octets of an 8-bit RGBA PNG."
  (let* ((width (canvas-width canvas)) (height (canvas-height canvas))
         (raw (make-octets (* height (1+ (* 4 width)))))
         (pixels (canvas-pixels canvas)))
    (loop for y below height
          do (push-octet raw 0)                    ; filter: none
             (loop for index from (* y width 4) below (* (1+ y) width 4)
                   do (push-octet raw (aref pixels index))))
    (let ((out (make-octets 1024)))
      (push-octets out #(137 80 78 71 13 10 26 10))
      (let ((ihdr (make-octets 16)))
        (push-be32 ihdr width)
        (push-be32 ihdr height)
        (push-octet ihdr 8)                        ; bit depth
        (push-octet ihdr 6)                        ; colour type: truecolour with alpha
        (push-octet ihdr 0) (push-octet ihdr 0) (push-octet ihdr 0)
        (push-chunk out "IHDR" ihdr))
      (push-chunk out "IDAT" (zlib-stored raw))
      (push-chunk out "IEND" (make-octets 0))
      out)))

(defun write-png (path canvas)
  "Write CANVAS to PATH as a PNG; answer the path's namestring."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (write-sequence (png-bytes canvas) stream))
  (namestring path))

(defun read-png-size (path)
  "(values WIDTH HEIGHT) read off a PNG's own IHDR, so a test checks the file."
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((head (make-array 24 :element-type '(unsigned-byte 8))))
      (read-sequence head stream)
      (flet ((be32 (at) (+ (ash (aref head at) 24) (ash (aref head (+ at 1)) 16)
                            (ash (aref head (+ at 2)) 8) (aref head (+ at 3)))))
        (values (be32 16) (be32 20))))))
