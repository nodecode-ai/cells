;;;; png.lisp --- a monochrome bitmap, and the PNG file it becomes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A frame is ink or paper and nothing between, so the file is PNG's
;;;; one-bit greyscale: eight pixels to the byte, 0 ink and 1 paper, every
;;;; row behind a zero filter byte. The image data then goes out as zlib's
;;;; stored blocks - a header, a length and its complement, the bytes, a
;;;; checksum - which needs no compressor beside SBCL and keeps the encoder
;;;; here readable. A 1200x960 frame is about 145 KB on disk, which a frame
;;;; of six thousand characters easily pays for.

(in-package #:nodecode-snapcompact)

;;; --- checksums ------------------------------------------------------------------

(defparameter +crc-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((value n))
        (dotimes (k 8)
          (setf value (if (logbitp 0 value) (logxor #xEDB88320 (ash value -1)) (ash value -1))))
        (setf (aref table n) value))))
  "CRC-32 of each byte value, PNG's chunk checksum table.")

(defun crc32 (octets &optional (crc #xFFFFFFFF))
  "CRC-32 over OCTETS, starting from CRC."
  (declare (type (unsigned-byte 32) crc))
  (loop for byte across octets
        do (setf crc (logxor (aref +crc-table+ (logand (logxor crc byte) #xFF))
                              (ash crc -8)))
        finally (return (logxor crc #xFFFFFFFF))))

(defun adler32 (octets)
  "Adler-32 over OCTETS, zlib's stream checksum."
  (let ((a 1) (b 0))
    (loop for byte across octets
          do (setf a (mod (+ a byte) 65521)
                   b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

;;; --- bytes out --------------------------------------------------------------------

(defun push-u32 (vector value)
  "VALUE, four bytes big-endian, onto VECTOR."
  (loop for shift from 24 downto 0 by 8
        do (vector-push-extend (ldb (byte 8 shift) value) vector)))

(defun push-chunk (vector type data)
  "The PNG chunk of TYPE carrying DATA onto VECTOR."
  (let ((head (map 'vector #'char-code type)))
    (push-u32 vector (length data))
    (loop for byte across head do (vector-push-extend byte vector))
    (loop for byte across data do (vector-push-extend byte vector))
    (push-u32 vector (crc32 (concatenate '(vector (unsigned-byte 8)) head data)))))

(defun zlib-stored (raw)
  "RAW as a zlib stream in stored blocks: the header, then each block's
length, its complement, and its bytes, then the checksum."
  (let ((out (make-array (+ 2 (length raw) (* 6 (1+ (floor (length raw) 65535))))
                          :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0)))
    (vector-push-extend #x78 out)
    (vector-push-extend #x01 out)
    (loop with start = 0
          while (< start (length raw))
          for length = (min 65535 (- (length raw) start))
          for last = (>= (+ start length) (length raw))
          do (vector-push-extend (if last 1 0) out)
             (vector-push-extend (ldb (byte 8 0) length) out)
             (vector-push-extend (ldb (byte 8 8) length) out)
             (vector-push-extend (ldb (byte 8 0) (logand #xFFFF (lognot length))) out)
             (vector-push-extend (ldb (byte 8 8) (logand #xFFFF (lognot length))) out)
             (loop for index from start below (+ start length)
                   do (vector-push-extend (aref raw index) out))
             (setf start (+ start length)))
    (push-u32 out (adler32 raw))
    out))

;;; --- a bitmap --------------------------------------------------------------------

(defstruct (bitmap (:constructor make-bitmap (width height)))
  "WIDTH, HEIGHT, and one bit a pixel: 1 paper, 0 ink, row by row."
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (pixels (make-array 0 :element-type 'bit) :type (simple-array bit (*))))

(defun make-blank-bitmap (width height)
  "A white bitmap WIDTH by HEIGHT."
  (let ((bitmap (make-bitmap width height)))
    (setf (bitmap-pixels bitmap)
          (make-array (* width height) :element-type 'bit :initial-element 1))
    bitmap))

(defun ink-pixel (bitmap x y)
  "Mark the pixel at X, Y as ink, or nothing when it falls outside."
  (let ((w (bitmap-width bitmap)) (h (bitmap-height bitmap)))
    (when (and (<= 0 x (1- w)) (<= 0 y (1- h)))
      (setf (aref (bitmap-pixels bitmap) (+ (* y w) x)) 0))))

(defun ink-glyph (bitmap rows x y scale)
  "Draw ROWS, seven strings of five, at X, Y, each pixel SCALE square."
  (loop for row from 0 below 7
        for line = (aref rows row)
        do (loop for column from 0 below (min 5 (length line))
                 when (char= #\# (char line column))
                   do (loop for dx from 0 below scale
                            do (loop for dy from 0 below scale
                                     do (ink-pixel bitmap
                                                   (+ x (* column scale) dx)
                                                   (+ y (* row scale) dy)))))))

;;; --- the file ---------------------------------------------------------------------

(defun bitmap-rows (bitmap)
  "BITMAP's image data as PNG wants it: each row a filter byte, then the
row's bits packed eight to a byte, the leftmost pixel in the high bit, and
the padding past the last column left paper."
  (let* ((width (bitmap-width bitmap))
         (height (bitmap-height bitmap))
         (stride (ceiling width 8))
         (pixels (bitmap-pixels bitmap))
         (raw (make-array (* height (1+ stride)) :element-type '(unsigned-byte 8)
                                                 :initial-element #xFF)))
    (dotimes (y height)
      (let ((out (+ (* y (1+ stride)) 1))
            (row (* y width)))
        (setf (aref raw (1- out)) 0)
        (dotimes (x width)
          (unless (= 1 (aref pixels (+ row x)))
            (let ((at (+ out (floor x 8))))
              (setf (aref raw at) (logand (aref raw at)
                                           (logxor #xFF (ash 1 (- 7 (mod x 8)))))))))))
    raw))

(defun png-of-bitmap (bitmap)
  "BITMAP as the bytes of a one-bit greyscale PNG file."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (header (make-array 13 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for byte across #(137 80 78 71 13 10 26 10)
          do (vector-push-extend byte out))
    (loop for (value at) in (list (list (bitmap-width bitmap) 0)
                                   (list (bitmap-height bitmap) 4))
          do (loop for k below 4
                   do (setf (aref header (+ at k)) (ldb (byte 8 (* 8 (- 3 k))) value))))
    (setf (aref header 8) 1     ; one bit a sample
          (aref header 9) 0)    ; greyscale
    (push-chunk out "IHDR" header)
    (push-chunk out "IDAT" (zlib-stored (bitmap-rows bitmap)))
    (push-chunk out "IEND" (make-array 0 :element-type '(unsigned-byte 8)))
    out))

(defun write-png (bitmap path)
  "Write BITMAP to PATH, creating its folder, and answer PATH."
  (ensure-directories-exist path)
  (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
    (write-sequence (png-of-bitmap bitmap) out))
  path)
