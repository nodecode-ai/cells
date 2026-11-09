;;;; cell.lisp --- image_utils: inspect, resize, crop, rotate, convert, optimize.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of MazenMkhinini/hermes-image-plugin, whose six
;;;; Pillow tools this keeps, name for name: image_info, image_resize,
;;;; image_crop, image_rotate, image_convert and image_optimize, with the
;;;; same overwrite-and-confirm gate and the same default output name beside
;;;; the input.
;;;;
;;;; Pillow does not come with it, and nothing here needs it: the engine
;;;; already reads PNG, GIF and WebP in Lisp and writes PNG and baseline
;;;; JPEG -- the codecs a vision attachment goes through, in engine/image/ --
;;;; so an edit happens in the same pixels the model is shown. BMP is read
;;;; and written here, in this file. A format none of these opens goes the
;;;; way an attachment goes: through whichever of ImageMagick, ffmpeg or sips
;;;; the machine has, and the answer names that when there is none.
;;;;
;;;; The pure half is where the behaviour is decided and what the tests
;;;; read: RESAMPLE-PIXELS, CROP-PIXELS, ROTATE-QUARTER, ROTATE-ANY,
;;;; FLATTEN-PIXELS, BMP-BYTES. A tool is a contract, a size calculation
;;;; and a write around it.

(defpackage #:nodecode-image-utils
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:run-slash
           #:image-info #:image-resize #:image-crop #:image-rotate
           #:image-convert #:image-optimize
           #:resolve-path #:sniff-format #:open-image #:opened-image-pixels
           #:opened-image-format #:opened-image-width #:opened-image-height
           #:resample-pixels #:crop-pixels #:rotate-quarter #:rotate-any
           #:flatten-pixels #:bmp-bytes #:bmp-pixels #:target-path
           #:format-label #:encode-pixels #:quality-of))

(in-package #:nodecode-image-utils)

(nlk:define-peripheral image-utils :not-running t)

;;; --- the one refusal ----------------------------------------------------------

(defmacro guard (&body body)
  "BODY, whose refusal (or any other error) is answered as the tool's text: a
sentence the model reads and repairs from, never a condition out of a call."
  `(handler-case (progn ,@body)
     (error (e) (format nil "image_utils: ~a" (princ-to-string e)))))

;;; --- paths and bytes -----------------------------------------------------------

(defun resolve-path (text)
  "TEXT, a path a tool was handed, as a pathname: ~ expanded, a relative one
read from this process's working directory. NIL for an empty TEXT."
  (let ((text (nlk:trimmed (or text ""))))
    (unless (zerop (length text))
      (let* ((text (cond ((string= text "~") (namestring (user-homedir-pathname)))
                          ((and (> (length text) 1)
                                (char= #\~ (char text 0)) (char= #\/ (char text 1)))
                           (namestring (merge-pathnames (subseq text 2)
                                                         (user-homedir-pathname))))
                          (t text)))
             (parsed (uiop:parse-native-namestring text)))
        (if (eq :absolute (car (pathname-directory parsed)))
            parsed
            (merge-pathnames parsed (uiop:getcwd)))))))

(defun read-octets (path)
  "PATH's whole contents, as octets; a path that names no file refuses here."
  (let ((true (and path (probe-file path))))
    (unless (and true (not (uiop:directory-pathname-p true)))
      (fail "~a is not a file." (if path (namestring path) path)))
    (alexandria:read-file-into-byte-vector true)))

(defun write-octets (octets path)
  "OCTETS written to PATH, folders made on the way. => the bytes written."
  (ensure-directories-exist path)
  (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
  (length octets))

(defun unsigned (bytes at count &optional little)
  "The COUNT bytes of BYTES at AT as one unsigned integer, high byte first or,
with LITTLE, low byte first."
  (loop for i below count
        sum (ash (aref bytes (+ at i)) (* 8 (if little i (- count i 1))))))

;;; --- which format these bytes are ------------------------------------------------

(defun bmp-header-p (bytes)
  "Whether BYTES lead with a BMP file header this cell reads: 24 or 32 bits a
pixel, uncompressed."
  (and (>= (length bytes) 54)
       (= #x42 (aref bytes 0)) (= #x4D (aref bytes 1))
       (member (unsigned bytes 14 4 t) '(40 108 124))
       (member (unsigned bytes 28 2 t) '(24 32))
       (zerop (unsigned bytes 30 4 t))))

(defun sniff-format (bytes)
  "The format BYTES are, read off their own first bytes -- never the file's
name: :png, :jpeg, :gif, :webp, :bmp, or NIL."
  (cond
    ((bmp-header-p bytes) :bmp)
    (t (let ((media (nlt:image-media-type bytes)))
         (cond ((equal media "image/png") :png)
               ((equal media "image/jpeg") :jpeg)
               ((equal media "image/gif") :gif)
               ((equal media "image/webp") :webp))))))

(defun format-label (format)
  "FORMAT as a person reads it: PNG, JPEG, GIF, WEBP, BMP."
  (and format (string-upcase (symbol-name format))))

(defun format-extension (format)
  "The file extension FORMAT is written with."
  (case format (:png "png") (:jpeg "jpg") (:gif "gif") (:webp "webp")
    (:bmp "bmp") (:tiff "tif") (:avif "avif") (t "png")))

(defun media-type-of (format)
  "The media type FORMAT's bytes carry, or NIL for a format no header names."
  (case format (:png "image/png") (:jpeg "image/jpeg") (:gif "image/gif")
    (:webp "image/webp")))

(defun format-from-text (text)
  "The format TEXT names -- PNG, jpeg, jpg, tif -- or NIL."
  (let ((word (string-upcase (nlk:trimmed (or text "")))))
    (cond ((string= word "PNG") :png)
          ((member word '("JPEG" "JPG") :test #'string=) :jpeg)
          ((member word '("TIFF" "TIF") :test #'string=) :tiff)
          ((string= word "GIF") :gif)
          ((string= word "WEBP") :webp)
          ((string= word "AVIF") :avif)
          ((string= word "BMP") :bmp))))

(defparameter +writable+ '(:png :jpeg :bmp)
  "The formats this cell writes with no program beside it.")

;;; --- BMP, read and written here -------------------------------------------------

(defun bmp-dimensions (bytes)
  "(values WIDTH HEIGHT) from a BMP's header, or NIL."
  (when (bmp-header-p bytes)
    (let ((raw (unsigned bytes 22 4 t)))
      (values (unsigned bytes 18 4 t)
              (abs (if (>= raw #x80000000) (- raw #x100000000) raw))))))

(defun bmp-pixels (bytes)
  "BYTES, a BMP, decoded to the engine's PIXELS: 24 bits a pixel as RGB, 32
as RGBA -- what an uncompressed BMP of either declares."
  (unless (bmp-header-p bytes)
    (fail "its header is not a 24- or 32-bit uncompressed BMP."))
  (let* ((offset (unsigned bytes 10 4 t))
         (width (unsigned bytes 18 4 t))
         (raw-height (unsigned bytes 22 4 t))
         (top-down (>= raw-height #x80000000))
         (height (abs (if top-down (- raw-height #x100000000) raw-height)))
         (bpp (unsigned bytes 28 2 t))
         (step (/ bpp 8))
         (channels (if (= bpp 32) 4 3))
         (stride (* 4 (ceiling (* width bpp) 32)))
         (pixels (nle::blank-pixels width height channels))
         (data (nle::pixels-data pixels)))
    (dotimes (row height)
      (let* ((y (if top-down row (- height 1 row)))
             (base (+ offset (* row stride))))
        (dotimes (x width)
          (let ((at (+ base (* x step)))
                (to (* (+ (* y width) x) channels)))
            (when (> (+ at step) (length bytes))
              (fail "it is a truncated BMP."))
            (setf (aref data to) (aref bytes (+ at 2))
                  (aref data (+ to 1)) (aref bytes (+ at 1))
                  (aref data (+ to 2)) (aref bytes at))
            (when (= channels 4)
              (setf (aref data (+ to 3)) (aref bytes (+ at 3))))))))
    pixels))

(defun bmp-bytes (pixels)
  "PIXELS as the bytes of a 24-bit BMP: bottom-up, BGR, every row padded to
four bytes, alpha laid over white."
  (let* ((flat (flatten-pixels pixels "white"))
         (width (nle::pixels-width flat))
         (height (nle::pixels-height flat))
         (stride (* 4 (ceiling (* width 3) 4)))
         (size (+ 54 (* stride height)))
         (out (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (data (nle::pixels-data flat)))
    (flet ((put (value at count)
             (loop for i below count
                   do (setf (aref out (+ at i)) (ldb (byte 8 (* 8 i)) value)))))
      (setf (aref out 0) #x42 (aref out 1) #x4D)
      (put size 2 4) (put 54 10 4) (put 40 14 4)
      (put width 18 4) (put height 22 4) (put 1 26 2) (put 24 28 2)
      (put (* stride height) 34 4))
    (dotimes (row height)
      (let ((base (+ 54 (* row stride)))
            (y (- height 1 row)))
        (dotimes (x width)
          (let ((from (* (+ (* y width) x) 3))
                (to (+ base (* x 3))))
            (setf (aref out to) (aref data (+ from 2))
                  (aref out (+ to 1)) (aref data (+ from 1))
                  (aref out (+ to 2)) (aref data from))))))
    out))

;;; --- one file opened -------------------------------------------------------------

(defstruct (opened-image (:constructor %make-opened-image))
  "One image file as this cell sees it: its PATH, its BYTES, the FORMAT their
header declares, the WIDTH and HEIGHT the header (or the decoded pixels)
give, the PIXELS when a codec here opened them, and REASON when none did."
  (path nil) (bytes #()) (format nil) (width 0) (height 0) (pixels nil) (reason nil))

(defun decode-pixels (bytes format)
  "BYTES decoded => (values PIXELS REASON): this cell's BMP reader, else the
engine's own readers, else whichever transcoder the machine has; REASON says
why when nothing opened them."
  (handler-case
      (cond ((null format)
             (values nil "its bytes open no PNG, JPEG, GIF, WebP or BMP"))
            ((eq format :bmp) (values (bmp-pixels bytes) nil))
            (t (values (nle::image-pixels bytes (media-type-of format)) nil)))
    (error (e) (values nil (princ-to-string e)))))

(defun header-size (bytes format)
  "(values WIDTH HEIGHT) the header of BYTES declares, or NIL."
  (case format
    (:bmp (bmp-dimensions bytes))
    ((:png :jpeg :gif :webp) (nlt:image-dimensions bytes))
    (t nil)))

(defun open-image (path &key (decode t))
  "PATH opened, with its pixels when DECODE. The size comes from the header
before anything is decoded, so a file claiming 30000x30000 costs nothing."
  (let* ((bytes (read-octets path))
         (format (sniff-format bytes))
         (image (%make-opened-image :path path :bytes bytes :format format)))
    (multiple-value-bind (width height) (header-size bytes format)
      (setf (opened-image-width image) (or width 0)
            (opened-image-height image) (or height 0)))
    (when decode
      (multiple-value-bind (pixels reason) (decode-pixels bytes format)
        (setf (opened-image-pixels image) pixels
              (opened-image-reason image) reason)
        (when pixels
          (setf (opened-image-width image) (nle::pixels-width pixels)
                (opened-image-height image) (nle::pixels-height pixels)))))
    image))

;;; --- EXIF, presence only ----------------------------------------------------------
;;; The original reported whether a file carries GPS, never where, and this reads
;;; the same way: IFD0's own entries, the orientation among them, and the one
;;; tag that says a GPS IFD is there.

(defun jpeg-app1 (bytes)
  "The Exif APP1 segment of JPEG BYTES as (values START END), its payload
between them, or NIL."
  (when (equal (sniff-format bytes) :jpeg)
    (loop with index = 2
          with end = (length bytes)
          while (< (+ index 9) end)
          do (cond
               ((/= #xFF (aref bytes index)) (return nil))
               ((= #xFF (aref bytes (1+ index))) (incf index))
               (t (let ((marker (aref bytes (1+ index)))
                        (length (unsigned bytes (+ index 2) 2)))
                    (when (< length 2) (return nil))
                    (when (and (= marker #xE1) (>= length 8)
                               (equal "Exif"
                                      (map 'string #'code-char
                                           (subseq bytes (+ index 4) (+ index 8)))))
                      (return (values (+ index 4) (min end (+ index 2 length)))))
                    (incf index (+ 2 length))))))))

(defun jpeg-exif (bytes)
  "(values ORIENTATION GPS-P) from JPEG BYTES' Exif IFD0: the orientation
tag's value, 1 when it names none, and whether a GPS IFD is there at all.
The coordinates are never read."
  (multiple-value-bind (start end) (jpeg-app1 bytes)
    (unless start (return-from jpeg-exif nil))
    (let* ((tiff (+ start 6))
           (order (and (<= (+ tiff 8) end)
                       (map 'string #'code-char (subseq bytes tiff (+ tiff 2)))))
           (little (equal order "II")))
      (unless (or little (equal order "MM")) (return-from jpeg-exif nil))
      (let ((at (+ tiff (unsigned bytes (+ tiff 4) 4 little)))
            (orientation 1)
            (gps nil))
        (when (<= (+ at 2) end)
          (loop repeat (unsigned bytes at 2 little)
                for entry from (+ at 2) by 12
                while (<= (+ entry 12) end)
                do (let ((tag (unsigned bytes entry 2 little)))
                     (case tag
                       (#x0112 (setf orientation (unsigned bytes (+ entry 8) 2 little)))
                       (#x8825 (setf gps t))))))
        (values orientation gps)))))

;;; --- pixels: crop, resample, rotate, flatten -------------------------------------

(defun crop-pixels (pixels left top right bottom)
  "PIXELS cropped to the box LEFT TOP RIGHT BOTTOM, refused when the box
leaves the image -- a crop never pads with black."
  (let* ((width (nle::pixels-width pixels))
         (height (nle::pixels-height pixels))
         (channels (nle::pixels-channels pixels)))
    (unless (and (<= 0 left) (< left right) (<= right width)
                 (<= 0 top) (< top bottom) (<= bottom height))
      (fail "the box ~d,~d - ~d,~d is not inside ~dx~d."
            left top right bottom width height))
    (let* ((out (nle::blank-pixels (- right left) (- bottom top) channels))
           (source (nle::pixels-data pixels))
           (target (nle::pixels-data out)))
      (dotimes (row (- bottom top))
        (let ((from (* (+ top row) width channels))
              (to (* row (- right left) channels)))
          (replace target source
                   :start1 to
                   :start2 (+ from (* left channels))
                   :end2 (+ from (* right channels)))))
      out)))

(defun nearest-pixels (pixels width height)
  "PIXELS resampled to WIDTH x HEIGHT by taking the nearest sample."
  (let* ((sw (nle::pixels-width pixels)) (sh (nle::pixels-height pixels))
         (channels (nle::pixels-channels pixels))
         (source (nle::pixels-data pixels))
         (out (nle::blank-pixels width height channels))
         (target (nle::pixels-data out)))
    (dotimes (y height)
      (let ((sy (min (1- sh) (floor (* y sh) height))))
        (dotimes (x width)
          (let* ((sx (min (1- sw) (floor (* x sw) width)))
                 (to (* (+ (* y width) x) channels))
                 (from (* (+ (* sy sw) sx) channels)))
            (dotimes (k channels)
              (setf (aref target (+ to k)) (aref source (+ from k))))))))
    out))

(defun bilinear-pixels (pixels width height)
  "PIXELS resampled to WIDTH x HEIGHT by interpolating the four samples around
each new pixel's centre: a smooth upscale, and the bicubic and bilinear of
the original's vocabulary."
  (let* ((sw (nle::pixels-width pixels)) (sh (nle::pixels-height pixels))
         (channels (nle::pixels-channels pixels))
         (source (nle::pixels-data pixels))
         (out (nle::blank-pixels width height channels))
         (target (nle::pixels-data out))
         (xscale (/ sw (float width)))
         (yscale (/ sh (float height))))
    (dotimes (y height)
      (let* ((fy (max 0.0 (- (* (+ y 0.5) yscale) 0.5)))
             (y0 (min (1- sh) (floor fy)))
             (y1 (min (1- sh) (1+ y0)))
             (wy (- fy y0)))
        (dotimes (x width)
          (let* ((fx (max 0.0 (- (* (+ x 0.5) xscale) 0.5)))
                 (x0 (min (1- sw) (floor fx)))
                 (x1 (min (1- sw) (1+ x0)))
                 (wx (- fx x0))
                 (to (* (+ (* y width) x) channels)))
            (dotimes (k channels)
              (let ((p00 (aref source (+ (* (+ (* y0 sw) x0) channels) k)))
                    (p10 (aref source (+ (* (+ (* y0 sw) x1) channels) k)))
                    (p01 (aref source (+ (* (+ (* y1 sw) x0) channels) k)))
                    (p11 (aref source (+ (* (+ (* y1 sw) x1) channels) k))))
                (setf (aref target (+ to k))
                      (min 255 (round (+ (* p00 (- 1 wx) (- 1 wy))
                                         (* p10 wx (- 1 wy))
                                         (* p01 (- 1 wx) wy)
                                         (* p11 wx wy)))))))))))
    out))

(defun resample-pixels (pixels width height mode)
  "PIXELS resampled to WIDTH x HEIGHT: :area averages every source pixel a
result covers (the engine's own shrinking, so a downscale loses no detail to
sampling), :bilinear interpolates, :nearest takes one sample."
  (let ((sw (nle::pixels-width pixels)) (sh (nle::pixels-height pixels)))
    (cond ((and (= sw width) (= sh height)) pixels)
          ((eq mode :nearest) (nearest-pixels pixels width height))
          ((and (eq mode :area) (<= width sw) (<= height sh))
           (nle::shrink-pixels pixels width height))
          (t (bilinear-pixels pixels width height)))))

(defun rotate-quarter (pixels turns)
  "PIXELS turned TURNS quarters counter-clockwise -- 1, 2 or 3 -- exactly:
every sample moves, none is blended."
  (let* ((width (nle::pixels-width pixels))
         (height (nle::pixels-height pixels))
         (channels (nle::pixels-channels pixels))
         (source (nle::pixels-data pixels))
         (dw (if (oddp turns) height width))
         (dh (if (oddp turns) width height))
         (out (nle::blank-pixels dw dh channels))
         (target (nle::pixels-data out)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((from (* (+ (* y width) x) channels))
               (to (ecase turns
                     (1 (* (+ (* (- width 1 x) dw) y) channels))
                     (2 (* (+ (* (- height 1 y) width) (- width 1 x)) channels))
                     (3 (* (+ (* x dw) (- height 1 y)) channels)))))
          (dotimes (k channels)
            (setf (aref target (+ to k)) (aref source (+ from k)))))))
    out))

(defun background-rgb (text)
  "TEXT, a fill colour -- white, black or #rrggbb -- as (R G B); :TRANSPARENT
for the keyword of that name."
  (cond
    ((eq text :transparent) :transparent)
    (t (let ((word (string-downcase (nlk:trimmed (or text "white")))))
         (cond ((string= word "white") '(255 255 255))
               ((string= word "black") '(0 0 0))
               ((and (= (length word) 7) (char= #\# (char word 0)))
                (list (parse-integer word :start 1 :end 3 :radix 16)
                      (parse-integer word :start 3 :end 5 :radix 16)
                      (parse-integer word :start 5 :end 7 :radix 16)))
               (t (fail "background takes white, black or #rrggbb, not ~s." text)))))))

(defun rotate-any (pixels degrees background expand)
  "PIXELS turned DEGREES counter-clockwise about its centre, sampled
bilinearly. EXPAND grows the canvas so no corner is cut off, and what the
turn leaves empty takes BACKGROUND -- transparent when the pixels carry alpha
and the call named no colour, as the original left it."
  (let* ((width (nle::pixels-width pixels))
         (height (nle::pixels-height pixels))
         (channels (nle::pixels-channels pixels))
         (alpha (= channels 4))
         (angle (* degrees (/ pi 180.0)))
         (cosine (cos angle))
         (sine (sin angle))
         (dw (if expand (max 1 (ceiling (+ (* width (abs cosine)) (* height (abs sine))))) width))
         (dh (if expand (max 1 (ceiling (+ (* width (abs sine)) (* height (abs cosine))))) height))
         (out (nle::blank-pixels dw dh channels))
         (target (nle::pixels-data out))
         (source (nle::pixels-data pixels))
         (rgb (background-rgb (or background (if alpha :transparent "white"))))
         (clear (and alpha (eq rgb :transparent)))
         (rgb (unless clear rgb)))
    (fill target 0)
    (unless clear
      (dotimes (y dh)
        (dotimes (x dw)
          (let ((to (* (+ (* y dw) x) channels)))
            (setf (aref target to) (first rgb)
                  (aref target (+ to 1)) (second rgb)
                  (aref target (+ to 2)) (third rgb))
            (when alpha (setf (aref target (+ to 3)) 255))))))
    (dotimes (y dh)
      (dotimes (x dw)
        (let* ((up (- (+ x 0.5) (/ dw 2.0)))
               (vp (- (+ y 0.5) (/ dh 2.0)))
               (sx (+ (- (* up cosine) (* vp sine)) (- (/ width 2.0) 0.5)))
               (sy (+ (+ (* up sine) (* vp cosine)) (- (/ height 2.0) 0.5)))
               (x0 (floor sx))
               (y0 (floor sy)))
          (when (and (<= 0 x0) (<= 0 y0) (< (1+ x0) width) (< (1+ y0) height))
            (let ((wx (- sx x0))
                  (wy (- sy y0))
                  (to (* (+ (* y dw) x) channels)))
              (dotimes (k channels)
                (let ((p00 (aref source (+ (* (+ (* y0 width) x0) channels) k)))
                      (p10 (aref source (+ (* (+ (* y0 width) (1+ x0)) channels) k)))
                      (p01 (aref source (+ (* (+ (* (1+ y0) width) x0) channels) k)))
                      (p11 (aref source (+ (* (+ (* (1+ y0) width) (1+ x0)) channels) k))))
                  (setf (aref target (+ to k))
                        (min 255 (round (+ (* p00 (- 1 wx) (- 1 wy))
                                           (* p10 wx (- 1 wy))
                                           (* p01 (- 1 wx) wy)
                                           (* p11 wx wy))))))))))))
    out))

(defun flatten-pixels (pixels background)
  "PIXELS in three channels: alpha laid over BACKGROUND, (R G B), or white
when it is :TRANSPARENT -- which JPEG and BMP cannot carry anyway. Pixels
without alpha are copied."
  (let* ((channels (nle::pixels-channels pixels))
         (width (nle::pixels-width pixels))
         (height (nle::pixels-height pixels))
         (count (* width height))
         (source (nle::pixels-data pixels))
         (out (nle::blank-pixels width height 3))
         (target (nle::pixels-data out))
         (rgb (let ((rgb (background-rgb background)))
                (if (eq rgb :transparent) '(255 255 255) rgb))))
    (ecase channels
      (3 (replace target source))
      (1 (dotimes (i count)
           (let ((v (aref source i)))
             (setf (aref target (* 3 i)) v
                   (aref target (+ 1 (* 3 i))) v
                   (aref target (+ 2 (* 3 i))) v))))
      (4 (dotimes (i count)
           (let ((a (aref source (+ 3 (* 4 i)))))
             (dotimes (k 3)
               (let ((bg (nth k rgb)))
                 (setf (aref target (+ k (* 3 i)))
                       (floor (+ (* (aref source (+ k (* 4 i))) a)
                                 (* bg (- 255 a))
                                 127)
                              255))))))))
    out))

(defun quality-of (quality)
  "QUALITY kept inside what an encoder takes, 1 to 100; the section's
jpeg_quality when the call named none."
  (max 1 (min 100 (or quality (setting :jpeg-quality)))))

(defun encode-pixels (pixels format &key (quality 85) (background :white))
  "PIXELS as FORMAT's bytes: PNG keeps its alpha, JPEG and BMP lay it over
BACKGROUND, and JPEG is encoded at QUALITY."
  (case format
    (:png (nle::pixels-png pixels))
    (:jpeg (nle::pixels-jpeg (flatten-pixels pixels background) (quality-of quality)))
    (:bmp (bmp-bytes pixels))
    (t (fail "~a is not a format this cell writes: PNG, JPEG and BMP are."
             (format-label format)))))

;;; --- where a result goes ----------------------------------------------------------

(defun target-path (source operation format output-path)
  "Where an edit writes: OUTPUT-PATH when the call names one, else beside the
input as <stem>_<operation>.<ext> -- the original's own default."
  (let ((named (and output-path (nlk:trimmed output-path))))
    (if (and named (plusp (length named)))
        (let ((path (resolve-path named)))
          (when (null (pathname-name path))
            (fail "output_path must name a file, not a folder."))
          path)
        (merge-pathnames (format nil "~a_~a.~a" (pathname-name source) operation
                                 (format-extension format))
                         (uiop:pathname-directory-pathname source)))))

(defvar *stats* '(:written 0)
  "What this process has written since the cell started: what /image-utils
reports.")

(defun save-pixels (image pixels format operation
                    &key output-path overwrite confirm quality (background :white))
  "PIXELS encoded as FORMAT and written where the call says. A target that
already holds a file is refused unless OVERWRITE and CONFIRM are both true --
the original's gate, kept. => one line saying what was written, how the bytes
changed, and whether the result grew."
  (let* ((target (target-path (opened-image-path image) operation format output-path))
         (name (namestring target))
         (exists (probe-file name)))
    (when (and exists (not (and overwrite confirm)))
      (fail "~a already holds a file: pass overwrite=true and confirm=true to replace it, or name another output_path."
            (file-namestring name)))
    (let* ((bytes (encode-pixels pixels format :quality quality :background background))
           (before (length (opened-image-bytes image)))
           (after (length bytes)))
      (write-octets bytes name)
      (incf (getf *stats* :written))
      (format nil "written to ~a (~:d -> ~:d bytes~@[~a~])"
              (file-namestring name) before after
              (and (> after before) ", larger than the input")))))

;;; --- the six tools' arguments -------------------------------------------------------

(nlk:define-contract info-arguments
  (:doc "Read-only facts about one image file on this machine: format, dimensions, samples and alpha, bytes, whether its pixels decode here, and - for a JPEG that carries EXIF - its orientation and whether a GPS IFD is present (presence only, never coordinates). Writes nothing.")
  ("path" :text :required t
   :doc "the image file: absolute, relative to the working directory, or starting with ~"))

(nlk:define-contract resize-arguments
  (:doc "Resize one image. Pass exactly one of width, height or percent; the shape is kept from that single value, and naming both width and height needs allow_distort. Never upscales unless allow_upscale. The result stays in the input's format and is written beside it unless output_path says otherwise.")
  ("path" :text :required t :doc "the image file")
  ("width" :integer :doc "target width in pixels")
  ("height" :integer :doc "target height in pixels")
  ("percent" :number :doc "scale in percent: 50 for half size")
  ("allow_distort" :boolean :default nil :doc "let width and height change the shape, not only the size")
  ("allow_upscale" :boolean :default nil :doc "let the result be larger than the input")
  ("resample" :string :default "lanczos"
              :doc "lanczos, bicubic, bilinear or nearest: lanczos averages every source pixel a result covers, bicubic and bilinear interpolate, nearest takes one sample")
  ("output_path" :string :doc "where to write; default beside the input as <stem>_resized.<ext>. Refused when it already holds a file, or equals the input, unless overwrite and confirm are both true")
  ("overwrite" :boolean :default nil :doc "allow replacing a file that is already there; needs confirm")
  ("confirm" :boolean :default nil :doc "the second key for overwrite: both must be true or nothing is written"))

(nlk:define-contract crop-arguments
  (:doc "Crop one image. Pass exactly one mode: box = [left, top, right, bottom] in pixels, checked against the image's bounds and refused rather than padded when it does not fit; aspect = 16:9, 1:1, 4:3 - the largest centred rectangle of that shape; or a centred width and/or height that must fit. A crop never creates pixels.")
  ("path" :text :required t :doc "the image file")
  ("box" :array :of :integer :doc "left, top, right, bottom in pixels, with 0 <= left < right <= width and 0 <= top < bottom <= height")
  ("aspect" :string :doc "a centred aspect crop, e.g. 1:1, 16:9, 4:3: the largest rectangle of that shape that fits")
  ("width" :integer :doc "centred crop width in pixels")
  ("height" :integer :doc "centred crop height in pixels")
  ("output_path" :string :doc "where to write; default beside the input as <stem>_cropped.<ext>")
  ("overwrite" :boolean :default nil :doc "allow replacing a file that is already there; needs confirm")
  ("confirm" :boolean :default nil :doc "the second key for overwrite"))

(nlk:define-contract rotate-arguments
  (:doc "Rotate one image counter-clockwise by angle degrees: 90, 180 and 270 move every sample exactly, any other angle grows the canvas and leaves background-coloured corners. auto_orient, on by default, first turns the pixels upright by the file's EXIF orientation; omitting angle only applies that. The result stays in the input's format.")
  ("path" :text :required t :doc "the image file")
  ("angle" :number :doc "degrees counter-clockwise: 90, 180, 270, 45; omit it to only fix the EXIF orientation")
  ("auto_orient" :boolean :default t :doc "turn the pixels upright by the file's EXIF orientation first")
  ("expand" :boolean :default t :doc "grow the canvas so an arbitrary angle cuts nothing off")
  ("background" :string :doc "the corners' colour for an arbitrary angle: white (default), black or #rrggbb; an image with alpha is left transparent unless this is set")
  ("output_path" :string :doc "where to write; default beside the input as <stem>_rotated.<ext>")
  ("overwrite" :boolean :default nil :doc "allow replacing a file that is already there; needs confirm")
  ("confirm" :boolean :default nil :doc "the second key for overwrite"))

(nlk:define-contract convert-arguments
  (:doc "Convert one image to another format. PNG keeps alpha and carries the most; JPEG and BMP carry none, so an image with real transparency needs flatten=true and is laid over background. JPEG is re-encoded at quality.")
  ("path" :text :required t :doc "the image file")
  ("format" :text :required t :slot target-format
   :doc "the target format: PNG, JPEG or BMP are written here; GIF, WEBP, TIFF and AVIF are named as not written")
  ("quality" :integer :doc "JPEG quality 1-100; the section's jpeg_quality otherwise")
  ("flatten" :boolean :default nil :doc "required to put an image with alpha into JPEG or BMP: composites onto background. Without it the call is refused rather than losing transparency")
  ("background" :string :default "white" :doc "the colour flatten composites onto: white, black or #rrggbb")
  ("output_path" :string :doc "where to write; default beside the input as <stem>_converted.<ext>")
  ("overwrite" :boolean :default nil :doc "allow replacing a file that is already there; needs confirm")
  ("confirm" :boolean :default nil :doc "the second key for overwrite"))

(nlk:define-contract optimize-arguments
  (:doc "Re-encode an image in its own format to shrink it: JPEG at a quality, and optionally cap the longest side. Reports before and after bytes and says when the result came out larger.")
  ("path" :text :required t :doc "the image file")
  ("quality" :integer :doc "JPEG quality 1-100; the section's jpeg_quality otherwise")
  ("max_dimension" :integer :doc "cap the longest side at this many pixels, averaging down and never up")
  ("output_path" :string :doc "where to write; default beside the input as <stem>_optimized.<ext>")
  ("overwrite" :boolean :default nil :doc "allow replacing a file that is already there; needs confirm")
  ("confirm" :boolean :default nil :doc "the second key for overwrite"))

;;; --- the six tools -------------------------------------------------------------------

(defun open-input (path-text)
  "PATH-TEXT opened with its pixels, after the refusals every edit makes: no
path, no file, bytes nothing opens, or more pixels than max_pixels allows."
  (let ((path (resolve-path path-text)))
    (unless path (fail "it needs a path to an image file."))
    (let* ((image (open-image path))
           (pixels (opened-image-pixels image)))
      (unless pixels
        (fail "~a: ~a" (namestring path)
              (or (opened-image-reason image) "nothing here opens it")))
      (let ((limit (* (setting :max-pixels) 1000000)))
        (when (> (* (nle::pixels-width pixels) (nle::pixels-height pixels)) limit)
          (fail "it is ~dx~d, over the ~:d megapixels max_pixels allows."
                (nle::pixels-width pixels) (nle::pixels-height pixels)
                (setting :max-pixels))))
      image)))

(defun info-text (image)
  "IMAGE described as one line of facts: what a model needs before it edits
anything, and the answer a call that found nothing to edit is repaired from."
  (let* ((pixels (opened-image-pixels image))
         (channels (and pixels (nle::pixels-channels pixels)))
         (facts (list (format nil "~a ~dx~d"
                               (or (format-label (opened-image-format image))
                                   "unknown format")
                               (opened-image-width image) (opened-image-height image))
                      (if pixels
                          (format nil "pixels decode: yes, ~a"
                                  (cond ((= channels 1) "one sample a pixel")
                                        ((= channels 3) "three samples a pixel, no alpha")
                                        ((nle::opaque-p pixels)
                                         "four samples a pixel, alpha that hides nothing")
                                        (t "four samples a pixel, with alpha")))
                          (format nil "pixels decode: no (~a)"
                                  (opened-image-reason image)))
                      (format nil "~:d bytes" (length (opened-image-bytes image))))))
    (multiple-value-bind (orientation gps) (jpeg-exif (opened-image-bytes image))
      (when orientation
        (setf facts (append facts
                            (list (format nil "EXIF orientation ~d" orientation)
                                  (if gps "EXIF carries GPS (presence only)"
                                      "no GPS"))))))
    (format nil "~a: ~{~a~^; ~}" (file-namestring (opened-image-path image)) facts)))

(defun image-info (arguments)
  "The image_info tool: what one file is, read-only."
  (guard
    (running-settings)
    (let ((path (resolve-path (info-arguments-path arguments))))
      (unless path (fail "it needs a path to an image file."))
      (info-text (open-image path)))))

(defun target-size (width height want-width want-height percent distort)
  "(values TARGET-WIDTH TARGET-HEIGHT) from what a resize call named: one of
the three, or both dimensions with DISTORT."
  (when (and percent (or want-width want-height))
    (fail "pass exactly one of width, height or percent."))
  (when (and percent (not (plusp percent)))
    (fail "percent must be over zero."))
  (cond
    (percent (values (max 1 (round (* width percent 1/100)))
                     (max 1 (round (* height percent 1/100)))))
    ((and want-width want-height)
     (unless distort
       (fail "width and height together change the shape: pass allow_distort=true, or name one of them."))
     (values want-width want-height))
    (want-width (values want-width (max 1 (round (* height want-width) width))))
    (want-height (values (max 1 (round (* width want-height) height)) want-height))
    (t (fail "it needs one of width, height or percent."))))

(defun resample-of (text)
  "The resampling TEXT names: :area for lanczos (the default), :nearest, or
:bilinear for bicubic and bilinear."
  (let ((word (string-downcase (nlk:trimmed (or text "lanczos")))))
    (cond ((string= word "nearest") :nearest)
          ((member word '("lanczos" "area" "box") :test #'string=) :area)
          ((member word '("bicubic" "bilinear" "linear") :test #'string=) :bilinear)
          (t (fail "resample takes lanczos, bicubic, bilinear or nearest, not ~s."
                   (or text ""))))))

(defun image-resize (arguments)
  "The image_resize tool."
  (guard
    (running-settings)
    (let* ((image (open-input (resize-arguments-path arguments)))
           (pixels (opened-image-pixels image))
           (width (nle::pixels-width pixels))
           (height (nle::pixels-height pixels))
           (mode (resample-of (resize-arguments-resample arguments))))
      (multiple-value-bind (tw th)
          (target-size width height
                       (resize-arguments-width arguments)
                       (resize-arguments-height arguments)
                       (resize-arguments-percent arguments)
                       (resize-arguments-allow-distort arguments))
        (unless (or (resize-arguments-allow-upscale arguments)
                    (and (<= tw width) (<= th height)))
          (fail "~dx~d is larger than the input, ~dx~d: pass allow_upscale=true to grow it."
                tw th width height))
        (let ((limit (* (setting :max-pixels) 1000000)))
          (when (> (* tw th) limit)
            (fail "~dx~d is over the ~:d megapixels max_pixels allows."
                  tw th (setting :max-pixels))))
        (format nil "image_utils: resized ~dx~d -> ~dx~d (~a, ~(~a~)); ~a"
                width height tw th (format-label (opened-image-format image)) mode
                (save-pixels image (resample-pixels pixels tw th mode)
                              (opened-image-format image) "resized"
                              :output-path (resize-arguments-output-path arguments)
                              :overwrite (resize-arguments-overwrite arguments)
                              :confirm (resize-arguments-confirm arguments)))))))

(defun crop-box (width height box aspect want-width want-height)
  "(values LEFT TOP RIGHT BOTTOM): the box a crop call named, refused when it
does not fit inside WIDTH x HEIGHT."
  (flet ((centred (rw rh)
           (let ((left (floor (- width rw) 2))
                 (top (floor (- height rh) 2)))
             (values left top (+ left rw) (+ top rh)))))
    (cond
      (box
       (let ((box (coerce box 'list)))
         (unless (= 4 (length box))
           (fail "box takes four numbers: left, top, right, bottom."))
         (destructuring-bind (left top right bottom) box
           (unless (and (integerp left) (integerp top)
                        (integerp right) (integerp bottom))
             (fail "box takes four whole numbers: left, top, right, bottom."))
           (unless (and (<= 0 left) (< left right) (<= right width)
                        (<= 0 top) (< top bottom) (<= bottom height))
             (fail "box ~a is not inside ~dx~d: 0 <= left < right <= ~d and 0 <= top < bottom <= ~d."
                   box width height width height))
           (values left top right bottom))))
      (aspect
       (let* ((colon (position #\: aspect))
              (aw (and colon (ignore-errors (parse-integer aspect :end colon))))
              (ah (and colon (ignore-errors (parse-integer aspect :start (1+ colon))))))
         (unless (and aw ah (plusp aw) (plusp ah))
           (fail "aspect takes two numbers with a colon, as 16:9 or 1:1, not ~s." aspect))
         (let ((wide (<= (/ width aw) (/ height ah))))
           (multiple-value-bind (left top right bottom)
               (if wide
                   (centred width (min height (round (* width ah) aw)))
                   (centred (min width (round (* height aw) ah)) height))
             (values left top right bottom)))))
      ((or want-width want-height)
       (let ((rw (or want-width width))
             (rh (or want-height height)))
         (when (or (> rw width) (> rh height))
           (fail "~dx~d does not fit inside ~dx~d: a crop never creates pixels."
                 rw rh width height))
         (multiple-value-bind (left top right bottom) (centred rw rh)
           (values left top right bottom))))
      (t (fail "it needs one of box, aspect, or width and/or height.")))))

(defun image-crop (arguments)
  "The image_crop tool."
  (guard
    (running-settings)
    (let* ((image (open-input (crop-arguments-path arguments)))
           (pixels (opened-image-pixels image))
           (width (nle::pixels-width pixels))
           (height (nle::pixels-height pixels)))
      (multiple-value-bind (left top right bottom)
          (crop-box width height
                    (crop-arguments-box arguments)
                    (crop-arguments-aspect arguments)
                    (crop-arguments-width arguments)
                    (crop-arguments-height arguments))
        (format nil "image_utils: cropped ~dx~d -> ~dx~d from ~d,~d; ~a"
                width height (- right left) (- bottom top) left top
                (save-pixels image (crop-pixels pixels left top right bottom)
                              (opened-image-format image) "cropped"
                              :output-path (crop-arguments-output-path arguments)
                              :overwrite (crop-arguments-overwrite arguments)
                              :confirm (crop-arguments-confirm arguments)))))))

(defun image-rotate (arguments)
  "The image_rotate tool: EXIF uprighting first when asked, then the angle."
  (guard
    (running-settings)
    (let* ((image (open-input (rotate-arguments-path arguments)))
           (pixels (opened-image-pixels image))
           (angle (rotate-arguments-angle arguments))
           (auto (rotate-arguments-auto-orient arguments))
           (expand (rotate-arguments-expand arguments))
           (background (rotate-arguments-background arguments))
           (orientation 1)
           (note ""))
      (when (and auto (eq (opened-image-format image) :jpeg))
        (setf orientation (nle::jpeg-orientation (opened-image-bytes image)))
        (unless (= orientation 1)
          (setf pixels (nle::orient-pixels pixels orientation)
                note (format nil "EXIF orientation ~d applied; " orientation))))
      (when angle
        (let ((turns (mod (round (/ angle 90)) 4)))
          (setf pixels
                (cond ((and (zerop (mod angle 90)) (plusp turns))
                       (rotate-quarter pixels turns))
                      ((zerop (mod angle 90)) pixels)
                      (t (rotate-any pixels angle background expand)))
                note (format nil "~a~d degrees counter-clockwise; " note angle))))
      (when (and (null angle) (= orientation 1))
        (return-from image-rotate
          (format nil "image_utils: ~a carries no EXIF orientation and no angle was given: nothing to write."
                  (file-namestring (opened-image-path image)))))
      (format nil "image_utils: rotated, ~a~dx~d; ~a"
              note (nle::pixels-width pixels) (nle::pixels-height pixels)
              (save-pixels image pixels (opened-image-format image) "rotated"
                            :output-path (rotate-arguments-output-path arguments)
                            :overwrite (rotate-arguments-overwrite arguments)
                            :confirm (rotate-arguments-confirm arguments))))))

(defun image-convert (arguments)
  "The image_convert tool."
  (guard
    (running-settings)
    (let* ((image (open-input (convert-arguments-path arguments)))
           (pixels (opened-image-pixels image))
           (format (format-from-text (convert-arguments-target-format arguments)))
           (flatten (convert-arguments-flatten arguments))
           (background (or (convert-arguments-background arguments) "white"))
           (quality (convert-arguments-quality arguments)))
      (unless format
        (fail "format takes PNG, JPEG or BMP, which this writes, or GIF, WEBP, TIFF and AVIF, which it names as not written."))
      (unless (member format +writable+)
        (fail "~a is not a format this cell writes: PNG, JPEG and BMP are."
              (format-label format)))
      (when (and (member format '(:jpeg :bmp))
                 (= (nle::pixels-channels pixels) 4)
                 (not (nle::opaque-p pixels))
                 (not flatten))
        (fail "the image carries alpha and ~a cannot: pass flatten=true to lay it over background, or convert to PNG."
              (format-label format)))
      (format nil "image_utils: ~a -> ~a, ~dx~d~@[ at quality ~d~]; ~a"
              (format-label (opened-image-format image)) (format-label format)
              (nle::pixels-width pixels) (nle::pixels-height pixels)
              (and (eq format :jpeg) (quality-of quality))
              (save-pixels image pixels format "converted"
                            :quality quality :background background
                            :output-path (convert-arguments-output-path arguments)
                            :overwrite (convert-arguments-overwrite arguments)
                            :confirm (convert-arguments-confirm arguments))))))

(defun image-optimize (arguments)
  "The image_optimize tool: the same picture, re-encoded smaller."
  (guard
    (running-settings)
    (let* ((image (open-input (optimize-arguments-path arguments)))
           (pixels (opened-image-pixels image))
           (format (opened-image-format image))
           (quality (quality-of (optimize-arguments-quality arguments)))
           (cap (optimize-arguments-max-dimension arguments))
           (width (nle::pixels-width pixels))
           (height (nle::pixels-height pixels))
           (cap-note ""))
      (unless (member format +writable+)
        (fail "this re-encodes PNG, JPEG and BMP in their own format; convert a ~a instead."
              (format-label format)))
      (when (and cap (plusp cap) (> (max width height) cap))
        (let ((scale (/ cap (float (max width height)))))
          (setf pixels (resample-pixels pixels
                                        (max 1 (round (* width scale)))
                                        (max 1 (round (* height scale)))
                                        :area)
                cap-note (format nil ", capped to ~dx~d"
                                  (nle::pixels-width pixels)
                                  (nle::pixels-height pixels)))))
      (format nil "image_utils: re-encoded ~a ~dx~d~a; ~a"
              (format-label format) width height cap-note
              (save-pixels image pixels format "optimized" :quality quality
                            :output-path (optimize-arguments-output-path arguments)
                            :overwrite (optimize-arguments-overwrite arguments)
                            :confirm (optimize-arguments-confirm arguments))))))

;;; --- /image-utils ---------------------------------------------------------------------

(defun report-text ()
  "What /image-utils says bare: what it reads and writes, the section's
settings, and how many files it has written since it started."
  (format nil "image-utils: reads PNG, JPEG, GIF, WebP and BMP; writes PNG, JPEG and BMP ~
               (a format it cannot write is named in the answer, not guessed at) ~
               - max_pixels ~d, jpeg_quality ~d; results land beside their input unless ~
               output_path says otherwise; ~d file~:p written since start"
          (setting :max-pixels) (setting :jpeg-quality) (getf *stats* :written)))

(defun run-slash (args session-id)
  "The /image-utils command: bare the report; `info PATH` the facts about one
file, writing nothing."
  (declare (ignore session-id))
  (running-settings)
  (let* ((words (nlk:split-words (or args "")))
         (word (first words)))
    (cond
      ((null word) (report-text))
      ((string-equal word "info")
       (let ((path (second words)))
         (unless path (fail "info takes a path: /image-utils info photo.png"))
         (guard (info-text (open-image (resolve-path path))))))
      (t (format nil "image-utils: ~s is not one of its words - info PATH, or nothing for the report."
                 word)))))

;;; --- the manual ------------------------------------------------------------------------

(defparameter +manual+
  "image-utils: six local image tools on one file at a time, with nothing leaving this process. image_info reads a file's format, size, samples and EXIF orientation - and whether GPS is present, never where - and writes nothing. image_resize takes exactly one of width, height or percent, keeps the shape from that one value, refuses to upscale unless allow_upscale, and names both dimensions only with allow_distort. image_crop takes box = [left, top, right, bottom] checked against the bounds, or a centred aspect such as 16:9, or a centred width and/or height that must fit. image_rotate turns counter-clockwise: 90, 180 and 270 move every sample exactly, another angle grows the canvas, and auto_orient first applies the file's EXIF orientation. image_convert writes PNG, JPEG or BMP - alpha into JPEG or BMP must be declared with flatten=true, laid over background - and image_optimize re-encodes in the input's own format, optionally capping the longest side. Every write lands beside its input as <stem>_<operation>.<ext> unless output_path says otherwise, and a target that already holds a file is refused unless overwrite and confirm are both true. Reads are PNG, GIF, WebP and JPEG through the engine's own codecs and BMP here; a format none of them opens goes through whichever of ImageMagick, ffmpeg or sips the machine has, and the answer says so when there is none. /image-utils reports the settings and what it has written; /image-utils info PATH answers one file's facts."
  "What (help :image-utils) answers while the cell runs.")

;;; --- the cell ---------------------------------------------------------------------------

(nle:define-cell image-utils
  (:section ("image-utils")
    (:guide "nothing here is required: an image is opened at up to 64 megapixels, JPEG is written at quality 85, and every result lands beside its input unless output_path says otherwise")
    ("max_pixels" :integer :default 64 :min 1
                  :doc "the megapixels one image is opened at")
    ("jpeg_quality" :integer :default 85 :min 1
                    :doc "the quality image_convert and image_optimize write JPEG at, 1 to 100 (a value over 100 is read as 100)"))
  (:tool "image_info" (nlk:contract 'info-arguments) 'image-info)
  (:tool "image_resize" (nlk:contract 'resize-arguments) 'image-resize)
  (:tool "image_crop" (nlk:contract 'crop-arguments) 'image-crop)
  (:tool "image_rotate" (nlk:contract 'rotate-arguments) 'image-rotate)
  (:tool "image_convert" (nlk:contract 'convert-arguments) 'image-convert)
  (:tool "image_optimize" (nlk:contract 'optimize-arguments) 'image-optimize)
  (:command "image-utils" 'run-slash
            :description "What the image tools read and write, and what they have written"
            :argument-hint "info PATH")
  (:help :image-utils
         "image-utils: inspect, resize, crop, rotate, convert and shrink one local image - /image-utils reports"
         +manual+)
  (:start (lambda ()
            (setf *stats* '(:written 0)))))
