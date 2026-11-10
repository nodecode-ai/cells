;;;; render.lisp --- source in, PNG out, with no image library and no network.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two of the original's three source kinds, rendered locally:
;;;;
;;;; * ``pixel``: a palette-grid sprite spec, drawn here as a PNG written
;;;;   byte by byte (stored deflate, no compression library, nothing
;;;;   executed).
;;;; * ``svg``: a literal SVG document, rasterised by the first program
;;;;   found on PATH -- rsvg-convert, resvg, magick, convert, cairosvg or
;;;;   inkscape -- since an SVG renderer is a C library, not a Lisp. A
;;;;   stubbed rasteriser is what the tests run.
;;;;
;;;; The original's third kind, a Python drawing program in a sandboxed
;;;; interpreter, is left out: the session's own model writes the source, so
;;;; no code of the model's runs anywhere.
;;;;
;;;; Both kinds expand seeded tokens before drawing -- {{seed}},
;;;; {{rand:0:100}}, {{pick:a,b,c}} and {{gem}} or {{palette.gem}} for a
;;;; palette role -- so a re-roll with a new seed is a real variation of the
;;;; same source, with no model call.

(in-package #:nodecode-procgen-studio)

;;; --- a PNG, written by hand ----------------------------------------------------------

(defparameter *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (declare (ignore k))
          (setf c (if (logbitp 0 c) (logxor #xedb88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) (logand #xffffffff c)))))
  "CRC-32's table, the polynomial PNG's chunks are checked with.")

(defun crc32 (octets)
  "The CRC-32 of OCTETS, as PNG writes it."
  (let ((crc #xffffffff))
    (dotimes (i (length octets))
      (setf crc (logxor (aref *crc-table* (logand #xff (logxor crc (aref octets i))))
                         (ash crc -8))))
    (logand #xffffffff (logxor crc #xffffffff))))

(defun adler32 (octets)
  "The Adler-32 of OCTETS, the checksum zlib's header ends with."
  (let ((a 1) (b 0))
    (dotimes (i (length octets))
      (setf a (mod (+ a (aref octets i)) 65521)
            b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

(defun be-octets (value count)
  "VALUE as COUNT big-endian octets."
  (let ((out (make-array count :element-type '(unsigned-byte 8))))
    (loop for i from (1- count) downto 0
          do (setf (aref out i) (logand #xff value)
                   value (ash value -8)))
    out))

(defun octets (&rest parts)
  "PARTS -- octet vectors and lists of numbers -- as one octet vector."
  (let* ((size (reduce #'+ parts :key (lambda (p) (length p))))
         (out (make-array size :element-type '(unsigned-byte 8)))
         (at 0))
    (dolist (part parts out)
      (if (vectorp part)
          (progn (replace out part :start1 at) (incf at (length part)))
          (dolist (byte part) (setf (aref out at) byte) (incf at))))))

(defun png-chunk (type data)
  "One PNG chunk: its length, TYPE, DATA and the CRC over both."
  (let* ((head (map 'vector #'char-code type))
         (body (octets head data)))
    (octets (be-octets (length data) 4) body (be-octets (crc32 body) 4))))

(defun zlib-stored (raw)
  "RAW as a zlib stream with stored (uncompressed) deflate blocks."
  (let ((blocks '())
        (at 0)
        (size (length raw)))
    (loop
      (let* ((left (- size at))
             (take (min left 65535))
             (final (if (>= (+ at take) size) 1 0)))
        (push (octets (list final)
                      (list (logand #xff take) (logand #xff (ash take -8)))
                      (list (logand #xff (lognot take)) (logand #xff (ash (lognot take) -8)))
                      (subseq raw at (+ at take)))
              blocks)
        (incf at take)
        (when (>= at size) (return))))
    ;; One vector a part: the blocks are collected with APPLY, never as a list
    ;; inside a list, which OCTETS would read as numbers.
    (apply #'octets (append (list (list #x78 #x01)) (nreverse blocks)
                            (list (be-octets (adler32 raw) 4))))))

(defun write-png (path width height pixel)
  "Write a PNG at PATH, WIDTH by HEIGHT, taking each pixel's colour from
PIXEL, called with (x y) and answering (values RED GREEN BLUE)."
  (let* ((stride (1+ (* 3 width)))
         (raw (make-array (* stride height) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (let ((base (* y stride)))
        (setf (aref raw base) 0)
        (dotimes (x width)
          (multiple-value-bind (red green blue) (funcall pixel x y)
            (let ((at (+ base 1 (* 3 x))))
              (setf (aref raw at) red
                    (aref raw (+ at 1)) green
                    (aref raw (+ at 2)) blue))))))
    (let ((png (octets (list #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)
                       (png-chunk "IHDR" (octets (be-octets width 4) (be-octets height 4)
                                                  (list 8 2 0 0 0)))
                       (png-chunk "IDAT" (zlib-stored raw))
                       (png-chunk "IEND" #()))))
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
        (write-sequence png out))
      png)))

;;; --- colour --------------------------------------------------------------------------

(defun hex-rgb (value)
  "VALUE -- #rrggbb -- as (values RED GREEN BLUE)."
  (unless (and (stringp value) (= 7 (length value)) (char= #\# (char value 0)))
    (fail "~s is not a #rrggbb colour" value))
  (values (parse-integer value :start 1 :end 3 :radix 16)
          (parse-integer value :start 3 :end 5 :radix 16)
          (parse-integer value :start 5 :end 7 :radix 16)))

(defun role-colour (role style)
  "ROLE's colour in STYLE, or a refusal naming the roles."
  (let ((role (or (find role *palette-roles* :test #'string-equal)
                  (fail "unknown palette role ~s (roles: ~{~a~^, ~})" role *palette-roles*))))
    (or (cdr (assoc role (style-palette style) :test #'string=))
        (fail "the palette has no ~a" role))))

(defun drawn-colour (name style)
  "NAME -- a palette role or a #hex colour -- as a colour."
  (if (and (plusp (length name)) (char= #\# (char name 0)))
      name
      (role-colour name style)))

;;; --- seeded tokens -------------------------------------------------------------------

(defun seed-state (seed)
  "The generator one drawing's tokens are drawn from, as a cons of one fixnum."
  (let ((state (logand #xffffffff (if (integerp seed) (abs seed) 0))))
    (cons (if (zerop state) #x9e3779b9 state) nil)))

(defun rng-next (state)
  "Advance STATE and answer a number in 0 .. 2^32-1."
  (let ((x (car state)))
    (setf x (logand #xffffffff (logxor x (ash x 13)))
          x (logand #xffffffff (logxor x (ash x -17)))
          x (logand #xffffffff (logxor x (ash x 5))))
    (setf (car state) x)
    x))

(defun token-value (token state style seed)
  "TOKEN's replacement: the seed, a seeded number, a seeded pick or a colour."
  (let ((token (string-trim '(#\Space #\Tab) token)))
    (cond
      ((string-equal token "seed") (princ-to-string seed))
      ((uiop:string-prefix-p "rand:" token)
       (let* ((rest (subseq token 5))
              (colon (position #\: rest)))
         (unless colon (fail "{{rand:MIN:MAX}} needs two numbers, got {{~a}}" token))
         (let ((low (parse-integer rest :end colon :junk-allowed t))
               (high (parse-integer rest :start (1+ colon) :junk-allowed t)))
           (unless (and low high (< low high))
             (fail "{{rand:MIN:MAX}} needs MIN < MAX, got {{~a}}" token))
           (princ-to-string (+ low (mod (rng-next state) (1+ (- high low))))))))
      ((uiop:string-prefix-p "pick:" token)
       (let* ((items (uiop:split-string (subseq token 5) :separator (list #\,)))
              (count (length items)))
         (when (zerop count) (fail "{{pick:a,b,c}} needs at least one choice"))
         (string-trim '(#\Space) (nth (mod (rng-next state) count) items))))
      ((uiop:string-prefix-p "palette." token)
       (drawn-colour (role-colour (subseq token 8) style) style))
      (t (drawn-colour (role-colour token style) style)))))

(defun expand-tokens (text style seed)
  "TEXT with every {{token}} replaced: the seed, a seeded number, a seeded
pick or a palette role's colour. A token nobody knows is a refusal."
  (let ((state (seed-state seed))
        (out (make-string-output-stream))
        (at 0))
    (loop
      (let ((start (search "{{" text :start2 at)))
        (if (null start)
            (progn (write-string (subseq text at) out) (return))
            (let ((end (search "}}" text :start2 (+ start 2))))
              (unless end (fail "the source has an unclosed {{ at character ~d" start))
              (write-string (subseq text at start) out)
              (write-string (token-value (subseq text (+ start 2) end) state style seed) out)
              (setf at (+ end 2))))))
    (get-output-stream-string out)))

;;; --- a pixel-grid source --------------------------------------------------------------

(defun spec-object (source)
  "SOURCE -- a pixel spec -- as one decoded JSON object."
  (let ((spec (ignore-errors (nlk:decode-json source))))
    (unless (hash-table-p spec)
      (fail "a pixel-grid source is one JSON object: {~s}" "\"palette\": {...}, \"frames\": [[\"..\", ...]]}"))
    spec))

(defun spec-frames (spec)
  "SPEC's frames, each a list of row strings; a bare sprite reads as one frame."
  (let* ((frames (nlk:json-value spec :array "frames"))
         (sprite (nlk:json-value spec :array "sprite"))
         (raw (cond (frames (coerce frames 'list))
                    (sprite (list (coerce sprite 'list)))
                    (t (fail "a pixel spec needs \"frames\" (a list of frames) or \"sprite\" (one frame of rows)"))))
         (rows (mapcar (lambda (frame)
                         (mapcar (lambda (row)
                                   (unless (stringp row) (fail "every pixel row is a string of one character a cell"))
                                   row)
                                 (coerce frame 'list)))
                       raw)))
    (when (some #'null rows) (fail "a pixel frame needs at least one row"))
    rows))

(defun spec-cell-width (ch palette)
  "CH's colour name in PALETTE, or NIL for a background cell."
  (cond ((member ch '(#\. #\Space) :test #'char=) nil)
        (t (or (gethash (string ch) palette)
               (gethash (string (char-upcase ch)) palette)))))

(defun draw-pixel-source (source style seed width height path)
  "Render SOURCE -- a pixel-grid spec -- at PATH. => (values WIDTH HEIGHT FRAMES)."
  (declare (ignore seed))
  (let* ((spec (spec-object source))
         (frames (spec-frames spec))
         (palette (or (nlk:json-value spec :object "palette")
                      (nlk:make-json-object)))
         (frame-width (reduce #'max (mapcar (lambda (frame) (reduce #'max (mapcar #'length frame))) frames)))
         (frame-height (reduce #'max (mapcar #'length frames)))
         (cell (max 1 (floor (min (/ width (* frame-width (length frames))) (/ height frame-height)))))
         (image-width (* cell frame-width (length frames)))
         (image-height (* cell frame-height))
         (background (let ((name (nlk:json-value spec :string "background")))
                       (cond ((or (null name) (string= name "checker")) :checker)
                             (t (drawn-colour name style))))))
    (flet ((background-at (x y)
             (if (eq background :checker)
                 (multiple-value-bind (red green blue)
                     (hex-rgb (role-colour (if (zerop (mod (+ (floor x (* 2 cell)) (floor y (* 2 cell))) 2))
                                               "bg" "bg2")
                                            style))
                   (values red green blue))
                 (hex-rgb background))))
      (write-png path image-width image-height
                 (lambda (x y)
                   (let* ((frame (floor x (* cell frame-width)))
                          (row (nth (floor y cell) (nth frame frames)))
                          (cx (floor (mod x (* cell frame-width)) cell))
                          (ch (and row (< cx (length row)) (char row cx)))
                          (name (and ch (spec-cell-width ch palette))))
                     (if name
                         (hex-rgb (drawn-colour (string name) style))
                         (background-at x y))))))
    (values image-width image-height (length frames))))

;;; --- an SVG source, rasterised by the user's own program ------------------------------

(defparameter *rasterize* :auto
  "How an SVG source becomes a PNG: :AUTO finds the first program of
*RASTERIZER-CANDIDATES* on PATH and keeps it, a FUNCTION is called with
(SVG-PATH PNG-PATH WIDTH HEIGHT) -- which is what the tests stub -- a cons
of (NAME . BUILDER) runs that program, and NIL refuses every SVG source.")

(defparameter *rasterizer-candidates*
  (list (cons "rsvg-convert"
              (lambda (svg png width height)
                (list "rsvg-convert" "--width" (princ-to-string width)
                      "--height" (princ-to-string height) "--output" png svg)))
        (cons "resvg"
              (lambda (svg png width height)
                (list "resvg" "--width" (princ-to-string width)
                      "--height" (princ-to-string height) svg png)))
        (cons "magick"
              (lambda (svg png width height)
                (list "magick" "-background" "none" svg "-resize"
                      (format nil "~dx~d!" width height) png)))
        (cons "convert"
              (lambda (svg png width height)
                (list "convert" "-background" "none" svg "-resize"
                      (format nil "~dx~d!" width height) png)))
        (cons "cairosvg"
              (lambda (svg png width height)
                (list "cairosvg" svg "--output" png
                      "--output-width" (princ-to-string width)
                      "--output-height" (princ-to-string height))))
        (cons "inkscape"
              (lambda (svg png width height)
                (list "inkscape" svg (format nil "--export-filename=~a" png)
                      "--export-width" (princ-to-string width)
                      "--export-height" (princ-to-string height)))))
  "An SVG renderer, in preference order, and how each takes its size.")

(defun detect-rasterizer ()
  "The first candidate on PATH, as (NAME . BUILDER), or NIL."
  (loop for (name . builder) in *rasterizer-candidates*
        when (eql 0 (ignore-errors (nth-value 2 (nlk:run-bounded (list name "--version") :seconds 10))))
          return (cons name builder)))

(defun ensure-rasterizer ()
  "What *RASTERIZE* resolves to now, finding a program once."
  (when (eq *rasterize* :auto)
    (setf *rasterize* (or (detect-rasterizer) :none)))
  *rasterize*)

(defun rasterizer-name ()
  "The program SVG sources are rasterised by, or NIL."
  (let ((how (ensure-rasterizer)))
    (and (consp how) (car how))))

(defun rasterize-svg (svg png width height)
  "Turn the SVG at SVG into a PNG at PNG, WIDTH by HEIGHT."
  (let ((how (ensure-rasterizer)))
    (cond
      ((functionp how) (funcall how svg png width height))
      ((consp how)
       (multiple-value-bind (out err status)
           (handler-case
               (nlk:run-bounded (funcall (cdr how) svg png width height)
                                :seconds (setting :render-timeout))
             (error (error)
               (fail "~a could not be run: ~a" (car how) error)))
         (declare (ignore out))
         (unless (eql 0 status)
           (fail "~a could not draw the SVG (exit ~a~@[: ~a~])"
                 (car how) status (and (stringp err) (nlk:trimmed err))))))
      (t (fail "an SVG source needs a rasteriser on PATH -- rsvg-convert, resvg, magick, convert, ~
cairosvg or inkscape -- and none was found; a pixel-grid source needs none")))))

;;; --- one source, whichever kind it is ---------------------------------------------------

(defun source-kind (source)
  "SOURCE's kind: :SVG for a document, :PIXEL for a palette grid."
  (let ((head (string-left-trim '(#\Space #\Tab #\Newline #\Return) source)))
    (cond ((or (uiop:string-prefix-p "<svg" head) (uiop:string-prefix-p "<?xml" head)) :svg)
          ((and (uiop:string-prefix-p "{" head) (search "\"frames\"" head)) :pixel)
          ((and (uiop:string-prefix-p "{" head) (search "\"sprite\"" head)) :pixel)
          (t (fail "the source is neither an SVG document (a string starting <svg) nor a pixel-grid ~
spec (one JSON object with \"frames\" or \"sprite\" and a \"palette\")")))))

(defun draw-source (source style seed width height directory)
  "Write SOURCE -- of whichever kind -- under DIRECTORY and render it to
image.png there, expanding its seeded tokens first.
 => (values SOURCE-PATH PNG-PATH WIDTH HEIGHT RENDERER FRAMES)."
  (let* ((kind (source-kind source))
         (expanded (expand-tokens source style seed))
         (source-path (merge-pathnames (if (eq kind :svg) "source.svg" "source.pixel.json") directory))
         (drawn-path (merge-pathnames (if (eq kind :svg) "render.svg" "render.pixel.json") directory))
         (png-path (merge-pathnames "image.png" directory)))
    (ensure-directories-exist source-path)
    ;; The raw source is what is kept -- tokens and all -- so a re-roll with a
    ;; new seed draws the same program with new numbers in it.
    (with-open-file (out source-path :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-string source out))
    ;; The expanded text is written beside the source: SOURCE-PATH is what to
    ;; edit, DRAWN-PATH is exactly what was drawn, which is what makes a seed
    ;; reproducible and a re-roll checkable.
    (with-open-file (out drawn-path :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-string expanded out))
    (ecase kind
      (:pixel
       (multiple-value-bind (image-width image-height frames)
           (draw-pixel-source expanded style seed width height png-path)
         (values source-path png-path image-width image-height "pixel" frames)))
      (:svg
       (rasterize-svg (namestring drawn-path) (namestring png-path) width height)
       (values source-path png-path width height
               (or (rasterizer-name) "svg") 1)))))
