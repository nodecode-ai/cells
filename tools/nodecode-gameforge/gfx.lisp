;;;; gfx.lisp --- pixels: palettes, a sprite from a mask, a tileset from a
;;;; noise lattice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original draws its characters and items from masks and tints them
;;;; from a hue-shifted ramp (shadows lean cool, highlights lean warm), which
;;;; is most of what separates pixel art from tinted squares. The masks are
;;;; the rewrite's -- the archetype, monster and item names are the
;;;; original's -- and every pixel of them is a function of the seed.

(in-package #:nodecode-gameforge)

;;; --- colour ------------------------------------------------------------------

(defun hsv->rgb (hue saturation value)
  "HUE in 0..1, SATURATION and VALUE in 0..1, as (r g b) 0-255."
  (let* ((h (mod hue 1.0d0))
         (s (max 0.0d0 (min 1.0d0 saturation)))
         (v (max 0.0d0 (min 1.0d0 value)))
         (sector (floor (* h 6.0d0)))
         (fraction (- (* h 6.0d0) sector))
         (p (* v (- 1.0d0 s)))
         (q (* v (- 1.0d0 (* s fraction))))
         (t* (* v (- 1.0d0 (* s (- 1.0d0 fraction))))))
    (flet ((to-byte (component) (round (* 255.0d0 component))))
      (let ((components (case (mod sector 6)
                          (0 (list v t* p)) (1 (list q v p)) (2 (list p v t*))
                          (3 (list p q v)) (4 (list t* p v)) (5 (list v p q)))))
        (list (to-byte (first components)) (to-byte (second components)) (to-byte (third components)))))))

(defun rgb-hue (r g b)
  "The hue of a colour, 0..1."
  (let* ((red (/ (coerce r 'double-float) 255.0d0))
         (green (/ (coerce g 'double-float) 255.0d0))
         (blue (/ (coerce b 'double-float) 255.0d0))
         (high (max red green blue)) (low (min red green blue)) (span (- high low)))
    (if (zerop span)
        0.0d0
        (/ (mod (cond ((= high red) (/ (- green blue) span))
                     ((= high green) (+ 2.0d0 (/ (- blue red) span)))
                     (t (+ 4.0d0 (/ (- red green) span))))
              6.0d0)
           6.0d0))))

(defun hex->rgb (text)
  "#rrggbb or rrggbb as (r g b)."
  (let* ((hex (string-left-trim "#" (string text)))
         (digit (lambda (char) (or (digit-char-p char 16) (fail "~a is not a hex colour" text)))))
    (when (< (length hex) 6) (fail "~a is not a hex colour; write it as #rrggbb" text))
    (list (+ (* 16 (funcall digit (char hex 0))) (funcall digit (char hex 1)))
          (+ (* 16 (funcall digit (char hex 2))) (funcall digit (char hex 3)))
          (+ (* 16 (funcall digit (char hex 4))) (funcall digit (char hex 5))))))

(defun rgb->hex (color)
  (format nil "#~2,'0x~2,'0x~2,'0x" (first color) (second color) (third color)))

(defparameter +palettes+
  '(("pico8" . ("1d2b53" "7e2553" "008751" "ab5236" "5f574f" "c2c3c7" "fff1e8" "ff004d"
                "ffa300" "ffec27" "00e436" "29adff" "83769c" "ff77a8" "ffccaa" "000000"))
    ("sweetie16" . ("1a1c2c" "5d275d" "b13e53" "ef7d57" "ffcd75" "a7f070" "38b764" "257179"
                    "29366f" "3b5dc9" "41a6f6" "73eff7" "f4f4f4" "94b0c2" "566c86" "333c57"))
    ("endesga32" . ("be4a2f" "d77643" "ead4aa" "e4a672" "b86f50" "733e39" "3e2731" "a22633"
                    "e43b44" "f77622" "feae34" "fee761" "63c74d" "3e8948" "265c42" "193c3e"
                    "124e89" "0099db" "2ce8f5" "ffffff" "c0cbdc" "8b9bb4" "5a6988" "3a4466"
                    "262b44" "181425" "ff0044" "68386c" "b55088" "f6757a" "e8b796" "c28569"))
    ("gameboy" . ("0f380f" "306230" "8bac0f" "9bbc0f"))
    ("nes" . ("000000" "fcfcfc" "bcbcbc" "7c7c7c" "3cbcfc" "0078f8" "0000fc" "6888fc"
               "f8b8f8" "f878f8" "f85898" "e40058" "f0d0b0" "f87858" "f83800" "fce0a8"
               "fca044" "e45c10" "f8d878" "f8b800" "ac7c00" "d8f878" "b8f818" "00b800"
               "58d854" "00a800" "b8f8d8" "58f898" "00a844" "00fcfc" "00e8d8" "008888")))
  "The palettes a call may quantize to: the original's, and the same names.")

(defun palette-names () (mapcar #'car +palettes+))

(defun palette-colors (name)
  "NAME's colours as a list of (r g b), or a refusal naming the palettes there are."
  (let ((found (assoc (string-downcase (string name)) +palettes+ :test #'string=)))
    (unless found
      (fail "~a is not a palette this cell carries; choose from ~{~a~^, ~}" name (palette-names)))
    (mapcar #'hex->rgb (cdr found))))

(defparameter +hues+
  '(("red" . 0.0) ("orange" . 0.07) ("gold" . 0.12) ("yellow" . 0.15) ("lime" . 0.25)
    ("green" . 0.33) ("teal" . 0.47) ("cyan" . 0.5) ("blue" . 0.6) ("indigo" . 0.68)
    ("purple" . 0.75) ("magenta" . 0.83) ("pink" . 0.92) ("brown" . 0.07))
  "The colour names a call may give, and the hue each names.")

(defun hue-of (colour default)
  "COLOUR (a name, #rrggbb, or NIL) as a hue in 0..1."
  (cond ((or (null colour) (and (stringp colour) (zerop (length (nlk:trimmed colour))))) default)
        ((stringp colour)
         (let ((found (assoc (string-downcase (nlk:trimmed colour)) +hues+ :test #'string=)))
           (if found
               (cdr found)
               (destructuring-bind (r g b) (hex->rgb colour) (rgb-hue r g b)))))
        (t (mod (coerce colour 'double-float) 1.0d0))))

(defun ramp (hue &key (steps 4) (saturation 0.65d0) (value 0.75d0))
  "A dark to light ramp, hue shifted the way pixel art shifts it: shadows
cool, highlights warm."
  (loop for index below steps
        for t* = (/ (coerce index 'double-float) (max 1 (1- steps)))
        for shift = (* (- t* 0.5d0) 0.08d0)
        for h = (if (< 0.08d0 (mod hue 1.0d0) 0.6d0) (+ hue shift) (- hue shift))
        collect (hsv->rgb h (* saturation (- 1.1d0 (* 0.35d0 t*)))
                             (* value (+ 0.45d0 (* 0.7d0 t*))))))

(defun shade (color amount)
  "COLOR darkened (AMOUNT negative) or lightened, keeping its hue."
  (destructuring-bind (r g b) color
    (list (max 0 (min 255 (round (* r (+ 1.0d0 amount)))))
          (max 0 (min 255 (round (* g (+ 1.0d0 amount)))))
          (max 0 (min 255 (round (* b (+ 1.0d0 amount))))))))

(defun nearest-palette-color (color palette)
  "COLOR snapped to the closest of PALETTE, by redmean distance."
  (let ((best nil) (best-distance most-positive-fixnum))
    (destructuring-bind (r g b) color
      (dolist (candidate palette)
        (destructuring-bind (pr pg pb) candidate
          (let* ((red-mean (/ (+ r pr) 2.0d0))
                 (dr (- r pr)) (dg (- g pg)) (db (- b pb))
                 (distance (+ (* (+ 2.0d0 (/ red-mean 256.0d0)) dr dr)
                              (* 4.0d0 dg dg)
                              (* (+ 2.0d0 (/ (- 255.0d0 red-mean) 256.0d0)) db db))))
            (when (< distance best-distance)
              (setf best candidate best-distance distance))))))
    (or best color)))

;;; --- a mask is the drawing ---------------------------------------------------
;;; One character a pixel: `.` is transparent, `o` an outline, `e` an eye, and
;;; the letters name a region the palette tints. A mask is mirrored onto the
;;; canvas, so every sprite is symmetric and any archetype is eight columns
;;; wide.

(defparameter +archetypes+
  '(("hero" . (:hair "spiky" :body "blue" :legs "brown"))
    ("knight" . (:hair "helmet" :body "blue" :legs "indigo"))
    ("mage" . (:hair "hood" :body "purple" :legs "indigo"))
    ("rogue" . (:hair "hood" :body "teal" :legs "brown"))
    ("villager" . (:hair "short" :body "lime" :legs "brown"))
    ("princess" . (:hair "long" :body "pink" :legs "pink"))
    ("healer" . (:hair "long" :body "cyan" :legs "teal"))))

(defun archetype-names () (mapcar #'car +archetypes+))

(defparameter +character-mask+
  '("........"
    "..hhhh.."
    ".hhhhhh."
    ".hssssh."
    ".sesses."
    ".ssssss."
    "..ssss.."
    ".bbbbbb."
    "bbbbbbbb"
    ".bbbbbb."
    "..llll.."
    ".ll..ll.")
  "A villager's silhouette: hair, skin, eyes, body, legs.")

(defparameter +monsters+
  '(("slime" . ("........" "........" "..5555.." ".555555." "5e5555e5" "55555555"
                 "55555555" "55555555" "55555555" ".555555." "..4444.." "........"))
    ("bat" . ("........" "5......5" "55....55" "55555555" "5e5555e5" ".555555."
               "..5555.." "..5555.." "...55..." "........" "........" "........"))
    ("ghost" . ("........" "..5555.." ".555555." "55555555" "55e55e55" "55555555"
                 "55555555" "55555555" "55555555" "55555555" "5.5555.5" "........"))
    ("beast" . ("........" "..5....." ".555.55." ".5555555" "5e55555." "55555555"
                 ".555555." "55555555" "55555555" "5.5555.5" "55....55" "........"))
    ("golem" . ("........" "..5555.." ".555555." "55555555" "5e5555e5" "55555555"
                 "55555555" ".555555." ".555555." ".555555." "5.5555.5" "........"))
    ("spaceship" . ("........" ".....55." "....5555" "...55555" "..555555" ".5555555"
                     "5555e555" "55555555" ".4444444" "..444444" "...4444." "........"))))

(defun monster-names () (mapcar #'car +monsters+))

(defparameter +items+
  '(("coin" . ("........" "..3333.." ".333333." "33e33e33" "33333333" "33333333"
                "33333333" "33333333" ".333333." "..3333.." "........" "........"))
    ("heart" . ("........" ".55..55." "55555555" "55555555" "55555555" ".555555."
                 "..5555.." "...55..." "....5..." "........" "........" "........"))
    ("potion" . ("........" "...33..." "...33..." "..5555.." ".555555." ".5e55e5."
                  ".555555." ".555555." ".555555." "..5555.." "........" "........"))
    ("sword" . ("......55" ".....55." "....55.." "...55..." "..55...." ".e55...."
                 ".355...." "..3....." "..3....." "..3....." "........" "........"))
    ("gem" . ("........" "..5555.." ".55ee55." "55555555" "55555555" ".555555."
               "..5555.." "...55..." "........" "........" "........" "........"))
    ("key" . ("........" "..3333.." ".33..33." ".33..33." "..3333.." "...33..."
               "...33..." "...33..." "...333.." "...33..." "........" "........"))
    ("chest" . ("........" "..3333.." ".333333." "33333333" "33e33e33" "33333333"
                 "33333333" "33333333" ".333333." "........" "........" "........"))
    ("star" . ("........" "...55..." "...55..." ".5555555" "..5555.." "..5555.."
                ".55..55." ".5....5." "........" "........" "........" "........"))
    ("mushroom" . ("........" "..5555.." ".5ee5e5." "55555555" "55555555" ".444444."
                   "..4444.." "..4444.." "..4444.." "........" "........" "........"))
    ("bomb" . ("........" "...33..." "..3333.." ".333333." ".3e33e3." ".333333."
                ".333333." "..3333.." "...33..." "........" "........" "........"))))

(defun item-names () (mapcar #'car +items+))

(defparameter +item-hues+
  '(("coin" . 0.12d0) ("heart" . 0.0d0) ("potion" . 0.92d0) ("sword" . 0.6d0)
    ("gem" . 0.5d0) ("key" . 0.12d0) ("chest" . 0.08d0) ("star" . 0.15d0)
    ("mushroom" . 0.0d0) ("bomb" . 0.7d0))
  "The hue each item is drawn in unless a call names one, as the original's
default_hue table does.")

(defun mask-for (kind name)
  "(values MASK HUE) for a sprite of KIND named NAME."
  (let ((entry (cond ((string= kind "character") (assoc (string-downcase name) +archetypes+ :test #'string=))
                     ((string= kind "monster") (assoc (string-downcase name) +monsters+ :test #'string=))
                     (t (assoc (string-downcase name) +items+ :test #'string=)))))
    (unless entry
      (fail "~a is not a ~a this cell draws; choose from ~{~a~^, ~}"
            name kind
            (cond ((string= kind "character") (archetype-names))
                  ((string= kind "monster") (monster-names))
                  (t (item-names)))))
    (cond ((string= kind "character")
           (values +character-mask+ (hue-of (getf (cdr entry) :body) 0.6d0)))
          ((string= kind "monster") (values (cdr entry) 0.35d0))
          (t (values (cdr entry) (or (cdr (assoc (string-downcase name) +item-hues+ :test #'string=)) 0.12d0))))))

(defun mask-color (letter hue rng)
  "The colour LETTER stands for, tinted from HUE."
  (let ((ramp (ramp hue :steps 4 :saturation 0.6d0 :value 0.8d0)))
    (case letter
      (#\s (nth 2 (ramp 0.07d0 :steps 3 :saturation 0.35d0 :value 0.95d0)))
      (#\h (nth (rint rng 2) (ramp (mod (+ hue 0.5d0) 1.0d0) :steps 3)))
      (#\b (nth 2 ramp))
      (#\l (nth 1 ramp))
      (#\e (list 250 250 250))
      (#\o (shade (nth 0 ramp) -0.55))
      (t (let ((digit (digit-char-p letter 10)))
           (if digit (nth (max 1 (min 3 (1- digit))) ramp) (nth 2 ramp)))))))

(defun draw-mask (mask size hue rng)
  "MASK mirrored onto a SIZE by SIZE canvas, shaded and outlined."
  (let* ((canvas (make-canvas size size))
         (rows (length mask))
         (columns (length (first mask)))
         (pixel-size (max 1 (floor size (max rows columns))))
         (offset-x (floor (- size (* 2 columns pixel-size)) 2))
         (offset-y (floor (- size (* rows pixel-size)) 2)))
    (loop for row from 0 below rows
          for line = (nth row mask)
          do (loop for column from 0 below columns
                   for letter = (char line column)
                   do (unless (char= letter #\.)
                        (let ((color (mask-color letter hue rng)))
                          (when color
                            ;; mirrored: the left half, then its reflection
                            (loop for mirror from 0 below 2
                                  for x = (+ offset-x (* pixel-size
                                                         (if (zerop mirror)
                                                             column
                                                             (- (* 2 columns) 1 column))))
                                  do (loop for dy below pixel-size
                                           do (loop for dx below pixel-size
                                                    do (set-pixel canvas (+ x dx) (+ offset-y (* row pixel-size) dy)
                                                                  (append color (list 255)))))))))))
    (outline-canvas canvas (shade (or (mask-color #\b hue rng) '(0 0 0)) -0.55))
    canvas))

(defun outline-canvas (canvas color)
  "Every opaque edge pixel of CANVAS painted COLOR: the 1-pixel silhouette
that makes pixel art read at any size."
  (let ((width (canvas-width canvas)) (height (canvas-height canvas))
        (edges '()))
    (loop for y below height
          do (loop for x below width
                   do (let ((pixel (pixel-at canvas x y)))
                        (when (and pixel (plusp (fourth pixel)))
                          (when (or (null (let ((p (pixel-at canvas (1- x) y))) (and p (plusp (fourth p)))))
                                    (null (let ((p (pixel-at canvas (1+ x) y))) (and p (plusp (fourth p)))))
                                    (null (let ((p (pixel-at canvas x (1- y)))) (and p (plusp (fourth p)))))
                                    (null (let ((p (pixel-at canvas x (1+ y)))) (and p (plusp (fourth p))))))
                            (push (cons x y) edges))))))
    (dolist (edge edges)
      (set-pixel canvas (car edge) (cdr edge) (append color (list 255))))
    canvas))

(defun sprite-sheet (kind name seed &key (size 16) (palette nil) (hue nil))
  "Two frames of KIND named NAME, side by side, at SIZE each. A sheet rather
than one image: a walk and an idle, which is what an engine atlas wants."
  (multiple-value-bind (mask default-hue) (mask-for kind name)
    (let* ((sheet (make-canvas (* 2 size) size))
           (hue (or (hue-of hue default-hue) default-hue))
           (first-frame (draw-mask mask size hue (make-rng seed)))
           (second-frame (draw-mask mask size hue (make-rng (+ seed 1)))))
      (canvas-blit sheet first-frame 0 0)
      ;; the second frame squashes one pixel: an idle bob, no new drawing
      (canvas-blit sheet (if (> size 16) second-frame second-frame) size 1)
      (when palette
        (let ((colors (palette-colors palette)))
          (loop for y below (canvas-height sheet)
                do (loop for x below (canvas-width sheet)
                         do (let ((pixel (pixel-at sheet x y)))
                              (when (and pixel (plusp (fourth pixel)))
                                (set-pixel sheet x y
                                           (append (nearest-palette-color (subseq pixel 0 3) colors)
                                                   (list (fourth pixel))))))))))
      sheet)))

;;; --- terrain, and a tileset that tiles ----------------------------------------

(defparameter +terrains+
  '(("grass" :hue 0.30 :saturation 0.65 :value 0.72 :detail :blades :solid nil)
    ("dirt" :hue 0.07 :saturation 0.55 :value 0.58 :detail :pebbles :solid nil)
    ("sand" :hue 0.11 :saturation 0.45 :value 0.92 :detail :speckle :solid nil)
    ("water" :hue 0.57 :saturation 0.7 :value 0.85 :detail :waves :solid t)
    ("deep_water" :hue 0.62 :saturation 0.75 :value 0.6 :detail :waves :solid t)
    ("stone" :hue 0.62 :saturation 0.1 :value 0.62 :detail :cracks :solid nil)
    ("snow" :hue 0.58 :saturation 0.08 :value 1.0 :detail :speckle :solid nil)
    ("lava" :hue 0.03 :saturation 0.9 :value 1.0 :detail :bubbles :solid t))
  "The terrains a tileset knows; the original's names, and its solid flag.")

(defun terrain-names () (mapcar #'car +terrains+))

(defun terrain-of (name)
  (let ((found (assoc (string-downcase (string name)) +terrains+ :test #'string=)))
    (unless found
      (fail "~a is not a terrain this cell tiles; choose from ~{~a~^, ~}" name (terrain-names)))
    found))

(defun lattice-value (x y seed)
  "A hash of a lattice point to 0..1: the noise a tile is drawn from."
  (let ((value (logand (* (+ (* x 374761393) (* y 668265263) (* seed 2246822519)) 1) #xFFFFFFFF)))
    (setf value (logand (* value 1274126177) #xFFFFFFFF))
    (/ (logand (logxor value (ash value -13)) #xFFFF) 65535.0d0)))

(defun wrapped-noise (x y period seed)
  "Value noise on a PERIOD lattice, so a tile meets itself at every edge."
  (let* ((cell 4)
         (cx (floor x cell)) (cy (floor y cell))
         (fx (/ (- x (* cx cell)) (coerce cell 'double-float)))
         (fy (/ (- y (* cy cell)) (coerce cell 'double-float)))
         (wrap (lambda (value) (mod value period)))
         (x0 (funcall wrap cx)) (y0 (funcall wrap cy))
         (x1 (funcall wrap (1+ cx))) (y1 (funcall wrap (1+ cy)))
         (smooth-x (* fx fx (- 3.0d0 (* 2.0d0 fx))))
         (smooth-y (* fy fy (- 3.0d0 (* 2.0d0 fy)))))
    (let* ((a (lattice-value x0 y0 seed)) (b (lattice-value x1 y0 seed))
           (c (lattice-value x0 y1 seed)) (d (lattice-value x1 y1 seed))
           (top (+ a (* (- b a) smooth-x)))
           (bottom (+ c (* (- d c) smooth-x)))
           (blended (+ top (* (- bottom top) smooth-y))))
      blended)))

(defun terrain-color (terrain x y seed)
  "The colour TERRAIN has at X, Y: its ramp, its noise, and its detail."
  (destructuring-bind (name &key hue saturation value detail &allow-other-keys) terrain
    (declare (ignore name))
    (let* ((colors (ramp hue :steps 4 :saturation saturation :value value))
           (grain (wrapped-noise x y 8 seed))
           (index (cond ((< grain 0.35d0) 1) ((< grain 0.7d0) 2) (t 3))))
      (case detail
        (:speckle (if (< (wrapped-noise (+ x 3) (+ y 5) 8 (+ seed 7)) 0.15d0)
                      (nth 3 colors) (nth index colors)))
        (:blades (if (and (< (wrapped-noise (* 2 x) y 16 (+ seed 3)) 0.2d0) (< grain 0.6d0))
                     (nth 3 colors) (nth index colors)))
        (:waves (nth (if (< (wrapped-noise x (* 2 y) 8 seed) 0.3d0) 3 2) colors))
        (:bubbles (if (< (wrapped-noise x y 8 (+ seed 11)) 0.12d0) (list 255 236 120) (nth index colors)))
        (:cracks (if (< (wrapped-noise (* 3 x) (* 3 y) 16 seed) 0.14d0) (nth 0 colors) (nth index colors)))
        (:pebbles (if (< (wrapped-noise (+ x 6) y 8 (+ seed 5)) 0.18d0) (nth 0 colors) (nth index colors)))
        (t (nth index colors))))))

(defun tileset-sheet (lower upper seed &key (tile 16) (scale 2))
  "A Wang corner set: sixteen TILE by TILE cells, four by four, each corner
choosing LOWER or UPPER, drawn from one noise field so the cells meet."
  (let* ((lower-terrain (terrain-of lower))
         (upper-terrain (terrain-of upper))
         (size (* tile scale))
         (canvas (make-canvas (* 4 size) (* 4 size))))
    (loop for mask from 0 below 16
          for cell-x = (mod mask 4)
          for cell-y = (floor mask 4)
          do (loop for y below size
                   do (loop for x below size
                            for global-x = (+ (* cell-x size) x)
                            for global-y = (+ (* cell-y size) y)
                            do (let* ((corner (if (< x (/ size 2))
                                                  (if (< y (/ size 2)) 0 2)
                                                  (if (< y (/ size 2)) 1 3)))
                                      (covered (logbitp corner mask))
                                      ;; a wobbly boundary, so a corner is a shape and not a square
                                      (wobble (* 6.0d0 (- (wrapped-noise global-x global-y 8 seed) 0.5d0)))
                                      (distance (sqrt (+ (expt (- x (/ size 2.0d0)) 2)
                                                         (expt (- y (/ size 2.0d0)) 2))))
                                      (corner-in (if covered
                                                     (< distance (+ (/ size 2.0d0) wobble))
                                                     (> distance (+ (* 0.7d0 size) wobble))))
                                      (terrain (if corner-in upper-terrain lower-terrain))
                                      (color (terrain-color terrain
                                                             (mod global-x tile)
                                                             (mod global-y tile) seed)))
                                 (set-pixel canvas global-x global-y
                                            (append (if (and (plusp x) (plusp y)
                                                             (not (equal terrain
                                                                         (if (zerop (mod (+ global-x 1) tile)) terrain
                                                                             terrain))))
                                                        color color)
                                                    (list 255)))))))
    canvas))

(defparameter +sprite-kinds+ '("character" "monster" "item"))
(defparameter +archetype-names+ (mapcar #'car +archetypes+))
(defparameter +monster-names+ (mapcar #'car +monsters+))
(defparameter +item-names+ (mapcar #'car +items+))
(defparameter +palette-name-list+ (mapcar #'car +palettes+))
(defparameter +terrain-name-list+ (mapcar #'car +terrains+))
