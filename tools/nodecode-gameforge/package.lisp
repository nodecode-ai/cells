;;;; package.lisp --- the cell's own package, its condition, and the two
;;;; primitives every other file shares: a seeded random source and a byte
;;;; sink.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-gameforge", which is this folder's name.

(defpackage #:nodecode-gameforge
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*gameforge*
           #:gameforge-error #:gameforge-error-detail))

(in-package #:nodecode-gameforge)

(nlk:define-peripheral gameforge :not-running t)

;;; --- a seeded random source ------------------------------------------------
;;; Every asset is a function of its seed: the same seed and the same
;;; arguments write the same bytes, which is what makes a variant cheap (bump
;;; the seed) and a rebuild honest. SBCL's RANDOM is not portable across
;;; versions, so the generator is here, xorshift32, and stayed the same.

(defstruct (gameforge-rng (:constructor make-gameforge-rng (state)))
  (state 1 :type (unsigned-byte 32)))

(defun make-rng (seed)
  "A generator for SEED: an integer, any sign."
  (make-gameforge-rng (logand (+ (* (abs (truncate (or seed 0))) 2654435761) 2463534242)
                               #xFFFFFFFF)))

(defun rnext (rng)
  "The next 32-bit value, and the generator's new state."
  (let ((x (gameforge-rng-state rng)))
    (setf x (logxor x (ash x 13)))
    (setf x (logxor x (ash x -17)))
    (setf x (logxor x (ash x 5)))
    (setf x (logand x #xFFFFFFFF))
    (setf (gameforge-rng-state rng) x)
    x))

(defun rfloat (rng)
  "A real in [0, 1)."
  (/ (rnext rng) 4294967296.0))

(defun rrange (rng low high)
  "A real in [LOW, HIGH)."
  (+ low (* (- high low) (rfloat rng))))

(defun rint (rng n)
  "An integer in [0, N)."
  (if (plusp n) (mod (rnext rng) n) 0))

(defun rchoice (rng sequence)
  "One element of SEQUENCE, chosen by RNG."
  (when (plusp (length sequence))
    (elt sequence (rint rng (length sequence)))))

(defun rchance (rng probability)
  "T when RNG rolls under PROBABILITY."
  (< (rfloat rng) probability))

;;; --- a byte sink -----------------------------------------------------------
;;; WAV and PNG are byte formats; one adjustable octet vector, filled here
;;; and written once, keeps the encoders free of stream bookkeeping.

(defun make-octets (&optional (capacity 256))
  "An empty adjustable octet vector."
  (make-array capacity :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun push-octet (octets value)
  (vector-push-extend (logand value #xFF) octets))

(defun push-octets (octets sequence)
  (loop for octet across sequence do (push-octet octets octet))
  octets)

(defun push-ascii (octets text)
  (loop for char across text do (push-octet octets (char-code char)))
  octets)

(defun push-le16 (octets value)
  (push-octet octets value)
  (push-octet octets (ash value -8)))

(defun push-le32 (octets value)
  (push-octet octets value)
  (push-octet octets (ash value -8))
  (push-octet octets (ash value -16))
  (push-octet octets (ash value -24)))

(defun push-be32 (octets value)
  (push-octet octets (ash value -24))
  (push-octet octets (ash value -16))
  (push-octet octets (ash value -8))
  (push-octet octets value))

(defun octets-to-string (octets)
  (map 'string #'code-char octets))
