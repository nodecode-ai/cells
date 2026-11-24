;;;; contrast.lisp --- opaque sRGB contrast, measured, not guessed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's hardlight/contrast.py, in Lisp: WCAG relative luminance
;;;; with the unrounded thresholds, the role chosen by the caller, and one
;;;; answer per pair. Opaque colours only: an effect or a composite paint is
;;;; not measured here, and the answer says so.

(in-package #:nodecode-hardlight)

(defparameter +contrast-thresholds+
  '(("text" . 4.5d0) ("large_text" . 3.0d0) ("ui" . 3.0d0))
  "WCAG 2.2's thresholds for the three roles, unrounded.")

(defparameter +contrast-coverage+
  "Opaque sRGB pairs only; the role is supplied by the caller. Not a WCAG conformance audit."
  "What one measurement can and cannot establish, said in the answer.")

(defun hex-channels (text)
  "TEXT as (R G B) in 0..1, or NIL when it is not an opaque #RGB or #RRGGBB."
  (when (stringp text)
    (let ((digits (when (and (plusp (length text)) (char= #\# (char text 0)))
                    (subseq text 1))))
      (when digits
        (when (= 3 (length digits))
          (setf digits (with-output-to-string (out)
                         (loop for character across digits
                               do (write-char character out) (write-char character out)))))
        (when (and (= 6 (length digits))
                   (every (lambda (character) (digit-char-p character 16)) digits))
          (flet ((channel (at)
                   (/ (parse-integer (subseq digits at (+ at 2)) :radix 16) 255.0d0)))
            (list (channel 0) (channel 2) (channel 4))))))))

(defun relative-luminance (text)
  "TEXT's WCAG relative luminance, or NIL when TEXT is not a hex colour."
  (let ((channels (hex-channels text)))
    (when channels
      (destructuring-bind (red green blue) channels
        (flet ((linear (channel)
                 (if (<= channel 0.04045d0)
                     (/ channel 12.92d0)
                     (expt (/ (+ channel 0.055d0) 1.055d0) 2.4d0))))
          (+ (* 0.2126d0 (linear red))
             (* 0.7152d0 (linear green))
             (* 0.0722d0 (linear blue))))))))

(defun contrast-ratio (foreground background)
  "The WCAG contrast ratio of two opaque colours, or NIL when one is not a hex colour."
  (let ((one (relative-luminance foreground))
        (two (relative-luminance background)))
    (when (and one two)
      (let ((light (max one two)) (dark (min one two)))
        (/ (+ light 0.05d0) (+ dark 0.05d0))))))

(defun two-places (number)
  "NUMBER rounded to two places, as the plain decimal a reader compares at a glance."
  (format nil "~,2f" number))

;;; --- the boundary -----------------------------------------------------------

(nlk:define-contract color-pair
  (:doc "One opaque foreground/background pair and the role it plays: text, large text or a user-interface edge.")
  ("foreground" :text :required t :doc "the text or edge colour, as #RGB or #RRGGBB")
  ("background" :text :required t :doc "the surface colour it is painted on, as #RGB or #RRGGBB")
  ("role" :string :required t :options '("text" "large_text" "ui")
         :doc "text is body copy (4.5:1), large_text is large or bold text (3:1), ui is an edge, icon or control (3:1)")
  ("label" :string :default "" :doc "what the pair is for, echoed back in the answer"))

(nlk:define-contract contrast-call
  (:doc "Measure opaque sRGB contrast for explicit pairs: the ratio, the threshold the role carries and whether the pair reaches it. Effects and unknown composites are not measured, and this is not a conformance audit.")
  ("pairs" :array :of color-pair :required t
          :doc "1..100 pairs; each is one painted foreground on one solid surface"))

(defun pair-result (pair index)
  "PAIR measured, or (values NIL WHY) when a colour is not opaque hex."
  (let* ((foreground (color-pair-foreground pair))
         (background (color-pair-background pair))
         (role (color-pair-role pair))
         (ratio (contrast-ratio foreground background))
         (threshold (cdr (assoc role +contrast-thresholds+ :test #'equal))))
    (if (null ratio)
        (values nil (format nil "pairs[~a] needs opaque #RGB or #RRGGBB colours" index))
        (values (nlk:json-object "foreground" foreground "background" background
                                  "role" role "ratio" (format nil "~,2f" ratio)
                                  "threshold" (format nil "~,1f" threshold)
                                  "status" (if (>= ratio threshold) "pass" "fail")
                                  "label" (color-pair-label pair))
                nil))))

(defun check-contrast (call)
  "hardlight_contrast: the pairs of CALL measured, as JSON."
  (let* ((pairs (contrast-call-pairs call))
         (count (length pairs)))
    (if (or (zerop count) (> count 100))
        (nlk:encode-json-object
         (nlk:json-object "ok" nil "error" "pairs must carry 1..100 colour pairs"))
        (let ((results '()) (why nil))
          (loop for pair across pairs
                for index from 0
                until why
                do (multiple-value-bind (result reason) (pair-result pair index)
                     (if result (push result results) (setf why reason))))
          (if why
              (nlk:encode-json-object (nlk:json-object "ok" nil "error" why))
              (nlk:encode-json-object
               (nlk:json-object "ok" t
                                "pairs" (coerce (nreverse results) 'vector)
                                "coverage" +contrast-coverage+)))))))
