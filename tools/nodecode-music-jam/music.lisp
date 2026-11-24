;;;; music.lisp --- pure musical data: the controls, the four roles' local
;;;; patterns, the session shape and a standard MIDI file. No audio, no
;;;; threads, no model. The rules are the original's, kept note for note.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-music-jam)

(defparameter *roles* '("drums" "bass" "keys" "lead")
  "The four musicians, in the order a session names them.")

(defparameter *ranges* '(("drums" 35 51) ("bass" 28 60) ("keys" 48 88) ("lead" 55 96))
  "Each role's MIDI range.")

(defparameter *voices* '(("drums" . 5) ("bass" . 1) ("keys" . 6) ("lead" . 1))
  "How many voices each role may sound at once.")

(defparameter *drums* '(36 38 42 46 49)
  "The only drum pitches the ensemble plays: kick, snare, closed hat, open hat, crash.")

(defparameter *chords* '((50 53 57 60 64) (43 46 50 53 57) (46 50 53 57 60) (45 49 52 55 58))
  "Dm9, Gm9, Bbmaj7, A7: one chord a bar.")

(defparameter *default-controls* '((:density . 0.55) (:energy . 0.55) (:complexity . 0.35)
                                   (:register . 0.0) (:swing . 0.12) (:variation . 0.25))
  "Every musician's controls, and their defaults.")

(defparameter *control-names* '("density" "energy" "complexity" "register" "swing" "variation")
  "The control names a direction or a tool call may set.")

(defparameter *intentions*
  '("A dry backbeat. Let the pocket breathe."
    "Rooted, warm, locked to the kick."
    "Soft ninths, with air between chords."
    "A small motif. Then a little silence.")
  "What each musician says it is playing, in the four roles' order.")

(defparameter *progression* '("Dm9" "Gm9" "Bbmaj7" "A7")
  "The one progression this ensemble plays.")

(defparameter *instruments* '(("piano" . 0) ("electric" . 4) ("organ" . 16)
                              ("pad" . 89) ("pluck" . 10))
  "The two browser voices' instruments and their General MIDI programs.")

;;; --- numbers and controls ------------------------------------------------------------

(defun as-number (value low high)
  "VALUE as a finite number in LOW..HIGH, or a refusal naming the range."
  (unless (and (realp value) (<= low value high) (= value value))
    (fail "expected a number from ~a to ~a, got ~s" low high value))
  (coerce value 'double-float))

(defun whole-number (value low high)
  "VALUE as an integer in LOW..HIGH, or a refusal."
  (let ((value (as-number value low high)))
    (unless (= value (fround value))
      (fail "whole numbers only, got ~s" value))
    (round value)))

(defun controls (data)
  "DATA, an alist or plist of control name to value, as the controls a
musician takes: each name known, 0..1 except register at -12..12."
  (let ((pairs (cond ((null data) '())
                      ((and (listp data) (keywordp (first data)))
                       (loop for (key value) on data by #'cddr
                             collect (cons (string-downcase (symbol-name key)) value)))
                      ((listp data)
                       (loop for pair in data
                             collect (cons (string-downcase (string (car pair))) (cdr pair))))
                      (t (fail "controls are an object of names to values, got ~s" data)))))
    (loop for (name . value) in pairs
          do (unless (member name *control-names* :test #'equal)
               (fail "unknown control ~s: ~{~a~^, ~}" name *control-names*))
          collect (cons name (if (equal name "register")
                                 (as-number value -12 12)
                                 (as-number value 0 1))))))

(defun state-control (state name)
  "STATE's control NAME, or its default."
  (or (cdr (assoc name (getf state :controls) :test #'equal))
      (cdr (assoc (intern (string-upcase name) :keyword) *default-controls*))))

(defun role-range (role)
  "ROLE's (LOW HIGH) MIDI range, or a refusal."
  (or (cdr (assoc role *ranges* :test #'equal))
      (fail "~s is no musician: ~{~a~^, ~}" role *roles*)))

;;; --- patterns --------------------------------------------------------------------------

(defun notes (role events)
  "EVENTS as ROLE's four-bar pattern: every field in range, the drums on the
five pitches, the polyphony the role allows, or a refusal naming what broke."
  (destructuring-bind (low high) (role-range role)
    (unless (and (listp events) (<= (length events) 256))
      (fail "a pattern is a list of at most 256 events"))
    (let ((clean
            (loop for event in events
                  collect
                  (progn
                    (unless (listp event) (fail "an event is an object, got ~s" event))
                    (let* ((bar (whole-number (getf event :bar) 0 3))
                           (beat (as-number (getf event :beat) 0 3.999))
                           (pitch (whole-number (getf event :pitch) low high))
                           (duration (as-number (getf event :duration) 0.04 4))
                           (velocity (as-number (getf event :velocity) 0.01 1))
                           (articulation (or (getf event :articulation) "normal")))
                      (when (> (+ beat duration) 4.001)
                        (fail "an event stays inside its bar: beat ~a plus duration ~a" beat duration))
                      (when (and (equal role "drums") (not (member pitch *drums*)))
                        (fail "the drums play 36, 38, 42, 46 and 49 only, not ~a" pitch))
                      (unless (member articulation '("normal" "staccato" "accent") :test #'equal)
                        (fail "articulation is normal, staccato or accent, not ~s" articulation))
                      (list :bar bar :beat beat :pitch pitch :duration duration
                            :velocity velocity :articulation articulation))))))
      (dotimes (bar 4)
        (let ((voices 0)
              (limit (or (cdr (assoc role *voices* :test #'equal)) 1)))
          (dolist (edge (stable-sort (loop for event in clean
                                           when (= (getf event :bar) bar)
                                             append (list (cons (+ (getf event :beat)
                                                                   (getf event :duration)) -1)
                                                          (cons (getf event :beat) 1)))
                                      (function <) :key #'car))
            (incf voices (cdr edge))
            (when (> voices limit)
              (fail "~a plays at most ~a voices at once" role limit)))))
      (stable-sort clean
                   (lambda (a b)
                     (or (< (getf a :bar) (getf b :bar))
                         (and (= (getf a :bar) (getf b :bar))
                              (or (< (getf a :beat) (getf b :beat))
                                  (and (= (getf a :beat) (getf b :beat))
                                       (< (getf a :pitch) (getf b :pitch)))))))))))

(defun voice (out role controls bar beat pitch duration velocity)
  "One note appended to OUT, a one-element box: register-shifted, swung, its
velocity scaled by the energy control, as the original's local planner
writes them."
  (let* ((range (role-range role))
         (pitch (if (equal role "drums") pitch
                    (max (first range)
                         (min (second range) (+ pitch (round (state-control controls "register")))))))
         (beat (if (= (mod beat 1) 0.5)
                   (+ beat (* (state-control controls "swing") 0.25))
                   beat))
         (duration (min duration (- 4 beat))))
    (setf (car out) (nconc (car out) (list (list :bar bar :beat beat :pitch pitch :duration duration
                                  :velocity (min 0.94 (* velocity
                                                          (+ 0.55 (* (state-control controls "energy") 0.6))))
                                  :articulation "normal"))))))

(defun local-pattern (role controls &optional motif)
  "The four-bar pattern ROLE plays at CONTROLS with no model in the loop: the
original's own stand-alone rule planner."
  (let ((out (list nil))
        (density (state-control (list :controls controls) "density"))
        (complexity (state-control (list :controls controls) "complexity")))
    (loop for chord in *chords*
          for bar from 0
          do (cond
               ((equal role "drums")
                (dolist (beat '(0 2)) (voice out role controls bar beat 36 0.14 0.85))
                (dolist (beat '(1 3)) (voice out role controls bar beat 38 0.12 0.66))
                (dotimes (i (if (> density 0.35) 8 4))
                  (voice out role controls bar (* i (if (> density 0.35) 0.5 1)) 42 0.06
                         (if (evenp i) 0.48 0.32)))
                (when (and (> complexity 0.65) (= bar 3))
                  (dolist (beat '(3.25 3.5 3.75))
                    (voice out role controls bar beat 38 0.08 0.42))))
               ((equal role "bass")
                (voice out role controls bar 0 (- (first chord) 12) 0.85 0.8)
                (voice out role controls bar 2 (- (first chord) 12) 0.7 0.65)
                (when (> density 0.45)
                  (voice out role controls bar 1.5 (- (first chord) 5) 0.3 0.55))
                (when (> density 0.7)
                  (voice out role controls bar 3.5
                         (- (first (nth (mod (1+ bar) 4) *chords*)) 13) 0.3 0.5)))
               ((equal role "keys")
                (dolist (beat (if (> density 0.4) '(0.5 2.5) '(0.5)))
                  (dolist (pitch (rest chord))
                    (voice out role controls bar beat (+ pitch 12)
                           (if (< (state-control (list :controls controls) "energy") 0.7) 1.05 0.5)
                           0.34))))
               ((and (equal role "lead") (> density 0.05))
                (let ((notes (or motif '(69 72 76 74))))
                  (loop for beat in (if (> density 0.65) '(0.5 1.5 2.5) '(0.5 2))
                        for i from 0
                        when (or (evenp bar)
                                 (> (state-control (list :controls controls) "variation") 0.6))
                          do (voice out role controls bar beat
                                    (nth (mod (+ bar i) (length notes)) notes) 0.42 0.43))))))
    (let ((ordered (stable-sort (nreverse (car out))
                                 (lambda (a b)
                                   (or (< (getf a :bar) (getf b :bar))
                                       (and (= (getf a :bar) (getf b :bar))
                                            (< (getf a :beat) (getf b :beat))))))))
      (when (member role '("bass" "lead") :test #'equal)
        (loop for (event following) on ordered
              when (and following (= (getf event :bar) (getf following :bar)))
                do (setf (getf event :duration)
                         (min (getf event :duration)
                              (- (getf following :beat) (getf event :beat))))))
      (notes role ordered))))

;;; --- the session --------------------------------------------------------------------

(defun browser-sounds ()
  "The two browser voices, keys and the human's phrase, and the mixer."
  (list :sounds (list (cons "keys" (list :instrument "electric" :tone 0.5))
                       (cons "human" (list :instrument "piano" :tone 0.5)))
        :mix (loop for role in (append *roles* '("human"))
                   collect (cons role (list :volume (if (equal role "human") 0.65 0.7)
                                            :mute nil :solo nil)))))

(defun initial-session ()
  "A fresh jam: four musicians on their defaults, nothing recorded."
  (list :version 1 :tempo 92 :key "D minor" :meter 4
        :progression (copy-list *progression*)
        :browser (browser-sounds)
        :roles (loop for role in *roles* for i from 0
                     collect (cons role (list :controls (copy-tree *default-controls*)
                                              :pattern (local-pattern role *default-controls*)
                                              :motif '(69 72 76 74)
                                              :intention (nth i *intentions*)
                                              :muted nil)))
        :arrangement '()
        :history (loop for role in *roles* collect (cons role '()))
        :pending '() :performance '() :next-bar 0))

(defun role-state (session role)
  "SESSION's state for ROLE."
  (cdr (assoc role (getf session :roles) :test #'equal)))

;;; --- the MIDI file -----------------------------------------------------------------------

(defun midi (bars &optional sounds)
  "BARS, the performance, as a standard MIDI file's octets: the tempo meta to
open, then a track a musician and one for the human phrase."
  (let* ((tempo (or (getf (first bars) :tempo) 92))
         (origin (or (getf (first bars) :bar) 0))
         (tracks '()))
    (labels ((u32 (n) (list (ldb (byte 8 24) n) (ldb (byte 8 16) n)
                             (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
             (vlq (n)
               (let ((values (list (logand n 127))))
                 (loop while (> n 127) do (setf n (ash n -7))
                       do (push (logior (logand n 127) 128) values))
                 values))
             (text (string) (map 'list #'char-code string))
             (shifted (value) (max 0 (min 127 (round value)))))
      (push (append (list 0 #xff #x51 3) (u32 (round 60000000 tempo))
                    (list 0 #xff #x58 4 4 2 24 8 0 #xff #x2f 0))
            tracks)
      (loop for (role channel . program) in '(("drums" 9 0) ("bass" 0 33) ("keys" 1 4)
                                              ("lead" 2 80) ("human" 3 4))
            do (let* ((sound (cdr (assoc role sounds :test #'equal)))
                      (program (or (cdr (assoc (getf sound :instrument) *instruments* :test #'equal))
                                   program))
                      (events '()))
                 (dolist (bar bars)
                   (dolist (event (if (equal role "human")
                                      (getf bar :human)
                                      (getf (cdr (assoc role (getf bar :roles) :test #'equal))
                                            :pattern)))
                     (let* ((tick (round (* (+ (* (- (getf bar :bar) origin) 4)
                                               (getf event :beat)) 480)))
                            (length (max 1 (round (* (getf event :duration) 480))))
                            (pitch (getf event :pitch)))
                       (push (list tick 1 (list (+ #x90 channel) pitch
                                                (shifted (* (getf event :velocity) 127))))
                             events)
                       (push (list (+ tick length) 0 (list (+ #x80 channel) pitch 0)) events))))
                 (let ((body (append (list 0 #xff #x03) (vlq (length (text role))) (text role)
                                     (list 0 (+ #xc0 channel) program)))
                       (previous 0))
                   (dolist (event (stable-sort events (function <) :key #'car))
                     (setf body (append body (vlq (- (first event) previous)) (third event))
                           previous (first event)))
                   (setf body (append body (list 0 (+ #xb0 channel) 64 0)
                                      (list 0 (+ #xb0 channel) 123 0)
                                      (list 0 #xff #x2f 0)))
                   (push (append (text "MTrk") (u32 (length body)) body) tracks))))
      (coerce (append (text "MThd") (u32 6) (list 0 1 (ldb (byte 8 8) (length tracks))
                                               (ldb (byte 8 0) (length tracks)) 1 #xe0)
                      (apply #'append (nreverse tracks)))
              '(vector (unsigned-byte 8))))))
