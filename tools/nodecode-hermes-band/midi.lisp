;;;; midi.lisp --- a Standard MIDI File writer, format 1, one track per part.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Written from scratch for the same reason the plugin wrote its own: the
;;;; whole engine then needs nothing but this file and the fluidsynth the
;;;; operator already has.

(in-package #:nodecode-hermes-band)

(defun vlq (number)
  "NUMBER as a MIDI variable-length quantity, as a list of octets."
  (let ((out (list (logand number #x7F)))
        (rest (ash number -7)))
    (loop while (plusp rest)
          do (push (logior #x80 (logand rest #x7F)) out)
             (setf rest (ash rest -7)))
    out))

(defun u16 (number) (list (ldb (byte 8 8) number) (ldb (byte 8 0) number)))
(defun u32 (number)
  (list (ldb (byte 8 24) number) (ldb (byte 8 16) number)
        (ldb (byte 8 8) number) (ldb (byte 8 0) number)))

(defun channel-of (part)
  "The MIDI channel a part plays on: drums on 10, everything else round-robin."
  (if (string= (band-part-role part) "drums") 9 0))

(defun note-events (events channel)
  "EVENTS as (TICK ORDER . OCTETS): note-off sorts before note-on at one tick,
so a repeated pitch is retriggered rather than swallowed."
  (loop for (tick pitch velocity duration) in events
        append (list (list (+ tick duration) 0 (logior #x80 channel) pitch 0)
                     (list tick 5 (logior #x90 channel) pitch
                           (max 1 (min 127 velocity))))))

(defun track-chunk (events &key (bpm nil) (name nil) (program 0) (channel 0)
                               (volume 100) (pan 64) (send 40))
  "One MTrk chunk: the tempo and name on the first track, then the part's
program, its mix controllers, its notes and an end-of-track."
  (let ((all '()))
    (when bpm
      (let ((micros (round (/ 60000000.0 bpm))))
        (push (list 0 -20 #xFF #x51 #x03
                    (ldb (byte 8 16) micros) (ldb (byte 8 8) micros) (ldb (byte 8 0) micros))
              all)))
    (when name
      (let ((octets (map 'list #'char-code (subseq name 0 (min 40 (length name))))))
        (push (append (list 0 -19 #xFF #x03) (vlq (length octets)) octets) all)))
    (push (list 0 -18 (logior #xC0 channel) (logand 127 program)) all)
    (push (list 0 -17 (logior #xB0 channel) 7 (logand 127 volume)) all)
    (push (list 0 -16 (logior #xB0 channel) 10 (logand 127 pan)) all)
    (push (list 0 -15 (logior #xB0 channel) 91 (logand 127 send)) all)
    (push (list 0 -14 (logior #xB0 channel) 93 0) all)
    (dolist (event (note-events events channel)) (push event all))
    (setf all (sort all (lambda (a b) (or (< (first a) (first b))
                                           (and (= (first a) (first b)) (< (second a) (second b)))))))
    (let ((data '()) (previous 0))
      (dolist (event all)
        (let ((tick (first event)))
          (setf data (append data (vlq (max 0 (- tick previous)))))
          (setf data (append data (cddr event)))
          (setf previous tick)))
      (setf data (append data (vlq 0) (list #xFF #x2F #x00)))
      (append (map 'list #'char-code "MTrk") (u32 (length data)) data))))

(defun midi-bytes (score tracks)
  "TRACKS as a complete format-1 file: (NAME EVENTS PROGRAM CHANNEL PAN SEND)."
  (let* ((chunks (loop for (name events program channel pan send) in tracks
                        for index from 0
                        collect (track-chunk events :bpm (and (zerop index) (band-score-bpm score))
                                             :name (and (zerop index) (band-score-name score))
                                             :program program :channel channel
                                             :pan pan :send send)))
         (body (apply #'append chunks))
         (header (append (map 'list #'char-code "MThd") (u32 6) (u16 1)
                         (u16 (length chunks)) (u16 +div+))))
    (append header body)))

(defun write-midi-file (path score tracks)
  "Write TRACKS to PATH as octets. => PATH."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence (coerce (midi-bytes score tracks) '(vector (unsigned-byte 8))) out))
  path)

(defun part-track (part events)
  "The (NAME EVENTS PROGRAM CHANNEL PAN SEND) tuple one part writes."
  (list (format nil "~a ~a" (band-part-inst part) (band-part-role part))
        events
        (or (cdr (assoc (band-part-inst part) +programs+ :test #'string=)) 0)
        (channel-of part)
        (round (+ 64 (* 63 (max -1.0 (min 1.0 (band-part-pan part))))))
        (round (* 40 (or (band-part-send part) 1.0)))))
