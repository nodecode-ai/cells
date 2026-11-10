;;;; library.lisp --- reference circuits to start from, as the original bundled them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The netlists are the original repository's own (library/*.cir), unchanged
;;;; except for the annotation lines this cell does not measure: three circuits
;;;; that need only resistors, capacitors, sources and a linear op-amp, which is
;;;; what this rewrite simulates.

(in-package #:nodecode-hotwire)

(defparameter *library*
  '(("rc_lowpass"
     "RC low-pass filter (1 kHz)"
     "*@ title RC low-pass filter (1 kHz)
* First-order passive low-pass: flat below 1 kHz, then -20 dB/decade.
*@ in VIN
*@ out out
*@ spec fc in 960..1160 Hz
*@ spec gain_db(100) > -0.1 dB
*@ spec gain_db(10000) in -21..-18.5 dB
VIN in 0 DC 0 AC 1
R1 in out 10k
C1 out 0 15n
.end")
    ("sallen_key_lowpass"
     "Sallen-Key low-pass filter (1 kHz Butterworth)"
     "*@ title Sallen-Key low-pass filter (1 kHz Butterworth)
* Second-order unity-gain low-pass: -40 dB/decade above 1 kHz, twice as steep as a plain RC.
*@ in VIN
*@ out out
*@ supply VCC
*@ spec fc in 980..1260 Hz
*@ spec gain_db(100) in -0.3..0.3 dB
*@ spec gain_db(10000) < -36 dB
*@ spec v(fo) in 4.3..4.7 V
*@ spec supply_current < 2m
VIN in 0 DC 0 AC 1
VCC vcc 0 DC 9
* input coupling and 4.5 V bias
RPD in 0 1meg
CIN in a 1u
RB a vref 100k
RREF1 vcc vref 10k
RREF2 vref 0 10k
CREF vref 0 47u
* the filter: equal resistors, C1 = 2.2 x C2 sets Q close to 0.707
R1 a b 10k
R2 b c 10k
C1 b fo 22n
C2 c 0 10n
XU1 c fo vcc 0 fo TL072
COUT fo out 1u
RL out 0 100k
.end")
    ("passive_tone_stack"
     "Three-knob passive amp tone stack"
     "*@ title Three-knob passive amp tone stack
* The classic bass/middle/treble passive network from tube amps.
*@ in VIN
*@ out out
*@ pot RVT b1 out t 250k taper=log pos=0.5
*@ pot RVB b1 m m 1meg taper=log pos=0.5
*@ pot RVM m 0 0 25k taper=lin pos=0.5
*@ spec gain_db(100) in -5..-1 dB
*@ spec gain_db(1000) < -7 dB
*@ spec gain_db(5000) in -10..-4 dB
VIN in 0 DC 0 AC 1
C1 in t 470p
R1 in n 33k
C2 n b1 22n
C3 n m 22n
RL out 0 1meg
.end"))
  "Reference circuits: an id, a title, and the netlist text.")

(defun library-ids ()
  "The ids of every bundled reference circuit."
  (mapcar #'first *library*))

(defun library-entry (query)
  "The library entry QUERY names, or the ones whose text mentions it."
  (let ((query (string-downcase (string-trim " " (or query "")))))
    (or (find query *library* :key #'first :test #'string=)
        (find query *library* :key (lambda (entry) (string-downcase (second entry))) :test #'search)
        (find-if (lambda (entry) (search query (string-downcase (third entry)))) *library*))))

(defun library-text ()
  "The library as the list a model or the operator reads."
  (format nil "~{~a~}"
          (loop for (id title nil) in *library*
                collect (format nil "  ~a  ~a~%" id title))))
