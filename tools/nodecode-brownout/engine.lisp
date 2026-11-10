;;;; engine.lisp --- what the readings mean, with no I/O of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's core/policy.py and core/forecast.py, kept
;;;; pure the same way and for the same reason: the decision is data a test
;;;; reads, and the caller performs it. States are the original's —
;;;; ON_AC -> ON_BATTERY (an alert, waiting for a person) -> PARKED ->
;;;; ON_AC (resume) — with UNKNOWN for a host that cannot say.
;;;;
;;;; The one automatic action without a person's answer is a reversible
;;;; park at the critical level. Nothing is killed, deleted or stopped
;;;; mid-write: a park holds new tool calls and nothing else.

(in-package #:nodecode-brownout)

(defconstant +max-samples+ 720 "Readings kept: 15 s apart, that is three hours.")
(defconstant +min-points+ 3 "Readings on battery before a slope is trusted.")
(defconstant +min-span-seconds+ 120 "Time on battery before a slope is trusted.")
(defconstant +min-ticks+ 3 "Whole-percent drops before a slope is trusted.")
(defconstant +floor-pct+ 5 "The charge the runway is measured down to.")

;;; --- the forecast ---------------------------------------------------------------

(defstruct (runway (:constructor make-runway (minutes rate basis)))
  "What the discharge measured so far says. MINUTES is NIL until there is
enough evidence; the cell never guesses."
  minutes
  rate
  basis)

(defun battery-stretch (samples)
  "SAMPLES (newest first) after the most recent switch to battery, oldest
first, the ones carrying a charge figure."
  (let ((stretch '()))
    (dolist (sample (reverse samples))
      (cond ((sample-on-ac sample) (setf stretch '()))
            ((sample-pct sample) (push sample stretch))))
    (nreverse stretch)))

(defun runway (samples)
  "Forecast the runway from SAMPLES, newest first: the rate measured
tick-to-tick over the current battery stretch, and the minutes until
+floor-pct+ at that rate."
  (let ((stretch (battery-stretch samples)))
    (cond
    ((or (< (length stretch) +min-points+)
         (< (- (sample-time (car (last stretch))) (sample-time (first stretch)))
            +min-span-seconds+))
     (make-runway nil nil (format nil "measuring (~d readings on battery so far)" (length stretch))))
    (t
     ;; OS charge readings are whole percents, so a flat run says nothing about
     ;; the rate: each downward tick marks the moment the true charge crossed a
     ;; whole percent, and the rate is measured tick to tick.
     (let ((ticks '()) (previous nil))
       (dolist (sample stretch)
         (when (and previous (< (sample-pct sample) (sample-pct previous)))
           (push (cons (sample-time sample) (sample-pct sample)) ticks))
         (setf previous sample))
       (setf ticks (nreverse ticks))
       (if (< (length ticks) +min-ticks+)
           (make-runway nil nil (format nil "charge not falling yet (~d of ~d drops seen)"
                                        (length ticks) +min-ticks+))
           (let* ((first-tick (first ticks)) (last-tick (car (last ticks)))
                  (t1 (car first-tick)) (p1 (cdr first-tick))
                  (t2 (car last-tick)) (p2 (cdr last-tick)))
             (if (<= t2 t1)
                 (make-runway nil nil "charge not falling yet")
                 (let* ((rate (/ (- p1 p2) (/ (- t2 t1) 60.0)))
                        (left (and (plusp rate)
                                   (max 0.0 (/ (- (sample-pct (car (last stretch))) +floor-pct+) rate)))))
                   (make-runway left rate
                                 (format nil "~,2f%/min, ~d pts over ~d min"
                                         rate (- p1 p2) (round (/ (- t2 t1) 60)))))))))))))

;;; --- the state machine ----------------------------------------------------------

(defstruct (engine (:conc-name eng-))
  "One host's power history and what the cell has decided about it. The cell
holds the lock; the engine writes nothing."
  (state :on-ac)
  (samples '())
  (kept nil)
  (announced nil)
  (battery-since nil)
  (pending nil))

(defun engine-command (engine command)
  "Queue a person's COMMAND (:park, :keep or :resume) for the next tick."
  (if (eq command :keep)
      (setf (eng-kept engine) t)
      (setf (eng-pending engine) command)))

(defun engine-remember (engine sample)
  "ENGINE with SAMPLE as its newest reading, the oldest one dropped past
+max-samples+."
  (let ((all (cons sample (eng-samples engine))))
    (setf (eng-samples engine)
          (if (> (length all) +max-samples+) (subseq all 0 +max-samples+) all))))

(defun engine-decide (engine sample human runway-value cfg)
  "The original's policy.step: one tick's decision from the previous state,
SAMPLE, a fresh HUMAN command, the KEPT answer already given, RUNWAY-VALUE
and CFG."
  (let ((previous (eng-state engine))
        (critical (and sample
                       (or (and (sample-pct sample)
                                (<= (sample-pct sample) (getf cfg :critical-pct)))
                           (and (runway-minutes runway-value)
                                (<= (runway-minutes runway-value)
                                    (getf cfg :critical-minutes)))))))
    (cond
      ((null sample)
       (if (eq previous :unknown)
           (list :state :unknown :actions '() :reason "still no power reading")
           (list :state :unknown :actions '(:notify-unknown)
                 :reason "no power reading available; nothing is acted on")))
      ((sample-on-ac sample)
       (cond ((and (eq previous :parked)
                   (or (getf cfg :auto-resume) (eq human :resume)))
              (list :state :on-ac :actions '(:notify-resumed) :reason "power is back"))
             ((eq previous :parked)
              (list :state :parked :actions '()
                    :reason "power is back; waiting for /brownout resume"))
             ((eq previous :on-battery)
              (list :state :on-ac :actions '(:notify-back) :reason "power is back"))
             (t (list :state :on-ac :actions '() :reason "on mains power"))))
      ((and (eq human :park) (not (eq previous :parked)))
       (list :state :parked :actions '(:park) :reason "the operator chose to park"))
      ((and (eq human :resume) (eq previous :parked))
       (list :state :on-battery :actions '() :reason "resumed on battery"))
      ((member previous '(:on-ac :unknown))
       (list :state :on-battery :actions '(:alert) :reason "power lost"))
      ((eq previous :parked)
       (list :state :parked :actions '() :reason "parked on battery"))
      ((and critical (not (eng-kept engine)))
       (list :state :parked :actions '(:park)
             :reason "critical battery and no answer: parked"))
      (t (list :state :on-battery :actions '() :reason "on battery")))))

(defun engine-tick (engine sample cfg)
  "Advance ENGINE by SAMPLE — NIL when the host had no reading — and answer
the decision (:state KEYWORD :actions (KEYWORD...) :reason TEXT) with
:runway among the actions the first time a measured runway is known in this
battery stretch. Performs nothing."
  (when sample (engine-remember engine sample))
  (let* ((runway-value (runway (eng-samples engine)))
         (human (shiftf (eng-pending engine) nil))
         (decision (engine-decide engine sample human runway-value cfg))
         (actions (getf decision :actions)))
    (setf (eng-state engine) (getf decision :state))
    (cond ((and sample (sample-on-ac sample))
           (setf (eng-kept engine) nil (eng-announced engine) nil (eng-battery-since engine) nil))
          ((and sample (null (eng-battery-since engine)))
           (setf (eng-battery-since engine) (sample-time sample))))
    (when (and (eq (getf decision :state) :on-battery)
               (not (eng-announced engine))
               (runway-minutes runway-value))
      (setf (eng-announced engine) t
            actions (append actions '(:runway))))
    (setf (getf decision :actions) actions)
    decision))

;;; --- what the state reads as ----------------------------------------------------

(defun state-text (engine)
  "The state in words."
  (case (eng-state engine)
    (:on-ac "on mains power")
    (:on-battery "on battery")
    (:parked "parked on battery")
    (t "no power reading yet")))

(defun power-text (engine)
  "The newest reading in words, or \"no reading\"."
  (let ((sample (first (eng-samples engine))))
    (cond ((null sample) "no reading")
          ((sample-pct sample)
           (format nil "~a ~d% via ~a" (if (sample-on-ac sample) "mains" "battery")
                   (sample-pct sample) (sample-source sample)))
          (t (format nil "~a via ~a" (if (sample-on-ac sample) "mains" "battery")
                     (sample-source sample))))))

(defun runway-text (engine)
  "The runway in words, or how far the measurement has got."
  (let ((value (runway (eng-samples engine))))
    (if (runway-minutes value)
        (format nil "about ~d min (~a)" (round (runway-minutes value)) (runway-basis value))
        (runway-basis value))))
