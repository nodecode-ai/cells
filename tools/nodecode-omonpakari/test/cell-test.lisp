;;;; cell-test.lisp --- lifecycle, the journal, pressure against a stubbed sky.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here leaves the image: OpenWeather is a stubbed NLK:HTTP and the
;;;; journal is one JSON file in a scratch directory each test names.

(in-package #:nodecode.test)

(define-test-slice "omonpakari" "OMONPAKARI-CELL-" :start nodecode-omonpakari:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defun op-scratch-dir () "A fresh store directory for one test."
  (namestring (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "omonpakari-~a-~a/"
                                         (get-universal-time) (random 1000000))
                                (uiop:temporary-directory)))))

(defmacro with-omonpakari ((&rest pairs) &body body) "The cell started on a scratch store, stopped after."
  `(with-cell-stop ((apply #'omonpakari-start (append (list "data_dir" (op-scratch-dir))
                                                     (list ,@pairs))))
     ,@body))

(defun op-call (tool &rest pairs) "TOOL called with PAIRS, the way a model's call reaches its body."
  (let ((entry (nle::find-tool tool)))
    (unless entry (error "no tool ~a" tool))
    (funcall (third entry) (apply #'nlk:make-json-object pairs))))

(defun op-now () (nodecode-omonpakari::now-utc))

(defparameter +current-body+
  (format nil "{\"main\":{\"pressure\":1006,\"temp\":4.5},\"dt\":~a}" (op-now)))

(defparameter +forecast-body+
  (format nil "{\"list\":[{\"main\":{\"pressure\":1002},\"dt\":~a},{\"main\":{\"pressure\":999},\"dt\":~a}]}"
          (+ (op-now) (* 3 3600)) (+ (op-now) (* 6 3600))))

(defparameter *urls* nil "Every URL the stub answered, newest first.")

(defmacro with-sky (&body body) "BODY with NLK:HTTP answering the fixture, and every URL it saw kept."
  `(let ((*urls* '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method options))
                                (push url *urls*)
                                (if (search "/forecast" url)
                                    (values +forecast-body+ 200 nil nil)
                                    (values +current-body+ 200 nil nil)))
       ,@body)))

(defmacro with-key (&body body) "BODY with the section's key env naming a key, as the operator's would."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                                (and (equal name "OMONPAKARI_TEST_KEY") "k-1"))
     ,@body))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "omonpakari"
  (:help :omonpakari)
  (:command "omonpakari")
  (:running
   (is (nle::find-tool "omonpakari_pressure") "the pressure tool is registered")
   (is (nle::find-tool "omonpakari_log") "and the journal")
   (is (nle::find-tool "omonpakari_cycle") "and the cycle")
   (is (nle::find-tool "omonpakari_interactions") "and the interaction check")
   (is (nle::find-tool "omonpakari_life") "and life notes")
   (is (nle::find-tool "omonpakari_waves") "and the composed states")
   (is (nle::find-tool "omonpakari_correlate") "and the correlation"))
  (:stopped
   (is (null (nle::find-tool "omonpakari_pressure")) "stop takes the pressure tool back")
   (is (null (nle::find-registered-command "omonpakari")) "and the command"))
  (:refused ("window_h" 1)
            ("latitude" "200")))

(deftest omonpakari-cell-a-bad-place-refuses-and-installs-nothing ()
  (is (signals-error nlk:config-refusal (omonpakari-start "latitude" "north")) "a latitude that is not a number refuses")
  (is (null (nle::find-tool "omonpakari_log")) "and the refused start installed nothing"))

;;; --- pressure -------------------------------------------------------------------

(deftest omonpakari-cell-pressure-reads-a-stubbed-sky (with-sky)
  (with-key
    (with-omonpakari ("openweather_api_key_env" "OMONPAKARI_TEST_KEY")
      (let ((answer (op-call "omonpakari_pressure" "lat" 59.91 "lon" 10.75)))
        (is (search "1006" answer) (format nil "the measured pressure is answered: ~a" answer))
        (is (search "\"low\"" answer) "and its level, below the low threshold")
        (is (search "lowest_ahead" answer) "and the lowest point ahead")
        (is (search "appid=k-1" (or (car *urls*) "")) "the key travelled to OpenWeather")
        (is (search "lat=59.91" (or (car *urls*) "")) "with the place that was given")
        (is (find-if (lambda (url) (search "/forecast" url)) *urls*) "and the forecast endpoint was read too")))))

(deftest omonpakari-cell-pressure-without-a-key-says-so (with-sky)
  (with-omonpakari ("openweather_api_key_env" "OMONPAKARI_TEST_KEY")
    (let ((answer (op-call "omonpakari_pressure" "lat" 59.91 "lon" 10.75)))
      (is (search "missing_key" answer) "no key is a reason, not an error")
      (is (null *urls*) "and nothing was dialled"))))

(deftest omonpakari-cell-pressure-without-a-place-says-so (with-sky)
  (with-key
    (with-omonpakari ("openweather_api_key_env" "OMONPAKARI_TEST_KEY")
      (let ((answer (op-call "omonpakari_pressure")))
        (is (search "location_not_set" answer) "no place is a reason of its own")))))

;;; --- the journal -----------------------------------------------------------------

(deftest omonpakari-cell-journal-round-trip (with-omonpakari ())
  (let ((added (op-call "omonpakari_log" "action" "add" "kind" "symptom"
                         "label" "headache" "severity" 3)))
    (is (search "headache" added) "the entry is answered back")
    (is (search "\"id\":" added) "with the id it was given"))
  (op-call "omonpakari_log" "action" "add" "kind" "food" "label" "cocoa")
  (let ((listed (op-call "omonpakari_log" "action" "list" "days" 1)))
    (is (search "headache" listed) "list reads the symptom back")
    (is (search "cocoa" listed) "and the food"))
  (is (search "\"deleted\":" (op-call "omonpakari_log" "action" "delete" "id" 1)) "delete takes one entry out")
  (is (search "confirm" (op-call "omonpakari_log" "action" "purge")) "purge without confirm says what it needs")
  (is (search "purged" (op-call "omonpakari_log" "action" "purge" "confirm" t)) "purge erases everything once confirmed"))

;;; --- the cycle -------------------------------------------------------------------

(deftest omonpakari-cell-cycle-window-is-a-range (with-omonpakari ())
  (let ((now (op-now)))
    (op-call "omonpakari_cycle" "action" "mark" "mark" "start" "at" (- now (* 56 86400)))
    (op-call "omonpakari_cycle" "action" "mark" "mark" "start" "at" (- now (* 28 86400)))
    (let ((status (op-call "omonpakari_cycle" "action" "status")))
      (is (search "\"average_days\":28" status) (format nil "one observed length of 28 days: ~a" status))
      (is (search "next_window" status) "the next window is a range of dates")
      (is (search "\"phase\":" status) "and the phase is an estimate"))))

;;; --- interactions and life notes --------------------------------------------------

(deftest omonpakari-cell-iron-and-cocoa-are-flagged-with-a-source (with-omonpakari ())
  (let ((now (op-now)))
    (op-call "omonpakari_log" "action" "add" "kind" "supplement" "label" "iron supplement"
             "at" now)
    (op-call "omonpakari_log" "action" "add" "kind" "food" "label" "cocoa" "at" (- now 1800))
    (let ((answer (op-call "omonpakari_interactions" "action" "check" "days" 1)))
      (is (search "cocoa" answer) "cocoa is named as the substance near iron")
      (is (search "inhibits" answer) "with what it does")
      (is (search "Hurrell" answer) "and where the fact comes from")))
  (is (search "vitamin C" (op-call "omonpakari_interactions" "action" "table")) "the table answers itself too"))

(deftest omonpakari-cell-life-note-is-the-alternative (with-omonpakari ())
  (op-call "omonpakari_life" "action" "set" "topic" "morning drink"
           "note" "warm hojicha" "relates" "cocoa")
  (is (search "hojicha" (op-call "omonpakari_life" "action" "list")) "the note is kept in the person's words")
  (let ((now (op-now)))
    (op-call "omonpakari_log" "action" "add" "kind" "supplement" "label" "iron" "at" now)
    (op-call "omonpakari_log" "action" "add" "kind" "food" "label" "cocoa" "at" now))
  (is (search "hojicha" (op-call "omonpakari_interactions" "action" "check" "days" 1)) "and it is offered as the alternative to the flagged item"))

;;; --- the quiet picture -----------------------------------------------------------

(deftest omonpakari-cell-the-picture-rides-the-standing-context (with-omonpakari ())
  (op-call "omonpakari_log" "action" "add" "kind" "symptom" "label" "headache")
  (let* ((sections (nodecode-omonpakari::quiet-sections
                    (lambda (session) (declare (ignore session)) nil) "op-test"))
         (picture (cdr (assoc "omonpakari" sections :test #'equal))))
    (is picture "the picture is a standing section")
    (is (search "headache" (or picture "")) "carrying today's entries")
    (is (search "Background only" (or picture "")) "and the instruction that it is context, not a message")))

(deftest omonpakari-cell-quiet-context-off-leaves-the-context-alone (with-omonpakari ())
  (with-cell-stop ((omonpakari-start "data_dir" (op-scratch-dir) "quiet_context" nil))
    (op-call "omonpakari_log" "action" "add" "kind" "note" "label" "tired")
    (is (null (nodecode-omonpakari::quiet-sections
               (lambda (session) (declare (ignore session)) nil) "op-test")) "with quiet_context off the hook adds nothing")))
