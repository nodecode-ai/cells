;;;; cell-test.lisp --- the android-emulator cell against a stubbed adb.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs adb, an emulator or a device: every run goes through
;;;; NODECODE-ANDROID-EMULATOR::*RUN*, a stub that records the command line it
;;;; was handed (and the timeout) and answers what the test chose. What is
;;;; under test is the part this cell owns: the argv it builds out of a
;;;; declared call, the path and name checks before anything runs, and the
;;;; string it answers from what came back.

(in-package #:nodecode.test)

(define-test-slice "android-emulator" "ANDROID-EMULATOR-CELL-"
  :start nodecode-android-emulator:start-cell)

(define-cell-lifecycle-tests "android-emulator"
  (:help :android-emulator)
  (:running
   (is (equal '("adb") (android-emulator::setting :adb)) "adb on PATH is the default")
   (is (equal "emulator-5554" (android-emulator::setting :serial)) "and the first emulator's serial")
   (is (nle::find-tool "emu_tap") "the touch tools are registered")
   (is (nle::find-tool "emu_deeplink") "and the deep-link one")
   (is (= 18 (count-if (lambda (entry) (eql 0 (search "emu_" (first entry)))) nle::*tools*))
       "eighteen tools, the original plugin's"))
  (:stopped
   (is (null (nle::find-tool "emu_tap")) "and stop takes every one back")
   (is (null (nle::find-tool "emu_deeplink"))))
  (:refused ("serial" "R58M90ABCDE")
            ("adb" (vector))
            ("timeout_seconds" 0)))

;;; --- the stub ------------------------------------------------------------------

(defvar *calls* nil
  "The runs the stub was handed, newest first, each (ARGV TIMEOUT).")

(defmacro with-emulator ((&key (out "") (err "") (status 0) answer) &body body)
  "BODY with the cell started and every adb run answered by a stub: OUT, ERR
and STATUS stand in for adb's, or ANSWER, a function of ARGV, answers
(values OUT ERR STATUS). *CALLS* records (ARGV TIMEOUT)."
  `(let* ((*calls* nil)
          (nodecode-android-emulator::*run*
            (lambda (argv &key timeout)
              (push (list argv timeout) *calls*)
              ,(if answer
                   `(funcall ,answer argv)
                   `(values ,out ,err ,status)))))
     (with-cell-stop ((android-emulator-start))
       ,@body)))

(defun ran (&optional (newest 0))
  "The ARGV of the NEWEST'th run, 0 the last one."
  (first (nth newest *calls*)))

(defun ran-timeout (&optional (newest 0))
  (second (nth newest *calls*)))

(defun emu-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "emu-call" name arguments)))

(defun status-stub (argv)
  "What adb answers for a booted emulator, by the command it was given."
  (cond ((member "devices" argv :test #'equal)
         (values (format nil "List of devices attached~%emulator-5554 device product:sdk_gphone64 model:Pixel_7_Pro~%")
                 "" 0))
        ((member "ro.product.model" argv :test #'equal) (values "Pixel 7 Pro" "" 0))
        ((member "ro.build.version.release" argv :test #'equal) (values "14" "" 0))
        ((member "ro.build.version.sdk" argv :test #'equal) (values "34" "" 0))
        ((member "wm" argv :test #'equal) (values "Physical size: 1080x2400" "" 0))
        ((member "sys.boot_completed" argv :test #'equal) (values "1" "" 0))
        ((member "df" argv :test #'equal) (values "/dev/block/dm-0 5.6G 2.1G 3.5G 38% /data" "" 0))
        (t (values "" "" 0))))

;;; --- the section --------------------------------------------------------------

(deftest android-emulator-cell-without-a-program-refuses ()
  ;; The one thing no verb can do without: adb's own argv prefix. A section
  ;; that names no program refuses at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (android-emulator-start "adb" (vector)))))
    (is (search "adb names no program" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "emu_status")) "and installs nothing")))

(deftest android-emulator-cell-a-phone-serial-refuses-unless-allowed ()
  (let ((condition (signals-error nlk:config-refusal
                       (android-emulator-start "serial" "R58M90ABCDE"))))
    (is (search "allow_physical" (princ-to-string condition)) "the refusal names the setting that would allow it")
    (is (null (nle::find-tool "emu_status")) "and nothing installed"))
  (with-cell-stop ((android-emulator-start "serial" "R58M90ABCDE" "allow_physical" t))
    (is (equal "R58M90ABCDE" (android-emulator::setting :serial)) "allow_physical honours it")))

;;; --- the verbs ----------------------------------------------------------------

(deftest android-emulator-cell-a-tap-is-one-pinned-command ()
  (with-emulator (:out "tap ok")
    (let ((answer (emu-call "emu_tap" "{\"x\":540,\"y\":1200}")))
      (is (equal '("adb" "-s" "emulator-5554" "shell" "input" "tap" "540" "1200") (ran))
          "the serial is pinned and the coordinates are integers")
      (is (null (ran-timeout)) "no per-call bound: run-adb uses the section's")
      (is (search "exit 0" answer) "the answer says how the run ended")
      (is (search "tap ok" answer) "and carries what adb wrote"))))

(deftest android-emulator-cell-a-swipe-clamps-its-duration ()
  (with-emulator ()
    (emu-call "emu_swipe" "{\"x1\":1,\"y1\":2,\"x2\":3,\"y2\":4,\"duration_ms\":99999}")
    (is (equal "10000" (car (last (ran)))) "a duration past 10 s is clamped")
    (is (search "swiped 1,2 to 3,4" (emu-call "emu_swipe" "{\"x1\":1,\"y1\":2,\"x2\":3,\"y2\":4}"))
        "and the answer says what was swiped")))

(deftest android-emulator-cell-typing-escapes-the-device-shell ()
  (with-emulator ()
    (emu-call "emu_type" "{\"text\":\"hello world; rm -rf /sdcard\"}")
    (is (equal "'hello%sworld;%srm%s-rf%s/sdcard'" (car (last (ran))))
        "spaces become %s and the whole is one quoted device-shell word")))

(deftest android-emulator-cell-a-keycode-gets-its-prefix ()
  (with-emulator ()
    (emu-call "emu_key" "{\"keycode\":\"home\"}")
    (is (equal "KEYCODE_HOME" (car (last (ran)))) "a bare name is prefixed and uppercased")
    (emu-call "emu_key" "{\"keycode\":\"KEYCODE_BACK\"}")
    (is (equal "KEYCODE_BACK" (car (last (ran)))) "a full name is left alone")))

(deftest android-emulator-cell-a-shell-command-rides-one-quoted-word ()
  (with-emulator ()
    (emu-call "emu_shell" "{\"command\":\"pm list packages\",\"timeout_seconds\":900}")
    (is (equal '("adb" "-s" "emulator-5554" "shell" "sh" "-c" "'pm list packages'") (ran))
        "the command is one argument to the device's sh -c")
    (is (equal 600 (ran-timeout)) "and the timeout is clamped to 600 s")))

(deftest android-emulator-cell-lists-packages ()
  (with-emulator (:out (format nil "package:com.example.one~%package:com.example.two~%package:org.other~%"))
    (let ((answer (emu-call "emu_packages" "{\"filter\":\"com.example\"}")))
      (is (equal '("adb" "-s" "emulator-5554" "shell" "pm" "list" "packages" "com.example") (ran))
          "the filter is one more argument")
      (is (search "3 packages matching \"com.example\"" answer) "the answer counts them, as adb answered")
      (is (search "com.example.one" answer) "and names them"))))

(deftest android-emulator-cell-reports-a-connected-emulator ()
  (with-emulator (:answer #'status-stub)
    (let ((answer (emu-call "emu_status" "{}")))
      (is (search "emulator-5554 is connected" answer))
      (is (search "model: Pixel 7 Pro" answer) "the properties come through")
      (is (search "android_version: 14" answer))
      (is (= 7 (length *calls*)) "one device list, then one call per property"))))

(deftest android-emulator-cell-says-when-nothing-is-connected ()
  (with-emulator (:out "List of devices attached~%")
    (let ((answer (emu-call "emu_status" "{}")))
      (is (search "emulator-5554 is not connected" answer))
      (is (search "Start one" answer) "and what to do about it"))))

(deftest android-emulator-cell-a-screenshot-captures-then-pulls ()
  (let ((path (uiop:native-namestring
               (merge-pathnames (format nil "emu-shot-~d.png" (get-universal-time))
                                (uiop:temporary-directory)))))
    (with-emulator (:out "1 file pulled")
      (let ((answer (emu-call "emu_screenshot" (format nil "{\"output_path\": ~s}" path))))
        (is (equal 3 (length *calls*)) "capture, pull, remove")
        (is (equal '("shell" "screencap" "-p") (subseq (ran 2) 3 6)) "it captures on the device first")
        (is (search "/sdcard/emu_screenshot_" (car (last (ran 2)))) "to a file of its own")
        (is (equal (list "adb" "-s" "emulator-5554" "pull" (car (last (ran 2))) path) (ran 1))
            "then pulls that file here")
        (is (search "saved" (format nil "saved ~a" path)) "and the answer carries the path")
        (is (search path answer) "the path is in the answer")
        (is (equal '("shell" "rm" "-f") (subseq (ran 0) 3 6)) "and removes it from the device")))))

(deftest android-emulator-cell-refuses-a-write-outside-its-roots ()
  (with-emulator ()
    (let ((answer (emu-call "emu_screenshot" "{\"output_path\":\"/etc/emu_screenshot.png\"}")))
      (is (search "under the home directory" answer) "a path outside the roots is refused")
      (is (null *calls*) "before anything runs"))))

(deftest android-emulator-cell-refuses-an-existing-file-without-overwrite ()
  (let ((path (uiop:native-namestring
               (merge-pathnames (format nil "emu-taken-~d.png" (get-universal-time))
                                (uiop:temporary-directory)))))
    (with-open-file (out path :direction :output :if-exists :supersede) (write-string "x" out))
    (unwind-protect
         (with-emulator ()
           (let ((answer (emu-call "emu_screenshot" (format nil "{\"output_path\": ~s}" path))))
             (is (search "refusing to overwrite" answer))
             (is (null *calls*) "and nothing runs")))
      (ignore-errors (delete-file path)))))

(deftest android-emulator-cell-refuses-a-bad-name-before-running ()
  (with-emulator ()
    (is (search "not a package name" (emu-call "emu_uninstall" "{\"package\":\"com.example/app\"}")))
    (is (search "not a URL" (emu-call "emu_deeplink" "{\"url\":\"example.com\"}")))
    (is (search "not a path inside the emulator" (emu-call "emu_push" "{\"local_path\":\"/etc/hosts\",\"remote_path\":\"sdcard/x\"}")))
    (is (search "not a logcat filter spec" (emu-call "emu_logcat" "{\"filter\":\"nonsense\"}")))
    (is (null *calls*) "none of them reached adb")))

(deftest android-emulator-cell-a-location-rides-longitude-first ()
  (with-emulator ()
    (emu-call "emu_gps" "{\"lat\":37.7749,\"lng\":-122.4194}")
    (is (equal '("emu" "geo" "fix") (subseq (ran) 3 6)) "the fix rides adb emu geo fix")
    (is (search "-122.4" (seventh (ran))) "longitude first, as adb takes it")
    (is (search "37.7" (eighth (ran))) "and latitude after it")
    (emu-call "emu_gps" "{\"clear\":true}")
    (is (equal '("adb" "-s" "emulator-5554" "emu" "geo" "nmea" "$GPGGA,,,,,,0,,,,,,,,*66") (ran))
        "clearing sends the empty NMEA sentence")
    (is (search "give both lat and lng" (emu-call "emu_gps" "{\"lat\":1}"))
        "half a location refuses")))

(deftest android-emulator-cell-a-network-condition-is-one-of-three ()
  (with-emulator ()
    (let ((answer (emu-call "emu_network" "{\"condition\":\"offline\"}")))
      (is (search "wifi off" answer))
      (is (equal 2 (length *calls*)) "offline is wifi and data off"))
    (let ((answer (emu-call "emu_network" "{\"condition\":\"lte\"}")))
      (is (search "one of offline, slow, fast" answer) "an unknown condition is refused by the declaration"))))
