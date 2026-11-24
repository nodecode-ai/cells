;;;; cell-test.lisp --- the clock the model reads, and every way it is read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: the only files these tests touch are zone files of their own,
;;;; written byte by byte in a scratch folder of the home -- a tz database with
;;;; a zone of two types and two transitions, and a zone of one type and none --
;;;; and the clock is pinned with *CLOCK* where an instant's reading is
;;;; asserted. What is pinned is what a user gets: the standing section at the
;;;; head of a turn's context, the same reading from the tool and from /time,
;;;; the machine's own zone when none is configured, and a refusal, not a wrong
;;;; hour, for a zone that is not there.

(in-package #:nodecode.test)

(define-test-slice "live-time" "LIVE-TIME-" :start nodecode-live-time:start-cell)

;;; --- the cell's own lifecycle -------------------------------------------------

(define-cell-lifecycle-tests "live-time"
  (:config)
  (:hooks 'nle::read-harness-sections)
  (:command "time")
  (:help :live-time)
  (:running
   (let* ((sections (nle::read-harness-sections "s1"))
          (section (cdr (assoc "live-time" sections :test #'string=))))
     (is-present section "the standing section rides the harness"
                 (is (search "[LIVE-TIME] Now:" section) "with a reading the model can use")
                 (is (search "authoritative current time" section) "and says what it is for"))
     (is (= 1 (count "live-time" sections :key #'car :test #'string=)) "one of it, read any number of times")))
  (:refused ("timezone" "No/Such_Zone")
            ("timezone" "../etc/passwd")
            ("timezone" "Europe/Berlin/")
            ("timezone" "Europe/Berlin and more"))
  (:idle nodecode-live-time:live-time-error (nodecode-live-time::setting :timezone)))

;;; --- a tz database of this test's own ------------------------------------------

(defun live-time-tzif (&key (version #\2) (transitions '(1000000000 3900000000))
                         (types '((3600 . nil) (7200 . t))) (abbrs "CET\0CEST\0"))
  "A TZif file: TRANSITIONS, the type each switches to, and ((OFFSET . DST) ...)
per type. Version 1 keeps the one block of 4-byte times; version 2 adds the
header and block of 8-byte times the tz database has used since 2004."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((put (value size)
             (dotimes (index size)
               (vector-push-extend (ldb (byte 8 (* 8 (- size 1 index))) value) out)))
           (text (string)
             (loop for char across string do (vector-push-extend (char-code char) out))))
      (labels ((header ()
                 (text "TZif")
                 (vector-push-extend (char-code version) out)
                 (dotimes (index 15) (vector-push-extend 0 out))
                 (put 0 4)                             ; UT/local indicators
                 (put 0 4)                             ; standard/wall indicators
                 (put 0 4)                             ; leap seconds
                 (put (length transitions) 4)
                 (put (length types) 4)
                 (put (length abbrs) 4))
               (segment (time-size)
                 (dolist (transition transitions) (put transition time-size))
                 (loop for index from 0 below (length transitions)
                       do (vector-push-extend (mod index (length types)) out))
                 (loop for (offset . daylight) in types
                       for index from 0
                       do (put offset 4)
                          (vector-push-extend (if daylight 1 0) out)
                          (vector-push-extend (* 4 index) out))
                 (text abbrs)))
        (header) (segment 4)
        (when (char= version #\2)
          (header) (segment 8)
          (text (format nil "~%~a~%" abbrs)))
        (coerce out '(vector (unsigned-byte 8)))))))

(defun live-time-write-zone (dir name &rest keys)
  "NAME under the tz database at DIR, holding the zone KEYS describes."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname dir))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence (apply #'live-time-tzif keys) out))
    (namestring path)))

(defun live-time-zone-dir (&key (body nil))
  "A tz database of this test's own, holding Test/Zone, or BODY as its bytes."
  (let* ((dir (merge-pathnames (format nil "live-time-zones-~12,'0x/" (random (expt 16 10)))
                                (nlk:home)))
         (path (merge-pathnames "Test/Zone" dir)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence (or body (live-time-tzif)) out))
    (namestring dir)))

(defmacro with-zone-dir ((dir &rest keys) &body body)
  "BODY with DIR a tz database of the test's own, removed after."
  `(let ((,dir (live-time-zone-dir ,@keys)))
     (nlk:with-cleanup ((uiop:delete-directory-tree (pathname ,dir) :validate t
                                                                :if-does-not-exist :ignore))
       ,@body)))

(defun live-time-call (&rest pairs)
  "The current_time tool as the engine calls it: the declared contract
decoded, then the body."
  (let ((entry (nle::find-tool "current_time")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

;;; --- the zone file, read the way the tz database writes it ---------------------

(deftest live-time-reads-a-zone-files-transitions ()
  (with-zone-dir (dir)
    (let* ((table (nodecode-live-time::read-zone-table
                   (nodecode-live-time::zone-file "Test/Zone" dir)))
           (first (nodecode-live-time::zone-table-transitions table)))
      (flet ((offset (moment) (nodecode-live-time::zone-offset table moment)))
        (is (= 2 (length first)) "both transitions are read")
        (is (= 3600 (offset 900000000)) "before the first transition the standard type stands")
        (is (= 7200 (offset (aref first 1))) "at a transition the type it switches to is in force")
        (is (= 7200 (offset 3908988800)) "and it stands until the next one")))
    (let* ((path (live-time-write-zone dir "Fixed" :transitions '() :types '((-34200 . nil))
                                               :abbrs "FIX\0"))
           (table (nodecode-live-time::read-zone-table path)))
      (is (= -34200 (nodecode-live-time::zone-offset table 3908988800)) "a zone with no transition keeps its type")
      (is (equal "UTC-09:30" (nodecode-live-time::offset-label -34200)) "half an hour is said as one")))
  (with-zone-dir (dir :body (live-time-tzif :version #\0
                                             :transitions '(1000000000 1100000000)))
    (let* ((table (nodecode-live-time::read-zone-table
                   (nodecode-live-time::zone-file "Test/Zone" dir)))
           (offset (lambda (moment) (nodecode-live-time::zone-offset table moment))))
      (is (= 3600 (funcall offset 900000000)) "a version 1 file reads too")
      (is (= 7200 (funcall offset 1200000000)) "and its transitions land where they were written")))
  (with-zone-dir (dir :body #(110 111 116 32 97 32 122 111 110 101 32 102 105 108 101))
    (let ((table (nodecode-live-time::zone-file "Test/Zone" dir)))
      (is (signals-error nodecode-live-time:live-time-error
                         (nodecode-live-time::read-zone-table table)) "not a zone file, and refused"))))

;;; --- what the model reads ------------------------------------------------------

(deftest live-time-the-configured-zone-rides-the-standing-section ()
  (with-zone-dir (dir)
    (live-time-write-zone dir "Fixed" :transitions '() :types '((-34200 . nil)) :abbrs "FIX\0")
    (with-cell-stop ((live-time-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      (let* ((nodecode-live-time::*clock* 3908988800)
             (sections (nle::read-harness-sections "s1"))
             (section (cdr (assoc "live-time" sections :test #'string=)))
             (wanted "[LIVE-TIME] Now: 2023-11-15 00:13:20 (Weekday 3/7, Wed), TZ Test/Zone, UTC+02:00."))
        (is (search wanted section) "the clock, the weekday, the zone and its offset are all there")
        (is (search "TZ Test/Zone, UTC+02:00" (live-time-call)) "the tool reads the same clock mid-turn")
        (is (search "2023-11-15 00:13:20" (live-time-call "timezone" "Test/Zone")) "and the zone a call names")
        (let ((now (nodecode-live-time::run-time "" "s1")))
          (is (search "Now: 2023-11-15 00:13:20, Wed, Test/Zone, UTC+02:00" now) "/time answers the same reading"))
        (is (search "UTC+02:00" (nodecode-live-time::run-time "Test/Zone" "s1")) "/time takes a zone typed after it")
        (let ((fixed (nodecode-live-time::run-time "Fixed" "s1")))
          (is (search "Fixed, UTC-09:30" fixed) "and reads any zone the database holds"))))))

(deftest live-time-an-unknown-zone-is-named-not-guessed ()
  (with-zone-dir (dir)
    (with-cell-stop ((live-time-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      (let ((answer (live-time-call "timezone" "No/Such_Zone"))
            (now (nodecode-live-time::run-time "No/Such_Zone" "s1")))
        (is (search "ERROR:" answer) "the tool answers a failure")
        (is (search "No/Such_Zone" answer) "naming the zone it could not find")
        (is (search "tz database" answer) "and the database it looked in")
        (is (search "No/Such_Zone" now) "/time says the same in words")
        (is (not (search "[LIVE-TIME]" now)) "and no reading is invented")))))

(deftest live-time-with-no-zone-reads-this-machine ()
  (with-cell-stop ((live-time-start))
    (let* ((local (nodecode-live-time::local-offset (nodecode-live-time::clock-now)))
           (label (nodecode-live-time::offset-label local))
           (sections (nle::read-harness-sections "s1"))
           (section (cdr (assoc "live-time" sections :test #'string=))))
      (is (search (format nil "TZ local, ~a." label) section) "the machine's own zone, with the offset it has")
      (is (search "Weekday" section) "and the weekday")
      (is (search "Now:" (nodecode-live-time::run-time "" "s1")) "/time answers without a zone too"))))

(deftest live-time-a-zone-file-taken-away-is-a-fallback-not-a-gap ()
  (with-zone-dir (dir)
    (with-cell-stop ((live-time-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      ;; A database that moves under a running cell: the turn still gets a
      ;; reading, from the machine's own zone, rather than no section at all.
      (uiop:delete-file-if-exists (nodecode-live-time::zone-file "Test/Zone" dir))
      (let* ((sections (nle::read-harness-sections "s1"))
             (section (cdr (assoc "live-time" sections :test #'string=))))
        (is (search "TZ local" section) "the reading falls back to the machine's zone")
        (is (search "[LIVE-TIME] Now:" section) "and the model still knows what time it is")))))
