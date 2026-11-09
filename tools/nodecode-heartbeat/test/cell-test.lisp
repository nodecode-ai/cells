;;;; cell-test.lisp --- lifecycle, the probes, the clock, the boundary.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: the cell starts and stops cleanly and a section of
;;;; the wrong shape refuses; a file watch is a baseline on its first look and
;;;; speaks only when the file changes; a url watch speaks on a break and
;;;; not again while it stays broken; a clock watch wakes the session that
;;;; armed it, once per interval; a one-shot retires after its wake and a
;;;; paused watch says nothing; a watch that cannot be honoured is refused
;;;; with nothing left behind; and the pulse tool, decoded through its
;;;; contract, arms, lists and refuses an action it does not carry. Nothing
;;;; here dials the network: *HTTP* and SUBMIT-WAKE are both stubbed.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "heartbeat"
  (:config "tick_seconds" 60)
  (:help :heartbeat)
  (:command "heartbeat")
  (:running (is (= 60 (nodecode-heartbeat::setting :tick-seconds)) "tick_seconds is read")
            (is (equal nodecode-heartbeat::+primer+ (nle:help :heartbeat)) "the manual is the primer"))
  (:stopped (is (null nodecode-heartbeat::*worker*) "and the ticker is gone")
            (is (null nodecode-heartbeat::*watches*) "and no watch outlives the stop"))
  (:refused ("max_watches" 0))
  (:idle heartbeat:heartbeat-error (heartbeat:watches) (heartbeat:add :title "x" :every "1m")))

(deftest heartbeat-cell-file-watch-speaks-only-when-the-file-changes (with-heartbeat-runtime ()
  (with-captured-wakes
    (let* ((path (nlk:home "heartbeat-probe.txt"))
           (watch (heartbeat-arm :title "probe file" :kind "file" :path (namestring path)
                                 :every "1m" :fire-on "change" :now 1000)))
      (nlk:write-file-atomically path "one")
      (is (= 0 (nodecode-heartbeat::tick 1000)) "the first look is a baseline, not a wake")
      (is (equal "ok" (nodecode-heartbeat::watch-get watch :last-state)) "and it recorded what it saw")
      (nlk:write-file-atomically path "one and then two")
      (is (= 1 (nodecode-heartbeat::tick 1060)) "a change wakes once")
      (is (= 1 (length (heartbeat-submits))) "one wake went out")
      (is (search "probe file" (getf (first (heartbeat-submits)) :prompt)) "and it names the watch")
      (is (= 0 (nodecode-heartbeat::tick 1120)) "an unchanged file stays quiet")
      (is (= 1 (length (heartbeat-submits))) "still one wake")))))

(deftest heartbeat-cell-url-watch-speaks-when-the-site-breaks (with-heartbeat-runtime ()
  (with-captured-wakes
    (let ((status 200))
      (let ((nodecode-heartbeat::*http*
              (lambda (method url &key timeout)
                (declare (ignore method url timeout))
                (values "" status nil))))
        (heartbeat-arm :title "site" :kind "url" :url "https://example.invalid/" :every "5m" :now 1000)
        (is (= 0 (nodecode-heartbeat::tick 1000)) "a healthy site is a baseline")
        (setf status 503)
        (is (= 1 (nodecode-heartbeat::tick 1300)) "the break wakes once")
        (is (search "503" (getf (first (heartbeat-submits)) :prompt)) "the wake carries the receipt")
        (is (= 0 (nodecode-heartbeat::tick 1600)) "still broken, and quiet"))))))

(deftest heartbeat-cell-a-clock-watch-wakes-the-session-that-armed-it (with-heartbeat-runtime ()
  (with-captured-wakes
    (let ((watch (heartbeat-arm :title "check the build" :note "ask whether the build passed"
                                :kind "every" :every "30m" :now 1000)))
      (is (= 0 (nodecode-heartbeat::tick 2000)) "not due yet")
      (is (= 1 (nodecode-heartbeat::tick 2800)) "due at half an hour")
      (let ((wake (first (heartbeat-submits))))
        (is (equal "s1" (getf wake :session)) "the session that armed it, and no other")
        (is (search "ask whether the build passed" (getf wake :prompt)) "the note rides the wake"))
      (is (= 1 (nodecode-heartbeat::watch-get watch :fires)) "the fire is counted")
      (is (= 4600 (nodecode-heartbeat::watch-get watch :next-at)) "and the clock moved on")
      (is (= 0 (nodecode-heartbeat::tick 2801)) "one wake per interval")))))

(deftest heartbeat-cell-a-one-shot-retires-and-a-paused-watch-waits (with-heartbeat-runtime ()
  (with-captured-wakes
    (let ((once (heartbeat-arm :title "one shot" :kind "after" :every "10m" :now 0)))
      (is (= 1 (nodecode-heartbeat::tick 600)) "the one shot wakes")
      (is (null (nodecode-heartbeat::find-watch (nodecode-heartbeat::watch-get once :id)))
          "and retires after it"))
    (let ((clock (heartbeat-arm :title "paused clock" :kind "every" :every "1m" :now 0)))
      (heartbeat:pause (nodecode-heartbeat::watch-get clock :id))
      (is (= 0 (nodecode-heartbeat::tick 600)) "a paused watch says nothing")
      (heartbeat:resume (nodecode-heartbeat::watch-get clock :id))
      (is (= 1 (nodecode-heartbeat::tick (get-universal-time))) "and wakes once resumed")))))

(deftest heartbeat-cell-refuses-a-watch-it-cannot-honour (with-heartbeat-runtime ()
  (is (signals-error heartbeat:heartbeat-error
        (heartbeat-arm :title "nameless file" :kind "file" :every "1m"))
      "a file watch with no path is refused")
  (is (signals-error heartbeat:heartbeat-error
        (heartbeat-arm :title "bad kind" :kind "moon" :every "1m"))
      "an unknown kind is refused")
  (is (signals-error heartbeat:heartbeat-error
        (heartbeat-arm :title "no clock" :kind "every" :every "soon"))
      "a duration that names none is refused")
  (is (null nodecode-heartbeat::*watches*) "a refused watch leaves nothing behind")))

(deftest heartbeat-cell-the-pulse-tool-arms-lists-and-refuses (with-heartbeat-runtime ()
  (let ((nodecode-heartbeat::*http*
          (lambda (method url &key timeout)
            (declare (ignore method url timeout))
            (values "" 200 nil))))
    (is (search "no watch stands"
                (nodecode-heartbeat::run-pulse
                 (nodecode-heartbeat::decode-pulse (nlk:json-object "action" "list"))))
        "the tool answers from an empty registry")
    (is (search "armed watch-the-site"
                (nodecode-heartbeat::run-pulse
                 (nodecode-heartbeat::decode-pulse
                  (nlk:json-object "action" "create" "title" "watch the site"
                                   "kind" "url" "url" "https://example.invalid/" "every" "5m"))))
        "the tool arms a watch")
    (is (search "watch the site" (heartbeat:watches)) "and the verb lists it")
    (is (= 1 (length nodecode-heartbeat::*watches*)))
    (is (signals-error nlk:contract-refusal
          (nodecode-heartbeat::decode-pulse (nlk:json-object "action" "explode")))
        "an action the contract does not carry is refused before the body"))))
