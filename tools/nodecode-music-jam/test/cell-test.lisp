;;;; cell-test.lisp --- the declaration, the queue, the page's API, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is offline: no model, no network. The page's API is
;;;; called through HANDLE with the request a browser would send, and the one
;;;; socket test binds 127.0.0.1 and asks the cell's own listener for the page.

(in-package #:nodecode.test)

(define-test-slice "music-jam" "MUSIC-JAM-CELL-" :start nodecode-music-jam:start-cell)

(defun music-jam-body (data)
  "DATA as the octets a browser sends."
  (sb-ext:string-to-octets (sb-ext:octets-to-string (nlk:encode-json-octets data)
                                                    :external-format :utf-8)
                           :external-format :utf-8))

(defun music-jam-ask (server action data &key (token nil token-given-p))
  "SERVER's ACTION called as a browser would, => (values STATUS BODY)."
  (multiple-value-bind (status mime octets)
      (nodecode-music-jam::handle
       server "POST" (concatenate 'string "/api/" action)
       (list (cons "host" (format nil "127.0.0.1:~d" (nodecode-music-jam::band-server-port server)))
             (cons "authorization" (format nil "Bearer ~a"
                                           (if token-given-p token
                                               (nodecode-music-jam::band-server-token server))))
             (cons "content-type" "application/json"))
       (music-jam-body (nodecode-music-jam::jsonable data)))
    (declare (ignore mime))
    (values status (sb-ext:octets-to-string octets :external-format :utf-8))))

(defmacro refuses (form &body body)
  "BODY asserting FORM signals the band's own refusal."
  `(is (handler-case (progn ,form nil)
         (nodecode-music-jam:music-jam-error () t))
       ,@body))

(defmacro with-band (&body body)
  "BODY over a fresh band and a server struct that never opened a socket."
  `(let* ((jam (nodecode-music-jam::make-jam))
          (server (nodecode-music-jam::%make-server :port 8099 :token "test-token" :jam jam)))
     (declare (ignorable server))
     ,@body))

;;; --- the declaration ---------------------------------------------------------------

(define-cell-lifecycle-tests "music-jam"
  (:help :music-jam)
  (:command "music-jam")
  (:running
   (is (nle::find-tool "jam_start") "jam_start is registered")
   (is (nle::find-tool "jam_export") "and the export")
   (is (= 10 (count-if (lambda (entry) (eql 0 (search "jam_" (first entry)))) nle::*tools*)) "ten tools, and no others"))
  (:stopped
   (is (null (nle::find-tool "jam_start")) "stop takes every tool back"))
  (:refused ("port" -1)))

;;; --- the band's own rules -----------------------------------------------------------

(deftest music-jam-cell-every-musician-plays-four-valid-bars ()
  (let ((session (nodecode-music-jam::initial-session)))
    (dolist (role '("drums" "bass" "keys" "lead"))
      (let ((pattern (getf (nodecode-music-jam::role-state session role) :pattern)))
        (is (plusp (length pattern)) (format nil "~a plays something" role))
        (is (every (lambda (event) (<= 0 (getf event :bar) 3)) pattern)
            (format nil "~a keeps to four bars" role))))
    (refuses (nodecode-music-jam::notes "drums" (list (list :bar 0 :beat 0 :pitch 60
                                                             :duration 0.5 :velocity 0.5))) "the drums refuse a melodic pitch")
    (refuses (nodecode-music-jam::notes "keys" (list (list :bar 0 :beat 0 :pitch 60
                                                            :duration 5 :velocity 0.5))) "a note may not leave its bar")))

(deftest music-jam-cell-a-direction-waits-for-the-next-bar ()
  (with-band
    (let ((answer (nodecode-music-jam::direct jam "keys less busy, darker")))
      (is (equal "planning done" (getf answer :status)) "the direction is taken")
      (is (= 1 (length (getf (nodecode-music-jam::jam-session jam) :pending))) "one change is queued")
      (is (= 0 (getf (first (getf (nodecode-music-jam::jam-session jam) :pending)) :earliest)) "and waits for the first bar"))
    (nodecode-music-jam::start-band jam "browser-1")
    (let ((bars (getf (nodecode-music-jam::bars jam "browser-1" 0 1) :bars)))
      (is (= 1 (length bars)) "one bar comes back")
      (is (= 0 (length (getf (nodecode-music-jam::jam-session jam) :pending))) "the queued change activated on the bar it was given")
      (is (equal "Your controls, in the pocket."
                 (getf (cdr (assoc "keys" (getf (first bars) :roles) :test #'equal))
                       :intention)) "and the musician says so"))
    (refuses (nodecode-music-jam::direct jam "make it better") "a direction that names no musician refuses")
    (refuses (nodecode-music-jam::restate jam "horns" '(("density" . 0.5))) "and so does an unknown musician")))

(deftest music-jam-cell-undo-takes-back-one-change-for-one-musician ()
  (with-band
    (nodecode-music-jam::restate jam "bass" '(("density" . 0.9)))
    (let ((answer (nodecode-music-jam::undo jam "bass")))
      (is (equal "cancelled the queued change" (getf answer :status)) "a queued change is cancelled first"))
    (refuses (nodecode-music-jam::undo jam "bass") "with nothing queued and nothing played there is nothing to undo")))

;;; --- the page's API ------------------------------------------------------------------

(deftest music-jam-cell-the-page-is-served-and-its-api-wants-the-token ()
  (with-band
    (multiple-value-bind (status mime octets)
        (nodecode-music-jam::handle server "GET" "/" '(("host" . "127.0.0.1:8099")) nil)
      (declare (ignore mime))
      (is (= 200 status) "the page is served")
      (is (search "Music Jam" (sb-ext:octets-to-string octets :external-format :utf-8)) "and it is the band's page"))
    (multiple-value-bind (status body) (music-jam-ask server "status" (list :owner "x")
                                                      :token "wrong")
      (is (= 401 status) "a wrong token is refused")
      (is (search "unauthorized" body) "and says so"))
    (multiple-value-bind (status body) (music-jam-ask server "start" (list :owner "browser-1"))
      (is (= 200 status) "a start is answered")
      (is (search "next-bar" body) "with the bar frontier"))
    (multiple-value-bind (status body) (music-jam-ask server "bars"
                                                       (list :owner "browser-1" :start 0 :count 1))
      (is (= 200 status) "a bar is answered")
      (is (search "\"bar\":0" (remove #\Space body)) "and it is bar zero"))
    (multiple-value-bind (status body) (music-jam-ask server "bars"
                                                       (list :owner "someone-else" :start 1 :count 1))
      (is (= 400 status) "another browser is not the transport")
      (is (search "transport" body) "and is told why"))))

(deftest music-jam-cell-the-listener-opens-and-closes-on-loopback ()
  (with-band
    (let ((opened (nodecode-music-jam::open-server jam)))
      (unwind-protect
           (progn
             (is (plusp (nodecode-music-jam::band-server-port opened)) "a port is chosen")
             (is (= 48 (length (nodecode-music-jam::band-server-token opened))) "a 24-byte token, in hex")
             (is (search "#" (nodecode-music-jam::page-url opened)) "the address carries it")
             (multiple-value-bind (status mime octets)
                 (nodecode-music-jam::handle opened "GET" "/app.js"
                                             (list (cons "host" (format nil "127.0.0.1:~d"
                                                                         (nodecode-music-jam::band-server-port opened))))
                                             nil)
               (declare (ignore mime))
               (is (= 200 status) "the script is served too")
               (is (search "AudioContext" (sb-ext:octets-to-string octets :external-format :utf-8)) "and it is the page's own")))
        (nodecode-music-jam::close-server opened)
        (is (null (nodecode-music-jam::band-server-socket opened)) "closing leaves no listener")))))

;;; --- /music-jam ----------------------------------------------------------------------

(deftest music-jam-cell-the-command-answers-the-page ()
  (let ((text (nodecode-music-jam::run-command "" "s1")))
    (is (search "Music Jam" text) "the command names the band")
    (is (search "page: http://127.0.0.1:" text) "and hands over the page's address"))
  (unwind-protect
       (let ((text (nodecode-music-jam::run-command "keys darker" "s1")))
         (is (search "changes" text) "a direction typed as an argument is taken as one"))
    (nodecode-music-jam::close-server nodecode-music-jam::*server*)
    (setf nodecode-music-jam::*server* nil nodecode-music-jam::*jam* nil)))
