;;;; cell-test.lisp --- the cell against a stubbed myradio station.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches a real station
;;;; and no port is dialled. What is proved is the cell's own wiring: the
;;;; section (a healthy start, and bad shapes refused), the twelve tools
;;;; through the dispatch the model calls them by -- the URL, the station
;;;; token, the operation and the arguments on the wire -- the station's own
;;;; words coming back as a failure, the owner-only refusals, and /radio.

(in-package #:nodecode.test)

(define-test-slice "myradio" "MYRADIO-CELL-" :start nodecode-myradio:start-cell)

;;; --- the station's answers -------------------------------------------------------

(defparameter +station-status+
  "{\"station\":{\"name\":\"Big Earl Radio\",\"tagline\":\"music for the shop\"},
    \"running\":true,\"elapsed\":75,
    \"current\":{\"title\":\"Big Earl's Theme\",\"artist\":\"The Earls\",\"kind\":\"music\",\"duration\":225},
    \"queue\":[{\"id\":7,\"title\":\"Second Song\",\"artist\":\"Someone\",\"kind\":\"music\",\"duration\":180}],
    \"library\":[{\"id\":\"t1\",\"title\":\"Big Earl's Theme\",\"artist\":\"The Earls\",\"kind\":\"music\",\"duration\":225},
                {\"id\":\"t2\",\"title\":\"Second Song\",\"artist\":\"Someone\",\"kind\":\"music\",\"duration\":180},
                {\"id\":\"t3\",\"title\":\"Station ID\",\"kind\":\"speech\",\"duration\":12}],
    \"history\":[],\"schedules\":[],
    \"capabilities\":{\"music_generation\":true,\"speech_generation\":false},
    \"broadcast\":{\"state\":\"disconnected\",\"configured\":false},
    \"stream\":{\"listeners\":0}}"
  "What a running station answers /api/status, with two songs and one speech cart.")

(defparameter +operations+
  "[{\"name\":\"shows.plan\",\"description\":\"Plan the next hours\",\"arguments\":{\"hours\":{\"type\":\"integer\"}}},
    {\"name\":\"broadcast_start\",\"description\":\"Go live\",\"arguments\":{}}]"
  "What a running station answers /api/operations.")

(defparameter *asked* nil
  "Every request the stub answered, newest first, as (METHOD URL HEADERS CONTENT).")

(defparameter *answer* nil
  "When set, (FRAGMENT . (TEXT STATUS)): what the stub answers for a URL holding FRAGMENT.")

;;; --- fixtures --------------------------------------------------------------------

(defun myradio-data-folder (&optional (token "control-code-0123456789-abcdefghij"))
  "A fresh station data folder holding control.token, as the station writes one."
  (let ((dir (ensure-directories-exist (temp-path "myradio"))))
    (when token
      (with-open-file (out (merge-pathnames "control.token" dir) :direction :output
                                                                   :if-exists :supersede
                                                                   :if-does-not-exist :create)
        (write-string token out)
        (terpri out)))
    dir))

(defun myradio-stub (method url options)
  "NLK:HTTP against a station that answers from here; nothing leaves the machine."
  (push (list method url (getf options :headers) (getf options :content)) *asked*)
  (cond ((and *answer* (search (car *answer*) url))
         (values (first (cdr *answer*)) (second (cdr *answer*)) nil nil))
        ((search "/api/status" url) (values +station-status+ 200 nil nil))
        ((search "/api/operations" url) (values +operations+ 200 nil nil))
        ((search "/api/action" url) (values "{\"ok\":true}" 200 nil nil))
        (t (values "{}" 404 nil nil))))

(defmacro with-myradio-http (&body body)
  "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*asked* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (myradio-stub method url options))
       ,@body)))

(defun myradio-request (&optional (index 0))
  "The INDEX'th newest request the cell sent, as (METHOD URL HEADERS CONTENT)."
  (nth index *asked*))

(defun myradio-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun myradio-sent (&optional (index 0))
  "The JSON body of the INDEX'th newest request, decoded."
  (let ((content (fourth (myradio-request index))))
    (and content (nlk:decode-json content))))

(defun myradio-called (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "myradio-call" name arguments)))

(defun myradio-started (&rest pairs)
  "The cell started on the fixture's data folder, plus PAIRS."
  (apply #'myradio-start (append (list "data_dir" (namestring (myradio-data-folder))) pairs)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "myradio"
  (:fixture with-myradio-http)
  (:help :myradio)
  (:command "radio")
  (:running
   (is (nle::find-tool "myradio_status") "the status tool is registered")
   (is (nle::find-tool "myradio_queue_add") "and the queue")
   (is (nle::find-tool "myradio_make") "and the makers")
   (is (nle::find-tool "myradio_run") "and one escape hatch for the rest")
   (is (= 12 (count-if (lambda (entry) (eql 0 (search "myradio_" (first entry)))) nle::*tools*)) "twelve of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "myradio_status")) "and stop takes every one back")
   (is (null (nle::find-tool "myradio_run"))))
  (:refused ("port" 0) ("timeout_seconds" 0)))

;;; --- the wire ------------------------------------------------------------------

(deftest myradio-cell-status-reads-the-running-station (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (is-carrying (text (myradio-called "myradio_status" "{}"))
                 ("\"station\":\"Big Earl Radio\"" "the station's name")
                 ("Big Earl's Theme" "what is on air")
                 ("\"queue_id\":\"7\"" "the queue entry's id as a string, as every queue verb wants it")
                 ("\"dj_voice\":false" "the makers that are ready")
                 ("\"library_counts\"" "and the count by kind"))))

(deftest myradio-cell-the-token-and-port-ride-every-request (with-myradio-http)
  (let* ((dir (myradio-data-folder "control-code-fedcba9876543210-zyxwvu"))
         (resource (with-cell-stop ((myradio-start "data_dir" (namestring dir) "port" 8766))
                     (myradio-called "myradio_status" "{}")
                     (myradio-request))))
    (is (equal :get (first resource)) "the status is a GET")
    (is (equal "http://127.0.0.1:8766/api/status" (second resource)) "on the port the section names")
    (is (equal "control-code-fedcba9876543210-zyxwvu" (myradio-header resource "x-myradio-token")) "with the station's own token, read from its data folder")
    (is (equal "nodecode" (myradio-header resource "x-myradio-client")) "and the client that is asking")))

(deftest myradio-cell-queue-add-posts-one-operation (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (myradio-called "myradio_queue_add" "{\"track_id\": \"t2\", \"position\": 0}")
    (let* ((request (myradio-request))
           (body (myradio-sent)))
      (is (equal :post (first request)) "a change is a POST")
      (is (search "/api/action" (second request)) "to the one action endpoint")
      (is (equal "application/json" (myradio-header request "content-type")) "carrying JSON")
      (is (equal "queue_add" (nlk:json-value body :string "operation")) "the station's own operation name")
      (is (equal "t2" (nlk:json-value (nlk:json-value body :object "arguments") :string "track_id")) "and the arguments the model gave"))))

(deftest myradio-cell-queue-move-is-its-own-operation (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (myradio-called "myradio_queue_move" "{\"queue_id\": \"7\", \"position\": 0}")
    (is (equal "queue_move" (nlk:json-value (myradio-sent) :string "operation")) "a move is not an add")))

(deftest myradio-cell-library-search-answers-matches (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (let ((answer (nlk:decode-json (myradio-called "myradio_library" "{\"query\": \"second\"}"))))
      (is (equal 1 (nlk:json-value answer :integer "matches")) "one track holds the word")
      (is (equal "Second Song" (nlk:json-value (aref (nlk:json-value answer :array "tracks") 0) :string "title")) "and the model reads its title and track_id"))))

(deftest myradio-cell-a-station-that-refuses-says-why (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (let ((*answer* '("/api/action" . ("{\"error\": \"That track is not in the library.\"}" 400))))
      (is-carrying (text (myradio-called "myradio_queue_add" "{\"track_id\": \"nope\"}"))
                   ("That track is not in the library." "the station's own words")))))

(deftest myradio-cell-going-live-stays-the-owners (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (is-carrying (text (myradio-called "myradio_run" "{\"operation\": \"broadcast_start\"}"))
                 ("only the owner" "the model is told, not obeyed"))))

(deftest myradio-cell-run-passes-the-operation-through (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (myradio-called "myradio_run" "{\"operation\": \"shows.plan\", \"arguments\": {\"hours\": 3}}")
    (let ((body (myradio-sent)))
      (is (equal "shows.plan" (nlk:json-value body :string "operation")) "the catalog's own name")
      (is (equal 3 (nlk:json-value (nlk:json-value body :object "arguments") :integer "hours")) "and the arguments it was given"))))

(deftest myradio-cell-no-data-folder-refuses (with-myradio-http)
  (let ((dir (ensure-directories-exist (temp-path "myradio-empty"))))
    (with-cell-stop ((myradio-start "data_dir" (namestring dir)))
      (is-carrying (text (myradio-called "myradio_status" "{}"))
                   ("no myradio station found" "the folder's own words")
                   ("control.token" "naming the file the station writes"))
      (is (null *asked*) "and nothing was dialled"))))

;;; --- /radio --------------------------------------------------------------------

(deftest myradio-cell-radio-answers-the-card (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (is-carrying (text (cell-entry "nodecode-myradio" "radio" ""))
                 ("Big Earl Radio" "the station's name")
                 ("ON AIR" "what is on air")
                 ("1:15 / 3:45" "the time in the track")
                 ("UP NEXT  1. Second Song" "and what is next"))))

(deftest myradio-cell-radio-play-queues-the-match-next (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (is-carrying (text (cell-entry "nodecode-myradio" "radio" "play big earl"))
                 ("Up next: Big Earl's Theme" "the best match goes out"))
    (let ((body (myradio-sent)))
      (is (equal "queue_add" (nlk:json-value body :string "operation")) "queued on the station")
      (is (equal 0 (nlk:json-value (nlk:json-value body :object "arguments") :integer "position")) "at the front"))))

(deftest myradio-cell-radio-skips-on-request (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (is (equal "Skipped." (cell-entry "nodecode-myradio" "radio" "skip")) "the owner's word does it")
    (is (equal "transport_skip" (nlk:json-value (myradio-sent) :string "operation")) "on the station")))

(deftest myradio-cell-radio-help-lists-the-words (with-myradio-http)
  (with-cell-stop ((myradio-started))
    (is-carrying (text (cell-entry "nodecode-myradio" "radio" "what"))
                 ("/radio play <words>" "the words it knows"))))
