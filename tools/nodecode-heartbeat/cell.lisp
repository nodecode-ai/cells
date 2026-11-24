;;;; cell.lisp --- the manual, the contract, the verbs, /heartbeat, START-CELL.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The model reaches the cell two ways, both the same code: the "pulse"
;;;; tool (one boundary contract, so a call the declaration does not carry is
;;;; refused before the body), and the HEARTBEAT package's verbs called from
;;;; eval. The operator reaches it through /heartbeat. A watch wakes the
;;;; session that armed it -- there is no destination argument at all -- and
;;;; the registry survives a restart in the home's heartbeat/watches.json.

(in-package #:nodecode-heartbeat)

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "Wakes and watchers are available through the nodecode-heartbeat cell: a watch is a timed
check-in, or a file, url or command looked at out of band, that wakes THIS session when it is
due, changes or breaks. There is no destination: a watch always wakes the session that armed it.
A probe costs no model turn until it fires. Verbs callable from eval, each answering a string:
  (heartbeat:add :title \"nightly build\" :kind \"command\" :command '(\"gh\" \"run\" \"list\")
                 :every \"10m\" :fire-on \"failure\" :note \"say what broke\")
      kinds: \"every\" (a wake on a clock), \"after\" (one wake, once, that far from now),
      \"file\" (:path), \"url\" (:url), \"command\" (:command, an argv run without a shell).
      :every is \"30s\", \"10m\", \"2h\", \"1d\"; :fire-on is \"failure\" (a break), \"change\"
      (any new value) or \"success\" (a recovery); :max-fires caps the wakes (0 is no limit).
  (heartbeat:watches)   one line per watch.   (heartbeat:show \"id\")   its whole record.
  (heartbeat:pause \"id\")   (heartbeat:resume \"id\")   (heartbeat:forget \"id\")
  (heartbeat:check \"id\")   looks now and says what it saw.
A time watch submits its wake as this session's next request: end your turn, and it arrives.
A probe that fires does the same, with the receipt of what it saw. The operator types /heartbeat."
  "What (help :heartbeat) answers while the cell runs.")

;;; --- arming a watch ------------------------------------------------------------------

(defun arm (&key title (note "") (kind "every") (every "") path url command
                 (fire-on "failure") (max-fires 0) (now (get-universal-time)))
  "The one place a watch is made: check it, mint its id, record it under the lock, save it.
=> the watch plist. Every refusal is HEARTBEAT-ERROR."
  (let ((title (and title (string-trim '(#\Space #\Tab) title))))
    (unless (and title (plusp (length title)))
      (fail "a watch needs a title: what it is for, in a few words"))
    (unless (member kind +kinds+ :test #'equal)
      (fail "kind is one of ~{~a~^, ~}, not ~s" +kinds+ kind))
    (unless (member fire-on +fire-on+ :test #'equal)
      (fail "fire_on is one of ~{~a~^, ~}, not ~s" +fire-on+ fire-on))
    (when (>= (length *watches*) (setting :max-watches))
      (fail "~d watches already stand (max_watches): (heartbeat:forget \"id\") one first"
            (setting :max-watches)))
    (let* ((seconds (parse-duration every))
           (command (typecase command
                      (null #())
                      (string (vector command))
                      (vector command)
                      (list (coerce command 'vector))))
           (path (and path (plusp (length path)) path))
           (url (and url (plusp (length url)) url))
           (probe-p (member kind +probe-kinds+ :test #'equal)))
      (unless (and seconds (plusp seconds))
        (fail "every is how often it looks, as \"30s\", \"10m\", \"2h\" or \"1d\": ~s names no duration" every))
      (cond ((equal kind "file")
             (unless path (fail "a file watch needs path: the file to watch")))
            ((equal kind "url")
             (unless (and url (cl-ppcre:scan "^https?://" url))
               (fail "a url watch needs an http:// or https:// url, not ~s" url)))
            ((equal kind "command")
             (unless (plusp (length command))
               (fail "a command watch needs command: the argv to run, as a list of strings")))
            ((equal kind "after")
             (when (plusp max-fires) (fail "an after watch fires once: leave max_fires at 0"))))
      (let* ((session (or (arming-session)
                          (fail "arm a watch from a session: it wakes the session that armed it")))
             (watch (list :id (unused-id (id-slug title))
                          :title title
                          :note note
                          :kind kind
                          :every seconds
                          :path (and (equal kind "file") path)
                          :url (and (equal kind "url") url)
                          :command (if (equal kind "command") command #())
                          :fire-on fire-on
                          :session session
                          :next-at (if probe-p now (+ now seconds))
                          :last-value nil
                          :last-ok nil
                          :last-state nil
                          :fires 0
                          :max-fires (max 0 (or max-fires 0))
                          :paused nil
                          :created-at now)))
        (with-heartbeat-lock (push watch *watches*))
        (save-watches)
        watch))))

;;; --- the verbs ---------------------------------------------------------------------

(defun require-watch (id)
  "The watch ID names, or a refusal the model can read."
  (or (find-watch id)
      (fail "no watch ~s stands: (heartbeat:watches) lists the ids" id)))

(define-verb add (&key title note kind every path url command fire-on max-fires)
  "Arm a watch: a timed check-in that wakes this session, or a file, url or command
looked at out of band that wakes it only when it changes or breaks."
  (let ((watch (arm :title title :note (or note "") :kind (or kind "every")
                    :every (or every "") :path path :url url :command command
                    :fire-on (or fire-on "failure") :max-fires (or max-fires 0))))
    (format nil "armed ~a: ~a" (watch-get watch :id) (watch-line watch))))

(define-verb watches (&optional (now (get-universal-time)))
  "Every standing watch, one line each, newest first."
  (if (null *watches*)
      "no watch stands: arm one with the pulse tool or (heartbeat:add :title \"...\" :kind \"url\" :url \"...\" :every \"10m\")"
      (format nil "~{~a~%~}"
              (mapcar (lambda (watch) (watch-line watch now))
                      (with-heartbeat-lock (copy-list *watches*))))))

(define-verb show (id)
  "One watch's whole record."
  (let ((watch (require-watch id)))
    (format nil "~a~%~{  ~a: ~a~%~}"
            (watch-line watch)
            (loop for (key value) on watch by #'cddr
                  append (list (string-downcase (symbol-name key)) value)))))

(define-verb pause (id)
  "Stop a watch's clock. It stays armed and forgets nothing."
  (let ((watch (require-watch id)))
    (with-heartbeat-lock (setf (watch-get watch :paused) t))
    (save-watches)
    (format nil "paused ~a: ~a" (watch-get watch :id) (watch-trigger-text watch))))

(define-verb resume (id)
  "Start a paused watch's clock again, from now."
  (let ((watch (require-watch id)))
    (with-heartbeat-lock
      (setf (watch-get watch :paused) nil
            (watch-get watch :next-at) (get-universal-time)))
    (save-watches)
    (format nil "resumed ~a: ~a" (watch-get watch :id) (watch-trigger-text watch))))

(define-verb forget (id)
  "Take a watch away for good."
  (let ((watch (require-watch id)))
    (with-heartbeat-lock (setf *watches* (cl:remove watch *watches* :test #'eq)))
    (save-watches)
    (nle:notice nil :key (format nil "heartbeat:~a" (watch-get watch :id)))
    (format nil "forgot ~a" (watch-get watch :id))))

(define-verb check (id)
  "Look at a watch's subject now, without waiting for its clock, and say what it saw."
  (let* ((watch (require-watch id))
         (probe-p (member (watch-get watch :kind) +probe-kinds+ :test #'equal))
         (state (and probe-p (probe-watch watch))))
    (if (null probe-p)
        (format nil "~a: ~a, ~a" (watch-get watch :id) (watch-trigger-text watch)
                (watch-when-text watch))
        (let ((fires (watch-fires-p watch state)))
          (with-heartbeat-lock
            (setf (watch-get watch :last-value) (getf state :value)
                  (watch-get watch :last-ok) (getf state :ok)
                  (watch-get watch :last-state) (if (getf state :ok) "ok" "fail")))
          (save-watches)
          (format nil "~a: ~a -- ~a~:[~; It would wake its session now.~]"
                  (watch-get watch :id) (watch-trigger-text watch) (getf state :detail) fires)))))

;;; --- the model's one boundary ------------------------------------------------------------

(nlk:define-contract pulse
  (:doc "Arm, inspect or stop a heartbeat watch: a timed check-in that wakes this session, or a file, url or command checked out of band that wakes it only when it changes or breaks. A watch always wakes the session that armed it; there is no destination.")
  ("action" :text :required t
            :options '("create" "list" "show" "pause" "resume" "remove" "check")
            :doc "create arms one; the rest act on an id")
  ("id" :string :default "" :doc "the watch's id, for everything but create")
  ("title" :string :default "watch"
           :doc "a few words for what this watches; the id is made from it")
  ("note" :string :default ""
          :doc "what the operator wants of a wake from this watch, in one sentence")
  ("kind" :string :default "every" :options '("every" "after" "file" "url" "command")
          :doc "every: a wake on a clock; after: one wake, that far from now; file, url, command: a probe")
  ("every" :string :default "" :slot every-seconds
           :doc "how often it looks, or for after how far from now: \"30s\", \"10m\", \"2h\", \"1d\"")
  ("path" :string :default "" :doc "the file a file watch looks at")
  ("url" :string :default "" :doc "the http or https url a url watch looks at")
  ("command" :array :of :string :default #()
             :doc "the argv a command watch runs, without a shell")
  ("fire_on" :string :default "failure" :options '("failure" "change" "success")
             :doc "what a probe speaks up about: failure (a break), change (any new value), success (a recovery)")
  ("max_fires" :integer :default 0 :doc "how many wakes it may send; 0 is no limit"))

(defun run-pulse (record)
  "The pulse tool: run-pulse on the value the contract materialized."
  (let ((action (pulse-action record)))
    (cond ((equal action "create")
           (let ((watch (arm :title (pulse-title record)
                             :note (pulse-note record)
                             :kind (pulse-kind record)
                             :every (pulse-every-seconds record)
                             :path (pulse-path record)
                             :url (pulse-url record)
                             :command (pulse-command record)
                             :fire-on (pulse-fire-on record)
                             :max-fires (pulse-max-fires record))))
             (format nil "armed ~a: ~a A probe costs nothing until it fires; a clock watch arrives as this session's next request."
                     (watch-get watch :id) (watch-line watch))))
          ((equal action "list") (watches))
          ((equal action "show") (show (pulse-id record)))
          ((equal action "pause") (pause (pulse-id record)))
          ((equal action "resume") (resume (pulse-id record)))
          ((equal action "remove") (forget (pulse-id record)))
          ((equal action "check") (check (pulse-id record)))
          (t (fail "action is one of create, list, show, pause, resume, remove or check, not ~s" action)))))

;;; --- /heartbeat --------------------------------------------------------------------------

(defun run-slash (args session-id)
  "One /heartbeat: no argument lists the watches; \"pause ID\" (and resume, show, check,
remove) acts on one. => the panel's text."
  (declare (ignore session-id))
  (let* ((words (cl:remove "" (cl-ppcre:split "\\s+" (string-trim '(#\Space #\Tab) (or args "")))
                           :test #'equal))
         (verb (first words))
         (id (second words)))
    (cond ((null words) (watches))
          ((member verb '("list" "watches") :test #'equal) (watches))
          ((and (member verb '("pause" "resume" "show" "check" "remove" "forget") :test #'equal) id)
           (cond ((equal verb "pause") (pause id))
                 ((equal verb "resume") (resume id))
                 ((member verb '("remove" "forget") :test #'equal) (forget id))
                 ((equal verb "show") (show id))
                 (t (check id))))
          (t (format nil "usage:~%  /heartbeat~%  /heartbeat pause|resume|show|check|remove ID")))))

;;; --- start ---------------------------------------------------------------------------------

(defun install ()
  "Read the registry back, start the ticker, and register the inverse."
  (with-heartbeat-lock (setf *watches* '()) (load-watches))
  (when (and *tick-p* (null *worker*))
    (setf *worker* (nlk:worker-start "heartbeat" #'tick
                                      :wake (lambda () (setting :tick-seconds))
                                      :prime t)))
  (nle:on-stop (lambda ()
                 (setf *worker* (nlk:worker-stop *worker*))
                 (with-heartbeat-lock (setf *watches* '()))))
  (let ((standing (with-heartbeat-lock (length *watches*))))
    (when (plusp standing)
      (nle:keep-running (format nil "~d heartbeat watch~:p" standing)))))

(nle:define-cell heartbeat
  (:section ("heartbeat")
    (:guide "tick_seconds is how often the watches are looked at; max_watches caps how many may stand")
    ("tick_seconds" :integer :default 15 :min 5
                    :doc "seconds between two passes over the watches")
    ("max_watches" :integer :default 25 :min 1
                   :doc "how many watches may stand at once")
    ("check_timeout_seconds" :integer :default 10 :min 1
                             :doc "one url check's ceiling"))
  (:tool "pulse" (nlk:contract 'pulse) 'run-pulse)
  (:help :heartbeat
         "heartbeat: a timed check-in, or a file, url or command that wakes this session when it changes"
         +primer+)
  (:command "heartbeat" 'run-slash
            :description "Wakes and watchers: what stands, and pausing, resuming, checking or removing one")
  (:start #'install))
