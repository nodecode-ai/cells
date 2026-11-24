;;;; engine.lisp --- the band: four musicians, a queue of changes waiting for
;;;; the next bar, the bars the browser has already clocked out, and undo.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original let the browser alone assign and report audio-clock bars and
;;;; kept every change in a queue until a bar boundary reached its earliest
;;;; bar. This keeps the same order: a change is queued with the next free
;;;; bar as its floor, and BARS moves it into the session when the browser
;;;; asks for a bar it may start on. Nothing here waits, and nothing here
;;;; plays: the page does.

(in-package #:nodecode-music-jam)

(defstruct (jam (:constructor make-jam ()))
  "One session's state and the lock over it."
  (mutex (sb-thread:make-mutex :name "music-jam") :read-only t)
  (session (initial-session))
  (playing nil)
  (generation 0)
  (jobs '())
  (phrase '())
  (clock (list :bar -1 :beat 0 :audio-time 0 :late-events 0))
  (owner nil)
  (seen 0)
  (revisions (loop for role in *roles* collect (cons role 0))))

(defmacro with-jam ((jam) &body body)
  "BODY under JAM's lock, which is recursive: a queued change is made from
inside a section that already holds it."
  `(sb-thread:with-recursive-lock ((jam-mutex ,jam)) ,@body))

(defvar *jam* nil
  "The one band this process plays, or NIL until a jam starts.")

(defun running-jam ()
  "The band, or a refusal saying how to start one."
  (or *jam* (fail "no jam is running: jam_start first")))

;;; --- what the band says about itself -----------------------------------------------

(defun status (jam)
  "JAM's state as data: the session without its performance, the clock, the
jobs, who is playing and how much is recorded."
  (with-jam (jam)
    (let ((session (copy-tree (jam-session jam))))
      (remf session :performance)
      (list :session session
            :playing (jam-playing jam)
            :clock (copy-tree (jam-clock jam))
            :jobs (last (copy-tree (jam-jobs jam)) 20)
            :recorded-bars (length (getf (jam-session jam) :performance))
            :generation (jam-generation jam)
            :pending (length (getf (jam-session jam) :pending))))))

;;; --- queuing ----------------------------------------------------------------------

(defun queue-change (jam role state &key (after 0) (label "change") job)
  "Queue STATE for ROLE, no earlier than AFTER bars past the next free bar."
  (let ((session (jam-session jam)))
    (when (>= (length (getf session :pending)) 64)
      (fail "the arrangement queue is full: stop or save first"))
    (setf (getf (jam-session jam) :pending)
          (append (getf session :pending)
                  (list (list :id (subseq (format nil "~36r" (random (expt 36 12))) 0 10)
                              :role role :state (copy-tree state)
                              :earliest (+ (getf session :next-bar) (round after))
                              :label label :job job))))
    (getf (jam-session jam) :pending)))

(defun restate (jam role values &key (after 0) muted)
  "ROLE's state with VALUES merged into its controls, its pattern replayed
from those controls and its intention set to the operator's own words."
  (with-jam (jam)
    (let ((session (jam-session jam)))
      (role-range role)
      (let* ((state (copy-tree (role-state session role)))
             (controls (append (controls values) (getf state :controls)))
             (merged (loop for name in *control-names*
                           collect (cons name
                                         (or (cdr (assoc name controls :test #'equal))
                                             (state-control state name))))))
        (setf (getf state :controls) merged
              (getf state :pattern) (local-pattern role merged (getf state :motif))
              (getf state :intention) "Your controls, in the pocket.")
        (when muted (setf (getf state :muted) muted))
        (setf (cdr (assoc role (jam-revisions jam) :test #'equal))
              (1+ (cdr (assoc role (jam-revisions jam) :test #'equal))))
        (queue-change jam role state :after after :label (if muted "mute" "change"))
        (list :status "waiting for the next bar" :role role :after after)))))

;;; --- what the operator asks for -----------------------------------------------------

(defun direction-plan (text session)
  "TEXT, a direction in words, as changes: the original's stand-alone
vocabulary, since no model is asked here."
  (let* ((low (string-downcase text))
         (named (loop for role in *roles*
                      when (or (search role low)
                               (and (equal role "bass") (search "bassist" low))
                               (and (equal role "drums") (search "drummer" low)))
                        collect role))
         (delay (if (or (search "after four" low) (search "after 4" low)) 4 0)))
    (cond
      ((or (search "solo" low) (search "breakdown" low))
       (append (loop for role in '("keys" "lead")
                     collect (list :role role :controls '(("density" . 0.15))
                                   :muted (equal role "lead")
                                   :intention "Space for your phrase." :after 0))
               (loop for role in '("keys" "lead")
                     collect (list :role role :controls (getf (role-state session role) :controls)
                                   :muted nil :intention "Back in the conversation."
                                   :after 4))))
      ((null named)
       (fail "name a musician (drums, bass, keys, lead) and say less busy, darker, a fill, ~
              more energy, swing, an answer or space for a solo"))
      (t
       (loop for role in named
             append
             (let ((c '()))
               (when (some (lambda (word) (search word low)) '("less" "space" "simpler"))
                 (setf c (append c '(("density" . 0.2) ("complexity" . 0.15)))))
               (when (some (lambda (word) (search word low)) '("busier" "more energy" "build"))
                 (setf c (append c '(("density" . 0.8) ("energy" . 0.75)))))
               (when (search "darker" low)
                 (setf c (append c '(("register" . -5) ("energy" . 0.35)))))
               (when (search "fill" low) (setf c (append c '(("complexity" . 0.85)))))
               (when (search "swing" low) (setf c (append c '(("swing" . 0.65)))))
               (when (search "answer" low) (setf c (append c '(("variation" . 0.8)))))
               (when (and (null c)
                          (not (some (lambda (word) (search word low)) '("mute" "back" "return"))))
                 (fail "that direction needs more words: say less busy, darker, a fill, ~
                        more energy, an answer, or mute and return"))
               (append (list (list :role role :controls c :muted (and (search "mute" low) t)
                                   :intention (subseq text 0 (min 180 (length text)))
                                   :after delay))
                       (when (search "fill" low)
                         (list (list :role role
                                     :controls (getf (role-state session role) :controls)
                                     :muted nil :intention "Return to the pocket." :after 1))))))))))

(defun direct (jam text)
  "TEXT as a direction to the band: each change queued for the next bar."
  (unless (and (stringp text) (<= 1 (length (string-trim '(#\Space #\Tab) text)) 2000))
    (fail "a direction is 1 to 2000 characters of words"))
  (let ((changes (direction-plan text (jam-session jam))))
    (with-jam (jam)
      (loop for change in changes
            do (restate jam (getf change :role) (getf change :controls)
                        :after (getf change :after) :muted (getf change :muted)))
      (list :status "planning done" :changes (length changes)
            :activating "the next bar each change was given"))))

(defun undo (jam role)
  "Take ROLE back one change: cancel what it has queued, else restore the
state it last replaced, no earlier than the next bar."
  (with-jam (jam)
    (let* ((session (jam-session jam))
           (pending (remove-if (lambda (change) (equal (getf change :role) role))
                               (getf session :pending))))
      (role-range role)
      (if (/= (length pending) (length (getf session :pending)))
          (progn (setf (getf session :pending) pending)
                 (list :status "cancelled the queued change" :role role))
          (let ((history (cdr (assoc role (getf session :history) :test #'equal))))
            (unless history (fail "nothing to undo for ~a" role))
            (let ((previous (car (last history))))
              (setf (cdr (assoc role (getf session :history) :test #'equal))
                    (butlast history))
              (setf (cdr (assoc role (jam-revisions jam) :test #'equal))
                    (1+ (cdr (assoc role (jam-revisions jam) :test #'equal))))
              (queue-change jam role previous :label "undo")
              (list :status "undo waiting for the next bar" :role role)))))))

;;; --- the browser's clock --------------------------------------------------------------

(defun start-band (jam owner)
  "OWNER's browser takes the transport, if another is not already playing."
  (with-jam (jam)
    (when (and (jam-playing jam) (not (equal (jam-owner jam) owner))
               (< (- (get-internal-real-time) (jam-seen jam))
                  (* 5 internal-time-units-per-second)))
      (fail "another browser is playing this jam"))
    (setf (jam-owner jam) owner (jam-seen jam) (get-internal-real-time) (jam-playing jam) t)
    (list :next-bar (getf (jam-session jam) :next-bar) :generation (jam-generation jam))))

(defun stop-band (jam)
  "Stop the transport and cancel every change still waiting."
  (with-jam (jam)
    (let ((session (jam-session jam)))
      (setf (jam-playing jam) nil (jam-generation jam) (1+ (jam-generation jam)))
      (setf (getf session :pending) '())
      (dolist (job (jam-jobs jam))
        (when (member (getf job :status) '("planning" "waiting") :test #'equal)
          (setf (getf job :status) "cancelled")))
      (list :status "stopped" :generation (jam-generation jam)))))

(defun bars (jam owner start &optional (count 1))
  "Bars START..START+COUNT-1 as the browser plays them: every queued change
whose earliest bar has come moves into the session first, and each bar
carries one turn of its musicians' patterns."
  (let ((start (whole-number start 0 100000))
        (count (whole-number count 1 4)))
    (with-jam (jam)
      (let ((session (jam-session jam)))
        (unless (jam-playing jam) (fail "start the browser first"))
        (unless (equal (jam-owner jam) owner) (fail "this browser is not the transport"))
        (setf (jam-seen jam) (get-internal-real-time))
        (unless (= start (getf session :next-bar))
          (fail "the bar frontier moved: restart the transport"))
        (when (> (+ (length (getf session :performance)) count) 4096)
          (fail "the session is full: save it and start a fresh jam"))
        (let ((out '()))
          (dotimes (offset count)
            (let ((bar (+ start offset)))
              (dolist (change (copy-list (getf session :pending)))
                (when (<= (getf change :earliest) bar)
                  (let* ((role (getf change :role))
                         (history (cdr (assoc role (getf session :history) :test #'equal))))
                    (unless (equal (getf change :label) "undo")
                      (setf history (append history (list (copy-tree (role-state session role))))
                            history (last history (min 64 (length history)))
                            (cdr (assoc role (getf session :history) :test #'equal)) history))
                    (setf (cdr (assoc role (getf session :roles) :test #'equal))
                          (copy-tree (getf change :state)))
                    (setf (getf session :pending) (remove change (getf session :pending)))
                    (setf (getf session :arrangement)
                          (last (append (getf session :arrangement)
                                        (list (list :role role :bar bar
                                                    :label (getf change :label))))
                                4096)))))
              (let ((record (list :bar bar :tempo (getf session :tempo)
                                  :roles (loop for role in *roles*
                                               for state = (role-state session role)
                                               collect (cons role
                                                             (list :pattern (if (getf state :muted)
                                                                                '()
                                                                                (loop for event in (getf state :pattern)
                                                                                      when (= (getf event :bar) (mod bar 4))
                                                                                        collect (copy-tree event)))
                                                                   :intention (getf state :intention))))
                                  :human '())))
                (setf (getf session :performance)
                      (append (getf session :performance) (list (copy-tree record))))
                (push record out))))
          (setf (getf session :next-bar) (+ start count))
          (list :bars (nreverse out) :generation (jam-generation jam)))))))

(defun heartbeat (jam owner clock)
  "The browser's audio clock, as it reports it each beat."
  (with-jam (jam)
    (unless (equal (jam-owner jam) owner) (fail "this browser is not the transport"))
    (setf (jam-clock jam) (list :bar (whole-number (getf clock :bar) -1 100000)
                                :beat (as-number (getf clock :beat) 0 4)
                                :audio-time (as-number (getf clock :audio-time) 0 1e9)
                                :late-events (whole-number (getf clock :late-events) 0 1000000))
          (jam-seen jam) (get-internal-real-time))
    (status jam)))

(defun human (jam events)
  "The operator's own phrase, 1 to 64 notes, kept for the export."
  (unless (and (listp events) (<= 1 (length events) 64))
    (fail "a phrase is 1 to 64 notes"))
  (let ((clean (loop for event in events
                     collect (list :bar (whole-number (getf event :bar) 0 100000)
                                   :beat (as-number (getf event :beat) 0 3.999)
                                   :pitch (whole-number (getf event :pitch) 21 108)
                                   :duration (as-number (getf event :duration) 0.04 4)
                                   :velocity (as-number (getf event :velocity) 0.01 1)))))
    (with-jam (jam)
      (setf (jam-phrase jam) clean)
      (dolist (event clean)
        (let ((bar (find (getf event :bar) (getf (jam-session jam) :performance)
                         :key (lambda (record) (getf record :bar)))))
          (when (and bar (< (length (getf bar :human)) 128))
            (setf (getf bar :human) (append (getf bar :human) (list event))))))
      (list :status "phrase kept" :notes (length clean)))))

;;; --- what outlives the process -----------------------------------------------------

(defun session-file ()
  "Where the saved session is written, under the Nodecode home."
  (nlk:home "music-jam/session.json"))

(defun export-file ()
  "Where the MIDI export is written, under the Nodecode home."
  (nlk:home "music-jam/agent-music-jam.mid"))

(defun save (jam)
  "The whole session, written where the next boot can read it."
  (let ((session (with-jam (jam) (copy-tree (jam-session jam))))
        (path (session-file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil) (*package* (find-package :nodecode-music-jam)))
        (write session :stream out :readably t)))
    (list :status "saved" :bars (length (getf session :performance))
          :file (namestring path))))

(defun restored-session (data)
  "DATA, a saved session, checked enough to play again."
  (unless (and (listp data) (eql (getf data :version) 1))
    (fail "that is no saved jam"))
  (as-number (getf data :tempo) 40 200)
  (unless (equal (getf data :key) "D minor")
    (fail "this ensemble plays D minor only"))
  (dolist (role *roles*)
    (let ((state (role-state data role)))
      (unless state (fail "the saved jam is missing its ~a" role))
      (setf (getf state :controls) (controls (getf state :controls)))
      (notes role (getf state :pattern))))
  (when (> (length (getf data :performance)) 4096)
    (fail "the saved jam is longer than a session may hold"))
  data)

(defun restore (jam data)
  "Put DATA back in place, stopped, with nothing queued."
  (let ((session (restored-session (copy-tree data))))
    (with-jam (jam)
      (stop-band jam)
      (setf (jam-session jam) session
            (jam-jobs jam) '()
            (jam-phrase jam) '()
            (jam-clock jam) (list :bar -1 :beat 0 :audio-time 0 :late-events 0))
      (list :status "restored, stopped" :next-bar (getf session :next-bar)))))

(defun export-midi (jam)
  "The performance so far as a standard MIDI file, written and named."
  (let* ((session (with-jam (jam) (copy-tree (jam-session jam))))
         (bars (or (getf session :performance) '()))
         (path (export-file)))
    (unless bars (fail "nothing is recorded yet: play a few bars first"))
    (ensure-directories-exist path)
    (let ((octets (midi bars (getf (getf session :browser) :sounds))))
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create :element-type '(unsigned-byte 8))
        (write-sequence octets out))
      (list :status "exported" :bars (length bars) :format "audio/midi"
            :file (namestring path) :bytes (length octets)))))
