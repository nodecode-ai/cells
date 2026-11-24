;;;; cell.lisp --- the band as a cell: ten tools, one page, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing runs until a tool or the command does. jam_start opens the
;;;; loopback page; the page owns the audio clock and asks the band for bars;
;;;; the tools queue changes the page picks up at the next bar. Everything a
;;;; tool answers is JSON text, as the original's tools answered it.

(in-package #:nodecode-music-jam)

(declaim (ftype (function (&rest t) *) setting))

(defvar *server* nil
  "The listener the page is served on, or NIL before jam_start.")

(defparameter *primer*
  "Music Jam is a four-piece band in this process: drums, bass, keys and lead,
each with its own controls and a four-bar pattern. jam_start opens a page on
127.0.0.1 with a token; the page plays the band through the browser's own
Web Audio, keeps the audio clock, and is the only place bars are assigned.
The tools conduct the band in plain words or with numbers; a change waits for
the next bar instead of jumping in mid-phrase. Nothing plays without a click
on the page's Play button."
  "What (help :music-jam) answers while the cell runs.")

;;; --- answers ----------------------------------------------------------------------

(defun answer (value)
  "VALUE as the JSON text a tool answers with: plists and alists as objects."
  (sb-ext:octets-to-string (nlk:encode-json-octets (jsonable value))
                            :external-format :utf-8))

(defun arg (arguments name &key (type :text))
  "NAME in the tool call's ARGUMENTS, as TYPE."
  (nlk:json-value arguments type name))

(defun controls-arg (arguments)
  "The controls a call set, as an alist, or NIL."
  (let ((object (arg arguments "controls" :type :object)))
    (when object
      (loop for name in *control-names*
            for value = (nlk:json-value object :number name)
            when value collect (cons name value)))))

;;; --- the tools ---------------------------------------------------------------------

(defun start-tool (arguments session)
  "Open the band's page and answer its address."
  (declare (ignore arguments session))
  (let* ((jam (or *jam* (setf *jam* (make-jam))))
         (server (or *server* (setf *server* (open-server jam)))))
    (answer (list :url (page-url server)
                  :status "waiting for the browser's Play button"
                  :note "the address is private: it carries the page's token"))))

(defun inspect-tool (arguments session)
  "The band as it stands: who plays what, what waits for a bar, what is recorded."
  (declare (ignore arguments session))
  (answer (status (running-jam))))

(defun stop-tool (arguments session)
  "Stop the transport and cancel every queued change."
  (declare (ignore arguments session))
  (answer (stop-band (running-jam))))

(defun direct-tool (arguments session)
  "TEXT as a direction: which musician changes, and how."
  (declare (ignore session))
  (answer (direct (running-jam) (arg arguments "text"))))

(defun configure-tool (arguments session)
  "ROLE's controls changed, every other musician left as it is."
  (declare (ignore session))
  (answer (restate (running-jam) (arg arguments "role") (controls-arg arguments)
                   :after (or (arg arguments "after_bars" :type :number) 0))))

(defun schedule-tool (arguments session)
  "ROLE's change, AFTER_BARS past the next bar, optionally muted or back."
  (declare (ignore session))
  (answer (restate (running-jam) (arg arguments "role") (controls-arg arguments)
                   :after (or (arg arguments "after_bars" :type :number) 0)
                   :muted (arg arguments "muted" :type :boolean))))

(defun undo-tool (arguments session)
  "ROLE one change back."
  (declare (ignore session))
  (answer (undo (running-jam) (arg arguments "role"))))

(defun save-tool (arguments session)
  "The whole session written under the Nodecode home."
  (declare (ignore arguments session))
  (answer (save (running-jam))))

(defun restore-tool (arguments session)
  "The last saved session back, stopped."
  (declare (ignore arguments session))
  (let ((path (session-file)))
    (unless (probe-file path) (fail "no jam has been saved yet"))
    (answer (restore (running-jam)
                     (with-open-file (in path :external-format :utf-8)
                       (let ((*read-eval* nil) (*package* (find-package :nodecode-music-jam)))
                         (read in)))))))

(defun export-tool (arguments session)
  "The performance so far as a standard MIDI file."
  (declare (ignore session))
  (let ((format (or (arg arguments "format" :type :text) "midi")))
    (unless (equal format "midi")
      (fail "this rewrite exports ~s only; the page plays WAV and keeps none" format))
    (answer (export-midi (running-jam)))))

;;; --- /music-jam -------------------------------------------------------------------

(defun run-command (args session-id)
  "The band's page, and the state of the band behind it."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond
      ((and (plusp (length text)) (not (member (char text 0) '(#\/ #\Space))))
       (if *jam*
           (answer (direct *jam* text))
           "no jam is running: jam_start first, then direct the band"))
      (t
       (let* ((jam (or *jam* (setf *jam* (make-jam))))
              (server (or *server* (setf *server* (open-server jam))))
              (state (status jam)))
         (format nil "Music Jam - ~a~%page: ~a~%~d bars recorded, ~d change~:p waiting for the next bar.~%~
Say /music-jam with a direction, or call jam_direct."
                 (if (getf state :playing) "playing" "stopped, waiting for the page's Play")
                 (page-url server)
                 (getf state :recorded-bars)
                 (getf state :pending)))))))

;;; --- the declaration -----------------------------------------------------------------

(defparameter +empty-schema+
  (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object))
  "The schema of a tool that takes no arguments.")

(defparameter +role-schema+
  (nlk:make-json-object "type" "object"
                        "properties" (nlk:make-json-object
                                      "role" (nlk:make-json-object
                                              "type" "string"
                                              "enum" (vector "drums" "bass" "keys" "lead")
                                              "description" "the one musician this change is for"))
                        "required" (vector "role"))
  "One musician named: jam_undo's arguments.")

(defparameter +controls-schema+
  (nlk:make-json-object
   "type" "object"
   "properties" (nlk:make-json-object
                 "role" (nlk:make-json-object "type" "string"
                                               "enum" (vector "drums" "bass" "keys" "lead")
                                               "description" "the one musician this change is for")
                 "controls" (nlk:make-json-object
                             "type" "object"
                             "properties" (nlk:make-json-object
                                           "density" (nlk:make-json-object "type" "number" "minimum" 0 "maximum" 1)
                                           "energy" (nlk:make-json-object "type" "number" "minimum" 0 "maximum" 1)
                                           "complexity" (nlk:make-json-object "type" "number" "minimum" 0 "maximum" 1)
                                           "register" (nlk:make-json-object "type" "number" "minimum" -12 "maximum" 12)
                                           "swing" (nlk:make-json-object "type" "number" "minimum" 0 "maximum" 1)
                                           "variation" (nlk:make-json-object "type" "number" "minimum" 0 "maximum" 1))
                             "description" "0 to 1 each, except register at -12 to 12 semitones")
                 "after_bars" (nlk:make-json-object "type" "integer" "minimum" 0 "maximum" 64
                                                     "description" "whole bars past the next free bar"))
   "required" (vector "role"))
  "One musician and the controls to set: jam_configure's arguments.")

(defparameter +schedule-schema+
  (nlk:make-json-object
   "type" "object"
   "properties" (nlk:make-json-object
                 "role" (nlk:make-json-object "type" "string"
                                               "enum" (vector "drums" "bass" "keys" "lead")
                                               "description" "the one musician this change is for")
                 "controls" (nlk:make-json-object "type" "object"
                                                   "description" "0 to 1 each, except register at -12 to 12 semitones")
                 "after_bars" (nlk:make-json-object "type" "integer" "minimum" 0 "maximum" 64
                                                     "description" "whole bars past the next free bar")
                 "muted" (nlk:make-json-object "type" "boolean"
                                                "description" "TRUE mutes this musician, FALSE brings it back"))
   "required" (vector "role"))
  "One musician, a delay and a mute: jam_schedule's arguments.")

(defparameter +direct-schema+
  (nlk:make-json-object
   "type" "object"
   "properties" (nlk:make-json-object
                 "text" (nlk:make-json-object
                         "type" "string" "minLength" 1 "maxLength" 2000
                         "description" "name a musician and say less busy, darker, a fill, more energy, swing, an answer or space for a solo"))
   "required" (vector "text"))
  "A direction in words: jam_direct's arguments.")

(defparameter +export-schema+
  (nlk:make-json-object "type" "object"
                        "properties" (nlk:make-json-object
                                      "format" (nlk:make-json-object "type" "string"
                                                                      "enum" (vector "midi")
                                                                      "description" "the file this rewrite writes"))
                        "required" (vector "format"))
  "jam_export's arguments.")

(nle:define-cell music-jam
  (:section ("music-jam")
    (:guide "the band's page is served on 127.0.0.1; port 0 picks a free port, and a fixed one is what a bookmark needs")
    ("port" :integer :default 0 :min 0
            :doc "the loopback port the page is served on; 0 picks an available one"))
  (:tool "jam_start" (list :description "Open the local band page; audio needs a click on the page's Play button."
                             :parameters +empty-schema+)
         'start-tool)
  (:tool "jam_inspect" (list :description "Inspect the musicians' intentions, the changes queued for a bar, and the playback."
                             :parameters +empty-schema+)
         'inspect-tool)
  (:tool "jam_stop" (list :description "Stop playback and cancel every queued change."
                          :parameters +empty-schema+)
         'stop-tool)
  (:tool "jam_direct" (list :description "Direct the band in plain words: name a musician and say less busy, darker, a fill, more energy, swing, an answer or space for a solo."
                            :parameters +direct-schema+)
         'direct-tool)
  (:tool "jam_configure" (list :description "Adjust one musician's controls, 0 to 1 except register at -12 to 12, and leave the others as they are."
                               :parameters +controls-schema+)
         'configure-tool)
  (:tool "jam_schedule" (list :description "Schedule one musician's change after N bars past the next free bar, or mute it and bring it back."
                              :parameters +schedule-schema+)
         'schedule-tool)
  (:tool "jam_undo" (list :description "Undo only the chosen musician, at the next bar."
                          :parameters +role-schema+)
         'undo-tool)
  (:tool "jam_save" (list :description "Save the whole session under the Nodecode home."
                          :parameters +empty-schema+)
         'save-tool)
  (:tool "jam_restore" (list :description "Restore the last saved session, stopped."
                             :parameters +empty-schema+)
         'restore-tool)
  (:tool "jam_export" (list :description "Export the recorded bars as a standard MIDI file and answer where it is."
                            :parameters +export-schema+)
         'export-tool)
  (:command "music-jam" 'run-command
            :description "The band's page and its state, or a direction to the band"
            :argument-hint "DIRECTION")
  (:help :music-jam "music-jam: a four-piece synthesized band on a local page, conducted by tools" *primer*)
  (:start (lambda ()
            (setf *jam* nil *server* nil *port* (max 0 (setting :port)))
            (nle:on-stop (lambda ()
                           (close-server *server*)
                           (when *jam* (stop-band *jam*))
                           (setf *server* nil *jam* nil)))
            (nle:notice "music-jam: jam_start opens the band's page" :key "nodecode-music-jam"))))
