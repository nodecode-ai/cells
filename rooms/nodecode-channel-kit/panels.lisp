;;;; panels.lisp --- what /channels, /sethome and /agent answer typed alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A command is its name: typed alone in a channel's room, each answers a
;;;; panel (NLE:LIST-DIALOG) whose rows run its lines or ask for the one text
;;;; a line takes, so nobody spells a verb after the name. The lines are the
;;;; commands' own words (status.lisp CHANNELS-COMMAND, operator.lisp
;;;; SETHOME-COMMAND and AGENT-COMMAND); this file only offers them, after
;;;; operator.lisp has defined the hosts and rooms the panels read.

(in-package #:nodecode-channel-kit)

(defun channels-panel (session-id)
  "The dialog bare /channels answers in SESSION-ID: the setup card where a
channel's room asks, a pairing code let in, who is paired, and a row per
paired person that takes their pairing back."
  (nle:list-dialog
   :title "Channels" :summary (format nil "~d channel~:p running" (length *hosts*))
   :rows (append
          (when (and session-id (room-host session-id))
            (list (nle:run-row "Set this channel up" "/channels setup"
                               :detail "where it answers and who may use it")))
          (list (nle:ask-row "Let someone in" "Pairing code" "/channels pair"
                             :placeholder "the code the bot gave them" :verb "pair"
                             :detail "a person the bot does not know yet")
                (nle:run-row "Who is paired" "/channels paired" :detail "and how many codes wait"))
          (loop for host in *hosts*
                append (loop for person in (paired host)
                             for id = (nlk:json-value person :string "id")
                             collect (nle:run-row (format nil "Take back ~a" (or (nlk:json-value person :string "name") id))
                                                  (format nil "/channels unpair ~a" id)
                                                  :detail (format nil "~a, id ~a" (host-id host) id)))))))
(defun sethome-panel (session-id)
  "The dialog bare /sethome answers in the room SESSION-ID: make it the home
channel, or have none -- the one that stands marked."
  (multiple-value-bind (host target) (room-host session-id)
    (let* ((home (home-target host))
           (here (and home (equal (getf home :channel-id) (getf target :channel-id))
                      (equal (getf home :thread-id) (getf target :thread-id)))))
      (nle:list-dialog
       :title "Home channel"
       :summary (if home
                    (format nil "Home is ~:[another channel~;here~]" here)
                    "No home channel")
       :rows (list (nle:run-row "Make this the home channel" "/sethome here"
                                :detail "restart notices, new releases, pairing requests and refused commands come here"
                                :mark (if here "●" "") :mark-tone :accent)
                   (nle:run-row "No home channel" "/sethome off"
                                :detail "the bot reports to the operator's shells alone"
                                :mark (if home "" "●") :mark-tone :accent))))))
(defun sethome-entry (args session-id)
  "/sethome typed alone in a channel's room answers its panel; from a shell,
where each home is; a row's line sets it (SETHOME-COMMAND)."
  (if (and (zerop (length (nlk:trimmed (or args "")))) session-id (room-host session-id))
      (let ((panel (sethome-panel session-id)))
        (values (getf panel :summary) panel))
      (sethome-command args session-id)))
(defun agent-panel (room)
  "The dialog bare /agent answers in ROOM: a row per agent its channel names,
beside its folder, then the routes -- each hands the room over."
  (nle:list-dialog
   :title "Agent" :summary "Which agent answers in this room"
   :rows (loop for choice in (agent-choices "" room)
               collect (nle:run-row (getf choice :name) (format nil "/agent ~a" (getf choice :value))))))
(defun agent-entry (args room)
  "/agent typed alone in ROOM: who answers there and why, and its panel; a
row's line hands the room over (AGENT-COMMAND)."
  (if (and (zerop (length (nlk:trimmed (or args "")))) room (room-host room))
      (values (agent-command args room) (agent-panel room))
      (agent-command args room)))