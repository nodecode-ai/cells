;;;; panels.lisp --- what /channels answers typed alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A command is its name: typed alone in a channel's room, /channels answers
;;;; a panel (NLE:LIST-DIALOG) whose rows run its lines or ask for the one
;;;; text a line takes, so nobody spells a verb after the name. The lines are
;;;; the command's own words (status.lisp CHANNELS-COMMAND); this file only
;;;; offers them, after operator.lisp has defined the hosts and rooms the
;;;; panel reads. /sethome and /agent answer theirs where they are
;;;; (operator.lisp SETHOME-COMMAND, AGENT-DIALOG).

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
