;;;; commands-test.lisp --- the command menu as MSC4391 state, end to end.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What one command's event says and where it is kept (checked against the
;;;; state key mautrix-go computes, spelled here from Python's hashlib), then
;;;; the lane over the fake homeserver: every room it is in holds the whole
;;;; menu once and is not written again, a room that will not let the bot set
;;;; state is left alone until its levels say otherwise, and a command the
;;;; catalog no longer has is emptied.

(in-package #:nodecode.test)

(defparameter +matrix-menu-type+ "org.matrix.msc4391.command_description")

(defun matrix-menu-size ()
  "How many commands the kit's menu holds: one event each."
  (length (nck::menu-entries (nle:slash-catalog))))

(defun matrix-text-body (container)
  "The words of an extensible text CONTAINER."
  (nlk:json-value (aref (gethash "m.text" container) 0) :string "body"))

(defun matrix-room-menu-puts (fake)
  "The state PUTs into !room:fake so far."
  (matrix-state-puts fake :room "!room:fake"))

(deftest channel-matrix-command-event ()
  (let ((key (ncm:command-state-key "models" "@bot:fake")))
    (is (equal "6LU5sYBdyib4jLFHi/8H4Lw8UCf5DnDXgCzV6hQKCpw=" key) "the state key is mautrix-go's"))
  (let* ((models (ncm:command-content (list :name "models" :description "Show or change the models"
                                            :usage "/models [provider] [model]" :autocomplete t)))
         (help (ncm:command-content (list :name "help" :description "Show commands" :usage "")))
         (parameter (aref (gethash "parameters" models) 0)))
    (is (equal "models" (gethash "command" models)) "the command is named without its slash")
    (is (equal "Show or change the models" (matrix-text-body (gethash "description" models))) "described in words")
    (is (equal "args" (gethash "key" parameter)) "a command that takes arguments has one parameter")
    (is (eq t (gethash "optional" parameter)) "which may be left out")
    (is (equal "string" (nlk:json-value parameter :string "schema" "type")) "and is free text")
    (is (equal "[provider] [model]" (matrix-text-body (gethash "description" parameter))) "its usage, after the command")
    (is (equal "args" (gethash "fi.mau.tail_parameter" models)) "which takes the rest of the line")
    (is (null (nth-value 1 (gethash "parameters" help))) "a command that takes nothing declares no parameter")
    (is (null (nth-value 1 (gethash "fi.mau.tail_parameter" help))) "and no tail")))

(deftest channel-matrix-menu-published-per-room ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!room:fake" :members '("@kim:fake"))
    (with-matrix-lane (fake "allowed_users" (vector "@kim:fake") "owner" (vector "@kim:fake"))
      (let ((size (matrix-menu-size)))
        (is (await (:timeout 15) (= size (length (matrix-room-menu-puts fake)))) "every command is set in the room")
        (let* ((puts (matrix-room-menu-puts fake))
               (models (find "models" puts :key (lambda (put) (gethash "command" (fourth put))) :test #'equal)))
          (is (every (lambda (put) (equal +matrix-menu-type+ (second put))) puts) "each as a command description")
          (is (equal (ncm:command-state-key "models" +fake-bot+) (third models)) "under its own state key"))
        ;; The echoes land in the book; a lap or two later nothing is set again.
        (sleep 1.5)
        (is (= size (length (matrix-room-menu-puts fake))) "a room holding the menu is not written again")))))

(deftest channel-matrix-menu-waits-for-power ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!room:fake" :members '("@kim:fake"))
    (let ((users (nlk:json-object "@kim:fake" 100)))
      (fake-matrix-state fake "!room:fake" "m.room.power_levels" ""
                         (nlk:json-object "users" users "state_default" 50))
      (with-matrix-lane (fake "allowed_users" (vector "@kim:fake") "owner" (vector "@kim:fake"))
        (sleep 1.5)
        (is (null (matrix-room-menu-puts fake)) "a room where the bot may not set state is not written")
        (fake-matrix-push fake "!room:fake"
                          (fake-matrix-state fake "!room:fake" "m.room.power_levels" ""
                                             (nlk:json-object "users" users "state_default" 50
                                                              "events" (nlk:json-object "org.matrix.msc4391.command_description" 0))))
        (is (await (:timeout 15) (= (matrix-menu-size) (length (matrix-room-menu-puts fake)))) "once it may, it is")))))

(deftest channel-matrix-menu-stale-command-emptied ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!room:fake" :members '("@kim:fake"))
    (let ((stale (ncm:command-state-key "retired" +fake-bot+)))
      (fake-matrix-state fake "!room:fake" +matrix-menu-type+ stale (nlk:json-object "command" "retired")
                         :sender +fake-bot+)
      (with-matrix-lane (fake "allowed_users" (vector "@kim:fake") "owner" (vector "@kim:fake"))
        (let ((put (await (:timeout 15) (find stale (matrix-room-menu-puts fake) :key #'third :test #'equal))))
          (is put "a command of the bot's the catalog no longer names is set again")
          (is (and put (zerop (hash-table-count (fourth put)))) "as empty, which takes it off the menu"))))))
