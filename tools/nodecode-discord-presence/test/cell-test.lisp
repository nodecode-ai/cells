;;;; cell-test.lisp --- the discord-presence cell against a stubbed Discord client.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No Discord client is dialled: *OPEN-TRANSPORT* is a pipe of the test's
;;;; own, which keeps every octet the cell writes, so the handshake, the
;;;; SET_ACTIVITY frame and the clear are read back as the client would read
;;;; them. Each tool is called the way a model's call reaches it — through the
;;;; real dispatch, contract and all.

(in-package #:nodecode.test)

(define-test-slice "discord-presence" "DISCORD-PRESENCE-CELL-"
  :start nodecode-discord-presence:start-cell
  :defaults (("client_id" "123456789012345678")))

;;; --- the stubbed client -------------------------------------------------------------

(defvar *sent* nil
  "Every octet written to the stubbed Discord client, newest first, and the
:closed marker a close leaves.")

(defvar *client* t
  "Whether the stubbed Discord client is listening at all.")

(defun fake-open-transport (name)
  "The test's pipe: it keeps what it is given and answers nothing, which is
all the handshake needs of a client that is listening."
  (when *client*
    (nodecode-discord-presence::make-transport
     :label name
     :send (lambda (octets) (push (copy-seq octets) *sent*))
     :recv (lambda (count timeout) (declare (ignore count timeout)) nil)
     :close (lambda () (push :closed *sent*)))))

(defmacro with-discord-client (&body body)
  "BODY with a Discord client that answers, its frames kept in *SENT*.
SETF and not LET: the writer is the cell's own worker thread, which sees no
binding this thread made."
  `(with-saved-globals ((*sent* '())
                        (*client* t)
                        (nodecode-discord-presence::*open-transport* #'fake-open-transport)
                        (nodecode-discord-presence::*worker* nil)
                        (nodecode-discord-presence::*transport* nil)
                        (nodecode-discord-presence::*last-key* nil)
                        (nodecode-discord-presence::*last-error* nil)
                        (nodecode-discord-presence::*paused* nil)
                        (nodecode-discord-presence::*override* nil)
                        (nodecode-discord-presence::*activity* nil)
                        (nodecode-discord-presence::*activity-until* 0)
                        (nodecode-discord-presence::*last-tokens* 0))
     ,@body))

(defmacro with-no-client (&body body)
  "BODY with no Discord client listening: every socket refuses."
  `(with-discord-client
     (with-saved-globals ((*client* nil))
       ,@body)))

(defun sent-frames ()
  "The frames written, oldest first, as (:opcode N :text JSON)."
  (loop for octets in (reverse *sent*)
        when (vectorp octets)
          collect (let* ((opcode (nodecode-discord-presence::octets-u32le octets 0))
                         (length (nodecode-discord-presence::octets-u32le octets 4)))
                    (list :opcode opcode
                          :text (if (zerop length)
                                    ""
                                    (sb-ext:octets-to-string
                                     (subseq octets 8 (+ 8 length))
                                     :external-format :utf-8))))))

(defun frame-json (frame)
  "FRAME's body as the JSON object Discord reads."
  (nlk:decode-json (getf frame :text)))

(defun card-frames ()
  "The SET_ACTIVITY frames, oldest first."
  (remove-if-not (lambda (frame) (= 1 (getf frame :opcode))) (sent-frames)))

(defun wait-for-card (&key (timeout 5))
  "The first card the cell sends, waiting at most TIMEOUT seconds for the
worker that sends it."
  (let ((deadline (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second))))
    (loop for frame = (first (card-frames))
          when frame return frame
          when (> (get-internal-real-time) deadline) return nil
          do (sleep 0.02))))

(defun last-card ()
  (car (last (card-frames))))

(defun tool-answer (tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it."
  (princ-to-string (nle::message-content
                    (execute-wire-call (format nil "call-~36r" (random (expt 2 32)))
                                        tool arguments))))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "discord-presence"
  (:fixture with-discord-client)
  (:config "session_title_mode" "generic")
  (:hooks :tool)
  (:help :discord-presence)
  (:command "presence")
  (:running (is (not (null nodecode-discord-presence::*worker*)) "the tick thread runs")
            (is (wait-for-card) "and a card went out"))
  (:stopped (is (null nodecode-discord-presence::*worker*) "the tick thread is gone")
            (is (null nodecode-discord-presence::*transport*) "and the pipe is closed"))
  (:refused ("client_id" "") ("client_id" 5)))

(deftest discord-presence-cell-without-an-application-id-refuses ()
  ;; The one setting no card can be shown under: a Discord application id of
  ;; the operator's own. A section naming none is refused at start, and
  ;; nothing installs.
  (let ((condition (signals-error nlk:config-refusal (discord-presence-start "client_id" ""))))
    (is (search "no client_id" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "presence_show")) "and installs nothing")))

;;; --- the card ---------------------------------------------------------------------

(deftest discord-presence-cell-publishes-the-session-card (with-discord-client)
  (with-cell-stop ((discord-presence-start))
    (let ((frame (wait-for-card)))
      (is (not (null frame)) "the cell sends a card")
      (let ((json (frame-json frame)))
        (is (equal "SET_ACTIVITY" (nlk:json-value json :string "cmd")) "one SET_ACTIVITY command")
        (is (equal "Active Session"
                   (nlk:json-value json :string "args" "activity" "details")) "generic mode says Active Session, not the folder's name")
        (is (search "Nodecode"
                    (nlk:json-value json :string "args" "activity" "large_text")))
        (is (integerp (nlk:json-value json :integer "args" "pid")) "the pid Discord asks for")))
    (let ((handshake (first (sent-frames))))
      (is (= 0 (getf handshake :opcode)) "the handshake goes first")
      (is (equal "123456789012345678"
                 (nlk:json-value (frame-json handshake) :string "client_id")) "under the operator's own application id")
      (is (= 1 (nlk:json-value (frame-json handshake) :integer "v")) "protocol version 1"))))

(deftest discord-presence-cell-names-the-tool-the-agent-runs (with-discord-client)
  (with-cell-stop ((discord-presence-start))
    (wait-for-card)
    (let ((before (length (card-frames))))
      (nodecode-discord-presence::observe-tool (list :name "read-file")
                                                (lambda (op) (declare (ignore op)) t))
      (nodecode-discord-presence::tick)
      (is (> (length (card-frames)) before) "the new activity is sent")
      (is (search "[Reading File]" (getf (last-card) :text)) "the tool the agent is running stands on the card"))))

(deftest discord-presence-cell-stealth-says-nothing-about-the-work (with-discord-client)
  (with-cell-stop ((discord-presence-start "stealth_mode" t))
    (let ((json (frame-json (wait-for-card))))
      (is (null (nlk:json-value json :string "args" "activity" "details")) "no first line")
      (is (null (nlk:json-value json :string "args" "activity" "state")) "no second line"))))

(deftest discord-presence-cell-full-mode-names-the-workspace (with-discord-client)
  (with-cell-stop ((discord-presence-start "session_title_mode" "full"))
    (let* ((json (frame-json (wait-for-card)))
           (details (nlk:json-value json :string "args" "activity" "details")))
      (is (stringp details) "the workspace folder's name, not Active Session")
      (is (search (nodecode-discord-presence::workspace-name) details)))))

;;; --- the three tools ---------------------------------------------------------------

(deftest discord-presence-cell-show-clears-and-reports (with-discord-client)
  (with-cell-stop ((discord-presence-start))
    (wait-for-card)
    (is (search "Building the release"
                (tool-answer "presence_show"
                             "{\"details\":\"Building the release\",\"state\":\"ci\"}")) "presence_show answers what it put up")
    (let ((json (frame-json (last-card))))
      (is (equal "Building the release"
                 (nlk:json-value json :string "args" "activity" "details")))
      (is (equal "ci" (nlk:json-value json :string "args" "activity" "state"))))
    (is (search "stays down" (tool-answer "presence_clear" "{\"pause\":true}")) "presence_clear takes the card down")
    (is (search "\"activity\":null" (getf (last-card) :text)) "which is an activity of null on the wire")
    (is (not (null nodecode-discord-presence::*paused*)) "and nothing more is published")
    (let ((before (length (card-frames))))
      (nodecode-discord-presence::tick)
      (is (= before (length (card-frames))) "a paused card sends nothing"))
    (is (search "the card is off" (tool-answer "presence_status" "{}")) "the status says the card is off")))

(deftest discord-presence-cell-without-a-discord-client-says-so (with-no-client)
  (with-cell-stop ((discord-presence-start))
    (let ((answer (tool-answer "presence_show" "{\"details\":\"a line\"}")))
      (is (search "no Discord client answered" answer) "the tool's failure names the missing client"))
    (is (search "no Discord client answered" nodecode-discord-presence::*last-error*) "and the tick kept the same reason")))

;;; --- /presence --------------------------------------------------------------------

(deftest discord-presence-cell-the-command-answers-the-status (with-discord-client)
  (with-cell-stop ((discord-presence-start))
    (wait-for-card)
    (multiple-value-bind (text dialog)
        (nodecode-discord-presence::run-slash "" nil)
      (is (search "Active Session" text) "bare /presence reads the card back")
      (is (search "updating every" text))
      (is (not (null dialog)) "with the one row that sets a line"))
    (is (search "stays down"
                (nodecode-discord-presence::run-slash "off" nil)) "/presence off takes it down")
    (is (search "back on" (nodecode-discord-presence::run-slash "on" nil)) "/presence on puts it back")
    (is (search "reviewing the log"
                (nodecode-discord-presence::run-slash "reviewing the log" nil)) "anything else is the line to show")
    (is (equal "reviewing the log"
               (nlk:json-value (frame-json (last-card))
                               :string "args" "activity" "details")))))
