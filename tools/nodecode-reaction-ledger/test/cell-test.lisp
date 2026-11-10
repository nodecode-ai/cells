;;;; cell-test.lisp --- the ledger, the observer, the reports, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network or a chat room: a candidate is built the
;;;; way a room adapter builds it and handed to the observer with a stubbed
;;;; NEXT, the ledger is a file in the scratch home, and the clock is the one
;;;; thing the tests move (a stubbed NOW the rows carry).

(in-package #:nodecode.test)

(define-test-slice "reaction-ledger" "REACTION-LEDGER-CELL-"
  :start nodecode-reaction-ledger:start-cell)

(define-cell-lifecycle-tests "reaction-ledger"
  (:help :reaction-ledger)
  (:command "reaction-log")
  (:running (is (= 5000 (nodecode-reaction-ledger::setting :rows)) "rows defaults to five thousand"))
  (:refused ("rows" 0) ("rows" 4) ("rows" "many"))
  (:idle nodecode-reaction-ledger:reaction-ledger-error
         (reaction-ledger:summary)
         (reaction-ledger:recent)
         (reaction-ledger:clear)))

;;; --- the fixture ---------------------------------------------------------------------

(defun rl-ledger-file ()
  "A ledger file of this test's own, in the scratch home, empty."
  (let ((path (nlk:home "reaction-ledger-test/run.jsonl")))
    (ensure-directories-exist path)
    (when (probe-file path) (delete-file path))
    path))

(defmacro with-ledger ((&key (now 1700000000)) &body body)
  "BODY with the cell started, its ledger a fresh file, and the clock at NOW,
  a unix second, for every row written."
  (let ((path (gensym "PATH")))
    `(let ((,path (rl-ledger-file)))
       (with-stubbed-fdefinitions
           ((nodecode-reaction-ledger::ledger-path () ,path)
            (nodecode-reaction-ledger::now () ,now))
         (with-cell-stop ((reaction-ledger-start))
           (unwind-protect (progn ,@body)
             (when (probe-file ,path) (delete-file ,path))))))))

(defun rl-candidate (&key (room "telegram") (chat "chat-1") (message "message-1")
                        (emoji "👍") (action "add"))
  "A reaction candidate as a room adapter hands one to the kit: the emoji is
its text, and the chat, message and add-or-take-back are the source's."
  (nlk:json-object
   "text" emoji
   "source" (nlk:json-object "platform" room "chat_kind" "group"
                             "channel_id" chat "user_id" "user-one"
                             "user_name" "someone"
                             "reply_to_message_id" message
                             "reaction" action "addressed" t)))

(defun rl-answer (&optional (answer :observe))
  "A stubbed NEXT for a function hook: it records nothing and answers ANSWER."
  (lambda (&rest arguments) (declare (ignore arguments)) answer))

(defun rl-react (&rest keys)
  "One candidate through the observer's advice."
  (funcall #'nodecode-reaction-ledger::observe-reaction (rl-answer) nil
           (apply #'rl-candidate keys)))

(defun rl-rows ()
  "The ledger's rows, oldest first."
  (nodecode-reaction-ledger::rows))

(defun rl-handler ()
  "/reaction-log's RUN, as the catalog's own headless arm answers it."
  (nodecode.evolved::slash-command-headless (nle::find-registered-command "reaction-log")))

;;; --- one row a reaction ---------------------------------------------------------------

(deftest reaction-ledger-cell-every-reaction-becomes-one-row ()
  (with-ledger ()
    (is (eq :observe (rl-react)) "the advice answers what the kit answered")
    (rl-react :emoji "❤")
    (rl-react :message "message-2" :emoji "👍")
    (let ((rows (rl-rows)))
      (is (= 3 (length rows)) "three reactions, three rows")
      (let ((emojis (mapcar #'nodecode-reaction-ledger::row-emoji rows))
            (rooms (mapcar #'nodecode-reaction-ledger::row-room rows)))
        (is (equal '("👍" "❤" "👍") emojis) "each row keeps the emoji it was given")
        (is (equal '("telegram" "telegram" "telegram") rooms) "and the room it happened in"))
      (let ((message (nodecode-reaction-ledger::row-message (first rows))))
        (is (= 12 (length message)) "twelve hex digits stand in for the message")
        (is (every (lambda (char) (digit-char-p char 16)) message) "and they are hex"))
      (let ((file (nlk:read-text (nodecode-reaction-ledger::ledger-path))))
        (is (not (search "chat-1" file)) "the chat id itself is never written")
        (is (not (search "message-1" file)) "nor the message id")
        (is (not (search "user-one" file)) "nor who reacted")))))

(deftest reaction-ledger-cell-a-taken-back-reaction-is-its-own-row ()
  (with-ledger ()
    (rl-react)
    (rl-react :action "remove")
    (let ((rows (rl-rows)))
      (is (= 2 (length rows)) "setting and taking back are two rows")
      (is (equal '("add" "remove") (mapcar #'nodecode-reaction-ledger::row-action rows))))
    (is-carrying (text (reaction-ledger:summary))
      ("none recorded" "the message carries nothing now")
      ("removed from 1 message" "and the summary says so"))))

(deftest reaction-ledger-cell-a-broken-write-never-breaks-the-room ()
  (with-ledger ()
    (with-stubbed-fdefinitions
        ((nodecode-reaction-ledger::append-change (&rest arguments)
           (error "the disk is full after ~d argument(s)" (length arguments))))
      (is (eq :observe (rl-react)) "the kit's own answer stands")
      (is (null (rl-rows)) "and nothing reached the ledger"))))

;;; --- the reports ---------------------------------------------------------------------

(deftest reaction-ledger-cell-summary-counts-messages-and-chats ()
  (with-ledger ()
    (rl-react :message "message-1" :emoji "👍")
    (rl-react :message "message-2" :emoji "👍" :chat "chat-2")
    (rl-react :message "message-3" :emoji "❤")
    (rl-react :message "message-2" :emoji "❤")
    (is-carrying (text (reaction-ledger:summary))
      ("Reactions, last 24 hours:") ("emoji") ("messages") ("chats") ("last")
      ("Each message counts once" "the note under the table")
      (:absent "🔥" "an emoji nobody used is not listed"))
    (let* ((text (reaction-ledger:summary))
           (line (find-if (lambda (row) (search "👍" row)) (nlk:lines text))))
      (is (search "2" line) "two messages carry the thumbs up"))
    (is (search "day" (reaction-ledger:summary :by "day")) "a day grouping names that column")
    (is-carrying (text (reaction-ledger:summary :window "all"))
      ("all recorded" "the whole ledger"))))

(deftest reaction-ledger-cell-the-command-line-choices ()
  (with-ledger ()
    (rl-react :message "message-1" :emoji "👍")
    (rl-react :message "message-2" :emoji "❤")
    (let ((handler (rl-handler)))
      (is-carrying (text (funcall handler "7d" "s1"))
        ("Reactions, last 7 days:") ("👍" "an emoji that was used"))
      (is-carrying (text (funcall handler "days" "s1")) ("day") ("messages"))
      (is-carrying (text (funcall handler "recent" "s1"))
        ("newest 2 of 2") ("👍 added" "the change as it happened") ("❤ added"))
      (is-carrying (text (funcall handler "recent 1" "s1")) ("newest 1 of 2") ("when"))
      (is-carrying (text (funcall handler "nonsense" "s1")) ("Unknown option: nonsense"))
      (is-carrying (text (funcall handler "all 12h" "s1")) ("all recorded"))
      (is-carrying (text (funcall handler "clear" "s1"))
        ("deleted 2 recorded reaction change(s)"))
      (is (null (rl-rows)) "and the ledger is gone"))))

(deftest reaction-ledger-cell-a-window-leaves-older-rows-behind ()
  (with-ledger ()
    (rl-react :message "message-1" :emoji "👍")
    (with-stubbed-fdefinitions ((nodecode-reaction-ledger::now () (- 1700000000 (* 3 86400))))
      (rl-react :message "message-old" :emoji "🔥"))
    (is (not (search "🔥" (reaction-ledger:summary))) "three days ago is outside a day")
    (is (search "🔥" (reaction-ledger:summary :window "all")) "and inside everything")
    (is (search "last 4 days" (funcall (rl-handler) "4d" "s1")) "a window word titles the report")
    (is (search "last 24 hours" (funcall (rl-handler) "" "s1")))))

;;; --- the ledger keeps its shape -------------------------------------------------------

(deftest reaction-ledger-cell-a-full-ledger-keeps-the-newest-rows ()
  (with-ledger ()
    (dotimes (n 8)
      (with-stubbed-fdefinitions ((nodecode-reaction-ledger::now () (+ 1700000000 n)))
        (rl-react :message (format nil "message-~d" n))))
    (nodecode-reaction-ledger::trim (nodecode-reaction-ledger::ledger-path) 5)
    (let ((rows (rl-rows)))
      (is (= 5 (length rows)) "the file holds the bound")
      (let ((third (nodecode-reaction-ledger::tag "telegram" "chat-1" "message-3"))
            (seventh (nodecode-reaction-ledger::tag "telegram" "chat-1" "message-7")))
        (let ((first-message (nodecode-reaction-ledger::row-message (first rows)))
              (last-message (nodecode-reaction-ledger::row-message (car (last rows)))))
          (is (string= third first-message) "the oldest rows went")
          (is (string= seventh last-message) "and the newest of them stayed"))))))
