;;;; board-test.lisp --- the thread board's pure halves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What decides a thread's section and when it closes, and the message the
;;;; board says them in, held inside Discord's caps on one message.

(in-package #:nodecode.test)

(deftest channel-discord-board-sorts-a-thread-by-who-moves-next ()
  ;; A turn running or waiting there decides first; else the newest words:
  ;; the bot's answer marked waiting (NCK:ANSWER-WAITING) needs you, the
  ;; bot's otherwise are done -- a question mark marks nothing -- anyone
  ;; else's, or none, wait for an answer.
  (is (eq :running (ncd:board-state :phase :running :bot-p t :waits "Which one?")))
  (is (eq :queued (ncd:board-state :phase :queued)))
  (is (eq :needs (ncd:board-state :bot-p t :waits "Push the pin?")))
  (is (eq :needs (ncd:board-state :bot-p t :waits "")) "marked, on the answer's own last line")
  (is (eq :done (ncd:board-state :bot-p t)) "an unmarked answer waits on nobody, whatever it ends on")
  (is (eq :noans (ncd:board-state :bot-p nil :waits "Push?")) "someone else spoke last")
  (is (eq :noans (ncd:board-state)) "nobody said anything"))

(deftest channel-discord-board-closes-what-went-quiet ()
  ;; Never while a turn runs or waits; a done thread after 48 hours quiet;
  ;; any other after 7 days.
  (let ((hour (* 60 60 1000)) (day (* 24 60 60 1000)))
    (is (not (ncd:board-close-p :running (* 30 day))))
    (is (not (ncd:board-close-p :queued (* 30 day))))
    (is (not (ncd:board-close-p :done (* 47 hour))))
    (is (ncd:board-close-p :done (* 48 hour)))
    (is (not (ncd:board-close-p :needs (* 6 day))))
    (is (ncd:board-close-p :needs (* 7 day)))
    (is (ncd:board-close-p :noans (* 7 day)))))

(defun board-test-parts (components)
  "Every component inside COMPONENTS, however nested, in order."
  (loop for part across components
        collect part
        append (let ((inner (nlk:json-value part :array "components")))
                 (and inner (board-test-parts inner)))
        append (let ((accessory (nlk:json-value part :object "accessory")))
                 (and accessory (list accessory)))))

(deftest channel-discord-board-message-holds-inside-discords-caps ()
  ;; Sections in the board's order, each its accent, its newest five threads
  ;; and the rest counted; Needs you's threads carry their question; Show all
  ;; and, while any is done, Close all done; and the whole inside Discord's
  ;; caps of 40 components and 4000 characters.
  (let* ((rows (append (loop for i below 9
                             collect (list :id (format nil "10~d" i) :state :needs :quiet-ms (* (1+ i) 3600000)
                                           :question "Should I push it?"))
                       (list (list :id "200" :state :running :quiet-ms 60000)
                             (list :id "201" :state :queued :quiet-ms 1000)
                             (list :id "300" :state :done :quiet-ms 7200000))))
         (components (ncd:board-components rows :closing 3))
         (parts (board-test-parts components))
         (containers (remove-if-not (lambda (part) (eql 17 (nlk:json-value part :integer "type"))) parts))
         (texts (loop for part in parts
                      when (eql 10 (nlk:json-value part :integer "type"))
                        collect (nlk:json-value part :string "content")))
         (buttons (remove-if-not (lambda (part) (eql 2 (nlk:json-value part :integer "type"))) parts)))
    (is (equal '(#xFA8B3E #x23A55A #x80848E)
               (mapcar (lambda (part) (nlk:json-value part :integer "accent_color")) containers)) "Needs you, Running, Done: a section with no thread is left out")
    (let ((needs (find "**Needs you**" texts :test (lambda (needle text) (search needle text)))))
      (is (search "**Needs you** · 9" needs))
      (is (= 5 (count-matches "<#10" needs)) "the newest five")
      (is (search "<#100> · 1h" needs) "newest first, with its age")
      (is (search "-# Should I push it?" needs) "each with the question it ends on")
      (is (search "+4 more" needs)))
    (is (find-if (lambda (text) (search "<#201> · waiting" text)) texts) "a waiting turn sits under Running")
    (is (find-if (lambda (text) (search "12 open · 3 more closing" text)) texts))
    (is (equal '("Show all" "Close all done (1)")
               (mapcar (lambda (part) (nlk:json-value part :string "label")) buttons)))
    (is (<= (length parts) 40) "inside Discord's 40 components")
    (is (<= (reduce #'+ texts :key #'length) 4000) "and its 4000 characters")
    (is (equal '("Show all")
               (mapcar (lambda (part) (nlk:json-value part :string "label"))
                       (remove-if-not (lambda (part) (eql 2 (nlk:json-value part :integer "type")))
                                      (board-test-parts (ncd:board-components (butlast rows)))))) "no Close all done while nothing is done")))

(deftest channel-discord-board-list-answers-every-thread ()
  ;; Show all and /threads list every open thread by section, cut to one
  ;; message with what was left out counted.
  (let ((rows (list (list :id "1" :state :needs :quiet-ms 3600000 :question "Push it?")
                    (list :id "2" :state :done :quiet-ms 60000))))
    (is (equal (format nil "**Needs you** · 1~%<#1> · 1h — Push it?~%**Done** · 1~%<#2> · 1m")
               (ncd:board-list-text rows)))
    (is (equal "No thread is open." (ncd:board-list-text '())))
    (let ((cut (ncd:board-list-text (loop for i below 200 collect (list :id (format nil "~d" (+ 1000 i)) :state :done :quiet-ms 60000)))))
      (is (< (length cut) 2000))
      (is (search "more lines" cut)))))

(deftest channel-discord-thread-close-plan-archives ()
  (is-plan (ncd:thread-close-plan "42") :method "PATCH" :path "/channels/42" :label "close_thread"
           (:boolean "archived") t))

(defun count-matches (needle text &aux (count 0) (start 0))
  "How many times NEEDLE occurs in TEXT."
  (loop for at = (search needle text :start2 start)
        while at do (incf count) (setf start (1+ at)))
  count)
