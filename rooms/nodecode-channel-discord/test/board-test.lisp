;;;; board-test.lisp --- the thread board's pure halves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The message the board says its threads in, held inside Discord's caps on
;;;; one message; what decides a thread's section and when it closes is the
;;;; kit's (its board-test.lisp).

(in-package #:nodecode.test)

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
               (nck:board-list-text rows #'ncd:board-list-line)))
    (is (equal "No thread is open." (nck:board-list-text '() #'ncd:board-list-line)))
    (let ((cut (nck:board-list-text (loop for i below 200 collect (list :id (format nil "~d" (+ 1000 i)) :state :done :quiet-ms 60000))
                                     #'ncd:board-list-line)))
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
