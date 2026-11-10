;;;; cell-test.lisp --- lifecycle, the spool, the turn, the cap, /fivedive.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: a healthy start installs the tool, the command and
;;;; the manual topic and a stop takes them back; a refused section names its
;;;; member; a message the bridge left in the inbox becomes one turn in the
;;;; chat's own session, and the answer lands in the outbox the way the bridge
;;;; reads it; an inbox file that is not a message is dropped, and a message is
;;;; taken once; a voice note makes the next reply spoken; an answer over the
;;;; word cap is held and the agent is asked for a shorter one, twice, then it
;;;; goes as written; /fivedive reports and sends; and the tool sends with
;;;; files. Every turn is a stubbed ingress and every folder a scratch spool,
;;;; so no test starts an agent and no test dials anything.

(in-package #:nodecode.test)

;;; --- a scratch spool ------------------------------------------------------------

(defun scratch-spool ()
  "A fresh spool folder under the temporary directory."
  (namestring
   (uiop:ensure-directory-pathname
    (merge-pathnames (format nil "fivedive-test-~8,'0x/" (random #xffffffff))
                     (uiop:temporary-directory)))))

(defmacro with-spool ((variable) &body body)
  "BODY with VARIABLE bound to a fresh spool folder, removed on the way out."
  `(let ((,variable (scratch-spool)))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,variable)
                                                  :validate t)))))

(defun inbox-file (spool name)
  "The path of NAME.json in SPOOL's inbox, its folder made."
  (let ((file (merge-pathnames (format nil "inbox/~a.json" name)
                               (uiop:ensure-directory-pathname spool))))
    (ensure-directories-exist file)
    file))

(defun drop-message (spool name object)
  "Write OBJECT as NAME.json into SPOOL's inbox, the way the bridge does."
  (let ((file (inbox-file spool name)))
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object object) out))
    file))

(defun drop-raw (spool name text)
  "Write TEXT as NAME.json into SPOOL's inbox, whatever TEXT is."
  (let ((file (inbox-file spool name)))
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (write-string text out))
    file))

(defun inbox-names (spool)
  (let ((dir (merge-pathnames "inbox/" (uiop:ensure-directory-pathname spool))))
    (and (uiop:directory-exists-p dir)
         (mapcar #'file-namestring (uiop:directory-files dir)))))

(defun outbox-replies (spool &optional (chat "dashboard"))
  "Every reply the cell left in SPOOL's outbox, oldest first, decoded."
  (let ((dir (merge-pathnames (format nil "outbox/~a/" chat)
                              (uiop:ensure-directory-pathname spool))))
    (when (uiop:directory-exists-p dir)
      (mapcar (lambda (file) (nlk:decode-json (uiop:read-file-string file)))
              (sort (uiop:directory-files dir) #'string< :key #'file-namestring)))))

(defun fivedive-message (&key (text "hello") (chat "dashboard") voice upload (sender nil))
  "One inbox file's body, as the bridge writes it."
  (apply #'nlk:make-json-object
         "id" "m1" "text" text "received_at" "2026-10-30T23:00:00Z"
         (append (when upload (list "image_path" upload))
                 (when sender (list "sender" sender))
                 (list "route" (apply #'nlk:make-json-object
                                      "source" "dashboard" "chat_id" chat
                                      (when voice (list "voice" t)))))))

;;; --- the ingress, stubbed --------------------------------------------------------

(defvar *submits* '()
  "Every prompt the cell submitted, (SESSION PROMPT COMMAND-ID SOURCE-ID), newest first.")

(defmacro with-submits (&body body)
  "BODY with the cell's ingress recording instead of starting a turn."
  `(let ((*submits* '()))
     (with-stubbed-fdefinition (nodecode-fivedive::submit-prompt (session prompt command-id source-id)
                                (push (list session prompt command-id source-id) *submits*)
                                :started)
       ,@body)))

(defun asked (index)
  "The INDEXth prompt the cell submitted, oldest first."
  (nth index (reverse *submits*)))

(defmacro with-answer ((text) &body body)
  "BODY with the chat's turn finished and its answer being TEXT."
  `(with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) nil)
                               (nlk:turn-answer (session) (declare (ignore session)) ,text))
     ,@body))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "fivedive" "FIVEDIVE-CELL-")

(defun fivedive-config (&rest pairs)
  "A whole shared config with a channels.fivedive section built from PAIRS."
  (nlk:json-object "channels" (nlk:json-object "fivedive" (apply #'nlk:make-json-object pairs))))

(defun fivedive-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-fivedive:start-cell (apply #'fivedive-config pairs)))

(define-cell-lifecycle-tests "fivedive"
  (:config "spool_dir" (scratch-spool) "poll_seconds" 3600)
  (:help :fivedive)
  (:command "fivedive")
  (:running (is (nle::find-tool "fivedive_send") "fivedive_send is registered")
            (is (= 70 (nodecode-fivedive::setting :reply-max-words)))
            (is (nodecode-fivedive::spool) "the spool is the section's")
            (is nodecode-fivedive::*worker* "the spool worker runs"))
  (:stopped (is (null (nle::find-tool "fivedive_send")) "stop unregisters the tool")
            (is (null nodecode-fivedive::*worker*) "and the worker is gone"))
  (:refused ("spool_dir" (scratch-spool) "poll_seconds" 0)
            ("spool_dir" (scratch-spool) "max_length" 5)
            ("spool_dir" (scratch-spool) "reply_max_words" -1))
  (:idle nodecode-fivedive:fivedive-error (nodecode-fivedive::run-command "" nil)))

(deftest fivedive-cell-a-start-without-a-spool-refuses-by-name ()
  (let ((text (handler-case (progn (fivedive-start "poll_seconds" 3600) "")
                (error (condition) (princ-to-string condition)))))
    (is-carrying (text text) "spool_dir" "channels.fivedive"))
  (is (null (nle::find-tool "fivedive_send")) "a refused start installed nothing"))

;;; --- the spool ----------------------------------------------------------------

(deftest fivedive-cell-a-message-becomes-a-turn-and-its-answer-lands-in-the-outbox ()
  (with-spool (spool)
    (with-submits
      (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600))
        (drop-message spool "0001" (fivedive-message :text "what is on my plate?"))
        (is (= 1 (length (inbox-names spool))) "the bridge left one message")
        (dolist (message (nodecode-fivedive:take-inbox)) (nodecode-fivedive::handle-message message))
        (is (null (inbox-names spool)) "the message is taken, not read twice")
        (is (= 1 (length *submits*)) "one message, one turn")
        (is (equal "fivedive-dashboard" (first (asked 0))) "the chat's own session")
        (is-carrying (prompt (second (asked 0))) "what is on my plate?" "5dive app chat")
        (is (null (outbox-replies spool)) "nothing is written before the turn answers")
        (with-answer ("Pizza, and the report.") (nodecode-fivedive::deliver-pending))
        (let ((reply (first (outbox-replies spool))))
          (is (hash-table-p reply) "one reply in the outbox")
          (is (equal "dashboard" (nlk:json-value reply :string "source")))
          (is (equal "dashboard" (nlk:json-value reply :string "chat_id")))
          (is (equal "Pizza, and the report." (nlk:json-value reply :string "text")))
          (is (null (nlk:json-value reply :boolean "voice")) "a typed message is not spoken"))
        (is (null nodecode-fivedive::*owed*) "the chat is not owed twice")))))

(deftest fivedive-cell-an-unreadable-and-an-incomplete-file-are-dropped ()
  (with-spool (spool)
    (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600))
      (drop-raw spool "0001" "{not json")
      (drop-message spool "0002" (nlk:json-object "text" "no route here"))
      (drop-message spool "0003" (fivedive-message :text "this one counts"))
      (let ((taken (nodecode-fivedive:take-inbox)))
        (is (= 1 (length taken)) "only the complete message is handed on")
        (is (equal "this one counts" (nodecode-fivedive::message-text (first taken)))))
      (is (null (inbox-names spool)) "and all three files are gone"))))

(deftest fivedive-cell-a-voice-note-is-answered-by-voice ()
  (with-spool (spool)
    (with-submits
      (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600))
        (drop-message spool "0001" (fivedive-message :text "remind me" :voice t))
        (dolist (message (nodecode-fivedive:take-inbox)) (nodecode-fivedive::handle-message message))
        (is-carrying (prompt (second (asked 0))) "[voice note]" "remind me")
        (with-answer ("At six.") (nodecode-fivedive::deliver-pending))
        (is (nlk:json-value (first (outbox-replies spool)) :boolean "voice") "the reply after a voice note is spoken")))))

(deftest fivedive-cell-a-stranger-is-ignored-and-an-upload-is-named ()
  (with-spool (spool)
    (with-submits
      (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600
                                       "allowed_senders" (vector "5dive-app")))
        (drop-message spool "0001" (fivedive-message :text "let me in" :sender "someone-else"))
        (drop-message spool "0002" (fivedive-message :text "" :upload "/tmp/a/photo.png"))
        (dolist (message (nodecode-fivedive:take-inbox)) (nodecode-fivedive::handle-message message))
        (is (= 1 (length *submits*)) "only the admitted message becomes a turn")
        (is (search "/tmp/a/photo.png" (second (asked 0))) "the upload is named in the prompt")
        (is (search "A file arrived" (second (asked 0))))))))

;;; --- the reply cap -------------------------------------------------------------

(defun long-answer (&optional (words 90))
  (format nil "~{~a~^ ~}" (loop repeat words collect "word")))

(deftest fivedive-cell-an-over-cap-answer-is-held-and-a-rewrite-asked ()
  (with-spool (spool)
    (with-submits
      (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600))
        (drop-message spool "0001" (fivedive-message :text "tell me everything"))
        (dolist (message (nodecode-fivedive:take-inbox)) (nodecode-fivedive::handle-message message))
        (with-answer ((long-answer 90)) (nodecode-fivedive::deliver-pending))
        (is (null (outbox-replies spool)) "the long answer is not sent")
        (is (= 2 (length *submits*)) "the agent is asked to shorten it")
        (is (search "too long: 90 words" (second (asked 1))) "the refusal says the count")
        (is (equal '("dashboard" "dashboard") (mapcar #'fourth *submits*)) "the rewrite rides the same chat")
        (with-answer ("Pizza.") (nodecode-fivedive::deliver-pending))
        (is (equal '("Pizza.") (mapcar (lambda (reply) (nlk:json-value reply :string "text")) (outbox-replies spool))) "the short version reaches the chat")
        (is (null nodecode-fivedive::*owed*) "and the chat is not owed another")))))

(deftest fivedive-cell-after-two-rewrites-the-long-answer-goes-as-written ()
  (with-spool (spool)
    (with-submits
      (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600))
        (drop-message spool "0001" (fivedive-message :text "tell me everything"))
        (dolist (message (nodecode-fivedive:take-inbox)) (nodecode-fivedive::handle-message message))
        (with-answer ((long-answer 90)) (nodecode-fivedive::deliver-pending))
        (with-answer ((long-answer 85)) (nodecode-fivedive::deliver-pending))
        (is (= 3 (length *submits*)) "two rewrites, no more")
        (with-answer ((long-answer 80)) (nodecode-fivedive::deliver-pending))
        (is (= 1 (length (outbox-replies spool))) "the third long answer goes as written")
        (is (= 80 (nodecode-fivedive:count-reply-words
                   (nlk:json-value (first (outbox-replies spool)) :string "text"))))
        (is (null nodecode-fivedive::*owed*))))))

;;; --- the command, the tool and the count -----------------------------------------

(deftest fivedive-cell-the-command-reports-and-sends ()
  (with-spool (spool)
    (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600))
      (let ((text (nodecode-fivedive::run-command "" nil)))
        (is-carrying (text text) "5dive app chat" "dashboard" "70 words"))
      (let ((answer (nodecode-fivedive::run-command "dinner is ready" nil)))
        (is-carrying answer "Sent"))
      (let ((reply (first (outbox-replies spool))))
        (is (equal "dinner is ready" (nlk:json-value reply :string "text")))))))

(deftest fivedive-cell-the-tool-sends-with-files ()
  (with-spool (spool)
    (with-cell-stop ((fivedive-start "spool_dir" spool "poll_seconds" 3600))
      (let ((answer (nodecode-fivedive::tool-send
                     (nlk:json-object "text" "the report" "files" (vector "/tmp/a/report.pdf")))))
        (is (search "Sent" answer)))
      (let ((reply (first (outbox-replies spool))))
        (is (equal "the report" (nlk:json-value reply :string "text")))
        (is (equal '("/tmp/a/report.pdf")
                   (coerce (nlk:json-value reply :any "files") 'list)))))))

(deftest fivedive-cell-the-word-count-leaves-code-out ()
  (is (= 2 (nodecode-fivedive:count-reply-words "one two")))
  (is (= 1 (nodecode-fivedive:count-reply-words (format nil "one~%```~%(define (f) 3)~%```"))))
  (is (null (nodecode-fivedive:reply-too-long "one two" 5)))
  (is (search "too long" (nodecode-fivedive:reply-too-long (long-answer 9) 5))))
