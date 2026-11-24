;;;; cell-test.lisp --- lifecycle, the lap, the commands, the plain text, /zalo.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the section (a healthy start, a member out of its
;;;; floor refused, a start with nobody allowed refused), the lap turning one
;;;; update into one turn and its answer into plain chunked messages, the
;;;; commands the bot answers itself, a stranger getting nothing, the token
;;;; never reaching an error's text, and /zalo. Every request is a stubbed
;;;; NLK:HTTP and every turn a stubbed ingress, so no test dials Zalo and no
;;;; test starts an agent.

(in-package #:nodecode.test)

;;; --- the wire, stubbed ----------------------------------------------------------

(defvar *calls* '()
  "Every stubbed Bot API call, (ENDPOINT BODY), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of ENDPOINT and BODY =>
(values TEXT STATUS).")

(defun endpoint-of (url)
  "URL's last path segment: the Bot API method it names."
  (let ((slash (position #\/ url :from-end t)))
    (subseq url (if slash (1+ slash) 0))))

(defmacro with-zalo-wire (&body body)
  "BODY with the cell's one network call stubbed: each request is logged in
*CALLS* and answered by *ANSWER-FN*, so nothing reaches Zalo."
  `(let ((*calls* '())
         (*answer-fn* (lambda (endpoint body)
                        (declare (ignore endpoint body))
                        (values "{\"ok\":true,\"result\":{}}" 200))))
     (with-stubbed-fdefinition (nodecode-zalo::http (method url &rest args)
                                (declare (ignore method))
                                (let ((endpoint (endpoint-of url))
                                      (body (ignore-errors (nlk:decode-json (getf args :content)))))
                                  (push (list endpoint body) *calls*)
                                  (funcall *answer-fn* endpoint body)))
       ,@body)))

(defun calls-of (endpoint)
  "The stubbed calls to ENDPOINT, oldest first, their bodies decoded."
  (loop for (name body) in (reverse *calls*)
        when (equal name endpoint) collect body))

(defun sent-texts ()
  "The text of every sendMessage this test made, oldest first."
  (loop for body in (calls-of "sendMessage") collect (nlk:json-value body :string "text")))

(defun answering (table)
  "Answer every endpoint as TABLE says -- (ENDPOINT . (TEXT STATUS)) entries
-- and anything else with an empty ok."
  (setf *answer-fn*
        (lambda (endpoint body)
          (declare (ignore body))
          (let ((entry (assoc endpoint table :test #'equal)))
            (if entry
                (values (second entry) (third entry))
                (values "{\"ok\":true,\"result\":{}}" 200))))))

;;; --- the ingress, stubbed --------------------------------------------------------

(defvar *submits* '()
  "Every prompt the cell submitted, (SESSION PROMPT COMMAND-ID SOURCE-ID), newest first.")

(defmacro with-submits (&body body)
  "BODY with the cell's ingress recording instead of starting a turn."
  `(let ((*submits* '()))
     (with-stubbed-fdefinition (nodecode-zalo::submit-prompt (session prompt command-id source-id)
                                (push (list session prompt command-id source-id) *submits*)
                                :started)
       ,@body)))

(defun zalo-update (chat user text)
  "One Bot API update carrying TEXT from USER in CHAT, as Zalo spells it."
  (nlk:json-object "event_name" "message.text.received"
                    "message" (nlk:json-object
                               "from" (nlk:json-object "id" user "display_name" "Lan")
                               "chat" (nlk:json-object "id" chat "chat_type" "group")
                               "text" text)))

(defvar *bot-token-file* nil
  "A file holding a test token, made once per image.")

(defun bot-token-file ()
  (or *bot-token-file*
      (setf *bot-token-file*
            (uiop:with-temporary-file (:stream out :pathname file :keep t :prefix "zalo-token")
              (write-string "1234:test-secret" out)
              (namestring file)))))

(defmacro with-zalo ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS and a token, stopped on the way out."
  `(with-cell-stop ((zalo-start "bot_token" "1234:test-secret" "allowed_users" (vector "u1")
                                 "api_base" "https://zalo.test" ,@pairs))
     ,@body))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "zalo" "ZALO-CELL-")

(defun zalo-config (&rest pairs)
  "A whole shared config with a channels.zalo section built from PAIRS."
  (nlk:json-object "channels" (nlk:json-object "zalo" (apply #'nlk:make-json-object pairs))))

(defun zalo-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-zalo:start-cell (apply #'zalo-config pairs)))

(define-cell-lifecycle-tests "zalo"
  (:fixture with-zalo-wire)
  (:config "bot_token" "1234:test-secret" "allowed_users" (vector "u1")
           "api_base" "https://zalo.test")
  (:help :zalo)
  (:command "zalo")
  (:running (is (nle::find-tool "zalo_send") "zalo_send is registered")
            (is (equal "https://zalo.test" (nodecode-zalo::api-base)))
            (is (= 2000 (nodecode-zalo::setting :max-length)))
            (is (equal '("u1") (nodecode-zalo::setting :allowed-users)))
            (is nodecode-zalo::*poll-worker* "the long-poll worker runs"))
  (:stopped (is (null (nle::find-tool "zalo_send")) "stop unregisters the tool")
            (is (null nodecode-zalo::*poll-worker*) "and the worker is gone"))
  (:refused ("poll_seconds" 0) ("max_length" 5) ("allowed_users" #()))
  (:idle nodecode-zalo:zalo-error (nodecode-zalo::run-command "" nil)))

(deftest zalo-cell-a-token-file-is-enough ()
  (with-zalo-wire
    (with-cell-stop ((zalo-start "bot_token_file" (bot-token-file) "allow_all" t
                                 "api_base" "https://zalo.test"))
      (is (equal "1234:test-secret" (nodecode-zalo::bot-token)) "the token is read once, at start"))))

(deftest zalo-cell-nobody-allowed-refuses-by-name ()
  (with-zalo-wire
    (let ((text (handler-case (progn (zalo-start "bot_token" "t" "allowed_users" #()) "")
                  (error (condition) (princ-to-string condition)))))
      (is-carrying (text text) ("allowed_users" "allow_all")))
    (is (null (nle::find-tool "zalo_send")) "a refused start installed nothing")))

;;; --- the lap -------------------------------------------------------------------

(deftest zalo-cell-a-message-becomes-one-turn-and-its-answer-comes-back ()
  (with-zalo-wire
    (with-submits
      (with-zalo ()
        (nodecode-zalo::handle-update (zalo-update "c1" "u1" "Xin chào"))
        (is (= 1 (length *submits*)) "one update, one turn")
        (is (equal "zalo-c1" (first (first *submits*))) "the chat's own session")
        (is-carrying (prompt (second (first *submits*))) ("Xin chào" "u1"))
        (is (null (sent-texts)) "nothing is sent before the turn answers")
        (with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) nil)
                                    (nlk:turn-answer (session)
                                      (declare (ignore session))
                                      "**Chào** bạn, mình khoẻ."))
          (nodecode-zalo::deliver-pending))
        (is (equal '("Chào bạn, mình khoẻ.") (sent-texts)) "the answer goes back as plain text")
        (is (null nodecode-zalo::*asking*) "the chat is not owed twice")))))

(deftest zalo-cell-an-open-turn-is-left-alone-and-a-long-answer-is-chunked ()
  (with-zalo-wire
    (with-zalo ("max_length" 100)
      (setf nodecode-zalo::*asking* (list (cons "c1" "zalo-c1")))
      (with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) t)
                                  (nlk:turn-answer (session) (declare (ignore session)) "not yet"))
        (nodecode-zalo::deliver-pending))
      (is (null (sent-texts)) "a running turn is not posted")
      (with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) nil)
                                  (nlk:turn-answer (session)
                                    (declare (ignore session))
                                    (make-string 250 :initial-element #\a)))
        (nodecode-zalo::deliver-pending))
      (is (= 3 (length (sent-texts))) "250 characters at 100 each is three messages"))))

(deftest zalo-cell-a-stranger-and-a-silent-update-are-ignored ()
  (with-zalo-wire
    (with-submits
      (with-zalo ()
        (nodecode-zalo::handle-update (zalo-update "c1" "u9" "let me in"))
        (nodecode-zalo::handle-update (zalo-update "c1" "u1" ""))
        (is (null *submits*) "neither is a turn")
        (is (null (sent-texts)) "and neither is answered")))))

;;; --- the commands the bot answers itself ----------------------------------------

(deftest zalo-cell-a-command-answers-without-a-turn ()
  (with-zalo-wire
    (with-submits
      (with-zalo ()
        (nodecode-zalo::handle-update (zalo-update "c1" "u1" "/menu"))
        (is (null *submits*) "a menu is no turn")
        (is-carrying (text (first (sent-texts))) ("Menu lệnh" "/rules"))
        (nodecode-zalo::handle-update (zalo-update "c1" "u1" "/poll Ăn gì? | phở, cơm"))
        (let ((body (first (calls-of "createPoll"))))
          (is (equal "Ăn gì?" (nlk:json-value body :string "question")))
          (is (equal '("phở" "cơm") (coerce (nlk:json-value body :array "options") 'list))))
        (nodecode-zalo::handle-update (zalo-update "c1" "u1" "/kick @42"))
        (is (equal "42" (nlk:json-value (first (calls-of "kickGroupMember")) :string "user_id")))
        (nodecode-zalo::handle-update (zalo-update "c1" "u1" "/nope"))
        (is-carrying (text (car (last (sent-texts)))) ("/nope" "/menu"))))))

(deftest zalo-cell-the-token-never-reaches-an-error ()
  (with-zalo-wire
    (answering (list (cons "getMe" (list "{\"message\":\"bad token 1234:test-secret\"}" 500))))
    (with-zalo ()
      (let ((text (handler-case (nodecode-zalo::request "getMe")
                    (error (condition) (princ-to-string condition)))))
        (is-carrying (text text) ("HTTP 500" "***"))
        (is (null (search "1234:test-secret" text)) "the token is scrubbed out")))))

;;; --- plain text, and the commands the shell types --------------------------------

(deftest zalo-cell-markdown-is-stripped-for-a-platform-that-renders-none ()
  (is (equal "Title"
             (nodecode-zalo:strip-markdown "# Title")))
  (is (equal "bold and link (https://example.test/a) and code"
             (nodecode-zalo:strip-markdown "**bold** and [link](https://example.test/a) and `code`")))
  (is (equal '("abc" "de") (nodecode-zalo:chunks "abcde" 3)))
  (is (equal '("") (nodecode-zalo:chunks "" 3))))

(deftest zalo-cell-the-slash-command-shows-the-bot-and-sends ()
  (with-zalo-wire
    (answering (list (cons "getMe" (list "{\"ok\":true,\"result\":{\"display_name\":\"Zalo Test Bot\"}}" 200))))
    (with-zalo ()
      (is-carrying (text (nodecode-zalo::run-command "" nil)) ("Zalo Test Bot" "u1"))
      (is (equal "Sent to c7." (nodecode-zalo::run-command "c7 **hi**" nil)))
      (is (equal '("hi") (sent-texts))))))

(deftest zalo-cell-the-tool-sends-one-message ()
  (with-zalo-wire
    (with-zalo ()
      (let ((entry (nle::find-tool "zalo_send")))
        (is entry "zalo_send is registered")
        (is (equal "Sent 1 message to c3."
                   (funcall (third entry) (nlk:json-object "chat_id" "c3" "text" "hello"))))
        (is (equal '("hello") (sent-texts)))))))
