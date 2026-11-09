;;;; cell-test.lisp --- the protonmail cell against a scripted Proton and a scripted gpg.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No network and no account: the cell's HTTP seam is a table of answers,
;;;; its gpg seam a function answering a message's plaintext, and the session
;;;; is stubbed so the SRP handshake never runs. Each verb is called the way
;;;; the gateway calls it, with the JSON arguments a model's call carries.

(in-package #:nodecode.test)

(define-test-slice "protonmail" "PROTONMAIL-CELL-" :start nodecode-protonmail:start-cell
  :defaults (("username" "someone@proton.me") ("password" "secret")))

(defparameter *answers* '()
  "Alist of (METHOD PATH-FRAGMENT JSON), consulted by the stub.")

(defparameter *asked* '()
  "Every (METHOD URL CONTENT) the cell asked for, newest first.")

(defun stub-http (method url headers content timeout binary)
  (declare (ignore headers timeout binary))
  (push (list method url content) *asked*)
  (let ((letter (find-if (lambda (entry)
                            (and (equal (first entry) method)
                                 (search (second entry) url)))
                          *answers*)))
    (values (if letter (third letter) "{\"Code\":1000}") 200)))

(defun octets->text (octets)
  "OCTETS as the one-byte-per-character string a gpg seam answers with."
  (map 'string #'code-char octets))

(defun fake-packet (tag payload)
  "A packet of TAG carrying PAYLOAD, as a string."
  (octets->text (nodecode-protonmail::packet tag payload)))

(defparameter *session-key-hex*
  "00112233445566778899AABBCCDDEEFF00112233445566778899AABBCCDDEEFF"
  "The session key the stubbed gpg claims it used.")

(defun stub-gpg (args &key input password)
  (declare (ignore input password))
  (cond ((member "--import" args :test #'equal) (values "" 0))
        ((member "--detach-sign" args :test #'equal)
         (values (fake-packet 2 (map '(vector (unsigned-byte 8)) #'char-code "SIGNATURE")) 0))
        ((member "--encrypt" args :test #'equal)
         ;; One key packet for this account, one for the inside recipient,
         ;; then the body: the order gpg writes them in.
         (values (concatenate 'string
                              (fake-packet 1 "OWN")
                              (fake-packet 1 "THEIRS")
                              (fake-packet 18 "BODY"))
                 0))
        ((member "--show-session-key" args :test #'equal)
         (values (format nil "gpg: session key: '9:~a'~%" *session-key-hex*) 0))
        ((member "--decrypt" args :test #'equal)
         (values "MIME-Version: 1.0\r\nSubject: hi\r\n\r\nHello <b>there</b>, the invoice is ready."
                 0))
        (t (values "" 0))))

(defun argument-hash (&rest pairs)
  "A tool call's arguments, as the gateway decodes them."
  (apply #'nlk:make-json-object pairs))

(defun install-stub ()
  "Point the cell's seams at the stubs, and hand it a signed-in session."
  (setf (symbol-value 'nodecode-protonmail::*http*) #'stub-http
        (symbol-value 'nodecode-protonmail::*gpg*) #'stub-gpg
        (symbol-value 'nodecode-protonmail::*imported*) t
        (symbol-value 'nodecode-protonmail::*settings*)
        (list :username "someone@proton.me" :password "secret"
              :host "https://mail.proton.me/api" :app-version "Other"
              :gpg-program "gpg" :timeout 30)
        (symbol-value 'nodecode-protonmail::*session*)
        (nodecode-protonmail::make-session :uid "uid-1" :token "token-1"
                                           :refresh "refresh-1"
                                           :username "someone@proton.me"
                                           :passphrase "passphrase"))
  nil)

(defmacro with-proton ((&rest answers) &body body)
  "BODY with the cell's seams stubbed and ANSWERS each (METHOD path json)."
  `(progn
     (setf (symbol-value 'nodecode.test::*answers*)
           (list ,@(loop for (method path json) in answers
                         collect (list 'list method path json))))
     (setf (symbol-value 'nodecode.test::*asked*) '())
     (install-stub)
     ,@body))

(defun tget (object key)
  "KEY of the decoded OBJECT, or NIL."
  (nodecode-protonmail::jget object key))

(defun tlist (object key)
  "KEY of the decoded OBJECT as a vector, empty when absent."
  (nodecode-protonmail::jlist object key))

(defun answered (text)
  "TEXT decoded when it is JSON, else as it came."
  (handler-case (nlk:decode-json text) (error () text)))

;;; --- the lifecycle, from the one declaration ------------------------------------------

(define-cell-lifecycle-tests "protonmail"
  (:config "username" "someone@proton.me" "password" "secret")
  (:help :protonmail)
  (:refused ("username" "")))

(deftest protonmail-cell-without-a-password-refuses ()
  ;; password and password_env both unset: the section is refused at start.
  (let ((condition (signals-error nlk:config-refusal
                                  (protonmail-start "password" ""))))
    (is (search "password" (princ-to-string condition)) "the refusal names the key")))

;;; --- proton_mail_folders -------------------------------------------------------------

(deftest protonmail-cell-lists-folders-with-unread-counts ()
  (with-proton (("GET" "/mail/v4/labels"
                 "{\"Labels\":[{\"ID\":\"label-1\",\"Name\":\"Work\"}]}")
                ("GET" "/mail/v4/messages/count"
                 "{\"Counts\":[{\"LabelID\":\"label-1\",\"Unread\":4},{\"LabelID\":\"0\",\"Unread\":2}]}"))
    (let* ((object (answered (nodecode-protonmail:folders (argument-hash))))
           (folders (tlist object "folders"))
           (named (lambda (name)
                    (find name folders
                          :key (lambda (row) (tget row "name"))
                          :test #'equal))))
      (is (plusp (length folders)) "the system folders and the account's own come through")
      (is (= 4 (tget (funcall named "Work") "unread")) "the unread count is Proton's")
      (is (= 2 (tget (funcall named "Inbox") "unread")) "the inbox is a folder too"))))

;;; --- proton_mail_inbox ---------------------------------------------------------------

(deftest protonmail-cell-lists-the-inbox ()
  (with-proton (("GET" "/mail/v4/messages"
                 "{\"Messages\":[{\"ID\":\"msg-1\",\"Subject\":\"Invoice 42\",\"Time\":1762600000,\"Unread\":1,\"NumAttachments\":1,\"Sender\":{\"Address\":\"ada@example.com\",\"Name\":\"Ada\"}}]}"))
    (let* ((object (answered (nodecode-protonmail:inbox
                               (argument-hash "unread_only" t "limit" 5))))
           (messages (tlist object "messages"))
           (url (second (first *asked*))))
      (is (search "LabelID=0" url) "the inbox is label 0")
      (is (search "Unread=1" url) "unread_only reaches the wire")
      (is (search "PageSize=5" url) "the limit is the page size")
      (is (= 1 (length messages)) "one summary")
      (is (equal "Invoice 42" (tget (aref messages 0) "subject")))
      (is (tget (aref messages 0) "unread") "it is unread"))))

;;; --- proton_mail_read ----------------------------------------------------------------

(deftest protonmail-cell-reads-a-message-through-gpg ()
  (with-proton (("GET" "/mail/v4/messages/msg-1"
                 "{\"Message\":{\"ID\":\"msg-1\",\"Subject\":\"Invoice 42\",\"Unread\":1,\"Body\":\"-----BEGIN PGP MESSAGE-----\",\"Sender\":{\"Address\":\"ada@example.com\"},\"Attachments\":[{\"Name\":\"report.pdf\",\"Size\":2048}]}}"))
    (let* ((object (answered (nodecode-protonmail:read-message
                               (argument-hash "message_id" "msg-1"))))
           (body (tget object "body"))
           (attachments (tlist object "attachments")))
      (is (equal "Invoice 42" (tget object "subject")))
      (is (search "Hello there" body) "the decrypted body is read as prose")
      (is (not (search "<b>" body)) "its markup is not")
      (is (= 1 (length attachments)) "the attachment is listed, not fetched")
      (is (equal "report.pdf" (tget (aref attachments 0) "name")))
      (is (tget object "unread") "reading it changed nothing"))))

;;; --- proton_mail_mark ------------------------------------------------------------------

(deftest protonmail-cell-marks-a-message-read ()
  (with-proton (("POST" "/mail/v4/messages/read" "{\"Code\":1000}"))
    (let ((object (answered (nodecode-protonmail:mark-message
                             (argument-hash "message_id" "msg-1" "read" t))))
          (asked (first *asked*)))
      (is (equal "POST" (first asked)) "it is a write")
      (is (search "/messages/read" (second asked)) "the read endpoint, not the unread one")
      (is (search "msg-1" (third asked)) "the message is the one asked for")
      (is (tget object "read")))))

;;; --- proton_mail_trash -----------------------------------------------------------------

(deftest protonmail-cell-trashes-a-message ()
  (with-proton (("POST" "/mail/v4/messages/trash" "{\"Code\":1000}"))
    (let ((object (answered (nodecode-protonmail:trash-messages
                             (argument-hash "message_ids" (vector "msg-1" "msg-2")))))
          (asked (first *asked*)))
      (is (search "/messages/trash" (second asked)) "the trash endpoint, not the permanent one")
      (is (search "msg-1" (third asked)) "both messages travel")
      (is (equal "msg-2" (aref (tlist object "ids") 1)) "and come back in the answer"))))

;;; --- proton_mail_attachment ------------------------------------------------------------

(deftest protonmail-cell-saves-an-attachment ()
  (let* ((path (merge-pathnames (format nil "pmail-att-~a.pdf" (random 1000000))
                                 (uiop:temporary-directory)))
         (row "{\"Message\":{\"Attachments\":[{\"ID\":\"att-1\",\"Name\":\"report.pdf\",\"KeyPackets\":\"QUJD\"}]}}"))
    (unwind-protect
         (with-proton (("GET" "/mail/v4/messages/msg-1" row)
                       ("GET" "/mail/v4/attachments/att-1" "DATA"))
           (let ((object (answered (nodecode-protonmail:save-attachment
                                    (argument-hash "message_id" "msg-1"
                                                   "path" (namestring path))))))
             (is (probe-file path) "the file is written")
             (is (equal "report.pdf" (tget object "name")) "the name comes from Proton")
             (is (> (tget object "size") 0) "and the size from what was written")))
      (ignore-errors (delete-file path)))))

;;; --- proton_mail_send ------------------------------------------------------------------

(defparameter +addresses+ "{\"Addresses\":[{\"ID\":\"a1\",\"Email\":\"me@proton.me\",\"DisplayName\":\"Me\",\"Status\":1,\"Send\":1,\"Order\":1,\"Keys\":[{\"ID\":\"k1\",\"PrivateKey\":\"PRIVATE\"}]}]}")

(deftest protonmail-cell-sends-to-both-sides ()
  ;; One recipient inside Proton, one outside: the first gets a key packet,
  ;; the second the session key in the clear, as Proton's own client sends.
  (with-proton (("POST" "/mail/v4/messages/draft-1" "{\"Sent\":{\"ID\":\"msg-9\"}}")
                ("POST" "/mail/v4/messages" "{\"Message\":{\"ID\":\"draft-1\"}}")
                ("GET" "/core/v4/keys?Email=friend%40proton.me"
                 "{\"RecipientType\":1,\"Keys\":[{\"PublicKey\":\"PUBLIC\"}]}")
                ("GET" "/core/v4/keys?Email=outside%40example.com"
                 "{\"RecipientType\":2,\"Keys\":[]}")
                ("GET" "/core/v4/addresses" +addresses+))
    (let* ((object (answered (nodecode-protonmail:send-message
                               (argument-hash "to" "friend@proton.me,outside@example.com"
                                              "subject" "Invoice 42"
                                              "body" "Here it is."))))
           (send (third (first *asked*)))
           (draft (third (second *asked*))))
      (is (equal "msg-9" (tget object "id")) "the sent message's id is answered")
      (is (search "\"Invoice 42\"" draft) "the draft carries the subject")
      (is (search "BEGIN PGP MESSAGE" draft) "and an encrypted body")
      (is (search "\"Type\":1" send) "the inside recipient gets a key packet")
      (is (search "\"BodyKeyPacket\"" send) "which is where the session key is wrapped")
      (is (search "\"Type\":4" send) "the outside recipient is a clear package")
      (is (search "\"BodyKey\"" send) "with the session key in the clear")
      (is (search "aes256" send) "named by its algorithm"))))
