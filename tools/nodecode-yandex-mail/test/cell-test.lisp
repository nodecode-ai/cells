;;;; cell-test.lisp --- the yandex-mail cell against a scripted mailbox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No socket, no network and no account: the transport wire.lisp opens is
;;;; replaced by a script of server lines, so the IMAP and SMTP commands the
;;;; cell writes and the answers it parses are the real ones, and nothing
;;;; leaves the machine. Each tool is called the way a model's call reaches
;;;; it - through the real dispatch, contract and all.

(in-package #:nodecode.test)

(define-test-slice "yandex-mail" "YANDEX-MAIL-CELL-"
  :start nodecode-yandex-mail:start-cell
  :defaults (("login" "someone@yandex.ru") ("app_password" "app-password")))

;;; --- a scripted mailbox -------------------------------------------------------------

(defstruct (fake-mail (:constructor make-fake-mail))
  "A stand-in for one connection: the lines a server would say, the lines
the cell said, and whether it was closed."
  (queue '()) (script '()) (sent '()) (closed nil) (host "imap.test"))

(defvar *script* '()
  "The current script: (MATCHER . ROWS) entries, MATCHER a string the
client's line contains or a function of it, ROWS a list of lines and octet
vectors, or a function of the client's line answering that list.")

(defvar *connection* nil "The last connection opened, for a test to read.")

(defvar *greeting* "* OK scripted mailbox ready"
  "The first line the connection says: an IMAP greeting, or SMTP's 220.")

(defun fake-ok (line)
  "The tagged OK a server answers LINE's command with."
  (format nil "~a OK done" (subseq line 0 (or (position #\Space line) (length line)))))

(defun literal-response (head octets tail)
  "ROWS for a response whose HEAD carries a trailing literal."
  (list (format nil "~a {~d}" head (length octets)) octets ")" tail))

(defun scripted-transport (host port tls timeout)
  (declare (ignore port tls timeout))
  (setf *connection*
        (make-fake-mail :host host
                        ;; Every connection logs in; a script that says more
                        ;; about LOGIN comes first.
                        :script (append (list (cons (lambda (line)
                                                      (and (search " LOGIN" line)
                                                           (not (search "AUTH" line))))
                                                    (lambda (line) (list (fake-ok line)))))
                                        *script*)
                        :queue (list *greeting*))))

(defmethod nodecode-yandex-mail::transport-write ((transport fake-mail) text)
  (push text (fake-mail-sent transport))
  (when (stringp text)
    (let ((entry (find-if (lambda (entry)
                            (let ((matcher (first entry)))
                              (if (functionp matcher)
                                  (funcall matcher text)
                                  (search matcher text))))
                          (fake-mail-script transport))))
      (when entry
        (let ((rows (if (functionp (rest entry)) (funcall (rest entry) text) (rest entry))))
          (setf (fake-mail-queue transport)
                (append (fake-mail-queue transport) rows))))))
  text)

(defmethod nodecode-yandex-mail::transport-read-line ((transport fake-mail))
  (let ((item (pop (fake-mail-queue transport))))
    (unless item (error "the scripted mailbox ran out of lines"))
    (when (typep item '(vector (unsigned-byte 8)))
      (push item (fake-mail-queue transport))
      (error "the scripted mailbox expected an octet literal here, not a line"))
    item))

(defmethod nodecode-yandex-mail::transport-read-octets ((transport fake-mail) size)
  (let ((item (pop (fake-mail-queue transport))))
    (unless (and (typep item '(vector (unsigned-byte 8))) (= size (length item)))
      (error "the scripted mailbox expected ~d octets" size))
    item))

(defmethod nodecode-yandex-mail::transport-starttls ((transport fake-mail)) transport)

(defmethod nodecode-yandex-mail::transport-close ((transport fake-mail))
  (setf (fake-mail-closed transport) t)
  nil)

(defmacro with-script ((&rest script) &body body)
  "BODY with the cell's open-transport replaced by a mailbox saying SCRIPT."
  `(let ((*script* (list ,@(loop for (matcher responder) in script
                                  collect `(cons ,matcher ,responder))))
         (*connection* nil)
         (nodecode-yandex-mail::*open-transport* #'scripted-transport))
     ,@body))

;;; --- the answers a model reads -------------------------------------------------------

(defun answer (id tool arguments)
  "TOOL's answer to ARGUMENTS as the model reads it, through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun answered (id tool arguments)
  "The same answer, decoded."
  (let ((text (answer id tool arguments)))
    (handler-case (nlk:decode-json text)
      (error () text))))

;;; --- the message the scripts carry ----------------------------------------------------

(defparameter +headers+
  (nodecode-yandex-mail::utf8
   (format nil "From: Ada <ada@example.com>~c~cTo: someone@yandex.ru~c~cSubject: Invoice 42~c~cDate: Sat, 08 Nov 2026 10:00:00 +0000~c~cMessage-ID: <abc123@example.com>~c~c~c~c"
           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
           #\Return #\Linefeed #\Return #\Linefeed))
  "The header block a FETCH HEADER.FIELDS answers with.")

(defparameter +message+
  (nodecode-yandex-mail::utf8
   (format nil "From: Ada <ada@example.com>~c~cTo: someone@yandex.ru~c~cSubject: Invoice 42~c~cMIME-Version: 1.0~c~cContent-Type: multipart/mixed; boundary=\"B1\"~c~c~c~c--B1~c~cContent-Type: text/plain; charset=utf-8~c~c~c~cHello there, the invoice is attached.~c~c--B1~c~cContent-Type: application/pdf; name=\"report.pdf\"~c~cContent-Disposition: attachment; filename=\"report.pdf\"~c~cContent-Transfer-Encoding: base64~c~c~c~cJVBERi0xLjQK~c~c--B1--~c~c"
           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
           #\Return #\Linefeed))
  "One message: a text part and a base64 PDF.")

;;; --- the lifecycle, from the one declaration ------------------------------------------

(define-cell-lifecycle-tests "yandex-mail"
  (:config "app_password" "app-password")
  (:help :yandex-mail)
  (:refused ("app_password" "") ("login" "")))

;;; --- yandex_mail_list_folders --------------------------------------------------------

(deftest yandex-mail-cell-lists-folders-with-counts ()
  ;; Both folders come through, with the counts STATUS answered, and a folder
  ;; the account cannot select is left out.
  (with-script (("LIST" (lambda (line)
                             (list "* LIST (\\HasNoChildren) \"/\" \"INBOX\""
                                   "* LIST (\\Noselect) \"/\" \"[Yandex]\""
                                   "* LIST (\\HasNoChildren \\Trash) \"/\" \"Trash\""
                                   (fake-ok line))))
                ("STATUS" (lambda (line)
                              (list (if (search "INBOX" line)
                                        "* STATUS \"INBOX\" (MESSAGES 3 UNSEEN 1)"
                                        "* STATUS \"Trash\" (MESSAGES 0 UNSEEN 0)")
                                    (fake-ok line)))))
    (with-cell-stop ((yandex-mail-start))
      (let* ((text (answer "ym-1" "yandex_mail_list_folders" "{}"))
             (object (handler-case (nlk:decode-json text) (error () (format t "~%ANSWER: ~a~%" text) nil)))
             (folders (if object (nlk:json-array object "folders") #())))
        (is (= 2 (length folders)) "the selectable folders, and only those")
        (is (equal "INBOX" (nlk:json-value (aref folders 0) :string "name")))
        (is (= 3 (nlk:json-value (aref folders 0) :integer "messages")) "the count STATUS answered")
        (is (= 1 (nlk:json-value (aref folders 0) :integer "unread")))))))

(deftest yandex-mail-cell-honours-the-folder-fence ()
  (with-script (("LIST" (lambda (line)
                             (list "* LIST (\\HasNoChildren) \"/\" \"INBOX\""
                                   "* LIST (\\HasNoChildren) \"/\" \"Archive\""
                                   (fake-ok line)))))
    (with-cell-stop ((yandex-mail-start "folders" #("Archive")))
      (let* ((object (answered "ym-fence" "yandex_mail_list_folders" "{\"include_counts\": false}"))
             (folders (nlk:json-array object "folders")))
        (is (= 1 (length folders)) "only the folder the settings allow")
        (is (equal "Archive" (nlk:json-value (aref folders 0) :string "name")))))))

;;; --- yandex_mail_search_messages ------------------------------------------------------

(deftest yandex-mail-cell-searches-and-summarises ()
  (with-script (("EXAMINE" (lambda (line) (list (fake-ok line))))
                ("SEARCH" (lambda (line)
                              (is (search "SUBJECT \"invoice\"" line) "the criterion reaches the server")
                              (is (search "UNSEEN" line) "and so does the unread one")
                              (list "* SEARCH 103 104 105" (fake-ok line))))
                ("FETCH" (lambda (line)
                             (literal-response
                              "* 1 FETCH (UID 105 FLAGS (\\Seen) RFC822.SIZE 2048 INTERNALDATE \"08-Nov-2026 10:00:00 +0000\" BODY[HEADER.FIELDS (FROM TO CC SUBJECT DATE MESSAGE-ID)]"
                              +headers+
                              (fake-ok line)))))
    (with-cell-stop ((yandex-mail-start))
      (let* ((object (answered "ym-2" "yandex_mail_search_messages"
                                "{\"folder\":\"INBOX\",\"subject\":\"invoice\",\"unread_only\":true,\"limit\":1}"))
             (messages (nlk:json-array object "messages")))
        (is (= 3 (nlk:json-value object :integer "total")) "every match is counted")
        (is (= 1 (length messages)) "the page is the limit's")
        (is (equal "Invoice 42" (nlk:json-value (aref messages 0) :string "subject")) "the header is decoded")
        (is (equal "ada@example.com" (nodecode-yandex-mail::bare-address
                                       (nlk:json-value (aref messages 0) :string "from"))) "the sender's address is read off the display form")))))

;;; --- yandex_mail_read_message ---------------------------------------------------------

(deftest yandex-mail-cell-reads-a-paged-message ()
  (with-script (("EXAMINE" (lambda (line) (list (fake-ok line))))
                ("FETCH" (lambda (line)
                             (literal-response
                              "* 1 FETCH (UID 105 FLAGS () RFC822.SIZE 700 INTERNALDATE \"08-Nov-2026 10:00:00 +0000\" BODY[]"
                              +message+
                              (fake-ok line)))))
    (with-cell-stop ((yandex-mail-start))
      (let* ((object (answered "ym-3" "yandex_mail_read_message"
                                "{\"folder\":\"INBOX\",\"uid\":105,\"max_chars\":12}"))
             (message (nlk:json-value object :object "message"))
             (attachments (nlk:json-array message "attachments")))
        (is (search "Hello there" (nlk:json-value message :string "body")) "the text part is the body")
        (is (nlk:json-value message :boolean "truncated") "twelve characters is a page, not the message")
        (is (= 1 (length attachments)) "the attachment is listed")
        (is (equal "report.pdf" (nlk:json-value (aref attachments 0) :string "filename")))
        (is (equal "application/pdf" (nlk:json-value (aref attachments 0) :string "content_type")))
        (is (nlk:json-value message :boolean "unread") "reading it did not mark it read")))))

;;; --- yandex_mail_save_attachment -------------------------------------------------------

(deftest yandex-mail-cell-saves-an-attachment ()
  (let ((directory (merge-pathnames (format nil "ymail-~a/" (random 1000000)) (uiop:temporary-directory))))
    (with-script (("EXAMINE" (lambda (line) (list (fake-ok line))))
                  ("FETCH" (lambda (line)
                               (literal-response
                                "* 1 FETCH (UID 105 FLAGS () RFC822.SIZE 700 INTERNALDATE \"08-Nov-2026 10:00:00 +0000\" BODY[]"
                                +message+
                                (fake-ok line)))))
      (with-cell-stop ((yandex-mail-start "actions" #("read_message" "save_attachment" "list_folders")
                                          "attachment_dir" (namestring directory)))
        (let* ((object (answered "ym-4" "yandex_mail_save_attachment"
                                  "{\"folder\":\"INBOX\",\"uid\":105,\"part_id\":\"1.2\"}"))
               (attachment (nlk:json-value object :object "attachment"))
               (path (nlk:json-value attachment :string "path")))
          (is (equal "report.pdf" (nlk:json-value attachment :string "filename")))
          (is (= 9 (nlk:json-value attachment :integer "size")) "the base64 part is decoded")
          (is (probe-file path) "the bytes are in a file")
          (is (equal "%PDF-1.4" (subseq (uiop:read-file-string path) 0 8)) "and they are the attachment's own"))))))

;;; --- yandex_mail_send_message ---------------------------------------------------------

(deftest yandex-mail-cell-sends-over-smtp ()
  (let ((*greeting* "220 smtp.test ready"))
   (with-script (("EHLO" (lambda (line) (declare (ignore line)) (list "250-smtp.test" "250 AUTH PLAIN LOGIN")))
                ("AUTH" (lambda (line) (list "235 accepted" )))
                ("MAIL FROM" (lambda (line) (list "250 sender ok")))
                ("RCPT TO" (lambda (line) (list "250 recipient ok")))
                ("DATA" (lambda (line) (list "354 go ahead")))
                ((lambda (line) (string= (string-trim '(#\Return #\Linefeed) line) "."))
                 (lambda (line) (list "250 queued as 42")))
                ("QUIT" (lambda (line) (list "221 bye"))))
    (with-cell-stop ((yandex-mail-start "actions" #("read_message" "send_message")))
      (let* ((object (answered "ym-5" "yandex_mail_send_message"
                                "{\"to\":[\"ada@example.com\"],\"subject\":\"Re: Invoice 42\",\"body\":\"Thanks.\"}"))
             (sent (fake-mail-sent *connection*)))
        (is (nlk:json-value object :boolean "sent") "the server accepted it")
        (is (some (lambda (line) (search "RCPT TO:<ada@example.com>" line)) sent) "the recipient reached the wire")
        (is (some (lambda (line) (search "Content-Transfer-Encoding: base64" line)) sent) "the body is a message, not a bare string"))))))

(deftest yandex-mail-cell-holds-sending-to-the-fence ()
  ;; send_to is the account's fence: an address outside it is refused before
  ;; a password is spent, and the refusal says so.
  (with-script (("EHLO" (lambda (line) (list "250 AUTH PLAIN"))))
    (with-cell-stop ((yandex-mail-start "actions" #("send_message")
                                        "send_to" #("@example.com")))
      (let* ((object (answered "ym-6" "yandex_mail_send_message"
                                "{\"to\":[\"ada@elsewhere.test\"],\"subject\":\"hi\",\"body\":\"x\"}"))
             (text (format nil "~a" object)))
        (is (search "send_to" text) "the refusal names the fence")))))

;;; --- a missing setting refuses ------------------------------------------------------------

(deftest yandex-mail-cell-without-a-password-refuses ()
  ;; The one setting no verb can work without: with neither app_password nor
  ;; app_password_env the section is refused at start, and nothing installs.
  (let ((condition (signals-error nlk:config-refusal (yandex-mail-start "app_password" ""))))
    (is (search "app password" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "yandex_mail_list_folders")) "and installs nothing")))

(deftest yandex-mail-cell-answers-a-refusal-not-an-error ()
  ;; A call the folder fence refuses comes back as the call's failure, in
  ;; words the model can read, and never as an unhandled error.
  (with-cell-stop ((yandex-mail-start "folders" #("Archive")))
    (let ((text (answer "ym-7" "yandex_mail_search_messages" "{\"folder\":\"INBOX\"}")))
      (is (stringp text) "an answer either way")
      (is (search "Archive" text) "the refusal names the folders the fence allows")
      (is (search "ERROR" text) "and it is the call's failure, not an error"))))

;;; --- the parser, on its own ---------------------------------------------------------------

(deftest yandex-mail-cell-parses-a-message ()
  (let* ((entity (nodecode-yandex-mail:parse-message +message+))
         (parts (nodecode-yandex-mail:part-attachments entity)))
    (is (search "Hello there"
                (nodecode-yandex-mail:part-text (nodecode-yandex-mail:parse-message +message+))) "the text part is the message's text")
    (is (= 1 (length parts)) "one attachment")
    (is (equal "1.2" (getf (first parts) :part_id)) "numbered the way IMAP numbers it")))

(deftest yandex-mail-cell-pages-text ()
  (multiple-value-bind (page next eof)
      (nodecode-yandex-mail:page-text "abcdefghij" 0 4)
    (is (equal "abcd" page))
    (is (= 4 next) "the next page starts where this one stopped")
    (is (not eof) "and there is more")))
