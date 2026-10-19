;;;; wire-test.lisp --- IDLE and APPEND against a scripted IMAP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server is a thread on loopback speaking the lines RFC 2177 (IDLE),
;;;; RFC 6154 (special-use LIST) and RFC 3501 (APPEND and its literal) give
;;;; the client; the wire is the adapter's own, without TLS.

(in-package #:nodecode.test)

(defun email-elapsed (start)
  (/ (- (get-internal-real-time) start) internal-time-units-per-second))

(deftest channel-email-idle-wakes-the-moment-mail-arrives ()
  (let ((seen '()) (start (get-internal-real-time)))
    (multiple-value-bind (outcome failure)
        (call-with-email-imap
         (lambda (read-line write read-octets)
           (declare (ignore read-octets))
           (push (funcall read-line) seen)
           (funcall write (email-crlf "+ idling"))
           (sleep 0.4)
           (funcall write (email-crlf "* 2 FETCH (FLAGS (\\Seen))" "* 4 EXISTS" "* 1 RECENT"))
           (push (funcall read-line) seen)
           (funcall write (email-crlf "N1 OK Idle completed.")))
         (lambda (wire) (nce:imap-idle wire 60 (constantly nil))))
      (is (null failure) "the server saw what it expected")
      (is (eq :new outcome) "EXISTS ends the wait, a flag change does not")
      (is (< (email-elapsed start) 5) "read at once, not at the next poll")
      (is (equal '("N1 IDLE" "DONE") (reverse seen)) "IDLE, then DONE"))))

(deftest channel-email-idle-ends-on-stop-and-on-its-time ()
  (dolist (case '(:stop :quiet))
    (let ((seen '()) (start (get-internal-real-time)))
      (multiple-value-bind (outcome failure)
          (call-with-email-imap
           (lambda (read-line write read-octets)
             (declare (ignore read-octets))
             (push (funcall read-line) seen)
             (funcall write (email-crlf "+ idling" "* OK Still here"))
             (push (funcall read-line) seen)
             (funcall write (email-crlf "N1 OK Idle completed.")))
           (lambda (wire)
             (if (eq case :stop)
                 (nce:imap-idle wire 60 (lambda () (> (email-elapsed start) 0.5)))
                 (nce:imap-idle wire 0.5 (constantly nil)))))
        (is (null failure))
        (is (eq case outcome))
        (is (< (email-elapsed start) 5) "a stop or the deadline ends the wait promptly")
        (is (equal '("N1 IDLE" "DONE") (reverse seen)) "the wait is always ended with DONE")))))

(deftest channel-email-a-sent-copy-is-appended-to-the-sent-folder ()
  (let* ((data (nce:compose-message :from "bot+nc@example.com" :to "kim@example.com" :subject "Re: Hello"
                                    :text "**hi**" :html (nce:markdown-html "**hi**") :message-id "<nc.1@example.com>"))
         (seen '()) (filed nil))
    (multiple-value-bind (result failure)
        (call-with-email-imap
         (lambda (read-line write read-octets)
           (push (funcall read-line) seen)
           (funcall write (email-crlf "* LIST (\\HasNoChildren) \"/\" INBOX"
                                "* LIST (\\HasNoChildren \\Drafts) \"/\" {6}" "Drafts"
                                "* LIST (\\HasNoChildren \\Sent) \"/\" \"Sent Items\""
                                "N1 OK List completed."))
           (let* ((command (funcall read-line))
                  (size (parse-integer command :start (1+ (position #\{ command)) :junk-allowed t)))
             (push command seen)
             (funcall write (email-crlf "+ OK"))
             (setf filed (funcall read-octets size))
             (push (funcall read-line) seen)
             (funcall write (email-crlf "N2 OK [APPENDUID 1700000000 42] Append completed."))))
         (lambda (wire)
           (let ((folder (nce:imap-special-mailbox wire "\\Sent")))
             (list folder (nce:imap-append wire folder data)))))
      (is (null failure))
      (is (equal '("Sent Items" 42) result) "filed in the folder marked \\Sent, its UID read back")
      (is (equal (list "N1 LIST \"\" \"*\""
                       (format nil "N2 APPEND \"Sent Items\" (\\Seen) {~d}" (length data))
                       "")
                 (reverse seen)) "the copy is filed read, as one literal")
      (is (equal data (and filed (map 'string #'code-char filed))) "the copy is the message sent, byte for byte"))))

(deftest channel-email-a-copy-that-fails-never-fails-the-send ()
  (let ((warnings '()))
    (handler-bind ((warning (lambda (w) (push (princ-to-string w) warnings) (muffle-warning w))))
      (is (equal '(:filed "Sent" :filed-uid 7) (nce::file-copy (lambda (data) (declare (ignore data)) (values "Sent" 7)) "m")))
      (is (search "no room" (or (getf (nce::file-copy (lambda (data) (declare (ignore data)) (error "no room")) "m") :file-error) "")) "a failed copy is named in the result"))
    (is (= 1 (length warnings)) "and warned of once")))
