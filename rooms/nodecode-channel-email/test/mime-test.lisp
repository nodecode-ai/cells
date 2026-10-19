;;;; mime-test.lisp --- one message read, gated, placed and written.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure: the shapes are RFC 5322/2045/2047/2231/3834/8601's, and the quoting
;;;; lines the common clients write.

(in-package #:nodecode.test)

(defun crlf-lines (&rest lines)
  (format nil (concatenate 'string "~{~a" (coerce '(#\Return #\Newline) 'string) "~}") lines))

(deftest channel-email-reads-a-multipart-message ()
  (let ((mail (nce:parse-message
               (crlf-lines "From: =?UTF-8?Q?Kim_K=C3=B6nig?= <Kim@Example.com>"
                           "Subject: =?utf-8?B?SMOpbGxv?= =?utf-8?B?IHdvcmxk?="
                           "References: <r0@x>"
                           "  <r1@x>"
                           "Content-Type: multipart/mixed; boundary=\"b1\""
                           ""
                           "--b1"
                           "Content-Type: multipart/alternative; boundary=b2"
                           ""
                           "--b2"
                           "Content-Type: text/plain; charset=utf-8"
                           "Content-Transfer-Encoding: quoted-printable"
                           ""
                           "Caf=C3=A9 is ="
                           "open?"
                           "--b2"
                           "Content-Type: text/html"
                           ""
                           "<p>ignored when there is plain text</p>"
                           "--b2--"
                           "--b1"
                           "Content-Type: application/octet-stream"
                           "Content-Disposition: attachment; filename*=utf-8''na%C3%AFve.bin"
                           "Content-Transfer-Encoding: base64"
                           ""
                           "AAEC"
                           "--b1--"))))
    (is (equal '("kim@example.com" "Kim König") (multiple-value-list (nce:from-address mail))))
    (is (equal "Héllo world" (nce:header-text (nce:header mail "subject"))) "adjacent encoded words join")
    (is (equal "Café is open?" (nce:mail-text mail)) "quoted-printable, soft break joined")
    (is (equal '("<r0@x>" "<r1@x>") (nce:message-ids (nce:header mail "references"))) "folded header unfolded")
    (is-present (file (first (nce:mail-attachments mail))) "the attachment is a file"
      (is (equal "naïve.bin" (getf file :filename)) "RFC 2231 filename")
      (is (equalp #(0 1 2) (getf file :octets))))))

(deftest channel-email-reads-an-html-only-message ()
  (let ((mail (nce:parse-message
               (crlf-lines "From: kim@example.com"
                           "Content-Type: text/html; charset=iso-8859-1"
                           ""
                           "<html><head><style>p{}</style></head><body><div>Hi &amp; <b>bye</b><br>café</div>"
                           "<div class=\"gmail_quote\">On Mon kim wrote: old</div></body></html>"))))
    (is (equal (format nil "Hi & bye~%café") (nce:mail-text mail)) "tags gone, the quoted thread cut")))

(deftest channel-email-cuts-the-quoted-thread ()
  (is (equal "Sounds good."
             (nce:strip-quoted-reply (format nil "Sounds good.~%~%On Mon, 19 Oct 2026 at 18:00, Bot <b@x> wrote:~%> old answer~%> more")))
      "Gmail's attribution")
  (is (equal "Ja."
             (nce:strip-quoted-reply (format nil "Ja.~%~%Op ma 19 okt. 2026 om 18:00 schreef Bot <b@x>:~%~%> oud")))
      "a Dutch attribution")
  (is (equal "Yes."
             (nce:strip-quoted-reply (format nil "Yes.~%~%On Mon, 19 Oct 2026 at 18:00, Bot~%<b@x> wrote:~%> old")))
      "an attribution wrapped over two lines")
  (is (equal "Fine."
             (nce:strip-quoted-reply (format nil "Fine.~%________________________________~%From: Bot~%Sent: Monday~%old")))
      "Outlook's divider")
  (is (equal "answer" (nce:strip-quoted-reply (format nil "answer~%~%> quoted~%> more")))
      "a trailing quoted block")
  (let ((interleaved (format nil "On Mon Bot wrote:~%> q1~%a1~%> q2~%a2")))
    (is (equal interleaved (nce:strip-quoted-reply interleaved))
        "a reply written between the quoted lines keeps them"))
  (is (equal "> only a quote" (nce:strip-quoted-reply "> only a quote"))
      "never cut to nothing"))

(deftest channel-email-machines-get-no-answer ()
  (flet ((machine (&rest headers)
           (nce:automated-p (nce:parse-message (apply #'crlf-lines (append headers (list "" "x"))))
                            :self "bot@x.com")))
    (is (null (machine "From: kim@example.com")) "a person's")
    (is (null (machine "From: kim@example.com" "Auto-Submitted: no")) "Auto-Submitted: no is a person's")
    (is (equal "auto-submitted" (machine "From: kim@example.com" "Auto-Submitted: auto-replied")))
    (is (equal "bulk precedence" (machine "From: kim@example.com" "Precedence: bulk")))
    (is (equal "mailing list" (machine "From: kim@example.com" "List-Id: <dev.lists.x>")))
    (is (equal "bounce" (machine "From: kim@example.com" "Return-Path: <>")))
    (is (equal "delivery report" (machine "From: kim@example.com" "Content-Type: multipart/report; report-type=delivery-status")))
    (is (equal "system sender" (machine "From: MAILER-DAEMON@example.com")))
    (is (equal "system sender" (machine "From: no-reply@example.com")))
    (is (equal "own message" (machine "From: Bot <BOT@x.com>")))))

(deftest channel-email-only-the-top-stamp-vouches ()
  (flet ((vouched (from &rest stamps)
           (multiple-value-list
            (nce:authenticated-p (nce:parse-message
                                  (apply #'crlf-lines
                                         (append (mapcar (lambda (s) (format nil "Authentication-Results: ~a" s)) stamps)
                                                 (list (format nil "From: ~a" from) "" "x"))))))))
    (is (equal '(t "dmarc") (vouched "kim@example.com" "mx.example.org (amavis); dmarc=pass (p=none) header.from=example.com")))
    (is (equal '(t "dkim") (vouched "kim@mail.example.com" "mx; dkim=pass (2048-bit) header.d=example.com; dmarc=none"))
        "a parent domain's signature aligns")
    (is (equal '(t "spf") (vouched "kim@example.com" "mx; spf=pass smtp.mailfrom=bounce@example.com")))
    (is (null (first (vouched "kim@example.com" "mx; dkim=pass header.d=evil.com; spf=fail smtp.mailfrom=x@evil.com")))
        "another domain's signature vouches for nothing")
    (is (null (first (vouched "kim@example.com" "mx; dmarc=fail header.from=example.com"
                              "forged; dmarc=pass header.from=example.com")))
        "a stamp below the receiving server's is the sender's own text")
    (is (null (first (vouched "kim@example.com")))
        "no stamp, no trust")
    (is (equal "stamped by mx, not mx.example.org"
               (second (multiple-value-list
                        (nce:authenticated-p (nce:parse-message (crlf-lines "Authentication-Results: mx; dmarc=pass"
                                                                            "From: kim@example.com" "" "x"))
                                             :authserv "mx.example.org")))))))

(deftest channel-email-senders-match-whole ()
  (is (nce:sender-allowed-p "kim@example.com" '("Kim@Example.com")))
  (is (nce:sender-allowed-p "kim@example.com" '("@example.com")))
  (is (not (nce:sender-allowed-p "kim@evilexample.com" '("@example.com"))))
  (is (not (nce:sender-allowed-p "kim@sub.example.com" '("@example.com"))))
  (is (not (nce:sender-allowed-p nil '("@example.com")))))

(deftest channel-email-a-thread-is-one-room-per-sender ()
  (flet ((root (&rest headers)
           (nce:thread-root (nce:parse-message (apply #'crlf-lines (append headers (list "" "x")))))))
    (is (equal "<r0@x>" (root "Message-ID: <m2@x>" "In-Reply-To: <m1@x>" "References: <r0@x> <m1@x>")))
    (is (equal "<m1@x>" (root "Message-ID: <m2@x>" "In-Reply-To: <m1@x>")))
    (is (equal "<m2@x>" (root "Message-ID: <m2@x>"))))
  (let ((kim (nce:thread-channel "kim@example.com" "<r0@x>")))
    (is (equal kim (nce:thread-channel "KIM@example.com" "<R0@x>")))
    (is (not (equal kim (nce:thread-channel "eve@example.com" "<r0@x>")))
        "a forged References reaches no one else's room")
    (is (= 16 (length kim)))
    (is (every (lambda (c) (digit-char-p c 16)) kim) "no hyphen: the kit's session names carry it whole")))

(deftest channel-email-a-reply-is-written-whole ()
  (let* ((text (nce:compose-message :from "bot+nc@example.com" :from-name "Nodecode"
                                    :to "kim@example.com" :to-name "Kim König"
                                    :subject (nce:reply-subject "RE: Re: Héllo")
                                    :text (format nil "Hi Kim~%café ✓")
                                    :message-id "<nc.1@example.com>"
                                    :in-reply-to "<m1@x>" :references '("<r0@x>" "<m1@x>")
                                    :attachments (list (list :filename "näme.txt" :media-type "text/plain"
                                                             :octets (babel:string-to-octets "file body" :encoding :utf-8)))))
         (mail (nce:parse-message text)))
    (is (every (lambda (c) (< (char-code c) 128)) text) "7-bit clean")
    (is (every (lambda (line) (<= (length line) 998)) (uiop:split-string text :separator '(#\Newline))))
    (is (search (format nil "~c~%" #\Return) text) "CRLF lines")
    (is (equal "Re: Héllo" (nce:header-text (nce:header mail "subject"))) "one Re:")
    (is (equal '("kim@example.com" "Kim König") (multiple-value-list (nce:parse-address (nce:header mail "to")))))
    (is (equal "<m1@x>" (nce:header mail "in-reply-to")))
    (is (equal "<r0@x> <m1@x>" (nce:header mail "references")))
    (is (equal (format nil "Hi Kim~%café ✓") (nce:mail-text mail)))
    (is (equal "auto-submitted" (nce:automated-p mail)) "another responder never answers it")
    (is-present (file (first (nce:mail-attachments mail))) "the file rides as its own part"
      (is (equal "näme.txt" (getf file :filename)))
      (is (equal "file body" (babel:octets-to-string (getf file :octets) :encoding :utf-8))))))

(deftest channel-email-a-card-sends-nothing-and-files-ride-the-answer ()
  (let* ((book (nce:make-mail-book))
         (identity (list :address "bot+nc@example.com" :name "Nodecode"))
         (channel (nce:thread-channel "kim@example.com" "<m1@x>"))
         (target (list :channel-id channel))
         (chunk (nck::make-text-chunk :text "the answer"))
         (path (merge-pathnames (format nil "nce-file-~36r.txt" (random (expt 2 40))) (uiop:temporary-directory))))
    (setf (gethash channel (nce::mail-book-threads book))
          (list :to "kim@example.com" :name "Kim" :subject "Hello" :references '("<m1@x>") :last "<m1@x>"))
    (is (equal "NOOP" (nck:request-plan-method (nce::message-plan book identity target chunk :card t :timeout-seconds 30)))
        "the status card is no email")
    (is (equal "NOOP" (nck:request-plan-method (nce::message-plan book identity target chunk :panel t :timeout-seconds 30))))
    (is (equal "FAIL" (nck:request-plan-method (nce::message-plan book identity (list :channel-id "0000000000000000") chunk :timeout-seconds 30)))
        "a thread the book never saw fails loudly, never mails a stranger")
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede) (write-string "chart data" out))
           (let* ((plan (nce::message-plan book identity target chunk :files (list path) :timeout-seconds 30))
                  (body (nck:request-plan-body plan))
                  (sent (nce:parse-message (getf body :data))))
             (is (equal "SEND" (nck:request-plan-method plan)))
             (is (equal "the answer" (nce:mail-text sent)))
             (is (equal "<m1@x>" (nce:header sent "in-reply-to")))
             (is-present (file (first (nce:mail-attachments sent))) "the answer's file rides it"
               (is (equal (file-namestring path) (getf file :filename)))
               (is (equal "chart data" (babel:octets-to-string (getf file :octets)))))))
      (ignore-errors (delete-file path)))))
