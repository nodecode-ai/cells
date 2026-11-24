;;;; cell-test.lisp --- lifecycle, the detectors, the advice, the tool, /mask.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring and its own rules: a start
;;;; installs one advice, one tool, one command and one manual topic and a stop
;;;; takes them all back, a section the declaration cannot read refuses and
;;;; leaves nothing behind, the ten detectors mask what they say they mask and
;;;; leave the rest alone, a number is an ID card before it is a bank card, the
;;;; same words keep their placeholder number, the advice rewrites user text
;;;; only and hands the fold's second value back, and /mask restore writes the
;;;; plaintext to a file instead of printing it.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "mask"
  (:hooks 'nodecode.evolved::wire-messages-from-events)
  (:command "mask")
  (:help :mask)
  (:running (is (nle::find-tool "mask_test") "the tool is registered")
            (is (equal '("phone" "email" "id-card" "bank-card" "key")
                       (nodecode-mask::setting :entities)) "the defaults run every detector but IP"))
  (:stopped (is (null (nle::find-tool "mask_test")) "stop unregisters the tool")
            (is (null (mask-wire-hook)) "and takes the advice off the fold")
            (is (null (nle::find-registered-command "mask")) "and the command"))
  (:refused ("entities" (vector "passport")) ("entities" (vector))))

(deftest mask-cell-refuses-a-shape-it-cannot-run (with-saved-globals (nle:*tools*))
  (is (signals-error nlk:config-refusal (mask-start "entities" "email")) "a string where the array belongs is refused")
  (is (null (nle::find-tool "mask_test")) "and the refused start installed nothing")
  (is (null (mask-wire-hook)) "not even the advice"))

;;; --- the detectors -------------------------------------------------------------

(deftest mask-cell-masks-the-structured-kinds (with-cell-stop ((mask-start
                                                                "entities" (vector "phone" "email" "id-card"
                                                                                   "bank-card" "key" "ip")))
  (let ((masked (nodecode-mask:mask-text
                 "mail anna.rossi@example.it, call 13812345678, id 11010519491231002X, card 4111111111111111, key sk-abcdefghijklmnopqrst, host 10.1.2.3")))
    (is (search "<EMAIL_1>" masked) "an address is named, not shown")
    (is (not (search "anna.rossi" masked)) "and its local part is gone")
    (is (search "<PHONE_1>" masked) "a mainland mobile is a phone")
    (is (search "<ID_CARD_1>" masked) "eighteen characters ending in X are an ID card")
    (is (search "<BANK_CARD_1>" masked) "sixteen digits are a bank card")
    (is (search "<KEY_1>" masked) "an sk- string is a key")
    (is (search "<IP_1>" masked) "four dotted octets are an address")
    (is (not (search "13812345678" masked)) "and the digits themselves are gone"))))

(deftest mask-cell-leaves-what-is-not-personal (with-cell-stop ((mask-start))
  (is (equal "the build is 12345678" (nodecode-mask:mask-text "the build is 12345678")) "eight digits are an id, not a phone and not a card")
  (is (equal "v1.2.3" (nodecode-mask:mask-text "v1.2.3")) "a version is no address while IP is off")
  (is (equal "ANTHROPIC_KEY=hello" (nodecode-mask:mask-text "ANTHROPIC_KEY=hello")) "a bare name is no key")
  (is (equal "no address here at all" (nodecode-mask:mask-text "no address here at all")) "and plain prose is left whole")
  (is (search "10.1.2.3" (nodecode-mask:mask-text "host 10.1.2.3")) "IP is off unless the section names it")))

(deftest mask-cell-the-credential-shapes-are-keys (with-cell-stop ((mask-start))
  (let ((masked (nodecode-mask:mask-text
                 "ghp_abcdefghijklmnopqrst xoxb-1234567890 AKIAIOSFODNN7EXAMPLE Bearer abcdefgh123")))
    (is (not (search "ghp_" masked)) "a GitHub token is a key")
    (is (not (search "xoxb-" masked)) "so is a Slack token")
    (is (not (search "AKIA" masked)) "so is an AWS key id")
    (is (not (search "abcdefgh123" masked)) "so is a bearer token")
    (is (equal "<KEY_1> <KEY_2> <KEY_3> <KEY_4>" masked) "each one its own number"))))

(deftest mask-cell-a-number-is-an-id-card-before-a-bank-card (with-cell-stop ((mask-start))
  (is (equal "<ID_CARD_1>" (nodecode-mask:mask-text "110105194912310021")) "eighteen digits are the card with the higher score")
  (is (equal "<BANK_CARD_1>" (nodecode-mask:mask-text "4111111111111111111")) "nineteen digits are no ID card")
  (is (equal "<EMAIL_1> and <PHONE_1>"
             (nodecode-mask:mask-text "a@example.it and 13812345678")) "two separate detections stay two")))

(deftest mask-cell-placeholders-are-numbered-and-stable (with-cell-stop ((mask-start))
  (is (equal "write to <EMAIL_1>" (nodecode-mask:mask-text "write to a@example.it")) "the first address is number one")
  (is (equal "<EMAIL_1> again" (nodecode-mask:mask-text "a@example.it again")) "the same words keep their number")
  (is (equal "<EMAIL_2>" (nodecode-mask:mask-text "b@example.it")) "a new address takes the next number")))

;;; --- the advice on the fold -----------------------------------------------------

(deftest mask-cell-masks-the-wire-not-the-message (with-cell-stop ((mask-start))
  (let* ((system (mask-message "system" "write to anna@example.it"))
         (user (mask-message "user" "my mail is anna@example.it"))
         (assistant (mask-message "assistant" "noted anna@example.it"))
         (answer (multiple-value-list (mask-wire-answer (list system user assistant))))
         (masked (first answer)))
    (is (eq :clean (second answer)) "the fold's clean flag comes back untouched")
    (is (equal "write to anna@example.it" (gethash "content" (first masked))) "the system prompt is never read")
    (is (equal "my mail is <EMAIL_1>" (gethash "content" (second masked))) "the user's words are masked on the way out")
    (is (equal "noted anna@example.it" (gethash "content" (third masked))) "the assistant's answer is left as it was")
    (is (equal "my mail is anna@example.it" (gethash "content" user)) "the folded message is untouched: the log keeps the original")
    (is (not (eq user (second masked))) "what changed is a copy"))))

(deftest mask-cell-a-content-array-is-masked-part-by-part (with-cell-stop ((mask-start))
  (let* ((parts (vector (nlk:json-object "type" "text" "text" "call 13812345678")
                        (nlk:json-object "type" "image" "text" "not a part")))
         (content (gethash "content"
                           (first (mask-wire-answer (list (mask-message "user" parts)))))))
    (is (equal "call <PHONE_1>" (gethash "text" (aref content 0))) "a text part is masked")
    (is (equal "not a part" (gethash "text" (aref content 1))) "a part that carries no words is left alone"))))

(deftest mask-cell-a-broken-detector-fails-open (with-cell-stop ((mask-start))
  (with-stubbed-fdefinitions ((nodecode-mask::detect-hits (text &key entities)
                                  (progn text entities (error "the detector broke"))))
    (is (equal "anna@example.it" (nodecode-mask:mask-text "anna@example.it")) "a throw passes the text through"))
  (with-stubbed-fdefinitions ((nodecode-mask::mask-messages (messages)
                                  (progn messages (error "the advice broke"))))
    (let* ((messages (list (mask-message "user" "anna@example.it")))
           (answer (multiple-value-list (mask-wire-answer messages))))
      (is (eq messages (first answer)) "the fold's own answer survives")
      (is (eq :clean (second answer)) "and so does its second value")))))

;;; --- the tool and the command ---------------------------------------------------

(deftest mask-cell-the-tool-answers-a-preview (with-cell-stop ((mask-start))
  (let ((answer (mask-tool-call "call 13812345678 and mail a@example.it")))
    (is (search "<PHONE_1>" answer) "the tool answers the masked text")
    (is (search "<EMAIL_1>" answer) "the address too")
    (is (search "replaced 2 PII value(s) (EMAIL=1, PHONE=1)" answer) "and counts what it replaced, by kind")
    (is (not (search "13812345678" answer)) "never the value it replaced"))
  (is (search "replaced 0 PII value(s)" (mask-tool-call "nothing to see")) "an empty result says so")))

(deftest mask-cell-the-command-answers-status-and-preview (with-cell-stop ((mask-start))
  (is (search "mask: enabled" (mask-slash "")) "the command answers its state")
  (is (search "masking phone, email" (mask-slash "status")) "and names the detectors")
  (let ((preview (mask-slash "mail a@example.it")))
    (is (search "<EMAIL_1>" preview) "or previews any text")
    (is (search "masked 1 email" preview) "and says what it found"))))

(deftest mask-cell-off-lets-the-text-through (with-cell-stop ((mask-start))
  (is (search "masking is off" (mask-slash "off")) "the switch answers")
  (let ((answer (mask-wire-answer (list (mask-message "user" "call 13812345678")))))
    (is (equal "call 13812345678" (gethash "content" (first answer))) "the text goes out as it was written"))
  (is (search "masking is on" (mask-slash "on")) "and back on")))

(deftest mask-cell-restore-writes-the-plaintext-to-a-file
    (with-cell-stop ((mask-start "restore-dir" (namestring (uiop:temporary-directory))))
  (let* ((masked (nodecode-mask:mask-text "mail a@example.it"))
         (answer (mask-slash (format nil "restore ~a" masked)))
         (path (nodecode-mask::restore-file "s1")))
    (is (search "restored 1 placeholder(s): <EMAIL_1>" answer) "the answer names what it put back")
    (is (not (search "a@example.it" answer)) "and keeps the plaintext out of the log")
    (is (probe-file path) "the file is there")
    (is (equal "mail a@example.it" (uiop:read-file-string path)) "and holds the plaintext"))
  (is (search "nothing to restore" (mask-slash "restore nothing masked here")) "a text with no placeholder says so")))
