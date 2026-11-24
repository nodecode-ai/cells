;;;; cell-test.lisp --- lifecycle, the detectors, the advice, the tool and /pii.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its own rules: a start
;;;; installs one advice, one tool, one command and one manual topic and a stop
;;;; takes them back, a section the declaration cannot read refuses and leaves
;;;; nothing behind, the detectors mask what they say they mask and nothing
;;;; else, the advice rewrites user text only and never the folded message
;;;; itself, it hands the fold's second value back, and a detector that throws
;;;; passes the text through instead of taking the request with it.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "pii"
  (:hooks 'nodecode.evolved::wire-messages-from-events)
  (:command "pii")
  (:running (is (nle::find-tool "redact_text") "the tool is registered")
            (is (equal '("email" "phone" "card" "iban" "ip") (nodecode-pii::setting :entities)) "the defaults run every detector")
            (is (assoc :pii nle::*help-topics*) "(help :pii) is on the manual"))
  (:stopped (is (null (nle::find-tool "redact_text")) "stop unregisters the tool")
            (is (null (pii-wire-hook)) "and takes the advice off the fold")
            (is (null (nle::find-registered-command "pii")) "and the command"))
  (:refused ("entities" (vector "passport"))))

(deftest pii-cell-refuses-a-shape-it-cannot-run (with-saved-globals (nle:*tools*))
  (is (signals-error nlk:config-refusal (pii-start "entities" "email")) "a string where the array belongs is refused")
  (is (null (nle::find-tool "redact_text")) "and the refused start installed nothing")
  (is (null (pii-wire-hook)) "not even the advice"))

;;; --- the detectors -------------------------------------------------------------

(deftest pii-cell-masks-the-structured-kinds (with-cell-stop ((pii-start))
  (let ((redacted (nodecode-pii:redact-text "mail anna.rossi@example.it or call +39 055 1234567; card 4111111111111111, iban GB82WEST12345698765432, host 10.1.2.3.")))
    (is (search "<EMAIL_ADDRESS>" redacted) "an address is named, not shown")
    (is (not (search "anna.rossi" redacted)) "and its local part is gone")
    (is (search "<PHONE_NUMBER>" redacted) "a number written with a plus is a phone")
    (is (search "<CREDIT_CARD>" redacted) "a checksum-valid number is a card")
    (is (search "<IBAN_CODE>" redacted) "a check-remainder string is an IBAN")
    (is (search "<IP_ADDRESS>" redacted) "four dotted octets are an address")
    (is (not (search "4111" redacted)) "and the digits themselves are gone")))
)
(deftest pii-cell-leaves-what-is-not-personal (with-cell-stop ((pii-start))
  (is (equal "the build is 12345678" (nodecode-pii:redact-text "the build is 12345678")) "eight digits are an id, not a phone")
  (is (equal "1234 5678 9012 3456" (nodecode-pii:redact-text "1234 5678 9012 3456")) "sixteen digits that fail the checksum are no card")
  (is (equal "999.1.1.1" (nodecode-pii:redact-text "999.1.1.1")) "an octet above 255 is a version number")
  (is (equal "GB00WEST12345698765432" (nodecode-pii:redact-text "GB00WEST12345698765432")) "a wrong remainder is no IBAN")
  (is (equal "no address here at all" (nodecode-pii:redact-text "no address here at all")) "and plain prose is left whole"))
)
(deftest pii-cell-masks-the-names-the-operator-lists (with-cell-stop ((pii-start "terms" (vector "Giulia" "Rossi")))
  (is (equal "<PERSON> met <PERSON> at the port." (nodecode-pii:redact-text "Giulia met Rossi at the port.")) "a listed word is masked wherever it stands whole")
  (is (equal "<PERSON> and Anna" (nodecode-pii:redact-text "giulia and Anna")) "case does not save it, and an unlisted name stays")
  (is (equal "Giuliana" (nodecode-pii:redact-text "Giuliana")) "a listed word inside a longer one is another word")))

(deftest pii-cell-an-overlap-is-one-placeholder (with-cell-stop ((pii-start))
  (is (equal "<CREDIT_CARD>." (nodecode-pii:redact-text "4111 1111 1111 1111.")) "the union of the spans is covered once")
  (is (equal "<IP_ADDRESS> and <EMAIL_ADDRESS>" (nodecode-pii:redact-text "10.0.0.1 and anna@example.it")) "two separate detections stay two")))

;;; --- the advice on the fold -----------------------------------------------------

(deftest pii-cell-redacts-the-wire-not-the-message (with-cell-stop ((pii-start))
  (let* ((system (pii-message "system" "write to anna@example.it"))
         (user (pii-message "user" "my mail is anna@example.it"))
         (assistant (pii-message "assistant" "noted anna@example.it"))
         (answer (multiple-value-list (pii-wire-answer (list system user assistant))))
         (redacted (first answer)))
    (is (eq :clean (second answer)) "the fold's clean flag comes back untouched")
    (is (equal "write to anna@example.it" (gethash "content" (first redacted))) "the system prompt is never read")
    (is (equal "my mail is <EMAIL_ADDRESS>" (gethash "content" (second redacted))) "the user's words are masked on the way out")
    (is (equal "noted anna@example.it" (gethash "content" (third redacted))) "the assistant's answer is left as it was")
    (is (equal "my mail is anna@example.it" (gethash "content" user)) "the folded message is untouched: the log keeps the original")
    (is (not (eq user (second redacted))) "what changed is a copy")))
)
(deftest pii-cell-a-content-array-is-redacted-part-by-part (with-cell-stop ((pii-start))
  (let* ((parts (vector (nlk:json-object "type" "text" "text" "call 055 1234567")
                        (nlk:json-object "type" "image" "text" "not a part")))
         (content (gethash "content" (first (pii-wire-answer (list (pii-message "user" parts)))))) )
    (is (equal "call <PHONE_NUMBER>" (gethash "text" (aref content 0))) "a text part is masked")
    (is (equal "not a part" (gethash "text" (aref content 1))) "a part that carries no words is left alone")))
)
(deftest pii-cell-a-broken-detector-fails-open (with-cell-stop ((pii-start))
  (with-stubbed-fdefinitions ((nodecode-pii::detect-hits (text &key entities terms)
                                  (declare (ignore text entities terms))
                                  (error "the detector broke")))
    (is (equal "anna@example.it" (nodecode-pii:redact-text "anna@example.it")) "a throw passes the text through"))
  (with-stubbed-fdefinitions ((nodecode-pii::redact-messages (messages)
                                  (declare (ignore messages))
                                  (error "the advice broke")))
    (let* ((messages (list (pii-message "user" "anna@example.it")))
           (answer (multiple-value-list (pii-wire-answer messages))))
      (is (eq messages (first answer)) "the fold's own answer survives")
      (is (eq :clean (second answer)) "and so does its second value"))))

;;; --- the tool and the command ---------------------------------------------------
)
(deftest pii-cell-the-tool-and-the-command-answer (with-cell-stop ((pii-start))
  (let ((answer (pii-tool-call "write to anna@example.it")))
    (is (search "<EMAIL_ADDRESS>" answer) "the tool answers the redacted text")
    (is (search "masked 1 email address" answer) "and says what it found"))
  (is (search "masked 1 name" (pii-tool-call "ask Giulia" "Giulia")) "a word named in the call is masked too")
  (is (search "nothing found" (pii-tool-call "nothing to see")) "and an empty result says so")
  (is (search "pii: masking email" (pii-slash "")) "the command answers its state")
  (is (search "<IP_ADDRESS>" (pii-slash "host 10.1.2.3")) "or previews any text")))
