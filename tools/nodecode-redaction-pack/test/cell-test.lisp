;;;; cell-test.lisp --- lifecycle, the matcher, the advice, the tool and the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its own rules: a start
;;;; installs one advice, one tool, one command and one manual topic and a stop
;;;; takes them back, a section the declaration cannot read refuses and leaves
;;;; nothing behind, the formats mask what they say they mask and nothing else,
;;;; a token glued to a word is no token, the advice masks user and tool text
;;;; only and never the folded message itself, it hands the fold's second value
;;;; back, and a matcher that throws passes the text through instead of taking
;;;; the request with it.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "redaction-pack"
  (:hooks 'nodecode.evolved::wire-messages-from-events)
  (:command "redaction-pack")
  (:help :redaction-pack)
  (:running (is (nle::find-tool "redact_credentials") "the tool is registered")
            (is (equal "ends" (nodecode-redaction-pack:setting :style)) "the ordinary style is the default")
            (is (= 71 (length nodecode-redaction-pack::*active-formats*)) "every format in the table is masked"))
  (:stopped (is (null (nle::find-tool "redact_credentials")) "stop unregisters the tool")
            (is (null (rp-wire-hook)) "and takes the advice off the fold")
            (is (null (nle::find-registered-command "redaction-pack")) "and the command"))
  (:refused ("vendors" (vector "Mailgun")) ("style" "shout")))

;;; --- the matcher ---------------------------------------------------------------

(deftest redaction-pack-cell-masks-a-vendor-token (with-cell-stop ((redaction-pack-start))
  (let* ((token (rp-vault-token))
         (text (format nil "deploy failed: 401 for ~a (retrying)" token))
         (masked (nodecode-redaction-pack:redact-text text)))
    (is (not (search token masked)) "the token itself is gone")
    (is (search (subseq token 0 6) masked) "its first six characters stay, so a person can tell which one it was")
    (is (search "..." masked) "the middle is the mask")
    (is (not (equal text masked)) "and the answer is not the text"))))

(deftest redaction-pack-cell-masks-every-token-in-a-text (with-cell-stop ((redaction-pack-start))
  (let* ((vault (rp-vault-token))
         (cloudflare (rp-cloudflare-token))
         (shopify (rp-shopify-token))
         (masked (nodecode-redaction-pack:redact-text (format nil "~a ~a ~a" vault cloudflare shopify))))
    (is (not (search cloudflare masked)) "a Cloudflare key is masked too")
    (is (not (search shopify masked)) "and a Shopify token")
    (is (= 3 (ppcre:count-matches "\\.\\.\\." masked)) "one mask per token")
    (is (search (format nil "~a...~a" (subseq cloudflare 0 6) (subseq cloudflare (- (length cloudflare) 4)))
                masked) "a key of the cfk_ format keeps its first six and last four"))))

(deftest redaction-pack-cell-names-what-it-found (with-cell-stop ((redaction-pack-start))
  (let ((answer (rp-tool-call (format nil "vault ~a and doppler dp.pt.~a" (rp-vault-token) (rp-fill rp-alnum 43 2)))))
    (is (search "masked 2 credentials" answer) "the tool counts what it masked")
    (is (search "HashiCorp Vault service token" answer) "and names the vendor and the token")
    (is (search "Doppler CLI" answer) "for each of them"))
  (is (search "nothing found" (rp-tool-call "the build is 12345678")) "an empty result says so")))

(deftest redaction-pack-cell-leaves-what-is-not-a-token (with-cell-stop ((redaction-pack-start))
  (let ((token (rp-vault-token)))
    (is (equal (format nil "x~a" token) (nodecode-redaction-pack:redact-text (format nil "x~a" token))) "a token glued to a word character is part of that word")
    (let ((short (concatenate 'string "hvs." (rp-fill rp-urlsafe 89))))
      (is (equal short (nodecode-redaction-pack:redact-text short)) "a body one character short of the minimum is no token")))
  (is (equal "no credentials here at all" (nodecode-redaction-pack:redact-text "no credentials here at all")) "plain prose is left whole")))

(deftest redaction-pack-cell-a-sentinel-names-the-vendor (with-cell-stop ((redaction-pack-start))
  (let ((answer (rp-tool-call (format nil "token ~a" (rp-vault-token)) "sentinel")))
    (is (search "<<redacted: HashiCorp Vault service token>>" answer) "the sentinel names the vendor and the token")
    (is (not (search "hvs." answer)) "and carries no character of the token"))))

(deftest redaction-pack-cell-the-setting-narrows-the-vendors (with-cell-stop ((redaction-pack-start "vendors" (vector "hashicorp vault")))
  (is (equal '("HashiCorp Vault") (nodecode-redaction-pack:setting :vendors)) "a name is read as the table spells it")
  (is (not (search (rp-vault-token) (nodecode-redaction-pack:redact-text (format nil "v ~a" (rp-vault-token))))) "a vendor the setting names is masked")
  (let ((key (rp-cloudflare-token)))
    (is (equal (format nil "c ~a" key) (nodecode-redaction-pack:redact-text (format nil "c ~a" key))) "and a vendor it does not is left alone"))))

;;; --- the advice on the fold -----------------------------------------------------

(deftest redaction-pack-cell-masks-the-wire-not-the-log (with-cell-stop ((redaction-pack-start))
  (let* ((token (rp-vault-token))
         (system (rp-message "system" (format nil "vault token ~a" token)))
         (user (rp-message "user" (format nil "my token is ~a" token)))
         (assistant (rp-message "assistant" (format nil "noted ~a" token)))
         (tool (rp-message "tool" (format nil "stdout: ~a" token)))
         (answer (multiple-value-list (rp-wire-answer (list system user assistant tool))))
         (masked (first answer)))
    (is (eq :clean (second answer)) "the fold's clean flag comes back untouched")
    (is (search token (gethash "content" (first masked))) "the system prompt is never read")
    (is (search (subseq token 0 6) (gethash "content" (second masked))) "the user's own text is masked")
    (is (search token (gethash "content" (third masked))) "the assistant's answer is left as it was")
    (is (search "<<redacted: HashiCorp Vault service token>>" (gethash "content" (fourth masked))) "a tool result gets the sentinel, so it cannot be copied back")
    (is (search token (gethash "content" tool)) "the folded message is untouched: the log keeps the original")
    (is (not (eq tool (fourth masked))) "what changed is a copy"))))

(deftest redaction-pack-cell-a-content-array-is-masked-part-by-part (with-cell-stop ((redaction-pack-start))
  (let* ((parts (vector (nlk:json-object "type" "text" "text" (format nil "token ~a" (rp-vault-token)))
                        (nlk:json-object "type" "image" "text" "not a part")))
         (content (gethash "content" (first (rp-wire-answer (list (rp-message "user" parts)))))))
    (is (search "..." (gethash "text" (aref content 0))) "a text part is masked")
    (is (equal "not a part" (gethash "text" (aref content 1))) "a part that carries no words is left alone"))))

(deftest redaction-pack-cell-a-broken-matcher-fails-open (with-cell-stop ((redaction-pack-start))
  (with-stubbed-fdefinitions ((nodecode-redaction-pack::find-spans (text &optional formats)
                                  (declare (ignore text formats))
                                  (error "the matcher broke")))
    (is (equal "hvs.abc" (nodecode-redaction-pack:redact-text "hvs.abc")) "a throw passes the text through"))
  (with-stubbed-fdefinitions ((nodecode-redaction-pack::redact-messages (messages)
                                  (declare (ignore messages))
                                  (error "the advice broke")))
    (let* ((token (rp-vault-token))
           (messages (list (rp-message "user" token)))
           (answer (multiple-value-list (rp-wire-answer messages))))
      (is (eq messages (first answer)) "the fold's own answer survives")
      (is (eq :clean (second answer)) "and so does its second value")))))

;;; --- the command ----------------------------------------------------------------

(deftest redaction-pack-cell-the-command-answers (with-cell-stop ((redaction-pack-start))
  (is (search "71 format" (rp-slash "")) "with nothing typed it says how many formats it masks")
  (is (search "ends style" (rp-slash "")) "and how a mask reads")
  (is (search (subseq (rp-vault-token) 0 6) (rp-slash (format nil "token ~a" (rp-vault-token)))) "with text it previews what would leave")))
