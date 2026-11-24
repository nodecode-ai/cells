;;;; cell-test.lisp --- lifecycle, the detectors, the call, the ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its own rules: a start
;;;; installs one hook on the call, one on the round, one tool, one command
;;;; and one manual topic, and a stop takes every bit of it back; a section
;;;; the declaration cannot read refuses and leaves nothing behind; a call
;;;; carrying a credential does not run in enforce mode and does in the other
;;;; three; the ledger keeps metadata and never the secret; plain prose is
;;;; left alone.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "secretshield"
  (:hooks :tool :round)
  (:command "secretshield")
  (:help :secretshield)
  (:running (is (nle::find-tool "shield_status") "the tool is registered")
            (is (equal "enforce" (nodecode-secretshield:setting :mode)) "the shield stops a call by default"))
  (:stopped (is (null (nle::find-tool "shield_status")) "stop unregisters the tool")
            (is (null (nle::find-registered-command "secretshield")) "and takes /secretshield out of the catalog"))
  (:refused ("mode" "shout") ("disabled_kinds" (vector "not-a-kind"))))

;;; --- what the scanner reads ----------------------------------------------------

(deftest secretshield-cell-finds-a-credential-and-not-prose (with-cell-stop ((secretshield-start))
  (let ((token (ss-openai-token)))
    (let ((findings (nodecode-secretshield:scan-text (format nil "token ~a here" token))))
      (is (= 1 (length findings)) "one token, one finding")
      (is (equal "openai_key" (nodecode-secretshield:finding-kind (first findings))) "named by its vendor")
      (is (= (length token) (nodecode-secretshield:finding-length (first findings))) "its length is kept")
      (let ((preview (nodecode-secretshield:finding-preview (first findings))))
        (is (equal (concatenate 'string (subseq token 0 4) "****") preview) "four characters, never the secret")))
    (is (null (nodecode-secretshield:scan-text "the build is 12345678, retrying in 5s")) "plain prose trips nothing"))))

(deftest secretshield-cell-reads-a-bearer-header-and-an-assignment (with-cell-stop ((secretshield-start))
  (let* ((body (ss-bearer-token))
         (header (format nil "curl -H 'Authorization: Bearer ~a' https://example.test" body))
         (assigned (format nil "password = ~a" (ss-fill ss-alnum 16 3))))
    (is (= 1 (length (nodecode-secretshield:scan-text header))) "a bearer credential in a shell command is a finding")
    (is (= 1 (length (nodecode-secretshield:scan-text assigned))) "so is a password assignment"))))

;;; --- the call -------------------------------------------------------------------

(deftest secretshield-cell-stops-a-call-carrying-a-credential (with-cell-stop ((secretshield-start))
  (ss-forget-ledger)
  (multiple-value-bind (answer seen called)
      (ss-call (ss-command (format nil "curl -H 'x: ~a' https://example.test" (ss-openai-token))))
    (declare (ignore seen))
    (is (stringp answer) "the model reads a refusal in the call's place")
    (is (search "blocked by SecretShield" answer) "saying what stopped it")
    (is (search "openai_key" answer) "and the kind it found")
    (is (not called) "and the tool never ran"))))

(deftest secretshield-cell-lets-an-ordinary-call-through (with-cell-stop ((secretshield-start))
  (multiple-value-bind (answer seen called)
      (ss-call (ss-command "ls -la src"))
    (is (eq :ran answer) "the call's own answer comes back")
    (is called "the tool ran")
    (is (equal "ls -la src" (gethash "command" seen)) "with its arguments untouched"))))

(deftest secretshield-cell-redacts-in-redact-mode (with-cell-stop ((secretshield-start "mode" "redact"))
  (ss-forget-ledger)
  (let ((token (ss-openai-token)))
    (multiple-value-bind (answer seen called)
        (ss-call (ss-command (format nil "curl -H 'x: ~a' https://example.test" token)))
      (is called "the call runs")
      (is (eq :ran answer) "and answers as it would")
      (let ((command (gethash "command" seen)))
        (is (not (search token command)) "the credential is gone from the arguments")
        (is (search "[REDACTED by SecretShield]" command) "and a mask stands where it was"))))))

(deftest secretshield-cell-warns-without-changing (with-cell-stop ((secretshield-start "mode" "warn"))
  (ss-forget-ledger)
  (let ((token (ss-openai-token)))
    (multiple-value-bind (answer seen called)
        (ss-call (ss-command (format nil "echo ~a" token)))
      (is called "the call runs")
      (is (eq :ran answer) "untouched")
      (is (search token (gethash "command" seen)) "the credential is still in the arguments")))))

;;; --- the ledger -----------------------------------------------------------------

(deftest secretshield-cell-ledger-keeps-metadata-not-the-secret (with-cell-stop ((secretshield-start))
  (ss-forget-ledger)
  (let* ((token (ss-openai-token))
         (result (nth-value 0 (ss-call (ss-command (format nil "echo ~a" token)))))
         (text (with-open-file (in (ss-ledger-file) :external-format :utf-8)
                 (let ((buffer (make-string (file-length in))))
                   (subseq buffer 0 (read-sequence buffer in))))))
    (declare (ignore result))
    (is (plusp (length text)) "the incident is on disk")
    (is (search "blocked" text) "saying what was done")
    (is (search "openai_key" text) "and which kind was found")
    (is (not (search token text)) "and never the credential itself")
    (let ((rows (nodecode-secretshield:recent-incidents)))
      (is (= 1 (length rows)) "the ledger reads back as one incident")))))

(deftest secretshield-cell-status-and-command-answer (with-cell-stop ((secretshield-start))
  (ss-forget-ledger)
  (ss-call (ss-command (format nil "echo ~a" (ss-openai-token))))
  (let ((status (ss-status)))
    (is (search "\"incidents_recent\":1" status) "shield_status counts what was recorded")
    (is (search "\"mode\":\"enforce\"" status) "and names the mode"))
  (is (search "blocked" (ss-slash "")) "/secretshield reads the ledger")
  (let ((preview (ss-slash (format nil "token ~a" (ss-openai-token)))))
    (is (search "[REDACTED by SecretShield]" preview) "/secretshield TEXT shows the text as it would leave")
    (is (search "found 1 credential" preview) "and counts what it found")))
)
