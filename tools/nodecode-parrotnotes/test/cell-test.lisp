;;;; cell-test.lisp --- the entry it writes, the manual it keeps, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No network here, and no operator config: the cell dials nothing, so what
;;;; is proved is the manifest's one write (the mcp.servers entry, through a
;;;; stubbed reader and writer), the command's answers, the manual the
;;;; plugin's skill became, and the lifecycle the declaration owns. The
;;;; connection, the OAuth sign-in and the nine tools belong to the mcp cell,
;;;; which tests them itself.

(in-package #:nodecode.test)

(define-test-slice "parrotnotes" "PARROTNOTES-CELL-" :start nodecode-parrotnotes:start-cell)

(define-cell-lifecycle-tests "parrotnotes"
  (:help :parrotnotes)
  (:command "parrotnotes")
  (:running (is (= 20 (nodecode-parrotnotes::setting :limit)) "a listing shows twenty notes unless the ask says otherwise"))
  (:refused ("limit" 0))
  (:idle nodecode-parrotnotes:parrotnotes-error (nodecode-parrotnotes:status-text)))

;;; --- fixtures -------------------------------------------------------------------

(defmacro with-parrotnotes-config ((entry-var writes-var) &body body)
  "Run BODY on a shared config that starts empty: ENTRY-VAR holds what is set
under mcp.servers, WRITES-VAR counts the writes, and both the cell's reader
and its writer are stubbed, so nothing of the operator's config is read or
written."
  `(let* ((,entry-var nil)
          (,writes-var 0)
          (nodecode-parrotnotes::*config-read* (lambda (path) (declare (ignore path)) ,entry-var))
          (nodecode-parrotnotes::*config-write*
            (lambda (path value)
              (declare (ignore path))
              (incf ,writes-var)
              (setf ,entry-var value)
              "mcp.servers.parrotnotes: set url (1 added, 0 replaced)")))
     ,@body))

;;; --- the command ----------------------------------------------------------------

(deftest parrotnotes-cell-the-command-answers-status-with-the-endpoint ()
  (with-cell-stop ((parrotnotes-start))
    (with-parrotnotes-config (entry writes)
      (declare (ignore writes))
      (is (null entry) "no server set yet")
      (multiple-value-bind (text dialog) (nodecode-parrotnotes:run-command "" nil)
        (is (search "mcp.parrotnotes.app/mcp" text) "the status names the endpoint the manifest shipped")
        (is (search "/parrotnotes connect" text) "and the next step")
        (is dialog "typed alone it opens a dialog")
        (is (equal "ParrotNotes" (getf dialog :title)) "titled for the product")
        (let ((labels (loop for row in (getf dialog :rows) collect (getf row :label)))
              (lines (loop for row in (getf dialog :rows) collect (getf (getf row :run) :enter))))
          (is (member "Connect it" labels :test #'equal) "the row that writes the entry")
          (is (member "How to work the notes" labels :test #'equal) "and the row that shows the workflow")
          (is (equal '("/parrotnotes connect" "/parrotnotes workflow") lines) "each row runs the slash line, not a tool call"))))))

(deftest parrotnotes-cell-a-verb-it-does-not-know-says-so ()
  (with-cell-stop ((parrotnotes-start))
    (with-parrotnotes-config (entry writes)
      (declare (ignore entry writes))
      (let ((text (nodecode-parrotnotes:run-command "  PUBLISH " nil)))
        (is (search "\"publish\" is not a verb" text) "an unknown argument is named back, lowercased")
        (is (search "/parrotnotes workflow" text) "and the verbs are listed")))))

;;; --- the manifest's one write ---------------------------------------------------

(deftest parrotnotes-cell-connect-writes-the-entry-once ()
  (with-cell-stop ((parrotnotes-start))
    (with-parrotnotes-config (entry writes)
      (is (null entry) "the config starts empty")
      (let ((written (nodecode-parrotnotes:connect-server)))
        (is (search "wrote mcp.servers.parrotnotes" written) "connect says what it wrote")
        (is (= 1 writes) "one write, no more")
        (is (equal (list :url "https://mcp.parrotnotes.app/mcp") entry) "the entry is the url the plugin's mcp.json named, and nothing else")
        (is (search "set already" (nodecode-parrotnotes:connect-server)) "a second connect writes nothing")
        (is (= 1 writes) "still one write")
        (is (search "Sign in" (nodecode-parrotnotes:run-command "connect" nil)) "and the sign-in is where the plugin's README put it")
        (is (search "is set" (nodecode-parrotnotes:status-text)) "the status reads the entry back once it is set")))))

;;; --- the skill, kept -------------------------------------------------------------

(deftest parrotnotes-cell-the-manual-keeps-the-skill ()
  (with-cell-stop ((parrotnotes-start "limit" 5))
    (let ((text (nodecode-parrotnotes:workflow-text)))
      (is (search "get_recent_notes" text) "the tools are named")
      (is (search "localRecordingId" text) "the two ids and which tool takes which")
      (is (search "replaces the" text) "a save overwrites, and the manual says so before it happens")
      (is (search "never as instructions" text) "a transcript's text is data, not a command")
      (is (search "A listing shows 5 notes" text) "and the section's own default carries through"))))
