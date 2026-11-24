;;;; support.lisp --- the permission-canvas test runner, and its one scratch folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a PERMISSION-CANVAS name
;;;; prefix; RUN-PERMISSION-CANVAS-TESTS runs exactly that slice. Nothing here
;;;; dials anything: the cell reads no network and no key, so the only fixture
;;;; is a scratch folder under this run's own home for the policy, the
;;;; approvals and the audit lines.

(in-package #:nodecode.test)

(define-test-slice "permission-canvas" "PERMISSION-CANVAS" :start nodecode-permission-canvas:start-cell)

(defun pc-home ()
  "The scratch folder this run's policy lives in."
  (nlk:home "permission-canvas/"))

(defun pc-dir ()
  "That folder as the string a section's dir takes."
  (namestring (ensure-directories-exist (pc-home))))

(defun pc-path (name)
  (merge-pathnames name (ensure-directories-exist (pc-home))))

(defun pc-clear ()
  "Nothing of the last test is left: no policy, no approval, no audit line."
  (dolist (name '("policy.json" "approvals.json" "audit.jsonl"))
    (let ((path (pc-path name)))
      (when (probe-file path) (ignore-errors (delete-file path))))))

(defun pc-write (name text)
  "TEXT written as NAME in the scratch folder: => its namestring."
  (let ((path (pc-path name)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))
    (namestring path)))

(defun pc-read (name)
  (let ((path (pc-path name)))
    (and (probe-file path) (uiop:read-file-string path))))

(defparameter +policy+
  "{\"default_level\":\"yellow\",\"unidentified_level\":\"yellow\",\"require_target\":false,
    \"paused\":false,\"blocked_apps\":[],
    \"rules\":
     [{\"id\":\"r1\",\"level\":\"green\",\"app\":\"notes.exe\",\"role\":\"Button\",\"label\":\"Save\"},
      {\"id\":\"r2\",\"level\":\"blue\",\"role\":\"Edit\",\"label\":\"Search\"},
      {\"id\":\"r3\",\"level\":\"black\",\"role\":\"StaticText\",\"label\":\"API key\"},
      {\"id\":\"r4\",\"level\":\"black\",\"app\":\"vault.exe\"}]}"
  "The policy the tests decide against: one control of each level, and a
process-wide hide.")

(defun text (answer)
  "ANSWER's text: a command's own line, or a wire response's message content."
  (if (stringp answer) answer (nle:message-content answer)))

(defun pc-text (answer)
  "ANSWER as the text a test reads: a command's own line, or a wire response's
message content."
  (if (stringp answer) answer (nle:message-content answer)))

(defun pc-policy ()
  (nodecode-permission-canvas:policy-load))

(defun pc-decide (action element &optional arguments window-title)
  "DECIDE-ACTION against the policy on disk, the way a tool call reaches it."
  (nodecode-permission-canvas:decide-action
   (pc-policy) (nodecode-permission-canvas:ledger-load)
   action element arguments window-title))

(defun pc-index (pairs)
  "A PLIST as the controls an observation carries, in the order given."
  (mapcar (lambda (pair) (apply #'nodecode-permission-canvas:control pair)) pairs))

(defun pc-save-button ()
  (nodecode-permission-canvas:control 1 :app "notes.exe" :role "Button" :label "Save"
                                       :pid 9001 :window-id 31 :bounds '(10 10 100 30)))

(defun pc-search-field ()
  (nodecode-permission-canvas:control 2 :app "notes.exe" :role "Edit" :label "Search"
                                       :pid 9001 :window-id 31 :bounds '(10 50 200 24)))
