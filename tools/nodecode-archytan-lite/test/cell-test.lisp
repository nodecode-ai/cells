;;;; cell-test.lisp --- the archytan-lite cell against the core's seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every policy is a file under the test home, every call is a form handed
;;;; to the installed :TOOL hook, and the journal is a file of the test's own:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "archytan-lite" "ARCHYTAN-LITE-CELL-" :start nodecode-archytan-lite:start-cell)

(define-cell-lifecycle-tests "archytan-lite"
  (:config "workspace" "/tmp/archytan-ws")
  (:help :archytan)
  (:command "archytan")
  (:running (is (search "policy" (archytan:status)) "status names the policy it reads"))
  (:refused ("ungated" 5) ("workspace" #(1)))
  (:idle nodecode-archytan-lite:archytan-lite-error
         (archytan:check "file.read" "/x")))

;;; --- fixtures ---------------------------------------------------------------------

(defvar *archytan-files* 0
  "How many files this slice has made, so two tests never share one.")

(defun archytan-test-file (text &optional (type "json"))
  "A fresh file under the test home holding TEXT; its namestring."
  (let* ((name (format nil "archytan-test/~a-~d.~a" type (incf *archytan-files*) type))
         (path (nlk:home name)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path text)
    (namestring path)))

(defparameter +grants-workspace+
  "{\"version\": 1, \"actions\": [
    {\"action\": \"file.read\",  \"grants\": [{\"resource_ids\": [\"/tmp/archytan-ws/**\"]}]},
    {\"action\": \"file.write\", \"grants\": [{\"resource_ids\": [\"/tmp/archytan-ws/**\"]}]},
    {\"action\": \"shell.run\",  \"grants\": [{\"resource_ids\": [\"ls\", \"git/status\"]}]},
    {\"action\": \"shell.env\",  \"grants\": [{\"resource_ids\": [\"*\"]}]},
    {\"action\": \"web.fetch\",  \"grants\": [{\"resource_ids\": [\"https/*\"]}]},
    {\"action\": \"tool.call\",  \"grants\": [{\"resource_ids\": [\"*\"]}]}]}"
  "A policy that grants ordinary work in /tmp/archytan-ws and nothing else.")

(defparameter +two-ls-a-minute+
  "{\"version\": 1, \"actions\": [
    {\"action\": \"shell.run\", \"grants\": [{\"resource_ids\": [\"ls\"]}],
     \"assertions\": {\"max_per_window\": 2, \"window_seconds\": 60}}]}"
  "A policy with one limited action, so a window can be spent.")

(defun archytan-start-with (&key (workspace "/tmp/archytan-ws") (policy +grants-workspace+)
                                 (journal "") (extra '()))
  "Start the cell with WORKSPACE, POLICY written fresh (NIL: a path with no
file behind it) and JOURNAL, then EXTRA config pairs; answers the stop thunk."
  (let ((policy-path (if policy
                         (archytan-test-file policy)
                         (namestring (nlk:home "archytan-test/absent.json"))))
        (journal-path (archytan-test-file journal "jsonl")))
    (apply #'archytan-lite-start
           (append (list "workspace" (or workspace "")
                         "policy" policy-path
                         "journal" journal-path)
                   extra))))

(defun archytan-verdict (form &rest strings)
  "The installed hook's answer for FORM with the strings object STRINGS
(name text ...): :ALLOWED, or the refusal string."
  (funcall (cell-hook "nodecode-archytan-lite" :tool)
           (list :name "eval"
                 :arguments (if strings
                                (nlk:json-object "form" form
                                                 "strings" (let ((object (nlk:json-object)))
                                                             (loop for (name text) on strings by #'cddr
                                                                   do (setf (gethash name object) text))
                                                             object))
                                (nlk:json-object "form" form))
                 :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

;;; --- what the policy grants ---------------------------------------------------------

(deftest archytan-lite-cell-allows-what-the-policy-grants (with-cell-stop ((archytan-start-with))
  (is (eq :allowed (archytan-verdict "(sh \"ls -la\")")) "a granted program runs")
  (is (eq :allowed (archytan-verdict "(write-file \"notes.md\" \"hello\")")) "a write in the workspace runs")
  (is (eq :allowed (archytan-verdict "(* 6 7)")) "a pure form is the one tool call it is")
  (is (eq :allowed (archytan-verdict "(progn (sh \"ls\") (write-file \"x.md\" \"y\"))")) "both checks pass")))

(deftest archytan-lite-cell-holds-a-call-the-policy-does-not-grant (with-cell-stop ((archytan-start-with))
  (let ((verdict (archytan-verdict "(sh \"git push origin main\")")))
    (is (stringp verdict) "an ungranted program holds the call")
    (is (search "shell.run" verdict) "naming the action")
    (is (search "git/push" verdict) "and the resource, subcommand and all")
    (is (search "did not run" verdict) "and that nothing ran"))
  (let ((verdict (archytan-verdict "(write-file \"/etc/passwd\" \"x\")")))
    (is (search "file.write" verdict) "a write outside the workspace is held")
    (is (search "/etc/passwd" verdict) "naming the path it resolved to"))
  (let ((verdict (archytan-verdict "(sh \"git status > /tmp/outside.txt\")")))
    (is (stringp verdict) "a redirect out of the workspace holds the call")
    (is (search "file.write" verdict) "naming the write")
    (is (not (search "git/status" verdict)) "though the program it runs is granted"))))

(deftest archytan-lite-cell-reads-a-command-the-strings-carry (with-cell-stop ((archytan-start-with))
  (let ((verdict (archytan-verdict "(sh ($ \"s\"))" "s" "git push origin main")))
    (is (search "git/push" verdict) "a ($ \"name\") command is read from the strings object"))
  (is (eq :allowed (archytan-verdict "(sh ($ \"s\"))" "s" "ls -la")))
  (let ((verdict (archytan-verdict "(write-file \"x\" ($ \"t\"))" "t" "sudo rm -rf /")))
    (is (eq :allowed verdict) "a strings entry no command names is the payload it is"))))

(deftest archytan-lite-cell-holds-a-command-the-form-does-not-show (with-cell-stop ((archytan-start-with))
  (let ((variable (archytan-verdict "(sh \"echo $HOME\")")))
    (is (search "unreadable-command" variable) "a $VAR is the shell's to read, not the gate's"))
  (let ((held (archytan-verdict "(let ((cmd \"ls\")) (sh cmd))")))
    (is (search "unreadable-command" held) "so is a command held in a variable"))
  (let ((heredoc (archytan-verdict "(sh \"cat <<EOF\")")))
    (is (search "unreadable-command" heredoc) "and a heredoc"))))

;;; --- fail closed ---------------------------------------------------------------------

(deftest archytan-lite-cell-holds-every-call-without-a-workspace (with-cell-stop ((archytan-start-with :workspace nil))
  (let ((verdict (archytan-verdict "(sh \"ls\")")))
    (is (stringp verdict) "nothing passes without a workspace")
    (is (search "no workspace" verdict) "and the refusal says which setting is missing"))))

(deftest archytan-lite-cell-holds-every-call-without-a-policy (with-cell-stop ((archytan-start-with :policy nil))
  (let ((verdict (archytan-verdict "(sh \"ls\")")))
    (is (search "no policy file" verdict) "a machine with no policy runs nothing")
    (is (search "/archytan setup" verdict) "the way out is a starter policy"))))

;;; --- the window, and the journal ------------------------------------------------------

(deftest archytan-lite-cell-spends-a-window (with-cell-stop ((archytan-start-with :policy +two-ls-a-minute+))
  (is (eq :allowed (archytan-verdict "(sh \"ls\")")) "the first use passes")
  (is (eq :allowed (archytan-verdict "(sh \"ls -la\")")) "the second too")
  (let ((verdict (archytan-verdict "(sh \"ls /tmp\")")))
    (is (stringp verdict) "the third is held")
    (is (search "2 shell.run in 60 seconds" verdict) "saying what the limit is"))))

(deftest archytan-lite-cell-journals-what-it-decided (with-cell-stop ((archytan-start-with))
  (archytan-verdict "(sh \"ls\")")
  (archytan-verdict "(sh \"git push\")")
  (let ((lines (archytan:decisions :limit 10)))
    (is (= 2 (length lines)) "one line per decision")
    (is (search "\"decision\":\"allow\"" (first lines)) "the allow first")
    (is (search "\"decision\":\"block\"" (second lines)) "the block after it")
    (is (search "\"resource\":\"git/push\"" (second lines)) "with the resource it named"))))

(deftest archytan-lite-cell-check-names-what-the-policy-would-do (with-cell-stop ((archytan-start-with))
  (let ((answer (archytan:check "shell.run" "ls")))
    (is (eq t (getf answer :allowed)) "a granted action is allowed")
    (is (equal "" (getf answer :reason))))
  (let ((answer (archytan:check "shell.run" "curl")))
    (is (null (getf answer :allowed)) "an ungranted one is held")
    (is (search "curl" (getf answer :reason)) "and the reason names it"))))

(deftest archytan-lite-cell-setup-writes-a-starter-policy (with-cell-stop ((archytan-start-with :policy nil))
  (let* ((entry (nle::find-registered-command "archytan"))
         (run (nle::slash-command-headless entry))
         (text (funcall run "setup" "s1"))
         (path (nodecode-archytan-lite::gate-policy-path
                (nodecode-archytan-lite::setting :gate))))
    (declare (ignore text))
    (is (probe-file path) "the starter policy is on disk")
    (is (search "\"file.write\"" (nlk:read-text path)) "and grants ordinary work in the workspace")
    (is (eq :allowed (archytan-verdict "(sh \"ls\")")) "which the next call follows")
    (is (search "kept" (funcall run "setup" "s1")) "a second setup keeps it"))))

(deftest archytan-lite-cell-command-answers-its-status (with-cell-stop ((archytan-start-with))
  (let* ((entry (nle::find-registered-command "archytan"))
         (run (nle::slash-command-headless entry))
         (text (funcall run "" "s1")))
    (is (search "archytan-lite" text) "the status names the cell")
    (is (search "/tmp/archytan-ws" text) "the workspace")
    (is (search "6 actions" text) "and what the policy grants"))))
