;;;; cell-test.lisp --- the rewrite, the shapes it leaves alone, and the seam.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The rtk binary is stubbed: a short shell script written into a temporary
;;;; folder answers the protocol's codes -- 3 an accepted rewrite, 1 a
;;;; passthrough, 7 something unexpected -- and sleeps for one command so the
;;;; timeout path is exercised. Nothing here touches the network.

(in-package #:nodecode.test)

(defparameter *rtk-backends-stub-script*
  "#!/bin/sh

# A stand-in for the rtk binary: the answers the original's protocol names,
# and a slow one for the timeout.
case \"$1\" in
  --version) echo \"rtk 0.49.0-stub\"; exit 0 ;;
  rewrite)
    case \"$2\" in
      *slow*) sleep 5; printf 'rtk %s\\n' \"$2\"; exit 3 ;;
      *plain*) exit 1 ;;
      *broken*) exit 7 ;;
      *empty*) exit 3 ;;
      *same*) printf '%s\\n' \"$2\"; exit 3 ;;
      *) printf 'rtk %s\\n' \"$2\"; exit 3 ;;
    esac ;;
  *) exit 2 ;;
esac
"
  "The stand-in binary's text.")

(defvar *rtk-backends-stub-path* nil
  "Where the stand-in was written, once per run.")

(defun rtk-backends-stub ()
  "The path of the stand-in binary, written and made executable once."
  (or *rtk-backends-stub-path*
      (let* ((folder (merge-pathnames (format nil "rtk-backends-cell-~d/" (random 1000000))
                                       (uiop:temporary-directory)))
             (file (merge-pathnames "rtk" folder)))
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output :if-exists :supersede)
          (write-string *rtk-backends-stub-script* out))
        (uiop:run-program (list "chmod" "0755" (uiop:native-namestring file)))
        (setf *rtk-backends-stub-path* (uiop:native-namestring file)))))

(defun rtk-backends-verdict (name arguments)
  "Run the installed hook on one call, as the :TOOL chain runs it, and answer
the arguments it handed NEXT."
  (let ((seen nil))
    (funcall (cell-hook "nodecode-rtk-backends" :tool)
             (list :name name :arguments arguments :call-id "c1")
             (lambda (op) (setf seen (getf op :arguments)) "ran"))
    seen))

(defun rtk-backends-form (arguments)
  (gethash "form" arguments))

(defun rtk-backends-command (arguments)
  (gethash "command" arguments))

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "rtk-backends"
  (:config "binary" (rtk-backends-stub))
  (:hooks :tool)
  (:command "rtk-backends")
  (:help :rtk-backends)
  (:refused ("timeout_seconds" 0))
  (:running (is (= 2 (nodecode-rtk-backends::setting :timeout-seconds)) "the declared defaults are what it runs with")
            (is (string= (rtk-backends-stub) (nodecode-rtk-backends::setting :binary)) "and the section's binary")))

;;; --- the rewrite ---------------------------------------------------------------

(deftest rtk-backends-cell-rewrites-an-eval-shell-call (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let ((arguments (nlk:json-object "form" (format nil ";; what changed~%(sh \"git status\")"))))
    (rtk-backends-verdict "eval" arguments)
    (is (equal (format nil ";; what changed~%(sh \"rtk git status\")") (rtk-backends-form arguments)) "the command is rewritten and the snippet's note is kept")))

(deftest rtk-backends-cell-rewrites-every-shell-call-in-a-snippet (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let ((arguments (nlk:json-object "form" (format nil "~a~%(sh \"ls -la\")~%(+ 1 2)~%(sh \"git status\")"
                                                          ";; two commands"))))
    (rtk-backends-verdict "eval" arguments)
    (is (equal (format nil "~a~%(sh \"rtk ls -la\")~%(+ 1 2)~%(sh \"rtk git status\")"
                       ";; two commands")
               (rtk-backends-form arguments)) "each shell call is rewritten, and the form between them is not")))

(deftest rtk-backends-cell-keeps-a-quoted-command-readable (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let ((arguments (nlk:json-object "form" "(sh \"echo \\\"hi there\\\"\")")))
    (rtk-backends-verdict "eval" arguments)
    (is (equal "(sh \"rtk echo \\\"hi there\\\"\")" (rtk-backends-form arguments)) "the command's own quotes survive the round trip")
    (is (consp (read-from-string (rtk-backends-form arguments))) "and the snippet still reads")))

(deftest rtk-backends-cell-rewrites-a-strings-entry (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let* ((strings (nlk:json-object "cmd" "git status"))
         (arguments (nlk:json-object "form" "(sh ($ \"cmd\"))" "strings" strings))
         (form (rtk-backends-form arguments)))
    (rtk-backends-verdict "eval" arguments)
    (is (equal "rtk git status" (gethash "cmd" strings)) "the strings entry is rewritten")
    (is (equal form (rtk-backends-form arguments)) "and the form text is untouched")))

(deftest rtk-backends-cell-rewrites-a-shell-tools-command (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-backends-verdict "terminal" arguments)
    (is (equal "rtk git status" (rtk-backends-command arguments)) "a terminal tool's command is rewritten"))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-backends-verdict "write-file" arguments)
    (is (equal "git status" (rtk-backends-command arguments)) "a tool of another name is not")))

(deftest rtk-backends-cell-honours-extra-tools (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub) "extra_tools" (vector "mine"))))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-backends-verdict "mine" arguments)
    (is (equal "rtk git status" (rtk-backends-command arguments)) "a configured tool name is rewritten too")))

;;; --- what it leaves alone ---------------------------------------------------------

(deftest rtk-backends-cell-leaves-alone-what-rtk-leaves-alone (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (dolist (command '("plain build" "broken build" "empty build" "same build"))
    (let* ((form (format nil "(sh \"~a\")" command))
           (arguments (nlk:json-object "form" form)))
      (rtk-backends-verdict "eval" arguments)
      (is (equal form (rtk-backends-form arguments)) (format nil "~s comes back as the model wrote it" command))))
  (let ((arguments (nlk:json-object "form" "(reduce #'+ (list 1 2 3))")))
    (rtk-backends-verdict "eval" arguments)
    (is (equal "(reduce #'+ (list 1 2 3))" (rtk-backends-form arguments)) "a snippet that is no shell call is not read")))

(deftest rtk-backends-cell-leaves-whole-when-nothing-answers (with-cell-stop ((rtk-backends-start "binary" "/nonexistent/rtk-not-here")))
  (let* ((form "(sh \"git status\")")
         (arguments (nlk:json-object "form" form)))
    (rtk-backends-verdict "eval" arguments)
    (is (equal form (rtk-backends-form arguments)) "a missing binary is no rewrite, never an error")))

(deftest rtk-backends-cell-leaves-whole-past-the-timeout (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub) "timeout_seconds" 1)))
  (let* ((form "(sh \"slow build\")")
         (arguments (nlk:json-object "form" form)))
    (rtk-backends-verdict "eval" arguments)
    (is (equal form (rtk-backends-form arguments)) "a rewrite past timeout_seconds leaves the command as written")))

(deftest rtk-backends-cell-leaves-eval-alone-when-told-to (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub) "rewrite_eval" nil)))
  (let* ((form "(sh \"git status\")")
         (arguments (nlk:json-object "form" form)))
    (rtk-backends-verdict "eval" arguments)
    (is (equal form (rtk-backends-form arguments)) "rewrite_eval off leaves snippets alone"))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-backends-verdict "terminal" arguments)
    (is (equal "rtk git status" (rtk-backends-command arguments)) "and a shell tool's command is still rewritten")))

;;; --- the report ------------------------------------------------------------------

(deftest rtk-backends-cell-report-counts-what-it-rewrote (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (rtk-backends-verdict "eval" (nlk:json-object "form" "(sh \"git status\")"))
  (rtk-backends-verdict "eval" (nlk:json-object "form" "(sh \"plain build\")"))
  (let ((first (cell-entry "nodecode-rtk-backends" "rtk-backends" ""))
        (check (cell-entry "nodecode-rtk-backends" "rtk-backends" "check"))
        (reset (cell-entry "nodecode-rtk-backends" "rtk-backends" "reset"))
        (after (cell-entry "nodecode-rtk-backends" "rtk-backends" "")))
    (is-carrying first ("rewrote 1 of 2 commands" "what it rewrote of what it saw")
      ("1 left as written" "and what rtk left alone"))
    (is-carrying check ("answers" "check reports the binary"))
    (is-carrying reset ("counts reset" "reset zeroes them"))
    (is-carrying after ("rewrote 0 of 0" "and the next report starts over"))))

(deftest rtk-backends-cell-report-refuses-when-not-running ()
  (is (signals-error nodecode-rtk-backends::rtk-backends-error (nodecode-rtk-backends:report "" "s1"))))

(deftest rtk-backends-cell-hook-is-a-no-op-once-stopped (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let ((hook (cell-hook "nodecode-rtk-backends" :tool)))
    (funcall stop)
    (setf stop nil)
    (let* ((form "(sh \"git status\")")
           (arguments (nlk:json-object "form" form)))
      (funcall hook (list :name "eval" :arguments arguments :call-id "c1")
               (lambda (op) (declare (ignore op)) "ran"))
      (is (equal form (rtk-backends-form arguments)) "with the cell stopped, nothing is rewritten"))))

;;; --- the backend gate --------------------------------------------------------

(deftest rtk-backends-cell-reads-a-commands-backend ()
  (dolist (case '(("git status" . "local")
                  ("ls -la" . "local")
                  ("docker ps" . "docker")
                  ("/usr/bin/docker run -it box bash" . "docker")
                  ("sudo -n docker ps" . "docker")
                  ("env FOO=bar docker ps" . "docker")
                  ("PATH=/usr/bin git status" . "local")
                  ("podman-compose up" . "podman")
                  ("kubectl get pods" . "kubectl")
                  ("ssh box uptime" . "ssh")
                  ("" . "local")))
    (is (equal (cdr case) (nodecode-rtk-backends::command-backend (car case)))
        (format nil "~s runs in ~a" (car case) (cdr case)))))

(deftest rtk-backends-cell-leaves-a-backend-nobody-opted-in (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let* ((form "(sh \"docker ps\")")
         (arguments (nlk:json-object "form" form)))
    (rtk-backends-verdict "eval" arguments)
    (is (equal form (rtk-backends-form arguments)) "a docker command is not rewritten while only local is opted in"))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-backends-verdict "terminal" arguments)
    (is (equal "rtk git status" (rtk-backends-command arguments)) "a local command still is")))

(deftest rtk-backends-cell-rewrites-an-opted-in-backend (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub) "backends" (vector "local" "docker"))))
  (let ((arguments (nlk:json-object "command" "docker ps")))
    (rtk-backends-verdict "terminal" arguments)
    (is (equal "rtk docker ps" (rtk-backends-command arguments)) "opting docker in rewrites its commands"))
  (let ((arguments (nlk:json-object "command" "kubectl get pods")))
    (rtk-backends-verdict "terminal" arguments)
    (is (equal "kubectl get pods" (rtk-backends-command arguments)) "and leaves the backends not named")))

(deftest rtk-backends-cell-all-opts-every-backend-in (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub) "backends" (vector "all"))))
  (dolist (command '("git status" "docker ps" "ssh box uptime"))
    (let ((arguments (nlk:json-object "command" command)))
      (rtk-backends-verdict "terminal" arguments)
      (is (equal (format nil "rtk ~a" command) (rtk-backends-command arguments))
          (format nil "~s is rewritten with backends all" command)))))

(deftest rtk-backends-cell-report-counts-the-skipped (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (rtk-backends-verdict "terminal" (nlk:json-object "command" "git status"))
  (rtk-backends-verdict "terminal" (nlk:json-object "command" "docker ps"))
  (let ((line (cell-entry "nodecode-rtk-backends" "rtk-backends" "")))
    (is-carrying line
      ("1 in a backend not opted in" "the skipped command is counted")
      ("rewrote 1 of 2 commands" "and the rewritten one")
      ("backends local" "the backends it runs for"))))

(deftest rtk-backends-cell-report-answers-about-a-backend (with-cell-stop ((rtk-backends-start "binary" (rtk-backends-stub))))
  (let ((local (cell-entry "nodecode-rtk-backends" "rtk-backends" "local"))
        (docker (cell-entry "nodecode-rtk-backends" "rtk-backends" "docker")))
    (is-carrying local ("local is opted in" "a backend the config names"))
    (is-carrying docker ("docker is not opted in" "one it does not"))))
