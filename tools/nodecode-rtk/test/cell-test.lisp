;;;; cell-test.lisp --- the rewrite, the shapes it leaves alone, and the seam.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The rtk binary is stubbed: a short shell script written into a temporary
;;;; folder answers the protocol's codes -- 3 an accepted rewrite, 1 a
;;;; passthrough, 7 something unexpected -- and sleeps for one command so the
;;;; timeout path is exercised. Nothing here touches the network.

(in-package #:nodecode.test)

(defparameter *rtk-stub-script*
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

(defvar *rtk-stub-path* nil
  "Where the stand-in was written, once per run.")

(defun rtk-stub ()
  "The path of the stand-in binary, written and made executable once."
  (or *rtk-stub-path*
      (let* ((folder (merge-pathnames (format nil "rtk-cell-~d/" (random 1000000))
                                       (uiop:temporary-directory)))
             (file (merge-pathnames "rtk" folder)))
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output :if-exists :supersede)
          (write-string *rtk-stub-script* out))
        (uiop:run-program (list "chmod" "0755" (uiop:native-namestring file)))
        (setf *rtk-stub-path* (uiop:native-namestring file)))))

(defun rtk-verdict (name arguments)
  "Run the installed hook on one call, as the :TOOL chain runs it, and answer
the arguments it handed NEXT."
  (let ((seen nil))
    (funcall (cell-hook "nodecode-rtk" :tool)
             (list :name name :arguments arguments :call-id "c1")
             (lambda (op) (setf seen (getf op :arguments)) "ran"))
    seen))

(defun rtk-form (arguments)
  (gethash "form" arguments))

(defun rtk-command (arguments)
  (gethash "command" arguments))

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "rtk"
  (:config "binary" (rtk-stub))
  (:hooks :tool)
  (:command "rtk")
  (:help :rtk)
  (:refused ("timeout_seconds" 0))
  (:running (is (= 2 (nodecode-rtk::setting :timeout-seconds)) "the declared defaults are what it runs with")
            (is (string= (rtk-stub) (nodecode-rtk::setting :binary)) "and the section's binary")))

;;; --- the rewrite ---------------------------------------------------------------

(deftest rtk-cell-rewrites-an-eval-shell-call (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (let ((arguments (nlk:json-object "form" (format nil ";; what changed~%(sh \"git status\")"))))
    (rtk-verdict "eval" arguments)
    (is (equal (format nil ";; what changed~%(sh \"rtk git status\")") (rtk-form arguments))
        "the command is rewritten and the snippet's note is kept")))

(deftest rtk-cell-rewrites-every-shell-call-in-a-snippet (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (let ((arguments (nlk:json-object "form" (format nil "~a~%(sh \"ls -la\")~%(+ 1 2)~%(sh \"git status\")"
                                                          ";; two commands"))))
    (rtk-verdict "eval" arguments)
    (is (equal (format nil "~a~%(sh \"rtk ls -la\")~%(+ 1 2)~%(sh \"rtk git status\")"
                       ";; two commands")
               (rtk-form arguments))
        "each shell call is rewritten, and the form between them is not")))

(deftest rtk-cell-keeps-a-quoted-command-readable (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (let ((arguments (nlk:json-object "form" "(sh \"echo \\\"hi there\\\"\")")))
    (rtk-verdict "eval" arguments)
    (is (equal "(sh \"rtk echo \\\"hi there\\\"\")" (rtk-form arguments))
        "the command's own quotes survive the round trip")
    (is (consp (read-from-string (rtk-form arguments))) "and the snippet still reads")))

(deftest rtk-cell-rewrites-a-strings-entry (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (let* ((strings (nlk:json-object "cmd" "git status"))
         (arguments (nlk:json-object "form" "(sh ($ \"cmd\"))" "strings" strings))
         (form (rtk-form arguments)))
    (rtk-verdict "eval" arguments)
    (is (equal "rtk git status" (gethash "cmd" strings)) "the strings entry is rewritten")
    (is (equal form (rtk-form arguments)) "and the form text is untouched")))

(deftest rtk-cell-rewrites-a-shell-tools-command (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-verdict "terminal" arguments)
    (is (equal "rtk git status" (rtk-command arguments)) "a terminal tool's command is rewritten"))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-verdict "write-file" arguments)
    (is (equal "git status" (rtk-command arguments)) "a tool of another name is not")))

(deftest rtk-cell-honours-extra-tools (with-cell-stop ((rtk-start "binary" (rtk-stub) "extra_tools" (vector "mine"))))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-verdict "mine" arguments)
    (is (equal "rtk git status" (rtk-command arguments)) "a configured tool name is rewritten too")))

;;; --- what it leaves alone ---------------------------------------------------------

(deftest rtk-cell-leaves-alone-what-rtk-leaves-alone (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (dolist (command '("plain build" "broken build" "empty build" "same build"))
    (let* ((form (format nil "(sh \"~a\")" command))
           (arguments (nlk:json-object "form" form)))
      (rtk-verdict "eval" arguments)
      (is (equal form (rtk-form arguments)) (format nil "~s comes back as the model wrote it" command))))
  (let ((arguments (nlk:json-object "form" "(reduce #'+ (list 1 2 3))")))
    (rtk-verdict "eval" arguments)
    (is (equal "(reduce #'+ (list 1 2 3))" (rtk-form arguments)) "a snippet that is no shell call is not read")))

(deftest rtk-cell-leaves-whole-when-nothing-answers (with-cell-stop ((rtk-start "binary" "/nonexistent/rtk-not-here")))
  (let* ((form "(sh \"git status\")")
         (arguments (nlk:json-object "form" form)))
    (rtk-verdict "eval" arguments)
    (is (equal form (rtk-form arguments)) "a missing binary is no rewrite, never an error")))

(deftest rtk-cell-leaves-whole-past-the-timeout (with-cell-stop ((rtk-start "binary" (rtk-stub) "timeout_seconds" 1)))
  (let* ((form "(sh \"slow build\")")
         (arguments (nlk:json-object "form" form)))
    (rtk-verdict "eval" arguments)
    (is (equal form (rtk-form arguments)) "a rewrite past timeout_seconds leaves the command as written")))

(deftest rtk-cell-leaves-eval-alone-when-told-to (with-cell-stop ((rtk-start "binary" (rtk-stub) "rewrite_eval" nil)))
  (let* ((form "(sh \"git status\")")
         (arguments (nlk:json-object "form" form)))
    (rtk-verdict "eval" arguments)
    (is (equal form (rtk-form arguments)) "rewrite_eval off leaves snippets alone"))
  (let ((arguments (nlk:json-object "command" "git status")))
    (rtk-verdict "terminal" arguments)
    (is (equal "rtk git status" (rtk-command arguments)) "and a shell tool's command is still rewritten")))

;;; --- the report ------------------------------------------------------------------

(deftest rtk-cell-report-counts-what-it-rewrote (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (rtk-verdict "eval" (nlk:json-object "form" "(sh \"git status\")"))
  (rtk-verdict "eval" (nlk:json-object "form" "(sh \"plain build\")"))
  (let ((first (cell-entry "nodecode-rtk" "rtk" ""))
        (check (cell-entry "nodecode-rtk" "rtk" "check"))
        (reset (cell-entry "nodecode-rtk" "rtk" "reset"))
        (after (cell-entry "nodecode-rtk" "rtk" "")))
    (is-carrying first ("rewrote 1 of 2 commands" "what it rewrote of what it saw")
      ("1 left as written" "and what rtk left alone"))
    (is-carrying check ("answers" "check reports the binary"))
    (is-carrying reset ("counts reset" "reset zeroes them"))
    (is-carrying after ("rewrote 0 of 0" "and the next report starts over"))))

(deftest rtk-cell-report-refuses-when-not-running ()
  (is (signals-error nodecode-rtk::rtk-error (nodecode-rtk:report "" "s1"))))

(deftest rtk-cell-hook-is-a-no-op-once-stopped (with-cell-stop ((rtk-start "binary" (rtk-stub))))
  (let ((hook (cell-hook "nodecode-rtk" :tool)))
    (funcall stop)
    (setf stop nil)
    (let* ((form "(sh \"git status\")")
           (arguments (nlk:json-object "form" form)))
      (funcall hook (list :name "eval" :arguments arguments :call-id "c1")
               (lambda (op) (declare (ignore op)) "ran"))
      (is (equal form (rtk-form arguments)) "with the cell stopped, nothing is rewritten"))))
