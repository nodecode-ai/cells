;;;; classify.lisp --- which tool calls a staged copy cannot contain.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A copy of the workspace contains file changes inside the workspace.
;;;; Everything else a command can do -- talk to the network, install
;;;; software, write elsewhere on the machine -- really happens, so the plugin
;;;; held those calls for approval and labelled them NOT CONTAINED. The port
;;;; reads the same command text and answers the same label; what it does not
;;;; do is put up a prompt (see NOTICE).

(in-package #:nodecode-sluice)

(defstruct (finding (:constructor make-finding (category detail)))
  category
  detail)

(defun finding-message (finding)
  (format nil "NOT CONTAINED: ~a. Sluice only contains file changes inside the workspace."
          (finding-detail finding)))

(defparameter *safe-sinks*
  '("/dev/null" "/dev/stdout" "/dev/stderr" "/dev/tty" "/dev/zero"))

(defparameter *install-verbs*
  '(("pip" . "install") ("pip3" . "install") ("pipx" . "install")
    ("npm" . "install") ("npm" . "ci") ("pnpm" . "install") ("yarn" . "add")
    ("bun" . "install") ("bun" . "add") ("apt" . "install") ("apt-get" . "install")
    ("dnf" . "install") ("yum" . "install") ("brew" . "install") ("apk" . "add")
    ("cargo" . "install") ("gem" . "install") ("go" . "install")
    ("poetry" . "add") ("poetry" . "install") ("uv" . "add") ("uv" . "sync")
    ("conda" . "install") ("composer" . "require") ("nix-env" . "-i"))
  "Commands that install software, as (COMMAND . VERB).")

(defparameter *network-tools*
  '("scp" "sftp" "ftp" "nc" "ncat" "netcat" "telnet" "rsync" "ssh" "mosh")
  "Commands that send something off this machine.")

(defparameter *writers*
  '("rm" "rmdir" "mv" "cp" "touch" "mkdir" "chmod" "chown" "ln" "tee"
    "truncate" "install" "shred" "unlink")
  "Commands whose arguments may be outside the workspace.")

(defparameter *deleters* '("rm" "rmdir" "shred" "unlink"))

(defparameter *publishers*
  '(("npm" . "publish") ("pnpm" . "publish") ("yarn" . "publish")
    ("twine" . "upload") ("cargo" . "publish") ("cargo" . "yank")
    ("gem" . "push") ("docker" . "push") ("docker" . "login") ("podman" . "push")
    ("kubectl" . "apply") ("kubectl" . "delete") ("kubectl" . "create")
    ("helm" . "install") ("helm" . "upgrade") ("terraform" . "apply")
    ("terraform" . "destroy") ("tofu" . "apply") ("fly" . "deploy")
    ("flyctl" . "deploy") ("vercel" . "deploy") ("netlify" . "deploy")
    ("heroku" . "push"))
  "Commands that publish or change a remote system.")

(defparameter *runner-tools* '("npx" "bunx" "uvx" "dlx")
  "Commands that download and run code.")

(defparameter *quote-chars* (list (code-char 34) (code-char 39))
  "The characters that open and close a shell quotation.")

(defparameter *separators*
  (list #\; #\| #\& #\Newline)
  "An unquoted one of these starts the next command.")

;;; --- reading a command -------------------------------------------------------

(defun split-commands (text)
  "TEXT cut into the commands it runs: an unquoted ; | & or newline separates."
  (let ((parts '()) (start 0) (quote nil) (index 0) (length (length text)))
    (loop while (< index length)
          for character = (char text index)
          do (cond ((and quote (char= character (code-char 92))) (incf index 2))
                   (quote (when (char= character quote) (setf quote nil)) (incf index))
                   ((member character *quote-chars*) (setf quote character) (incf index))
                   ((member character *separators*)
                    (push (subseq text start (min index length)) parts)
                    (incf index)
                    (setf start index))
                   (t (incf index))))
    (push (subseq text (min start length)) parts)
    (remove-if (lambda (part)
                 (zerop (length (string-trim '(#\Space #\Tab #\Return) part))))
               (nreverse parts))))

(defun words (text)
  "TEXT cut into words, a shell quotation read as one word."
  (let ((out '()) (current (make-string-output-stream)) (quote nil))
    (flet ((flush ()
             (let ((word (get-output-stream-string current)))
               (when (plusp (length word)) (push word out)))))
      (loop for character across text
            do (cond ((and quote (char= character quote)) (setf quote nil))
                     (quote (write-char character current))
                     ((member character *quote-chars*) (setf quote character))
                     ((member character '(#\Space #\Tab #\Return))
                      (flush))
                     (t (write-char character current))))
      (flush))
    (nreverse out)))

(defun basename (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun outside-p (path root allow)
  "Whether PATH, as a command writes it, is outside ROOT and not allowed."
  (cond ((or (null path) (zerop (length path))) nil)
        ((member path *safe-sinks* :test #'equal) nil)
        ((char= #\- (char path 0)) nil)
        ((find #\$ path) t)
        (t (let* ((expanded (if (uiop:string-prefix-p "~/" path)
                                (native (nlk:expand-home path))
                                path))
                  (full (if (absolute-path-string-p expanded)
                            expanded
                            (path-join root expanded)))
                  (normal (normalize-path "/" (string-left-trim "/" full))))
             (and (not (path-under-p normal root))
                  (not (loop for one in allow
                             thereis (path-under-p normal (strip-slash one)))))))))

;;; --- the classifier -------------------------------------------------------------

(defun classify-command (command root &optional allow)
  "COMMAND read against ROOT => the FINDINGS a staged copy cannot contain."
  (let ((root (strip-slash root))
        (findings '()))
    (labels ((add (category detail)
               (let ((finding (make-finding category detail)))
                 (unless (find finding findings
                               :test (lambda (a b)
                                       (and (eq (finding-category a) (finding-category b))
                                            (equal (finding-detail a) (finding-detail b)))))
                   (push finding findings))))
             (note (others)
               (dolist (one others) (add (finding-category one) (finding-detail one))))
             (redirects (text)
               (loop for index from 0 below (length text)
                     when (char= #\> (char text index))
                       do (let* ((rest (string-left-trim '(#\Space #\Tab)
                                                          (subseq text (1+ index))))
                                 (target (or (first (words rest)) "")))
                            (when (and (plusp (length target)) (outside-p target root allow))
                              (add :outside-write
                                   (format nil "redirects output to ~a, outside the workspace"
                                           target))))))
             (segment (text)
               (let ((word-list (words text)))
                 (loop while (and word-list
                                  (or (find #\= (first word-list))
                                      (member (first word-list)
                                              '("env" "command" "builtin" "nohup" "time")
                                              :test #'equal)))
                       do (setf word-list (rest word-list)))
                 (when (member (first word-list) '("sudo" "su" "doas") :test #'equal)
                   (add :privilege "runs with elevated privileges")
                   (setf word-list (rest word-list)))
                 (let* ((command (basename (or (first word-list) "")))
                        (arguments (rest word-list))
                        (leading (subseq arguments 0 (min 3 (length arguments)))))
                   (cond
                     ((and (member command '("bash" "sh" "zsh") :test #'equal)
                           (member "-c" arguments :test #'equal))
                      (let ((inner (second (member "-c" arguments :test #'equal))))
                        (when inner (note (classify-command inner root allow)))))
                     ((equal command "eval")
                      (note (classify-command (format nil "~{~a~^ ~}" arguments) root allow)))
                     ((equal command "git")
                      (when (equal (find-if (lambda (word) (char/= #\- (char word 0))) arguments)
                                   "push")
                        (add :push "runs git push, which publishes commits to a remote")))
                     ((member command '("curl" "wget" "http" "https" "xh") :test #'equal)
                      (when (or (some (lambda (argument)
                                        (or (uiop:string-prefix-p "-d" argument)
                                            (uiop:string-prefix-p "--data" argument)
                                            (equal argument "-F")
                                            (uiop:string-prefix-p "--form" argument)
                                            (equal argument "-T")
                                            (uiop:string-prefix-p "--upload-file" argument)
                                            (uiop:string-prefix-p "--json" argument)))
                                      arguments)
                                (loop for (flag value) on arguments
                                      thereis (and (member flag '("-X" "--request") :test #'equal)
                                                   value
                                                   (member (string-upcase value)
                                                           '("POST" "PUT" "PATCH" "DELETE")
                                                           :test #'equal))))
                        (add :network (format nil "sends data with ~a" command))))
                     ((member command *network-tools* :test #'equal)
                      (add :network (format nil "uses ~a, which sends data off this machine" command)))
                     ((and (equal command "gh")
                           (member (or (first arguments) "") '("pr" "issue" "release" "repo" "gist")
                                   :test #'equal)
                           (member (or (second arguments) "") '("create" "merge" "close" "delete"
                                                                  "edit" "comment" "run")
                                   :test #'equal))
                      (add :network "changes GitHub state")))
                   (dolist (pair *publishers*)
                     (when (and (equal command (car pair))
                                (member (cdr pair) leading :test #'equal))
                       (add :network (format nil "publishes or changes a remote system with ~a ~a"
                                             command (cdr pair)))))
                   (dolist (pair *install-verbs*)
                     (when (and (equal command (car pair))
                                (member (cdr pair) leading :test #'equal))
                       (add :install (format nil "installs software with ~a" command))))
                   (when (member command *runner-tools* :test #'equal)
                     (add :install (format nil "downloads and runs code with ~a" command)))
                   (when (member command *writers* :test #'equal)
                     (dolist (target (remove-if (lambda (word) (char= #\- (char word 0))) arguments))
                       (when (outside-p target root allow)
                         (add (if (member command *deleters* :test #'equal)
                                  :outside-delete :outside-write)
                              (format nil "~a touches ~a, outside the workspace"
                                      command target)))))
                   (when (and (equal command "sed") (member "-i" arguments :test #'equal))
                     (dolist (target (rest (member "-i" arguments :test #'equal)))
                       (when (outside-p target root allow)
                         (add :outside-write
                              (format nil "edits ~a in place, outside the workspace" target)))))
                   (when (equal command "dd")
                     (dolist (word arguments)
                       (when (and (uiop:string-prefix-p "of=" word)
                                  (outside-p (subseq word 3) root allow))
                         (add :outside-write
                              (format nil "dd writes ~a, outside the workspace" (subseq word 3))))))))))
      (when (loop for (before after) on (split-commands command)
                  thereis (and after
                               (member (basename (or (first (words before)) ""))
                                       '("curl" "wget") :test #'equal)
                               (member (basename (or (first (words after)) ""))
                                       '("sh" "bash" "zsh") :test #'equal)))
        (add :install "downloads a script and runs it"))
      (redirects command)
      (dolist (one (split-commands command)) (segment one)))
    (nreverse findings)))

