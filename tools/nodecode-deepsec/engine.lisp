;;;; engine.lisp --- the rules, the walk, the findings, the rendering.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin ran an external Python CLI and rendered what it
;;;; answered. The rewrite keeps the same findings -- rule, severity, target,
;;;; line, evidence, suggestion -- and the same four verbs, and reads the
;;;; tree itself: pattern and secret rules (the original's L1 layer), prompt
;;;; rules for an agent's own configuration, and the dependency checks. It
;;;; reads files, never runs them.

(in-package #:nodecode-deepsec)

;;; --- severity ---------------------------------------------------------------

(defparameter *severity-rank* '("critical" "high" "medium" "low" "info")
  "The order a report sorts by: critical first.")

(defun rank (severity)
  "How severe SEVERITY is for sorting; an unknown word sorts with info."
  (or (position (string-downcase (princ-to-string severity)) *severity-rank*
                :test #'string=)
      (1- (length *severity-rank*))))

(defun text-has (needle text)
  "Whether the lowercased TEXT carries NEEDLE, as a boolean."
  (and (not (null (search needle text))) t))

(defun scannable-line (line)
  "LINE cut to what a rule reads, so a minified bundle costs one rule pass."
  (if (> (length line) 2000) (subseq line 0 2000) line))

;;; --- the rules --------------------------------------------------------------

(defstruct (rule (:constructor make-rule (id severity title suggestion test)))
  "One rule: what it names, how bad it is, what to do, and the matcher.
A matcher takes the lowercased line and answers an evidence string (or T),
exactly the shape the original's findings carried."
  (id "" :read-only t)
  (severity "medium" :read-only t)
  (title "" :read-only t)
  (suggestion "" :read-only t)
  (test (constantly nil) :read-only t))

(defparameter +secret-keys+
  '("api_key" "apikey" "api-key" "secret_key" "client_secret" "password" "passwd"
    "access_key" "auth_token" "private_key" "token_secret")
  "The names whose quoted value is a credential, in lowercase.")

(defparameter +secret-not-values+
  '("your" "xxxx" "changeme" "change_me" "example" "placeholder" "secret" "****"
    "none" "null" "true" "false" "todo" "dummy" "sample")
  "Values a rule names as obvious stand-ins, so a template is not a finding.")

(defun literal-value (line)
  "The value quoted after a credential name on LINE, or NIL when LINE holds
no credential-shaped literal."
  (let ((flat (string-downcase line)))
    (loop for key in +secret-keys+
          for at = (search key flat)
          when at
            do (let* ((quote (position-if (lambda (char) (member char '(#\" #\')))
                                          flat :start (+ at (length key))))
                      (close (and quote (position (char flat quote) flat :start (1+ quote)))))
                 (when close
                   (let ((value (subseq line (1+ quote) close)))
                     (when (and (>= (length value) 12)
                                (every (lambda (char)
                                         (or (and (< (char-code char) 128) (alphanumericp char))
                                             (find char "+/=_-.")))
                                       value)
                                (notany (lambda (word) (search word (string-downcase value)))
                                        +secret-not-values+))
                       (return value))))))))

(defun matcher-private-key (line)
  (and (text-has "-----begin" line) (text-has "private key" line)
       "a PEM private key block is in the file"))

(defun token-after (line prefix length)
  "The token on LINE that starts with PREFIX and runs LENGTH more word
characters, or NIL."
  (loop for at = (search prefix line)
        while at
          do (let ((end (+ at (length prefix) length)))
               (when (<= end (length line))
                 (let ((token (subseq line at end)))
                   (when (every (lambda (char)
                                  (or (and (< (char-code char) 128) (alphanumericp char))
                                      (member char '(#\_ #\-))))
                                token)
                     (return token)))))
             (setf at (search prefix line :start2 (1+ at)))))

(defun matcher-aws-key (line)
  (and (token-after line "akia" 16) "an AWS access key id is in the file"))

(defun matcher-github-token (line)
  (and (or (token-after line "ghp_" 36) (token-after line "github_pat_" 36))
       "a GitHub token is in the file"))

(defun matcher-slack-token (line)
  (and (or (token-after line "xoxb-" 10) (token-after line "xoxp-" 10))
       "a Slack token is in the file"))

(defun matcher-google-key (line)
  (and (token-after line "aiza" 35) "a Google API key is in the file"))

(defun matcher-dynamic-eval (line)
  (cond ((text-has "eval(" line) "eval() on a value")
        ((text-has "exec(" line) "exec() on a value")
        ((text-has "new function(" line) "new Function() on a value")))

(defun matcher-shell-command (line)
  (cond ((text-has "os.system(" line) "os.system()")
        ((text-has "shell=true" line) "a subprocess with shell=True")
        ((text-has "subprocess.popen(" line) "subprocess.Popen()")))

(defun matcher-sql-by-hand (line)
  (and (text-has "select " line)
       (or (text-has "%s" line) (text-has " + " line) (text-has "f\"" line)
           (text-has "f'" line) (text-has "${" line))
       "a SQL statement assembled from a value"))

(defun matcher-html-by-hand (line)
  (cond ((text-has "innerhtml" line) "innerHTML assigned")
        ((text-has "dangerouslysetinnerhtml" line) "dangerouslySetInnerHTML")
        ((text-has "document.write(" line) "document.write()")))

(defun matcher-tls-off (line)
  (cond ((text-has "verify=false" line) "TLS verification off")
        ((text-has "rejectunauthorized: false" line) "TLS verification off")
        ((text-has "insecureskipverify: true" line) "TLS verification off")))

(defun matcher-curl-pipe (line)
  (and (text-has "curl " line)
       (or (text-has "| sh" line) (text-has "|sh" line) (text-has "| bash" line)
           (text-has "|bash" line))
       "a downloaded script piped into a shell"))

(defun matcher-weak-hash (line)
  (cond ((text-has "md5(" line) "MD5")
        ((text-has "sha1(" line) "SHA-1")))

(defun matcher-world-writable (line)
  (and (text-has "chmod 777" line) "a world-writable mode"))

(defun matcher-unpinned-install (line)
  (and (or (text-has "pip install " line) (text-has "npm install " line))
       (not (text-has "==" line))
       (not (text-has " -r " line))
       "an unpinned install"))

(defparameter *rules*
  (list (make-rule "private_key" "critical" "A PEM private key is in the tree"
                   "Keep the key out of the tree and load it from the environment or a secret store."
                   #'matcher-private-key)
        (make-rule "aws_access_key" "critical" "An AWS access key id is in the tree"
                   "Move the credential to the environment or a secret store and rotate it."
                   #'matcher-aws-key)
        (make-rule "github_token" "critical" "A GitHub token is in the tree"
                   "Move the token to the environment and rotate it."
                   #'matcher-github-token)
        (make-rule "slack_token" "high" "A Slack token is in the tree"
                   "Move the token to the environment and rotate it."
                   #'matcher-slack-token)
        (make-rule "google_api_key" "high" "A Google API key is in the tree"
                   "Move the key to the environment and restrict it by referrer and API."
                   #'matcher-google-key)
        (make-rule "hardcoded_secret" "high" "A credential is assigned in the source"
                   "Read it from the environment, and keep the default a stand-in."
                   #'literal-value)
        (make-rule "curl_pipe_shell" "high" "A downloaded script is piped into a shell"
                   "Fetch it, check it, then run it."
                   #'matcher-curl-pipe)
        (make-rule "dynamic_eval" "medium" "Code is evaluated from a value"
                   "Parse the value, or keep an allow-list of what may be evaluated."
                   #'matcher-dynamic-eval)
        (make-rule "shell_command" "medium" "A shell command is built from a value"
                   "Pass an argument vector instead of a shell string."
                   #'matcher-shell-command)
        (make-rule "sql_by_hand" "medium" "A SQL statement is assembled from a value"
                   "Use a parameterised statement."
                   #'matcher-sql-by-hand)
        (make-rule "html_by_hand" "medium" "Markup is written from a value"
                   "Escape the value, or let the framework insert it."
                   #'matcher-html-by-hand)
        (make-rule "tls_verify_off" "medium" "TLS verification is switched off"
                   "Verify the certificate; pin one where a self-signed peer is expected."
                   #'matcher-tls-off)
        (make-rule "weak_hash" "low" "A weak hash is used"
                   "Use SHA-256 or a password hash for anything that matters."
                   #'matcher-weak-hash)
        (make-rule "world_writable" "low" "A file is left world-writable"
                   "Grant the narrowest mode that still works."
                   #'matcher-world-writable)
        (make-rule "unpinned_install" "low" "A dependency is installed without a version"
                   "Pin the version in the manifest."
                   #'matcher-unpinned-install))
  "The pattern layer: what the original's L1 scan looked for, as data. A layer
may push another rule; the walk reads this list.")

(defparameter *agent-rules*
  (list (make-rule "prompt_ignore_instructions" "high"
                   "A prompt tells the model to drop its instructions"
                   "Say which instruction it replaces, and why, in the same prompt."
                   (lambda (line)
                     (and (or (text-has "ignore previous" line)
                              (text-has "ignore all previous" line)
                              (text-has "ignore the above" line)
                              (text-has "disregard the above" line)
                              (text-has "disregard previous" line)
                              (text-has "忽略以上" line)
                              (text-has "忽略之前" line))
                          "the prompt overrides earlier instructions")))
        (make-rule "prompt_hide_from_user" "high"
                   "A prompt tells the model to hide what it does"
                   "Name the thing to keep quiet about, or drop the instruction."
                   (lambda (line)
                     (and (or (text-has "do not tell the user" line)
                              (text-has "don't tell the user" line)
                              (text-has "without telling the user" line)
                              (text-has "do not mention" line)
                              (text-has "hide this from" line)
                              (text-has "不要告诉" line))
                          "the prompt asks for a hidden action")))
        (make-rule "prompt_send_data_out" "high"
                   "A prompt sends data to an address"
                   "Name the address and the data, and whether the user agreed."
                   (lambda (line)
                     (and (or (text-has "webhook" line) (text-has "pastebin" line)
                              (text-has "to http" line))
                          (or (text-has "send " line) (text-has "post " line)
                              (text-has "upload" line) (text-has "forward " line))
                          "the prompt forwards data to a third party")))
        (make-rule "prompt_credential_path" "high"
                   "A prompt reads a credential store"
                   "Read only what the task needs; a token for another app is not one."
                   (lambda (line)
                     (and (or (text-has ".ssh/" line) (text-has "id_rsa" line)
                              (text-has ".aws/credentials" line)
                              (text-has ".netrc" line) (text-has "keychain" line)
                              (text-has "login data" line) (text-has "cookies.sqlite" line))
                          "the prompt names a credential store")))
        (make-rule "prompt_remote_exec" "high"
                   "A prompt runs a command it fetched"
                   "Fetch it, check it, then run it."
                   (lambda (line)
                     (and (or (text-has "curl " line) (text-has "wget " line))
                          (or (text-has "| sh" line) (text-has "|sh" line)
                              (text-has "| bash" line) (text-has "eval" line)
                              (text-has "base64 -d" line))
                          "the prompt pipes a download into a shell"))))
  "The agent-configuration layer: prompt injection, hidden action, egress and
credential reads, as the original's agent audit looked for them.")

;;; --- findings ---------------------------------------------------------------

(defun make-finding (rule severity title target line evidence suggestion)
  "One finding, the shape every renderer and the saved JSON agree on."
  (list :rule rule :severity severity :title title :target target :line line
        :evidence evidence :suggestion suggestion :layer "l1"))

(defun finding (key item) (getf item key))

(defun line-findings (line number target rules)
  "Every finding RULES make for LINE, the line NUMBER of TARGET."
  (let* ((line (scannable-line line))
         (flat (string-downcase line)))
    (loop for rule in rules
          for evidence = (funcall (rule-test rule) flat)
          when evidence
            collect (make-finding (rule-id rule) (rule-severity rule) (rule-title rule)
                                  target number
                                  (if (stringp evidence) evidence
                                      (nlk:clip (nlk:trimmed line) 160))
                                  (rule-suggestion rule)))))

(defun file-findings (path rules)
  "Every finding RULES make for the file PATH; a file that cannot be read as
text is one line of nothing, never an error."
  (let ((text (handler-case
                  (with-open-file (in path :direction :input :external-format :utf-8
                                           :element-type 'character)
                    (let ((buffer (make-string (min (file-length in) 4000000))))
                      (subseq buffer 0 (read-sequence buffer in))))
                (error () nil))))
    (when text
      (loop for line in (nlk:lines text)
            for number from 1
            append (line-findings line number (namestring path) rules)))))

;;; --- the walk ---------------------------------------------------------------

(defparameter *ignored-directories*
  '(".git" "node_modules" "dist" "build" "target" "vendor" "__pycache__" ".venv"
    "venv" ".mypy_cache" ".pytest_cache" "coverage" ".next" ".cache" ".idea")
  "Directory names the walk never enters.")

(defparameter *test-directories*
  '("test" "tests" "__tests__" "spec" "specs" "fixtures" "examples")
  "Directory names the walk enters only when the caller asks for tests.")

(defparameter *skipped-suffixes*
  '(".png" ".jpg" ".jpeg" ".gif" ".svg" ".ico" ".pdf" ".zip" ".gz" ".tgz" ".lock"
    ".map" ".woff" ".woff2" ".ttf" ".mp4" ".mov" ".so" ".dylib" ".dll" ".exe"
    ".pyc" ".class" ".jar" ".wasm" ".db" ".sqlite" ".mo" ".po")
  "File suffixes the walk never reads.")

(defparameter *max-files* 4000
  "How many files one walk reads; the rest are left, not scanned.")

(defun suffix-p (suffix name)
  "Whether NAME ends in SUFFIX."
  (and (> (length name) (length suffix))
       (string= suffix name :start2 (- (length name) (length suffix)))))

(defun skipped-file-p (path)
  "Whether PATH is a file the walk never reads."
  (let ((name (string-downcase (file-namestring path))))
    (or (some (lambda (suffix) (suffix-p suffix name)) *skipped-suffixes*)
        (suffix-p ".min.js" name))))

(defun tree-files (root &key include-tests)
  "The files under ROOT -- a file is itself -- in a stable order, skipping
what *IGNORED-DIRECTORIES* names and, unless INCLUDE-TESTS, test folders."
  (let ((files '()))
    (labels ((walk (directory)
               (dolist (file (sort (uiop:directory-files directory) #'string< :key #'namestring))
                 (when (and (< (length files) *max-files*) (not (skipped-file-p file)))
                   (push file files)))
               (dolist (sub (sort (uiop:subdirectories directory) #'string< :key #'namestring))
                 (let ((name (string-downcase (car (last (pathname-directory sub))))))
                   (unless (or (member name *ignored-directories* :test #'string=)
                               (and (not include-tests)
                                    (member name *test-directories* :test #'string=)))
                     (walk sub))))))
      (cond ((null root) nil)
            ((uiop:directory-exists-p root)
             (walk (uiop:ensure-directory-pathname root)))
            ((probe-file root) (list (uiop:ensure-pathname root)))
            (t nil)))
    (nreverse files)))

(defun sort-findings (findings)
  "FINDINGS worst first, then by target and line, the order a report reads in."
  (sort (copy-list findings)
        (lambda (a b)
          (let ((by-severity (- (rank (finding :severity a)) (rank (finding :severity b)))))
            (if (zerop by-severity)
                (let ((a-target (princ-to-string (finding :target a)))
                      (b-target (princ-to-string (finding :target b))))
                  (if (string= a-target b-target)
                      (< (or (finding :line a) 0) (or (finding :line b) 0))
                      (string< a-target b-target)))
                (minusp by-severity))))))

(defun scan-tree (root rules &key include-tests)
  "Every finding RULES make under ROOT, and how many files were read."
  (let* ((files (tree-files root :include-tests include-tests))
         (findings (loop for file in files
                         append (file-findings file rules))))
    (values (sort-findings findings) (length files))))

(defun by-severity (findings)
  "How many findings there are at each severity that has one, worst first."
  (let ((counts '()))
    (dolist (item findings)
      (let* ((severity (finding :severity item))
             (pair (assoc severity counts :test #'string=)))
        (if pair
            (incf (cdr pair))
            (push (cons severity 1) counts))))
    (sort counts (lambda (a b) (< (rank (car a)) (rank (car b)))))))

(defun highest-severity (findings)
  "The worst severity among FINDINGS, or NIL."
  (caar (by-severity findings)))

;;; --- the dependency checks ---------------------------------------------------

(defparameter *popular-names*
  (list :npm '("react" "express" "lodash" "axios" "typescript" "next" "vue" "webpack"
               "chalk" "commander" "debug" "moment" "request" "async" "jquery"
               "underscore" "mongoose" "socket.io" "eslint" "prettier" "jest"
               "dotenv" "uuid" "zod" "cors" "helmet" "passport" "redis" "pg" "mysql"
               "sequelize" "tslib" "glob" "minimist" "node-fetch" "yargs" "colors")
        :pypi '("requests" "numpy" "pandas" "flask" "django" "boto3" "pytest"
                "setuptools" "urllib3" "certifi" "pyyaml" "click" "six" "colorama"
                "scipy" "matplotlib" "tensorflow" "torch" "sqlalchemy"
                "beautifulsoup4" "lxml" "cryptography" "paramiko" "ansible" "celery"
                "pydantic" "fastapi" "aiohttp" "httpx" "rich" "tqdm" "jinja2"
                "pillow" "opencv-python" "nltk" "openai" "anthropic"))
  "The names a typosquat imitates, one list per registry, as data.")

(defun distance (a b)
  "The Damerau-Levenshtein distance between A and B: how many edits apart two
package names are, a transposition counting one."
  (let* ((n (length a)) (m (length b))
         (d (make-array (list (1+ n) (1+ m)) :initial-element 0)))
    (dotimes (i (1+ n)) (setf (aref d i 0) i))
    (dotimes (j (1+ m)) (setf (aref d 0 j) j))
    (loop for i from 1 to n
          do (loop for j from 1 to m
                   do (let ((cost (if (char-equal (char a (1- i)) (char b (1- j))) 0 1)))
                        (setf (aref d i j)
                              (min (1+ (aref d (1- i) j))
                                   (1+ (aref d i (1- j)))
                                   (+ cost (aref d (1- i) (1- j)))))
                        (when (and (> i 1) (> j 1)
                                   (char-equal (char a (1- i)) (char b (- j 2)))
                                   (char-equal (char a (- i 2)) (char b (1- j))))
                          (setf (aref d i j)
                                (min (aref d i j) (+ 1 (aref d (- i 2) (- j 2)))))))))
    (aref d n m)))

(defun typosquat-of (name registry)
  "The popular NAME imitates, one edit away and not a name itself, or NIL."
  (when (>= (length name) 5)
    (let ((flat (string-downcase name)))
      (find-if (lambda (candidate)
                 (and (not (string= flat candidate))
                      (<= (abs (- (length flat) (length candidate))) 2)
                      (<= (distance flat candidate) 1)))
               (getf *popular-names* registry)))))

(defun dependency-rows (path)
  "The dependencies a manifest PATH declares, as (NAME REGISTRY SPEC) rows; a
path that is neither a package manifest nor a requirements file declares none."
  (let ((name (string-downcase (file-namestring path)))
        (text (handler-case (uiop:read-file-string path :external-format :utf-8)
                (error () ""))))
    (cond ((string= name "package.json")
           (let ((object (handler-case (nlk:decode-json text) (error () nil))))
             (loop for field in '("dependencies" "devDependencies" "peerDependencies")
                   for table = (nlk:json-value object :object field)
                   when table
                     append (loop for key being the hash-keys of table
                                  collect (list key :npm (princ-to-string (gethash key table)))))))
          ((or (string= name "requirements.txt") (string= name "requirements-dev.txt"))
           (loop for line in (nlk:lines text)
                 for clean = (nlk:trimmed (or (car (uiop:split-string line :separator '(#\#))) ""))
                 when (and (plusp (length clean)) (not (find (char clean 0) "-. ")))
                   collect (list (string-downcase
                                  (subseq clean 0 (or (position-if
                                                       (lambda (char)
                                                         (member char '(#\= #\> #\< #\~ #\! #\[ #\Space #\;)))
                                                       clean)
                                                      (length clean))))
                                 :pypi clean))))))

(defun supply-chain-findings (root &key private-packages)
  "The dependency findings under ROOT: an imitation of a popular name, a
private name a public registry would answer for, and a fetch over plain HTTP."
  (let ((findings '()))
    (dolist (file (tree-files root :include-tests t))
      (let ((target (namestring file)))
        (dolist (row (dependency-rows file))
          (destructuring-bind (name registry spec) row
            (let ((looks-like (and (not (find #\/ name)) (typosquat-of name registry))))
              (when looks-like
                (push (make-finding "typosquat" "high"
                                    (format nil "~a looks like the popular ~a" name looks-like)
                                    target nil (format nil "~a ~a" name spec)
                                    "Check the spelling: a squatter publishes the misspelling.")
                      findings)))
            (when (member name private-packages :test #'string=)
              (push (make-finding "dependency_confusion" "high"
                                  (format nil "~a is declared private but a public registry answers for it" name)
                                  target nil (format nil "~a ~a" name spec)
                                  "Scope the name, or pin the private registry for it.")
                    findings))
            (when (and (stringp spec)
                       (or (text-has "http://" (string-downcase spec))
                           (text-has "git://" (string-downcase spec))))
              (push (make-finding "insecure_dependency_source" "medium"
                                  (format nil "~a is fetched over plain HTTP or git" name)
                                  target nil spec
                                  "Fetch it over HTTPS and pin the version.")
                    findings))))))
    (sort-findings findings)))

;;; --- rendering ---------------------------------------------------------------

(defun summary-line (findings files elapsed)
  "The first line of a report: how many findings, how bad, how much was read."
  (format nil "~d active finding~:p (~{~a~^, ~}) in ~d file~:p, ~,1f ms"
          (length findings)
          (or (mapcar (lambda (pair) (format nil "~a: ~d" (car pair) (cdr pair)))
                      (by-severity findings))
              (list "none"))
          files elapsed))

(defun finding-line (item)
  "One finding as a plain block: where, what, and what to do."
  (format nil "[~a] ~a~@[:~d~] (~a) ~a~@[~%  evidence: ~a~]~@[~%  fix: ~a~]"
          (finding :severity item) (finding :target item) (finding :line item)
          (finding :rule item) (finding :title item)
          (finding :evidence item) (finding :suggestion item)))

(defun shown (findings limit)
  "FINDINGS cut to LIMIT, or all of them when LIMIT is NIL."
  (if (and limit (< limit (length findings))) (subseq findings 0 limit) findings))

(defun render-text (findings files elapsed &key limit)
  "A plain report: the summary, then at most LIMIT findings."
  (format nil "~a~@[~%~{~a~^~%~}~]~@[~%... ~d more finding~:p~]"
          (summary-line findings files elapsed)
          (shown findings limit)
          (and limit (max 0 (- (length findings) limit)))))

(defun render-markdown (findings files elapsed &key limit)
  "A markdown report: a table row a finding, worst first."
  (with-output-to-string (out)
    (format out "## DeepSec Shield report~%~%~a~%~%" (summary-line findings files elapsed))
    (when findings
      (format out "| Severity | Where | Rule | Finding |~%| --- | --- | --- | --- |~%")
      (dolist (item (shown findings limit))
        (format out "| ~a | ~a~@[:~d~] | ~a | ~a |~%"
                (finding :severity item) (finding :target item) (finding :line item)
                (finding :rule item) (finding :title item)))
      (when (and limit (> (length findings) limit))
        (format out "~%~d more finding~:p not shown.~%" (- (length findings) limit))))))

(defun finding-object (item)
  "One finding as the JSON object a saved result holds."
  (nlk:make-json-object
   "rule" (finding :rule item)
   "severity" (finding :severity item)
   "title" (finding :title item)
   "target" (finding :target item)
   "line" (or (finding :line item) 0)
   "evidence" (or (finding :evidence item) "")
   "suggestion" (or (finding :suggestion item) "")
   "layer" (finding :layer item)))

(defun result-object (findings files elapsed)
  "A whole scan as the JSON object a saved result holds and a report reads."
  (nlk:make-json-object
   "tool" "deepsec"
   "version" 1
   "filesScanned" files
   "elapsedMs" (round elapsed)
   "layers" (vector "l1")
   "summary" (apply #'nlk:make-json-object
                    (append (list "activeFindings" (length findings)
                                  "highestSeverity" (or (highest-severity findings) ""))
                            (loop for (severity . count) in (by-severity findings)
                                  append (list severity count))))
   "findings" (coerce (mapcar #'finding-object findings) 'vector)))

(defun render-json (findings files elapsed)
  "A whole scan as JSON text."
  (nlk:encode-json-object (result-object findings files elapsed)))

(defun sarif-level (severity)
  "SARIF's own word for SEVERITY."
  (cond ((member severity '("critical" "high") :test #'string=) "error")
        ((string= severity "medium") "warning")
        (t "note")))

(defun sarif-result (item)
  "One finding as a SARIF result."
  (nlk:make-json-object
   "ruleId" (finding :rule item)
   "level" (sarif-level (finding :severity item))
   "message" (nlk:make-json-object
              "text" (format nil "~a: ~a" (finding :title item) (finding :evidence item)))
   "locations" (vector
                (nlk:make-json-object
                 "physicalLocation"
                 (nlk:make-json-object
                  "artifactLocation" (nlk:make-json-object "uri" (finding :target item))
                  "region" (nlk:make-json-object "startLine" (or (finding :line item) 1)))))))

(defun render-sarif (findings files elapsed)
  "A whole scan as SARIF 2.1.0, so a code host reads it."
  (declare (ignore files elapsed))
  (nlk:encode-json-object
   (nlk:make-json-object
    "version" "2.1.0"
    "$schema" "https://json.schemastore.org/sarif-2.1.0.json"
    "runs" (vector
            (nlk:make-json-object
             "tool" (nlk:make-json-object
                     "driver" (nlk:make-json-object
                               "name" "deepsec"
                               "informationUri" "https://github.com/Unclecheng-li/DeepSec"))
             "results" (map 'vector #'sarif-result findings))))))

(defun findings-of (json)
  "The findings a decoded saved result JSON holds, as plists."
  (loop for object across (nlk:json-value json :array "findings")
        collect (make-finding (or (nlk:json-value object :string "rule") "unknown")
                              (or (nlk:json-value object :string "severity") "info")
                              (or (nlk:json-value object :string "title") "")
                              (or (nlk:json-value object :string "target") "")
                              (nlk:json-value object :integer "line")
                              (or (nlk:json-value object :string "evidence") "")
                              (or (nlk:json-value object :string "suggestion") ""))))
