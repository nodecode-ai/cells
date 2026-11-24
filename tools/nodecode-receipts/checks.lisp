;;;; checks.lisp --- the three adapters: a file read, a command run, a page fetched.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each answers one plist, the same four members every check result has:
;;;; :STATE "passed", "failed" or "unverified", :REASON one line the
;;;; operator reads, :EVIDENCE what was actually seen, :PROVENANCE where it
;;;; came from. A failed check is a requirement that did not hold; an
;;;; unverified one is evidence missing or unobtainable -- never a quiet
;;;; pass. Nothing here writes, and only the test adapter runs anything.

(in-package #:nodecode-receipts)

(defun result (state reason evidence provenance)
  "One check's result."
  (list :state state :reason reason :evidence evidence :provenance provenance))

(defun octets-text (octets)
  "OCTETS, a vector of bytes, as text: UTF-8 where it decodes, Latin-1
elsewhere, NIL for something that is not bytes."
  (when (typep octets '(simple-array (unsigned-byte 8) (*)))
    (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
      (error () (handler-case (sb-ext:octets-to-string octets :external-format :latin-1)
                  (error () nil))))))

(defun read-capped (path cap)
  "PATH's bytes, at most CAP of them. => (values OCTETS TRUNCATED-P), or NIL
when it cannot be read."
  (handler-case
      (with-open-file (in path :direction :input :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
        (if (null in)
            nil
            (let* ((buffer (make-array (1+ cap) :element-type '(unsigned-byte 8)))
                   (count (read-sequence buffer in)))
              (values (subseq buffer 0 (min count cap)) (> count cap)))))
    (error () nil)))

;;; --- a file ------------------------------------------------------------------

(defun check-file (check spec)
  "CHECK, a file check, against SPEC's workspace: the file holds the required
text, or is a JSON object carrying the required keys."
  (let* ((relative (getf check :path))
         (root (uiop:ensure-directory-pathname
                (or (probe-file (getf spec :workspace-root)) (getf spec :workspace-root))))
         (cap (setting :read-cap-bytes))
         (evidence (jobj "relative_path" relative)))
    (handler-case
        (let ((path (merge-pathnames relative root)))
          (unless (probe-file root)
            (return-from check-file
              (result "unverified" "workspace does not exist" evidence "file-read")))
          (let ((truename (probe-file path)))
            (unless truename
              (return-from check-file
                (result "failed" "missing file" evidence "file-read")))
            (unless (uiop:subpathp truename root)
              (return-from check-file
                (result "failed" "file is outside the workspace" evidence "file-read")))
            (multiple-value-bind (octets truncated) (read-capped truename cap)
              (unless octets
                (return-from check-file
                  (result "unverified" "file could not be read" evidence "file-read")))
              (when truncated
                (return-from check-file
                  (result "unverified" (format nil "file exceeds the ~d byte read cap" cap)
                          evidence "file-read")))
              (setf evidence (jobj "relative_path" relative
                                   "size" (length octets)
                                   "sha256" (subseq (nlk:sha256-text octets) 7)))
              (let ((text (octets-text octets)))
                (cond ((null text)
                       (result "failed" "file is not valid UTF-8 text" evidence "file-read"))
                      ((getf check :json-keys)
                       (check-json-keys text (getf check :json-keys) relative
                                         (length octets) (subseq (nlk:sha256-text octets) 7)))
                      ((search (getf check :contains) text)
                       (result "passed" "required text present" evidence "file-read"))
                      (t (result "failed" "required text absent" evidence "file-read")))))))
      (error (condition)
        (result "unverified" (format nil "read failed: ~a" (type-of condition))
                evidence "file-read")))))

(defun check-json-keys (text keys relative size sha256)
  "TEXT parsed as JSON must be an object carrying every one of KEYS; RELATIVE,
SIZE and SHA256 are the file's own evidence."
  (let ((parsed (handler-case (nlk:decode-json text) (error () nil)))
        (evidence (jobj "relative_path" relative "size" size "sha256" sha256)))
    (cond ((null parsed)
           (result "failed" "file is not parseable JSON" evidence "file-read"))
          ((not (hash-table-p parsed))
           (result "failed" "JSON value is not an object" evidence "file-read"))
          (t (let ((missing (remove-if (lambda (key) (nth-value 1 (gethash key parsed))) keys)))
               (if missing
                   (result "failed" "missing JSON keys"
                           (jobj "relative_path" relative "size" size "sha256" sha256
                                 "missing_keys" (jarr missing))
                           "file-read")
                   (result "passed" "required JSON keys present"
                           evidence "file-read")))))))

;;; --- a command ----------------------------------------------------------------

(defun check-test (check)
  "CHECK, a test check: the declared command runs here, bounded, in its
declared workdir, and the receipt keeps what it answered."
  (let* ((command (getf check :command))
         (workdir (getf check :workdir))
         (expected (getf check :expected-exit))
         (needle (getf check :contains))
         (timeout (min (getf check :timeout) (setting :command-timeout)))
         (evidence (jobj "command" command "workdir" workdir)))
    (handler-case
        (multiple-value-bind (out err status)
            (nlk:run-bounded (nlk:shell-argv command) :seconds timeout :directory workdir
                             :output :string :error-output :string)
          (let* ((whole (concatenate 'string (or out "") (or err "")))
                 (cut (> (length whole) +output-keep+))
                 (kept (if cut (subseq whole 0 +output-keep+) whole))
                 (evidence (jobj "command" command "workdir" workdir
                                 "exit_code" (if (integerp status) status +null+)
                                 "output" kept
                                 "output_truncated" (if cut t :false)
                                 "timeout_seconds" timeout)))
            (cond ((eq status :timeout)
                   (result "unverified" (format nil "timed out after ~ds" timeout)
                           evidence "observed-command"))
                  ((not (integerp status))
                   (result "unverified" "no exit status" evidence "observed-command"))
                  ((/= status expected)
                   (result "failed" (format nil "exit ~d, expected ~d" status expected)
                           evidence "observed-command"))
                  ((and needle (not (search needle whole)))
                   (result "failed" "required output absent" evidence "observed-command"))
                  (needle (result "passed" "exit and output matched"
                                  evidence "observed-command"))
                  (t (result "passed" "exit matched" evidence "observed-command")))))
      (error (condition)
        (result "unverified" (format nil "command could not run: ~a" (type-of condition))
                evidence "observed-command")))))

;;; --- a page ------------------------------------------------------------------

(defun fetch-url (url timeout)
  "One GET of URL through NLK:HTTP, whole or TRUNCATED past the read cap.
=> (values TEXT STATUS TRUNCATED-P); tests stub this name, so no test opens a
socket."
  (multiple-value-bind (body status headers truncated)
      (nlk:http :get url :timeout timeout :max-bytes (setting :read-cap-bytes)
                      :headers '(("accept-encoding" . "identity")))
    (declare (ignore headers))
    (values (if (stringp body) body (octets-text body)) status truncated)))

(defun local-host-p (host)
  "Whether HOST, as a url writes it, is this machine: a bare name, localhost,
or a literal loopback or private address."
  (let ((host (string-downcase host)))
    (or (not (find #\. host))
        (member host '("localhost" "127.0.0.1" "::1" "[::1]") :test #'equal)
        (uiop:string-prefix-p "127." host)
        (uiop:string-prefix-p "10." host)
        (uiop:string-prefix-p "192.168." host)
        (uiop:string-prefix-p "[::1" host))))

(defun url-refusal (url allow-local)
  "Why URL may not be fetched under a spec's ALLOW-LOCAL, or NIL: the scheme,
the userinfo, and a local target without the opt-in."
  (cond ((not (stringp url)) "url must be a string")
        ((some (lambda (character)
                 (member character '(#\Space #\Tab #\Newline #\Return)))
               url)
         "unsupported url characters")
        ((not (or (uiop:string-prefix-p "https://" url)
                  (uiop:string-prefix-p "http://" url)))
         "unsupported scheme")
        (t (let* ((secure (uiop:string-prefix-p "https://" url))
                  (rest (subseq url (if secure 8 7)))
                  (authority (subseq rest 0 (or (position #\/ rest) (length rest)))))
             (cond ((zerop (length authority)) "missing host")
                   ((find #\@ authority) "url userinfo rejected")
                   ((and (local-host-p (first (uiop:split-string authority :separator ":")))
                         (not allow-local))
                    "a local target needs allow_loopback_http in the spec")
                   (t nil))))))

(defun check-website (check spec)
  "CHECK, a website check: the page answers the expected status and holds the
required text now."
  (let* ((url (getf check :url))
         (expected (getf check :expected-status))
         (needle (getf check :contains))
         (refusal (url-refusal url (getf spec :allow-loopback-http))))
    (when refusal
      (return-from check-website
        (result "unverified" refusal
                (jobj "url" url "content_expectation" (or needle +null+)) "website-get")))
    (handler-case
        (multiple-value-bind (text status truncated) (fetch-url url +website-deadline+)
          (let ((evidence (jobj "url" url
                                "content_expectation" (or needle +null+)
                                "status" (if (integerp status) status +null+))))
            (cond (truncated
                   (result "unverified" "response exceeds the read cap"
                           evidence "website-get"))
                  ((not (integerp status))
                   (result "unverified" "no HTTP status" evidence "website-get"))
                  ((/= status expected)
                   (result "failed" (format nil "status ~d, expected ~d" status expected)
                           evidence "website-get"))
                  ((not (stringp text))
                   (result "unverified" "response body unavailable" evidence "website-get"))
                  ((and needle (search needle text))
                   (result "passed" "status and content matched" evidence "website-get"))
                  (needle (result "failed" "status matched but required content absent"
                                  evidence "website-get"))
                  (t (result "passed" "status matched" evidence "website-get")))))
      (error (condition)
        (result "unverified" (format nil "request error: ~a" (type-of condition))
                (jobj "url" url) "website-get")))))

;;; --- all of them --------------------------------------------------------------

(defun evaluate-checks (spec)
  "Every check in SPEC evaluated now, in the order written."
  (loop for check in (getf spec :checks)
        collect (cond ((equal (getf check :kind) "file") (check-file check spec))
                      ((equal (getf check :kind) "test") (check-test check))
                      (t (check-website check spec)))))

(defun aggregate-state (states)
  "FAILED wins; otherwise a gap is UNVERIFIED; VERIFIED only when every
check passed."
  (cond ((null states) "unverified")
        ((member "failed" states :test #'equal) "failed")
        ((every (lambda (state) (equal state "passed")) states) "verified")
        (t "unverified")))
