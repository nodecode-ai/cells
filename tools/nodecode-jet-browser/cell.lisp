;;;; cell.lisp --- Jet Browser's acceptance check, in a network-isolated container.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of the Hermes integration in masakaai/jet-browser
;;;; (integrations/hermes, Apache-2.0). The original gave a model one tool,
;;;; jet_browser_verify, that ran Jet Browser's own bounded acceptance check:
;;;; start the project's digest-pinned image with networking off, type into
;;;; the fixture through native input events, read the DOM back, capture a
;;;; PNG, and remove the container. Nothing here is narrower than that.
;;;;
;;;; What runs: `docker run' with a fixed argument vector, no shell, and no
;;;; mount of anything of the user's; the container has no network, only
;;;; SETUID and SETGID, no-new-privileges, and memory, CPU, PID and shm
;;;; limits. It talks to the cell in Jet Browser's ordered JSONL protocol
;;;; over stdin/stdout, one request and one answer. Only the page the image
;;;; serves itself is loaded. Everything of the user's stays outside.
;;;;
;;;; Docker access is root-equivalent on the host, which is why the section
;;;; asks which image to run rather than guessing: the operator names it,
;;;; pinned by digest, once.

(defpackage #:nodecode-jet-browser
  (:use #:cl))

(in-package #:nodecode-jet-browser)

(nlk:define-peripheral jet-browser :not-running t)

;;; --- what the check is, as data ------------------------------------------------

(defparameter +image+
  (concatenate 'string
               "ghcr.io/masakaai/jet-browser@sha256:"
               "540a1fec531bf71b62c2c2d030c033249955ffc192ab7e0e7a1ac8d8412a661b")
  "The image the project publishes, pinned by digest: what the section's guide
names, and what the check runs when `image' answers with nothing.")

(defparameter +target+ "http://127.0.0.1:8080/"
  "The page the image serves itself: its built-in acceptance fixture.")

(defparameter +title+ "Jet Browser Ready"
  "The fixture's title, which the check reads back out of the DOM.")

(defparameter +typed+ "open-source runtime"
  "The text the check types through native pointer and text events, then reads
back out of the fixture's two fields.")

(defparameter +expression+
  "(()=>({value:document.querySelector('#message').value,result:document.querySelector('#result').value}))()"
  "The one expression the check evaluates: the fixture's typed field and the
field the fixture writes the confirmation into.")

(defparameter +line-limit+ (* 12 1024 1024)
  "How long one JSONL answer may be, in characters: a screenshot is base64.")

(defparameter +png-magic+ #(137 80 78 71 13 10 26 10)
  "A PNG's first eight octets.")

;;; --- small things --------------------------------------------------------------

(defun cut (text limit)
  "TEXT's first LIMIT characters, saying so when it was cut."
  (if (> (length text) limit)
      (concatenate 'string (subseq text 0 limit) "...")
      text))

(defun later (seconds)
  "The internal-real-time moment SECONDS from now."
  (+ (get-internal-real-time) (* seconds internal-time-units-per-second)))

(defun left (deadline)
  "The seconds still to run before DEADLINE, never less than a tenth."
  (max 0.1 (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))

(defun bounded (deadline seconds)
  "SECONDS, or what is left of DEADLINE when that is less."
  (min seconds (left deadline)))

(defun log-tail (path &optional (limit 4000))
  "The last LIMIT characters of the file at PATH, or NIL: what the docker
client said while the check ran."
  (let ((text (and path (probe-file path) (ignore-errors (uiop:read-file-string path)))))
    (and text (plusp (length text)) (subseq text (max 0 (- (length text) limit))))))

(defun image ()
  "The image this check runs: the section's, else the project's pin."
  (or (setting :image) +image+))

(defun screenshot-path ()
  "Where to keep the captured PNG, or NIL when the section names nowhere."
  (let ((path (setting :screenshot-file)))
    (cond ((null path) nil)
          ((stringp path) (and (plusp (length (nlk:trimmed path))) path))
          (t path))))

;;; --- base64, and a PNG --------------------------------------------------------

(defparameter +base64+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The base64 alphabet the protocol carries a screenshot in.")

(defun base64-decode (text)
  "TEXT as the octets it spells. Anything but base64 is a refusal."
  (let ((octets (make-array (floor (* 3 (length text)) 4)
                             :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0))
        (buffer 0)
        (bits 0))
    (loop for char across text
          do (case char
               ((#\Space #\Newline #\Return #\Tab #\=) nil)
               (t (let ((value (position char +base64+)))
                    (unless value
                      (fail "the screenshot carried ~s, which is not base64" char))
                    (setf buffer (logior (ash buffer 6) value)
                          bits (+ bits 6))
                    (when (>= bits 8)
                      (decf bits 8)
                      (vector-push (ldb (byte 8 bits) buffer) octets))))))
    (copy-seq octets)))

(defun digest (octets)
  "OCTETS' sha256 as bare lowercase hex, which is how the original answered it."
  (let ((text (nlk:sha256-text octets)))
    (if (and (> (length text) 7) (string= "sha256:" text :end2 7))
        (subseq text 7)
        text)))

(defun png-p (octets)
  "Whether OCTETS begin as a PNG does."
  (and (>= (length octets) (length +png-magic+))
       (loop for index below (length +png-magic+)
             always (= (aref octets index) (aref +png-magic+ index)))))

(defun write-screenshot (octets path)
  "Write OCTETS at PATH, answering its name; the setting's own words."
  (let ((pathname (uiop:parse-native-namestring path)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                  :if-does-not-exist :create
                                  :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    path))

;;; --- the transport: one request, one answer ------------------------------------

(defstruct (browser (:constructor make-browser (&key name send close log)))
  "One run's Jet Browser. SEND takes (COMMAND TIMEOUT) and answers the decoded
answer; CLOSE ends the client and answers its exit status; LOG is where the
client's own words went, kept only when a run fails."
  (name nil :read-only t)
  (send nil :read-only t)
  (close nil :read-only t)
  (log nil :read-only t))

(defun command-object (op fields)
  "The JSON object one request is: OP and FIELDS, a plist of members."
  (let ((object (nlk:json-object "op" op)))
    (loop for (key value) on fields by #'cddr
          do (setf (gethash key object) value))
    object))

(defun browser-call (browser op &key fields (timeout 20))
  "Ask BROWSER to do OP and answer the VALUE of its answer; a refusal in the
answer, or an answer that is not one, is this cell's refusal."
  (let ((answer (funcall (browser-send browser) (command-object op fields) timeout)))
    (unless (and (hash-table-p answer) (nlk:json-value answer :boolean "ok"))
      (fail "Jet Browser refused ~a: ~a" op
            (or (and (hash-table-p answer) (nlk:json-value answer :string "error"))
                "no answer")))
    (nlk:json-value answer :any "value")))

(defun read-line-within (stream seconds)
  "One line off STREAM within SECONDS: (values LINE NIL), or (values \"\" T)
when the stream ended first. A line over LINE-LIMIT is a refusal."
  (let ((out (make-string-output-stream))
        (deadline (later seconds))
        (count 0))
    (loop
      (if (listen stream)
          ;; a character is there: take it, and do not wait for the next
          (let ((char (read-char stream nil nil)))
            (cond ((null char) (return (values (get-output-stream-string out) t)))
                  ((char= char #\Newline) (return (values (get-output-stream-string out) nil)))
                  (t (write-char char out)
                     (when (> (incf count) +line-limit+)
                       (fail "Jet Browser answered with a line over ~:d characters"
                             +line-limit+)))))
          (progn
            (when (>= (get-internal-real-time) deadline)
              (fail "Jet Browser said nothing for ~,1f seconds" seconds))
            (sleep 0.02))))))

(defun docker-send (in out log)
  "How one request reaches the container: a line on IN, a line off OUT, the
docker client's own words at LOG for when it ends first."
  (lambda (command timeout)
    (write-string (nlk:encode-json-object command) in)
    (write-char #\Newline in)
    (finish-output in)
    (multiple-value-bind (line ended) (read-line-within out timeout)
      (when (or ended (zerop (length line)))
        (fail "Jet Browser stopped before answering ~a~@[: ~a~]~@[ (its client's log: ~a)~]"
              (nlk:json-value command :string "op") (log-tail log) log))
      (handler-case (nlk:decode-json line :whole t)
        (error ()
          (fail "Jet Browser answered ~s, which is not one JSON line" (cut line 200)))))))

(defun docker-close (process in name)
  "How one run ends: the client, then only the container this run named when
the client did not exit cleanly."
  (lambda ()
    (let ((status nil))
      (ignore-errors (close in))
      (loop repeat 50
            while (ignore-errors (uiop:process-alive-p process))
            do (sleep 0.1))
      (when (ignore-errors (uiop:process-alive-p process))
        (ignore-errors (uiop:terminate-process process :urgent t)))
      (setf status (ignore-errors (uiop:wait-process process)))
      (unless (eql status 0)
        (ignore-errors
          (uiop:run-program (list (setting :docker) "rm" "-f" name)
                             :output :ignore :error-output :ignore
                             :ignore-error-status t)))
      status)))

;;; --- the container the section describes ---------------------------------------

(defun seccomp-profile ()
  "The seccomp profile the container runs under: the section's when it names
one, else the one shipped beside this file, else NIL and Docker's own."
  (let ((named (setting :seccomp-profile)))
    (cond ((stringp named) (and (plusp (length named)) named))
          ((pathnamep named) (uiop:native-namestring named))
          (t (let ((shipped (ignore-errors
                              (asdf:system-relative-pathname "nodecode-jet-browser"
                                                              "seccomp_profile.json"))))
               (and shipped (probe-file shipped) (uiop:native-namestring shipped)))))))

(defun docker-argv (name)
  "The one command a check runs: a fixed vector, no shell, nothing mounted."
  (let ((seccomp (seccomp-profile)))
    (append
     (list (setting :docker) "run" "--rm" "-i" "--name" name
           (format nil "--platform=~a" (setting :platform))
           "--network=none"
           "--cap-drop=ALL"
           "--cap-add=SETUID"
           "--cap-add=SETGID"
           "--security-opt=no-new-privileges"
           "--security-opt=systempaths=unconfined"
           "--security-opt=apparmor=unconfined")
     (when seccomp (list (format nil "--security-opt=seccomp=~a" seccomp)))
     (list (format nil "--memory=~a" (setting :memory))
           (format nil "--cpus=~a" (setting :cpus))
           "--pids-limit=256"
           "--shm-size=256m"
           "--env=JET_BROWSER_SMOKE=1"
           (image)))))

(defun new-container-name ()
  "A name only this run answers to."
  (format nil "nodecode-jet-browser-~(~8,'0x~)" (random 4294967296)))

(defun open-browser ()
  "Start the container and answer the BROWSER that talks to it."
  (let* ((name (new-container-name))
         (log (merge-pathnames (format nil "~a.log" name) (uiop:temporary-directory)))
         (argv (docker-argv name))
         (process (handler-case
                      (uiop:launch-program argv :input :stream :output :stream
                                                :error-output log
                                                :element-type 'character)
                    (error (condition)
                      (fail "cannot run `~a~{ ~a~}' -- is Docker installed and on PATH? (~a)"
                            (first argv) (rest argv) condition)))))
    (make-browser :name name :log log
                  :send (docker-send (uiop:process-info-input process)
                                      (uiop:process-info-output process)
                                      log)
                  :close (docker-close process (uiop:process-info-input process) name))))

(defparameter *open-browser* #'open-browser
  "How one check reaches a running Jet Browser: () => a BROWSER. This starts
the container; the tests bind it to a scripted one, so no test starts Docker.")

;;; --- the check ----------------------------------------------------------------

(defun document-ready (state)
  "Whether STATE is the fixture, loaded."
  (and (hash-table-p state)
       (equal (nlk:json-value state :string "url") +target+)
       (member (nlk:json-value state :string "readyState")
               '("interactive" "complete") :test #'equal)))

(defun snapshot-value (answer key)
  "ANSWER's VALUE under KEY, as the evaluate answer nests it: RESULT, then the
answer's VALUE, then the fixture's own object; a flatter answer reads too."
  (or (nlk:json-value answer :any "result" "value" key)
      (nlk:json-value answer :any "value" key)))

(defun exercise (browser deadline)
  "Drive one acceptance check over BROWSER, answering its evidence as a JSON
object: startup, native input, the DOM's agreement, the PNG, the duration."
  (let ((started (get-internal-real-time)))
    (browser-call browser "create"
                   :fields (list "proxy" nil "profile_dir" nil "page_load_strategy" "none")
                   :timeout (bounded deadline 30))
    (browser-call browser "navigate" :fields (list "url" +target+)
                   :timeout (bounded deadline 30))
    (let ((state nil) (ready nil))
      (loop repeat 30
            while (not ready)
            do (setf state (browser-call browser "document_state"
                                          :timeout (bounded deadline 5))
                     ready (document-ready state))
               (unless ready (sleep 0.1)))
      (unless ready (fail "the fixture page never became ready: ~s" state)))
    (let ((snapshot (browser-call browser "snapshot" :timeout (bounded deadline 20))))
      (unless (equal (nlk:json-value snapshot :string "title") +title+)
        (fail "the fixture's title was ~s, not ~s"
              (nlk:json-value snapshot :string "title") +title+))
      (browser-call browser "input"
                    :fields (list "events"
                                  (vector (nlk:json-object "type" "pointer" "phase" "down"
                                                            "x" 180 "y" 42 "button" 0)
                                          (nlk:json-object "type" "pointer" "phase" "up"
                                                            "x" 180 "y" 42 "button" 0)
                                          (nlk:json-object "type" "text" "text" +typed+)))
                    :timeout (bounded deadline 20))
      (let ((evaluation (browser-call browser "evaluate"
                                       :fields (list "expression" +expression+)
                                       :timeout (bounded deadline 20))))
        (let ((observed (snapshot-value evaluation "value"))
              (confirmed (snapshot-value evaluation "result")))
          (unless (equal observed +typed+)
            (fail "native input did not reach the fixture: its field holds ~s" observed))
          (unless (equal confirmed +typed+)
            (fail "the fixture did not confirm the input: it says ~s" confirmed)))
        (let* ((encoded (browser-call browser "screenshot" :timeout (bounded deadline 20)))
               (octets (and (stringp encoded) (base64-decode encoded))))
          (unless (and octets (>= (length octets) 1000) (png-p octets))
            (fail "the screenshot was not a PNG of at least a thousand octets"))
          (let ((path (screenshot-path)))
            (prog1 (nlk:json-object
             "success" t
             "network" "disabled"
             "image" (image)
             "title" +title+
             "url" +target+
             "input" +typed+
             "screenshot_bytes" (length octets)
             "screenshot_sha256" (digest octets)
             "duration_ms" (round (* 1000 (/ (- (get-internal-real-time) started)
                                              internal-time-units-per-second)))
             :when path "screenshot_file" (write-screenshot octets path))
              (browser-call browser "close" :timeout (bounded deadline 15)))))))))

(defun run-check ()
  "Run one check end to end and answer its evidence; the docker client's log
is kept only when the run failed, and the container is closed on every path."
  (let* ((browser (funcall *open-browser*))
         (close (browser-close browser))
         (result (unwind-protect (exercise browser (later (setting :timeout-seconds)))
                   (when close (funcall close)))))
    (ignore-errors (delete-file (browser-log browser)))
    result))

;;; --- what the model calls, and what the operator types --------------------------

(defparameter *verify-schema*
  (nlk:json-object
   "type" "object"
   "properties" (nlk:json-object)
   "additionalProperties" nil)
  "jet_browser_verify takes no arguments: what it runs is the section's.")

(defun tool-verify (arguments)
  "jet_browser_verify: the check, as JSON evidence; a failed check is JSON
carrying the reason, as the original answered it."
  (declare (ignore arguments))
  (handler-case
      (nlk:encode-json-object (run-check))
    (jet-browser-error (condition)
      (nlk:encode-json-object (nlk:json-object "success" nil
                                                "error" (princ-to-string condition))))
    (error (condition)
      (nlk:encode-json-object (nlk:json-object "success" nil
                                                "error" (format nil "~a: ~a"
                                                                (type-of condition)
                                                                condition))))))

(defun panel-run (label line &rest keys)
  "A row of the panel whose Enter runs LINE; KEYS the rest of a row's words."
  (list* :label label :run (list :enter line) keys))

(defun panel (summary)
  "What bare /jetbrowser answers: what it would run, over a row that runs it."
  (nle:list-dialog
   :title "Jet Browser" :summary summary
   :rows (list (panel-run "Run the check" "/jetbrowser run"
                          :detail "one container, its network off, about a minute"))))

(defun run-command (args session-id)
  "jetbrowser typed alone shows what it would run; `run` runs the check and
answers its evidence as one line."
  (declare (ignore session-id))
  (let ((args (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((zerop (length args))
       (let ((summary (format nil "~a, network off, ~as ceiling"
                              (image) (setting :timeout-seconds))))
         (values summary (panel summary))))
      ((member args '("run" "verify") :test #'equal)
       (handler-case
           (let ((evidence (run-check)))
             (format nil "Jet Browser verified: ~a, typed ~s, PNG ~:d octets (~a), ~:d ms~@[; written to ~a~]"
                     (nlk:json-value evidence :string "title")
                     (nlk:json-value evidence :string "input")
                     (nlk:json-value evidence :integer "screenshot_bytes")
                     (cut (nlk:json-value evidence :string "screenshot_sha256") 16)
                     (nlk:json-value evidence :integer "duration_ms")
                     (nlk:json-value evidence :string "screenshot_file")))
         (jet-browser-error (condition)
           (format nil "jet-browser: ~a" condition))))
      (t (let ((summary (format nil "~a is not one of its choices" args)))
           (values summary (panel summary)))))))

;;; --- the manual -----------------------------------------------------------------

(defparameter +manual+
  "Jet Browser is a WPE WebKit browser whose runtime the project ships as one
digest-pinned container. This cell runs its own acceptance check there and
answers the evidence: the fixture's title and URL, the text native input typed
into its DOM, and the captured PNG's length and sha256.
  /jetbrowser         what the check would run, and one row that runs it
  /jetbrowser run     run it: one container, its network off, about a minute
  jet_browser_verify  the same, as the tool a model calls; it takes no arguments
What runs is the section's `image', pinned by digest, with a fixed docker
argument vector: no shell, nothing of yours mounted, its network off, only
SETUID and SETGID, no-new-privileges, and memory, CPU and PID limits. Docker
itself reaches the registry the first time an image is not local. Docker
access is root-equivalent on the host; that is the price of the check.
When a run fails the docker client's words are kept in the log file the answer
names. Set `screenshot_file' to keep the PNG the check captured."
  "What (help :jet-browser) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell jet-browser
  (:section ("jet-browser")
    (:guide "image is the digest-pinned Jet Browser the project publishes, one line to paste; docker is the CLI on your PATH; next step: /jetbrowser run")
    ("docker" :string :default "docker"
     :doc "the docker CLI to run, as PATH finds it")
    ("image" :string :required t
     :doc "the Jet Browser image to run, pinned by digest (the project publishes one; names the container that runs)")
    ("platform" :string :default "linux/amd64"
     :doc "the platform the image runs as")
    ("timeout_seconds" :integer :default 180 :min 30
     :doc "one whole verification's ceiling, in seconds")
    ("memory" :string :default "1g"
     :doc "the container's memory limit")
    ("cpus" :string :default "2"
     :doc "the container's CPU limit")
    ("seccomp_profile" :path
     :doc "a seccomp profile for the container; empty uses the one shipped beside this cell, and an empty name Docker's own")
    ("screenshot_file" :path
     :doc "where to write the PNG the check captures; empty keeps only its sha256"))
  (:tool "jet_browser_verify"
         (list :description
               "Run Jet Browser's bounded, network-isolated acceptance check: the pinned WPE WebKit container starts, takes native input, reports matching DOM state, captures a PNG, and is removed. Takes no arguments; the section names the image. Answers JSON evidence (title, URL, the typed text, the PNG's byte count and sha256, milliseconds) or JSON carrying the error."
               :parameters *verify-schema*)
         'tool-verify)
  (:command "jetbrowser" 'run-command
            :description "Run Jet Browser's own acceptance check in a container"
            :argument-hint "[run]")
  (:help :jet-browser
         "jet-browser: Jet Browser's acceptance check in a network-isolated container - /jetbrowser run"
         +manual+)
  (:start (lambda ()
            (unless (and (setting :image) (plusp (length (setting :image))))
              (error 'nlk:config-refusal
                     :detail (format nil "the jet-browser section needs `image': the digest-pinned Jet Browser to run, e.g. ~a"
                                     +image+))))))
