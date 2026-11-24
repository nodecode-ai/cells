;;;; gate.lisp --- gates, the state a gate shows Jev, the decision, the log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from superlowburn's hermes-jev-cron-gate (see NOTICE): its
;;;; gate.py (build_state, ask_jev, decide, main) and tools.py (the config
;;;; file, the state file, the decision log, setup/test/status/remove).
;;;;
;;;; A gate is a name, one yes/no question phrased so TRUE means the run is
;;;; worth doing, and the sources Jev reads to answer it: a file or its tail,
;;;; whether paths changed since the last wake, a localhost URL's body, or
;;;; static text. A scheduled run asks (cron-gate:check "name") at its top;
;;;; TypeSafe's System One scores the question over that snapshot and the
;;;; answer is :wake or :skip. Fail-open everywhere: no key, no config, a
;;;; network error or an answer of the wrong shape all wake the run. After
;;;; `max_consecutive_skips' skips in a row the gate wakes anyway, so a job
;;;; cannot stall silently. Every decision is one line of log.jsonl, which is
;;;; what (cron-gate:gates) counts.

(in-package #:nodecode-cron-gate)

;;; --- the words and the defaults ----------------------------------------------

(defparameter +base+ "https://api.typesafe.ai"
  "TypeSafe's public API root; the section's base_url wins over it.")

(defparameter +route+ "/v1/systemone"
  "The System One judgment route under the base.")

(defparameter +key-env+ "TYPESAFE_API_KEY"
  "The environment variable a key is read from when the section names none.")

(defparameter +other-key-env+ "JEV_API_KEY"
  "The original's second name for the same key.")

(defparameter +default-question+
  "Running the agent now would make real new progress, rather than re-reporting that it is still waiting on the same thing."
  "What a gate asks when its definition names no question.")

(defparameter +name-chars+ "abcdefghijklmnopqrstuvwxyz0123456789_-"
  "What a gate name is made of: 1 to 40 of these.")

(defparameter +default-skip-below+ 0.2
  "Skip when P(useful) is under this.")

(defparameter +default-max-skips+ 4
  "Force a wake after this many skips in a row.")

;;; --- where a gate lives ---------------------------------------------------------

(defun gate-path (name)
  "The config a gate NAME was defined by, under the home."
  (nlk:home (format nil "cron-gate/~a.json" name)))

(defun state-path (name)
  "The skips and the last wake of gate NAME, beside its config."
  (nlk:home (format nil "cron-gate/~a.state.json" name)))

(defun log-path ()
  "One JSON line per decision, every gate in one file."
  (nlk:home "cron-gate/log.jsonl"))

(defun gate-names ()
  "Every defined gate, by name, sorted."
  (let ((dir (nlk:home "cron-gate/")))
    (sort (loop for path in (ignore-errors (uiop:directory-files dir))
                for name = (pathname-name path)
                when (and name (string= "json" (or (pathname-type path) ""))
                          (not (search ".state" name)))
                  collect name)
          #'string<)))

(defun read-json-file (path)
  "PATH decoded, or NIL when it is absent or is not JSON."
  (when (probe-file path)
    (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))

(defun write-text (path text)
  "TEXT into PATH, making its directory."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text out))
  path)

;;; --- JSON, the little of it this cell writes -----------------------------------------

(defun json-quote (text)
  "TEXT as a JSON string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across (princ-to-string (or text ""))
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code character) 32)
                      (format out "\\u~4,'0x" (char-code character))
                      (write-char character out)))))
    (write-char #\" out)))

(defun json-list (texts)
  "TEXTS as a JSON array of strings."
  (format nil "[~{~a~^,~}]" (mapcar #'json-quote texts)))

;;; --- a gate's definition -----------------------------------------------------------

(defun gate-name (name)
  "NAME, lowercased and checked, or a refusal naming what a name is."
  (let ((name (string-downcase (nlk:trimmed (princ-to-string (or name ""))))))
    (unless (and (<= 1 (length name) 40)
                 (every (lambda (character) (find character +name-chars+ :test #'char=)) name))
      (fail "a gate name is 1 to 40 characters of a-z, 0-9, _ and -; ~s is not one" name))
    name))

(defun paths-of (value)
  "VALUE as a list of path strings: one string, a list, or a JSON array."
  (cond ((null value) nil)
        ((stringp value) (list value))
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (fail "a :changed source takes path strings"))))

(defun source-of (object)
  "OBJECT, a decoded JSON source, as the plist this cell runs on."
  (let ((source '()))
    (flet ((take (key member)
             (let ((value (nlk:json-value object :any member)))
               (unless (eq value :null)
                 (setf source (append source (list key value)))))))
      (take :file "file")
      (take :changed "changed")
      (take :url "url")
      (take :text "text")
      (take :tail "tail"))
    source))

(defun normalize-sources (sources)
  "SOURCES as a list of source plists: as written in Lisp, or as JSON text,
or as the array a tool call carries. Each is one of :file, :changed, :url
and :text."
  (let* ((given (cond ((null sources) nil)
                       ((stringp sources) (or (ignore-errors (nlk:decode-json sources))
                                              (fail "sources is not JSON: ~a" sources)))
                       (t sources)))
         (list (cond ((null given) nil)
                     ((hash-table-p given) (list given))
                     ((vectorp given) (coerce given 'list))
                     ((listp given) given)
                     (t (fail "sources is a list of sources")))))
    (mapcar (lambda (source)
              (let ((source (if (hash-table-p source) (source-of source) source)))
                (unless (some (lambda (key) (getf source key))
                              '(:file :changed :url :text))
                  (fail "a source is (:file PATH [:tail N]), (:changed (PATHS)), (:url URL) or (:text TEXT); ~s is none"
                        source))
                source))
            list)))

(defun source-json (source)
  "SOURCE as one JSON object, members in a settled order."
  (let ((fields '()))
    (flet ((field (key text) (push (cons key text) fields)))
      (cond
        ((getf source :file)
         (field "file" (json-quote (getf source :file)))
         (when (getf source :tail) (field "tail" (princ-to-string (getf source :tail)))))
        ((getf source :changed)
         (field "changed" (json-list (paths-of (getf source :changed))))
         (when (getf source :tail) (field "tail" (princ-to-string (getf source :tail)))))
        ((getf source :url)
         (field "url" (json-quote (getf source :url)))
         (when (getf source :tail) (field "tail" (princ-to-string (getf source :tail)))))
        (t (field "text" (json-quote (getf source :text))))))
    (format nil "{~{~a~^,~}}"
            (loop for (key . text) in (nreverse fields)
                  collect (format nil "~a:~a" (json-quote key) text)))))

(defun read-config (name)
  "Gate NAME's definition as a plist, or NIL when it has none."
  (let ((json (read-json-file (gate-path name))))
    (when (and json (plusp (hash-table-count json)))
      (list :question (nlk:json-value json :string "question")
            :sources (normalize-sources (nlk:json-array json "sources"))
            :skip-below (or (nlk:json-value json :number "skip_below") +default-skip-below+)
            :max-skips (or (nlk:json-value json :integer "max_consecutive_skips") +default-max-skips+)))))

(defun write-config (name question sources skip-below max-skips)
  (write-text (gate-path name)
              (format nil "{\"question\":~a,\"sources\":[~{~a~^,~}],\"skip_below\":~a,\"max_consecutive_skips\":~d}"
                      (json-quote question)
                      (mapcar #'source-json sources)
                      skip-below max-skips)))

;;; --- the snapshot Jev reads -----------------------------------------------------------

(defun expand-home (path)
  "PATH with a leading ~ expanded; a plain path kept."
  (let ((path (princ-to-string path)))
    (if (and (plusp (length path)) (char= #\~ (char path 0)))
        (namestring (merge-pathnames (subseq path (min 2 (length path))) (user-homedir-pathname)))
        path)))

(defun newest-write (path)
  "The newest file-write-date under PATH, a file or a directory, or 0; a
directory's dot entries and node_modules are not walked."
  (let* ((path (expand-home path))
         (directory (ignore-errors (uiop:directory-exists-p path))))
    (or (if directory
            (loop for file in (ignore-errors (uiop:directory-files path))
                  maximize (or (ignore-errors (file-write-date file)) 0))
            (ignore-errors (file-write-date path)))
        0)))

(defun tail-string (text limit)
  "TEXT's last LIMIT characters (the original keeps the tail)."
  (let ((text (princ-to-string (or text ""))))
    (if (and limit (> (length text) limit))
        (subseq text (- (length text) limit))
        text)))

(defun url-host (url)
  "URL's host, lowercased, without userinfo or port."
  (let* ((url (princ-to-string url))
         (start (if (search "://" url) (+ (search "://" url) 3) 0))
         (rest (subseq url start))
         (end (or (position-if (lambda (character) (find character "/?#" :test #'char=)) rest)
                  (length rest)))
         (host (subseq rest 0 end)))
    (let ((at (position #\@ host)))
      (when at (setf host (subseq host (1+ at)))))
    (if (and (plusp (length host)) (char= #\[ (char host 0)))
        (subseq host 1 (or (position #\] host) (length host)))
        (let ((colon (position #\: host)))
          (string-downcase (if colon (subseq host 0 colon) host))))))

(defun localhost-url-p (url)
  "Whether URL answers on this machine only: the original's rule."
  (member (url-host url) '("127.0.0.1" "localhost" "::1") :test #'string=))

(defun fetch-url (url tail)
  "URL's body, at most TAIL characters, by an HTTP GET."
  (multiple-value-bind (body status)
      (nlk:http :get url :timeout (setting :timeout-seconds)
                      :connect-timeout (setting :timeout-seconds))
    (if (and status (<= 200 status 299))
        (tail-string (nlk:body-text body) tail)
        (format nil "(HTTP ~a)" status))))

(defun source-text (source last-wake)
  "One source of the snapshot, as the text Jev reads."
  (cond
    ((getf source :file)
     (format nil "## ~a~%~a" (getf source :file)
             (tail-string (uiop:read-file-string (expand-home (getf source :file)))
                          (getf source :tail))))
    ((getf source :changed)
     (format nil "## Changed since the agent last ran~%{~{~a~^, ~}}"
             (loop for path in (paths-of (getf source :changed))
                   collect (format nil "~a: ~a" (json-quote path)
                                   (if (> (newest-write path) (or last-wake 0)) "true" "false")))))
    ((getf source :url)
     (let ((url (getf source :url)))
       (if (localhost-url-p url)
           (format nil "## ~a~%~a" url (fetch-url url (or (getf source :tail) 4000)))
           (format nil "## ~a~%(skipped: only localhost URLs are allowed)" url))))
    ((getf source :text) (princ-to-string (getf source :text)))
    (t "## source error: empty")))

(defun build-state (cfg last-wake)
  "The snapshot: every source in order, then the hours since the last wake,
capped at the section's max_state_chars from the end."
  (let ((parts '()))
    (dolist (source (getf cfg :sources))
      (push (handler-case (source-text source last-wake)
              (error (condition) (format nil "## source error: ~a" (type-of condition))))
            parts))
    (push (format nil "## Hours since the agent last ran~%~a"
                  (if last-wake
                      (format nil "~,1f" (/ (- (get-universal-time) last-wake) 3600.0))
                      "never"))
          parts)
    (tail-string (format nil "~{~a~^~2%~}" (nreverse parts)) (setting :max-state-chars))))

;;; --- one question to Jev --------------------------------------------------------------

(defun api-key ()
  "The key a request sends: the section's api_key, then api_key_env, then
the original's two variable names; NIL when none is set."
  (or (let ((key (setting :api-key))) (and key (plusp (length key)) key))
      (let ((name (or (let ((name (setting :api-key-env))) (and name (plusp (length name)) name))
                      +key-env+)))
        (nle::credential-env name))
      (nle::credential-env +other-key-env+)))

(defun base-url ()
  "The section's base_url, else TypeSafe's public root; no trailing slash."
  (string-right-trim "/" (let ((base (setting :base-url)))
                            (if (and base (plusp (length base))) base +base+))))

(defun model ()
  "The section's model, else jev-latest."
  (let ((model (setting :model)))
    (if (and model (plusp (length model))) model "jev-latest")))

(defun request-body (state question)
  "The System One request: one noul question over STATE."
  (format nil "{\"model\":~a,\"state\":~a,\"questions\":{\"useful\":{\"type\":\"noul\",\"instructions\":~a}}}"
          (json-quote (model)) (json-quote state) (json-quote question)))

(defun ask-jev (state question)
  "(values SCORE ERROR): the probability that QUESTION is true of STATE, or
NIL and why not. Never signals: every failure is a reason to wake."
  (let ((key (api-key)))
    (if (null key)
        (values nil (format nil "no key (set ~a, or the section's api_key)" +key-env+))
        (handler-case
            (multiple-value-bind (body status)
                (nlk:http :post (format nil "~a~a" (base-url) +route+)
                          :headers `(("authorization" . ,(format nil "Bearer ~a" key))
                                     ("accept" . "application/json")
                                     ("content-type" . "application/json"))
                          :content (request-body state question)
                          :timeout (setting :timeout-seconds)
                          :connect-timeout (setting :timeout-seconds))
              (if (and status (<= 200 status 299))
                  (let* ((answer (ignore-errors
                                   (nlk:json-value (nlk:decode-json (nlk:body-text body))
                                                   :object "answers" "useful")))
                         (score (and answer (ignore-errors (nlk:json-value answer :number "noul")))))
                    (if (numberp score)
                        (values (float score 1d0) nil)
                        (values nil "jev answered nothing for the useful question")))
                  (values nil (format nil "jev error (HTTP ~a)" status))))
          (cron-gate-error (condition) (values nil (format nil "jev error: ~a" condition)))
          (error (condition) (values nil (format nil "jev error: ~a" (type-of condition))))))))

;;; --- the decision -----------------------------------------------------------------------

(defun read-state (name)
  "Gate NAME's counters: the skips in a row and when it last woke."
  (let ((json (read-json-file (state-path name))))
    (list :skips (or (and json (nlk:json-value json :integer "skips")) 0)
          :last-wake (and json (nlk:json-value json :integer "last_wake")))))

(defun write-state (name skips last-wake)
  (write-text (state-path name)
              (format nil "{\"skips\":~d,\"last_wake\":~d}~%" skips (or last-wake 0))))

(defun timestamp ()
  "Now, as the log writes it."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun log-decision (name wake score note start)
  "One line for one decision, appended to the one log."
  (let ((path (log-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "{\"ts\":~a,\"gate\":~a,\"wake\":~a,\"score\":~a,\"note\":~a,\"ms\":~d}~%"
              (json-quote (timestamp)) (json-quote name)
              (if wake "true" "false")
              (if (numberp score) (format nil "~,3f" score) "null")
              (json-quote note)
              (round (* 1000 (- (get-internal-real-time) start))
                     internal-time-units-per-second)))))

(defun decide (name)
  "(values WAKE NOTE SCORE): whether a run should happen now, why, and the
answer that said so. A gate with no config, no key, a broken source or an
error from Jev wakes: a gate that can stop a run when it is broken is worse
than no gate."
  (let ((cfg (read-config name)))
    (if (null cfg)
        (values t "no gate config; waking" nil)
        (let* ((state (read-state name))
               (start (get-internal-real-time))
               (snapshot (build-state cfg (getf state :last-wake))))
          (multiple-value-bind (score error) (ask-jev snapshot (getf cfg :question))
            (let* ((wake (or (null score) (>= score (getf cfg :skip-below))))
                   (skips (if wake 0 (1+ (getf state :skips))))
                   (note (or error (format nil "useful=~,2f" score))))
              (when (> skips (getf cfg :max-skips))
                (setf wake t skips 0 note (format nil "~a (skip cap; waking)" note)))
              (write-state name skips (if wake (get-universal-time) (getf state :last-wake)))
              (log-decision name wake score note start)
              (values wake note score)))))))

;;; --- the verbs -----------------------------------------------------------------------------

(defun ensure-running ()
  "A setting read, so a verb called with nothing started refuses instead of
writing into a home nothing configured."
  (setting :model))

(defun gate (name &key question sources (skip-below +default-skip-below+)
                      (max-skips +default-max-skips+))
  "Define or update gate NAME: QUESTION is the statement Jev scores, phrased
so TRUE means the run is worth doing; SOURCES is what Jev reads, a list of
(:file PATH [:tail N]), (:changed (PATHS)) and (:url URL) and (:text TEXT);
SKIP-BELOW a probability; MAX-SKIPS how many skips in a row before a run is
forced. Answers a line about the gate."
  (ensure-running)
  (let ((name (gate-name name))
        (question (nlk:trimmed (princ-to-string (or question +default-question+))))
        (sources (normalize-sources sources))
        (skip-below (or skip-below +default-skip-below+))
        (max-skips (or max-skips +default-max-skips+)))
    (when (zerop (length question))
      (fail "a gate needs a question: one statement about the state, TRUE when the run is worth doing"))
    (unless (and (realp skip-below) (<= 0 skip-below 1))
      (fail "skip_below is a probability from 0 to 1; ~a is not one" skip-below))
    (unless (and (integerp max-skips) (plusp max-skips))
      (fail "max_consecutive_skips is a whole number above 0; ~a is not one" max-skips))
    (write-config name question sources skip-below max-skips)
    (format nil "cron-gate ~a: ~d source~:p, skips below ~,2f, wakes anyway after ~d skip~:p. A run asks (cron-gate:check ~s) at its top: :skip means it can answer \"nothing to do\" without the work."
            name (length sources) skip-below max-skips name)))

(defun check (name)
  "Decide gate NAME now, log it, and answer what the run should do, as text
and :wake or :skip."
  (ensure-running)
  (let* ((name (gate-name name)))
    (multiple-value-bind (wake note) (decide name)
      (values (format nil "cron-gate ~a: ~a, ~a" name (if wake "wake" "skip") note)
              (if wake :wake :skip)))))

(defun log-records (&optional name)
  "The decision log's lines, oldest first, narrowed to gate NAME."
  (let ((path (log-path)))
    (when (probe-file path)
      (loop for line in (uiop:read-file-lines path)
            for json = (ignore-errors (nlk:decode-json line))
            when (and json (or (null name) (equal name (nlk:json-value json :string "gate"))))
              collect json))))

(defun stats-line (name)
  "Gate NAME's record from the log, in one line."
  (let* ((records (log-records name))
         (runs (length records)))
    (if (zerop runs)
        "no decisions yet"
        (let* ((skips (count-if-not (lambda (record) (nlk:json-value record :boolean "wake")) records))
               (errors (count-if (lambda (record) (eq :null (nlk:json-value record :any "score"))) records))
               (last (car (last records))))
          (format nil "~d run~:p, ~d skipped (~d%)~@[, ~d with no answer~]~@[; last: ~a~]"
                  runs skips (round (* 100 skips) runs)
                  (and (plusp errors) errors)
                  (nlk:json-value last :string "note"))))))

(defun gates ()
  "Every gate, what it has decided, and where its definition lives."
  (ensure-running)
  (let ((names (gate-names)))
    (if (null names)
        "cron-gate: no gates yet. (cron-gate:gate \"heartbeat\" :question \"Running now would make progress.\" :sources '((:file \"NOTES.md\" :tail 900))) makes one."
        (with-output-to-string (out)
          (format out "cron-gate: ~d gate~:p~%" (length names))
          (dolist (name names)
            (format out "  ~a: ~a; skips below ~,2f, wakes anyway after ~d skip~:p~%"
                    name (stats-line name)
                    (or (getf (read-config name) :skip-below) +default-skip-below+)
                    (or (getf (read-config name) :max-skips) +default-max-skips+)))
          (format out "log: ~a" (log-path))))))

(defun forget (name)
  "Forget gate NAME: its definition and its counters, never the log."
  (ensure-running)
  (let* ((name (gate-name name))
         (removed (loop for path in (list (gate-path name) (state-path name))
                        when (probe-file path) count (progn (delete-file path) t))))
    (if (plusp removed)
        (format nil "cron-gate ~a: forgotten (~d file~:p); its line~:p stay in the log" name removed)
        (format nil "cron-gate ~a: no such gate" name))))
