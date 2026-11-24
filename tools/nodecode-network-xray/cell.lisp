;;;; cell.lisp --- the four calls the plugin gave a model, and the one
;;;; question the operator asks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; network_xray_probe walks one URL from this machine and saves the
;;;; evidence; network_xray_import records a bundle a peer produced;
;;;; network_xray_compare reads two saved records side by side; and
;;;; network_xray_history lists what this profile holds. /network-xray does
;;;; the same from the operator's side and prints the ladder as lines.
;;;;
;;;; A probe runs against THIS machine, whatever a front end sits on, so the
;;;; label says which vantage it is and a second vantage is either another
;;;; Nodecode's record copied over or a bundle its collector wrote.

(in-package #:nodecode-network-xray)

(nlk:define-peripheral network-xray :not-running t)

;;; --- the four calls a model makes ------------------------------------------------

(nlk:define-contract xray-probe
  (:doc "Walk one HTTP(S) URL from THIS machine and save the evidence: the addresses
DNS answered, the one address connected, whether the certificate verified over
TLS, and the status line of one HEAD. Direct, so proxy settings and browser
trust stores do not apply; no query string, credential, redirect followed or
response body. Private and loopback targets are allowed -- this is not a
scanner. Ask the user which URL and which machine they mean before a probe,
and read a collector timeout as inconclusive, never as proof of a DNS failure.")
  ("url" :text :required t
         :doc "The exact http:// or https:// URL to probe, without query or fragment.")
  ("label" :text
           :doc "Which machine this probe speaks for, e.g. 'laptop at home'. Defaults to the cell's vantage setting.")
  ("connect_ip" :text
                :doc "A numeric address to connect instead of the first DNS answer, an explicit manual experiment: the original Host, TLS server name and certificate verification are kept."))

(nlk:define-contract xray-import
  (:doc "Import a Network X-ray bundle another machine's collector wrote, at most 128
KiB. Its schema and content digest are checked and it is saved as an imported,
unauthenticated recorded observation -- a digest checks content, never origin.
Do not import a bundle that carries secrets, and do not call an imported probe
one that ran here.")
  ("bundle_json" :text :required t
                 :doc "The bundle's own JSON text, as the collector wrote it."))

(nlk:define-contract xray-compare
  (:doc "Compare two saved probes of the same URL for the same question from two
vantage points. Answers the stage differences, a verdict, hypotheses worded as
hypotheses, at most one manual follow-up and the limitations. A difference is
not proof of a root cause, and no suggested test is run.")
  ("left_id" :text :required t :doc "One saved probe's exact id, as the history lists it.")
  ("right_id" :text :required t :doc "The other saved probe's exact id."))

(nlk:define-contract xray-history
  (:doc "List the newest saved Network X-ray probes in this profile: id, vantage
label, URL, capture time, outcome and whether its digest is verified. No
network call.")
  ("limit" :integer
           :doc "The most records to list; the section's max_records when absent, 50 at most by default."))

(defparameter *label-limit* 48 "The longest vantage label one probe carries.")

(defun checked-label (text)
  "TEXT as a vantage label: 1..48 printable characters."
  (let ((label (nlk:trimmed (or text ""))))
    (unless (<= 1 (length label) *label-limit*)
      (fail "a vantage label is 1..~d characters: it says which machine this probe speaks for"
            *label-limit*))
    (unless (every #'graphic-char-p label)
      (fail "a vantage label carries printable characters only"))
    label))

(defun probe (url &key label connect-ip)
  "Probe URL from this machine under LABEL, saving the evidence => the record
the answer carries. No proxy, no redirect followed, no body, no credential;
one address per run, and the certificate verified."
  (running-settings)
  (let* ((label (checked-label (or label (setting :vantage))))
         (target (parse-target url connect-ip))
         (bundle (collect target label)))
    (save-record bundle "local")))

(defparameter *bundle-limit* 131072 "The most bytes of a bundle one import reads.")
(defparameter *bundle-keys*
  '("schema_version" "kind" "id" "captured_at" "vantage" "target" "policy"
    "capture" "stages" "integrity")
  "The members a bundle may carry: anything else is not one of these records.")

(defun bundle-refusal (bundle)
  "Why BUNDLE is not a network-xray bundle, or NIL when it is one."
  (cond
    ((not (hash-table-p bundle)) "a bundle is one JSON object")
    ((not (eql 1 (nlk:json-value bundle :integer "schema_version")))
     "a bundle's schema_version is 1")
    ((not (equal "network_xray.probe" (nlk:json-value bundle :string "kind")))
     "a bundle's kind is network_xray.probe")
    ((not (valid-id-p (or (nlk:json-value bundle :string "id") "")))
     "a bundle carries its probe id, 1..36 hex characters and dashes")
    ((not (nlk:json-value bundle :string "captured_at")) "a bundle carries its capture time")
    ((not (nlk:json-value bundle :string "vantage" "label"))
     "a bundle carries the vantage label it was taken under")
    ((not (nlk:json-value bundle :string "target" "url"))
     "a bundle carries its target's canonical URL")
    ((let ((stages (nlk:json-value bundle :array "stages")))
       (or (not stages) (/= 4 (length stages))
           (loop for index from 0 below 4
                 thereis (not (equal (aref *stage-names* index)
                                     (nlk:json-value (aref stages index) :string "name"))))))
     "a bundle's stages are dns, tcp, tls and http, in that order")
    ((some (lambda (key) (not (member key *bundle-keys* :test #'string=)))
           (loop for key being the hash-keys of bundle collect key))
     "a bundle carries only the members a network-xray record has")
    (t nil)))

(defun import-bundle (bundle-json)
  "BUNDLE-JSON, a peer's saved bundle, recorded as an imported observation
=> the record the answer carries. Its digest is checked and reported; it
checks content, never origin, so an imported record stays unauthenticated."
  (running-settings)
  (unless (and (stringp bundle-json) (<= 1 (length bundle-json) *bundle-limit*))
    (fail "bundle_json is the bundle's own JSON, 1..~d bytes" *bundle-limit*))
  (let ((bundle (handler-case (nlk:decode-json bundle-json :whole t)
                  (error () (fail "bundle_json is not JSON: import the bundle a collector wrote")))))
    (when (bundle-refusal bundle)
      (fail "~a" (bundle-refusal bundle)))
    (save-record bundle "imported")))

(defun compare-records-by-id (left-id right-id)
  "The saved records LEFT-ID and RIGHT-ID, compared."
  (running-settings)
  (multiple-value-bind (left-record left) (read-record left-id)
    (declare (ignore left-record))
    (multiple-value-bind (right-record right) (read-record right-id)
      (declare (ignore right-record))
      (compare-records left right))))

;;; --- what the four tools answer -------------------------------------------------

(defun answer (thunk &key (error-text "Network X-ray could not complete the call."))
  "THUNK's answer wrapped the way the original's tools answered: ok and
result, or ok false and one line saying what to check. An unexpected
failure still answers: a tool call never takes the turn down."
  (handler-case
      (nlk:encode-json-object
       (nlk:make-json-object "ok" t "result" (funcall thunk)))
    (network-xray-error (condition)
      (nlk:encode-json-object
       (nlk:make-json-object "ok" nil "error" (network-xray-error-detail condition))))
    (stage-failure (condition)
      ;; A URL outside the rule is the model's mistake to read and fix: the
      ;; refusal says which part of it, in the words of the original.
      (nlk:encode-json-object
       (nlk:make-json-object "ok" nil
                              "error" (stage-failure-text condition)
                              "code" (stage-failure-code condition))))
    (error (condition)
      (nlk:encode-json-object (nlk:make-json-object
                                "ok" nil "error" error-text
                                "detail" (safe-text condition))))))

(defun answer-probe (call)
  "network_xray_probe's answer."
  (answer (lambda ()
            (probe (xray-probe-url call)
                   :label (xray-probe-label call)
                   :connect-ip (xray-probe-connect-ip call)))))

(defun answer-import (call)
  "network_xray_import's answer."
  (answer (lambda () (import-bundle (xray-import-bundle-json call)))
          :error-text "The bundle is not a Network X-ray bundle: check its schema, id and stages."))

(defun answer-compare (call)
  "network_xray_compare's answer."
  (answer (lambda ()
            (compare-records-by-id (xray-compare-left-id call)
                                   (xray-compare-right-id call)))
          :error-text "Both probes must be saved, different, and of the same URL: list the history and use its exact ids."))

(defun answer-history (call)
  "network_xray_history's answer."
  (answer (lambda () (history (xray-history-limit call)))
          :error-text "The records directory could not be read."))

;;; --- the same question from the operator ----------------------------------------

(defun ladder-line (bundle index)
  "BUNDLE's stage INDEX as the line /network-xray prints."
  (let* ((stage (stage-of bundle index))
         (name (nlk:json-value stage :string "name"))
         (status (nlk:json-value stage :string "status"))
         (summary (or (nlk:json-value stage :string "summary") ""))
         (error (nlk:json-value stage :string "error" "code")))
    (format nil "  ~5a ~8a ~6@a ms  ~a~@[ (~a)~]"
            (or name "") (or status "") (nlk:json-value stage :integer "duration_ms")
            summary (and (equal status "error") error))))

(defun detail-lines (bundle)
  "What the stages saw, in the words /network-xray prints under the ladder."
  (let ((lines '()))
    (let ((server (nlk:json-value (stage-of bundle 3) :string "details" "server"))
          (type (nlk:json-value (stage-of bundle 3) :string "details" "content_type")))
      (when (or server type)
        (push (format nil "  server ~@[~a~]~@[ - ~a~]" server type) lines)))
    (let ((issuer (nlk:json-value (stage-of bundle 2) :string "details" "issuer"))
          (protocol (nlk:json-value (stage-of bundle 2) :string "details" "protocol")))
      (when (or issuer protocol)
        (push (format nil "  tls ~@[~a~]~@[ - certificate issued by ~a~]" protocol issuer) lines)))
    (let ((addresses (nlk:json-value (stage-of bundle 0) :array "details" "addresses")))
      (when addresses
        (push (format nil "  resolved ~{~a~^, ~}" (coerce addresses 'list)) lines)))
    (nreverse lines)))

(defun print-record (record)
  "RECORD as the lines /network-xray answers with."
  (let ((bundle (nlk:json-value record :object "probe")))
    (format nil "~a as seen from \"~a\" (~a, ~a)~%~{~a~%~}~{~a~%~}outcome: ~a"
            (nlk:json-value bundle :string "target" "url")
            (nlk:json-value bundle :string "vantage" "label")
            (nlk:json-value bundle :string "id")
            (nlk:json-value record :string "origin")
            (loop for index from 0 below 4 collect (ladder-line bundle index))
            (detail-lines bundle)
            (record-overall bundle))))

(defun print-history ()
  "The saved records as the lines /network-xray history answers with."
  (let* ((object (history))
         (probes (nlk:json-value object :array "probes")))
    (if (zerop (length probes))
        "network-xray: no probe is saved in this profile yet; /network-xray URL takes the first one"
        (format nil "~d probe~:p saved~%~{~a~%~}"
                (length probes)
                (loop for probe across probes
                      collect (format nil "~a  ~10a ~6a ~a~@[  (~a, digest ~a)~]"
                                      (nlk:json-value probe :string "captured_at")
                                      (nlk:json-value probe :string "label")
                                      (nlk:json-value probe :string "overall")
                                      (nlk:json-value probe :string "url")
                                      (nlk:json-value probe :string "id")
                                      (nlk:json-value probe :string "digest")))))))

(defun print-comparison (comparison)
  "COMPARISON as the lines /network-xray compare answers with."
  (let ((findings (nlk:json-value comparison :array "findings"))
        (tests (nlk:json-value comparison :array "next_tests")))
    (format nil "~a~%~%verdict: ~a~@[ (first divergence: ~a)~]~%~{~a~%~}~@[~%next: ~a~]"
            (nlk:json-value comparison :string "headline")
            (nlk:json-value comparison :string "verdict")
            (let ((first (nlk:json-value comparison :string "first_divergence")))
              (unless (string= "unknown" (or first "")) first))
            (loop for finding across findings
                  collect (format nil "  [~a] ~a: ~a"
                                  (nlk:json-value finding :string "kind")
                                  (nlk:json-value finding :string "stage")
                                  (nlk:json-value finding :string "text")))
            (when (plusp (length tests))
              (format nil "~a - ~a"
                      (nlk:json-value (aref tests 0) :string "title")
                      (nlk:json-value (aref tests 0) :string "connect_ip"))))))

(defparameter +usage+
  "network-xray: https://example.com/ | history | compare LEFT RIGHT | import BUNDLE.json"
  "What /network-xray answers when its argument names nothing it does.")

(defun slash-network-xray (args session-id)
  "One /network-xray invocation: ARGS a URL to probe, or one of the words
history, compare LEFT RIGHT, import BUNDLE.json."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (nlk:trimmed (or args ""))
                                              :separator '(#\Space #\Tab))
                        :test #'string=)))
    (cond
      ((null words) (print-history))
      ((string= (first words) "history") (print-history))
      ((and (string= (first words) "compare") (= 3 (length words)))
       (print-comparison (compare-records-by-id (second words) (third words))))
      ((string= (first words) "compare")
       (format nil "network-xray compare needs two saved ids: ~a" +usage+))
      ((and (string= (first words) "import") (= 2 (length words)))
       (let ((record (import-bundle (uiop:read-file-string (second words)))))
         (format nil "imported ~a from ~a"
                 (nlk:json-value record :string "id") (second words))))
      ((string= (first words) "import")
       (format nil "network-xray import reads a bundle's file: ~a" +usage+))
      ((uiop:string-prefix-p "http" (first words))
       (print-record (probe (first words) :label (second words))))
      (t (format nil "network-xray did not know \"~a\". ~a" (first words) +usage+)))))

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "One URL, four stages, from this machine: the dns, tcp, verified TLS and
HTTP HEAD evidence a works-here-not-there question turns on.
  (network-xray:probe \"https://example.com/\" :label \"laptop at home\")
  (network-xray:history)                       what this profile saved
  (network-xray:compare-records left right)    two records side by side
  (network-xray:import-bundle bundle-json)     a peer's record, unauthenticated
/network-xray URL, /network-xray history and /network-xray compare LEFT RIGHT
print the same as lines. A probe is one direct connection -- no proxy, no
redirect followed, no body, no credential -- and private and loopback
targets are allowed because that is usually where the difference is.
A recorded difference is an observation, never a root cause: read
first_divergence and the hypotheses, keep the limitations, and offer at
most one follow-up (connect_ip tests the peer's address with the original
Host and TLS name, and needs the user's approval). Keep private evidence
private: a bundle carries the URLs and addresses of the machine that took it."
  "What (help :network-xray) answers while the cell runs.")

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell network-xray
  (:section ("network-xray")
    (:guide "vantage is the one required setting: it names the machine a probe speaks for; timeout bounds one stage")
    ("vantage" :string :required t
               :doc "how a probe from this machine is labelled, e.g. 'laptop on home wifi'")
    ("timeout" :integer :default 3 :min 1
               :doc "seconds one stage may take before the capture gives up on it")
    ("max_records" :integer :default 50 :min 1
                   :doc "the most records one history answer lists")
    ("directory" :path :default ""
                 :doc "where records live; empty means this profile's own folder"))
  (:tool "network_xray_probe" (nlk:contract 'xray-probe) #'answer-probe)
  (:tool "network_xray_import" (nlk:contract 'xray-import) #'answer-import)
  (:tool "network_xray_compare" (nlk:contract 'xray-compare) #'answer-compare)
  (:tool "network_xray_history" (nlk:contract 'xray-history) #'answer-history)
  (:help :network-xray "network-xray: one URL's DNS, TCP, TLS and HTTP evidence from this machine" +primer+)
  (:command "network-xray" 'slash-network-xray
            :catches 'network-xray-error
            :description "X-ray one URL from this machine: DNS, TCP, TLS and HTTP, saved and compared"
            :argument-hint "URL|history|compare LEFT RIGHT|import FILE"))
