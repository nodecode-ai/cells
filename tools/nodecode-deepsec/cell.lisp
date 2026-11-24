;;;; cell.lisp --- the primer, the section, the four tools, /deepsec.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin wrapped a Python CLI. This cell keeps the four tool
;;;; names the original registered -- deepsec_scan, deepsec_agent_audit,
;;;; deepsec_supply_chain, deepsec_report -- and the finding shape they
;;;; answered, and reads the tree itself (engine.lisp). The model learns the
;;;; verbs through the manual: while the cell runs, (help :deepsec) answers
;;;; the primer below and every session's help section carries one line
;;;; naming it. Nothing dials and nothing runs: the cell reads files.

(in-package #:nodecode-deepsec)

;;; --- the words ----------------------------------------------------------------

(defparameter +primer+
  "DeepSec Shield is available through the nodecode-deepsec cell: four tools the model
calls, and the same verbs from eval as (deepsec:scan \"path\"), each answering a string.
  deepsec_scan          a file or a tree: hardcoded credentials, code evaluated from a
                         value, shell and SQL built from a value, markup written from a
                         value, TLS verification off, a download piped into a shell, a
                         weak hash, a world-writable mode, an unpinned install.
  deepsec_agent_audit    the same, plus what an agent's own prompts say: an instruction to
                         drop earlier ones, to hide an action from the user, to send data
                         to an address, to read a credential store, to run a download.
  deepsec_supply_chain   package.json and requirements.txt: a name one edit away from a
                         popular one, a name declared private that a public registry answers
                         for, a fetch over plain HTTP.
  deepsec_report          a saved scan's JSON, rendered as markdown, json, sarif or text.
Every tool answers a summary line and its findings, worst first, each with rule, target,
line, evidence and a fix; path is required and is a file or a directory. Test folders,
node_modules, .git, build output and binary files are skipped unless include_tests is set.
A scan reads code, it never runs it, so a finding is a lead, not a proof: say where it is
(target:line and its rule) and what the fix costs.
Operator: the settings are the deepsec section -- max_findings caps what a scan answers,
include_tests widens every walk, severity_floor drops the milder findings.
(help :deepsec) has this while the cell runs."
  "What (help :deepsec) answers while the cell runs.")

;;; --- reading the arguments ------------------------------------------------------

(defun argument (args key)
  "KEY's value in the tool call ARGS, as the string the model wrote, or NIL."
  (nlk:json-value args :string key))

(defun argument-boolean (args key)
  "Whether KEY is true in the tool call ARGS."
  (and (nlk:json-value args :boolean key) t))

(defun argument-list (args key)
  "KEY's array in the tool call ARGS as a list of strings."
  (loop for item across (or (nlk:json-value args :array key) #())
        when (stringp item) collect item))

(defun root-argument (args)
  "The path a call names, refused when it is not there."
  (let ((path (or (argument args "path") (argument args "source") ".")))
    (unless (probe-file path)
      (fail "~s is not there: name a file or a directory" path))
    path))

(defun milliseconds-since (start)
  "The whole milliseconds since START, an INTERNAL-REAL-TIME reading."
  (/ (- (get-internal-real-time) start) (/ internal-time-units-per-second 1000.0)))

(defun above-floor (findings)
  "FINDINGS at or above the section's severity floor."
  (let ((floor (rank (setting :severity-floor))))
    (remove-if (lambda (item) (> (rank (finding :severity item)) floor)) findings)))

(defun report-text (findings files elapsed)
  "A scan as the tool answers it: the summary, then at most max_findings findings."
  (render-text (above-floor findings) files elapsed :limit (setting :max-findings)))

(defun save-result (findings files elapsed output)
  "Write the whole scan to OUTPUT as the JSON deepsec_report reads. => OUTPUT."
  (when output
    (let ((path (merge-pathnames output (uiop:getcwd))))
      (ensure-directories-exist path)
      (nlk:write-file-atomically path (format nil "~a~%" (render-json findings files elapsed)))
      (namestring path))))

;;; --- the four verbs ---------------------------------------------------------------

(defun scan-as (args)
  "deepsec_scan: the pattern layer over a file or a tree. => text."
  (let* ((root (root-argument args))
         (include-tests (or (argument-boolean args "include_tests") (setting :include-tests)))
         (output (argument args "output"))
         (start (get-internal-real-time)))
    (multiple-value-bind (findings files) (scan-tree root *rules* :include-tests include-tests)
      (let* ((elapsed (milliseconds-since start))
             (saved (save-result findings files elapsed output)))
        (format nil "~@[saved: ~a~%~]~a" saved (report-text findings files elapsed))))))

(defun agent-audit-as (args)
  "deepsec_agent_audit: the pattern layer and the prompt layer. => text."
  (let* ((root (root-argument args))
         (include-tests (or (argument-boolean args "include_tests") (setting :include-tests)))
         (start (get-internal-real-time)))
    (multiple-value-bind (findings files)
        (scan-tree root (append *rules* *agent-rules*) :include-tests include-tests)
      (report-text findings files (milliseconds-since start)))))

(defun supply-chain-as (args)
  "deepsec_supply_chain: the dependency checks over a tree. => text."
  (let* ((root (root-argument args))
         (private (argument-list args "private_packages"))
         (start (get-internal-real-time))
         (findings (supply-chain-findings root :private-packages private)))
    (report-text findings (length (tree-files root :include-tests t))
                 (milliseconds-since start))))

(defun report-as (args)
  "deepsec_report: a saved scan's JSON as markdown, json, sarif or text. => text
or, with output, the path written."
  (let* ((source (or (argument args "source") (fail "deepsec_report needs the path of a saved scan")))
         (format-name (string-downcase (or (argument args "format") "markdown")))
         (output (argument args "output")))
    (unless (probe-file source)
      (fail "~s is not there: name the JSON a save wrote" source))
    (let* ((text (handler-case (uiop:read-file-string source :external-format :utf-8)
                   (error () (fail "~s cannot be read as text" source))))
           (object (handler-case (nlk:decode-json text)
                     (error () (fail "~s is not the JSON a scan saved" source))))
           (findings (findings-of object))
           (files (or (nlk:json-value object :integer "filesScanned") 0))
           (elapsed (or (nlk:json-value object :number "elapsedMs") 0))
           (rendered (cond ((member format-name '("markdown" "md") :test #'string=)
                            (render-markdown findings files elapsed :limit (setting :max-findings)))
                           ((string= format-name "json") (render-json findings files elapsed))
                           ((string= format-name "sarif") (render-sarif findings files elapsed))
                           ((string= format-name "text") (report-text findings files elapsed))
                           (t (fail "~s is no report format: markdown, json, sarif or text"
                                    format-name)))))
      (if output
          (let ((path (merge-pathnames output (uiop:getcwd))))
            (ensure-directories-exist path)
            (nlk:write-file-atomically path rendered)
            (format nil "wrote ~a (~d finding~:p)" (namestring path) (length findings)))
          rendered))))

;;; --- the schemas --------------------------------------------------------------

(defun field (type description &rest extra)
  "A JSON-schema member of TYPE and DESCRIPTION, plus pairs in EXTRA."
  (apply #'nlk:make-json-object (append (list "type" type "description" description) extra)))

(defun tool-spec (description &rest members)
  "A tool's registration: what it is for and its argument schema, from
MEMBERS, a wire name and its schema in turn."
  (list :description description
        :parameters (nlk:make-json-object
                     "type" "object"
                     "properties" (apply #'nlk:make-json-object members)
                     "additionalProperties" t)))

(defun scan-schema ()
  (tool-spec "Scan a file or a directory for hardcoded credentials and risky code: dynamic eval, shell and SQL built from a value, markup written from a value, TLS off, a download piped into a shell, a weak hash, an unpinned install. Answers a summary line and the findings, worst first, each with rule, target, line, evidence and a fix."
             "path" (field "string" "the file or directory to scan, absolute or relative to the session's own directory")
             "include_tests" (field "boolean" "also read test, fixture and example folders")
             "output" (field "string" "where to save the whole result as the JSON deepsec_report reads")))

(defun audit-schema ()
  (tool-spec "Audit an agent's own source, prompts and configuration: the same pattern rules, plus an instruction to drop earlier ones, to hide an action from the user, to send data to an address, to read a credential store, to run a download."
             "path" (field "string" "the agent source, prompts or configuration tree to audit")
             "include_tests" (field "boolean" "also read test, fixture and example folders")))

(defun supply-schema ()
  (tool-spec "Check dependency manifests -- package.json and requirements.txt -- for a name one edit away from a popular one, a name declared private that a public registry answers for, and a fetch over plain HTTP."
             "path" (field "string" "the directory, package.json or requirements.txt to check; defaults to the session's directory")
             "private_packages" (field "array" "names your organisation publishes privately, so a public copy of one is a finding"
                                       "items" (field "string" "a package name"))))

(defun report-schema ()
  (tool-spec "Render a JSON result a scan saved as markdown, json, sarif or text; with chain-style detail it is a report to hand over."
             "source" (field "string" "the path of a JSON result a scan saved")
             "format" (field "string" "markdown (default), json, sarif or text")
             "output" (field "string" "where to write the report; without it the report is the answer")))

;;; --- /deepsec ------------------------------------------------------------------

(defun run-slash (args session-id)
  "/deepsec PATH: scan PATH, or the session's own directory, and answer the
report. Typed alone it scans where the session works."
  (declare (ignore session-id))
  (let* ((path (nlk:trimmed (or args "")))
         (root (if (plusp (length path)) path (namestring (uiop:getcwd)))))
    (unless (probe-file root)
      (fail "~s is not there: name a file or a directory" root))
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (findings files)
          (scan-tree root *rules* :include-tests (setting :include-tests))
        (format nil "~a~%~a" (namestring (uiop:ensure-pathname root))
                (report-text findings files (milliseconds-since start)))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell deepsec
  (:section ("deepsec")
    (:guide "this cell needs nothing but a path: /deepsec scans where the session works. max_findings caps what one scan answers, include_tests widens every walk, severity_floor drops the milder findings from the answer (a saved result keeps them all)")
    ("max_findings" :integer :default 50 :min 1
                    :doc "the most findings one scan answers; the rest are counted in the summary line")
    ("include_tests" :boolean :default nil
                     :doc "read test, fixture and example folders too, in every walk a tool was not told otherwise")
    ("severity_floor" :choice :options '("info" "low" "medium" "high" "critical") :default "info"
                       :doc "the mildest severity the answer carries; a saved result keeps them all"))
  (:tool "deepsec_scan" (scan-schema) #'scan-as)
  (:tool "deepsec_agent_audit" (audit-schema) #'agent-audit-as)
  (:tool "deepsec_supply_chain" (supply-schema) #'supply-chain-as)
  (:tool "deepsec_report" (report-schema) #'report-as)
  (:command "deepsec" 'run-slash
            :catches 'deepsec-error
            :description "Security-scan a file or a tree where you work"
            :argument-hint "PATH")
  (:help :deepsec "deepsec: deepsec_scan, deepsec_agent_audit, deepsec_supply_chain and deepsec_report read a tree for secrets, risky code and dependency risks" +primer+))

;;; --- the same verbs from eval ------------------------------------------------

(defun scan (path)
  "Scan PATH -- a file or a directory -- for secrets and risky code. => the report text."
  (scan-as (nlk:make-json-object "path" (namestring path))))

(defun agent-audit (path)
  "Audit PATH, an agent's prompts or configuration, for injection and egress. => the report text."
  (agent-audit-as (nlk:make-json-object "path" (namestring path))))

(defun supply-chain (path &key private-packages)
  "Check PATH's dependency manifests for imitation names and public copies of
PRIVATE-PACKAGES. => the report text."
  (supply-chain-as (nlk:make-json-object
                    "path" (namestring path)
                    "private_packages" (coerce private-packages 'vector))))

(defun report (source &key (format "markdown") output)
  "Render the saved scan SOURCE as FORMAT, or write it to OUTPUT. => text or the path."
  (report-as (nlk:make-json-object "source" (namestring source) "format" format
                                    "output" (and output (namestring output)))))
