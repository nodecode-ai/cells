;;;; tools.lisp --- the nine tools, /security, and the declaration that installs them.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every tool's shape is one NLK:DEFINE-CONTRACT, so the schema the model
;;;; is shown and the value the body runs on come off one declaration. The
;;;; bodies take keyword arguments and answer a string; the tool wrappers
;;;; only pass a record's members along. An argument the declaration does not
;;;; carry has no reader and is refused before any body runs.

(in-package #:nodecode-security-scan)

;;; --- start ----------------------------------------------------------------------

(defparameter +producer-version+ "0.1.0")

(defun start-scan (&key path (mode "standard") allow-tests (context "") scope base head
                     (dependency-check t))
  "Walk PATH, plan the review, and answer the scan's id and what to do next."
  (settings)
  (let ((path (nlk:trimmed (or path ""))))
    (unless (plusp (length path))
      (fail "path is required: the folder of the repository to review"))
    (let ((root (ignore-errors (uiop:truename* (pathname path)))))
      (unless (and root (uiop:directory-pathname-p root) (probe-file root))
        (fail "~s is no folder on this machine: give the path of the repository to review" path))
      (when (equal (namestring root) "/")
        (fail "~a is the filesystem root: review one repository" (namestring root)))
      (let* ((mode (string-downcase (or mode "standard")))
             (root (uiop:truename* root)))
        (unless (member mode '("standard" "deep" "diff") :test #'string=)
          (fail "mode is standard, deep or diff, not ~s" mode))
        (let* ((deep (string= mode "deep"))
               (wanted (and (string= mode "diff") (changed-files root))))
          (multiple-value-bind (files skipped)
              (walk root :deep deep)
            (when wanted
              (let ((both (lambda (file) (member (getf file :path) wanted :test #'string=))))
                (setf skipped (append (remove-if both skipped)
                                      (loop for path in wanted
                                            unless (or (some both files)
                                                       (some (lambda (s) (string= path (getf s :path))) skipped))
                                              collect (list :path path :reason "gone at this snapshot"))))
                (setf files (remove-if-not both files))))
            (when (null files)
              (fail "~a holds nothing to review~@[ (~d file~:p skipped)~]: point the scan at the code"
                    (namestring root) (length skipped)))
            (let* ((chunks (chunk-files files (setting :max-chunk-lines)))
                   (scan (list :id (scan-id)
                               :path (namestring root)
                               :mode mode
                               :status "open"
                               :created-at (now)
                               :context (clip context 8000)
                               :scope (or scope '())
                               :allow-tests (and allow-tests t)
                               :base (or base "")
                               :head (or head "")
                               :producer-version +producer-version+
                               :digest (nlk:sha256-text
                                        (format nil "~{~a~%~}" (mapcar (lambda (file)
                                                                         (format nil "~a:~d" (getf file :path) (getf file :lines)))
                                                                       files)))
                               :files files
                               :skipped skipped
                               :chunks chunks
                               :workers '()
                               :coverage '()
                               :findings '()
                               :validations (list :receipts '() :plans '())
                               :chains '()
                               :grants '()
                               :activity '()
                               :dependencies (if (and dependency-check (setting :dependency-check))
                                                 (check-dependencies root)
                                                 (list :state "skipped" :packages 0 :advisories '()
                                                       :note "the scan asked for no dependency check")))))
              (note scan (format nil "started: ~d files, ~d chunks, mode ~a"
                                 (length files) (length chunks) mode))
              (unless (member (getf (getf scan :dependencies) :state) '("ok" "none") :test #'string=)
                (note scan (format nil "the dependency check did not run: ~a; the scan is partial"
                                   (getf (getf scan :dependencies) :note))))
              (save-scan scan)
              (format nil "scan ~a started~%  repository ~a~%  mode ~a~%  ~d file~:p read, ~d skipped, ~d chunk~:p of about ~d lines~%  dependency check: ~a~@[ (~d package~:p, ~d advisor~:p)~]~@[ -- ~a~]~%  snapshot ~a~%~%Next: security_scan_get section next, then review one chunk and answer with security_scan_submit_worker_result.~@[ ~d of them not reviewed yet, so a report now is not a clean result.~]"
                      (getf scan :id) (getf scan :path) (getf scan :mode)
                      (length (getf scan :files)) (length skipped) (length chunks)
                      (setting :max-chunk-lines)
                      (getf (getf scan :dependencies) :state)
                      (getf (getf scan :dependencies) :packages)
                      (length (getf (getf scan :dependencies) :advisories))
                      (let ((note (getf (getf scan :dependencies) :note)))
                        (and note (plusp (length note)) note))
                      (subseq (getf scan :digest) 0 (min 23 (length (getf scan :digest))))
                      (length (gaps scan))))))))))

;;; --- get ------------------------------------------------------------------------

(defun page-text (lines head limit offset &optional (unit "record"))
  (let* ((total (length lines))
         (from (min (max 0 offset) total))
         (to (min total (+ from limit))))
    (format nil "~@[~a~%~]~{~a~%~}~@[(~d ~a~:p, showing ~d-~d; read on with offset ~d)~]"
            head (subseq lines from to)
            (and (< to total) total) (and (< to total) unit) (and (< to total) (1+ from)) to
            (and (< to total) to))))

(defun chunk-line (chunk)
  (format nil "  ~a  ~7d line~:p  ~a  ~a"
          (getf chunk :id) (getf chunk :lines) (getf chunk :state)
          (clip (format nil "~{~a~^, ~}" (getf chunk :paths)) 160)))

(defun next-text (scan)
  (let ((status (or (getf scan :status) "open"))
        (gaps (gaps scan)))
    (cond
      ((equal status "sealed") (format nil "done: the reports are sealed in ~a; read one with security_scan_export"
                                       (namestring (scan-dir (getf scan :id)))))
      ((equal status "cancelled") "cancelled: no more work; start a new scan to review again")
      ((equal status "blocked") (format nil "blocked: ~a -- continue with security_scan_checkpoint, or cancel the scan"
                                        (or (getf scan :blocked-note) "the review stopped")))
      ((plusp (length gaps))
       (format nil "dispatch: ~d of ~d chunks are not reviewed~%; the first six:~%~
                   Call security_scan_submit_worker_result with a payload naming the scan, the worker, this snapshot's ~a and the candidates you found.~%~
                   ~{~a~%~}~
                   ~@[Also unread: ~@{~a~^, ~}~]"
               (length (gaps scan)) (length (getf scan :chunks))
               (subseq (getf scan :digest) 0 (min 19 (length (getf scan :digest))))
               (mapcar #'chunk-line (subseq gaps 0 (min 6 (length gaps))))
               (let ((rest (nthcdr 6 gaps)))
                 (and rest (format nil "and ~d more" (length rest)))))
       )
      ((some (lambda (finding) (equal (getf finding :validation) "NOT_RUN")) (getf scan :findings))
       (format nil "validate: every chunk is reviewed~%~
                   Record what was proved with security_scan_record_validations: a receipt per finding, CONFIRMED only when its test passed and a negative control behaved; otherwise the finding stays NOT_RUN.~%~
                   Then security_scan_record_chains, then security_scan_finalize."))
      ((null (getf scan :chains))
       "chains: the primitives are reviewed and validated; record the chains they compose, then finalize")
      (t "finalize: security_scan_finalize seals the reports; after that nothing changes them"))))

(defun summary-text (scan)
  (format nil "scan ~a~%  repository ~a~%  mode ~a, status ~a~%  started ~a~%  snapshot ~a~%  ~d file~:p read, ~d skipped, ~d chunk~:p (~d reviewed)~%  findings: ~d~%  dependency check: ~a~@[ (~d package~:p, ~d advisor~:p)~]~@[ -- ~a~]~@[~%  context: ~a~]~@[~%  blocked: ~a~]"
          (getf scan :id) (getf scan :path) (getf scan :mode) (or (getf scan :status) "open")
          (getf scan :created-at) (getf scan :digest)
          (length (getf scan :files)) (length (getf scan :skipped))
          (length (getf scan :chunks)) (length (reviewed-chunks scan))
          (length (getf scan :findings))
          (getf (getf scan :dependencies) :state)
          (getf (getf scan :dependencies) :packages)
          (length (getf (getf scan :dependencies) :advisories))
          (let ((note (getf (getf scan :dependencies) :note)))
            (and note (plusp (length note)) note))
          (let ((context (getf scan :context)))
            (and context (plusp (length context)) (clip (one-line context) 300)))
          (getf scan :blocked-note)))

(defun inventory-lines (scan)
  (mapcar (lambda (file) (format nil "  ~7d  ~a" (getf file :lines) (getf file :path)))
          (getf scan :files)))

(defun coverage-lines (scan)
  (let ((recorded (getf scan :coverage)))
    (if recorded
        (mapcar (lambda (unit) (format nil "  ~a  ~a~@[ -- ~a~]"
                                       (getf unit :unit) (getf unit :state) (getf unit :note)))
                recorded)
        (mapcar (lambda (chunk) (format nil "  ~a  ~a" (getf chunk :id) (getf chunk :state)))
                (getf scan :chunks)))))

(defun worker-lines (scan)
  (mapcar (lambda (worker) (format nil "  ~a  ~a  ~a  ~a"
                                   (getf worker :id) (getf worker :role)
                                   (getf worker :chunk) (getf worker :state)))
          (getf scan :workers)))

(defun finding-lines-of (scan)
  (mapcar (lambda (finding)
            (format nil "  ~a  ~10a  ~a  ~a  ~a"
                    (getf finding :id) (getf finding :severity) (getf finding :validation)
                    (finding-lines finding) (getf finding :title)))
          (reportable scan)))

(defun chain-lines (scan)
  (mapcar (lambda (chain) (format nil "  ~a  ~a  (~{~a~^, ~}): ~a"
                                  (getf chain :id) (getf chain :title)
                                  (mapcar (lambda (finding) (getf finding :id)) (getf chain :findings))
                                  (getf chain :summary)))
          (getf scan :chains)))

(defun validation-lines (scan)
  (let ((validations (getf scan :validations)))
    (append
     (mapcar (lambda (receipt)
               (format nil "  receipt ~a  ~a  positive ~a  negative ~a~@[ -- ~a~]"
                       (getf receipt :finding-id) (getf receipt :status)
                       (if (getf receipt :positive-control) "passed" "missing")
                       (if (getf receipt :negative-control) "behaved" "missing")
                       (getf receipt :note)))
             (getf validations :receipts))
     (mapcar (lambda (plan)
               (format nil "  plan ~a  ~a  ~a"
                       (getf plan :finding-id) (getf plan :kind) (getf plan :state)))
             (getf validations :plans)))))

(defun get-scan (scan-id &key (section "next") (limit 50) (offset 0))
  "One section of SCAN-ID: the step to take now, or the data under it."
  (settings)
  (let* ((scan (scan-or-fail scan-id))
         (section (string-downcase (or section "next")))
         (limit (max 1 (min 500 (or limit 50))))
         (offset (max 0 (or offset 0))))
    (cond
      ((string= section "next") (next-text scan))
      ((string= section "summary") (summary-text scan))
      ((string= section "packets") (page-text (mapcar #'chunk-line (getf scan :chunks)) nil limit offset "chunk"))
      ((string= section "inventory") (page-text (inventory-lines scan) nil limit offset "file"))
      ((string= section "coverage") (page-text (coverage-lines scan) nil limit offset "unit"))
      ((string= section "workers") (page-text (worker-lines scan) nil limit offset "worker"))
      ((string= section "findings") (page-text (finding-lines-of scan) nil limit offset "finding"))
      ((string= section "chains") (page-text (chain-lines scan) nil limit offset "chain"))
      ((string= section "validations") (page-text (validation-lines scan) nil limit offset "record"))
      ((string= section "activity")
       (page-text (mapcar (lambda (entry) (format nil "  ~a  ~a" (getf entry :at) (getf entry :text)))
                          (reverse (getf scan :activity)))
                  nil limit offset "entry"))
      ((string= section "manifest")
       (nlk:encode-json-object (to-wire (list :scan-id (getf scan :id) :path (getf scan :path)
                                               :mode (getf scan :mode) :status (getf scan :status)
                                               :snapshot-digest (getf scan :digest)
                                               :created-at (getf scan :created-at)
                                               :chunks (length (getf scan :chunks))
                                               :findings (length (getf scan :findings))))))
      ((string= section "report")
       (if (equal (getf scan :status) "sealed")
           (report-markdown scan)
           (fail "scan ~a is not sealed yet: finalize it, then read the report, or export it" (getf scan :id))))
      (t (fail "~s is no section: next, summary, packets, inventory, coverage, workers, validations, chains, activity, findings, manifest, report" section)))))

;;; --- checkpoint, worker results -------------------------------------------------

(defun unit-state (state)
  (let ((state (string-downcase (or state ""))))
    (cond ((member state '("reviewed" "read" "done" "covered") :test #'string=) "reviewed")
          ((member state '("skipped" "skipped-vendored" "excluded") :test #'string=) "skipped")
          ((member state '("pending" "planned" "todo") :test #'string=) "pending")
          (t "pending"))))

(defun checkpoint (scan-id &key coverage note blocked)
  "Save what was read. COVERAGE is a list of (:unit :state :note) objects."
  (settings)
  (let ((scan (scan-or-fail-open scan-id)))
    (when (and coverage (null (objects coverage)))
      (fail "coverage is a list of objects with unit, state and note"))
    (dolist (unit (objects coverage))
      (let* ((name (istr unit "unit"))
             (state (unit-state (istr unit "state")))
             (text (clip (one-line (istr unit "note")) 500)))
        (when (zerop (length name))
          (fail "a coverage unit needs a unit: the chunk id (C1) or the path it covers"))
        (setf (getf scan :coverage)
              (remove name (getf scan :coverage) :test #'string= :key (lambda (c) (getf c :unit)))
              (getf scan :coverage)
              (append (getf scan :coverage) (list (list :unit name :state state :note text))))
        (let ((chunk (find name (getf scan :chunks) :test #'string= :key (lambda (c) (getf c :id)))))
          (when chunk
            (setf (getf chunk :state)
                  (if (member state '("reviewed" "skipped") :test #'string=) "reviewed" "planned"))))))
    (when (and note (plusp (length note)))
      (note scan (clip (one-line note) 400)))
    (when blocked
      (setf (getf scan :status) "blocked"
            (getf scan :blocked-note) (clip (one-line (or note "the review cannot go on")) 400))
      (note scan (format nil "blocked: ~a" (getf scan :blocked-note))))
    (save-scan scan)
    (format nil "scan ~a: coverage saved (~d unit~:p recorded, ~d of ~d chunk~:p reviewed)~@[; blocked: ~a~]"
            (getf scan :id) (length (getf scan :coverage))
            (length (reviewed-chunks scan)) (length (getf scan :chunks))
            (getf scan :blocked-note))))

(defun submit-worker-result (payload)
  "Take one reviewer's PAYLOAD: its candidates become findings, its coverage the
record. A payload from another snapshot is refused."
  (settings)
  (let* ((scan-id (istr payload "scanId"))
         (scan (scan-or-assigned scan-id (istr payload "attemptId")))
         (digest (istr payload "snapshotDigest")))
    (unless (or (zerop (length digest)) (equal digest (getf scan :digest)))
      (fail "the payload cites snapshot ~a, but scan ~a reviews ~a: submit the result of the snapshot you were given"
            (clip digest 40) (getf scan :id) (getf scan :digest)))
    (let ((candidates (objects (iget payload "candidates")))
          (role (istr payload "workerRole" "review"))
          (attempt (istr payload "attemptId" "w1"))
          (added '()))
      (dolist (candidate candidates)
        (let ((finding (candidate-finding scan candidate)))
          (setf (getf scan :findings) (append (getf scan :findings) (list finding)))
          (push (getf finding :id) added)))
      (setf (getf scan :workers)
            (remove attempt (getf scan :workers) :test #'string= :key (lambda (w) (getf w :id))))
      (setf (getf scan :workers)
            (append (getf scan :workers)
                    (list (list :id attempt :role role :state "done"
                                :candidates (length candidates) :at (now)))))
      (dolist (unit (objects (iget payload "coverage")))
        (let ((name (istr unit "unit")))
          (when (plusp (length name))
            (setf (getf scan :coverage)
                  (remove name (getf scan :coverage) :test #'string= :key (lambda (c) (getf c :unit)))
                  (getf scan :coverage)
                  (append (getf scan :coverage)
                          (list (list :unit name :state (unit-state (istr unit "state" "reviewed"))
                                      :note (clip (one-line (istr unit "note")) 500)))))
            (let ((chunk (find name (getf scan :chunks) :test #'string= :key (lambda (c) (getf c :id)))))
              (when chunk (setf (getf chunk :state) "reviewed"))))))
      (note scan (format nil "worker ~a (~a) returned ~d candidate~:p~@[: ~{~a~^, ~}~]"
                         attempt role (length candidates) (and added (reverse added))))
      (save-scan scan)
      (let ((unvalidated (loop for finding in (getf scan :findings)
                               when (equal (getf finding :validation) "NOT_RUN") count finding)))
        (format nil "scan ~a: worker ~a recorded, ~d new candidate~:p (~{~a~^, ~}); ~d finding~:p in all, ~d not validated yet~%Next: ~a"
                (getf scan :id) attempt (length candidates) added
                (length (getf scan :findings)) unvalidated (next-text scan))))))

(defun scan-or-assigned (scan-id attempt)
  (when (zerop (length (or scan-id "")))
    (fail "the payload needs scanId: the id security_scan_start answered"))
  (scan-or-fail-open scan-id))

;;; --- validations and chains -----------------------------------------------------

(defun receipt-status (receipt)
  "What a receipt proves: a confirmation needs its test passed and a negative
control that behaved; anything else is no proof either way."
  (let ((status (string-upcase (or (istr receipt "status") "NOT_RUN")))
        (positive (not (null (iget receipt "positiveControl"))))
        (negative (not (null (iget receipt "negativeControl")))))
    (cond ((and (string= status "CONFIRMED") positive negative) "CONFIRMED")
          ((string= status "REFUTED") "REFUTED")
          ((string= status "CONFIRMED") "NOT_RUN")
          (t "NOT_RUN"))))

(defun record-validations (scan-id &key receipts plans)
  (settings)
  (let ((scan (scan-or-fail-open scan-id)))
    (dolist (receipt (objects receipts))
      (let* ((id (istr receipt "findingId"))
             (finding (find id (getf scan :findings) :test #'string= :key (lambda (f) (getf f :id)))))
        (unless finding
          (fail "no finding ~s on scan ~a: a receipt names the id the report carries" id (getf scan :id)))
        (let ((status (receipt-status receipt))
              (note (clip (one-line (istr receipt "note")) 500)))
          (setf (getf finding :validation) status
                (getf finding :validation-note) note)
          (setf (getf (getf scan :validations) :receipts)
                (remove id (getf (getf scan :validations) :receipts) :test #'string= :key (lambda (r) (getf r :finding-id)))
                (getf (getf scan :validations) :receipts)
                (append (getf (getf scan :validations) :receipts)
                        (list (list :finding-id id :status status
                                    :positive-control (not (null (iget receipt "positiveControl")))
                                    :negative-control (not (null (iget receipt "negativeControl")))
                                    :note note)))))))
    (dolist (plan (objects plans))
      (let ((id (istr plan "findingId"))
            (kind (istr plan "kind" "static"))
            (state (istr plan "state" "planned")))
        (unless (find id (getf scan :findings) :test #'string= :key (lambda (f) (getf f :id)))
          (fail "no finding ~s on scan ~a: a plan names the id the report carries" id (getf scan :id)))
        (when (and (string= (string-downcase kind) "local-command") (not (getf scan :allow-tests)))
          (fail "scan ~a was started without allow_tests, so no test may run: ask the operator and start a scan with it" (getf scan :id)))
        (setf (getf (getf scan :validations) :plans)
              (append (getf (getf scan :validations) :plans)
                      (list (list :finding-id id :kind kind :state state))))
        (note scan (format nil "validation plan for ~a: ~a (~a)" id kind state))))
    (save-scan scan)
    (format nil "scan ~a: ~d receipt~:p recorded, ~d plan~:p; ~d finding~:p confirmed of ~d~%Next: ~a"
            (getf scan :id) (length (objects receipts)) (length (objects plans))
            (count-if (lambda (finding) (equal (getf finding :validation) "CONFIRMED")) (getf scan :findings))
            (length (getf scan :findings)) (next-text scan))))

(defun compose-chains (scan)
  "One chain per file two or more primitives were found in: the chain an
attacker could walk, named in the report."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (finding (getf scan :findings))
      (dolist (location (getf finding :locations))
        (pushnew finding (gethash (getf location :path) groups) :test #'eq)))
    (let ((chains '()) (n 0))
      (maphash (lambda (path findings)
                 (when (>= (length findings) 2)
                   (let ((findings (stable-sort findings
                                                 (lambda (a b) (< (severity-rank (getf a :severity))
                                                                  (severity-rank (getf b :severity)))))))
                     (push (list :id (format nil "CH~d" (incf n))
                                 :title (format nil "primitives in ~a" path)
                                 :summary (format nil "~{~a~^, ~} were all found in ~a; a reader has to walk the path between them"
                                                  (mapcar (lambda (f) (getf f :id)) findings) path)
                                 :findings findings)
                           chains))))
               groups)
      (sort chains #'string< :key (lambda (chain) (getf chain :id))))))

(defun record-chains (scan-id &key explanations)
  "Compose the chains from the primitives, then take EXPLANATIONS by chain id."
  (settings)
  (let* ((scan (scan-or-fail-open scan-id))
         (chains (compose-chains scan)))
    (dolist (explanation (objects explanations))
      (let* ((id (istr explanation "chainId"))
             (chain (find id chains :test #'string= :key (lambda (c) (getf c :id)))))
        (cond ((null chain)
               (fail "no chain ~s: this scan composed ~{[~a]~^, ~}" id (mapcar (lambda (c) (getf c :id)) chains)))
              (t (let ((title (istr explanation "title"))
                       (summary (istr explanation "summary")))
                   (when (plusp (length title)) (setf (getf chain :title) (clip (one-line title) 200)))
                   (when (plusp (length summary)) (setf (getf chain :summary) (clip (one-line summary) 1200))))))))
    (setf (getf scan :chains) chains)
    (note scan (format nil "composed ~d chain~:p" (length chains)))
    (save-scan scan)
    (if chains
        (format nil "scan ~a: ~d chain~:p composed~%~{~a~%~}~%Next: ~a"
                (getf scan :id) (length chains) (chain-lines scan) (next-text scan))
        (format nil "scan ~a: no two findings share a file, so no chain was composed; continue with ~a"
                (getf scan :id) (next-text scan)))))

;;; --- finalize, cancel, export ---------------------------------------------------

(defun finalize-scan (scan-id)
  "Seal SCAN-ID: the four reports are written once and never change."
  (settings)
  (let ((scan (scan-or-fail-open scan-id)))
    (when (null (reviewed-chunks scan))
      (fail "scan ~a has no reviewed chunk yet: record what was read with security_scan_checkpoint, or submit a worker result, then finalize"
            (getf scan :id)))
    (setf (getf scan :status) "sealed"
          (getf scan :sealed-at) (now))
    (note scan (format nil "sealed: ~d finding~:p, ~d of ~d chunk~:p reviewed"
                       (length (getf scan :findings)) (length (reviewed-chunks scan)) (length (getf scan :chunks))))
    (transform-chains scan)
    (save-scan scan)
    (let ((dir (write-reports scan)))
      (format nil "scan ~a sealed in ~a~%  report.md, findings.json, results.sarif, findings.csv~%  ~d finding~:p, ~d of ~d chunk~:p reviewed~@[ (~d gap~:p: zero findings is not a clean result)~]~%Read one with security_scan_export."
              (getf scan :id) dir (length (getf scan :findings))
              (length (reviewed-chunks scan)) (length (getf scan :chunks))
              (length (gaps scan))))))

(defun transform-chains (scan)
  (setf (getf scan :chains)
        (mapcar (lambda (chain)
                  (let ((chain (copy-list chain)))
                    (setf (getf chain :findings) (mapcar (lambda (f) (list :id (getf f :id))) (getf chain :findings)))
                    chain))
                (getf scan :chains))))

(defun cancel-scan (scan-id)
  (settings)
  (let ((scan (scan-or-fail-open scan-id)))
    (setf (getf scan :status) "cancelled")
    (note scan "cancelled: the evidence collected so far is kept")
    (save-scan scan)
    (format nil "scan ~a cancelled: ~d finding~:p kept, no report sealed" (getf scan :id) (length (getf scan :findings)))))

(defparameter +export-formats+
  '(("md" . "report.md") ("json" . "findings.json") ("sarif" . "results.sarif") ("csv" . "findings.csv"))
  "The four exports, and the file each is.")

(defun export-scan (scan-id &key (fmt "md"))
  "One sealed report's text, exactly as it was sealed."
  (settings)
  (let* ((scan (scan-or-fail scan-id))
         (format (string-downcase (or fmt "md")))
         (file (cdr (assoc format +export-formats+ :test #'string=))))
    (unless file
      (fail "~s is no export format: md, json, sarif or csv" fmt))
    (unless (equal (getf scan :status) "sealed")
      (fail "scan ~a is ~a, not sealed: finalize it first, then export"
            (getf scan :id) (or (getf scan :status) "open")))
    (let ((path (uiop:subpathname* (scan-dir (getf scan :id)) file)))
      (unless (probe-file path)
        (write-reports scan))
      (uiop:read-file-string (uiop:subpathname* (scan-dir (getf scan :id)) file)))))

;;; --- /security ------------------------------------------------------------------

(defparameter +command-help+
  "/security status
/security authorize-validation <scanId> <origin>   let the scan test your running app (1 hour, 200 requests)
/security revoke <grantId>                          stop app testing
/security help
To scan, just ask: \"scan this repo for security issues\" (add \"and run tests to confirm\" to allow tests).
Only you can allow the testing of a running app: the agent's tools cannot grant it.")

(defun origin-p (text)
  "TEXT as an origin: scheme://host[:port], no credentials, path or query."
  (when (and (stringp text) (plusp (length text)) (not (find #\Space text)))
    (let ((rest (cond ((uiop:string-prefix-p "https://" text) (subseq text 8))
                      ((uiop:string-prefix-p "http://" text) (subseq text 7))
                      (t nil))))
      (when (and rest (plusp (length rest)) (not (find #\@ rest)))
        (let* ((slash (position #\/ rest))
               (authority (if slash (subseq rest 0 slash) rest))
               (tail (and slash (subseq rest slash)))
               (colon (position #\: authority))
               (host (if colon (subseq authority 0 colon) authority))
               (port (and colon (subseq authority (1+ colon)))))
          (and (plusp (length host))
               (or (null tail) (string= tail "/"))
               (or (null port)
                   (and (plusp (length port)) (every #'digit-char-p port)
                        (<= 1 (or (ignore-errors (parse-integer port)) 0) 65535)))))))))

(defun positive-int (text default maximum)
  "TEXT as an integer in 1..MAXIMUM, else DEFAULT."
  (let ((value (and text (ignore-errors (parse-integer text)))))
    (if (and value (<= 1 value maximum)) value default)))

(defun run-command (args session-id)
  "The /security command: status, authorize-validation, revoke, help."
  (declare (ignore session-id))
  (let* ((args (nlk:trimmed (or args "")))
         (tokens (remove "" (uiop:split-string args :separator " ") :test #'string=)))
    (handler-case
        (let ((verb (string-downcase (or (first tokens) "status"))))
          (cond
            ((member verb '("help" "?") :test #'string=) +command-help+)
            ((string= verb "status") (command-status))
            ((string= verb "authorize-validation") (command-authorize tokens))
            ((string= verb "revoke") (command-revoke (second tokens)))
            (t (format nil "~s is no /security command: status, authorize-validation <scan> <origin>, revoke <grant>, help" verb))))
      (security-scan-error (condition) (format nil "ERROR: SECURITY-SCAN-ERROR: ~a" condition)))))

(defun shown-scans (&aux (rows '()))
  (maphash (lambda (id scan) (push (list id scan) rows)) *scans*)
  (setf rows (subseq (sort rows (lambda (a b) (string> (getf (second a) :created-at) (getf (second b) :created-at))))
                     0 (min 10 (length rows))))
  rows)

(defun command-status ()
  (settings)
  (format nil "~d scan~:p on this machine, ~d of them sealed~%~{~a~%~}~@[~%recent:~%~{~a~%~}~]~%~a"
          (hash-table-count *scans*)
          (let ((n 0)) (maphash (lambda (id scan) (declare (ignore id)) (when (equal (getf scan :status) "sealed") (incf n))) *scans*) n)
          nil
          (let ((rows (shown-scans)))
            (and rows (mapcar (lambda (row)
                                (let ((scan (second row)))
                                  (format nil "  ~a  ~a  ~d finding~:p  ~d/~d chunk~:p  ~a"
                                          (getf scan :id) (getf scan :status) (length (getf scan :findings))
                                          (length (reviewed-chunks scan)) (length (getf scan :chunks))
                                          (getf scan :path))))
                              rows)))
          "Only you can let a scan test a running app: /security authorize-validation <scanId> <origin>"))

(defun command-authorize (tokens)
  (let ((scan-id (second tokens))
        (origin (third tokens)))
    (unless (and scan-id origin)
      (fail "authorize-validation needs the scan id and the app's origin, as /security authorize-validation <scanId> http://localhost:3000"))
    (unless (origin-p origin)
      (fail "~s is no origin: scheme://host[:port], with no credentials, path or query" origin))
    (let ((allowed (setting :allowed-origins)))
      (when (null allowed)
        (fail "no allowed_origins configured: set security-scan.allowed_origins to the app addresses a scan may test, then (restart-cells \"nodecode-security-scan\")"))
      (unless (member (string-right-trim "/" origin) (mapcar (lambda (o) (string-right-trim "/" o)) allowed) :test #'string=)
        (fail "~s is not in allowed_origins (~{~a~^, ~}): add it to the security-scan section to allow it" origin allowed)))
    (let* ((scan (scan-or-fail-open scan-id))
           (minutes (positive-int (second (member "--minutes" tokens :test #'string=))
                                  (setting :grant-minutes) 240))
           (requests (positive-int (second (member "--max-requests" tokens :test #'string=))
                                   (setting :grant-requests) 1000))
           (grant (list :id (format nil "G~d" (1+ (length (getf scan :grants))))
                        :origin (string-right-trim "/" origin)
                        :actions '("http-probe")
                        :minted-at (get-universal-time)
                        :expires-at (+ (get-universal-time) (* 60 minutes))
                        :max-requests requests
                        :requests 0
                        :state "active"
                        :created-by "user-command")))
      (setf (getf scan :grants) (append (getf scan :grants) (list grant)))
      (note scan (format nil "granted ~a: ~a for ~d minutes, at most ~d requests"
                         (getf grant :id) (getf grant :origin) minutes requests))
      (save-scan scan)
      (format nil "granted ~a on scan ~a: ~a may be tested for the next ~d minute~:p, at most ~d request~:p"
              (getf grant :id) (getf scan :id) (getf grant :origin) minutes requests))))

(defun command-revoke (grant-id)
  (unless (and grant-id (plusp (length grant-id)))
    (fail "revoke needs a grant id: /security revoke <grantId>, as status printed it"))
  (let ((found nil))
    (maphash (lambda (id scan)
               (declare (ignore id))
               (let ((grant (find grant-id (getf scan :grants) :test #'string= :key (lambda (g) (getf g :id)))))
                 (when grant
                   (setf (getf grant :state) "revoked"
                         found scan)
                   (note scan (format nil "revoked grant ~a" grant-id)))))
             *scans*)
    (if found
        (progn (save-scan found)
               (format nil "grant ~a revoked: the scan may no longer test your app" grant-id))
        (fail "no grant ~s on any scan here: /security status lists the scans" grant-id))))

;;; --- the shapes ----------------------------------------------------------------

(nlk:define-contract scan-start-params
  (:doc "Start a security scan of a local repository. Usually only path is needed: standard reviews the whole codebase, diff reviews recent changes (git's view of the working tree), deep reads tests, docs and examples too. The review is the model's: nothing in the repository runs.")
  ("path" :text :required t
          :doc "the repository folder to scan; never the home folder or the filesystem root")
  ("mode" :text :default "standard" :options '("standard" "deep" "diff")
          :doc "standard = the whole codebase, deep = tests, docs and examples too, diff = only the changes git reports")
  ("allow_tests" :boolean :default nil
                 :doc "true only when the operator asked for tests or reproductions that confirm findings")
  ("context" :text :default ""
             :doc "what the app does, what worries the operator, where to look first; at most 8000 characters")
  ("scope" :array :of :string
           :doc "optional: limit the review to these folders or paths")
  ("base" :text :default ""
          :doc "advanced, diff mode: the base revision the operator named")
  ("head" :text :default ""
          :doc "advanced, diff mode: the head revision the operator named")
  ("dependency_check" :boolean :default t
                      :doc "false skips the OSV check, only when the operator does not want package names sent to api.osv.dev"))

(nlk:define-contract scan-get-params
  (:doc "Read scan state, or one paginated section of it, before continuing a review. Read every page: a chunk nobody reviewed is a gap, and a gap is not a clean result.")
  ("scan_id" :text :required t :doc "the scan id security_scan_start answered")
  ("section" :text :default "next"
             :options '("next" "summary" "packets" "inventory" "coverage" "workers" "validations"
                        "chains" "activity" "findings" "manifest" "report")
             :doc "next is the one step to take now: dispatch, validate, chains, finalize, done or blocked")
  ("limit" :integer :default 50 :doc "records per page, 1 to 500")
  ("offset" :integer :default 0 :doc "zero-based page offset"))

(nlk:define-contract scan-checkpoint-params
  (:doc "Save what was actually read, and an optional progress note. Mark a unit reviewed only when the code was read.")
  ("scan_id" :text :required t :doc "the scan id")
  ("coverage" :array :doc "one object per covered unit, with unit (a chunk id like C1 or a path), state (reviewed, skipped, pending) and note")
  ("note" :text :default "" :doc "a progress note without a literal secret")
  ("blocked" :boolean :default nil
             :doc "true when the scan cannot go on; the note becomes the reason the operator reads"))

(nlk:define-contract scan-worker-params
  (:doc "Submit one reviewer's result after reading its chunks. Citations must match the snapshot the scan names, and no secret's value goes in the payload.")
  ("payload" :object :required t
             :doc "the canonical worker result: scanId, attemptId, workerRole, snapshotDigest, candidates and coverage; a candidate is an object with title, severity, summary, locations [{path, startLine, endLine}], ruleId, cwe and owasp"))

(nlk:define-contract scan-validation-params
  (:doc "Record what confirms a finding. A receipt for a static check, or a plan to run: local-command needs a scan started with allow_tests, http-probe needs the operator to allow the app's address. A confirmation needs its test passed and a negative control that behaved.")
  ("scan_id" :text :required t :doc "the scan id")
  ("receipts" :array :doc "one object per finding: findingId, status (CONFIRMED, REFUTED or NOT_RUN), positiveControl, negativeControl, note")
  ("plans" :array :doc "one object per check to run: findingId, kind (local-command or http-probe), state"))

(nlk:define-contract scan-chain-params
  (:doc "Compose and record the chains the reviewed primitives form. A chain's title and summary explain edges the findings already carry; they cannot invent one.")
  ("scan_id" :text :required t :doc "the scan id")
  ("explanations" :array :doc "one object per chain id: chainId, title and summary"))

(nlk:define-contract scan-seal-params
  (:doc "Validate and seal a reviewed scan into reports nothing changes afterwards. Call it after the workers finish; a chunk nobody reviewed stays a gap in the report.")
  ("scan_id" :text :required t :doc "the scan id"))

(nlk:define-contract scan-export-params
  (:doc "Read one sealed report, to hand to a person. The reports are the sealed bytes: export changes nothing.")
  ("scan_id" :text :required t :doc "the scan id")
  ("fmt" :text :default "md" :options '("md" "sarif" "json" "csv")
         :doc "the export: md, sarif (2.1.0), json (the canonical finding document) or csv"))

;;; --- the tool wrappers ---------------------------------------------------------

(defun tool-start (request)
  (start-scan :path (scan-start-params-path request)
              :mode (scan-start-params-mode request)
              :allow-tests (scan-start-params-allow-tests request)
              :context (scan-start-params-context request)
              :scope (scan-start-params-scope request)
              :base (scan-start-params-base request)
              :head (scan-start-params-head request)
              :dependency-check (scan-start-params-dependency-check request)))

(defun tool-get (request)
  (get-scan (scan-get-params-scan-id request)
            :section (scan-get-params-section request)
            :limit (scan-get-params-limit request)
            :offset (scan-get-params-offset request)))

(defun tool-checkpoint (request)
  (checkpoint (scan-checkpoint-params-scan-id request)
              :coverage (scan-checkpoint-params-coverage request)
              :note (scan-checkpoint-params-note request)
              :blocked (scan-checkpoint-params-blocked request)))

(defun tool-worker (request)
  (submit-worker-result (scan-worker-params-payload request)))

(defun tool-validations (request)
  (record-validations (scan-validation-params-scan-id request)
                      :receipts (scan-validation-params-receipts request)
                      :plans (scan-validation-params-plans request)))

(defun tool-chains (request)
  (record-chains (scan-chain-params-scan-id request)
                 :explanations (scan-chain-params-explanations request)))

(defun tool-finalize (request)
  (finalize-scan (scan-seal-params-scan-id request)))

(defun tool-cancel (request)
  (cancel-scan (scan-seal-params-scan-id request)))

(defun tool-export (request)
  (export-scan (scan-export-params-scan-id request)
               :fmt (scan-export-params-fmt request)))

;;; --- the manual ----------------------------------------------------------------

(defparameter +primer+
  "A security scan is a review of one local repository, kept as facts on this
machine, through the nodecode-security-scan cell: nine tools in the `security'
toolset, and the /security command.
  security_scan_start {path, mode, allow_tests, context}   walk the folder, plan
      chunks of about 4,000 lines, check the lockfiles against OSV, answer the
      scan id. Nothing in the repository runs.
  security_scan_get {scan_id, section}   read it: next is the one step to take
      now; summary, packets, inventory, coverage, workers, findings, chains,
      validations, activity, manifest, report are the data under it, paged.
  security_scan_checkpoint {scan_id, coverage}   mark what was really read.
  security_scan_submit_worker_result {payload}   one reviewer's candidates, with
      the snapshotDigest the scan names; a candidate without a location is
      uncited and says so.
  security_scan_record_validations {scan_id, receipts, plans}   CONFIRMED needs
      a passing test and a negative control that behaved; anything else is NOT_RUN.
  security_scan_record_chains {scan_id, explanations}   the chains the primitives
      in one file compose.
  security_scan_finalize {scan_id}, security_scan_cancel {scan_id},
  security_scan_export {scan_id, fmt}   seal into report.md, findings.json,
      results.sarif and findings.csv, then read one, sealed.
A finding is a rule, a severity, the exact path and line, its evidence state, its
taxonomy, its validation status and a fingerprint; never a secret's value. Zero
findings with chunks nobody read is not a clean result. Only the operator lets a
scan test a running app: /security authorize-validation <scanId> <origin>."
  "What (help :security-scan) answers while the cell runs.")

(nle:define-cell security-scan
  (:section ("security-scan")
    (:guide "nothing here is required to review a repository; allowed_origins is what lets a scan test your running app, and max_chunk_lines bounds one reviewer's reading")
    ("max_chunk_lines" :integer :default 4000 :min 200
                       :doc "how many lines one reviewer reads; the walk groups files into chunks of about this")
    ("dependency_check" :boolean :default t
                        :doc "ask api.osv.dev about the packages your manifests declare; no version, no code leaves")
    ("osv_url" :string :default "https://api.osv.dev"
               :doc "where the package query goes")
    ("allowed_origins" :list
                       :doc "the app addresses a scan may test once you allow it, each scheme://host[:port]")
    ("grant_minutes" :integer :default 60 :min 1
                     :doc "how long one validation grant lasts")
    ("grant_requests" :integer :default 200 :min 1
                       :doc "how many requests one grant allows"))
  (:tool "security_scan_start" (nlk:contract 'scan-start-params) 'tool-start)
  (:tool "security_scan_get" (nlk:contract 'scan-get-params) 'tool-get)
  (:tool "security_scan_checkpoint" (nlk:contract 'scan-checkpoint-params) 'tool-checkpoint)
  (:tool "security_scan_submit_worker_result" (nlk:contract 'scan-worker-params) 'tool-worker)
  (:tool "security_scan_record_validations" (nlk:contract 'scan-validation-params) 'tool-validations)
  (:tool "security_scan_record_chains" (nlk:contract 'scan-chain-params) 'tool-chains)
  (:tool "security_scan_finalize" (nlk:contract 'scan-seal-params) 'tool-finalize)
  (:tool "security_scan_cancel" (nlk:contract 'scan-seal-params) 'tool-cancel)
  (:tool "security_scan_export" (nlk:contract 'scan-export-params) 'tool-export)
  (:help :security-scan
         "security-scan: review a local repository for security bugs, with evidence, coverage and md/json/sarif/csv reports"
         +primer+)
  (:command "security" 'run-command
            :description "Security scans, and letting one test your running app"
            :argument-hint "status | authorize-validation <scan> <origin> | revoke <grant> | help")
  (:start (lambda ()
            (setf *scans* (make-hash-table :test #'equal)))))
