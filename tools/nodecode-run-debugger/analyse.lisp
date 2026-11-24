;;;; analyse.lisp --- what the recorded evidence supports, and nothing more.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Four readings of one imported run, each a pure function over the run
;;;; already in hand -- no subprocess, no socket, no database, no write
;;;; except the report's own file:
;;;;
;;;; TIMELINE   a partial order from the recorded causal parents, with the
;;;;            source sequence as the tie-break. Wall-clock order is not
;;;;            causation, and two events with no path between them are
;;;;            reported as concurrent rather than told as a story.
;;;; EXPLAIN    what the source REPORTS (failures, interrupts, anomalies),
;;;;            then hypotheses over it, ranked, each with its alternatives
;;;;            and what would falsify it; then the evidence that is missing,
;;;;            the read-only next checks, and the cost as reported.
;;;; REPLAY     the same order, walked, with the imported text shown as data.
;;;;            Nothing is executed, ever: "replay" re-reads, it does not rerun.
;;;;
;;;; Nothing here mints a correctness claim: a completed run is not a passed
;;;; task, and an absent error is not evidence of success.

(in-package #:nodecode-run-debugger)

;;; --- the rules, as data a layer can reword ---------------------------------------

(defparameter *rules*
  '(("R001" . "repeated provider errors consistent with retry exhaustion")
    ("R002" . "unrecovered tool operation with no later success of the same operation")
    ("R003" . "repeated identical operations consistent with a loop")
    ("R004" . "wrapper success and child failure describe different scopes")
    ("R005" . "evidence gaps limit what can be concluded")
    ("R006" . "long wall-clock gap without monotonic evidence")
    ("R007" . "reported argument or validation fault")
    ("R008" . "provider error the source did not classify")
    ("R009" . "run stopped through an interrupt or cancellation")
    ("R010" . "reported usage anomaly (not a cost conclusion)")
    ("R011" . "reported success that verified nothing")
    ("R012" . "identical reported errors repeated"))
  "The rule ids a hypothesis cites, and the one line each stands for.")

(defparameter *stall-seconds* 60
  "A wall-clock gap over this, with no monotonic evidence, is reported as a
suspect window -- as a hypothesis, never as a fact.")

(defparameter *usage-anomaly-tokens* 100000
  "One event reporting at least this many tokens is worth saying out loud; it
is a reported figure, not a cost conclusion.")

(defparameter *ambiguity-limit* 10
  "How many concurrent pairs a timeline names; above this the list stops
being read.")

(defparameter *ambiguity-events* 2000
  "Above this many events, ordering is still exact and ambiguity detection is
skipped in the text rather than ground through.")

;;; --- a partial order -------------------------------------------------------------

(defun event-index (events)
  "EVENT-ID -> event."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (event events) (setf (gethash (getf event :id) table) event))
    table))

(defun order-events (events)
  "EVENTS in the order the evidence supports. => (values ORDERED CYCLIC) where
ORDERED is a list of events, parents before children, ties broken by the
source sequence then the id; CYCLIC the ids a cycle left unorderable."
  (let ((index (event-index events))
        (children (make-hash-table :test #'equal))
        (indegree (make-hash-table :test #'equal))
        (key (make-hash-table :test #'equal)))
    (dolist (event events)
      (setf (gethash (getf event :id) indegree) 0
            (gethash (getf event :id) key) (cons (or (getf event :sequence) 0)
                                                  (getf event :id))))
    (dolist (event events)
      (dolist (parent (getf event :parents))
        (when (gethash parent index)
          (push (getf event :id) (gethash parent children))
          (incf (gethash (getf event :id) indegree)))))
    (let ((ready (sort (loop for event in events
                              when (zerop (gethash (getf event :id) indegree))
                                collect (getf event :id))
                       #'string< :key (lambda (id) (gethash id key))))
          (ordered '())
          (seen (make-hash-table :test #'equal)))
      (loop while ready
            for id = (pop ready)
            for event = (gethash id index)
            do (push event ordered)
               (setf (gethash id seen) t)
               (dolist (child (gethash id children))
                 (when (zerop (decf (gethash child indegree)))
                   (setf ready (sort (cons child ready) #'string< :key (lambda (c) (gethash c key)))))))
      (let ((cyclic (loop for event in events
                          unless (gethash (getf event :id) seen)
                            collect (getf event :id))))
        (values (nreverse ordered) cyclic)))))

(defun depths (ordered)
  "EVENT-ID -> depth from a root, over ORDERED (parents first)."
  (let ((depth (make-hash-table :test #'equal)))
    (dolist (event ordered)
      (let ((deepest -1))
        (dolist (parent (getf event :parents))
          (when (gethash parent depth)
            (setf deepest (max deepest (gethash parent depth)))))
        (setf (gethash (getf event :id) depth) (1+ deepest))))
    depth))

(defun ancestor-p (id candidate index)
  "Is ID reachable up the recorded parents from CANDIDATE?"
  (let ((seen (make-hash-table :test #'equal))
        (pending (list candidate))
        (found nil))
    (loop while (and pending (not found))
          for current = (pop pending)
          do (unless (gethash current seen)
               (setf (gethash current seen) t)
               (when (equal current id) (setf found t))
               (let ((event (gethash current index)))
                 (when event
                   (dolist (parent (getf event :parents))
                     (push parent pending))))))
    found))

(defun entries (run)
  "RUN's timeline entries: (ORDER DEPTH EVENT AMBIGUOUS-WITH)."
  (let* ((events (getf run :events))
         (index (event-index events)))
    (multiple-value-bind (ordered cyclic) (order-events events)
      (declare (ignore cyclic))
      (let ((depth (depths ordered))
            (result '())
            (order 0))
        (dolist (event ordered)
          (push (list (incf order) (gethash (getf event :id) depth) event) result))
        (setf result (nreverse result))
        ;; Two events with no path between them are concurrent, not a sequence.
        (when (<= (length result) *ambiguity-events*)
          (let ((pairs '()))
            (loop for tail on result
                  do (let ((a (third (first tail)))
                           (depth-a (second (first tail))))
                       (dolist (entry (rest tail))
                         (let ((b (third entry))
                               (depth-b (second entry)))
                           (when (and (= depth-a depth-b)
                                    (equal (getf a :attempt) (getf b :attempt))
                                    (not (ancestor-p (getf a :id) (getf b :id) index))
                                    (not (ancestor-p (getf b :id) (getf a :id) index))
                                    (< (length pairs) *ambiguity-limit*))
                           (push (cons (getf a :id) (getf b :id)) pairs)))))
            (setf result
                  (loop for (order depth event) in result
                        collect (list order depth event
                                      (loop for (a . b) in pairs
                                            when (equal a (getf event :id)) collect b
                                            when (equal b (getf event :id)) collect a)))))))
        result))))

(defun event-line (order depth event &optional ambiguous)
  "One timeline line for EVENT."
  (format nil "~4,'0d depth=~d ~a~@[ ~a~] status=~a~@[ at=~a~] [~a]~@[ tokens=~:d (reported)~]~@[ parents=~{~a~^,~}~]~@[ ambiguous-with=~{~a~^,~}~]"
          order depth (getf event :kind) (getf event :name) (getf event :status)
          (getf event :wall) (getf event :provenance)
          (let ((tokens (and (or (getf event :tokens-in) (getf event :tokens-out))
                             (+ (or (getf event :tokens-in) 0)
                                (or (getf event :tokens-out) 0)))))
            tokens)
          (getf event :parents) ambiguous))

(defun timeline (run-id)
  "RUN-ID's timeline as text: the recorded order, then the gaps and the
concurrent pairs it leaves visible."
  (running-settings)
  (let* ((run (find-run run-id))
         (ordered (entries run)))
    (with-output-to-string (out)
      (format out "timeline for run ~a~%ordering: recorded causal parents first, then attempt/source
sequence; wall-clock order is not causation.~%" (getf run :run-id))
      (format out "source: ~a, ~:d event~:p, digest ~a, completeness ~a~%"
              (getf (getf run :source) :name) (length (getf run :events))
              (getf run :digest) (getf run :completeness))
      (dolist (entry ordered)
        (destructuring-bind (order depth event &optional ambiguous) entry
          (format out "~a~%" (event-line order depth event ambiguous))))
      (format out "gaps: ~d~%" (length (getf run :gaps)))
      (dolist (gap (getf run :gaps))
        (format out "  - ~a — ~a~%" (getf gap :kind) (getf gap :detail))))))

(defun find-run (run-id)
  "The run RUN-ID, or a refusal naming what this cell does hold."
  (unless (and run-id (plusp (length (nlk:trimmed run-id))))
    (fail "a run id is required: import a trace first, then name the run it answered."))
  (or (run run-id)
      (fail "there is no imported run ~s here. The runs this cell holds: ~{[~a]~^ ~}."
            run-id (or (mapcar (lambda (r) (getf r :run-id)) (runs)) '("none")))))

;;; --- the diagnosis ---------------------------------------------------------------

(defun reported (text)
  "TEXT as one display line: control characters dropped, so a terminal escape
a source carried is shown, never obeyed."
  (let ((out (make-string-output-stream)))
    (loop for char across (or text "")
          do (write-char (if (or (< (char-code char) 32) (= (char-code char) 127)) #\Space char) out))
    (nlk:one-line (get-output-stream-string out))))

(defun zero-test-p (text)
  "Does TEXT report a success that verified nothing?"
  (and text (or (cl:search "0 tests" text :test #'char-equal)
                (cl:search "no tests" text :test #'char-equal)
                (cl:search "tests=0" text :test #'char-equal))))

(defun rate-limit-p (text)
  (and text (or (cl:search "429" text) (cl:search "rate limit" text :test #'char-equal)
                (cl:search "overloaded" text :test #'char-equal)
                (cl:search "too many requests" text :test #'char-equal))))

(defun of-kind (events kind)
  (remove-if-not (lambda (event) (equal kind (getf event :kind))) events))

(defun later-success-p (events event)
  "Does a later tool event of the same name report ok?"
  (let ((mine (or (getf event :name) "")))
    (some (lambda (other)
            (and (> (getf other :sequence) (getf event :sequence))
                 (equal (getf other :name) mine)
                 (equal "ok" (getf other :status))))
          events)))

(defun attempt-outcomes (run event)
  "The reported outcomes of the attempts EVENT belongs to."
  (let ((ids (list (getf event :attempt))))
    (remove-duplicates
     (loop for attempt in (getf run :attempts)
           when (member (getf attempt :id) ids :test #'equal)
             collect (or (getf attempt :outcome) "unknown"))
     :test #'equal)))

(defun hypothesis (rule statement confidence refs &key alternatives falsifiers)
  (list :rule rule :statement statement :confidence confidence :refs refs
        :alternatives alternatives :falsifiers falsifiers))

(defun observed-failures (run)
  "What the source reports, one plist each: (:kind :ref :detail)."
  (let ((observed '()))
    (dolist (event (getf run :events))
      (cond
        ((equal "failed" (getf event :status))
         (push (list :kind (format nil "failure:~a" (getf event :kind))
                     :ref (getf event :id)
                     :detail (format nil "the source reports a failed ~a~@[ (~a)~]~@[: ~a~]"
                                     (getf event :kind) (getf event :name)
                                     (and (getf event :error) (reported (getf event :error)))))
               observed))
        ((equal "interrupted" (getf event :status))
         (push (list :kind "interrupted:event" :ref (getf event :id)
                     :detail (format nil "the source reports an interrupted ~a" (getf event :kind)))
               observed)))
      (let ((tokens (and (or (getf event :tokens-in) (getf event :tokens-out))
                         (+ (or (getf event :tokens-in) 0) (or (getf event :tokens-out) 0)))))
        (when (and tokens (>= tokens *usage-anomaly-tokens*))
          (push (list :kind "anomaly:reported_usage" :ref (getf event :id)
                      :detail (format nil "~:d reported token~:p for one ~a is unusual; a
reported figure, not a cost conclusion" tokens (getf event :kind)))
                observed)))
      (when (and (equal "ok" (getf event :status)) (zero-test-p (getf event :error)))
        (push (list :kind "false_success:suspected" :ref (getf event :id)
                    :detail (format nil "the step reports success while its own text says
nothing was verified (~a); a success flag is not a test result"
                                    (reported (getf event :error))))
              observed)))
    (when (null (getf run :events))
      (push (list :kind "empty:no_events" :ref nil
                  :detail "no events were imported for this run, so nothing can be concluded")
            observed))
    (nreverse observed)))

(defun hypotheses (run)
  "The rules the evidence trips, ranked by confidence."
  (let* ((events (getf run :events))
         (found '())
         (providers (of-kind events "provider_error"))
         (tools (of-kind events "tool_call"))
         (failed-tools (remove-if-not (lambda (event) (equal "failed" (getf event :status))) tools))
         (gaps (getf run :gaps)))
    (when providers
      (let ((limited (remove-if-not (lambda (event) (rate-limit-p (getf event :error))) providers)))
        (push (hypothesis
               "R001"
               (format nil "consistent with provider-side failure or throttling exhausting the
retry budget: ~d provider error~:p reported~@[, ~d of them rate-limit or overload shaped~]"
                       (length providers) (and limited (length limited)))
               (if (>= (length providers) 3) "high" (if (= (length providers) 2) "medium" "low"))
               (mapcar (lambda (event) (getf event :id)) providers)
               :alternatives '("the errors are unrelated transients and the run failed for another reason"
                               "a local network or credential problem is presented as a server error")
               :falsifiers '("make one bounded request against the same provider from outside this
debugger and compare the reported error"
                             "compare the source's timestamps with the provider's own status history"))
              found))
      (let ((plain (remove-if (lambda (event) (rate-limit-p (getf event :error))) providers)))
        (when plain
          (push (hypothesis "R008"
                             (format nil "~d provider error~:p were not classified by the source, so
the cause is not identifiable from this log alone" (length plain))
                             "low" (mapcar (lambda (event) (getf event :id)) plain)
                             :alternatives '("the provider returned a documented error the source
did not record" "the error text was truncated or redacted before import")
                             :falsifiers '("obtain the provider's response body and compare it
with the recorded status"))
                found))))
    (let ((unrecovered (remove-if (lambda (event) (later-success-p tools event)) failed-tools)))
      (when unrecovered
        (let ((outcomes (attempt-outcomes run (first unrecovered))))
          (push (hypothesis
                 "R002"
                 (format nil "consistent with an unrecovered tool failure: ~d tool call~:p report
failure with no recorded later success of the same operation~@[ (~{~a~^, ~})~]; ~a"
                         (length unrecovered)
                         (remove nil (mapcar (lambda (event) (getf event :name)) unrecovered))
                         (cond ((equal outcomes '("failed"))
                                "the attempt's own outcome is reported failed")
                               ((equal outcomes '("interrupted"))
                                "the attempt is reported interrupted")
                               ((equal outcomes '("completed"))
                                "the attempt is reported completed, which is a finished run and
not a passed task")
                               (t "the run's terminal outcome is not recorded in this evidence,
so whether it ended at this step is unknown")))
                 "medium"
                 (mapcar (lambda (event) (getf event :id)) unrecovered)
                 :alternatives '("the operation was abandoned on purpose and the run moved on"
                                 "the failure was retried under another operation name, which this
evidence cannot connect")
                 :falsifiers '("read the referenced source lines and confirm the reported status
against the raw text" "look for a later success of the same operation under another name"))
                found))))
    (let ((loop-names '()))
      (dolist (event tools)
        (let ((same (remove-if-not (lambda (other)
                                      (and (equal (getf other :name) (getf event :name))
                                           (equal (getf other :status) (getf event :status))))
                                    tools)))
          (when (and (getf event :name) (>= (length same) 3)
                     (not (member (getf event :name) loop-names :test #'equal)))
            (push (getf event :name) loop-names)
            (push (hypothesis "R003"
                               (format nil "~d identical ~a operation~:p consistent with a loop,
not progress" (length same) (getf event :name))
                               "low" (mapcar (lambda (other) (getf other :id)) same)
                               :alternatives '("a legitimate batch over the same target"
                                               "a retry policy with backoff, recorded without waits")
                               :falsifiers '("read the arguments of the repeated calls: different
arguments are different operations"))
                  found))))
      (let ((errors (remove nil (mapcar (lambda (event) (getf event :error)) events))))
        (dolist (error errors)
          (let ((same (remove-if-not (lambda (other) (equal other error)) errors)))
            (when (and (>= (length same) 3)
                       (not (some (lambda (item) (equal "R012" (getf item :rule))) found)))
              (push (hypothesis "R012"
                                 (format nil "the same reported error repeats ~d times; the
mechanism is in the text, the scope is not" (length same))
                                 "low" '()
                                 :alternatives '("one root cause presented once per step"
                                                 "several causes sharing one message")
                                 :falsifiers '("read each referenced event's own status: the same
message can sit on steps that failed for different reasons"))
                    found))))))
    (let ((interrupts (remove-if-not (lambda (event)
                                       (or (equal "interrupt" (getf event :kind))
                                           (equal "interrupted" (getf event :status))))
                                     events)))
      (when interrupts
        (push (hypothesis "R009"
                           (format nil "the run stopped through an interrupt or cancellation
(~d event~:p)" (length interrupts))
                           "medium" (mapcar (lambda (event) (getf event :id)) interrupts)
                           :alternatives '("the operator stopped it" "a budget or a timeout did")
                           :falsifiers '("read the source's own interrupt record and say who
asked"))
              found)))
    (when gaps
      (push (hypothesis "R005"
                         (format nil "evidence gaps limit what can be concluded: ~{~a~^, ~}"
                                 (remove-duplicates (mapcar (lambda (gap) (getf gap :kind)) gaps)
                                                    :test #'equal))
                         "low" '()
                         :alternatives '("the source simply did not record the missing part"
                                         "the import bound cut it")
                         :falsifiers '("import a complete export of the same run and compare"))
            found))
    (sort found #'string< :key (lambda (item)
                                  (let ((confidence (getf item :confidence)))
                                    (format nil "~d~a"
                                            (cond ((equal confidence "high") 0)
                                                  ((equal confidence "medium") 1)
                                                  (t 2))
                                            (getf item :rule)))))))

(defun cost-of (run)
  "The reported usage, summed: what the source said, never a price invented."
  (let ((input 0) (output 0) (cost 0.0) (known 0) (unknown 0))
    (dolist (event (getf run :events))
      (if (or (getf event :tokens-in) (getf event :tokens-out) (getf event :cost))
          (progn (incf known)
                 (incf input (or (getf event :tokens-in) 0))
                 (incf output (or (getf event :tokens-out) 0))
                 (incf cost (or (getf event :cost) 0.0)))
          (incf unknown)))
    (list :input input :output output :cost cost :reported known :unknown unknown)))

(defun explain (run-id)
  "RUN-ID explained: what is reported, what it may mean, what is missing."
  (running-settings)
  (let* ((run (find-run run-id))
         (observed (observed-failures run))
         (guesses (hypotheses run))
         (cost (cost-of run))
         (gaps (getf run :gaps)))
    (with-output-to-string (out)
      (format out "diagnosis for run ~a~%observed (what the imported evidence reports): ~d~%"
              (getf run :run-id) (length observed))
      (dolist (item observed)
        (format out "  - ~a at ~a: ~a [provenance=reported]~%"
                (getf item :kind) (or (getf item :ref) "n/a") (getf item :detail)))
      (format out "hypotheses (evidence-linked, ranked): ~d~%" (length guesses))
      (dolist (item guesses)
        (format out "  - [~a] ~a (confidence: ~a; evidence: ~{~a~^, ~})~%"
                (getf item :rule) (getf item :statement) (getf item :confidence)
                (or (getf item :refs) '("none recorded")))
        (dolist (alternative (getf item :alternatives))
          (format out "      alternative reading: ~a~%" alternative))
        (dolist (falsifier (getf item :falsifiers))
          (format out "      falsify by: ~a~%" falsifier)))
      (format out "missing evidence:~%")
      (if gaps
          (dolist (gap gaps)
            (format out "  - ~a — ~a~%" (getf gap :kind) (getf gap :detail)))
          (format out "  - (none recorded)~%"))
      (format out "next checks (read-only; nothing is executed here):~%")
      (format out "  - read the referenced source lines and confirm each reported status
against the raw text~%")
      (format out "  - import a complete export of the same run and compare the gap list~%")
      (format out "cost (reported usage only): input ~:d, output ~:d, cost ~,4f; events reporting
usage ~d, events with unknown cost ~d~%"
              (getf cost :input) (getf cost :output) (getf cost :cost)
              (getf cost :reported) (getf cost :unknown))
      (format out "causal findings: none — a causal finding needs a controlled intervention,
which this cell does not perform~%")
      (format out "limitations:~%")
      (format out "  - an evidence reference is a pointer into the selected source, not a receipt~%")
      (format out "  - imported logs are testimony about a run, not proof of what happened~%")
      (format out "  - an absent error is not evidence of success: a missing terminal event
leaves the outcome unknown~%"))))

;;; --- the inert replay ---------------------------------------------------------------

(defun replay (run-id)
  "RUN-ID's timeline walked, nothing executed: the imported text is data."
  (running-settings)
  (let* ((run (find-run run-id))
         (entries (entries run)))
    (with-output-to-string (out)
      (format out "inert replay of run ~a (nothing is executed)~%"
              (getf run :run-id))
      (format out "note: replay re-reads the recorded timeline; no subprocess, no network, no
database, and no write happen~%")
      (format out "note: imported command and error text is displayed as data and is not
executed~%")
      (if (null entries)
          (format out "(no events to replay)~%")
          (dolist (entry entries)
            (destructuring-bind (order depth event &optional ambiguous) entry
              (declare (ignore depth))
              (format out "~4,'0d ~a~@[ ~a~] status=~a at=~a [~a]~@[ parent=~a~] :: ~a~%"
                      order (getf event :kind) (getf event :name) (getf event :status)
                      (or (getf event :wall) "unknown") (getf event :provenance)
                      (first (getf event :parents))
                      (reported (getf event :error)))
              (when ambiguous
                (dolist (other ambiguous)
                  (format out "ambiguous order: ~a <-> ~a (recorded as concurrent)~%"
                          (getf event :id) other))))))
      (dolist (gap (getf run :gaps))
        (format out "gap: ~a — ~a~%" (getf gap :kind) (getf gap :detail))))))

;;; --- the report -------------------------------------------------------------------

(defun report (run-id &optional out)
  "Write RUN-ID's timeline and diagnosis as markdown under the home (or to
OUT, the path the operator names); => the path written. The only write this
cell makes outside its run store."
  (running-settings)
  (let* ((run (find-run run-id))
         (path (or (and out (plusp (length (nlk:trimmed out))) (pathname (nlk:trimmed out)))
                   (merge-pathnames (format nil "~a.md" (nlk:short-digest (getf run :run-id)))
                                    (nlk:home "run-debugger/reports/")))))
    (ensure-directories-exist path)
    (nlk:write-file-atomically
     path
     (with-output-to-string (out)
       (format out "# Run ~a~%~%" (getf run :run-id))
       (format out "- source: ~a~%- digest: ~a~%- completeness: ~a~%- imported: ~a~%~%"
               (getf (getf run :source) :name) (getf run :digest) (getf run :completeness)
               (getf run :imported-at))
       (format out "## Timeline~%~%```~%~a```~%~%" (timeline (getf run :run-id)))
       (format out "## Diagnosis~%~%```~%~a```~%" (explain (getf run :run-id))))
     :mode #o600 :directory-mode #o700)
    (princ-to-string path)))

;;; --- the status -------------------------------------------------------------------

(defun status ()
  "What this debugger holds and what it reads, in text. Nothing leaves the
machine and no export route uploads anything."
  (running-settings)
  (let ((all (runs)))
    (with-output-to-string (out)
      (format out "run debugger: ~d imported run~:p~%" (length all))
      (dolist (run all)
        (format out "  ~a — ~:d event~:p, ~d gap~:p, ~a, imported ~a~%"
                (getf run :run-id) (length (getf run :events)) (length (getf run :gaps))
                (getf run :completeness) (getf run :imported-at)))
      (format out "lanes: import of one selected JSONL trace (format v1), timeline, explain,
replay, report — all local~%")
      (format out "refused in this build: capture (no hook), live session read, model narration,
sandboxed experiments, the Claude and Hermes transcript importers~%")
      (format out "bounds: max_bytes ~:d, max_line_bytes ~:d, max_events ~:d, allow_partial ~a,
retention_days ~d~%"
              (setting :max-bytes) (setting :max-line-bytes) (setting :max-events)
              (if (setting :allow-partial) "on" "off") (setting :retention-days))
      (format out "store: ~a~%" (princ-to-string (runs-directory))))))
