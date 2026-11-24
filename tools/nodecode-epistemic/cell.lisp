;;;; cell.lisp --- a case, its committed check, the observation, the revision.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An inquiry that could change its mind: before a selected check the model
;;;; writes down what it expects and what would change its belief; after the
;;;; check it records what it saw, whether that matched, and whether the
;;;; belief moved. Everything stays on this machine, in one file under the
;;;; home: a case is a plist in a list, and the file is its rendering.
;;;;
;;;; The prediction is the point. Without one, an agent reads the logs and
;;;; builds an explanation around whatever it happens to find; with one, the
;;;; observation has something to disagree with.

(in-package #:nodecode-epistemic)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "An epistemic case is one bounded inquiry: a decision, the unknown that could
change it, and a stopping condition. The nodecode-epistemic cell keeps it on
this machine; the model gets the epistemic_case and epistemic_probe tools.
  epistemic_case open    {case_id, decision, top_unknown, stopping_condition,
                          claims?, plan?} binds the case to your session.
  epistemic_case show    the receipt (the newest active case when case_id is
                          left out), epistemic_case list every case.
  epistemic_probe commit {tool_name, arguments, predicted?, would_change?}
                          records a prediction BEFORE the check runs; one
                          pending check per case.
  epistemic_probe compare {disposition, belief_change, rationale, evidence?}
                          records what the check showed, after it ran.
  epistemic_case revise  {claim_id, claim_status?, claim_text?, reason} after a
                          mismatch; epistemic_case close {outcome, summary}.
The check itself is an ordinary tool call, made separately: a commit does not
run anything. A mismatch marks the claim stale until a revision addresses it.
Nothing leaves this machine, and /case shows the session's case."
  "What (help :epistemic) answers while the cell runs.")

;;; --- small helpers --------------------------------------------------------------

(defun now () (get-universal-time))

(defun clip (text limit)
  "TEXT as a string, at most LIMIT characters, marked when cut."
  (let ((text (if (stringp text) text (princ-to-string (or text "")))))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "..."))))

(defun plist-set (plist key value)
  "Set KEY's value in PLIST in place: the key must already be there."
  (let ((tail (loop for tail on plist by #'cddr when (eq (car tail) key) return tail)))
    (if tail
        (setf (second tail) value)
        (error "~s is not a member of the record" key))
    value))

(defun answer (&rest members)
  "MEMBERS as the JSON text a tool answers with."
  (nlk:encode-json-object (apply #'nlk:make-json-object members)))

(defun refuse (text)
  (answer "ok" nil "error" text))

(defun items (texts prefix &optional (status "hypothesis"))
  "TEXTS as numbered items: ((:id \"C1\" :text ... :status ...) ...)."
  (loop for text in (coerce (or texts '()) 'list)
        for index from 1
        collect (list :id (format nil "~a~d" prefix index)
                      :text (princ-to-string text)
                      :status status
                      :stale nil)))

;;; --- the store -------------------------------------------------------------------
;;; One file, a list of case plists, written whole and replaced by rename: a
;;; torn write is never read back as a case.

(defvar *cases* '())
(defvar *loaded-p* nil)
(defvar *dir* "epistemic")

(defun store-path ()
  (nlk:home (format nil "~a/cases.sexp" (string-right-trim "/" *dir*))))

(defun read-store ()
  (let ((path (store-path)))
    (if (probe-file path)
        (handler-case
            (with-open-file (stream path :direction :input)
              (let ((*read-eval* nil)
                    (*package* (find-package '#:nodecode-epistemic)))
                (let ((form (read stream nil nil)))
                  (if (and (listp form) (every #'listp form)) form '()))))
          (error () '()))
        '())))

(defun write-store ()
  (let* ((path (store-path))
         (tmp (merge-pathnames "cases.sexp.new" path)))
    (ensure-directories-exist path)
    (with-open-file (stream tmp :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
      (let ((*print-pretty* t) (*print-length* nil) (*print-level* nil)
            (*package* (find-package '#:nodecode-epistemic)))
        (prin1 *cases* stream)
        (terpri stream)))
    (uiop:rename-file-overwriting-target tmp path)
    (values)))

(defun cases ()
  (unless *loaded-p*
    (setf *cases* (read-store) *loaded-p* t))
  *cases*)

(defun save () (write-store) (values))

(defun find-case (id)
  (and id (find id (cases) :key (lambda (c) (getf c :id)) :test #'string=)))

(defun live-cases ()
  (remove-if-not (lambda (c) (equal "active" (getf c :status))) (cases)))

(defun session-case (session)
  "The case SESSION has active, or NIL: the newest it opened."
  (and session
       (first (remove-if-not (lambda (c) (equal session (getf c :session)))
                             (live-cases)))))

(defun resolve-case (session id)
  "The case ID names, else the session's own, else the newest active one."
  (or (find-case id) (session-case session) (first (live-cases))))

(defun pending-probe (case)
  (and case (find "pending" (getf case :probes)
                  :key (lambda (p) (getf p :status)) :test #'string=)))

(defun trim-cases ()
  "Keep the newest `keep' closed cases and every active one."
  (let* ((keep (setting :keep))
         (closed (remove-if-not (lambda (c) (equal "closed" (getf c :status)))
                                 (cases)))
         (drop (nthcdr keep (sort (copy-list closed) #'>
                                   :key (lambda (c) (or (getf c :closed-at) 0))))))
    (when drop
      (setf *cases* (remove-if (lambda (c) (member c drop)) (cases))))))

;;; --- a case, as text and as data ---------------------------------------------------

(defun case-line (case)
  (format nil "~a [~a] round ~d: ~a"
          (getf case :id) (getf case :status) (getf case :round)
          (clip (getf case :top-unknown) 160)))

(defun describe-case (case &key (detail t))
  "CASE as the lines the model reads: the decision, the claims, the last
check, and where it stands."
  (let* ((probe (pending-probe case))
         (last (first (getf case :probes)))
         (lines
           (list (case-line case)
                 (format nil "  decision: ~a" (getf case :decision))
                 (format nil "  stop when: ~a" (getf case :stopping-condition)))))
    (dolist (item (getf case :claims))
      (push (format nil "  claim ~a [~a~@[ stale~]]: ~a"
                    (getf item :id) (getf item :status) (getf item :stale)
                    (getf item :text))
            lines))
    (dolist (item (getf case :plan))
      (push (format nil "  plan ~a: ~a" (getf item :id) (getf item :action)) lines))
    (when last
      (push (format nil "  last check ~a on ~a [~a]~@[: ~a~]"
                    (getf last :id) (getf last :tool) (getf last :status)
                    (and (getf last :observation) (getf last :observation :disposition)))
            lines))
    (when probe
      (push (format nil "  pending ~a on ~a: ~{~a~^; ~}~@[ -- expected to change: ~a~]"
                    (getf probe :id) (getf probe :tool)
                    (coerce (getf probe :predicted) 'list)
                    (getf probe :would-change))
            lines))
    (when detail
      (unless probe
        (push "  no check is pending: commit one before the next investigation, or close."
              lines)))
    (when (equal "closed" (getf case :status))
      (push (format nil "  closed ~a: ~a" (getf case :outcome) (getf case :summary)) lines))
    (format nil "~{~a~%~}" (nreverse lines))))

(defun case-object (case)
  "CASE as the JSON a tool answers with."
  (nlk:make-json-object
         "id" (getf case :id)
         "status" (getf case :status)
         "round" (getf case :round)
         "decision" (getf case :decision)
         "top_unknown" (getf case :top-unknown)
         "stopping_condition" (getf case :stopping-condition)
         "claims" (coerce (loop for item in (getf case :claims)
                                collect (nlk:make-json-object
                                         "id" (getf item :id)
                                         "text" (getf item :text)
                                         "status" (getf item :status)
                                         "stale" (and (getf item :stale) t)))
                          'vector)
         "plan" (coerce (mapcar (lambda (item)
                                   (nlk:make-json-object "id" (getf item :id)
                                                         "action" (getf item :action)))
                                 (getf case :plan))
                        'vector)
         "probes" (coerce (mapcar (lambda (probe)
                                    (nlk:make-json-object
                                     "id" (getf probe :id)
                                     "tool" (getf probe :tool)
                                     "status" (getf probe :status)
                                     "round" (getf probe :round)
                                     "would_change" (getf probe :would-change)
                                     "observed" (getf probe :observed)
                                     "disposition" (getf probe :observation :disposition)))
                                  (getf case :probes))
                          'vector)
         "evidence" (coerce (mapcar (lambda (row)
                                       (nlk:make-json-object "summary" (getf row :summary)
                                                             "source" (getf row :source)))
                                     (getf case :evidence))
                            'vector)
         "outcome" (getf case :outcome)
         "summary" (getf case :summary)))

;;; --- the operations ---------------------------------------------------------------

(defun open-case (session id decision top stopping claims plan)
  (let ((existing (find-case id)))
    (when (and existing (equal "active" (getf existing :status)))
      (return-from open-case
        (values nil (format nil "case ~a is already open on this session's store; ~
show it, or close it first" id))))
    (when existing (setf *cases* (remove existing (cases))))
    (let ((case (list :id id :session session :status "active" :round 1
                      :opened-at (now) :closed-at nil
                      :decision (clip decision (setting :case-chars))
                      :top-unknown (clip top (setting :case-chars))
                      :stopping-condition (clip stopping (setting :case-chars))
                      :claims (items claims "C")
                      :plan (mapcar (lambda (item)
                                      (list :id (getf item :id)
                                            :action (getf item :text)))
                                    (items plan "P"))
                      :probes '() :evidence '() :revisions '()
                      :outcome nil :summary nil)))
      (push case *cases*)
      (save)
      (values case nil))))

(defun commit-probe (case tool args predicted would claim-id)
  (let ((pending (pending-probe case)))
    (cond
      (pending
       (values nil (format nil "check ~a on ~a is still pending: compare or discard it first"
                           (getf pending :id) (getf pending :tool))))
      ((zerop (length (or predicted '())))
       (values nil "commit needs at least one predicted outcome: what would the check show?"))
      (t
       (let ((probe (list :id (format nil "q~d" (1+ (length (getf case :probes))))
                          :round (getf case :round)
                          :tool (clip tool 200)
                          :arguments (clip args 600)
                          :predicted (map 'list (lambda (one) (clip one 300)) predicted)
                          :would-change (clip would (setting :case-chars))
                          :claim (and claim-id
                                      (find claim-id (getf case :claims)
                                            :key (lambda (c) (getf c :id)) :test #'string=))
                          :status "pending" :at (now) :observed nil
                          :observation nil)))
         (push probe (getf case :probes))
         (save)
         (values probe nil))))))

(defun compare-probe (case disposition belief rationale evidence source)
  (let ((probe (pending-probe case)))
    (if (null probe)
        (values nil "no check is pending: commit one before the check runs, then compare it")
        (let ((bound (getf probe :claim)))
          (setf (getf probe :status) "compared")
          (setf (getf probe :observation)
                (list :disposition disposition :belief-change belief
                      :rationale (clip rationale (setting :case-chars))
                      :at (now)))
          (when (and (equal disposition "mismatch") bound)
            (plist-set bound :stale t))
          (when (and (equal disposition "mismatch") (null bound))
            (dolist (item (getf case :claims)) (plist-set item :stale t)))
          (when (and evidence (plusp (length evidence)))
            (push (list :summary (clip evidence (setting :case-chars))
                        :source (clip (or source "") 300) :at (now)
                        :probe (getf probe :id))
                  (getf case :evidence)))
          (when (equal belief "changed")
            (setf (getf case :round) (1+ (getf case :round))))
          (save)
          (values probe nil)))))

(defun discard-probe (case reason)
  (let ((probe (pending-probe case)))
    (if (null probe)
        (values nil "no check is pending to discard")
        (progn
          (setf (getf probe :status) "discarded")
          (setf (getf probe :observation) (list :disposition "discarded"
                                                :rationale (clip reason (setting :case-chars))))
          (save)
          (values probe nil)))))

(defun revise-case (case claim-id claim-status claim-text reason)
  (let ((item (and claim-id (find claim-id (getf case :claims)
                                   :key (lambda (c) (getf c :id)) :test #'string=))))
    (cond
      ((null reason) (values nil "revise needs a reason: what the observation changed"))
      ((and claim-id (null item)) (values nil (format nil "no claim ~a in this case" claim-id)))
      (t
       (when item
         (when claim-status (plist-set item :status claim-status))
         (when (and claim-text (plusp (length claim-text))) (plist-set item :text claim-text))
         (plist-set item :stale nil))
       (push (list :at (now) :reason (clip reason (setting :case-chars))
                   :claim (or claim-id "every claim")
                   :status claim-status :text claim-text)
             (getf case :revisions))
       (save)
       (values case nil)))))

(defun close-case (case outcome summary lesson)
  (cond
    ((pending-probe case)
     (values nil "compare or discard the pending check before closing"))
    ((null summary) (values nil "close needs a summary of what the case established"))
    (t
     (setf (getf case :status) "closed")
     (setf (getf case :outcome) outcome)
     (setf (getf case :summary) (clip summary (setting :case-chars)))
     (setf (getf case :closed-at) (now))
     (when (and lesson (plusp (length lesson)))
       (push (list :at (now) :lesson (clip lesson (setting :case-chars)))
             (getf case :revisions)))
     (trim-cases)
     (save)
     (values case nil))))

;;; --- the tools ---------------------------------------------------------------------

(nlk:define-contract case-request
  (:doc "Manage one epistemic case: a bounded inquiry with a decision, the
unknown that could change it and a stopping condition. Open one before an
investigation that could change a belief, commit a prediction with the probe
tool before the check, then revise or close here. A case lives on this machine,
bound to the session that opened it.")
  ("operation" :text :required t
   :options '("open" "show" "list" "revise" "close")
   :doc "what to do: open a case, show one, list them, revise a claim after a mismatch, or close")
  ("case_id" :text
   :doc "the case's short id, lowercase letters, digits and dashes; the newest active case when left out")
  ("decision" :text :doc "open: the decision the case is about, one sentence")
  ("top_unknown" :text :doc "open: the one unknown most likely to change that decision")
  ("stopping_condition" :text :doc "open: what would end the inquiry")
  ("claims" :array :of :string :doc "open: what you currently believe, one per entry")
  ("plan" :array :of :string :doc "open: the checks you mean to run")
  ("claim_id" :text :doc "revise: the claim to change, as show reports it")
  ("claim_status" :text :options '("grounded" "hypothesis" "unresolved" "refuted")
   :doc "revise: what the observation leaves the claim as")
  ("claim_text" :text :doc "revise: the claim's new wording")
  ("reason" :text :doc "revise: why, named against the observation")
  ("outcome" :text :options '("supported" "provisional" "mixed" "unresolved" "refuted")
   :doc "close: what the case established")
  ("summary" :text :doc "close: one paragraph, without claiming more than the checks showed")
  ("lesson" :text :doc "close: one reusable sentence, or leave it out"))

(nlk:define-contract probe-request
  (:doc "Record a prediction before one check runs, then what the check showed.
A commit runs nothing: make the ordinary tool call yourself, then compare. A
mismatch marks the claim stale until the case tool revises it.")
  ("operation" :text :required t :options '("commit" "compare" "discard")
   :doc "commit a prediction, compare it with what the check showed, or discard a check that never ran")
  ("case_id" :text :doc "the case; the newest active one when left out")
  ("tool_name" :text :doc "commit: the tool you are about to call")
  ("arguments" :text :doc "commit: the arguments you will pass, as text")
  ("predicted" :array :of :string
   :doc "commit: what you expect to see, and what each outcome would mean")
  ("would_change" :text :doc "commit: the observation that would change your belief")
  ("claim_id" :text :doc "commit: the claim this check is bound to")
  ("disposition" :text :options '("match" "mismatch" "unresolved")
   :doc "compare: whether the observation matched the prediction")
  ("belief_change" :text :options '("changed" "unchanged" "unresolved")
   :doc "compare: whether your belief moved; a match can leave it unchanged")
  ("rationale" :text :doc "compare: what the observation showed and what you made of it")
  ("evidence" :text :doc "compare: a short summary of what the check returned")
  ("source" :text :doc "compare: where it came from, a file, a URL, a command"))

(defun case-tool (arguments)
  (let* ((session (getf (nle:turn) :session-id))
         (operation (case-request-operation arguments))
         (id (case-request-case-id arguments)))
    (cond
      ((equal operation "list")
       (let ((rows (cases)))
         (answer "ok" t
                 "cases" (coerce (mapcar (lambda (case)
                                            (nlk:make-json-object
                                             "id" (getf case :id)
                                             "status" (getf case :status)
                                             "top_unknown" (getf case :top-unknown)))
                                          rows)
                                  'vector)
                 "count" (length rows)
                 "text" (if rows
                            (format nil "~{~a~%~}" (mapcar #'case-line rows))
                            "no cases yet: open one with the epistemic_case tool"))))
      ((equal operation "open")
       (destructuring-bind (&key case-id decision top-unknown stopping-condition
                              claims plan)
           (list :case-id id
                 :decision (case-request-decision arguments)
                 :top-unknown (case-request-top-unknown arguments)
                 :stopping-condition (case-request-stopping-condition arguments)
                 :claims (case-request-claims arguments)
                 :plan (case-request-plan arguments))
         (cond
           ((null case-id) (refuse "open needs a case_id: lowercase letters, digits and dashes"))
           ((null decision) (refuse "open needs a decision: what this case decides"))
           ((null top-unknown) (refuse "open needs a top_unknown: the one thing that could change it"))
           ((null stopping-condition) (refuse "open needs a stopping_condition: what ends the inquiry"))
           (t
            (multiple-value-bind (case problem)
                (open-case session case-id decision top-unknown stopping-condition
                           claims plan)
              (if problem
                  (refuse problem)
                  (answer "ok" t "case" (case-object case)
                          "text" (describe-case case)
                          "next" "commit a prediction with the epistemic_probe tool before the check runs")))))))
      (t
       (let ((case (resolve-case session id)))
         (cond
           ((null case) (refuse "no case here: open one with the epistemic_case tool"))
           ((equal operation "show")
            (answer "ok" t "case" (case-object case) "text" (describe-case case)))
           ((equal operation "revise")
            (multiple-value-bind (revised problem)
                (revise-case case (case-request-claim-id arguments)
                             (case-request-claim-status arguments)
                             (case-request-claim-text arguments)
                             (case-request-reason arguments))
              (if problem
                  (refuse problem)
                  (answer "ok" t "case" (case-object revised)
                          "text" (describe-case revised)
                          "next" "close the case when the decision is settled, or commit the next check"))))
           ((equal operation "close")
            (multiple-value-bind (closed problem)
                (close-case case (or (case-request-outcome arguments) "unresolved")
                            (case-request-summary arguments)
                            (case-request-lesson arguments))
              (if problem
                  (refuse problem)
                  (answer "ok" t "case" (case-object closed)
                          "text" (describe-case closed)))))
           (t (refuse (format nil "unknown operation ~s" operation)))))))))

(defun probe-tool (arguments)
  (let* ((session (getf (nle:turn) :session-id))
         (operation (probe-request-operation arguments))
         (case (resolve-case session (probe-request-case-id arguments))))
    (cond
      ((null case) (refuse "no case here: open one with the epistemic_case tool first"))
      ((equal "closed" (getf case :status)) (refuse "this case is closed: open a new one"))
      ((equal operation "commit")
       (multiple-value-bind (probe problem)
           (commit-probe case (probe-request-tool-name arguments)
                         (probe-request-arguments arguments)
                         (probe-request-predicted arguments)
                         (probe-request-would-change arguments)
                         (probe-request-claim-id arguments))
         (if problem
             (refuse problem)
             (answer "ok" t "case" (case-object case) "check" (getf probe :id)
                     "text" (describe-case case)
                     "next" "make the ordinary tool call now, in a call of its own, then compare"))))
      ((equal operation "compare")
       (multiple-value-bind (probe problem)
           (compare-probe case (probe-request-disposition arguments)
                          (probe-request-belief-change arguments)
                          (probe-request-rationale arguments)
                          (probe-request-evidence arguments)
                          (probe-request-source arguments))
         (if problem
             (refuse problem)
             (answer "ok" t "case" (case-object case) "check" (getf probe :id)
                     "text" (describe-case case)
                     "next" (if (equal (probe-request-disposition arguments) "mismatch")
                                "revise the claim the observation contradicts, then close"
                                "close the case when the decision is settled, or commit the next check")))))
      ((equal operation "discard")
       (multiple-value-bind (probe problem)
           (discard-probe case (or (probe-request-rationale arguments)
                                   (probe-request-evidence arguments)))
         (if problem
             (refuse problem)
             (answer "ok" t "case" (case-object case) "check" (getf probe :id)
                     "text" (describe-case case)))))
      (t (refuse (format nil "unknown operation ~s" operation))))))

;;; --- /case ------------------------------------------------------------------------

(defun run-slash (args session-id)
  "One /case invocation: the session's case, one by id, or every case.
=> (values TEXT DIALOG)."
  (let* ((words (nlk:split-words (or args "")))
         (word (string-downcase (or (first words) ""))))
    (cond
      ((equal word "list")
       (format nil "~{~a~%~}" (mapcar #'case-line (cases))))
      ((and (plusp (length word)) (find-case word))
       (describe-case (find-case word)))
      ((plusp (length word))
       (format nil "No case ~a. /case lists them; /case alone shows this session's." word))
      (t
       (let ((case (resolve-case session-id nil)))
         (if case
             (describe-case case)
             "No case on this session. Ask for one to be opened: an epistemic case records a prediction before a check and what the check showed after."))))))

;;; --- the two hooks -----------------------------------------------------------------

(defparameter *reminder*
  "[epistemic case ~a (round ~d): unknown ~a~%; decision ~a~%; ~@[check ~a on ~a is pending: ~{~a~^; ~}; after that call, compare it.~%~]Stop when: ~a. A case is a record of what was predicted, not a claim that it held.]"
  "The line a running case puts in front of the model at each request.")

(defun reminder (case)
  (let ((probe (pending-probe case)))
    (clip (format nil *reminder*
                  (getf case :id) (getf case :round)
                  (clip (getf case :top-unknown) 200)
                  (clip (getf case :decision) 200)
                  (and probe (getf probe :id)) (and probe (getf probe :tool))
                  (and probe (coerce (getf probe :predicted) 'list))
                  (clip (getf case :stopping-condition) 200))
          (setting :remind-chars))))

(defun observe-round (op)
  "At a model request: the session's running case, in one reminder."
  (let ((session (getf op :session-id))
        (turn (getf op :turn)))
    (when (and session turn (setting :remind))
      (let ((case (session-case session)))
        (when case
          (nlk:record-turn-context-prompt turn :text (reminder case)))))))

(defun observe-tool (op next)
  "Around one tool call: the committed check's own result, kept for compare.
The call runs first and is answered untouched; a store error here never fails
it."
  (let ((result (funcall next op)))
    (when (setting :capture)
      (handler-case
          (let* ((session (getf (nle:turn) :session-id))
                 (case (session-case session))
                 (probe (pending-probe case))
                 (name (getf op :name)))
            (when (and probe name (equal name (getf probe :tool)))
              (plist-set probe :observed
                         (clip (princ-to-string result) (setting :observe-chars)))
              (save)))
        (error (condition) (declare (ignore condition)) nil)))
    result))

;;; --- start -------------------------------------------------------------------------

(defun check-dir ()
  "Make the store's directory, or refuse the section naming dir."
  (handler-case (ensure-directories-exist (store-path))
    (error (condition)
      (declare (ignore condition))
      (nlk:config-error "dir ~s: no store can be written there"
                        (setting :dir)))))

(defun start-store ()
  (setf *dir* (princ-to-string (setting :dir)))
  (check-dir)
  (setf *cases* (read-store) *loaded-p* t)
  (nle:on-stop (lambda () (setf *cases* '() *loaded-p* nil)))
  (when (null (cases))
    (nle:notice "epistemic: no case yet; ask for one to be opened before a check that could change a belief"
                :key "nodecode-epistemic")))

(nlk:define-peripheral epistemic :not-running t)

(nle:define-cell epistemic
  (:section ("epistemic")
    (:guide "Local only: a case file under the home, no key and no network; dir moves the file, remind keeps the running case in front of the model")
    ("dir" :string :default "epistemic"
     :doc "where the cases file lives, under the Nodecode home")
    ("remind" :boolean :default t
     :doc "put the session's running case in front of the model at each request")
    ("remind_chars" :integer :default 900 :min 200
     :doc "how much of the reminder one request carries")
    ("capture" :boolean :default t
     :doc "keep the committed check's own tool result, so compare can be answered against it")
    ("observe_chars" :integer :default 600 :min 100
     :doc "how much of that result is kept")
    ("case_chars" :integer :default 1200 :min 200
     :doc "the longest single piece of case text kept")
    ("keep" :integer :default 50 :min 1
     :doc "how many closed cases stay in the file; an active one always stays"))
  (:tool "epistemic_case" (nlk:contract 'case-request) 'case-tool)
  (:tool "epistemic_probe" (nlk:contract 'probe-request) 'probe-tool)
  (:help :epistemic "epistemic: a case records a prediction before a check and what the check showed; epistemic_case and epistemic_probe" +primer+)
  (:command "case" 'run-slash
            :description "The session's epistemic case: the decision, the claims, the pending check"
            :argument-hint "[list|CASE-ID]")
  (:hook :round (nle:observer #'observe-round "epistemic"))
  (:hook :tool #'observe-tool)
  (:start #'start-store))
