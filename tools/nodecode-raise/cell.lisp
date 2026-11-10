;;;; cell.lisp --- the desk: thirteen tools, one command, one manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model ranks, writes and proposes; Raise checks. It proposes a score a
;;;; dimension with cited evidence ids, a name to draft to, a drafted text, and
;;;; the code verifies the citations, keeps the route inside the caps and lints
;;;; the draft before it exists. Nothing is ever sent: a draft is text the
;;;; founder pastes into their own mail client.

(in-package #:nodecode-raise)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Raise is a draft-only fundraising desk, in the raise: package, called through
eval or by the raise_* tools. Every verb answers a string; a refusal is
ERROR: RAISE-ERROR with the reason.
  (raise:focus &optional n)   the ranked investors worth working now.
  (raise:can-draft \"name\")   whether Raise may draft to that investor, and on
      which route: warm intro, a connected DM, a connect request, an email, a
      firm inbox -- warmest first, held for a few working days after a pending
      ask, and capped per channel over a window, so no one is pestered.
  (raise:lint-draft TEXT :facts FACTS)   the code checks on a drafted text:
      every number must be in a verified claim or a cited fact, the text must
      name something the investor actually did, and the style tells (em dashes,
      'not X but Y', a three-item list, a bold label, a staged opener, a stock
      AI phrase) come back as warnings.
  (raise:brief)   at most five actions today, replies first.
  (raise:status-text)   the pipeline in numbers, and where the store is.
The pipeline is raise/pipeline.json under the home. Rank on evidence: the model
proposes 0-10 a dimension and cites evidence ids, and the code zeroes any
dimension cited by nothing. A pass at a firm is a pass on the whole firm. Raise
never sends: it stops at the draft."
  "What (help :raise) answers while the cell runs.")

;;; --- the boundaries ------------------------------------------------------------

(nlk:define-contract profile-call
  (:doc "Read or write the founder's profile. With no members, answers it; FIELDS
merges the members the founder just answered, and CONFIRM marks the profile read
back and agreed. Profile members: company, one_liner, sector, stage, round_amount,
round_currency, traction, ideal_investor, anti_profile, voice.")
  ("fields" :object :doc "profile members to set, by their own name")
  ("confirm" :boolean :doc "the founder confirmed the profile as it reads"))

(nlk:define-contract claim-call
  (:doc "Record one claim the founder made about their company, tagged verified
(the founder can show its source), unverified (true as far as they know) or
projection (a plan). Every number in a draft must come from a verified claim or
from a cited fact about the investor.")
  ("text" :text :required t :doc "the claim in one sentence")
  ("tag" :string :required t :options '("verified" "unverified" "projection")
   :doc "how far the founder can stand behind it")
  ("source" :string :doc "where it is shown: a link, a dashboard, a filing"))

(nlk:define-contract investor-call
  (:doc "Add one investor to the pipeline, or update the one already there by
that name. An investor is a person at a firm; Raise only drafts to a person.")
  ("name" :string :doc "the person, as they write it")
  ("firm" :string :doc "the firm the person invests from")
  ("kind" :string :options '("vc" "angel" "angel_group" "corporate" "family_office")
   :doc "what the firm is; an angel is not judged on deal recency")
  ("email" :string :doc "the person's address")
  ("email_confirmed" :boolean :doc "whether that address is known to work")
  ("linkedin" :string :doc "the person's LinkedIn profile url")
  ("connected" :boolean :doc "whether the founder is already connected on LinkedIn")
  ("stage" :string :options '("new" "researching" "ready" "contacted" "in_talks")
   :doc "where they stand in the pipeline"))

(nlk:define-contract import-call
  (:doc "Import the founder's existing investor list: ROWS from their own
tool, or CSV, one investor a line with a header naming the columns. Columns are
matched by name (name, person, firm, company, email, linkedin, stage); a row
already in the pipeline by name and firm is updated, not doubled.")
  ("rows" :array :of :object :doc "investors, one object each, keyed by column name")
  ("csv" :text :doc "a CSV with a header line"))

(nlk:define-contract evidence-call
  (:doc "Record one cited fact about an investor: a deal, a thesis page, a post,
a portfolio company. Every ranking and every draft must stand on these, so the
source url is required.")
  ("investor" :text :required t :doc "the investor's name, or their id from raise_focus")
  ("kind" :string :required t :options '("deal" "thesis" "portfolio" "post" "team" "news")
   :doc "what the fact is")
  ("claim" :text :required t :doc "the fact, in one sentence")
  ("source_url" :text :required t :doc "where it is written")
  ("date" :string :doc "when it was published, YYYY-MM-DD")
  ("amount" :number :doc "a cheque size, when the fact is a deal")
  ("stage" :string :doc "the stage the deal was at, when the fact is a deal"))

(nlk:define-contract rank-call
  (:doc "Score one investor on cited evidence. DIMS holds 0-10 a dimension,
keyed thesis (22), similar_companies (18), recency (14), stage_fit (10), lead (8),
fund_cycle (8), reachability (8, computed from the route, no citation), value_add
(7), content (5), each with score, why and evidence_ids from raise_evidence.
The code zeroes any dimension that cites nothing and computes the total.")
  ("investor" :text :required t :doc "the investor's name, or their id")
  ("dims" :object :required t :doc "one object a dimension: {\"score\": 8, \"why\": \"...\", \"evidence_ids\": [3, 4]}"))

(nlk:define-contract focus-call
  (:doc "The ranked investors worth working now: ranked, not passed, open stage,
one person a firm, best first.")
  ("n" :integer :doc "how many, default focus_size")
  ("min_score" :number :doc "leave out anyone below this"))

(nlk:define-contract show-call
  (:doc "One investor in full: score a dimension, the evidence cited, the log,
the route and the draft gate.")
  ("investor" :text :required t :doc "the investor's name, or their id"))

(nlk:define-contract can-draft-call
  (:doc "Whether Raise may draft to this investor now, and on which channel,
without drafting: the gate's reasons, the route and its caps.")
  ("investor" :text :required t :doc "the investor's name, or their id"))

(nlk:define-contract lint-call
  (:doc "The code checks on a drafted text before it is shown to the founder:
every number in it must be in the founder's verified claims or in the cited
facts of the investor it is for, and it must mention something that investor
did. Findings marked hard mean the draft must be rewritten.")
  ("text" :text :required t :doc "the draft")
  ("investor" :text :doc "whose draft it is; their cited facts are what it may claim")
  ("channel" :string :default "email"
   :options '("email" "firm_inbox" "linkedin_dm" "connect" "intro" "x")
   :doc "how it reaches them: the word cap and the close depend on it")
  ("max_words" :integer :doc "override the channel's word cap"))

(nlk:define-contract log-call
  (:doc "Record what happened with an investor: a touch (the founder reached
out), a reply (they answered), a pass, an in-talks, a committed cheque, or a note.
A pass applies to the whole firm.")
  ("investor" :text :required t :doc "the investor's name, or their id")
  ("kind" :string :required t
   :options '("touch" "reply" "pass" "soft_pass" "in_talks" "committed" "note")
   :doc "what happened")
  ("channel" :string :options '("email" "linkedin_dm" "connect" "intro" "firm_inbox" "x")
   :doc "where it happened")
  ("direction" :string :options '("out" "in") :doc "who moved: out is the founder")
  ("summary" :string :doc "one line the founder would recognize"))

(nlk:define-contract brief-call
  (:doc "What to do today: at most five actions, replies first, then research
and ranking, then new outreach. Do them in order.")
  ("n" :integer :doc "how many actions, at most five"))

(nlk:define-contract export-call
  (:doc "The pipeline as rows (JSON) or as CSV text, for the founder's own CRM.
A draft's text travels with its investor's row.")
  ("format" :string :default "rows" :options (quote ("rows" "csv")) :slot form
   :doc "rows for a tool to read, csv for a spreadsheet"))

;;; --- what the desk says ---------------------------------------------------------

(defun profile-line (key)
  "One profile member as a line, or NIL when the founder has not answered it."
  (let ((value (getf (profile) key)))
    (when (and value (plusp (length (princ-to-string value))))
      (format nil "  ~a: ~a" (ppcre:regex-replace-all "_" (string-downcase (symbol-name key)) " ")
              value))))

(defun profile-text ()
  "The profile and the claims the drafts may use."
  (let ((lines (remove nil (mapcar #'profile-line
                                   '(:company :one-liner :sector :stage :round-amount
                                     :round-currency :traction :ideal-investor :anti-profile
                                     :voice))))
        (verified (verified-claims)))
    (format nil "~:[profile: nothing yet; raise_profile sets it~;profile:~%~{~a~}~]~%verified claims: ~d~@[~%~{~a~}~]"
            lines lines (length verified)
            (when verified
              (loop for claim in verified
                    collect (format nil "  #~a ~a~@[ (~a)~]" (getf claim :id) (getf claim :text)
                                    (getf claim :source)))))))

(defun awaiting-reply-p (record)
  "Whether the investor wrote last, so an answer is owed."
  (let ((ins (touches record :direction "in"))
        (outs (touches record)))
    (and ins
         (let ((last-in (parse-stamp (getf (car (last ins)) :at)))
               (last-out (and outs (parse-stamp (getf (car (last outs)) :at)))))
           (or (null last-out) (>= last-in last-out))))))

(defun pipeline-report ()
  "The pipeline in numbers."
  (let* ((all (remove-if (lambda (r) (or (getf r :archived) (getf r :excluded))) (investors)))
         (ranked (count-if (lambda (r) (getf r :score)) all))
         (awaiting (count-if #'awaiting-reply-p all))
         (by-stage (loop for stage in +open-stages+ collect (cons stage 0)))
         (touched (count-if (lambda (r) (touches r)) all)))
    (dolist (record all)
      (let ((cell (assoc (stage-of record) by-stage :test #'equal)))
        (when cell (incf (cdr cell)))))
    (format nil "Raise: ~d investor~:p, ~d ranked, ~d written to, ~d awaiting your reply.~%stages: ~{~a ~d~^, ~}~%store: ~a"
            (length all) ranked touched awaiting
            (loop for (stage . count) in by-stage append (list stage count))
            (namestring (store-file)))))

(defun score-text (record)
  (let ((score (getf record :score)))
    (if (numberp score) (format nil "~,1f" (float score 1.0)) "unranked")))

(defun focus-list (n min-score)
  "The investors worth working now, best first: ranked, not passed, open stage,
one a firm, ready to be drafted to."
  (let ((rows (remove-if-not
               (lambda (record)
                 (and (getf record :score)
                      (not (getf record :archived))
                      (not (getf record :excluded))
                      (member (stage-of record) +open-stages+ :test #'equal)
                      (not (equal (stage-of record) "passed"))
                      (or (null min-score) (>= (getf record :score) min-score))))
               (investors))))
    (setf rows (sort (copy-list rows) #'> :key (lambda (r) (float (getf r :score) 1.0))))
    ;; the firm is represented by the person a conversation is live with, else its best
    (let ((seen '()) (out '()))
      (dolist (record rows)
        (let ((key (name-key (getf record :firm))))
          (unless (and (not (individual-p record)) (member key seen :test #'string=))
            (when (and (not (individual-p record)) (plusp (length key)))
              (push key seen))
            (push record out))))
      (let ((count (or n (setting :focus-size))))
        (if (> (length out) count) (subseq out 0 count) out)))))

(defun focus-text (&optional n min-score)
  (let ((rows (focus-list n min-score)))
    (if (null rows)
        "no ranked investor yet: raise_evidence, then raise_rank, then raise_focus"
        (format nil "~d worth working now, best first:~%~{~a~}"
                (length rows)
                (loop for record in rows
                      for index from 1
                      collect (format nil "  ~d. #~a ~a (~a, ~a) -> ~a"
                                      index (getf record :id) (getf record :label)
                                      (score-text record) (stage-of record)
                                      (or (getf (contact-route record (now)) :next)
                                          "no open route")))))))

(defun gate-text (record &optional (now (now)))
  "The gate's answer as a founder reads it."
  (let ((gate (gate-record record)))
    (format nil "~a: ~:[may not be drafted to~;may be drafted to~]~@[ - ~a~] (~a)~%~@[  blocked: ~{~a~^~%~}~%~]~@[  warnings: ~{~a~^~%~}~%~]route:~%~a"
            (getf record :label)
            (getf gate :ok)
            (getf gate :channel)
            (getf gate :mode)
            (getf gate :reasons)
            (getf gate :warnings)
            (route-text (getf gate :route)))))

(defun card-text (record)
  "One investor in full."
  (format nil "#~a ~a~%  stage: ~a   score: ~a~@[   ranked ~a~]~@[~%  score by dimension:~%~{    ~a~^~%~}~]~%  email: ~@[~a~]~:[~; (confirmed)~]~%  firm inbox: ~@[~a~]  form: ~@[~a~]~%  evidence:~%~{~a~^~%~}~%  log:~%~{~a~^~%~}~%~a"
          (getf record :id)
          (getf record :label)
          (stage-of record)
          (score-text record)
          (getf record :ranked_at)
          (loop for dim in (getf record :dims)
                for score = (getf dim :score)
                collect (format nil " ~a: ~a (weight ~a)" (getf dim :name)
                                (if (numberp score) (format nil "~,0f" (float score 1.0)) "dropped")
                                (getf dim :weight)))
          (let ((email (getf record :email))) (and email (plusp (length email)) email))
          (getf record :email_confirmed)
          (let ((email (getf record :firm_email))) (and email (plusp (length email)) email))
          (let ((form (getf record :firm_form))) (and form (plusp (length form)) form))
          (if (null (evidence-of record))
              (list "  none: raise_evidence records the facts a ranking or a draft must cite")
              (loop for item in (evidence-of record)
                    collect (format nil "  #~a ~a: ~a~@[ (~a)~]~@[ stage ~a~]~@[ ~a~]"
                                    (getf item :id) (getf item :kind) (getf item :claim)
                                    (getf item :source_url) (getf item :stage) (getf item :date))))
          (if (null (events-of record))
              (list "  nothing recorded yet")
              (loop for event in (events-of record)
                    collect (format nil "  ~a ~a~@[ on ~a~]~@[ ~a~]"
                                    (getf event :at) (getf event :kind)
                                    (getf event :channel) (getf event :summary))))
          (gate-text record)))

(defun findings-text (findings)
  (format nil "~{~a~^~%~}"
          (loop for item in findings
                collect (format nil "  [~a] ~a" (getf item :rule) (getf item :message)))))

(defun lint-text (text record channel max-words)
  "The lint's answer: what blocks the draft, then what to fix before sending it."
  (let* ((now (now))
         (result (lint-draft-text text
                                  :channel (or channel "email")
                                  :verified (mapcar (lambda (claim) (getf claim :text)) (verified-claims))
                                  :facts (when record
                                           (loop for item in (evidence-of record)
                                                 collect (format nil "~a ~a"
                                                                 (or (getf item :claim) "")
                                                                 (or (getf item :date) ""))))
                                  :round-amount (when (and record (getf (profile) :round_amount))
                                                  (getf (profile) :round_amount))
                                  :max-words max-words)))
    (declare (ignore now))
    (format nil "~:[BLOCKED by the code checks~;passes the code checks~]~%~@[~a~%~]~@[warnings:~%~a~]"
            (getf result :ok)
            (let ((hard (getf result :hard))) (and hard (findings-text hard)))
            (let ((warnings (getf result :warnings))) (and warnings (findings-text warnings))))))

(defun draft-facts (record)
  (loop for item in (evidence-of record) collect (getf item :claim)))

(defun awaiting ()
  (sort (remove-if-not #'awaiting-reply-p
                        (remove-if (lambda (r) (or (getf r :archived) (getf r :excluded))) (investors)))
        #'string< :key (lambda (r) (getf r :label))))

(defun unranked ()
  (remove-if (lambda (r) (or (getf r :score) (getf r :archived) (getf r :excluded)))
             (investors)))

(defun brief-text (&optional n)
  "At most N actions today: replies first, then research, ranking, and new outreach."
  (let ((actions '())
        (limit (min 5 (or n 5))))
    (dolist (record (awaiting))
      (when (< (length actions) limit)
        (push (format nil "Answer ~a: they wrote last." (getf record :label)) actions)))
    (dolist (record (unranked))
      (when (< (length actions) limit)
        (push (format nil "Rank ~a~:[~; and record the facts first~]: raise_evidence then raise_rank."
                      (getf record :label) (null (evidence-of record)))
              actions)))
    (dolist (record (focus-list limit nil))
      (when (< (length actions) limit)
        (let ((gate (gate-record record)))
          (when (getf gate :ok)
            (push (format nil "Draft to ~a on ~a (~a), then raise_lint it."
                          (getf record :label) (getf gate :channel) (getf gate :mode))
                  actions)))))
    (if (null actions)
        (format nil "nothing pressing: ~a" (pipeline-report))
        (format nil "~d action~:p for today, in order:~%~{~a~}"
                (length actions)
                (loop for action in (reverse actions) for index from 1
                      collect (format nil "  ~d. ~a" index action))))))

(defparameter +export-fields+
  '("id" "name" "firm" "stage" "score" "email" "email_confirmed" "linkedin"
    "connected" "do_not_contact" "evidence" "last_event" "last_summary")
  "What a row of the export carries: the pipeline, not the conversation.")

(defun csv-cell (value)
  (let ((text (princ-to-string (or value ""))))
    (if (find-if (lambda (char) (member char '(#\, #\" #\Newline))) text)
        (format nil "\"~a\"" (ppcre:regex-replace-all "\"" text "\"\""))
        text)))

(defun export-rows ()
  (loop for record in (investors)
        for events = (events-of record)
        for last = (car (last events))
        collect (list :id (getf record :id)
                      :name (getf record :name)
                      :firm (getf record :firm)
                      :stage (stage-of record)
                      :score (getf record :score)
                      :email (getf record :email)
                      :email_confirmed (and (getf record :email_confirmed) t)
                      :linkedin (getf record :linkedin)
                      :connected (and (getf record :connected) t)
                      :do_not_contact (and (getf record :do_not_contact) t)
                      :evidence (length (evidence-of record))
                      :last_event (and last (getf last :at))
                      :last_summary (and last (getf last :summary)))))

(defun export-text (&optional (format "rows"))
  "The pipeline as rows, or as CSV text."
  (if (equal format "csv")
      (with-output-to-string (out)
        (write-line (format nil "~{~a~^,~}" +export-fields+) out)
        (dolist (row (export-rows))
          (write-line (format nil "~{~a~^,~}"
                               (loop for key in +export-fields+
                                     collect (csv-cell (getf row (intern (string-upcase key) :keyword)))))
                      out)))
      (format nil "~d rows~%~{~a~^~%~}"
              (length (export-rows))
              (loop for row in (export-rows)
                    collect (format nil "#~a ~a at ~a: ~a (~a) - ~a evidence, last ~a"
                                    (getf row :id) (getf row :name) (getf row :firm)
                                    (getf row :stage) (or (getf row :score) "unranked")
                                    (getf row :evidence) (or (getf row :last_event) "never"))))))

;;; --- the verbs ------------------------------------------------------------------

(define-verb status-text ()
  "The pipeline in numbers, and where it is written."
  (with-pipeline (:save nil) (pipeline-report)))

(define-verb focus (&optional n min-score)
  "The ranked investors worth working now, best first."
  (with-pipeline (:save nil) (focus-text n min-score)))

(define-verb can-draft (ref)
  "Whether Raise may draft to REF now, and on which route."
  (with-pipeline (:save nil) (gate-text (resolve-investor ref))))

(define-verb lint-draft (text &key investor channel max-words)
  "The code checks on a drafted TEXT, against the founder's verified claims and
the cited facts of the investor it is for."
  (with-pipeline (:save nil)
    (lint-text text (and investor (resolve-investor investor)) channel max-words)))

(define-verb brief (&optional n)
  "At most five actions today, replies first."
  (with-pipeline (:save nil) (brief-text n)))

;;; --- the tools ------------------------------------------------------------------

(defun tool-profile (call)
  (with-pipeline ()
    (let ((fields (profile-call-fields call)))
      (when (hash-table-p fields)
        (loop for key being the hash-keys of fields using (hash-value value)
              do (set-profile (list (intern (string-upcase (substitute (code-char 95) (code-char 45) key))
                                           :keyword)
                                    value))))
      (when (profile-call-confirm call)
        (setf (getf *pipeline* :profile_confirmed_at) (nlk:iso-now)))
      (profile-text))))

(defun tool-claim (call)
  (with-pipeline ()
    (let ((claim (add-claim (claim-call-text call) (claim-call-tag call)
                            :source (claim-call-source call))))
      (format nil "claim #~a recorded as ~a~@[ (source: ~a)~]. Drafts may state its numbers."
              (getf claim :id) (getf claim :tag) (getf claim :source)))))

(defun tool-investor (call)
  (with-pipeline ()
    (let* ((name (or (investor-call-name call) ""))
           (firm (or (investor-call-firm call) ""))
           (existing (or (and (plusp (length (name-key name)))
                              (find-if (lambda (record)
                                         (and (string= (name-key (getf record :name)) (name-key name))
                                              (string= (name-key (getf record :firm)) (name-key firm))))
                                       (investors)))
                         (find name (investors) :test #'name= :key (lambda (r) (getf r :label)))))
           (record (or existing (new-investor :name name :firm firm))))
      (macrolet ((set-member (key reader)
                   `(let ((value (,reader call)))
                      (when value (setf (getf record ,key) value)))))
        (set-member :kind investor-call-kind)
        (set-member :email investor-call-email)
        (set-member :linkedin investor-call-linkedin)
        (set-member :stage investor-call-stage)
        (set-member :email_confirmed investor-call-email-confirmed)
        (set-member :connected investor-call-connected))
      (when existing
        (when (listp (investor-call-email-confirmed call))
          (setf (getf record :email_confirmed) nil)))
      (setf (getf record :label) (investor-label record))
      (set-investor record)
      (format nil "~:[added~;updated~] #~a ~a (~a, ~a)"
              existing (getf record :id) (getf record :label)
              (getf record :firm_kind) (stage-of record)))))

(defun csv-lines (text)
  "TEXT's non-blank lines, each split on commas."
  (loop for line in (ppcre:split "[\\n\\r]" (or text ""))
        when (plusp (length (nlk:trimmed line)))
          collect (mapcar (lambda (cell) (nlk:trimmed (string-trim "\"" cell)))
                          (ppcre:split "," line))))

(defun column-value (row names)
  (loop for name in names
        for index = (position name (car row) :test #'string-equal)
        when index return (nth index (cdr row))))

(defun import-rows (rows)
  "ROWS as (NAME FIRM EMAIL LINKEDIN STAGE) lists from CSV or objects."
  (cond
    ((null rows) '())
    ((vectorp rows)
     (loop for row across rows
           when (hash-table-p row)
             collect (list (nlk:json-value row :string "name")
                           (nlk:json-value row :string "firm")
                           (nlk:json-value row :string "email")
                           (nlk:json-value row :string "linkedin")
                           (nlk:json-value row :string "stage"))))
    (t (let ((lines (mapcar (lambda (line) (cons (mapcar #'string-downcase (car line)) line))
                            (csv-lines rows))))
         (loop for line in (cdr lines)
               collect (list (column-value line '("name" "person" "investor" "full name"))
                             (column-value line '("firm" "company" "fund" "organization"))
                             (column-value line '("email" "email address"))
                             (column-value line '("linkedin" "linkedin url" "url"))
                             (column-value line '("stage"))))))))

(defun tool-import (call)
  (with-pipeline ()
    (let* ((rows (append (import-rows (import-call-rows call))
                         (import-rows (import-call-csv call))))
           (added 0)
           (updated 0))
      (when (null rows)
        (fail "nothing to import: give rows, or a csv with a header line naming name, firm, email, linkedin, stage"))
      (dolist (row rows)
        (destructuring-bind (name firm email linkedin stage) row
          (when (and name (plusp (length (name-key name))))
            (let* ((existing (find-if (lambda (record)
                                        (and (string= (name-key (getf record :name)) (name-key name))
                                             (string= (name-key (getf record :firm)) (name-key (or firm "")))))
                                      (investors)))
                   (record (or existing (new-investor :name name :firm firm))))
              (when email (setf (getf record :email) email))
              (when linkedin (setf (getf record :linkedin) linkedin))
              (when (and stage (member stage +open-stages+ :test #'string-equal))
                (setf (getf record :stage) (string-downcase stage)))
              (setf (getf record :label) (investor-label record))
              (set-investor record)
              (if existing (incf updated) (incf added))))))
      (format nil "imported: ~d added, ~d updated, ~d investors in the pipeline"
              added updated (length (investors))))))

(defun tool-evidence (call)
  (with-pipeline ()
    (let* ((record (resolve-investor (evidence-call-investor call)))
           (item (add-evidence record
                                :kind (evidence-call-kind call)
                                :claim (evidence-call-claim call)
                                :source-url (evidence-call-source-url call)
                                :date (evidence-call-date call)
                                :amount (evidence-call-amount call)
                                :stage (evidence-call-stage call))))
      (format nil "evidence #~a on ~a: ~a (~a). Cite it in raise_rank and drafts may use its numbers."
              (getf item :id) (getf record :label) (getf item :claim) (getf item :source_url)))))

(defun tool-show (call)
  (with-pipeline (:save nil) (card-text (resolve-investor (show-call-investor call)))))

(defun tool-rank (call)
  (with-pipeline ()
    (let* ((record (resolve-investor (rank-call-investor call)))
           (result (rank-record record (rank-call-dims call) (now))))
      (format nil "~a scores ~,1f~%~{~a~^~%~}"
              (getf record :label) (getf result :total)
              (loop for dim in (getf result :dims)
                    collect (format nil "  ~a: ~a (weight ~a)~@[ cite ~{~a~^,~}~]~@[ - ~a~]"
                                    (getf dim :name)
                                    (let ((score (getf dim :score)))
                                      (if (numberp score) (format nil "~,0f" (float score 1.0)) "dropped"))
                                    (getf dim :weight) (getf dim :evidence)
                                    (let ((note (getf dim :note)))
                                      (and note (plusp (length note)) note))))))))

(defun tool-focus (call)
  (focus (focus-call-n call) (focus-call-min-score call)))

(defun tool-can-draft (call)
  (can-draft (can-draft-call-investor call)))

(defun tool-lint (call)
  (lint-draft (lint-call-text call)
              :investor (lint-call-investor call)
              :channel (lint-call-channel call)
              :max-words (lint-call-max-words call)))

(defun tool-log (call)
  (with-pipeline ()
    (let* ((record (resolve-investor (log-call-investor call)))
           (event (add-event record :kind (log-call-kind call)
                                    :channel (log-call-channel call)
                                    :direction (log-call-direction call)
                                    :summary (log-call-summary call))))
      (format nil "logged ~a with ~a~@[ on ~a~] - stage now ~a"
              (getf event :kind) (getf record :label) (getf event :channel)
              (stage-of record)))))

(defun tool-brief (call)
  (brief (brief-call-n call)))

(defun tool-export (call)
  (with-pipeline (:save nil) (export-text (export-call-form call))))

;;; --- /raise -------------------------------------------------------------------

(defun panel ()
  "What bare /raise puts beside the pipeline: one tap to the desk's next moves."
  (nle:list-dialog
   :title "Raise" :summary "Your fundraising desk"
   :rows (list (nle:run-row "What to do today" "/raise next"
                             :detail "at most five actions, replies first")
               (nle:run-row "The ones worth working" "/raise focus"
                             :detail "ranked, not passed, one person a firm")
               (nle:ask-row "See one investor" "Investor" "/raise show"
                            :placeholder "a name, or an id" :verb "show"
                            :detail "score, evidence, log and the route")
               (nle:run-row "Export the pipeline as CSV" "/raise export csv"
                             :detail "for your own CRM or a spreadsheet"))))

(defun run-slash (args session-id)
  "/raise typed alone: the pipeline in numbers and the desk's panel. NAME shows
one investor; status, next, focus, can, show and export are the rest."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position (code-char 32) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond
      ((string= text "") (values (status-text) (panel)))
      ((member head '("status" "st") :test #'equal) (status-text))
      ((member head '("next" "brief") :test #'equal) (brief nil))
      ((string= head "focus")
       (focus (and (plusp (length rest)) (parse-integer rest :junk-allowed t)) nil))
      ((string= head "can") (can-draft rest))
      ((string= head "show")
       (with-pipeline (:save nil) (card-text (resolve-investor rest))))
      ((string= head "export")
       (with-pipeline (:save nil) (export-text (if (string= rest "csv") "csv" "rows"))))
      (t (with-pipeline (:save nil) (card-text (resolve-investor text)))))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell raise
  (:section ("raise")
    (:guide "nothing here is required: focus_size caps raise_focus, hold_workdays and cap_per_channel are the outreach rule of three, email_mode says where a draft is meant to land (Raise never sends)")
    ("focus_size" :integer :default 10 :min 1
     :doc "how many investors raise_focus lists")
    ("hold_workdays" :integer :default 3 :min 1
     :doc "working days a pending ask holds every later route")
    ("cap_per_channel" :integer :default 3 :min 1
     :doc "touches on one channel before the route moves on")
    ("cap_window_days" :integer :default 180 :min 1
     :doc "the window those touches are counted in")
    ("email_mode" :choice :options '("none" "gmail" "other") :default "none"
     :doc "where a draft is meant to land; Raise only ever drafts")
    ("draft_style" :list
     :doc "skills a draft must follow, by name: voice, humanizer"))
  (:tool "raise_profile" (nlk:contract 'profile-call) 'tool-profile)
  (:tool "raise_claim" (nlk:contract 'claim-call) 'tool-claim)
  (:tool "raise_investor" (nlk:contract 'investor-call) 'tool-investor)
  (:tool "raise_import" (nlk:contract 'import-call) 'tool-import)
  (:tool "raise_evidence" (nlk:contract 'evidence-call) 'tool-evidence)
  (:tool "raise_show" (nlk:contract 'show-call) 'tool-show)
  (:tool "raise_rank" (nlk:contract 'rank-call) 'tool-rank)
  (:tool "raise_focus" (nlk:contract 'focus-call) 'tool-focus)
  (:tool "raise_can_draft" (nlk:contract 'can-draft-call) 'tool-can-draft)
  (:tool "raise_lint" (nlk:contract 'lint-call) 'tool-lint)
  (:tool "raise_log" (nlk:contract 'log-call) 'tool-log)
  (:tool "raise_brief" (nlk:contract 'brief-call) 'tool-brief)
  (:tool "raise_export" (nlk:contract 'export-call) 'tool-export)
  (:help :raise "raise: a draft-only fundraising desk - rank investors on cited evidence, keep the route inside its caps, lint every draft; raise_focus, raise_brief, raise_lint, /raise" +primer+)
  (:command "raise" 'run-slash
            :catches 'raise-error
            :description "The fundraising desk: status, next, focus, show NAME, can NAME, export"))
