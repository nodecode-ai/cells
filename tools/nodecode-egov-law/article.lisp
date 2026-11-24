;;;; article.lisp --- one article's wording, and the law's amendment dates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; e-Gov answers law_data with the law's own XML as JSON: a tree of tags.
;;;; What a person reads is the sentence, the paragraph number and the item,
;;;; so the tree is flattened here once, the way the original plugin did, and
;;;; a table becomes one line a row with ｜ between cells.

(in-package #:nodecode-egov-law)

;;; --- the law XML as text ---------------------------------------------------------

(defun node-tag (node)
  "NODE's XML tag name, or NIL for a string leaf."
  (and (hash-table-p node) (field node "tag")))

(defun node-children (node)
  "NODE's children as a list."
  (let ((children (parts node "children")))
    (if children (coerce children 'list) '())))

(defun text-of (node)
  "NODE as plain text, with ruby readings dropped and a string trimmed."
  (cond ((stringp node) (string-trim " 　" node))
        ((hash-table-p node)
         (unless (equal (node-tag node) "Rt")
           (with-output-to-string (out)
             (dolist (child (node-children node)) (princ (text-of child) out)))))
        (t "")))

(defun sentence-text (node)
  "NODE's sentence, its columns separated by a full-width space."
  (with-output-to-string (out)
    (let ((first t))
      (dolist (child (node-children node))
        (if (equal (node-tag child) "Column")
            (progn (unless first (princ "　" out)) (princ (text-of child) out))
            (princ (text-of child) out))
        (setf first nil)))))

(defun title-tag-p (tag)
  "Whether TAG names a block's own number or heading."
  (or (member tag '("ParagraphNum" "ItemTitle") :test #'string=)
      (and tag (ppcre:scan "^Subitem[0-9]+Title$" tag))))

(defun sentence-tag-p (tag)
  "Whether TAG names a block's sentence."
  (or (member tag '("ParagraphSentence" "ItemSentence") :test #'string=)
      (and tag (ppcre:scan "^Subitem[0-9]+Sentence$" tag))))

(defun block-tag-p (tag)
  "Whether TAG names a block that nests under another."
  (or (member tag '("Paragraph" "Item") :test #'string=)
      (and tag (ppcre:scan "^Subitem[0-9]+$" tag))))

(defun other-lines (node)
  "NODE as lines: a table one line a row with ｜ between cells, else its text."
  (if (equal (node-tag node) "TableStruct")
      (let ((rows '()))
        (dolist (table (node-children node))
          (dolist (row (node-children table))
            (when (equal (node-tag row) "TableRow")
              (push (format nil "~{~a~^｜~}" (mapcar #'text-of (node-children row))) rows))))
        (or (nreverse rows) (list (text-of node))))
      (let ((text (text-of node)))
        (if (plusp (length text)) (list text) '()))))

(defun render-block (node depth)
  "NODE at DEPTH as a list of lines: its own heading and sentence, then its
items, each one level in."
  (let ((title "") (sentence "") (rest '()))
    (dolist (child (node-children node))
      (cond ((title-tag-p (node-tag child)) (setf title (text-of child)))
            ((sentence-tag-p (node-tag child)) (setf sentence (sentence-text child)))
            ((hash-table-p child) (push child rest))))
    (when (and (equal (node-tag node) "Paragraph") (zerop (length title)))
      (let ((number (or (field node "attr" "Num") "")))
        (setf title (if (member number '("" "1") :test #'string=) "" (fullwidth number)))))
    (let ((indent (make-string depth :initial-element #\　))
          (tail '()))
      (dolist (child (nreverse rest))
        (if (block-tag-p (node-tag child))
            (setf tail (append tail (render-block child (1+ depth))))
            (dolist (line (other-lines child))
              (setf tail (append tail (list (format nil "~a　~a" indent line)))))))
      (cons (format nil "~a~@[~a　~]~a" indent (and (plusp (length title)) title) sentence)
            tail))))
(defun render-suppl (node)
  "A whole 附則 as its label, then each article or paragraph it holds."
  (let ((lines '()) (paragraphs 0))
    (labels ((walk (parent)
               (dolist (child (node-children parent))
                 (let ((tag (node-tag child)))
                   (cond ((equal tag "SupplProvisionLabel") (push (text-of child) lines))
                         ((equal tag "Article")
                          (let ((rendered (render-article child)))
                            (push (getf rendered :text) lines)
                            (incf paragraphs (getf rendered :paragraphs))))
                         ((equal tag "Paragraph") (incf paragraphs) (setf lines (append (render-block child 0) (nreverse lines))))
                         ((and (stringp tag) (ppcre:scan "Title$" tag)
                               (not (equal tag "ArticleTitle")))
                          (push (text-of child) lines))
                         ((hash-table-p child) (walk child)))))))
      (walk node)
      (list :title "" :caption ""
            :text (string-trim '(#\Newline #\Space #\　)
                               (format nil "~{~a~^~%~}" (remove "" (nreverse lines))))
            :paragraphs paragraphs))))

(defun render-article (node)
  "NODE as (LIST :TITLE :CAPTION :TEXT :PARAGRAPHS), one article, one
paragraph, a whole 附則, or a list of them."
  (cond ((vectorp node)
         (let ((rendered (loop for child across node
                               when (hash-table-p child) collect (render-article child))))
           (list :title (format nil "~{~a~^ / ~}" (remove nil (mapcar (lambda (r) (getf r :title)) rendered)))
                 :caption (format nil "~{~a~^ / ~}" (remove nil (mapcar (lambda (r) (getf r :caption)) rendered)))
                 :text (format nil "~{~a~^~%~}" (mapcar (lambda (r) (getf r :text)) rendered))
                 :paragraphs (reduce #'+ rendered :key (lambda (r) (getf r :paragraphs)) :initial-value 0))))
        ((equal (node-tag node) "SupplProvision") (render-suppl node))
        ((equal (node-tag node) "Paragraph")
         (let ((lines (render-block node 0)))
           (list :title "" :caption ""
                 :text (string-trim '(#\Newline #\Space #\　)
                                    (format nil "~{~a~^~%~}" (nreverse lines)))
                 :paragraphs 1)))
        (t (let ((caption "") (title "") (lines '()) (paragraphs 0))
             (dolist (child (node-children node))
               (let ((tag (node-tag child)))
                 (cond ((equal tag "ArticleCaption") (setf caption (text-of child)))
                       ((equal tag "ArticleTitle") (setf title (text-of child)))
                       ((equal tag "Paragraph") (incf paragraphs) (setf lines (append (render-block child 0) (nreverse lines))))
                       ((hash-table-p child) (dolist (line (other-lines child)) (push line lines))))))
             (setf lines (nreverse lines))
             (cond ((and lines (plusp (length title)))
                    (setf (first lines) (format nil "~a　~a" title (string-left-trim "　" (first lines)))))
                   ((plusp (length title)) (push title lines)))
             (when (plusp (length caption)) (push caption lines))
             (list :title title :caption caption
                   :text (string-trim '(#\Newline #\Space #\　) (format nil "~{~a~^~%~}" lines))
                   :paragraphs paragraphs)))))

(defun trim-text (text limit)
  "TEXT cut at LIMIT characters — at a line break when one falls in the second
half of the budget — and whether it was cut."
  (if (<= (length text) limit)
      (values text nil)
      (let* ((head (subseq text 0 limit))
             (break (position #\Newline head :from-end t))
             (cut (if (and break (> break (floor limit 2))) (subseq head 0 break) head)))
        (values cut t))))

;;; --- validation -------------------------------------------------------------------

(defun checked-date (value name)
  "VALUE as YYYY-MM-DD, or a refusal; NIL passes through."
  (when (and value (plusp (length value)))
    (unless (ppcre:scan "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" value)
      (law-fail "bad_argument" "~a must be YYYY-MM-DD, not ~a" name value))
    (when (string< value "2017-04-01")
      (law-fail "bad_argument"
                "~a must be 2017-04-01 or later: e-Gov keeps versions from that day" name))
    value))

(defun law-data (key element &key as-of)
  "The law XML for ELEMENT of the law KEY, on AS-OF when given."
  (handler-case
      (get-json (format nil "law_data/~a" key)
                :params (list (cons "elm" element)
                              (cons "json_format" "full")
                              (cons "asof" as-of)
                              (cons "omit_amendment_suppl_provision" "true")))
    (egov-law-error (condition)
      (if (member *error-kind* '("not_found" "bad_request") :test #'string=)
          (law-fail (if (string= *error-kind* "not_found") "law_not_found" "article_not_found")
                    "e-Gov has no text for ~a~@[ as of ~a~]; check the law and article with egov_law_search"
                    element as-of)
          (error condition)))))

;;; --- the article ----------------------------------------------------------------

(defun article (law article &key paragraph (as-of nil) compare-with (max-chars nil))
  "Read ARTICLE of LAW => a JSON-ready object with its wording and source."
  (multiple-value-bind (element label) (article-element article paragraph)
    (setf max-chars (max 300 (min 20000 (or max-chars (setting :max-chars)))))
    (setf as-of (checked-date as-of "as_of") compare-with (checked-date compare-with "compare_with"))
    (multiple-value-bind (key how title) (resolve-law law)
      (let* ((data (law-data key element :as-of as-of))
             (full-text (nlk:json-value data :any "law_full_text"))
             (info (part data "law_info"))
             (revision (part data "revision_info"))
             (rendered (and full-text (render-article full-text))))
        (unless (and rendered (plusp (length (getf rendered :text))))
          (law-fail "bad_response"
                    "e-Gov returned no text for ~a of ~a; open the law's page to read it" label (or title law)))
        (multiple-value-bind (text truncated) (trim-text (getf rendered :text) max-chars)
          (let* ((law-title (or (field revision "law_title") title))
                 (law-id (field info "law_id"))
                 (page (and law-id (law-page law-id))))
            (object "law_title" law-title
                    "law_num" (field info "law_num")
                    "law_id" law-id
                    "article" (if (plusp (length (getf rendered :title))) (getf rendered :title) label)
                    "requested" label
                    "caption" (getf rendered :caption)
                    "text" text
                    "paragraphs" (getf rendered :paragraphs)
                    "in_force_since" (or (field revision "amendment_enforcement_date")
                                          (field revision "amendment_scheduled_enforcement_date"))
                    "status" (field revision "current_revision_status")
                    "last_amended_by" (field revision "amendment_law_title")
                    "as_of" (or as-of (today-jst))
                    "resolved_by" how
                    "truncated" (and truncated t)
                    "total_chars" (and truncated (length (getf rendered :text)))
                    "url" page
                    "source" (object "attribution" +attribution+
                                     "url" page "as_of" (or as-of (today-jst))
                                     "retrieved_at" (today-jst))
                    "next_step" (if truncated
                                     (format nil "text cut at ~d of ~d characters: call again with max_chars=~d, or one paragraph at a time"
                                             max-chars (length (getf rendered :text))
                                             (min 20000 (length (getf rendered :text))))
                                     nil)
                    "comparison" (and compare-with
                                       (compare-article key element (getf rendered :text)
                                                        compare-with max-chars)))))))))

(defun compare-article (key element text other limit)
  "Whether ELEMENT of KEY reads differently on OTHER, and how: the text there
and the lines that differ."
  (let* ((data (handler-case (law-data key element :as-of other)
                  (egov-law-error (condition)
                    (declare (ignore condition))
                    nil)))
         (other-node (and data (nlk:json-value data :any "law_full_text")))
         (other-text (and other-node (getf (render-article other-node) :text))))
    (when (null other-text)
      (law-fail "article_not_found"
                "~a does not exist on ~a; read it on another date, or without compare_with" element other))
    (let* ((here (ppcre:split "\n" text))
           (there (ppcre:split "\n" other-text))
           (diff (append (loop for line in here unless (member line there :test #'string=)
                               collect (format nil "-~a" line))
                         (loop for line in there unless (member line here :test #'string=)
                               collect (format nil "+~a" line)))))
      (multiple-value-bind (cut truncated) (trim-text other-text limit)
        (object "on" other
                "changed" (and (not (string= text other-text)) t)
                "diff" (and diff (coerce diff 'vector))
                "text" cut
                "truncated" (and truncated t))))))

;;; --- revisions -------------------------------------------------------------------

(defparameter +status-labels+
  '(("CurrentEnforced" . "in force now")
    ("PreviousEnforced" . "superseded (older version)")
    ("UnEnforced" . "not yet in force (scheduled)")
    ("Repeal" . "repealed")))

(defun status-label (raw)
  "e-Gov's status RAW as a phrase."
  (or (cdr (assoc raw +status-labels+ :test #'string=)) raw))

(defun enforcement-date (revision)
  "When REVISION takes effect; e-Gov sometimes leaves the first date empty."
  (or (field revision "amendment_enforcement_date")
      (field revision "amendment_scheduled_enforcement_date")))

(defun amended-by (revision)
  "The amending law of REVISION, title and number, or NIL."
  (let ((title (field revision "amendment_law_title"))
        (number (field revision "amendment_law_num")))
    (cond ((and title number) (format nil "~a（~a）" title number))
          (title title)
          (number number))))

(defun revision-row (revision)
  "REVISION as the answer lists it: when, by what, and the two dates that
read the article before and after it."
  (let ((date (enforcement-date revision)))
    (object "in_force_since" date
            "status" (status-label (field revision "current_revision_status"))
            "amended_by" (amended-by revision)
            "repeal_status" (field revision "repeal_status")
            "amendment_promulgated" (field revision "amendment_promulgate_date")
            "enforcement_note" (field revision "amendment_enforcement_comment")
            "law_revision_id" (field revision "law_revision_id")
            "check" (and date (object "as_of" (day-before date) "compare_with" date)))))

(defun day-before (date)
  "DATE — YYYY-MM-DD — minus one day."
  (let ((stamp (encode-universal-time 0 0 0
                                      (parse-integer date :start 8 :end 10)
                                      (parse-integer date :start 5 :end 7)
                                      (parse-integer date :start 0 :end 4)
                                      0)))
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time (- stamp 86400) 0)
      (declare (ignore second minute hour))
      (format nil "~4,'0d-~2,'0d-~2,'0d" year month day))))

(defun revisions (law &key (limit 10))
  "The amendment history of LAW => a JSON-ready object: what is scheduled,
what is in force now, and earlier versions."
  (setf limit (max 1 (min 50 (or limit 10))))
  (multiple-value-bind (key how title) (resolve-law law)
    (declare (ignore how))
    (let* ((data (handler-case (get-json (format nil "law_revisions/~a" key))
                   (egov-law-error (condition)
                     (if (string= *error-kind* "not_found")
                         (law-fail "law_not_found" "e-Gov has no revision history for ~a" law)
                         (error condition)))))
           (info (part data "law_info"))
           (rows (parts data "revisions"))
           (today (today-jst))
           (law-id (field info "law_id"))
           (page (and law-id (law-page law-id))))
      (unless rows
        (law-fail "bad_response" "e-Gov returned no revision list for this law; open the law's page"))
      (let* ((entries (loop for revision across rows
                             for row = (revision-row revision)
                             for date = (field revision "amendment_enforcement_date")
                             for scheduled = (field revision "amendment_scheduled_enforcement_date")
                             collect (list :row row
                                           :current (string= (field revision "current_revision_status") "CurrentEnforced")
                                           :date (or date scheduled)
                                           :future (or (string= (field revision "current_revision_status") "UnEnforced")
                                                       (and (or date scheduled) (string< today (or date scheduled))))
                                           :repeal (string= (field revision "repeal_status") "Repeal"))))
             (upcoming (sort (loop for entry in entries when (getf entry :future) collect entry)
                             #'string< :key (lambda (entry) (or (getf entry :date) "9999"))))
             (in-force (loop for entry in entries
                             when (and (not (getf entry :future))
                                       (getf entry :current))
                               return entry))
             (earlier (loop for entry in entries
                            when (and (not (getf entry :future)) (not (eq entry in-force)))
                              collect entry)))
        (object "law_title" title
                "law_num" (field info "law_num")
                "law_id" law-id
                "promulgation_date" (field info "promulgation_date")
                "next_amendment" (and upcoming (getf (first upcoming) :row))
                "scheduled_repeal" (loop for entry in upcoming when (getf entry :repeal)
                                          return (getf entry :row))
                "scheduled_amendments" (and upcoming
                                             (coerce (loop for entry in (subseq upcoming 0 (min limit (length upcoming)))
                                                           collect (getf entry :row))
                                                     'vector))
                "in_force_now" (and in-force (getf in-force :row))
                "earlier_versions" (coerce (loop for entry in (subseq earlier 0 (min limit (length earlier)))
                                                  collect (getf entry :row))
                                            'vector)
                "total_versions" (length entries)
                "url" page
                "source" (object "attribution" +attribution+ "url" page
                                 "retrieved_at" (today-jst))
                "note" "scheduled_amendments: soonest first, not yet in force. To see whether one changes an article, call egov_law_article with that row's check pair (as_of = the day before, compare_with = the day). Bills still in the Diet are not included.")))))

;;; --- the three questions a model asks --------------------------------------------

(defmacro answering (&body body)
  "BODY's object as JSON, or the refusal as JSON: a tool never signals."
  `(let ((*error-kind* nil))
     (handler-case (nlk:encode-json-object (progn ,@body))
       (egov-law-error (condition)
         (nlk:encode-json-object
          (object "error" (egov-law-error-detail condition)
                  "error_type" (or *error-kind* "internal")
                  "next_step" (case (intern (string-upcase (or *error-kind* "")) :keyword)
                                ((:law_not_found :ambiguous_law)
                                 "check the law with egov_law_search, then call this tool again with its law ID")
                                ((:not_found :bad_request :upstream) "e-Gov refused or failed: wait a minute and call again once")
                                ((:network) "check that this machine can reach https://laws.e-gov.go.jp and call again")
                                (t nil)))))
       (error (condition)
         (nlk:encode-json-object
          (object "error" (format nil "the egov-law cell failed: ~a" condition)
                  "error_type" "internal"
                  "next_step" "retry once; if it repeats, give the user the law's e-Gov page instead"))))))

(defun egov_law_search (arguments)
  "egov_law_search: a law by name, abbreviation, law number or ID; or the
laws whose text contains a phrase."
  (answering
    (search-laws (or (field arguments "query") (field arguments "law_name"))
            :mode (or (field arguments "mode") "title")
            :limit (nlk:json-value arguments :integer "limit")
            :include-repealed (and (eq t (nlk:json-value arguments :any "include_repealed")) t))))

(defun egov_law_article (arguments)
  "egov_law_article: one article's wording, today or on a date, and whether it
reads differently on another date."
  (answering
    (article (or (field arguments "law") (field arguments "law_id"))
             (or (field arguments "article") (field arguments "article_number"))
             :paragraph (nlk:json-value arguments :integer "paragraph")
             :as-of (field arguments "as_of")
             :compare-with (field arguments "compare_with")
             :max-chars (nlk:json-value arguments :integer "max_chars"))))

(defun egov_law_revisions (arguments)
  "egov_law_revisions: when this law's amendments took effect and when the
next one does."
  (answering
    (revisions (or (field arguments "law") (field arguments "law_id"))
               :limit (nlk:json-value arguments :integer "limit"))))
