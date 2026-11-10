;;;; laws.lisp --- resolve a law, read one article, list its amendment dates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The three things a person asks a law database, over e-Gov Law API v2:
;;;; find a statute by the name a person uses, read one article's wording in
;;;; force today or on a date, and see when the law's amendments take effect.
;;;; The API's own element names are built here (Article_32_2), its law XML
;;;; is flattened to text, and every answer carries the credit e-Gov asks for.

(in-package #:nodecode-egov-law)

;;; --- a JSON object out of pairs -----------------------------------------------

(defun object (&rest pairs)
  "A JSON object of PAIRS, key value ..., with every NIL member left out."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          when value do (setf (gethash key table) value))
    table))

(defun field (object &rest path)
  "PATH's string in OBJECT, or NIL when it is absent or another type."
  (apply #'nlk:json-value object :text path))

(defun part (object &rest path)
  "PATH's object in OBJECT, or NIL."
  (apply #'nlk:json-value object :object path))

(defun parts (object &rest path)
  "PATH's array in OBJECT, or NIL."
  (apply #'nlk:json-value object :array path))

;;; --- Japanese numbers and names -----------------------------------------------

(defparameter +kanji-digits+ "〇零一二三四五六七八九"
  "The digits as Japanese law text writes them; 零 and 〇 are both zero.")

(defparameter +kanji-units+ '((#\十 . 10) (#\百 . 100) (#\千 . 1000))
  "The units a written number multiplies.")

(defparameter +digits-to-kanji+ "〇一二三四五六七八九")

(defun kanji-to-int (text)
  "TEXT as an integer, written in digits or in kanji (三十二 => 32), or NIL."
  (cond ((zerop (length text)) nil)
        ((every #'digit-char-p text) (values (parse-integer text)))
        (t (let ((total 0) (current 0) (digit nil) (readable t))
             (loop for character across text
                   for value = (position character +kanji-digits+)
                   for unit = (cdr (assoc character +kanji-units+))
                   do (cond (value (setf current (if digit (+ (* current 10) value) value)
                                      digit t))
                            (unit (incf total (* (if (plusp current) current 1) unit))
                                  (setf current 0 digit nil))
                            (t (setf readable nil)))
                   finally (return (and readable (+ total current))))))))

(defun int-to-kanji (value)
  "VALUE as law text writes it: 32 => 三十二, 143 => 百四十三."
  (if (< value 10)
      (string (char +digits-to-kanji+ value))
      (with-output-to-string (out)
        (let ((rest value))
          (dolist (pair '((1000 . #\千) (100 . #\百) (10 . #\十)))
            (let* ((unit (car pair))
                   (character (cdr pair))
                   (count (floor rest unit)))
              (setf rest (mod rest unit))
              (when (plusp count)
                (when (> count 1) (princ (char +digits-to-kanji+ count) out))
                (princ character out))))
          (when (plusp rest) (princ (char +digits-to-kanji+ rest) out))))))
(defun fullwidth (text)
  "TEXT's digits as the full-width digits law text uses for paragraph numbers."
  (map 'string (lambda (character)
                  (if (digit-char-p character)
                      (code-char (+ #xFF10 (digit-char-p character)))
                      character))
       text))

(defun ascii-digits (text)
  "TEXT with full-width digits read as ASCII ones."
  (map 'string (lambda (character)
                  (let ((code (char-code character)))
                    (if (<= #xFF10 code #xFF19)
                        (code-char (+ (- code #xFF10) #x30))
                        character)))
       text))

(defun compact (text)
  "TEXT with the spaces a person types inside a law name taken out."
  (remove-if (lambda (character) (find character " 　")) text))

(defun normalize-name (text)
  "TEXT as a law name is compared: no spaces, no quote brackets."
  (remove-if (lambda (character) (find character " 　「」『』")) text))

(defparameter +aliases+
  '(("個情法" . "415AC0000000057")
    ("特商法" . "351AC0000000057")
    ("下請法" . "331AC0000000120")
    ("雇保法" . "349AC0000000116")
    ("育介法" . "403AC0000000076")
    ("育児休業法" . "403AC0000000076")
    ("パート法" . "405AC0000000076")
    ("パート有期法" . "405AC0000000076")
    ("フリーランス法" . "505AC0000000025")
    ("派遣法" . "360AC0000000088")
    ("均等法" . "347AC0000000113")
    ("憲法" . "321CONSTITUTION")
    ("健保法" . "211AC0000000070")
    ("厚年法" . "329AC0000000115")
    ("パワハラ防止法" . "341AC0000000132")
    ("高年法" . "346AC0000000068")
    ("労基則" . "322M40000100023")
    ("雇保則" . "350M50002000003"))
  "Common short names e-Gov does not index as a title or abbreviation.")

(defun alias-id (name)
  "The law ID a common short NAME stands for, or NIL."
  (cdr (assoc (normalize-name name) +aliases+ :test #'string=)))

;;; --- the element name an article is read by ------------------------------------

(defparameter +number+ "[0-9０-９〇零一二三四五六七八九十百千]+"
  "The characters an article or paragraph number is written with.")

(defun law-id-p (text)
  "TEXT as an e-Gov law ID (322AC0000000049, 321CONSTITUTION)."
  (and (ppcre:scan "^[0-9]{3}(?:[A-Z]{2}[0-9]{10}|CONSTITUTION)$" text) t))

(defun article-part (text)
  "TEXT as (VALUES ELEMENT NUMBERS BRANCH LABEL PARAGRAPH SUPPL): the e-Gov
element to read, the article's own numbers, and the paragraph an article or
the whole 附則 names; NIL when TEXT names no article at all."
  (let* ((text (ascii-digits (compact text)))
         (suppl (and (ppcre:scan "^附則" text) t))
         (rest (if suppl (subseq text (length "附則")) text))
         (paragraph nil))
    (multiple-value-bind (whole start end starts ends)
        (ppcre:scan (format nil "第?(~a)項$" +number+) rest)
      (declare (ignore end))
      (when whole
        (setf paragraph (kanji-to-int (subseq rest (aref starts 0) (aref ends 0))))
        (setf rest (string-right-trim " 　" (subseq rest 0 start)))))
    (cond ((and suppl (zerop (length rest)))
           (values "SupplProvision" nil nil "附則" paragraph t))
          (t
           (multiple-value-bind (whole groups)
               (and (plusp (length rest))
                    (ppcre:scan-to-strings
                     (format nil "^第?(~a)(?:条(?:の(~a))?)?$" +number+ +number+) rest))
             (when whole
               (let* ((numbers (kanji-to-int (aref groups 0)))
                      (branch (and (aref groups 1) (kanji-to-int (aref groups 1))))
                      (label (format nil "第~a条~@[の~a~]" (int-to-kanji numbers)
                                     (and branch (int-to-kanji branch)))))
                 (values (format nil "Article_~d~@[_~d~]" numbers branch)
                         numbers branch label paragraph suppl))))))))

(defun article-element (text paragraph)
  "TEXT as the e-Gov element to read, with its label, and the paragraph
number to cut to, or a refusal naming what could not be read."
  (multiple-value-bind (element numbers branch label written suppl) (article-part text)
    (declare (ignore branch))
    (unless (or element (and written (null numbers)))
      (law-fail "bad_argument"
                "article is not accepted (~a): pass digits or the written form, 32, 第32条, 三十二条 or 32の2"
                text))
    (let* ((paragraph (or paragraph written))
           (base (cond ((null element) "SupplProvision")
                       ((string= element "SupplProvision") "SupplProvision")
                       (suppl (concatenate 'string "SupplProvision-" element))
                       (t (concatenate 'string "MainProvision-" element)))))
      (values (if paragraph (format nil "~a-Paragraph_~d" base paragraph) base)
              (if paragraph (format nil "~a第~d項" label paragraph) label)
              paragraph))))

(defun paragraph-only (element)
  "Whether ELEMENT already names a paragraph, so it needs no MainProvision prefix."
  (and (ppcre:scan "^SupplProvision-Paragraph_" element) t))

;;; --- resolving a law ------------------------------------------------------------

(defun law-entry-summary (entry match)
  "ENTRY — one row of /laws — as the answer lists it, with MATCH saying how
its name matched."
  (let ((info (part entry "law_info"))
        (revision (part entry "revision_info")))
    (object "law_title" (field revision "law_title")
            "law_id" (field info "law_id")
            "law_num" (field info "law_num")
            "abbrev" (field revision "abbrev")
            "law_type" (field info "law_type")
            "category" (field revision "category")
            "match" match
            "url" (let ((id (field info "law_id"))) (and id (law-page id))))))

(defun match-kind (entry query)
  "How ENTRY's name matches QUERY: exact title, abbreviation, or a part."
  (let ((revision (part entry "revision_info"))
        (info (part entry "law_info")))
    (declare (ignore info))
    (cond ((and (field revision "law_title")
                (string= (normalize-name (field revision "law_title")) query))
           "exact title")
          ((and (field revision "abbrev")
                (string= (normalize-name (field revision "abbrev")) query))
           "abbreviation")
          (t "part of the title"))))

(defun rank (entries query)
  "ENTRIES with exact titles first, then abbreviations, then Acts before
orders and rules, each group by the shorter name."
  (stable-sort (copy-list entries)
               (lambda (left right)
                 (let ((left-kind (match-kind left query))
                       (right-kind (match-kind right query)))
                   (if (string= left-kind right-kind)
                       (< (length (or (field (part left "revision_info") "law_title") ""))
                          (length (or (field (part right "revision_info") "law_title") "")))
                       (string< left-kind right-kind))))
               :key #'identity))

(defun laws-of (params)
  "The /laws rows PARAMS selects, as a list."
  (let ((rows (parts (get-json "laws" :params params) "laws")))
    (if rows (coerce rows 'list) '())))

(defun resolve-law (law)
  "LAW as a person wrote it => (VALUES KEY HOW TITLE): the law ID or law
number the API reads by, how it was recognised, and the title when known."
  (let* ((raw (string-trim " 　" (or law "")))
         (text (compact raw)))
    (when (zerop (length raw))
      (law-fail "bad_argument"
                "law is empty: pass a law's name (労働基準法), e-Gov's abbreviation (労基法), a law number, or a law ID"))
    (cond ((law-id-p text) (values text "law ID" nil))
          ((alias-id text) (values (alias-id text) (format nil "common short name ~a" raw) nil))
          (t (let* ((query (normalize-name text))
                    (entries (laws-of (list (cons "law_title" text)
                                            (cons "repeal_status" "None")
                                            (cons "limit" "100"))))
                    (total (or (nlk:json-value (get-json "laws"
                                                          :params (list (cons "law_title" text)
                                                                        (cons "repeal_status" "None")
                                                                        (cons "limit" "1")))
                                               :integer "total_count")
                               (length entries))))
               (declare (ignore total))
               (when (zerop (length entries))
                 (law-fail "law_not_found"
                           "no law in force has a title or abbreviation containing ~a; try a shorter part of the name, or e-Gov's official title"
                           raw))
               (let* ((exact (remove-if-not (lambda (entry) (string= (match-kind entry query) "exact title")) entries))
                      (abbrev (remove-if-not (lambda (entry) (string= (match-kind entry query) "abbreviation")) entries))
                      (chosen (or (first (rank exact query))
                                  (first (rank abbrev query))
                                  (and (= (length entries) 1) (first entries)))))
                 (unless chosen
                   (law-fail "ambiguous_law"
                             "~d laws match ~a and none is an exact title or abbreviation; call again with one of these law IDs: ~{{~a ~a}~^; ~}"
                             (length entries) raw
                             (loop for entry in (subseq (rank entries query) 0 (min 8 (length entries)))
                                   collect (list (field (part entry "revision_info") "law_title")
                                                 (field (part entry "law_info") "law_id")))))
                 (values (field (part chosen "law_info") "law_id")
                         (match-kind chosen query)
                         (field (part chosen "revision_info") "law_title"))))))))

;;; --- search ------------------------------------------------------------------------

(defun search-laws (query &key (mode "title") (limit 10) include-repealed)
  "Laws whose title matches QUERY, or with MODE text, laws whose articles
mention it => a JSON-ready object."
  (let ((query (string-trim " 　" (or query ""))))
    (when (zerop (length query))
      (law-fail "bad_argument"
                "query is empty: pass a law name (労働基準法), an abbreviation (労基法) or, with mode text, a phrase"))
    (setf limit (max 1 (min 50 (or limit 10))))
    (if (string-equal mode "text")
        (search-text query limit)
        (let* ((text (compact query))
               (name (normalize-name text))
               (params (list (cons "limit" "100")
                             (cons (cond ((law-id-p text) "law_id") (t "law_title")) text)
                             (unless include-repealed (cons "repeal_status" "None"))))
               (entries (laws-of params))
               (results (loop for entry in (rank entries name)
                              for index from 1
                              when (<= index limit)
                                collect (law-entry-summary entry (match-kind entry name)))))
          (object "query" query
                  "mode" "title"
                  "returned" (length results)
                  "results" (coerce results 'vector)
                  "source" (object "attribution" +attribution+ "retrieved_at" (today-jst)
                                   "url" +site+)
                  "next_step" (cond ((null results)
                                      "no law's title contains that; try a shorter or official name, or call again with mode text")
                                     (t nil)))))))

(defun search-text (query limit)
  "Laws whose articles mention QUERY, grouped, each with a few snippets."
  (let* ((data (handler-case (get-json "keyword" :params (list (cons "keyword" query)
                                                               (cons "limit" (princ-to-string (min 100 (* 5 limit))))))
                  (egov-law-error (condition)
                    (if (string= *error-kind* "not_found")
                        (object "items" (vector))
                        (error condition)))))
         (items (parts data "items"))
         (grouped '()))
    (loop for item across (or items #())
          for info = (part item "law_info")
          for law-id = (field info "law_id")
          for known = (assoc law-id grouped :test #'string=)
          do (cond (known
                    (loop for sentence across (or (parts item "sentences") #())
                          for text = (field sentence "text")
                          when (and text (< (length (getf (cdr known) :snippets)) 3))
                            do (pushnew text (getf (cdr known) :snippets) :test #'string=)))
                   ((and law-id (< (length grouped) limit))
                    (let ((summary (law-entry-summary item "part of the law text")))
                      (setf (gethash "snippets" summary)
                            (let ((snippets '()))
                              (loop for sentence across (or (parts item "sentences") #())
                                    for text = (field sentence "text")
                                    when (and text (< (length snippets) 3))
                                      do (pushnew text snippets :test #'string=))
                              (coerce (reverse snippets) 'vector)))
                      (push (cons law-id (list :summary summary)) grouped)))))
    (let ((results (mapcar (lambda (cell) (getf (cdr cell) :summary)) (reverse grouped))))
      (object "query" query
              "mode" "text"
              "total_matching_sentences" (nlk:json-value data :integer "total_count")
              "returned_laws" (length results)
              "results" (coerce results 'vector)
              "source" (object "attribution" +attribution+ "retrieved_at" (today-jst)
                               "url" +site+)
              "next_step" (if results
                              "snippets carry no article numbers: call egov_law_article on the article you think applies"
                              "no law text contains that phrase; try fewer words, or mode title if it is part of a name")))))
