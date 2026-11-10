;;;; csv.lisp --- a type=5 CSV ZIP read without a ZIP library.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's metrics.py and the ZIP half of its save.py: EDINET's type=5
;;;; body is a ZIP of UTF-16 tab-separated CSV, one row per XBRL fact. This
;;;; file reads the archive's central directory, inflates a member with
;;;; CHIPZ (already in the image, for dexador), decodes the UTF-16 itself,
;;;; and picks the headline rows the plugin picked: a consolidated
;;;; current-year context, matched by its taxonomy element ids.

(in-package #:nodecode-edinet)

(defun u16 (octets at)
  "The little-endian 16-bit word at AT."
  (+ (aref octets at) (ash (aref octets (1+ at)) 8)))

(defun u32 (octets at)
  "The little-endian 32-bit word at AT."
  (+ (aref octets at)
     (ash (aref octets (+ at 1)) 8)
     (ash (aref octets (+ at 2)) 16)
     (ash (aref octets (+ at 3)) 24)))

(defun end-of-central-directory (octets)
  "Where OCTETS' end-of-central-directory record starts, or NIL."
  (loop for at from (- (length octets) 22) downto 0
        when (and (= (aref octets at) #x50) (= (aref octets (+ at 1)) #x4B)
                  (= (aref octets (+ at 2)) #x05) (= (aref octets (+ at 3)) #x06))
          return at))

(defun zip-members (octets)
  "OCTETS — a ZIP archive — as a list of (NAME METHOD COMPRESSED-SIZE LOCAL-HEADER-AT)."
  (let ((end (end-of-central-directory octets)))
    (unless end (fail "EDINET answered a ZIP with no end-of-central-directory record."))
    (loop with count = (u16 octets (+ end 10))
          with at = (u32 octets (+ end 16))
          repeat count
          collect (let ((name-size (u16 octets (+ at 28)))
                        (extra-size (u16 octets (+ at 30)))
                        (comment-size (u16 octets (+ at 32)))
                        (name (map 'string #'code-char
                                   (subseq octets (+ at 46) (+ at 46 (u16 octets (+ at 28))))))
                        (method (u16 octets (+ at 10)))
                        (size (u32 octets (+ at 20)))
                        (local (u32 octets (+ at 42))))
                    (prog1 (list name method size local)
                      (incf at (+ 46 name-size extra-size comment-size)))))))

(defun zip-member-bytes (octets entry)
  "ENTRY's bytes, inflated: a stored member as it is, a deflated one through CHIPZ."
  (destructuring-bind (name method size local) entry
    (let* ((name-size (u16 octets (+ local 26)))
           (extra-size (u16 octets (+ local 28)))
           (start (+ local 30 name-size extra-size))
           (raw (subseq octets start (+ start size))))
      (case method
        (0 raw)
        (8 (handler-case (chipz:decompress nil 'chipz:deflate raw)
             (error (condition)
               (fail "the EDINET ZIP member ~a would not inflate (~a)." name condition))))
        (t (fail "the EDINET ZIP member ~a uses compression method ~a, which this cell does not read."
                 name method))))))

(defun utf-16-string (octets)
  "OCTETS as a string: UTF-16, with a byte-order mark or without (then little-endian)."
  (let* ((length (length octets))
         (marked (and (>= length 2)
                       (or (and (= (aref octets 0) #xFF) (= (aref octets 1) #xFE))
                           (and (= (aref octets 0) #xFE) (= (aref octets 1) #xFF)))))
         (big (and marked (= (aref octets 0) #xFE)))
         (start (if marked 2 0)))
    (flet ((unit (at)
             (if big
                 (+ (ash (aref octets at) 8) (aref octets (1+ at)))
                 (u16 octets at))))
      (with-output-to-string (out)
        (loop with at = start
              while (<= (+ at 1) (1- length))
              do (let ((code (unit at)))
                   (when (<= #xD800 code #xDBFF)
                     (when (<= (+ at 3) (1- length))
                       (let ((low (unit (+ at 2))))
                         (when (<= #xDC00 low #xDFFF)
                           (setf code (+ #x10000 (ash (- code #xD800) 10) (- low #xDC00)))
                           (incf at 2)))))
                   (write-char (code-char (if (<= code #x10FFFF) code #xFFFD)) out)
                   (incf at 2)))))))

(defun split-fields (line separator)
  "LINE's fields between SEPARATOR characters, empty ones kept."
  (let ((fields '())
        (start 0))
    (loop
      (let ((at (position separator line :start start)))
        (push (subseq line start at) fields)
        (unless at (return))
        (setf start (1+ at))))
    (nreverse fields)))

(defun tsv-lines (text)
  "TEXT's lines, without the trailing carriage return each may carry."
  (loop for line in (split-fields text #\Newline)
        collect (string-right-trim '(#\Return) line)))

(defun csv-member-p (name)
  "Whether NAME is a CSV member of the archive."
  (let ((name (string-downcase name)))
    (and (> (length name) 4) (string= ".csv" name :start2 (- (length name) 4)))))

(defun column-index (header name)
  "The position of NAME in HEADER, a list of column titles, or a refusal."
  (let ((at (position name header :test #'string=)))
    (or at (fail "the EDINET CSV has no ~a column; its header is ~{~a~^, ~}."
                 name (mapcar (lambda (title) (clip title 24)) header)))))

(defun csv-rows (octets)
  "The rows of a type=5 CSV ZIP, each a plist of :ELEMENT :LABEL :CONTEXT :CONSOLIDATION
:UNIT and :VALUE, in the order the members are read."
  (let ((rows '()))
    (dolist (entry (remove-if-not (lambda (entry) (csv-member-p (first entry))) (zip-members octets)))
      (let* ((lines (tsv-lines (utf-16-string (zip-member-bytes octets entry))))
             (header (and lines (split-fields (first lines) #\Tab))))
        (when header
          (let ((element (column-index header "要素ID"))
                (label (column-index header "項目名"))
                (context (column-index header "コンテキストID"))
                (consolidation (column-index header "連結・個別"))
                (unit (column-index header "単位"))
                (value (column-index header "値")))
            (flet ((field (fields at) (if (< at (length fields)) (nlk:trimmed (nth at fields)) "")))
              (dolist (line (rest lines))
                (let ((fields (split-fields line #\Tab)))
                  (when (> (length fields) element)
                    (push (list :element (field fields element)
                                :label (field fields label)
                                :context (field fields context)
                                :consolidation (field fields consolidation)
                                :unit (field fields unit)
                                :value (field fields value))
                          rows)))))))))
    (nreverse rows)))

;;; --- the headline rows --------------------------------------------------------

(defparameter +metric-elements+
  '(("revenue"
     "jpcrp_cor:NetSalesSummaryOfBusinessResults"
     "jpcrp_cor:RevenuesUSGAAPSummaryOfBusinessResults"
     "jpcrp_cor:RevenueIFRSSummaryOfBusinessResults"
     "jppfs_cor:NetSales"
     "jpigp_cor:RevenueIFRS")
    ("operating_income"
     "jpcrp_cor:OperatingIncomeSummaryOfBusinessResults"
     "jpcrp_cor:OperatingIncomeUSGAAPSummaryOfBusinessResults"
     "jpcrp_cor:OperatingProfitLossIFRSSummaryOfBusinessResults"
     "jppfs_cor:OperatingIncome"
     "jpigp_cor:OperatingProfitLossIFRS")
    ("net_income"
     "jpcrp_cor:ProfitLossAttributableToOwnersOfParentSummaryOfBusinessResults"
     "jpcrp_cor:ProfitLossAttributableToOwnersOfParentIFRSSummaryOfBusinessResults"
     "jpcrp_cor:NetIncomeLossAttributableToOwnersOfParentUSGAAPSummaryOfBusinessResults"
     "jpigp_cor:ProfitLossAttributableToOwnersOfParentIFRS"
     "jppfs_cor:ProfitLossAttributableToOwnersOfParent"
     "jpcrp_cor:NetIncomeLossSummaryOfBusinessResults")
    ("total_assets"
     "jpcrp_cor:TotalAssetsSummaryOfBusinessResults"
     "jpcrp_cor:TotalAssetsIFRSSummaryOfBusinessResults"
     "jpcrp_cor:TotalAssetsUSGAAPSummaryOfBusinessResults"
     "jppfs_cor:Assets"
     "jpigp_cor:AssetsIFRS")
    ("net_assets"
     "jpcrp_cor:NetAssetsSummaryOfBusinessResults"
     "jpcrp_cor:EquityAttributableToOwnersOfParentIFRSSummaryOfBusinessResults"
     "jpcrp_cor:EquityAttributableToOwnersOfParentUSGAAPSummaryOfBusinessResults"
     "jpigp_cor:EquityAttributableToOwnersOfParentIFRS"
     "jppfs_cor:NetAssets")
    ("operating_cash_flow"
     "jpcrp_cor:CashFlowsFromUsedInOperatingActivitiesSummaryOfBusinessResults"
     "jpcrp_cor:CashFlowsFromUsedInOperatingActivitiesIFRSSummaryOfBusinessResults"
     "jpcrp_cor:CashFlowsFromUsedInOperatingActivitiesUSGAAPSummaryOfBusinessResults"
     "jppfs_cor:NetCashProvidedByUsedInOperatingActivities"
     "jpigp_cor:CashFlowsFromUsedInOperatingActivitiesIFRS"))
  "The taxonomy element ids each headline metric is read from, most specific first.
The plugin's own list: a company-extension tag outside it is REPORTED MISSING, never guessed.")

(defparameter +accounting-standard-element+ "jpdei_cor:AccountingStandardsDEI"
  "The DEI row that says which accounting standard the filing is in.")

(defparameter +headline-contexts+ '("CurrentYearDuration" "CurrentYearInstant")
  "A headline row carries no dimension member: a longer context id is a segment or a component.")

(defparameter +consolidated-labels+ '("連結" "その他")
  "EDINET labels a context with no consolidation axis その他; 連結 is the explicit one.
個別 (non-consolidated) sits on the same figures for a parent-only company.")

(defparameter +skipped-element-parts+
  '("TextBlock" "PerShare" "Ratio" "RateOfReturn" "NumberOf" "BookValue" "DetailsOf"
    "Notes" "Comprehensive" "NonOperating" "Intersegment" "FromExternalCustomers")
  "Element ids that look like a headline but are not one.")

(defun headline-context-p (row)
  "Whether ROW is a consolidated current-year row with no dimension member."
  (and (member (getf row :context) +headline-contexts+ :test #'string=)
       (member (getf row :consolidation) +consolidated-labels+ :test #'string=)
       (notany (lambda (part) (search part (getf row :element))) +skipped-element-parts+)))

(defun parse-figure (text)
  "TEXT as a number, or NIL: EDINET writes a dash for a figure it does not carry."
  (let ((text (remove #\, (or text ""))))
    (when (and (plusp (length text))
               (not (member text '("－" "-" "—" "―" "N/A" "n/a") :test #'string=)))
      (multiple-value-bind (number end) (ignore-errors (read-from-string text))
        (and (numberp number) (= end (length text)) number)))))

(defun metric-score (metric row)
  "How well ROW answers METRIC: an exact element id beats a longer company-extension one."
  (let ((element (getf row :element))
        (ids (rest (assoc metric +metric-elements+ :test #'string=))))
    (when (headline-context-p row)
      (let ((at (position element ids :test #'string=)))
        (cond (at (- 1000 at (if (search "SummaryOfBusinessResults" element) 0 20)))
              ((and ids
                    (some (lambda (id)
                            (search (car (last (split-fields id #\:))) element))
                          ids))
               500)
              (t nil))))))

(defun find-figure (metric rows)
  "ROW's best answer for METRIC, or NIL."
  (let ((best nil)
        (best-score -1))
    (dolist (row rows)
      (let ((score (metric-score metric row)))
        (when (and score (> score best-score) (parse-figure (getf row :value)))
          (setf best row
                best-score score))))
    best))

(defun accounting-standard (rows)
  "The filing's accounting standard, from its DEI row."
  (let ((row (find +accounting-standard-element+ rows :key (lambda (row) (getf row :element))
                                                        :test #'string=)))
    (and row (nonempty (getf row :value)) (getf row :value))))

(defun financials-text (rows &key doc-id)
  "The headline figures of ROWS, as the text one tool call answers with."
  (let* ((standard (accounting-standard rows))
         (names (mapcar #'first +metric-elements+)))
    (with-output-to-string (out)
      (format out "EDINET filing ~a~@[ (~a)~]~%" doc-id standard)
      (dolist (name names)
        (let* ((row (find-figure name rows))
               (figure (and row (parse-figure (getf row :value)))))
          (if row
              (format out "  ~12a ~:d ~a~@[ (~a)~]~%"
                      name (round figure) (or (nonempty (getf row :unit)) "")
                      (nonempty (getf row :label)))
              (format out "  ~12a missing: no consolidated current-year row matched this cell's ~
element ids (never guessed as 0)~%" name))))
      (format out "~a~%" (citation doc-id)))))
