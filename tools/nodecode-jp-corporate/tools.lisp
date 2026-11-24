;;;; tools.lisp --- the two tools, what each asks gBizINFO, and what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin published jp_corp_search and jp_corp_profile over the same
;;;; API; this cell keeps both names and both answers, shaped the way the
;;;; plugin shaped them (the same members, the same section names, the same
;;;; attribution), so a model that learned the tools from the plugin finds
;;;; itself at home. Each tool answers text: a laid-out record the model
;;;; reads, with the gBizINFO url it can cite.

(in-package #:nodecode-jp-corporate)

;;; --- reading a decoded answer ----------------------------------------------------

(defparameter +search-fetch-limit+ 1000
  "Rows asked of gBizINFO for one name search: one request, ranked here.")

(defparameter +default-results+ 10)
(defparameter +max-results+ 50)
(defparameter +default-items+ 10)
(defparameter +max-items+ 50)
(defparameter +max-chars+ 300)

(defun array-p (value)
  "Whether VALUE is a JSON array (a vector that is not a string)."
  (and (vectorp value) (not (stringp value))))

(defun objects (value)
  "VALUE — a JSON array — as a list of its objects."
  (and (array-p value) (remove-if-not #'hash-table-p (coerce value 'list))))

(defun field (object key &optional (type :any))
  "OBJECT's member KEY read as TYPE (:text, :integer, :object, :array, :any)."
  (and (hash-table-p object) (nlk:json-value object type key)))

(defun present-p (value)
  "Whether VALUE is worth laying out: not NIL, not an empty string, not an empty container."
  (cond ((null value) nil)
        ((eq value :null) nil)
        ((stringp value) (plusp (length value)))
        ((array-p value) (plusp (length value)))
        ((hash-table-p value) (plusp (hash-table-count value)))
        (t t)))

(defun bolt (&rest pairs)
  "A JSON object from PAIRS, leaving out every value PRESENT-P says is empty."
  (loop for (key value) on pairs by #'cddr
        when (present-p value) append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

(defun one-line (value)
  "VALUE as one line: a newline inside it becomes a separator, and an empty string is NIL."
  (when (stringp value)
    (let* ((flat (with-output-to-string (out)
                   (loop for character across value
                         do (if (member character '(#\Newline #\Return))
                                (write-string " / " out)
                                (write-char character out)))))
           (text (string-trim '(#\Space #\Tab) flat)))
      (and (plusp (length text)) text))))

(defun date-of (value)
  "VALUE as a YYYY-MM-DD date, or as it came."
  (when (stringp value)
    (let ((text (string-trim '(#\Space) value)))
      (if (>= (length text) 10) (subseq text 0 10) text))))

(defun integer-of (value)
  "VALUE as an integer when it holds one (1,234 included), else VALUE."
  (cond ((integerp value) value)
        ((and (stringp value) (plusp (length value)))
         (let ((text (remove #\, (string-trim '(#\Space) value))))
           (multiple-value-bind (number end) (parse-integer text :junk-allowed t)
             (if (and number (= end (length text))) number (one-line value)))))
        (t value)))

(defun postal-of (value)
  "VALUE as a postal code with its hyphen, when it is seven digits."
  (let ((text (and (stringp value) (string-trim '(#\Space) value))))
    (if (and text (= (length text) 7) (every #'digit-char-p text))
        (format nil "~a-~a" (subseq text 0 3) (subseq text 3))
        text)))

(defun status-of (value)
  "'closed' when gBizINFO says the registration is closed, else 'registered'.
Registered is not proof that the business is operating."
  (if (and (stringp value) (plusp (length value)) (not (string= value "-")))
      "closed"
      "registered"))

(defun page-url (number)
  "The gBizINFO page of one corporation."
  (format nil "https://info.gbiz.go.jp/hojin/ichiran?hojinBango=~a" number))

(defun jst-today ()
  "(values YEAR MONTH DAY) in Japan, where the API's terms date the use."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ (get-universal-time) (* 9 3600)))
    (declare (ignore second minute hour))
    (values year month day)))

(defun attribution (url)
  "The credit line gBizINFO's terms ask for: the source, the reformatting, the day."
  (multiple-value-bind (year month day) (jst-today)
    (bolt "ja" (format nil "出典：「Gビズインフォ」（経済産業省）（~a）を加工して作成（~d年~d月~d日に利用）"
                         url year month day)
          "en" (format nil "Source: gBizINFO (Ministry of Economy, Trade and Industry, Japan), ~a, retrieved ~d-~2,'0d-~2,'0d; reformatted by the nodecode-jp-corporate cell."
                        url year month day))))

;;; --- laying an answer out ---------------------------------------------------------

(defun sorted-members (object)
  "OBJECT's members as (KEY . VALUE), by key."
  (sort (loop for key being the hash-keys of object using (hash-value value)
               collect (cons key value))
        #'string< :key #'car))

(defun nested-p (value)
  "Whether VALUE gets lines of its own."
  (or (hash-table-p value) (and (array-p value) (plusp (length value)))))

(defun scalar-text (value)
  "VALUE — a JSON scalar — as one line."
  (cond ((stringp value) (one-line (if (> (length value) +max-chars+)
                                         (concatenate 'string (subseq value 0 +max-chars+) " ...")
                                         value)))
        ((null value) "")
        ((eq value :null) "")
        ((eq value t) "true")
        ((integerp value) (format nil "~d" value))
        ((numberp value) (format nil "~f" value))
        (t (princ-to-string value))))

(defun render-to (value out indent)
  "VALUE laid out on OUT at INDENT spaces."
  (let ((pad (make-string indent :initial-element #\Space)))
    (cond
      ((hash-table-p value)
       (let ((members (sorted-members value)))
         (if (null members)
             (format out "~a(none)~%" pad)
             (dolist (member members)
               (format out "~a~a: " pad (car member))
               (if (nested-p (cdr member))
                   (progn (terpri out) (render-to (cdr member) out (+ indent 2)))
                   (format out "~a~%" (scalar-text (cdr member))))))))
      ((array-p value)
       (if (zerop (length value))
           (format out "~a(none)~%" pad)
           (loop for entry across value
                 for index from 1
                 do (format out "~a~d) " pad index)
                    (if (nested-p entry)
                        (progn (terpri out) (render-to entry out (+ indent 3)))
                        (format out "~a~%" (scalar-text entry))))))
      (t (format out "~a~a~%" pad (scalar-text value))))))

(defun render (value)
  "VALUE — a decoded or built JSON object — as the text the model reads."
  (with-output-to-string (out) (render-to value out 0)))

(defun clamp (value default upper)
  "VALUE as an integer between 1 and UPPER, else DEFAULT."
  (let ((number (and value (ignore-errors (parse-integer (princ-to-string value) :junk-allowed t)))))
    (if number (max 1 (min upper number)) default)))

;;; --- jp_corp_search ---------------------------------------------------------------

(defun summary (info match)
  "One search row: who the corporation is, and where the registry has it."
  (let ((number (field info "corporate_number" :text)))
    (bolt "corporate_number" number
          "name" (field info "name" :text)
          "location" (field info "location" :text)
          "postal_code" (postal-of (field info "postal_code" :text))
          "status" (status-of (field info "status" :text))
          "match" match
          "gbizinfo_url" (and number (page-url number)))))

(defun name-check (given official)
  "How GIVEN compares with the registered name OFFICIAL."
  (cond ((string= (full-name given) (full-name official)) "match")
        ((string= (core-name given) (core-name official)) "match_except_legal_form")
        (t "mismatch")))

(defun municipality-of (folded)
  "The city, ward, town or village FOLDED starts with, or NIL."
  (let ((position (position-if (lambda (character) (find character "市区町村")) folded)))
    (and position (subseq folded 0 (1+ position)))))

(defun municipality-check (a b)
  "'same_municipality_different_street' when two folded addresses share a city,
else 'mismatch'."
  (let ((city-a (municipality-of a)) (city-b (municipality-of b)))
    (if (and city-a city-b (string= city-a city-b))
        "same_municipality_different_street"
        "mismatch")))

(defun address-check (given official)
  "How an address from a document compares with the registered head office."
  (multiple-value-bind (given-prefecture given-rest) (split-prefecture given)
    (multiple-value-bind (official-prefecture official-rest) (split-prefecture official)
      (if (and given-prefecture official-prefecture (/= given-prefecture official-prefecture))
          "mismatch"
          (let ((a (fold-address given-rest)) (b (fold-address official-rest)))
            (cond
              ((zerop (length a)) "partial")
              ((string= a b) "match")
              (t
               (let* ((a-shorter (< (length a) (length b)))
                      (shorter (if a-shorter a b))
                      (longer (if a-shorter b a))
                      (tail (and (has-prefix-p shorter longer) (subseq longer (length shorter))))
                      (ends-in-digit (and (plusp (length shorter)) (digit-char-p (char shorter (1- (length shorter)))))))
                 (cond
                   ;; 1 against 10: different lot numbers, unless the document only adds a building.
                   ((and tail (plusp (length tail)) ends-in-digit)
                    (if (digit-char-p (char tail 0)) (municipality-check a b) "match"))
                   ((and tail (string= shorter a)) "partial")
                   ((and tail (plusp (length tail)) (char= (char tail 0) #\-)) "more_detailed")
                   (t (municipality-check a b)))))))))))

(defparameter +checks-note+
  (concatenate 'string
               "name: match = the same after ignoring width, spaces and (株)/㈱ against 株式会社; "
               "match_except_legal_form = the same words under another legal form or position; mismatch otherwise. "
               "address: match = the same address (a building name after the lot number is ignored); "
               "partial = the given address stops early (a city only), so confirm the rest; "
               "more_detailed = the document adds a sub-number the registry does not have (1番地 against 1-1), "
               "usually the same place; same_municipality_different_street; mismatch. "
               "This checks the corporation behind the number, not qualified-invoice-issuer registration "
               "(that is https://www.invoice-kohyo.nta.go.jp/)."))

(defun lookup-number (raw-number name address)
  "One corporation by its number, with the checks an invoice wants."
  (let* ((number (parse-corporate-number raw-number))
         (data (gbiz-get number))
         (info (first (objects (field (or data (nlk:json-object)) "hojin-infos" :array)))))
    (if (null info)
        (bolt "found" 0
              "corporate_number" number
              "message" (format nil "~a is a well-formed 法人番号 (check digit OK), but gBizINFO has no corporation with it. Possible reasons: the number was mistyped in a way the check digit cannot catch; the entity is newly registered and not yet in gBizINFO; or it came from an invoice (T-number) of a sole proprietor, whose registration numbers are not 法人番号. Invoice numbers are checked at https://www.invoice-kohyo.nta.go.jp/." number)
              "attribution" (attribution +site+))
        (let ((checks (bolt "name" (and (plusp (length name)) (name-check name (or (field info "name" :text) "")))
                            "address" (and (plusp (length address))
                                            (address-check address (or (field info "location" :text) ""))))))
          (bolt "found" 1
                "results" (vector (summary info "corporate_number"))
                "checks" checks
                "checks_note" (and (plusp (hash-table-count checks)) +checks-note+)
                "warning" (if (string= (status-of (field info "status" :text)) "closed")
                               (format nil "This corporation's registration is closed (~a). Call jp_corp_profile for the reason."
                                       (or (date-of (field info "close_date" :text)) "date unknown")))
                "attribution" (attribution (page-url number)))))))

(defun search-object (name address limit)
  "A name search: one request, ranked here."
  (let* ((term (query-term name))
         (wanted (core-name name)))
    (when (or (zerop (length term)) (zerop (length wanted)))
      (fail "name ~s has nothing left once the legal form (株式会社, (株), ...) comes off: include the distinctive part of the name, e.g. トヨタ自動車." name))
    (multiple-value-bind (prefecture street) (split-prefecture address)
      (let* ((data (gbiz-get "" (list (cons "name" term)
                                       (cons "limit" +search-fetch-limit+)
                                       (cons "prefecture" (and prefecture (format nil "~2,'0d" prefecture))))))
             (rows (objects (field (or data (nlk:json-object)) "hojin-infos" :array)))
             (fetched (length rows))
             (wanted-full (full-name name))
             (gave-form (not (string= wanted-full wanted)))
             (street-key (fold-address street))
             (ranked '()))
        (dolist (info rows)
          (let ((official (or (field info "name" :text) "")))
            (when (and (or (search wanted (core-name official)) (search wanted (fold official)))
                       (or (zerop (length street-key))
                           (search street-key (fold-address (or (field info "location" :text) "")))))
              (let* ((official-core (core-name official))
                     (rank (cond ((string= official-core wanted) 0)
                                 ((has-prefix-p wanted official-core) 1)
                                 (t 2)))
                     (form-differs (and gave-form (not (string= (full-name official) wanted-full))))
                     (closed (string= (status-of (field info "status" :text)) "closed"))
                     (key (format nil "~d~d~d~6,'0d~a" rank (if closed 1 0) (if form-differs 1 0)
                                   (length official) (or (field info "corporate_number" :text) ""))))
                (push (list key info rank) ranked)))))
        (setf ranked (stable-sort ranked #'string< :key #'first))
        (let ((query (bolt "name" name
                            "searched_as" term
                            "address" (and (plusp (length address)) address)
                            "prefecture" (and prefecture (prefecture-name prefecture)))))
          (if (null ranked)
              (let ((hints '()))
                (when (plusp fetched)
                  (push (format nil "~d corporations match the name, but none at '~a': check the address, or search again without it."
                                fetched address)
                        hints))
                (when (and (zerop fetched) (plusp (length address)))
                  (push "Search again without the address." hints))
                (push "Try a shorter distinctive part of the name (drop 本店, ホールディングス or a branch name)." hints)
                (push "Try the other script: katakana against kanji against alphabet (ソニー / SONY)." hints)
                (bolt "query" query
                      "found" 0
                      "message" (format nil "0 results: gBizINFO has no corporation whose name contains '~a'~@[ in ~a~]."
                                         term (and prefecture (zerop fetched) (prefecture-name prefecture)))
                      "next_steps" (coerce (nreverse hints) 'vector)
                      "attribution" (attribution +site+)))
              (let* ((labels #("exact" "starts_with" "contains"))
                     (shown (min limit (length ranked)))
                     (notes '())
                     (results (loop for (nil info rank) in (subseq ranked 0 shown)
                                    collect (summary info (aref labels rank)))))
                (when (> (length ranked) shown)
                  (push (format nil "~d more not shown: add an address (prefecture or city) to narrow, or raise limit."
                                (- (length ranked) shown))
                        notes))
                (when (>= fetched +search-fetch-limit+)
                  (push (format nil "gBizINFO returned its first ~d rows only: the name is very common, so add an address to be sure the right company is included."
                                +search-fetch-limit+)
                        notes))
                (when (> (count-if (lambda (entry) (eql 0 (third entry))) ranked) 1)
                  (push "Several corporations share this exact name: tell them apart by location and status." notes))
                (bolt "query" query
                      "found" (length ranked)
                      "shown" shown
                      "results" (coerce results 'vector)
                      "note" (and notes (format nil "~{~a~^ ~}" (nreverse notes)))
                      "attribution" (attribution +site+)))))))))

;;; --- jp_corp_profile --------------------------------------------------------------

(defparameter +sections+
  '(("basic" . "") ("subsidy" . "subsidy") ("procurement" . "procurement")
    ("commendation" . "commendation") ("certification" . "certification")
    ("finance" . "finance") ("patent" . "patent") ("workplace" . "workplace")
    ("offices" . "corporation"))
  "Section name -> the route under /v2/hojin/<number>; basic is the number itself.")

(defparameter +kind-labels+
  '(("101" . "国の機関 (national government body)") ("201" . "地方公共団体 (local government)")
    ("301" . "株式会社 (kabushiki kaisha)") ("302" . "有限会社 (yugen kaisha)")
    ("303" . "合名会社 (general partnership company)") ("304" . "合資会社 (limited partnership company)")
    ("305" . "合同会社 (godo kaisha / LLC)") ("399" . "その他の設立登記法人 (other registered corporation)")
    ("401" . "外国会社等 (foreign company)") ("499" . "その他 (other)")))

(defparameter +close-causes+
  '(("01" . "清算の結了等 (liquidation completed)") ("11" . "合併による解散等 (dissolved by merger)")
    ("21" . "登記官による閉鎖 (closed by the registrar)")
    ("31" . "その他の清算の結了等 (other liquidation)")))

(defparameter +industry-labels+
  '(("A" . "農業，林業") ("B" . "漁業") ("C" . "鉱業，採石業，砂利採取業") ("D" . "建設業")
    ("E" . "製造業") ("F" . "電気・ガス・熱供給・水道業") ("G" . "情報通信業")
    ("H" . "運輸業，郵便業") ("I" . "卸売業，小売業") ("J" . "金融業，保険業")
    ("K" . "不動産業，物品賃貸業") ("L" . "学術研究，専門・技術サービス業")
    ("M" . "宿泊業，飲食サービス業") ("N" . "生活関連サービス業，娯楽業")
    ("O" . "教育，学習支援業") ("P" . "医療，福祉") ("Q" . "複合サービス事業")
    ("R" . "サービス業（他に分類されないもの）") ("S" . "公務") ("T" . "分類不能の産業")))

(defun section-path (name)
  "The route suffix of section NAME, or NIL when there is no such section."
  (let ((entry (assoc name +sections+ :test #'string=)))
    (and entry (cdr entry))))

(defun labelled (table value)
  "VALUE through TABLE's label, or VALUE itself."
  (or (and value (cdr (assoc (princ-to-string value) table :test #'string=))) value))

(defun representative-of (value)
  "'代表取締役    山  田  太  郎' -> '代表取締役 山田太郎' (the registry pads names)."
  (let ((text (one-line value)))
    (when text
      (with-output-to-string (out)
        (loop with index = 0
              while (< index (length text))
              do (if (char= (char text index) #\Space)
                     (let ((end (or (position-if-not (lambda (character) (char= character #\Space))
                                                     text :start index)
                                    (length text))))
                       ;; Three or more spaces separate the title from the name; two only pad it.
                       (write-string (if (>= (- end index) 3) " " "") out)
                       (setf index end))
                     (progn (write-char (char text index) out) (incf index))))))))

(defun freshness-note (updated)
  "A note when the record is more than a year old, else NIL."
  (when (and (stringp updated) (>= (length updated) 10))
    (multiple-value-bind (year month day) (jst-today)
      (let ((given (ignore-errors (encode-universal-time 0 0 0 (parse-integer updated :start 8 :end 10)
                                                         (parse-integer updated :start 5 :end 7)
                                                         (parse-integer updated :start 0 :end 4))))
            (now (encode-universal-time 0 0 0 day month year)))
        (when (and given now (> (- now given) (* 365 24 3600)))
          (format nil "Representative, capital and headcount figures are as of ~a and may be out of date; name, address and closure status come from the National Tax Agency's corporate-number data, which gBizINFO imports daily."
                  updated))))))

(defun industry-text (codes)
  "CODES — the registry's industry codes — each with the name it stands for."
  (and (array-p codes)
       (coerce (loop for code across codes
                     for text = (and (stringp code) code)
                     for label = (and text (assoc text +industry-labels+ :test #'string=))
                     collect (if label (format nil "~a ~a" text (cdr label)) code))
               'vector)))

(defun basic-object (info)
  "The registry's own record of one corporation."
  (let ((closed (string= (status-of (field info "status" :text)) "closed"))
        (updated (date-of (field info "update_date" :text))))
    (bolt "name" (field info "name" :text)
          "kana" (field info "kana" :text)
          "name_en" (field info "name_en" :text)
          "kind" (labelled +kind-labels+ (field info "kind" :text))
          "status" (if closed "closed" "registered")
          "closed_on" (and closed (date-of (field info "close_date" :text)))
          "close_reason" (and closed (labelled +close-causes+ (field info "close_cause" :text)))
          "location" (field info "location" :text)
          "postal_code" (postal-of (field info "postal_code" :text))
          "representative" (representative-of (field info "representative_name" :text))
          "capital_stock_jpy" (integer-of (field info "capital_stock"))
          "employees" (integer-of (field info "employee_number"))
          "established" (date-of (field info "date_of_establishment" :text))
          "founding_year" (field info "founding_year")
          "business_summary" (one-line (field info "business_summary" :text))
          "website" (field info "company_url" :text)
          "industry" (industry-text (field info "industry" :array))
          "national_procurement_grade" (field info "qualification_grade" :text)
          "gbizinfo_last_updated" updated
          "freshness_note" (freshness-note updated))))

(defun by-date (items key)
  "ITEMS newest first by KEY."
  (stable-sort (copy-list items) #'string> :key (lambda (item) (or (field item key :text) ""))))

(defun listed (items shaper max-items date-key)
  "One list section: how many there are, and the newest MAX-ITEMS of them."
  (let* ((shaped (mapcar shaper items))
         (sorted (if date-key (by-date shaped date-key) shaped)))
    (bolt "total" (length shaped)
          "items" (and sorted (coerce (subseq sorted 0 (min max-items (length sorted))) 'vector)))))

(defun subsidy-object (item)
  (bolt "date" (date-of (field item "date_of_approval" :text))
        "title" (one-line (field item "title" :text))
        "amount_jpy" (integer-of (field item "amount"))
        "target" (field item "target" :text)
        "ministry" (field item "government_departments" :text)))

(defun procurement-object (item)
  (bolt "date" (date-of (field item "date_of_order" :text))
        "title" (one-line (field item "title" :text))
        "amount_jpy" (integer-of (field item "amount"))
        "ordered_by" (field item "government_departments" :text)
        "note" (one-line (field item "note" :text))))

(defun commendation-object (item)
  (bolt "date" (date-of (field item "date_of_commendation" :text))
        "title" (one-line (field item "title" :text))
        "category" (field item "category" :text)
        "target" (field item "target" :text)
        "ministry" (field item "government_departments" :text)
        "note" (one-line (field item "note" :text))))

(defun certification-object (item)
  (bolt "date" (date-of (field item "date_of_approval" :text))
        "title" (one-line (field item "title" :text))
        "category" (field item "category" :text)
        "target" (field item "target" :text)
        "ministry" (field item "government_departments" :text)))

(defparameter +finance-fields+
  '(("net_sales_jpy" . "net_sales_summary_of_business_results")
    ("operating_revenue_jpy" . "operating_revenue1_summary_of_business_results")
    ("operating_receipts_jpy" . "operating_revenue2_summary_of_business_results")
    ("gross_operating_revenue_jpy" . "gross_operating_revenue_summary_of_business_results")
    ("ordinary_revenue_jpy" . "ordinary_income_summary_of_business_results")
    ("ordinary_profit_jpy" . "ordinary_income_loss_summary_of_business_results")
    ("net_income_jpy" . "net_income_loss_summary_of_business_results")
    ("net_premiums_written_jpy" . "net_premiums_written_summary_of_business_results_ins")
    ("total_assets_jpy" . "total_assets_summary_of_business_results")
    ("net_assets_jpy" . "net_assets_summary_of_business_results")
    ("capital_stock_jpy" . "capital_stock_summary_of_business_results")
    ("employees" . "number_of_employees"))
  "The securities report's figures, under the names this cell answers.")

(defun finance-year (row)
  "One year of a securities report's management index."
  (apply #'bolt "years_before_latest" (integer-of (field row "period"))
         (loop for (name . key) in +finance-fields+
               append (list name (integer-of (field row key))))))

(defun finance-object (finance max-items)
  "Sales, profit, assets and the major shareholders of a filing company."
  (if (or (null finance) (zerop (hash-table-count finance)))
      (bolt "total" 0)
      (let* ((years (sort (mapcar #'finance-year (objects (field finance "management_index" :array)))
                          #'< :key (lambda (year) (let ((period (gethash "years_before_latest" year)))
                                                    (if (integerp period) period 99)))))
             (holders (sort (objects (field finance "major_shareholders" :array))
                            #'> :key (lambda (holder)
                                       (let ((ratio (field holder "shareholding_ratio")))
                                         (if (numberp ratio) ratio 0))))))
        (bolt "fiscal_year" (one-line (field finance "fiscal_year_cover_page" :text))
              "accounting_standards" (field finance "accounting_standards" :text)
              "results_by_year" (and years (coerce years 'vector))
              "results_note" (concatenate 'string
                                          "Non-consolidated figures of the filing company alone (提出会社単体, from "
                                          "「提出会社の経営指標等の推移」 in the securities report), not the consolidated "
                                          "group. years_before_latest 0 is the fiscal year above, 1 the year before it, and so on.")
              "major_shareholders"
              (and holders
                   (coerce (loop for holder in (subseq holders 0 (min max-items (length holders)))
                                 for ratio = (field holder "shareholding_ratio")
                                 collect (bolt "name" (field holder "name_major_shareholders" :text)
                                               "share_pct" (and (numberp ratio) (coerce (* 100 ratio) 'single-float))))
                           'vector))))))

(defun workplace-object (workplace)
  "Average age, years of service, overtime and the share of women, from the
workplace survey the registry carries."
  (let ((base (or (field workplace "base_infos" :object) (nlk:json-object)))
        (women (or (field workplace "women_activity_infos" :object) (nlk:json-object)))
        (childcare (or (field workplace "compatibility_of_childcare_and_work" :object) (nlk:json-object))))
    (bolt "average_age" (field base "average_age")
          "average_years_of_service" (field base "average_continuous_service_years")
          "average_years_of_service_male" (field base "average_continuous_service_years_Male")
          "average_years_of_service_female" (field base "average_continuous_service_years_Female")
          "years_of_service_scope" (field base "average_continuous_service_years_type" :text)
          "monthly_overtime_hours" (field base "month_average_predetermined_overtime_hours")
          "female_worker_pct" (field women "female_workers_proportion")
          "female_worker_pct_scope" (field women "female_workers_proportion_type" :text)
          "managers_female" (field women "female_share_of_manager")
          "managers_total" (field women "gender_total_of_manager")
          "officers_female" (field women "female_share_of_officers")
          "officers_total" (field women "gender_total_of_officers")
          "childcare_leave_eligible_male" (field childcare "number_of_paternity_leave")
          "childcare_leave_taken_male" (field childcare "paternity_leave_acquisition_num")
          "childcare_leave_eligible_female" (field childcare "number_of_maternity_leave")
          "childcare_leave_taken_female" (field childcare "maternity_leave_acquisition_num"))))

(defun patents-object (items max-items)
  "Patents, designs and trademarks, one row per registration (the registry
repeats a patent once per classification code)."
  (let ((seen (make-hash-table :test #'equal))
        (rows '()))
    (dolist (item items)
      (let ((key (format nil "~a|~a|~a" (field item "patent_type" :text)
                         (field item "registration_number" :text)
                         (field item "title" :text))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push (bolt "type" (field item "patent_type" :text)
                      "registration_number" (field item "registration_number" :text)
                      "application_date" (date-of (field item "application_date" :text))
                      "title" (one-line (field item "title" :text))
                      "url" (field item "url" :text))
                rows))))
    (listed (nreverse rows) #'identity max-items "application_date")))

(defun office-object (item)
  (bolt "name" (one-line (field item "corporation_name" :text))
        "location" (one-line (field item "corporation_location" :text))
        "insured_employees" (integer-of (field item "insured_number"))
        "closed_on" (date-of (field item "loss_date" :text))))

(defun section-object (section info max-items)
  "One section of the profile, shaped."
  (cond ((string= section "basic") (basic-object info))
        ((string= section "subsidy")
         (listed (objects (field info "subsidy" :array)) #'subsidy-object max-items "date"))
        ((string= section "procurement")
         (listed (objects (field info "procurement" :array)) #'procurement-object max-items "date"))
        ((string= section "commendation")
         (listed (objects (field info "commendation" :array)) #'commendation-object max-items "date"))
        ((string= section "certification")
         (listed (objects (field info "certification" :array)) #'certification-object max-items "date"))
        ((string= section "finance") (finance-object (field info "finance" :object) max-items))
        ((string= section "patent")
         (patents-object (objects (field info "patent" :array)) max-items))
        ((string= section "workplace")
         (workplace-object (or (field info "workplace_info" :object) (nlk:json-object))))
        ((string= section "offices")
         (listed (objects (field info "corporation-info" :array)) #'office-object max-items nil))
        (t (fail "unknown section ~s: choose from ~{~a~^, ~}." section (mapcar #'car +sections+)))))

(defun profile-object (number section-names max-items)
  "One corporation's record, section by section; each section is one request."
  (let ((header nil)
        (out (nlk:json-object)))
    (dolist (section section-names)
      (let* ((path (section-path section))
             (route (if (plusp (length path)) (format nil "~a/~a" number path) number))
             (info (first (objects (field (or (gbiz-get route) (nlk:json-object)) "hojin-infos" :array)))))
        (when (and (null info) (string= section "basic"))
          (return-from profile-object
            (bolt "found" 0
                  "corporate_number" number
                  "message" (format nil "gBizINFO has no corporation with 法人番号 ~a (the number itself is well-formed): check it, or find the company by name with jp_corp_search." number)
                  "attribution" (attribution +site+))))
        (unless header
          (setf header (bolt "name" (field info "name" :text)
                             "location" (field info "location" :text))))
        (setf (gethash section out)
              (if info (section-object section info max-items) (bolt "total" 0)))))
    (bolt "corporate_number" number
          "name" (and header (gethash "name" header))
          "location" (and header (gethash "location" header))
          "gbizinfo_url" (page-url number)
          "message" (and (null header)
                          (concatenate 'string
                                       "gBizINFO returned no data for this number in the requested sections: "
                                       "add 'basic' to sections to confirm the corporation exists."))
          "sections" out
          "attribution" (attribution (page-url number)))))

(defun parse-sections (raw)
  "RAW — a sections array, a list or a separated string — as section names, in order."
  (let ((values (cond ((null raw) '("basic"))
                      ((stringp raw) (loop for start = 0 then (1+ end)
                                           for end = (or (position-if (lambda (character)
                                                                        (member character '(#\, #\Space)))
                                                                      raw :start start)
                                                          (length raw))
                                           for piece = (string-trim '(#\Space #\Tab) (subseq raw start end))
                                           when (plusp (length piece)) collect piece
                                           while (< end (length raw))))
                      ((array-p raw) (coerce raw 'list))
                      ((listp raw) raw)
                      (t (list (princ-to-string raw)))))
        (names '()))
    (dolist (value values)
      (let ((name (string-downcase (string-trim '(#\Space #\Tab) (princ-to-string value)))))
        (unless (assoc name +sections+ :test #'string=)
          (fail "unknown section ~s: choose from ~{~a~^, ~}." value (mapcar #'car +sections+)))
        (unless (member name names :test #'string=) (push name names))))
    (or (nreverse names) '("basic"))))

;;; --- the two tools the model calls -----------------------------------------------

(nlk:define-contract corp-search-call
  (:doc "Find a Japanese corporation in gBizINFO (Japan's Ministry of Economy, Trade and Industry) and
confirm it is registered. Use it to check a business partner, to look up a 法人番号 (corporate number)
from a company name, or to check the number printed on an invoice.
Give name to search (partial match; 株式会社/(株)/㈱, half-width kana and full-width letters are
handled, so '(株)ﾄﾖﾀ自動車' finds トヨタ自動車株式会社); add address (a prefecture and/or city, e.g.
'愛知県豊田市') to tell same-name companies apart. Results are ranked exact match first, registered
before closed.
Give corporate_number (13 digits; an invoice number such as T1180301018771 also works) to look one
corporation up; add name and/or address too and the answer says whether they match the official
record. A malformed number is refused without any request. Answers name, head-office address, status
and the gBizINFO page url; 'registered' does not prove the business is operating, and 'closed' means
the registration was closed (liquidation, merger, ...). It covers corporations only (not sole
proprietors), 0 results is not proof that a company does not exist, and it does not check
qualified-invoice-issuer (適格請求書発行事業者) registration. Costs at most one API request.")
  ("name" :string :doc "the company name or its distinctive part, e.g. 'トヨタ自動車' or '(株)ﾄﾖﾀ自動車'; with corporate_number, the name to verify")
  ("corporate_number" :string :doc "the 13-digit 法人番号, e.g. '1180301018771'; hyphens, spaces, full-width digits and a leading T (invoice number) are accepted")
  ("address" :string :doc "a prefecture and/or municipality to narrow by, e.g. '愛知県豊田市' or 'Tokyo'; with corporate_number, the address to verify")
  ("limit" :integer :default 10 :doc "how many results to list, 1 to 50 (default 10)"))

(nlk:define-contract corp-profile-call
  (:doc "Get gBizINFO's detail for one Japanese corporation by its 13-digit 法人番号 (find it with
jp_corp_search first). Pick only the sections you need; each section is one API request: basic (official
name, kana, status and close reason, address, representative, capital, employees, established date,
industry, website), subsidy (government subsidies received), procurement (government contracts won),
commendation (government awards), certification (registrations such as DX認定 or 健康経営), finance
(sales, profit, assets and major shareholders from securities reports; listed companies mostly), patent
(patents, designs, trademarks), workplace (average age, years of service, overtime, women in management,
childcare leave), offices (business sites in employees' pension and health insurance, with insured
headcount). Lists are newest first with a total count. Representative, capital and headcount figures can
be years old: quote gbizinfo_last_updated with them (freshness_note appears past a year).")
  ("corporate_number" :string :required t :doc "the 13-digit 法人番号, e.g. '4000012090001'; a leading T is accepted")
  ("sections" :array :of :string :doc "basic, subsidy, procurement, commendation, certification, finance, patent, workplace, offices; default basic")
  ("max_items" :integer :default 10 :doc "how many rows of each list section to lay out, 1 to 50 (default 10)"))

(defun corp-search (call)
  "jp_corp_search: one corporation by number, or a name search."
  (let ((name (or (nonempty (corp-search-call-name call)) ""))
        (number (or (nonempty (corp-search-call-corporate-number call)) ""))
        (address (or (nonempty (corp-search-call-address call)) ""))
        (limit (clamp (corp-search-call-limit call) +default-results+ +max-results+)))
    (redacted (render (cond ((plusp (length number)) (lookup-number number name address))
                            ((zerop (length name))
                             (fail "give a company name (name) or a 13-digit 法人番号 (corporate_number)."))
                            (t (search-object name address limit)))))))

(defun corp-profile (call)
  "jp_corp_profile: one corporation's record, section by section."
  (let ((number (parse-corporate-number (corp-profile-call-corporate-number call)))
        (sections (parse-sections (corp-profile-call-sections call)))
        (max-items (clamp (corp-profile-call-max-items call) +default-items+ +max-items+)))
    (redacted (render (profile-object number sections max-items)))))
