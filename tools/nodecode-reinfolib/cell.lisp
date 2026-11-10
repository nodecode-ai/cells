;;;; cell.lisp --- MLIT's real estate information library, one city and one
;;;; quarter at a time.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin jp-reinfolib. One GET to the real estate
;;;; information library of the Ministry of Land, Infrastructure, Transport and
;;;; Tourism (XIT001, transaction prices), projected into a row count, a median
;;;; TradePrice, counts by type and at most twenty rows, with MLIT's
;;;; attribution and its terms sentence kept verbatim.
;;;;
;;;; The subscription key rides one header and never a URL, and the answer is
;;;; scanned for it before it is shown; nothing is written to disk, and the
;;;; only state the cell keeps is the time of its last request, since MLIT asks
;;;; for a second between them.

(defpackage #:nodecode-reinfolib
  (:use #:cl)
  (:nicknames #:reinfolib)
  (:export #:quarter #:report-text #:check-arguments #:*primer*
           #:reinfolib-error))

(in-package #:nodecode-reinfolib)

(nlk:define-peripheral reinfolib :not-running t)

;;; --- the words -------------------------------------------------------------------

(defparameter +primer+
  "reinfolib: MLIT real estate transaction prices -- one Japanese city code, one quarter.
  reinfolib_quarter (tool), or /reinfolib CITY YEAR QUARTER, answers one quarter: the
      row count, the median TradePrice, counts by type, and at most 20 rows.
  (reinfolib:quarter \"13104\" 2015 2)  the same from eval, as a plist.
The city is a 5-digit code, not a name: 13104 is Shinjuku-ku. A designated city needs a
ward code (14101, not 14100). The year starts at 2005, and 2005 starts at quarter 3; a
quarter is 1, 2, 3 or 4, and one that has not started yet is refused.
HTTP 404 is no data for that city, year and quarter: it is not zero rows and it is not
unchanged prices, and publication lags, so the latest started quarter is often 404.
An answer carries MLIT's attribution and its terms sentence: keep both beside any row you
quote. Nothing compares one read with another, so never call a row new and never call a
price unchanged.
A weekly check, with the cron cell: read the latest started quarter and the one before
it, and stay quiet when both are 404. (help :reinfolib) has this while the cell runs."
  "What (help :reinfolib) answers while the cell runs.")

(defparameter +path+ "/ex-api/external/XIT001"
  "The one endpoint, on whatever origin the section names.")

(defparameter +default-origin+ "https://www.reinfolib.mlit.go.jp"
  "MLIT's own host, used when base_url is empty.")

(defparameter +key-header+ "Ocp-Apim-Subscription-Key"
  "The one header the subscription key travels in.")

(defparameter +body-cap+ (expt 2 23)
  "Eight megabytes: an answer larger than this is not one quarter of one city.")

(defparameter +record-cap+ 20
  "How many rows an answer carries, as the plugin's RECORD_CAP did.")

(defparameter +year-min+ 2005
  "Transaction prices start in 2005, at quarter 3.")

(defparameter +text-max+ 200
  "A free-text column (Type, Prefecture, DistrictName) is kept up to this length.")

(defparameter +credit+
  "「このサービスは、国土交通省の不動産情報ライブラリのAPI機能を使用していますが、提供情報の最新性、正確性、完全性等が保証されたものではありません」"
  "MLIT's terms, article 7, uncut: it rides every answer, the plugin's CREDIT.")

(defparameter +attribution+ "出典：国土交通省　不動産情報ライブラリ")

(defparameter +top-url+ "https://www.reinfolib.mlit.go.jp/")

(defparameter +processing+ "「不動産取引価格情報」（国土交通省）をもとに件数と抜粋を作成")

(defparameter +price-category+ "不動産取引価格情報"
  "The one PriceCategory XIT001 answers, and the only one kept.")

(defparameter +designated-cities+
  '("01100" "04100" "11100" "12100" "14100" "14130" "14150" "15100"
    "22100" "22130" "23100" "26100" "27100" "27140" "28100" "33100"
    "34100" "40100" "40130" "43100")
  "The designated cities' own codes, which XIT001 answers nothing for: a ward
code is what it wants, the plugin's DESIGNATED_CITY_CODES.")

(defparameter +columns+
  '(("Type" :text) ("Region" :text) ("MunicipalityCode" :city) ("Prefecture" :text)
    ("Municipality" :text) ("DistrictName" :text) ("TradePrice" :digits)
    ("PricePerUnit" :digits) ("Area" :digits) ("UnitPrice" :digits)
    ("Period" :period) ("PriceCategory" :price-category))
  "The twelve columns an answer carries, in this order: the plugin's COLUMNS.
Every other XIT001 key is dropped.")

(defparameter +error-messages+
  '(("bad_city" . "the city code was refused before any request was sent")
    ("bad_year" . "the year was refused before any request was sent")
    ("bad_quarter" . "the quarter was refused before any request was sent")
    ("future_quarter" . "the quarter has not started yet, so nothing was requested")
    ("bad_price_classification" . "price_classification was refused before any request was sent")
    ("bad_language" . "language was refused before any request was sent")
    ("config" . "the cell is not configured, so nothing was requested")
    ("auth" . "the API refused the subscription key, and this is not an empty result")
    ("rate_limited" . "the server answered HTTP 429, and the request was not retried")
    ("not_found" . "no data for that city, year and quarter (HTTP 404): not zero rows, and not unchanged prices")
    ("redirect" . "the server answered a redirect, which was not followed")
    ("timeout" . "no answer arrived in time, and the request was not retried")
    ("invalid_response" . "the answer was not a usable XIT001 body")
    ("upstream" . "the request failed, and it was not retried")
    ("internal" . "the answer was dropped"))
  "One sentence per error type, the plugin's ERRORS, kept whole.")

;;; --- a failure, and a refusal -----------------------------------------------------

(defun refusal (error-type &key detail status-code field problem expected)
  "A failure plist: what went wrong, and what a caller can do about it."
  (list :ok nil
        :error-type error-type
        :message (or (cdr (assoc error-type +error-messages+ :test #'string=))
                     "the call was refused")
        :detail detail :status-code status-code
        :field field :problem problem :expected expected))

(defun refused (error-type field problem expected)
  "An argument refused before any request, with the repair it takes."
  (refusal error-type :field field :problem problem :expected expected))

;;; --- text, and the shapes an argument may take ------------------------------------

(defun blank-char-p (char)
  "Whether CHAR is whitespace this cell trims."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun trimmed (text)
  (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun fold (text)
  "TEXT with the full-width digits and the ideographic space a Japanese keyboard
produces folded to ASCII, its ends trimmed: what a model mistypes and means."
  (when (stringp text)
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (map 'string
                      (lambda (char)
                        (let ((code (char-code char)))
                          (cond ((<= #xFF10 code #xFF19) (code-char (+ #x30 (- code #xFF10))))
                                ((= code #x3000) #\Space)
                                (t char))))
                      text))))

(defun digit-string-p (text &key (max 24))
  "Whether TEXT is one run of ASCII digits, no longer than MAX."
  (and (stringp text) (plusp (length text)) (<= (length text) max)
       (every (lambda (char) (digit-char-p char)) text)))

(defun city-code-p (text)
  "Whether TEXT is a 5-digit municipal code: the plugin's CHECK-CITY."
  (and (stringp text) (= 5 (length text)) (digit-string-p text)
       (<= 1 (parse-integer text :end 2) 47)))

(defun designated-city-p (code)
  (and (stringp code) (member code +designated-cities+ :test #'string=) t))

(defun today ()
  "The local date as two values, the year and the month."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour date))
    (values year month)))

(defun quarter-started-p (year quarter this-year this-month)
  "Whether QUARTER of YEAR has begun, given today's YEAR and MONTH: the plugin's
QUARTER-STARTED, with 2005 starting at quarter 3."
  (let ((first-month (+ (* 3 (1- quarter)) 1)))
    (and (not (and (= year +year-min+) (< quarter 3)))
         (not (and (= year this-year) (> first-month this-month))))))

(defun period-name (year quarter)
  "The period string an XIT001 row must carry for YEAR and QUARTER."
  (format nil "~d年第~d四半期" year quarter))

(defun check-arguments (city year quarter &key (price-classification "01") (language "ja"))
  "=> (VALUES PARSED PROBLEM): the four arguments normalized, or the failure that
refused them. The order is the plugin's VALIDATE-ARGUMENTS: the city's shape
first, then the year, the quarter, the classification and the language, then the
two rules that need all of them -- 2005 starts at quarter 3, and a quarter that
has not begun is not readable."
  (let ((city (fold city))
        (price (fold price-classification))
        (language (fold language)))
    (multiple-value-bind (this-year this-month) (today)
      (cond
        ((designated-city-p city)
         (values nil (refused "bad_city" "city" "designated_city"
                              "Use a ward code, not the city code: 14101 is Tsurumi-ku, Yokohama. 区のコードを使う（例: 14101 = 横浜市鶴見区）。")))
        ((not (city-code-p city))
         (values nil (refused "bad_city" "city" "shape"
                              "5 ASCII digits for a ward, or for a city that is not a designated city, a town or a village: 13104 is Shinjuku-ku. A municipality name is not a code.")))
        ((not (and (integerp year) (<= +year-min+ year this-year)))
         (values nil (refused "bad_year" "year" "range"
                              "An integer from 2005 through the current year. 2005 starts at quarter 3.")))
        ((not (and (integerp quarter) (<= 1 quarter 4)))
         (values nil (refused "bad_quarter" "quarter" "range"
                              "An integer 1, 2, 3 or 4. For 2005, only 3 and 4.")))
        ((not (string= price "01"))
         (values nil (refused "bad_price_classification" "price_classification" "value"
                              "The only accepted value is 01.")))
        ((not (string= (string-downcase language) "ja"))
         (values nil (refused "bad_language" "language" "value"
                              "The only accepted value is ja.")))
        ((and (= year +year-min+) (< quarter 3))
         (values nil (refused "bad_quarter" "quarter" "range"
                              "An integer 1, 2, 3 or 4. For 2005, only 3 and 4.")))
        ((not (quarter-started-p year quarter this-year this-month))
         (values nil (refused "future_quarter" "quarter" "not_started"
                              "A quarter whose first month is not after today's month.")))
        (t (values (list :city city :year year :quarter quarter
                         :price-classification "01" :language "ja")
                   nil))))))

;;; --- the origin, and the key the section holds ------------------------------------

(defun nonblank (text)
  (let ((text (trimmed text)))
    (and text (plusp (length text)) text)))

(defun origin-problem (detail)
  "The failure that refuses a base_url the cell cannot use."
  (refusal "config" :detail detail))

(defun parse-origin (text)
  "=> (VALUES ORIGIN PROBLEM): the scheme://host[:port] a request goes to, or the
failure that refused TEXT. The plugin's PARSE-ORIGIN: http or https, a host and
an optional port, and nothing else -- a path, a query, a fragment, a user or a
password is refused, and plain http only for loopback. One trailing slash is
accepted and dropped."
  (let* ((text (if (and (stringp text) (plusp (length text))) text +default-origin+))
         (text (if (and (plusp (length text)) (char= (char text (1- (length text))) #\/))
                   (subseq text 0 (1- (length text)))
                   text)))
    (cond
      ((not (string= text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
       (values nil (origin-problem "bad_origin")))
      ((find-if #'blank-char-p text) (values nil (origin-problem "bad_origin")))
      ((find #\\ text) (values nil (origin-problem "bad_origin")))
      (t
       (let* ((mark (search "://" text))
              (scheme (and mark (string-downcase (subseq text 0 mark))))
              (rest (and mark (subseq text (+ 3 mark))))
              (end (and rest (or (position-if (lambda (char) (find char "/?#" :test #'char=)) rest)
                                 (length rest))))
              (authority (and rest (subseq rest 0 end)))
              (tail (and rest (subseq rest end))))
         (cond
           ((not (member scheme '("http" "https") :test #'string=))
            (values nil (origin-problem "bad_origin")))
           ((not (and authority (plusp (length authority))))
            (values nil (origin-problem "bad_origin")))
           ((find #\@ authority) (values nil (origin-problem "bad_origin")))
           ((not (zerop (length tail))) (values nil (origin-problem "bad_origin")))
           (t
            (let* ((colon (position #\: authority :from-end t))
                   (host (string-downcase (if colon (subseq authority 0 colon) authority))))
              (cond
                ((zerop (length host)) (values nil (origin-problem "bad_origin")))
                ((and (string= scheme "http")
                      (not (member host '("127.0.0.1" "localhost") :test #'string=)))
                 (values nil (origin-problem "plain_http")))
                (t (values (format nil "~a://~a" scheme authority) nil)))))))))))

(defun read-settings (values table)
  "The declared VALUES with the key and the origin resolved once, here. A section
with no key, with both a key and the name of one, with whitespace inside the key,
or a base_url that is not an origin, refuses at start: the plugin's own refusal
in Client.__init__ and parse_origin, in the same words it used."
  (declare (ignore table))
  (let* ((inline (nonblank (getf values :api-key)))
         (env (nonblank (getf values :api-key-env)))
         (named (and env (nonblank (nle::credential-env env))))
         (plist (copy-list values)))
    (when (and inline env)
      (nlk:config-error "set one of api_key and api_key_env in the reinfolib section, not both"))
    (let ((key (or inline named)))
      (unless key
        (nlk:config-error "no MLIT subscription key: set api_key in the reinfolib section, or api_key_env naming the variable that holds it. A key is free at https://www.reinfolib.mlit.go.jp/"))
      (when (find-if #'blank-char-p (string key))
        (nlk:config-error "the subscription key has whitespace in it: paste the key alone, with no spaces, tabs or newlines"))
      (setf (getf plist :api-key) key))
    (multiple-value-bind (origin problem) (parse-origin (getf values :base-url))
      (unless origin
        (nlk:config-error "base_url ~s is not an origin: give the scheme and the host only, e.g. https://www.reinfolib.mlit.go.jp (~a)"
                          (getf values :base-url) (getf problem :detail)))
      (setf (getf plist :origin) origin))
    plist))

;;; --- the wire: one GET, and the gap MLIT asks for ---------------------------------

(defvar *last-request* nil
  "The internal real time of this process's last outbound GET, or NIL.")

(defvar *gap-seconds* 1
  "The least time between two outbound GETs, which the plugin keeps at one
second. A test binds 0.")

(defun wait-gap ()
  "Sleep until at least *GAP-SECONDS* have passed since the last request."
  (when (and *last-request* (plusp *gap-seconds*))
    (let ((wait (- (+ *last-request* (* *gap-seconds* internal-time-units-per-second))
                   (get-internal-real-time))))
      (when (plusp wait)
        (sleep (/ wait internal-time-units-per-second 1.0))))))

(defun fetch (url timeout)
  "URL through one GET => (VALUES TEXT STATUS TRUNCATED). The key rides one
header; the body is read under the plugin's own eight-megabyte cap."
  (multiple-value-bind (body status headers truncated)
      (nlk:http :get url
                :headers (list (cons "accept" "application/json")
                               (cons +key-header+ (setting :api-key)))
                :timeout timeout
                :connect-timeout (min 10 timeout)
                :max-bytes +body-cap+)
    (declare (ignore headers))
    (values (if (stringp body) body (nlk:body-text body)) status truncated)))

(defun get-text (url timeout)
  "URL read once => (VALUES TEXT STATUS TRUNCATED PROBLEM), PROBLEM naming a
transport failure the way the plugin did: a deadline is a timeout, anything else
the network threw is upstream."
  (handler-case
      (multiple-value-bind (text status truncated) (fetch url timeout)
        (values text status truncated nil))
    (sb-sys:deadline-timeout () (values "" nil nil "timeout"))
    (error () (values "" nil nil "upstream"))))

(defun query-url (origin parsed)
  "The one URL: the origin, the one path, and the five parameters the plugin sent."
  (format nil "~a~a?year=~d&quarter=~d&city=~a&priceClassification=01&language=ja"
          origin +path+ (getf parsed :year) (getf parsed :quarter) (getf parsed :city)))

;;; --- the body, projected onto the twelve columns ----------------------------------

(defun control-or-format-p (char)
  "Whether CHAR is a control or a format character, which a text column refuses."
  (let ((code (char-code char)))
    (or (< code 32) (= code 127)
        (<= #x200B code #x200F) (<= #x202A code #x202E) (= code #xFEFF))))

(defun text-column-p (value)
  (and (stringp value) (<= (length value) +text-max+)
       (notany #'control-or-format-p value)))

(defun project-value (type value parsed)
  "=> (VALUES KEPT DETAIL): one column of one row. A value that is not what the
column wants is marked and kept; a Period or a PriceCategory that does not match
the request drops the whole body, as the plugin's PROJECT-BODY did."
  (case type
    (:period (if (equal value (period-name (getf parsed :year) (getf parsed :quarter)))
                 (values value nil)
                 (values nil "mismatched_record")))
    (:price-category (if (equal value +price-category+)
                         (values value nil)
                         (values nil "mismatched_record")))
    (:city (cond ((and (city-code-p value) (equal value (getf parsed :city)))
                  (values value nil))
                 ((city-code-p value) (values (list :other-municipality value) nil))
                 (t (values (list :not-code) nil))))
    (:digits (if (and (stringp value)
                      (or (zerop (length value)) (digit-string-p value)))
                 (values value nil)
                 (values (list :not-digits) nil)))
    (t (if (text-column-p value)
           (values value nil)
           (values (list :not-text) nil)))))

(defun project-record (item parsed)
  "=> (VALUES ROW DETAIL): ITEM's twelve columns as an alist in +COLUMNS+ order."
  (let ((row '()))
    (dolist (column +columns+ (values (nreverse row) nil))
      (multiple-value-bind (kept detail)
          (project-value (second column) (nlk:json-value item :any (first column)) parsed)
        (when detail
          (return-from project-record (values nil detail)))
        (push (cons (first column) kept) row)))))

(defun project-body (text parsed)
  "=> (VALUES RECORDS DETAIL): the twelve columns of every row TEXT carries."
  (let ((body (ignore-errors (nlk:decode-json (trimmed text)))))
    (cond
      ((not (hash-table-p body)) (values nil "not_json"))
      ((nth-value 1 (nlk:json-value body :any "statusCode")) (values nil "not_ok"))
      ((not (equal "OK" (nlk:json-value body :string "status"))) (values nil "not_ok"))
      ((not (vectorp (nlk:json-value body :array "data"))) (values nil "not_object"))
      (t
       (let ((data (nlk:json-value body :array "data"))
             (rows '()))
         (when (zerop (length data))
           (return-from project-body (values nil "empty_data")))
         (dotimes (index (length data))
           (let ((item (aref data index)))
             (unless (hash-table-p item)
               (return-from project-body (values nil "bad_record")))
             (multiple-value-bind (row detail) (project-record item parsed)
               (when detail
                 (return-from project-body (values nil detail)))
               (push row rows))))
         (values (nreverse rows) nil))))))

;;; --- the count, the medians, and the types ---------------------------------------

(defun record-field (row label)
  "ROW's LABEL, or NIL."
  (cdr (assoc label row :test #'string=)))

(defun record-text (row label)
  "ROW's LABEL when it is a string, else the empty string."
  (let ((value (record-field row label)))
    (if (stringp value) value "")))

(defun record-price (row)
  "ROW's TradePrice as an integer, or NIL when it is absent or not digits."
  (let ((price (record-field row "TradePrice")))
    (and (stringp price) (plusp (length price)) (digit-string-p price)
         (parse-integer price))))

(defun median-string (numbers)
  "NUMBERS' middle, or the mean of the two middles with its .5 half: the
plugin's TRADE-MEDIAN, which answers a string and never a float."
  (when numbers
    (let* ((sorted (sort (copy-list numbers) #'<))
           (total (length sorted))
           (middle (floor total 2)))
      (if (oddp total)
          (format nil "~d" (nth middle sorted))
          (let ((sum (+ (nth (1- middle) sorted) (nth middle sorted))))
            (if (evenp sum)
                (format nil "~d" (floor sum 2))
                (format nil "~d.5" (floor sum 2))))))))

(defun by-type (records)
  "RECORDS grouped by Type => ((TYPE . ROWS) ...), a type's first appearance first."
  (let ((groups '()))
    (dolist (row records (nreverse groups))
      (let ((kind (record-field row "Type")))
        (when (stringp kind)
          (let ((group (assoc kind groups :test #'string=)))
            (if group
                (push row (cdr group))
                (push (list kind row) groups))))))))

(defun count-by-type (records)
  "One (TYPE . COUNT) per type."
  (loop for (kind . rows) in (by-type records) collect (cons kind (length rows))))

(defun median-by-type (records)
  "One (TYPE . MEDIAN) per type that has a digit TradePrice."
  (loop for (kind . rows) in (by-type records)
        for median = (median-string (remove nil (mapcar #'record-price rows)))
        when median collect (cons kind median)))

(defun utc-stamp ()
  "Now as the ISO 8601 UTC stamp the plugin put in fetched_at."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour minute second)))

(defun success (parsed records)
  "The answer: the count, the medians and at most +RECORD-CAP+ rows."
  (list :ok t
        :city (getf parsed :city)
        :year (getf parsed :year)
        :quarter (getf parsed :quarter)
        :period (period-name (getf parsed :year) (getf parsed :quarter))
        :price-classification (getf parsed :price-classification)
        :fetched-at (utc-stamp)
        :count (length records)
        :truncated (> (length records) +record-cap+)
        :records (subseq records 0 (min +record-cap+ (length records)))
        :count-by-type (count-by-type records)
        :median (median-string (remove nil (mapcar #'record-price records)))
        :median-by-type (median-by-type records)))

;;; --- the key, kept out of the answer ---------------------------------------------

(defun hex-digit-value (char)
  (and char (digit-char-p char 16)))

(defun percent-decode (text)
  "TEXT with its %XX runs read back as UTF-8."
  (if (not (find #\% text))
      text
      (let ((octets (sb-ext:string-to-octets text :external-format :utf-8))
            (out (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
        (loop with index = 0 with length = (length octets)
              while (< index length)
              do (let ((byte (aref octets index)))
                   (if (and (= byte 37) (< (+ index 2) length)
                            (hex-digit-value (code-char (aref octets (1+ index))))
                            (hex-digit-value (code-char (aref octets (+ index 2)))))
                       (progn
                         (vector-push-extend (+ (* 16 (hex-digit-value (code-char (aref octets (1+ index)))))
                                                (hex-digit-value (code-char (aref octets (+ index 2)))))
                                              out)
                         (incf index 3))
                       (progn (vector-push-extend byte out) (incf index)))))
        (sb-ext:octets-to-string (coerce out '(simple-array (unsigned-byte 8) (*)))
                                 :external-format '(:utf-8 :replacement #\?)))))

(defun unicode-unescape (text)
  "TEXT with its \\uXXXX sequences read as characters."
  (if (not (search "\\u" text))
      text
      (with-output-to-string (out)
        (loop with index = 0 with length = (length text)
              while (< index length)
              do (if (and (char= (char text index) #\\) (< (+ index 5) length)
                          (char= (char text (1+ index)) #\u)
                          (every (lambda (offset) (hex-digit-value (char text (+ index offset))))
                                 '(2 3 4 5)))
                     (progn
                       (write-char (code-char (parse-integer text :start (+ index 2)
                                                                   :end (+ index 6) :radix 16))
                                   out)
                       (incf index 6))
                     (progn (write-char (char text index) out) (incf index)))))))

(defun leak-forms (text)
  "TEXT and the forms one decode of it yields, which is where a key would show."
  (let ((percent (ignore-errors (percent-decode text)))
        (escaped (ignore-errors (unicode-unescape text))))
    (remove-duplicates
     (remove nil (list text percent escaped
                       (and percent (ignore-errors (unicode-unescape percent)))
                       (and escaped (ignore-errors (percent-decode escaped)))))
     :test #'equal)))

(defun spills-key-p (text key)
  "Whether KEY, or any 12-character span of it, survives one decode in TEXT."
  (and (stringp text) (stringp key) (plusp (length key))
       (let ((spans (if (>= (length key) 12)
                        (loop for start from 0 to (- (length key) 12)
                              collect (subseq key start (+ start 12)))
                        (list key))))
         (some (lambda (form)
                 (some (lambda (span) (search span form)) spans))
               (leak-forms text)))))

(defun seal (result text key)
  "RESULT, or the internal failure that drops an answer TEXT carried the key in."
  (if (spills-key-p text key) (refusal "internal" :detail "secret") result))

;;; --- one call ---------------------------------------------------------------------

(defun request (parsed)
  "PARSED through one GET => the answer, or the failure that stands in for it."
  (let* ((origin (setting :origin))
         (key (setting :api-key))
         (url (query-url origin parsed)))
    (when (and (plusp (length key)) (search key url))
      (return-from request (refusal "config" :detail "key_in_url")))
    (wait-gap)
    (multiple-value-bind (text status truncated problem)
        (get-text url (setting :timeout-seconds))
      (setf *last-request* (get-internal-real-time))
      (seal (cond
              (problem (refusal problem :detail (if (string= problem "timeout") "timeout" "network")))
              ((and (integerp status) (<= 300 status 399)) (refusal "redirect" :status-code status))
              ((eql status 404) (refusal "not_found" :status-code 404))
              ((member status '(401 403)) (refusal "auth" :status-code status))
              ((eql status 429) (refusal "rate_limited" :status-code 429))
              ((not (eql status 200)) (refusal "upstream" :status-code status))
              (truncated (refusal "invalid_response" :detail "too_big" :status-code 200))
              (t (multiple-value-bind (records detail) (project-body text parsed)
                   (if records
                       (success parsed records)
                       (refusal "invalid_response" :detail detail :status-code 200)))))
            text key))))

(defun quarter (city year quarter-number &key (price-classification "01") (language "ja"))
  "One quarter's transaction prices as a result plist, for eval and the tool:
(reinfolib:quarter \"13104\" 2015 2). An argument that is refused answers a
failure plist."
  (multiple-value-bind (parsed problem)
      (check-arguments city year quarter-number
                        :price-classification price-classification :language language)
    (if problem problem (request parsed))))

;;; --- the words an answer comes back as --------------------------------------------

(defun render-value (value)
  "VALUE as text: a string whole, a marker in brackets."
  (cond ((stringp value) value)
        ((and (consp value) (eq (first value) :other-municipality))
         (format nil "[other municipality ~a]" (second value)))
        ((equal value '(:not-code)) "[not a code]")
        ((equal value '(:not-digits)) "[not digits]")
        ((equal value '(:not-text)) "[not text]")
        (t "?")))

(defun record-line (row)
  "One row as text: its type, where it is, and the numbers that are set."
  (let ((where (format nil "~{~a~^ ~}"
                        (remove "" (list (record-text row "Prefecture")
                                         (record-text row "Municipality")
                                         (record-text row "DistrictName"))
                                :test #'equal)))
        (numbers (loop for label in '("TradePrice" "PricePerUnit" "Area" "UnitPrice")
                       for value = (record-field row label)
                       when (and value (not (equal value "")))
                         collect (format nil "~a ~a" label (render-value value)))))
    (format nil "~a~@[ · ~a~]~{ · ~a~}"
            (render-value (record-field row "Type"))
            (and (plusp (length where)) where)
            numbers)))

(defun type-line (result)
  "The one by-type line, or NIL when no row carries a type."
  (let ((counts (getf result :count-by-type)))
    (when counts
      (format nil "by type: ~{~a~^ · ~}"
              (loop for (kind . number) in counts
                    for median = (cdr (assoc kind (getf result :median-by-type) :test #'string=))
                    collect (format nil "~a ~d~@[ (median ~a)~]" kind number median))))))

(defun success-text (result)
  "RESULT as the model and the operator read it."
  (with-output-to-string (out)
    (format out "~a · ~a · ~d row~:p~@[, median TradePrice ~a~]~%"
            (getf result :city) (getf result :period) (getf result :count)
            (getf result :median))
    (let ((types (type-line result)))
      (when types (format out "~a~%" types)))
    (loop for row in (getf result :records)
          for index from 1
          do (format out "  ~d. ~a~%" index (record-line row)))
    (when (getf result :truncated)
      (format out "  (~d rows, the first ~d returned)~%" (getf result :count) +record-cap+))
    (format out "~@[fetched ~a · ~]~a · ~a~%~a~%~a~%"
            (getf result :fetched-at) +attribution+ +top-url+ +processing+ +credit+)))

(defun failure-text (result)
  "RESULT, a failure, as text: what happened, and the repair it takes."
  (with-output-to-string (out)
    (format out "reinfolib: ~a" (getf result :message))
    (when (getf result :field)
      (format out "~%  ~a (~a): ~a" (getf result :field) (getf result :problem)
              (getf result :expected)))
    (when (getf result :detail)
      (format out "~%  detail: ~a" (getf result :detail)))
    (when (getf result :status-code)
      (format out "~%  HTTP ~a" (getf result :status-code)))
    (format out "~%  ~a" +credit+)))

(defun report-text (result)
  "RESULT as text: the answer, or the failure that stands in for it."
  (if (getf result :ok) (success-text result) (failure-text result)))

(defun answer-text (result)
  "RESULT as a call answers it: a failed read is marked as the call's failure, a
404 no-data is the plain fact the plugin insists it is."
  (let ((text (report-text result)))
    (if (or (getf result :ok) (equal "not_found" (getf result :error-type)))
        text
        (nle:failure text))))

(defmacro answering (&body body)
  "BODY's text; a refusal or a dead network becomes that text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (reinfolib-error (condition)
       (nle:failure (format nil "ERROR: reinfolib: ~a" (reinfolib-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: reinfolib: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- the tool and the command -----------------------------------------------------

(nlk:define-contract quarter-call
  (:doc "One quarter of MLIT real estate transaction prices for one Japanese city code:
the row count, the median TradePrice, counts by type and at most 20 rows. HTTP 404 means
no data for that city, year and quarter, not zero rows and not unchanged prices.")
  ("city" :text :required t
          :doc "5-digit code for a ward, or for a city that is not a designated city: 13104 is Shinjuku-ku. Not a municipality name; a designated city needs a ward code (14101, not 14100).")
  ("year" :integer :required t
          :doc "calendar year, from 2005 through the current year; transaction prices start at 2005 quarter 3")
  ("quarter" :integer :required t
          :doc "1, 2, 3 or 4; a quarter that has not started yet is refused")
  ("price_classification" :text :default "01"
                          :doc "optional; 01, transaction prices, is the only accepted value")
  ("language" :text :default "ja" :doc "optional; ja is the only accepted value"))

(defun quarter-tool (call)
  "The reinfolib_quarter tool: one city, one quarter, answered as text."
  (answering
    (answer-text (quarter (quarter-call-city call) (quarter-call-year call)
                          (quarter-call-quarter call)
                          :price-classification (quarter-call-price-classification call)
                          :language (quarter-call-language call)))))

(defun split-words (text)
  "TEXT split at whitespace."
  (let ((words '())
        (start nil))
    (loop for index from 0 to (length text)
          do (let ((char (and (< index (length text)) (char text index))))
               (cond ((or (null char) (blank-char-p char))
                      (when start
                        (push (subseq text start index) words)
                        (setf start nil)))
                     ((null start) (setf start index)))))
    (nreverse words)))

(defun token-integer (text)
  "TEXT as an integer when it is one run of digits, else TEXT, which the
argument checks then refuse by name."
  (let ((text (fold text)))
    (if (and (stringp text) (digit-string-p text :max 6)) (parse-integer text) text)))

(defparameter +usage+
  "Usage: /reinfolib CITY YEAR QUARTER, e.g. /reinfolib 13104 2015 2 -- a 5-digit city code, not a municipality name.")

(defun run-command (args session-id)
  "/reinfolib CITY YEAR QUARTER: one quarter's prices, as the tool answers them."
  (declare (ignore session-id))
  (let ((words (split-words (or args ""))))
    (if (/= 3 (length words))
        (values (format nil "~a (~d given)" +usage+ (length words)) nil)
        (answering
          (answer-text (quarter (first words)
                                (token-integer (second words))
                                (token-integer (third words))))))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell reinfolib
  (:section ("reinfolib")
    (:guide "get a free MLIT subscription key at https://www.reinfolib.mlit.go.jp/ (API usage), then set api_key, or api_key_env naming the variable that holds it; base_url stays empty for MLIT's own host; next step: /reinfolib 13104 2015 2")
    ("api_key" :secret
               :doc "the MLIT subscription key; it rides the Ocp-Apim-Subscription-Key header and nothing else")
    ("api_key_env" :env
                   :doc "the environment variable holding that key, when it is not written here; set one, not both")
    ("base_url" :string :default ""
                :doc "the origin a request goes to, scheme and host only, e.g. https://www.reinfolib.mlit.go.jp; empty uses that host")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "how long one request may take, in seconds"))
  (:settings #'read-settings)
  (:tool "reinfolib_quarter" (nlk:contract 'quarter-call) #'quarter-tool)
  (:command "reinfolib" #'run-command
            :catches 'reinfolib-error
            :description "MLIT transaction prices for one city code and one quarter"
            :argument-hint "CITY YEAR QUARTER")
  (:help :reinfolib
         "reinfolib: MLIT quarterly real estate transaction prices for one Japanese city code - /reinfolib 13104 2015 2"
         +primer+)
  (:start (lambda () (setf *last-request* nil))))
