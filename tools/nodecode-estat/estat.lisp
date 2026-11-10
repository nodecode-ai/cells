;;;; estat.lisp --- e-Stat's API, its table metadata, the name-to-code
;;;; lookup, and the blocks one call answers with.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The peer is https://api.e-stat.go.jp and nothing else: three endpoints
;;;; of the same REST API (getStatsList, getMetaInfo, getStatsData) and one
;;;; query parameter, appId, which e-Stat itself requires. The application
;;;; ID is scrubbed from every message that leaves this file. Nothing is
;;;; written, no subprocess runs, and the only state is the metadata cache
;;;; below. CELL.LISP is the only caller.

(in-package #:nodecode-estat)

;;; --- what it talks to ---------------------------------------------------------

(defparameter +api-host+ "api.e-stat.go.jp")

(defparameter +api-base+ "https://api.e-stat.go.jp/rest/3.0/app/json/"
  "The one base URL: e-Stat's REST API, version 3, JSON.")

(defparameter +table-url+ "https://www.e-stat.go.jp/dbview?sid=~a"
  "The human page for one table id, quoted beside every figure.")

(defparameter +user-agent+ "nodecode-estat/0.1.0 (+https://github.com/nodecode-ai/cells)")

(defparameter +max-response-bytes+ (* 16 1024 1024)
  "The most one answer may be: a 500-row table with its metadata.")

(defparameter +credit+ (concatenate 'string
                                    "このサービスは、政府統計総合窓口(e-Stat)のAPI機能を使用していますが、"
                                    "サービスの内容は国によって保証されたものではありません。")
  "The credit e-Stat's terms ask every service built on its API to show.")

(defparameter +meta-ttl-seconds+ 1800
  "How long one table's metadata is kept: CPI metadata is ~150 KB, and every
figure needs it to turn names into codes.")

(defparameter +meta-cache-size+ 16)

(defvar *meta-cache* (make-hash-table :test #'equal)
  "table id -> (universal-time . metadata root). In memory; nothing on disk.")

(defun clear-meta-cache ()
  "Forget every cached table (start does this, so a restart re-reads)."
  (clrhash *meta-cache*))

;;; --- json, read leniently -----------------------------------------------------

(defun jget (object key)
  "OBJECT's member KEY when OBJECT is a JSON object, else NIL."
  (and (hash-table-p object) (gethash key object)))

(defun jtext (object key &optional (default ""))
  "OBJECT's member KEY as text: DEFAULT when absent, null or not text."
  (let ((value (jget object key)))
    (if (stringp value) value default)))

(defun jint (object key &optional (default 0))
  "OBJECT's member KEY as an integer: DEFAULT when absent or not numeric."
  (let ((value (jget object key)))
    (cond ((integerp value) value)
          ((numberp value) (round value))
          ((stringp value) (or (parse-integer value :junk-allowed t) default))
          (t default))))

(defun jlist (value)
  "VALUE as a list of JSON values. e-Stat writes one-member arrays as the
member alone, so an object reads as a list of one."
  (cond ((vectorp value) (coerce value 'list))
        ((hash-table-p value) (list value))
        ((consp value) value)
        (t '())))

(defun first-json-value (object)
  "OBJECT's one value: the answer names its own root (GET_STATS_LIST, ...)."
  (and (hash-table-p object)
       (loop for value being the hash-values of object return value)))

(defun label-of (value)
  "A table header field, which e-Stat writes either as text or as an object
carrying it under $."
  (cond ((stringp value) value)
        ((hash-table-p value) (jtext value "$"))
        (t "")))

;;; --- the one network call ----------------------------------------------------

(defun http (url timeout)
  "The cell's one network call: NLK:HTTP GET of URL. Answers (values BODY
STATUS). Tests stub this name and never touch the network."
  (nlk:http :get url
            :headers (list (cons "accept" "application/json")
                           (cons "user-agent" +user-agent+))
            :timeout timeout
            :max-bytes +max-response-bytes+))

(defun query-string (params)
  "PARAMS, an alist of (name . value), as an encoded query string."
  (format nil "~{~a=~a~^&~}"
          (loop for (name . value) in params
                append (list (quri:url-encode (string name))
                             (quri:url-encode (princ-to-string value) :encoding :utf-8)))))

(defun replace-all (string part replacement)
  "STRING with every PART replaced by REPLACEMENT."
  (if (or (null part) (zerop (length part)))
      string
      (with-output-to-string (out)
        (loop with part-length = (length part)
              for old-pos = 0 then (+ pos part-length)
              for pos = (search part string :start2 old-pos)
              do (write-string string out :start old-pos :end (or pos (length string)))
              when pos do (write-string replacement out)
              while pos))))

(defun scrub (text app-id)
  "TEXT with the application ID and any URL taken out, so a message can be
shown without handing back the one secret in the request."
  (let ((text (or text "")))
    (when (and app-id (plusp (length app-id)))
      (setf text (replace-all text app-id "***")))
    (if (search "http" text) (replace-all text "appId" "appId") text)))

(defun check-result (head app-id)
  "HEAD's RESULT status, refused as a condition when e-Stat rejected the
call; HEAD itself otherwise (status 1, no matching data, is not a failure)."
  (let* ((result (jget head "RESULT"))
         (status (jint result "STATUS" 0))
         (message (scrub (jtext result "ERROR_MSG" "") app-id)))
    (cond ((= status 100)
           (fail "e-Stat rejected the application ID in the estat section (status 100~@[: ~a~]);~%check that the whole ID was copied from the e-Stat My Page" message))
          ((and (>= status 200) (< status 300))
           (fail "e-Stat answered status ~d~@[: ~a~]; try again in a minute" status message))
          ((= status 300)
           (fail "e-Stat has no such table (status 300~@[: ~a~])" message)))
    head))

(defun call-api (endpoint params app-id timeout)
  "One e-Stat endpoint, its body under the answer's root key."
  (let* ((params (delete-if (lambda (pair) (or (null (cdr pair)) (equal (cdr pair) "")))
                            (copy-list params)))
         (url (format nil "~a~a?~a" +api-base+ endpoint
                      (query-string (cons (cons "appId" (or app-id "")) params)))))
    (multiple-value-bind (body status) (http url timeout)
      (unless (eql status 200)
        (fail "~a answered HTTP ~a" +api-host+ status))
      (let ((json (ignore-errors
                    (nlk:decode-json (if (stringp body) body (nlk:decode-octets body))))))
        (unless (hash-table-p json)
          (fail "~a answered something that is not JSON (a maintenance page?)" +api-host+))
        (check-result (first-json-value json) app-id)))))

;;; --- one table's metadata ----------------------------------------------------

(defstruct (item (:copier nil))
  code name level unit)

(defstruct (dim (:copier nil))
  id name items)

(defun item-of (object)
  (make-item :code (jtext object "@code")
             :name (jtext object "@name")
             :level (let ((value (jget object "@level")))
                      (if (or (null value) (equal value ""))
                          nil
                          (ignore-errors (parse-integer (princ-to-string value)))))
             :unit (jtext object "@unit")))

(defun meta-parts (root)
  "ROOT's table header and its dimensions."
  (let* ((metadata (jget root "METADATA_INF"))
         (table (jget metadata "TABLE_INF"))
         (classes (jlist (jget (jget metadata "CLASS_INF") "CLASS_OBJ"))))
    (values table
            (loop for class in classes
                  collect (make-dim :id (jtext class "@id")
                                    :name (jtext class "@name")
                                    :items (mapcar #'item-of (jlist (jget class "CLASS"))))))))

(defun table-meta (table-id app-id timeout)
  "TABLE-ID's metadata root, from the cache when it is fresh. => (values
TABLE dims), where DIMS is a list of DIM."
  (let ((hit (gethash table-id *meta-cache*)))
    (when (and hit (> (- (get-universal-time) (car hit)) +meta-ttl-seconds+))
      (remhash table-id *meta-cache*)
      (setf hit nil))
    (let ((root (or (cdr hit)
                    (let ((fresh (call-api "getMetaInfo"
                                           (list (cons "statsDataId" table-id)
                                                 (cons "explanationGetFlg" "N"))
                                           app-id timeout)))
                      (when (> (hash-table-count *meta-cache*) +meta-cache-size+)
                        (clrhash *meta-cache*))
                      (setf (gethash table-id *meta-cache*)
                            (cons (get-universal-time) fresh))
                      fresh))))
      (meta-parts root))))

;;; --- names to codes ----------------------------------------------------------

(defun fold (text)
  "TEXT as it is compared here: lowercased, its spaces (half and full
width) dropped, so 東京都 and ' 東京都 ' read alike."
  (string-downcase
   (remove-if (lambda (char) (or (char= char #\Space) (char= char #\Tab)
                                  (char= char (code-char 12288))))
              (or text ""))))

(defun strip-code-prefix (name)
  "NAME without the code e-Stat sometimes leads an item name with: '0010
総数' -> '総数'."
  (let ((space (position-if (lambda (char) (or (char= char #\Space)
                                                (char= char (code-char 12288))))
                             name)))
    (if (and space (< space (1- (length name)))
             (every (lambda (char) (or (alphanumericp char)
                                        (member char '(#\- #\_ #\~))))
                    (subseq name 0 space)))
        (string-left-trim '(#\Space #\Tab) (subseq name space))
        name)))

(defun item-aliases (item)
  "The spellings one item answers to: its name, its name without the code
prefix, and its code."
  (let ((name (or (item-name item) "")))
    (remove "" (list (fold name) (fold (strip-code-prefix name)) (fold (item-code item)))
            :test #'equal)))

(defun closest (dim text &optional (limit 5))
  "Up to LIMIT items of DIM that read closest to TEXT, for a refusal that
lets the model repair the call."
  (let* ((query (fold text))
         (items (dim-items dim))
         (head (subseq query 0 (min 2 (length query)))))
    (let ((near (remove-if-not (lambda (item)
                                 (let ((name (fold (item-name item))))
                                   (and (>= (length name) (length head))
                                        (string= head name :end2 (length head)))))
                               items)))
      (if near
          (subseq near 0 (min limit (length near)))
          (subseq items 0 (min limit (length items)))))))

(defun names-of (items)
  (format nil "~{~a~^, ~}" (mapcar (lambda (item) (format nil "~a (~a)" (item-name item) (item-code item)))
                                    items)))

(defun resolve-one (dim text)
  "The one item of DIM that TEXT names. => (values ITEM NIL), or (values
NIL CANDIDATES) when nothing or several things match."
  (let* ((query (fold text))
         (items (dim-items dim))
         (exacts (remove-if-not (lambda (item) (member query (item-aliases item) :test #'string=))
                                 items)))
    (cond
      ((and exacts (null (rest exacts))) (values (first exacts) nil))
      (exacts (values nil (subseq exacts 0 (min 5 (length exacts)))))
      ((zerop (length query)) (values nil (closest dim text)))
      (t
       (let* ((starts (remove-if-not (lambda (item) (let ((name (fold (item-name item))))
                                                      (and (>= (length name) (length query))
                                                           (string= query name :end2 (length query)))))
                                      items))
              (begins (remove-if-not (lambda (item) (let ((name (fold (item-name item))))
                                                      (and (>= (length query) (length name))
                                                           (plusp (length name))
                                                           (string= name query :end2 (length name)))))
                                     items))
              (inside (remove-if-not (lambda (item) (search query (fold (item-name item)))) items))
              )
         (cond ((and starts (null (rest starts))) (values (first starts) nil))
               (starts (values nil (subseq starts 0 (min 5 (length starts)))))
               ((and begins (null (rest begins))) (values (first begins) nil))
               (begins (values nil (subseq begins 0 (min 5 (length begins)))))
               ((and inside (null (rest inside))) (values (first inside) nil))
               (inside (values nil (subseq inside 0 (min 5 (length inside)))))
               (t (values nil (closest dim text)))))))))

(defun split-values (text)
  "TEXT as the values it lists: commas (half and full width) and 読点."
  (let ((parts '()) (start 0))
    (loop for index from 0 below (length text)
          for char = (char text index)
          when (member char '(#\, #\uFF0C #\u3001))
            do (push (subseq text start index) parts) (setf start (1+ index)))
    (push (subseq text start) parts)
    (remove "" (mapcar (lambda (part) (string-trim '(#\Space #\Tab) part)) (nreverse parts))
            :test #'string=)))

(defun resolve-values (dim text)
  "Every item of DIM that TEXT names, one code list. Refuses, naming the
closest items, when a name matches nothing or several things."
  (let ((items '()))
    (dolist (part (split-values text))
      (multiple-value-bind (item candidates) (resolve-one dim part)
        (cond (item (pushnew item items :test #'eq))
              (candidates (fail "~a: no item called \"~a\"~@[; closest are ~a~]"
                                (dim-name dim) part (names-of candidates)))
              (t (fail "~a has no item called \"~a\"" (dim-name dim) part)))))
    (nreverse items)))

(defun dimension-for (dims text)
  "The dimension TEXT names, by its key (area, time, cat01) or its name
(地域, 男女)."
  (let ((query (fold text)))
    (or (find-if (lambda (dim) (string= query (fold (dim-id dim)))) dims)
        (find-if (lambda (dim) (string= query (fold (dim-name dim)))) dims)
        (find-if (lambda (dim) (search query (fold (dim-name dim)))) dims))))

(defun param-name (dim-id)
  "The query parameter one dimension filters by: area -> cdArea, cat01 ->
cdCat01."
  (format nil "cd~@(~a~)" dim-id))

;;; --- periods -----------------------------------------------------------------

(defparameter +era-base+ '(("令和" . 2018) ("平成" . 1988) ("昭和" . 1925) ("大正" . 1911)))

(defun western-period (text)
  "TEXT with an era year turned into its western one: 令和2年 -> 2020年,
and 2020-08 -> 2020年8月."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (dolist (pair +era-base+)
      (let* ((era (car pair))
             (at (search era text)))
        (when (and at (zerop at))
          (let* ((rest (subseq text (length era)))
                 (year (parse-integer rest :junk-allowed t)))
            (when year
              (setf text (format nil "~d年~a" (+ year (cdr pair))
                                 (string-left-trim "年 " (subseq rest (length (princ-to-string year)))))))))))
    (when (and (= (length text) 7) (char= (char text 4) #\-)
               (every #'digit-char-p (subseq text 0 4))
               (every #'digit-char-p (subseq text 5)))
      (setf text (format nil "~d年~d月" (parse-integer text :end 4) (parse-integer text :start 5))))
    text))

(defun latest-items (dim count)
  "DIM's COUNT newest items, by code: e-Stat writes a period's code so that
it sorts as the period does (2020000000, 2025080000)."
  (let ((items (sort (copy-list (dim-items dim)) #'string> :key (lambda (item) (item-code item)))))
    (if (>= (length items) count) (subseq items 0 count) items)))

(defun resolve-period (dim text)
  "The time items TEXT names, newest first, including latest, latest:N,
2020-2023 and 2020年1月~2020年12月."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (labels ((range (from to)
               (let* ((start (first (resolve-values dim from)))
                      (end (first (resolve-values dim to)))
                      (left (item-code start))
                      (right (item-code end))
                      (low (if (string<= left right) left right))
                      (high (if (string<= left right) right left)))
                 (remove-if-not (lambda (item) (and (string<= low (item-code item))
                                                    (string<= (item-code item) high)))
                                (dim-items dim)))))
      (cond
        ((equal (fold text) "latest") (latest-items dim 1))
        ((and (> (length (fold text)) 6)
              (string= "latest:" (fold text) :end2 7))
         (latest-items dim (max 1 (or (parse-integer text :start 7 :junk-allowed t) 1))))
        ((search "~" text)
         (let ((at (position #\~ text)))
           (range (subseq text 0 at) (subseq text (1+ at)))))
        ((and (= (length text) 9) (char= (char text 4) #\-)
              (every #'digit-char-p (subseq text 0 4))
              (every #'digit-char-p (subseq text 5)))
         (let* ((from (subseq text 0 4)) (to (subseq text 5))
                (low (parse-integer from)) (high (parse-integer to)))
           (when (<= low high)
             (remove-if-not (lambda (item)
                              (let ((year (parse-integer (item-name item) :junk-allowed t)))
                                (and year (<= low year) (<= year high))))
                            (dim-items dim)))))
        (t (resolve-values dim (western-period text)))))))

(defun resolve-area (dim text)
  "The area items TEXT names, including children:X (X and everything
below it, by name) and level:N (every area at that level)."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (cond
      ((and (> (length text) 9) (string-equal "children:" text :end2 9))
       (let* ((parent (fold (subseq text 9)))
              (family (remove-if-not (lambda (item) (let ((name (fold (item-name item))))
                                                      (and (>= (length name) (length parent))
                                                           (string= parent name :end2 (length parent)))))
                                     (dim-items dim))))
         (or family (fail "~a: no area called \"~a\"" (dim-name dim) (subseq text 9)))))
      ((and (> (length text) 6) (string-equal "level:" text :end2 6))
       (let* ((level (parse-integer text :start 6 :junk-allowed t))
              (of-level (remove-if-not (lambda (item) (eql (item-level item) level))
                                       (dim-items dim))))
         (or of-level (fail "~a has no area at level ~a" (dim-name dim) level))))
      (t (resolve-values dim text)))))

;;; --- the search block --------------------------------------------------------

(defun table-line (row index)
  "One line of the search block."
  (let* ((id (jtext row "@id"))
         (title (jtext row "@title"))
         (stat (jtext row "@statName"))
         (org (jtext row "@govOrg"))
         (survey (jtext row "@surveyDate"))
         (area (jtext row "@areaLevel"))
         (cycle (jtext row "@frequency" "-"))
         (parts (remove "" (list org survey area (unless (equal cycle "-") cycle)) :test #'string=)))
    (format nil "~d. ~a  ~a~@[~%     ~a~]~@[ (~{~a~^; ~})~]~%"
            index id (if (plusp (length stat)) stat title)
            (if (and (plusp (length title)) (plusp (length stat))) title nil)
            parts)))

(defun search-block (rows total app-id query)
  (declare (ignore app-id))
  (if (null rows)
      (format nil "e-Stat has no table for \"~a\".~%Try Japanese words a table title carries (人口 世帯数, 消費者物価指数), or name the survey (国勢調査) in survey."
              query)
      (with-output-to-string (out)
        (format out "e-Stat tables for \"~a\"~@[: ~d in total, best ~d shown~]:~%"
                query (and (> total (length rows)) total) (length rows))
        (loop for row in rows
              for index from 1
              do (write-string (table-line row index) out))
        (write-string "next: estat_table_info(table_id) for one table's dimensions, estat_get_data(table_id, area=..., time=...) for its numbers." out))))

(defun search-rows (root)
  (let* ((data (jget root "DATA_LIST_INF"))
         (rows (jlist (jget data "DATA_LIST"))))
    (values rows
            (let ((total (jint (jget root "RESULT") "TOTAL_NUMBER" (length rows))))
              (max total (length rows))))))

;;; --- the table block ---------------------------------------------------------

(defun dimension-line (dim max-items)
  (let* ((items (dim-items dim))
         (shown (subseq items 0 (min max-items (length items)))))
    (format nil "  ~a (~a): ~d item~:p -- ~{~a~^, ~}~@[, and ~d more~]~%"
            (dim-name dim) (dim-id dim) (length items)
            (mapcar (lambda (item)
                      (let ((level (item-level item)))
                        (if (and level (> level 1))
                            (format nil "~a[~d]" (item-name item) level)
                            (item-name item))))
                    shown)
            (and (> (length items) (length shown)) (- (length items) (length shown))))))

(defun example-call (table-id dims)
  (let* ((area (find "area" dims :key #'dim-id :test #'string=))
         (time (find "time" dims :key #'dim-id :test #'string=))
         (area-item (and area (first (last (dim-items area)))))
         (time-item (and time (first (dim-items time)))))
    (format nil "estat_get_data(table_id=\"~a\"~@[, area=\"~a\"~]~@[, time=\"~a\"~])"
            table-id (and area-item (item-name area-item)) (and time-item (item-name time-item)))))

(defun table-block (table-id table dims max-items)
  (with-output-to-string (out)
    (format out "e-Stat ~a -- ~a~@[ (~a)~]~@[, ~a~]~%"
            table-id (label-of (jget table "TITLE"))
            (label-of (jget table "GOV_ORG"))
            (label-of (jget table "SURVEY_DATE")))
    (let ((stat (label-of (jget table "STATISTICS_NAME"))))
      (when (plusp (length stat)) (format out "  survey: ~a~%" stat)))
    (format out "dimensions:~%")
    (dolist (dim dims) (write-string (dimension-line dim max-items) out))
    (format out "next: ~a~%" (example-call table-id dims))
    (format out "page: ~a~%" (format nil +table-url+ table-id))))

;;; --- the figures block -------------------------------------------------------

(defun find-item (dim code)
  (find code (dim-items dim) :key #'item-code :test #'string=))

(defun row-line (row dims)
  (let* ((value (jget row "$"))
         (unit (jtext row "@unit"))
         (labels-list
           (loop for dim in dims
                 for code = (jget row (format nil "@~a" (dim-id dim)))
                 when (and (stringp code) (plusp (length code)))
                   collect (let ((item (find-item dim code)))
                             (or (and item (item-name item)) code)))))
    (format nil "  ~{~a~^ | ~} = ~a~@[ ~a~]~%"
            labels-list
            (if (and (stringp value) (plusp (length value))) value "(no value)")
            (and (plusp (length unit)) unit))))

(defun data-block (table-id table dims rows selection)
  (with-output-to-string (out)
    (format out "e-Stat ~a -- ~a~@[ (~a)~]~@[, ~a~]~%"
            table-id (label-of (jget table "TITLE"))
            (label-of (jget table "GOV_ORG"))
            (label-of (jget table "SURVEY_DATE")))
    (when selection (format out "selection: ~a~%" selection))
    (format out "~d value~:p~%" (length rows))
    (dolist (row rows) (write-string (row-line row dims) out))
    (when (search "速報" (label-of (jget table "TITLE")))
      (format out "warning: this table is preliminary (速報); its figures may be revised.~%"))
    (format out "citation: 出典：政府統計の総合窓口(e-Stat)「~a」(~a)~%"
            (label-of (jget table "TITLE")) (label-of (jget table "GOV_ORG")))
    (format out "credit: ~a~%" +credit+)
    (format out "page: ~a~%" (format nil +table-url+ table-id))))
