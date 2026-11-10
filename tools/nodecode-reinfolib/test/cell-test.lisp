;;;; cell-test.lisp --- the reinfolib cell against a stubbed MLIT.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP: nothing reaches MLIT, the network or
;;;; the operator's files. The tool is called the way a model's call reaches it,
;;;; through the real dispatch, contract and all, and the command through the
;;;; registered entry, so what a user calls and what the body runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "reinfolib" "REINFOLIB-CELL-"
  :start nodecode-reinfolib:start-cell
  :defaults (("api_key" "test-key")))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-reinfolib-fixture (&body body)
  "BODY with the gap between two requests at zero: no test sleeps for MLIT's second."
  `(let ((nodecode-reinfolib::*gap-seconds* 0))
     ,@body))

(defun mlit-row (&key (type "宅地(土地)") (price "32000000") (area "100")
                       (municipality "13104") (district "西新宿")
                       (period "2015年第2四半期")
                       (price-category "不動産取引価格情報"))
  "One XIT001 row, with the twelve columns an answer keeps and two it drops."
  (format nil "{\"Type\":\"~a\",\"Region\":\"\",\"MunicipalityCode\":\"~a\",\"Prefecture\":\"東京都\",\"Municipality\":\"新宿区\",\"DistrictName\":\"~a\",\"TradePrice\":\"~a\",\"PricePerUnit\":\"\",\"Area\":\"~a\",\"UnitPrice\":\"\",\"Period\":\"~a\",\"PriceCategory\":\"~a\",\"FloorPlan\":\"\",\"Structure\":\"ＲＣ\"}"
          type municipality district price area period price-category))

(defun mlit-body (&rest rows)
  "A whole XIT001 body holding ROWS."
  (format nil "{\"status\":\"OK\",\"data\":[~{~a~^,~}]}" rows))

(defparameter +quarter-body+
  (mlit-body (mlit-row :type "中古マンション等" :price "63000000" :area "42")
             (mlit-row)
             (mlit-row :price "48000001" :district "百人町"))
  "Three rows: a median in the middle, a .5 half in one type's own median.")

(defparameter +many-body+
  (apply #'mlit-body (loop for index from 1 to 25
                            collect (mlit-row :price (format nil "~d000000" index))))
  "Twenty-five rows, five more than an answer carries.")

(defmacro with-mlit-wire ((asked body &key (status 200)) &body forms)
  "FORMS with NLK:HTTP answering BODY at STATUS for every URL. ASKED collects
(METHOD URL HEADERS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '())
           (answer ,body)
           (code ,status))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :headers)) ,log)
                                   (values answer code nil nil))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@forms)))))

(defun reinfolib-answer (id arguments)
  "reinfolib_quarter's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id "reinfolib_quarter" arguments)))

(defun reinfolib-header (headers name)
  "HEADERS' NAME, compared without case."
  (cdr (assoc name headers :test #'string-equal)))

(defun reinfolib-row-lines (text)
  "How many numbered row lines TEXT carries: a line whose first token is a number and a dot."
  (count-if (lambda (line)
              (let* ((start (position-if-not (lambda (char) (char= char #\Space)) line))
                     (stop (and start (position-if-not (lambda (char) (digit-char-p char))
                                                       line :start start))))
                (and stop (char= (char line stop) #\.))))
            (uiop:split-string text :separator '(#\Newline))))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "reinfolib"
  (:fixture with-reinfolib-fixture)
  (:help :reinfolib)
  (:command "reinfolib")
  (:running (is (equal "test-key" (nodecode-reinfolib::setting :api-key)) "the key the section names is what runs")
            (is (equal "https://www.reinfolib.mlit.go.jp" (nodecode-reinfolib::setting :origin)) "and MLIT's own origin while base_url is empty")
            (is (= 30 (nodecode-reinfolib::setting :timeout-seconds)) "with the plugin's own ceiling on one request"))
  (:refused ("api_key" "") ("api_key" "with space")
            ("api_key" "k" "base_url" "https://mirror.test/path"))
  (:idle nodecode-reinfolib:reinfolib-error
         (nodecode-reinfolib:quarter "13104" 2015 2)))

;;; --- the tool ---------------------------------------------------------------------

(deftest reinfolib-cell-answers-one-quarter-through-the-wire (with-reinfolib-fixture)
  (with-cell-stop ((reinfolib-start))
    (with-mlit-wire (asked +quarter-body+)
      (let* ((text (reinfolib-answer "r1" "{\"city\":\"13104\",\"year\":2015,\"quarter\":2}"))
              (url (second (first asked))))
        (is (= 1 (length asked)) "one quarter, one request")
        (is (eq :get (first (first asked))) "a GET")
        (is (search "/ex-api/external/XIT001?year=2015&quarter=2&city=13104" url) "the plugin's path and its parameters")
        (is (search "priceClassification=01&language=ja" url) "the classification and the language, as it sent them")
        (is (equal "test-key" (reinfolib-header (third (first asked)) "Ocp-Apim-Subscription-Key")) "the key rides its one header")
        (is (not (search "test-key" url)) "and never the URL")
        (is (search "13104 · 2015年第2四半期 · 3 rows" text) "the quarter and its row count")
        (is (search "median TradePrice 48000001" text) "the middle of three prices")
        (is (search "宅地(土地) 2 (median 40000000.5)" text) "a type's own median, with its .5 half")
        (is (search "中古マンション等 · 東京都 新宿区 西新宿" text) "a row with where it is")
        (is (search "TradePrice 63000000 · Area 42" text) "and the numbers that are set")
        (is (search "出典：国土交通省　不動産情報ライブラリ" text) "MLIT's attribution, unchanged")
        (is (search "提供情報の最新性、正確性、完全性等が保証されたものではありません" text) "and its terms sentence, uncut")
        (is (not (search "test-key" text)) "the key is not in the answer")))))

(deftest reinfolib-cell-keeps-twenty-rows-and-says-so (with-reinfolib-fixture)
  (with-cell-stop ((reinfolib-start))
    (with-mlit-wire (asked +many-body+)
      (let ((text (reinfolib-answer "r2" "{\"city\":\"13104\",\"year\":2015,\"quarter\":2}")))
        (is (search "25 rows" text) "the count is the full length")
        (is (search "(25 rows, the first 20 returned)" text) "and the cut is said, as truncated was")
        (is (= 20 (reinfolib-row-lines text)) "with twenty numbered rows, and no more")
        (is (not (search "TradePrice 25000000" text)) "so the twenty-first row is not among them")))))

(deftest reinfolib-cell-a-404-is-no-data-and-not-zero-rows (with-reinfolib-fixture)
  (with-cell-stop ((reinfolib-start))
    (with-mlit-wire (asked "{\"statusCode\":404}" :status 404)
      (let ((text (reinfolib-answer "r3" "{\"city\":\"13104\",\"year\":2015,\"quarter\":2}")))
        (is (search "no data for that city, year and quarter" text) "the plugin's one insistence")
        (is (search "not zero rows, and not unchanged prices" text) "no data is not zero rows")
        (is (search "HTTP 404" text) "and the status is named")))))

(deftest reinfolib-cell-a-refused-key-is-not-an-empty-result (with-reinfolib-fixture)
  (with-cell-stop ((reinfolib-start))
    (with-mlit-wire (asked "{}" :status 401)
      (let ((text (reinfolib-answer "r4" "{\"city\":\"13104\",\"year\":2015,\"quarter\":2}")))
        (is (search "API refused the subscription key" text) "a refusal is named as one")
        (is (search "not an empty result" text) "and is never read as an empty quarter")))))

(deftest reinfolib-cell-an-answer-that-carries-the-key-is-dropped (with-reinfolib-fixture)
  (with-cell-stop ((reinfolib-start))
    (with-mlit-wire (asked (mlit-body (mlit-row :district "test-key")))
      (let ((text (reinfolib-answer "r5" "{\"city\":\"13104\",\"year\":2015,\"quarter\":2}")))
        (is (search "the answer was dropped" text) "the plugin's SEAL, kept")
        (is (not (search "test-key" text)) "and the key is in no answer")))))

(deftest reinfolib-cell-a-designated-city-code-is-refused-before-the-wire (with-reinfolib-fixture)
  (with-cell-stop ((reinfolib-start))
    (with-mlit-wire (asked +quarter-body+)
      (let ((text (reinfolib-answer "r6" "{\"city\":\"14100\",\"year\":2015,\"quarter\":2}")))
        (is (search "Use a ward code, not the city code" text) "14100 is Yokohama, and XIT001 wants a ward")
        (is (search "designated_city" text) "the problem is named")
        (is (null asked) "and nothing was requested")))))

(deftest reinfolib-cell-the-command-answers-and-says-its-usage (with-reinfolib-fixture)
  (with-cell-stop ((reinfolib-start))
    (with-mlit-wire (asked +quarter-body+)
      (let ((text (cell-entry "nodecode-reinfolib" "reinfolib" "13104 2015 2")))
        (is (search "13104 · 2015年第2四半期 · 3 rows" text) "three arguments are one quarter")
        (is (= 1 (length asked)) "and one request"))
      (let ((usage (cell-entry "nodecode-reinfolib" "reinfolib" "13104 2015")))
        (is (search "Usage: /reinfolib CITY YEAR QUARTER" usage) "two arguments say how it is used")))))

(deftest reinfolib-cell-the-arguments-are-checked-as-the-plugin-checked-them (with-reinfolib-fixture)
  (multiple-value-bind (parsed problem)
      (nodecode-reinfolib:check-arguments "１３１０４" 2015 2)
    (declare (ignore problem))
    (is (equal "13104" (getf parsed :city)) "full-width digits a Japanese keyboard makes are folded"))
  (multiple-value-bind (parsed problem) (nodecode-reinfolib:check-arguments "13104" 2005 2)
    (declare (ignore parsed))
    (is (equal "bad_quarter" (getf problem :error-type)) "2005 starts at quarter 3"))
  (multiple-value-bind (parsed problem) (nodecode-reinfolib:check-arguments "1310" 2015 2)
    (declare (ignore parsed))
    (is (equal "shape" (getf problem :problem)) "four digits are not a city code"))
  (multiple-value-bind (parsed problem) (nodecode-reinfolib:check-arguments "13104" 2015 5)
    (declare (ignore parsed))
    (is (equal "bad_quarter" (getf problem :error-type)) "there are four quarters")))
