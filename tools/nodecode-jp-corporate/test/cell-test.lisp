;;;; cell-test.lisp --- lifecycle, the folding, the two tools and /jp-corporate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches gBizINFO, needs a
;;;; token or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, a wrong value and a missing token refused), the
;;;; folding that runs before any request (a corporate number's check digit, an
;;;; abbreviated name, a prefecture), the two tools through the seam the model
;;;; calls them by -- their names, their arguments, their headers and their answer
;;;; -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '() "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-jp-corporate-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials the API."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected gBizINFO request"))))
     (nodecode-jp-corporate::clear-cache)
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun jp-corporate-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun jp-corporate-url (request)
  (second request))

(defun jp-corporate-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun jp-corporate-headers ()
  "A decoded header table, as NLK:HTTP answers one."
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "content-type" table) "application/json")
    table))

(defun jp-corporate-serving (&rest answers)
  "A registry that answers the first request with the first of ANSWERS (text,
status), the second with the second, and refuses to be asked a third time."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url))
          (let ((answer (or (pop answers)
                            (error "unexpected extra request to ~a" url))))
            (values (first answer) (second answer) (jp-corporate-headers))))))

(defun jp-corporate-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "jp-corporate-call" name arguments)))

(defparameter +toyota+
  (format nil "{\"hojin-infos\": [{\"corporate_number\": \"1180301018771\",~%
     \"name\": \"トヨタ自動車株式会社\", \"kana\": \"トヨタジドウシャ\",~%
     \"location\": \"愛知県豊田市トヨタ町１番地\", \"postal_code\": \"4718571\",~%
     \"status\": \"-\", \"kind\": \"301\", \"capital_stock\": \"635401000000\",~%
     \"employee_number\": \"70000\", \"industry\": [\"E\"],~%
     \"update_date\": \"2024-12-01\", \"company_url\": \"https://global.toyota/\"}]}")
  "A gBizINFO answer for one corporation, in the API's own shape.")

(defparameter +toyota-subsidy+
  (format nil "{\"hojin-infos\": [{\"name\": \"トヨタ自動車株式会社\",~%
     \"subsidy\": [{\"date_of_approval\": \"2023-04-01\", \"title\": \"ものづくり補助金\",~%
                    \"amount\": \"1000000\", \"target\": \"設備投資\",~%
                    \"government_departments\": \"経済産業省\"}]}]}")
  "A subsidy answer for the same corporation.")

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "jp-corporate" "JP-CORPORATE-CELL-" :start nodecode-jp-corporate:start-cell)

(define-cell-lifecycle-tests "jp-corporate"
  (:fixture with-jp-corporate-wire)
  (:config "api_key" "gbiz-test")
  (:help :jp-corporate)
  (:command "jp-corporate")
  (:running
   (is (nle::find-tool "jp_corp_search") "search is registered")
   (is (nle::find-tool "jp_corp_profile") "and the profile")
   (is (= 2 (count-if (lambda (entry) (eql 0 (search "jp_corp_" (first entry)))) nle::*tools*)) "two of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "jp_corp_search")) "stop takes the search back")
   (is (null (nle::find-tool "jp_corp_profile")) "and the profile"))
  (:refused ("timeout_seconds" 0)))

(deftest jp-corporate-cell-a-start-with-no-token-refuses-and-installs-nothing (with-jp-corporate-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
    (let ((text (refusal-text nlk:config-refusal (jp-corporate-start))))
      (is-carrying text "needs a free gBizINFO API token" "GBIZINFO_API_TOKEN")
      (is (null (nle::find-tool "jp_corp_search")) "nothing was installed"))))

(deftest jp-corporate-cell-the-token-may-come-from-the-environment (with-jp-corporate-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (if (equal name "GBIZINFO_API_TOKEN") "gbiz-env" nil))
    (with-cell-stop ((jp-corporate-start))
      (jp-corporate-serving (list +toyota+ 200))
      (jp-corporate-call "jp_corp_search" "{\"corporate_number\": \"1180301018771\"}")
      (let ((sent (jp-corporate-header (first (jp-corporate-asked)) "X-hojinInfo-api-token")))
        (is (equal "gbiz-env" sent) "the exported token is the one sent")))))

;;; --- what runs before any request -------------------------------------------------

(deftest jp-corporate-cell-the-check-digit-refuses-a-typo-without-a-request (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (let ((answer (jp-corporate-call "jp_corp_search" "{\"corporate_number\": \"1180301018772\"}")))
      (is (search "ERROR: " answer) "a wrong digit is the model's repair, not a condition up the stack")
      (is (search "check digit" answer) "the answer names the check digit")
      (is (null (jp-corporate-asked)) "and nothing was sent"))))

(deftest jp-corporate-cell-an-invoice-number-is-a-corporate-number (with-jp-corporate-wire)
  (is (equal "1180301018771" (nodecode-jp-corporate::parse-corporate-number "T1180301018771")) "an invoice number's leading T comes off")
  (is (equal "1180301018771" (nodecode-jp-corporate::parse-corporate-number "１１８０-３０１０-１８７７１")) "full-width digits, spaces and hyphens come off too"))

(deftest jp-corporate-cell-names-and-addresses-fold-before-they-are-compared ()
  (is (equal "トヨタ自動車" (nodecode-jp-corporate::core-name "(株)ﾄﾖﾀ自動車")) "an abbreviated, half-width name folds to its core")
  (is (equal "トヨタ自動車" (nodecode-jp-corporate::query-term "株式会社トヨタ自動車")) "the legal form comes off the string sent to the API")
  (multiple-value-bind (prefecture rest) (nodecode-jp-corporate::split-prefecture "〒471-0826 愛知県豊田市")
    (is (eql 23 prefecture) "a postal code and a prefecture are read apart")
    (is (equal "豊田市" rest) "and the rest of the address is kept")))

;;; --- jp_corp_search ---------------------------------------------------------------

(deftest jp-corporate-cell-a-name-search-asks-once-and-ranks (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (jp-corporate-serving (list +toyota+ 200))
    (let ((answer (jp-corporate-call "jp_corp_search" "{\"name\": \"(株)ﾄﾖﾀ自動車\", \"limit\": 5}")))
      (let* ((request (first (jp-corporate-asked)))
             (url (jp-corporate-url request)))
        (is (= 1 (length (jp-corporate-asked))) "one request per search")
        (is (search "https://api.info.gbiz.go.jp/hojin/v2/hojin?" url) "to the plugin's endpoint")
        (is (search "limit=1000" url) "asking the registry once for a page of rows")
        (is (search "%E3%83%88%E3%83%A8%E3%82%BF" url) "with the name folded and encoded")
        (is (equal "gbiz-test" (jp-corporate-header request "X-hojinInfo-api-token")) "with the token in the header the API names"))
      (is (search "トヨタ自動車株式会社" answer) "the answer names the corporation")
      (is (search "1180301018771" answer) "with its corporate number")
      (is (search "exact" answer) "and how it matched")
      (is (search "https://info.gbiz.go.jp/hojin/ichiran?hojinBango=1180301018771" answer) "and the page the operator can open")
      (is (search "Source: gBizINFO" answer) "the attribution the terms ask for is there"))))

(deftest jp-corporate-cell-a-number-checks-the-name-and-the-address-it-was-given (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (jp-corporate-serving (list +toyota+ 200))
    (let ((answer (jp-corporate-call
                   "jp_corp_search"
                   "{\"corporate_number\": \"1180301018771\", \"name\": \"トヨタ自動車\", \"address\": \"愛知県豊田市トヨタ町1番地\"}")))
      (is (search "https://api.info.gbiz.go.jp/hojin/v2/hojin/1180301018771" (jp-corporate-url (first (jp-corporate-asked)))) "one corporation is fetched by its own route")
      (is (search "name: match" answer) "the name matches the official one")
      (is (search "address: match" answer) "and so does the address"))))

(deftest jp-corporate-cell-a-refused-token-is-the-model-s-repair (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (jp-corporate-serving (list "{\"message\": \"unauthorized\"}" 401))
    (let ((answer (jp-corporate-call "jp_corp_search" "{\"name\": \"トヨタ自動車\"}")))
      (is (search "ERROR: " answer) "a refused status is the tool result")
      (is (search "rejected the API token (401)" answer) "that says what to do about it")
      (is (null (search "gbiz-test" answer)) "and never echoes the token"))))

;;; --- jp_corp_profile --------------------------------------------------------------

(deftest jp-corporate-cell-a-profile-asks-for-the-sections-it-was-given (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (jp-corporate-serving (list +toyota+ 200) (list +toyota-subsidy+ 200))
    (let ((answer (jp-corporate-call
                   "jp_corp_profile"
                   "{\"corporate_number\": \"1180301018771\", \"sections\": [\"basic\", \"subsidy\"]}")))
      (is (equal '("https://api.info.gbiz.go.jp/hojin/v2/hojin/1180301018771"
                   "https://api.info.gbiz.go.jp/hojin/v2/hojin/1180301018771/subsidy")
                 (mapcar #'jp-corporate-url (jp-corporate-asked))) "one request per section, each on its own route")
      (is (search "トヨタ自動車株式会社" answer) "the answer carries the basic record")
      (is (search "株式会社 (kabushiki kaisha)" answer) "with its legal kind spelled out")
      (is (search "ものづくり補助金" answer) "and the subsidy it was given")
      (is (search "total: 1" answer) "with how many there were"))))

(deftest jp-corporate-cell-a-profile-of-a-number-the-registry-does-not-know (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (jp-corporate-serving (list "{}" 404))
    (let ((answer (jp-corporate-call "jp_corp_profile" "{\"corporate_number\": \"4000012090001\"}")))
      (is (search "found: 0" answer) "a 404 is an answer, not a condition")
      (is (search "has no corporation with 法人番号 4000012090001" answer) "and says which number it was"))))

(deftest jp-corporate-cell-an-unknown-section-is-refused-before-any-request (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (let ((answer (jp-corporate-call "jp_corp_profile" "{\"corporate_number\": \"1180301018771\", \"sections\": [\"sales\"]}")))
      (is (search "unknown section" answer) "the refusal names the section")
      (is (search "subsidy" answer) "and lists the ones there are")
      (is (null (jp-corporate-asked)) "no request was sent"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest jp-corporate-cell-the-command-searches-or-says-what-it-is-connected-to (with-jp-corporate-wire)
  (with-cell-stop ((jp-corporate-start "api_key" "gbiz-test"))
    (let ((status (nodecode-jp-corporate::run-command "" nil)))
      (is (search "https://api.info.gbiz.go.jp/hojin/v2/hojin" status) "no argument reports the endpoint")
      (is (search "token set" status) "and that a token is there")
      (is (null (search "gbiz-test" status)) "without printing it")
      (is (null (jp-corporate-asked)) "and sends nothing"))
    (jp-corporate-serving (list +toyota+ 200))
    (let ((answer (nodecode-jp-corporate::run-command "  トヨタ自動車  " nil)))
      (is (search "トヨタ自動車株式会社" answer) "a name runs one search")
      (is (search "name=%E3%83%88%E3%83%A8%E3%82%BF" (jp-corporate-url (first (jp-corporate-asked)))) "with the name trimmed and sent"))
    (nodecode-jp-corporate::clear-cache)
    (setf *requests* '())
    (jp-corporate-serving (list +toyota+ 200))
    (nodecode-jp-corporate::run-command "1180301018771" nil)
    (is (search "/1180301018771" (jp-corporate-url (first (jp-corporate-asked)))) "a 13-digit number is looked up by its route")))
