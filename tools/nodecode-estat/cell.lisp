;;;; cell.lisp --- the three tools, /estat, the manual, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model reads e-Stat with three tools, the operator with /estat; both
;;;; answer the same blocks, built by ESTAT.LISP. The section is `estat':
;;;;   "estat": {"enabled": true, "app_id": "...",
;;;;              "app_id_env": "ESTAT_APP_ID",
;;;;              "timeout_seconds": 20, "max_rows": 50}
;;;; The application ID is the one required setting, pasted or named by a
;;;; variable -- as the original read ESTAT_APP_ID -- and a section naming
;;;; none refuses the start.

(in-package #:nodecode-estat)

;;; --- the manual --------------------------------------------------------------

(defparameter *primer*
  "Japan's official statistics are reachable through the nodecode-estat cell: three read-only
tools over e-Stat, the government statistics portal (政府統計の総合窓口).
  estat_search_tables(query, survey, year, area_level, limit)
      Tables whose title matches: id, title, the survey it belongs to, who published it, its
      survey year, the area it is tabulated to. Search in Japanese (人口 世帯数, 賃金 産業,
      消費者物価指数); limit is 1-30. Starts a lookup; no numbers yet.
  estat_table_info(table_id, dimension, search, max_items)
      One table's dimensions: 地域 (area), 時点 (time), 表章項目 (tab) and the categories
      (男女, 産業), with the codes and the names that estat_get_data accepts, plus a ready call.
  estat_get_data(table_id, area, time, filters, max_rows)
      The numbers. Ask by name: area=\"東京都\", time=\"2020年\" or \"latest\",
      filters={\"男女\": \"女\"}. Any of them left out is not filtered. A name that matches
      nothing or several things answers with the closest names, so the call repairs itself.
      Every block carries the unit e-Stat gives, the citation to copy and e-Stat's credit.
  /estat <words>        the same search in the conversation; a 10-digit id gives its table.
Doctrine: report only the numbers a call returned, with the citation beside them, and pass on
the warning when a table is preliminary (速報). One search, then one table: a figure from
memory is not an e-Stat figure. The application ID belongs to the operator; nothing else
leaves the machine."
  "What (help :estat) answers while the cell runs.")

;;; --- reading the section -----------------------------------------------------

(defun nonblank (value)
  "VALUE as text when it carries something, else NIL."
  (let ((value (and (stringp value) (nlk:trimmed value))))
    (and value (plusp (length value)) value)))

(defun read-settings (values table)
  "The declared VALUES, with the application ID read here, once: the inline
secret, else the variable app_id_env names, else a refusal."
  (declare (ignore table))
  (let* ((key (nonblank (getf values :app-id)))
         (env (nonblank (getf values :app-id-env)))
         (plist (copy-list values)))
    (setf (getf plist :app-id)
          (or key
              (and env (nonblank (nle::credential-env env)))
              (nlk:config-error
               "no application ID: paste the free e-Stat ID into app_id, or name the ~
variable holding it in app_id_env~@[ (~a is unset or empty)~]; get one from the e-Stat ~
My Page (マイページ -> API機能)"
               env)))
    plist))

;;; --- the parameters the API takes ---------------------------------------------

(defparameter +area-levels+ '(("national" . "1") ("prefecture" . "2") ("municipality" . "3"))
  "collectArea: the narrowest area one table is tabulated to.")

(defun survey-years (year)
  "YEAR as e-Stat's surveyYears takes it: 2020, 202010, or a run of years."
  (when (and year (plusp (length year)))
    (let ((text (western-period year)))
      (if (and (= (length text) 9) (char= (char text 4) #\-))
          (let ((from (parse-integer text :end 4)) (to (parse-integer text :start 5))
                (years '()))
            (if (<= from to)
                (loop for year from from to to do (push (princ-to-string year) years))
                (list text))
            (format nil "~{~a~^,~}" (nreverse years)))
          (remove-if-not #'digit-char-p text)))))

(defun area-level-code (level)
  (let ((level (fold level)))
    (when (plusp (length level))
      (or (cdr (assoc level +area-levels+ :test #'string=))
          (and (member level '("1" "2" "3") :test #'string=) level)
          (fail "area_level is national, prefecture or municipality, not \"~a\"" level)))))

;;; --- the tools ---------------------------------------------------------------

(define-verb search-tables (query &key (survey "") (year "") (area-level "") (limit 10))
  "The `estat_search_tables' tool: e-Stat tables whose title matches QUERY,
as lines the model reads. A failure is the call's text, not the turn's."
  (handler-case
      (let* ((limit (max 1 (min 30 (or limit 10))))
             (query (or (nonblank query)
                        (fail "query is empty: give Japanese words a table title carries, ~
such as 人口 世帯数 or 消費者物価指数")))
             (root (call-api "getStatsList"
                              (list (cons "searchWord" query)
                                    (cons "limit" (princ-to-string limit))
                                    (cons "explanationGetFlg" "N")
                                    (cons "surveyYears" (survey-years year))
                                    (cons "collectArea" (area-level-code area-level)))
                              (setting :app-id) (setting :timeout-seconds))))
        (multiple-value-bind (rows total) (search-rows root)
          (let ((rows (if (and survey (plusp (length survey)))
                          (remove-if-not (lambda (row)
                                           (search (fold survey)
                                                   (fold (concatenate 'string
                                                                      (jtext row "@statName")
                                                                      (jtext row "@title")))))
                                         rows)
                          rows)))
            (search-block (subseq rows 0 (min limit (length rows))) total (setting :app-id) query))))
    (estat-error (condition) (format nil "ERROR: ~a" condition))))

(define-verb table-info (table-id &key (dimension "") (pattern "") (max-items 12))
  "The `estat_table_info' tool: one table's dimensions and the names its
figures can be asked by."
  (handler-case
      (let ((max-items (max 1 (min 300 (or max-items 12)))))
        (multiple-value-bind (table dims)
            (table-meta (or (nonblank table-id) (fail "table_id is empty"))
                        (setting :app-id) (setting :timeout-seconds))
          (when (and dimension (plusp (length dimension)))
            (let ((one (or (dimension-for dims dimension)
                           (fail "this table has no dimension called \"~a\"; it has ~{~a~^, ~}"
                                 dimension (mapcar #'dim-name dims)))))
              (setf dims (list one))))
          (when (and pattern (plusp (length pattern)))
            (setf dims (loop for dim in dims
                              for items = (remove-if-not
                                           (lambda (item)
                                             (search (fold pattern) (fold (item-name item))))
                                           (dim-items dim))
                              when items
                                collect (make-dim :id (dim-id dim) :name (dim-name dim)
                                                  :items items))))
          (table-block table-id table dims max-items)))
    (estat-error (condition) (format nil "ERROR: ~a" condition))))

(defun filter-pairs (filters)
  "FILTERS, the tool's object of dimension -> names, as a list of (name .
value)."
  (cond ((hash-table-p filters)
         (loop for name being the hash-keys of filters using (hash-value value)
               when (and (stringp value) (plusp (length value)))
                 collect (cons name value)))
        ((listp filters)
         (loop for (name value) on filters by #'cddr
               when (and (stringp value) (plusp (length value)))
                 collect (cons name value)))
        (t '())))

(define-verb get-data (table-id &key (area "") (period "") filters (max-rows 50))
  "The `estat_get_data' tool: the figures one table holds for the areas,
periods and categories named, with the citation and the credit."
  (handler-case
      (let ((max-rows (max 1 (min 500 (or max-rows 50))))
            (selected '())
            (described '()))
        (multiple-value-bind (table dims)
            (table-meta (or (nonblank table-id) (fail "table_id is empty"))
                        (setting :app-id) (setting :timeout-seconds))
          (flet ((take (dim items)
                   (when items
                     (push (cons dim items) selected)
                     (push (format nil "~a=~{~a~^, ~}" (dim-name dim)
                                   (mapcar #'item-name items))
                           described))))
            (let ((dim (and (nonblank area) (dimension-for dims "area"))))
              (when dim (take dim (resolve-area dim area))))
            (let ((dim (and (nonblank period) (dimension-for dims "time"))))
              (when dim (take dim (resolve-period dim period))))
            (dolist (pair (filter-pairs filters))
              (let ((dim (or (dimension-for dims (car pair))
                             (fail "this table has no dimension called \"~a\"; it has ~{~a~^, ~}"
                                   (car pair) (mapcar #'dim-name dims)))))
                (take dim (resolve-values dim (cdr pair))))))
          (let* ((params (append (list (cons "statsDataId" table-id)
                                       (cons "limit" (princ-to-string (1+ max-rows)))
                                       (cons "metaGetFlg" "N")
                                       (cons "explanationGetFlg" "N"))
                                 (loop for (dim . items) in selected
                                       collect (cons (param-name (dim-id dim))
                                                     (format nil "~{~a~^,~}"
                                                             (mapcar #'item-code items))))))
                 (root (call-api "getStatsData" params (setting :app-id)
                                 (setting :timeout-seconds)))
                 (values-rows (jlist (jget (jget (jget root "STATISTICAL_DATA") "DATA_INF")
                                           "VALUE")))
                 (selection (format nil "~{~a~^; ~}" (nreverse described)))
                 (rows (subseq values-rows 0 (min max-rows (length values-rows)))))
            (let ((block (data-block table-id table dims rows selection)))
              (if (> (length values-rows) max-rows)
                  (concatenate 'string block
                               (format nil "showing the first ~d of ~d values; narrow it with area, time or filters, or raise max_rows.~%"
                                       max-rows (length values-rows)))
                  block)))))
    (estat-error (condition) (format nil "ERROR: ~a" condition))))

;;; --- the command -------------------------------------------------------------

(defun run-estat (args session-id)
  "The /estat command: the same search in the conversation, one table when a
10-digit id is typed. Bare, it asks for the words instead."
  (declare (ignore session-id))
  (let ((args (nlk:trimmed (or args ""))))
    (cond
      ((zerop (length args))
       (values "Japan's official statistics from e-Stat"
               (nle:list-dialog
                :title "e-Stat"
                :summary "search tables, then read one table's figures"
                :rows (list (nle:ask-row "Find a table" "Words" "/estat"
                                          :placeholder "人口 世帯数, or a 10-digit table id"
                                          :verb "search")))))
      ((and (>= (length args) 8) (every #'digit-char-p args))
       (table-info args))
      (t (search-tables args :limit 5)))))

;;; --- the boundary ------------------------------------------------------------

(nlk:define-contract estat-search-request
  (:doc "Search e-Stat, Japan's government statistics portal, for tables: populations, households, wages, prices. Answers table ids, titles and the survey each belongs to; call estat_table_info or estat_get_data with one. Keywords are Japanese.")
  ("query" :text :required t
           :doc "Japanese words a table title carries: 人口 世帯数, 賃金 産業, 消費者物価指数")
  ("survey" :string :default ""
            :doc "only this survey, by name (国勢調査, 消費者物価指数) or code (00200521)")
  ("year" :string :default ""
          :doc "a survey year or range: 2020, 令和2年, 2020-2023; leave it out for monthly series")
  ("area_level" :string :default ""
                :doc "national, prefecture or municipality: only tables tabulated to it")
  ("limit" :integer :default 10
           :doc "how many tables to show, 1 to 30"))

(nlk:define-contract estat-table-request
  (:doc "One e-Stat table's dimensions: the areas, periods and categories its figures can be asked by, with their names and codes. Read before a call whose names you are unsure of; it also answers the table's citation.")
  ("table_id" :text :required t
              :doc "the table id a search returned, e.g. 0003445078")
  ("dimension" :string :default ""
               :doc "only this dimension, by key (area, time, cat01) or name (地域, 男女)")
  ("search" :string :default "" :slot search-text
            :doc "only items whose name contains this text")
  ("max_items" :integer :default 12
               :doc "items listed per dimension (100 or more when one dimension is named)"))

(nlk:define-contract estat-data-request
  (:doc "The figures one e-Stat table holds, for the areas, periods and categories named by name (東京都, 2020年, latest, 女). Answers each value with its labels, then the citation to copy and e-Stat's credit; a name that matches nothing or several things answers with the closest names.")
  ("table_id" :text :required t
              :doc "the table id a search returned, e.g. 0003445078")
  ("area" :string :default ""
          :doc "areas by name or code: 東京都, 大阪府, 東京都,大阪府, children:東京都 (with its parts), level:2 (every prefecture)")
  ("time" :string :default "" :slot period
          :doc "periods: 2020年, 令和2年, 2026年8月, 2020-2023, 2020年1月~2020年12月, latest, latest:12")
  ("filters" :object
             :doc "other dimensions by name or key, each to the item name(s): {\"男女\": \"女\", \"産業分類\": \"建設業\"}")
  ("max_rows" :integer :default 50
              :doc "the most values to return, 1 to 500"))

(defun search-tool (request)
  (search-tables (estat-search-request-query request)
                  :survey (estat-search-request-survey request)
                  :year (estat-search-request-year request)
                  :area-level (estat-search-request-area-level request)
                  :limit (estat-search-request-limit request)))

(defun table-tool (request)
  (table-info (estat-table-request-table-id request)
              :dimension (estat-table-request-dimension request)
              :pattern (estat-table-request-search-text request)
              :max-items (estat-table-request-max-items request)))

(defun data-tool (request)
  (get-data (estat-data-request-table-id request)
            :area (estat-data-request-area request)
            :period (estat-data-request-period request)
            :filters (estat-data-request-filters request)
            :max-rows (estat-data-request-max-rows request)))

;;; --- the declaration ---------------------------------------------------------

(nle:define-cell estat
  (:section ("estat")
    (:guide "get a free application ID from the e-Stat My Page (register at www.e-stat.go.jp, then マイページ -> API機能（アプリケーションID発行）), paste it into app_id or name the variable holding it in app_id_env; max_rows bounds one answer")
    ("app_id" :secret
             :doc "the e-Stat application ID, written here; left empty, the variable app_id_env names is read")
    ("app_id_env" :env :default "ESTAT_APP_ID"
                 :doc "the environment variable holding the ID, read when app_id is empty")
    ("timeout_seconds" :integer :default 20 :min 1
                       :doc "how long one request to e-Stat may take")
    ("max_rows" :integer :default 50 :min 1
                :doc "values a call that names none returns; more is narrowed, not answered"))
  (:settings #'read-settings)
  (:tool "estat_search_tables" (nlk:contract 'estat-search-request) #'search-tool)
  (:tool "estat_table_info" (nlk:contract 'estat-table-request) #'table-tool)
  (:tool "estat_get_data" (nlk:contract 'estat-data-request) #'data-tool)
  (:command "estat" 'run-estat
            :description "Japan's official statistics from e-Stat"
            :argument-hint "WORDS|TABLE-ID")
  (:help :estat "estat: Japan's government statistics (e-Stat) - search tables, then read their figures" *primer*)
  (:start (lambda () (clear-meta-cache))))
