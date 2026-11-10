;;;; plan.lisp --- check a chart request and turn it into a plan the renderer draws as-is.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's spec.py: everything that can be wrong is
;;;; caught before anything is drawn, and reported with the row, the column or
;;;; the key at fault and the next step. What was kept is the original's own
;;;; judgement about a chart - bars from zero, a share band instead of a pie,
;;;; dates in time order, a bar sorted largest first, the unit and the source
;;;; always printed - and its limits: one row of the same table.

(in-package #:nodecode-charts)

(defparameter +chart-types+ '("bar" "line" "stacked_bar" "share_bar")
  "The four chart types offered. A pie chart is answered with a share band.")

(defparameter +series-max+ 8
  "The most series one chart carries: more colors than this stop being tellable apart.")

(defparameter +category-limits+
  '(("bar" . 40) ("stacked_bar" . 30) ("share_bar" . 15) ("line" . 500))
  "How many categories each type can label legibly.")

(defparameter +presets+
  '(("mobile" 1080 1350) ("square" 1080 1080) ("wide" 1600 900))
  "mobile 4:5 for a phone in a chat, square, and wide 16:9 for a slide.")

(defparameter +ja-chart+
  '(("棒" . "bar") ("棒グラフ" . "bar") ("縦棒" . "column") ("縦棒グラフ" . "column")
    ("横棒" . "barh") ("横棒グラフ" . "barh") ("折れ線" . "line") ("折れ線グラフ" . "line")
    ("推移" . "line") ("積み上げ" . "stacked") ("積み上げ棒グラフ" . "stacked")
    ("構成比" . "share") ("帯グラフ" . "share") ("シェア" . "share")
    ("円グラフ" . "pie") ("ドーナツ" . "donut"))
  "The Japanese names the original accepts, 円グラフ included so it is answered
rather than drawn.")

(defparameter +aliases+
  '(("column" "bar" "vertical") ("columns" "bar" "vertical") ("vertical_bar" "bar" "vertical")
    ("barh" "bar" "horizontal") ("horizontal_bar" "bar" "horizontal") ("bars" "bar" nil)
    ("lines" . "line") ("time_series" . "line")
    ("stacked" . "stacked_bar") ("stacked_column" . "stacked_bar")
    ("share" . "share_bar") ("100_stacked_bar" . "share_bar")
    ("percent_stacked_bar" . "share_bar") ("band" . "share_bar")
    ("composition" . "share_bar"))
  "The names people reach for, and what each means: a three-element row forces
an orientation as well as the type.")

(defparameter +refused+
  '(("pie" . "Pie and donut charts make shares hard to compare. Use chart_type 'share_bar' (a 100% band); it shows the same shares as lengths.")
    ("donut" . "Pie and donut charts make shares hard to compare. Use chart_type 'share_bar'.")
    ("doughnut" . "Pie and donut charts make shares hard to compare. Use chart_type 'share_bar'.")
    ("3d" . "3D charts distort lengths. Use 'bar'.")
    ("dual_axis" . "Two value axes on one chart invite false comparisons. Draw two charts."))
  "What is refused, and the chart to draw instead.")

(defstruct plan
  (chart-type "bar") (orientation "horizontal")
  (categories #()) (series #()) (values (make-hash-table :test #'equal))
  (unit "") (source "") (title "") (subtitle "")
  (x-title "") (y-title "") (series-labels nil)
  (locale "en") (number-style "plain") (decimals 0)
  (preset "mobile") (width 1080) (height 1350)
  (highlight nil) (notes nil) (warnings nil) (inferred nil)
  (filename "") (category-total 0))

;;; --- small readers ---------------------------------------------------------------

(defun chart-key (text)
  "TEXT as the key a chart type, alias or refused name is looked up by."
  (let ((out (make-string-output-stream))
        (last-underscore nil))
    (loop for character across (string-downcase (string-trim " " (string text)))
          do (if (find character " %-　" :test #'char=)
                 (unless last-underscore (write-char #\_ out) (setf last-underscore t))
                 (progn (write-char character out) (setf last-underscore nil))))
    (string-trim "_" (get-output-stream-string out))))

(defun find-one (key table)
  (cdr (assoc key table :test #'string=)))

(defun field-name (encoding)
  "The column (or columns) ENCODING names: a string, a list, or an object
with field or fields, as the original's reader takes them."
  (cond ((null encoding) nil)
        ((stringp encoding) encoding)
        ((and (vectorp encoding) (not (stringp encoding)))
         (loop for each across encoding collect (field-name each)))
        ((hash-table-p encoding)
         (or (nlk:json-value encoding :string "field")
             (let ((fields (nlk:json-value encoding :array "fields")))
               (and fields (loop for each across fields when (stringp each) collect each)))))
        (t nil)))

(defun encoding-title (encoding)
  (if (hash-table-p encoding) (or (nlk:json-value encoding :string "title") "") ""))

(defun encoding-unit (encoding)
  (if (hash-table-p encoding) (or (nlk:json-value encoding :string "unit") "") ""))

(defun column-index (table column where)
  (let ((at (position (string column) (table-columns table) :test #'string=)))
    (unless at
      (refuse-chart "unknown_column" (format nil "There is no column ~s." column) :where where
            :fix (format nil "Use one of the columns in the data: ~{~s~^, ~}."
                         (coerce (table-columns table) 'list))))
    at))

(defun numeric-column-p (table column)
  "T when COLUMN holds numbers, a cell that means no value aside."
  (let ((at (position column (table-columns table) :test #'string=))
        (seen nil))
    (and at
         (loop for row across (table-rows table)
               always (handler-case
                          (multiple-value-bind (number percent) (parse-number (aref row at))
                            (declare (ignore percent))
                            (when number (setf seen t))
                            t)
                        (not-a-number () nil)))
         seen)))

(defun read-column (table column unit problems)
  "COLUMN's numbers, a cell that means no value as NIL; PROBLEMS collects the
cells that are present but are not numbers."
  (let ((at (position column (table-columns table) :test #'string=))
        (numbers nil) (marks nil) (percent nil))
    (loop for row across (table-rows table)
          for label across (table-labels table)
          for raw = (aref row at)
          do (handler-case
                 (multiple-value-bind (number percentp) (parse-number raw unit)
                   (when percentp (setf percent t))
                   (when (and (null number) (plusp (length (cell-text raw))))
                     (pushnew (cell-text raw) marks :test #'string=))
                   (push number numbers))
               (not-a-number ()
                 (push (format nil "~a, column ~s: ~s is not a number" label column (cell-text raw)) problems)
                 (push nil numbers))))
    (values (nreverse numbers) marks percent (nreverse problems))))

;;; --- dates -----------------------------------------------------------------------

(defun digit-runs (text)
  (let ((runs nil) (run (make-string-output-stream)))
    (flet ((cut ()
             (let ((chunk (get-output-stream-string run)))
               (when (plusp (length chunk)) (push chunk runs)))))
      (loop for character across text
            do (if (digit-char-p character) (write-char character run) (cut)))
      (cut))
    (nreverse runs)))

(defun date-key (label)
  "(YEAR MONTH DAY) when LABEL reads as a date or a period, else NIL."
  (let* ((text (normalize-text (string label)))
         (runs (digit-runs text)))
    (cond ((and runs (= (length (first runs)) 4))
           (let ((year (parse-integer (first runs)))
                 (month 0) (day 0))
             (when (second runs)
               (let ((second-value (parse-integer (second runs))))
                 (setf month (if (find-if (lambda (c) (find c "qQＱ" :test #'char=)) text)
                                 (* 3 second-value)
                                 second-value))))
             (when (third runs) (setf day (parse-integer (third runs))))
             (when (<= 0 month 12) (list year month day))))
          ((and (= (length runs) 1) (= (length (first runs)) 1) (find #\月 text))
           (let ((month (parse-integer (first runs))))
             (when (<= 1 month 12) (list 0 month 0)))))))

(defun triple< (a b)
  "(YEAR MONTH DAY) A before B."
  (or (< (first a) (first b))
      (and (= (first a) (first b))
           (or (< (second a) (second b))
               (and (= (second a) (second b)) (< (third a) (third b)))))))

(defun time-labels-p (labels)
  "The date keys of LABELS when every one reads as a date, else NIL."
  (let ((keys (mapcar #'date-key labels)))
    (and labels (every #'identity keys) keys)))

(defun display-length (text)
  "How wide TEXT sets: a wide character twice, a narrow one once."
  (loop for character across (string text)
        sum (if (> (char-code character) #x2000) 2 1)))

(defun japanese-p (text)
  (loop for character across (string text)
        for code = (char-code character)
        thereis (or (<= #x3040 code #x30FF) (<= #x4E00 code #x9FFF) (<= #xFF01 code #xFF60))))

(defun preset-size (preset)
  (let ((entry (find preset +presets+ :key #'first :test #'string=)))
    (values (second entry) (third entry))))

;;; --- the axes --------------------------------------------------------------------

(defun chart-type-of (spec)
  "SPEC's chart type and orientation, as (VALUES KEY ORIENTATION TEXT)."
  (let* ((raw (or (nlk:json-value spec :string "chart_type")
                  (nlk:json-value spec :string "chartType")
                  (nlk:json-value spec :string "type")
                  ""))
         (raw (or (find-one (string-trim " " raw) +ja-chart+) raw))
         (key (chart-key raw))
         (orientation (string-downcase (or (nlk:json-value spec :string "orientation") "auto"))))
    (when (find-one key +refused+)
      (refuse-chart "unsupported_chart" (format nil "chart_type ~s is not offered." raw)
            :where "chart_spec.chart_type" :fix (find-one key +refused+)))
    (let ((alias (assoc key +aliases+ :test #'string=)))
      (when alias
        (setf key (if (consp (cdr alias)) (second alias) (cdr alias)))
        (when (and (consp (cdr alias)) (third alias) (string-equal orientation "auto"))
          (setf orientation (third alias)))))
    (when (zerop (length key))
      (refuse-chart "missing_chart_type" "chart_spec.chart_type is missing." :where "chart_spec.chart_type"
            :fix (format nil "Use one of: ~{~a~^, ~}. Call chart_types for when to use each." +chart-types+)))
    (unless (member key +chart-types+ :test #'string=)
      (refuse-chart "unsupported_chart" (format nil "chart_type ~s is not one of ~{~a~^, ~}." raw +chart-types+)
            :where "chart_spec.chart_type"
            :fix "Pick the closest supported type; call chart_types for guidance."))
    (unless (member orientation '("auto" "horizontal" "vertical") :test #'string-equal)
      (refuse-chart "bad_spec" "orientation must be auto, horizontal or vertical." :where "chart_spec.orientation"))
    (values key (string-downcase orientation))))

(defun resolve-axes (table spec warnings)
  "The columns a chart is drawn from, as (VALUES X-NAME Y-COLUMNS COLOR-NAME):
what the encodings name, or what the table's own shape makes plain."
  (let* ((enc (or (nlk:json-value spec :object "encodings") (nlk:make-json-object)))
         (x-name (field-name (nlk:json-value enc :any "x")))
         (y-name (field-name (nlk:json-value enc :any "y")))
         (c-name (or (field-name (nlk:json-value enc :any "color"))
                     (field-name (nlk:json-value enc :any "series")))))
    (when (and x-name (listp x-name))
      (refuse-chart "bad_spec" "encodings.x takes one column (the categories or dates)."
            :where "chart_spec.encodings.x"))
    (when (and x-name y-name (listp y-name) (= 1 (length y-name)))
      (setf y-name (first y-name)))
    (let ((numeric-columns (loop for column across (table-columns table)
                                 when (numeric-column-p table column) collect column)))
      ;; x and y swapped - a habit from horizontal bars elsewhere: swap them back.
      (when (and (stringp x-name) (stringp y-name)
                 (find x-name (table-columns table) :test #'string=)
                 (find y-name (table-columns table) :test #'string=)
                 (numeric-column-p table x-name) (not (numeric-column-p table y-name)))
        (rotatef x-name y-name)
        (push (concatenate 'string
                           "encodings.x held the numbers and encodings.y the labels; they were swapped. "
                           "In this tool x is always the category/date column and y the numbers, "
                           "for horizontal bars too.")
              warnings))
      (unless x-name
        (let ((candidates (remove-if (lambda (column)
                                       (or (member column numeric-columns :test #'string=)
                                           (and c-name (string= column c-name))))
                                     (coerce (table-columns table) 'list))))
          (unless candidates
            (refuse-chart "missing_x" "Could not tell which column holds the categories or dates."
                  :where "chart_spec.encodings.x"
                  :fix (format nil "Set encodings.x to one of ~{~s~^, ~}." (coerce (table-columns table) 'list))))
          (setf x-name (first candidates))))
      (column-index table x-name "chart_spec.encodings.x")
      (when c-name (column-index table c-name "chart_spec.encodings.color"))
      (let ((y-columns
              (cond ((null y-name)
                     (let ((rest (remove-if (lambda (column)
                                              (or (string= column x-name)
                                                  (and c-name (string= column c-name))))
                                            numeric-columns)))
                       (unless rest
                         (refuse-chart "missing_y" "No numeric column was found to draw."
                               :where "chart_spec.encodings.y"
                               :fix "Set encodings.y to the column with the numbers, and check that it holds numbers."))
                       rest))
                    ((listp y-name)
                     (dolist (column y-name) (column-index table column "chart_spec.encodings.y"))
                     y-name)
                    (t (column-index table y-name "chart_spec.encodings.y") (list y-name)))))
        (when (and c-name (cdr y-columns))
          (refuse-chart "bad_spec" "Use either several y columns (wide table) or a color column (long table), not both."
                :where "chart_spec.encodings"
                :fix "Keep encodings.color and a single encodings.y, or drop encodings.color."))
        (values x-name y-columns c-name)))))

;;; --- the reshape -----------------------------------------------------------------

(defun reshape (table x-name y-columns c-name unit)
  "The categories, series and numbers, as (VALUES CATEGORIES SERIES VALUES MARKS PERCENT)."
  (let* ((x-index (position x-name (table-columns table) :test #'string=))
         (x-values (loop for row across (table-rows table) collect (cell-text (aref row x-index))))
         (categories nil) (series nil) (values (make-hash-table :test #'equal))
         (marks nil) (percent nil) (problems nil) (parsed (make-hash-table :test #'equal)))
    (loop for label across (table-labels table)
          for xv in x-values
          do (when (zerop (length xv))
               (refuse-chart "missing_label" (format nil "~a has an empty ~s." label x-name)
                     :where (format nil "~a, column ~s" label x-name)
                     :fix "Every row needs a category or date label; remove the row or fill it in.")))
    (dolist (column y-columns)
      (multiple-value-bind (numbers column-marks column-percent column-problems)
          (read-column table column unit nil)
        (setf (gethash column parsed) numbers)
        (setf marks (union marks column-marks :test #'string=))
        (setf problems (append problems column-problems))
        (when column-percent (setf percent t))))
    (when problems
      (refuse-chart "type_mismatch"
            (format nil "~d value(s) in the number column(s) are not numbers~@[ (and ~d more)~]."
                    (length problems) (let ((more (- (length problems) 5))) (and (plusp more) more)))
            :where (format nil "~{~a~^; ~}" (subseq problems 0 (min 5 (length problems))))
            :fix "Fix or remove those cells. Thousands separators, full-width digits, 万/億 and △ for negatives are fine; leave a cell empty (or '-') when there is no value."))
    (if c-name
        (let* ((y (first y-columns))
               (c-index (position c-name (table-columns table) :test #'string=))
               (c-values (loop for row across (table-rows table) collect (cell-text (aref row c-index))))
               (seen (make-hash-table :test #'equal)))
          (loop for xv in x-values
                do (unless (member xv categories :test #'string=)
                     (setf categories (append categories (list xv)))))
          (loop for cv in c-values
                do (unless (member cv series :test #'string=)
                     (setf series (append series (list cv)))))
          (dolist (s series) (setf (gethash s values) (make-list (length categories) :initial-element nil)))
          (loop for label across (table-labels table)
                for xv in x-values
                for cv in c-values
                for v in (gethash y parsed)
                for at = (position xv categories :test #'string=)
                do (let ((pair (format nil "~a/~a" xv cv)))
                     (multiple-value-bind (previous present) (gethash pair seen)
                       (when present
                         (refuse-chart "duplicate_rows" (format nil "~s / ~s appears in two rows." xv cv)
                               :where (format nil "~a and ~a" previous label)
                               :fix "Each category and series pair needs one row. Sum or average the duplicates first."))
                       (setf (gethash pair seen) label))
                     (setf (nth at (gethash cv values)) v))))
        (let ((first-seen (make-hash-table :test #'equal)))
          (loop for label across (table-labels table)
                for xv in x-values
                do (multiple-value-bind (previous present) (gethash xv first-seen)
                     (when present
                       (refuse-chart "duplicate_rows" (format nil "~s appears in more than one row." xv)
                             :where (format nil "~a and ~a, column ~s" previous label x-name)
                             :fix "Each category or date needs one row. Sum the duplicates first, or name a series column in encodings.color if the rows belong to different series."))
                     (setf (gethash xv first-seen) label)))
          (setf categories x-values
                series y-columns)
          (dolist (column y-columns)
            (setf (gethash column values) (copy-list (gethash column parsed))))))
    (when (> (length series) +series-max+)
      (refuse-chart "too_many_series"
            (format nil "~d series is more than ~d distinguishable colors." (length series) +series-max+)
            :where (if c-name "chart_spec.encodings.color" "chart_spec.encodings.y")
            :fix (format nil "Keep the ~d largest series and add the rest together as one, or split the data into several charts."
                         (1- +series-max+))))
    (when (every (lambda (s) (every #'null (gethash s values))) series)
      (refuse-chart "empty_data" "Every value is empty, so there is nothing to draw." :where "data"
            :fix "Check that encodings.y names the column with the numbers."))
    (values categories series values marks percent)))

(defun reorder (categories values series order)
  "(VALUES CATEGORIES VALUES) with every list put in ORDER, a list of indices."
  (dolist (s series)
    (setf (gethash s values) (mapcar (lambda (i) (nth i (gethash s values))) order)))
  (values (mapcar (lambda (i) (nth i categories)) order) values))

(defun totals (categories values series)
  (loop for i below (length categories)
        collect (loop for s in series sum (or (nth i (gethash s values)) 0d0))))

(defun check-stack (key categories series values)
  "A stacked or share chart's parts must be zero or more; a share needs every
part and a total above zero."
  (when (member key '("stacked_bar" "share_bar") :test #'string=)
    (dolist (s series)
      (loop for cat across categories
            for v in (gethash s values)
            do (when (and v (minusp (coerce v 'double-float)))
                 (refuse-chart "negative_in_stack"
                       (format nil "~s for ~s is negative; stacked parts must be zero or more." s cat)
                       :where (format nil "category ~s, series ~s" cat s)
                       :fix "Use chart_type 'bar' (grouped bars show negatives honestly).")))))
  (when (string= key "share_bar")
    (loop for i below (length categories)
          for cat = (aref categories i)
          do (let ((parts (mapcar (lambda (s) (nth i (gethash s values))) series)))
               (when (some #'null parts)
                 (refuse-chart "missing_part"
                       (format nil "~s has no value for ~{~s~^, ~}." cat
                               (loop for s in series for p in parts when (null p) collect s))
                       :where (format nil "category ~s" cat)
                       :fix "A share needs every part. Fill in 0 if the part is truly zero."))
               (when (<= (reduce #'+ parts :key (lambda (p) (or p 0d0))) 0d0)
                 (refuse-chart "zero_total" (format nil "The parts of ~s add up to 0, so there is no share." cat)
                       :where (format nil "category ~s" cat)
                       :fix "Remove that row or check the numbers."))))
    (when (< (length series) 2)
      (refuse-chart "too_few_parts" "A share band needs at least two parts." :where "chart_spec.encodings"
            :fix "Pass the parts as several y columns, or as rows with encodings.color."))))

;;; --- the plan --------------------------------------------------------------------

(defun build-plan (arguments &key (preset "mobile") (locale "auto"))
  "ARGUMENTS as a PLAN, or a CHART-ERROR naming what to fix."
  (unless (hash-table-p arguments)
    (refuse-chart "bad_spec" "Arguments must be an object with data and chart_spec." :where "arguments"))
  (let ((data (nlk:json-value arguments :object "data"))
        (spec (or (nlk:json-value arguments :object "chart_spec") (nlk:make-json-object)))
        (output (or (nlk:json-value arguments :object "output") (nlk:make-json-object)))
        (display (or (nlk:json-value arguments :object "field_display_names") (nlk:make-json-object)))
        (warnings nil) (inferred nil))
    (unless data
      (refuse-chart "empty_data" "data is missing." :where "data"
            :fix "Send data as values, columns + rows, text, json or path."))
    ;; A top-level chart_type, unit and source is the same call as chart_spec.
    (let ((folded (nlk:make-json-object)))
      (maphash (lambda (key value) (setf (gethash key folded) value)) spec)
      (dolist (key '("chart_type" "chartType" "type" "unit" "source" "title" "subtitle"
                     "encodings" "orientation" "top_n" "highlight" "y_zero" "notes" "note" "sort"))
        (multiple-value-bind (value present) (gethash key arguments)
          (when (and present value (null (gethash key folded)))
            (setf (gethash key folded) value))))
      (setf spec folded))
    (let ((table (load-table data)))
      (multiple-value-bind (key orientation) (chart-type-of spec)
        (multiple-value-bind (x-name y-columns c-name) (resolve-axes table spec warnings)
          (let* ((enc (or (nlk:json-value spec :object "encodings") (nlk:make-json-object)))
                 (unit (string-trim " " (or (nlk:json-value spec :string "unit")
                                             (encoding-unit (nlk:json-value enc :any "y"))
                                             (getf (table-hints table) :unit) "")))
                 (source (string-trim " " (or (nlk:json-value spec :string "source")
                                               (getf (table-hints table) :source) ""))))
            (when (zerop (length unit))
              (refuse-chart "missing_unit" "chart_spec.unit is missing. Every chart prints its unit on the value axis."
                    :where "chart_spec.unit"
                    :fix "Set unit to what one number counts, e.g. '人', '万円', '円', '%', '件', 'kWh', or 'index (2020=100)'."))
            (when (zerop (length source))
              (refuse-chart "missing_source" "chart_spec.source is missing. Every chart prints where its numbers come from."
                    :where "chart_spec.source"
                    :fix "Set source to the publisher and date, or 'ユーザー提供データ' / 'Data provided by the user' when the user typed the numbers."))
            (multiple-value-bind (categories series values marks percent) (reshape table x-name y-columns c-name unit)
              (let ((categories (coerce categories 'vector)) (sort (string-downcase (or (nlk:json-value spec :string "sort") "auto"))))
                (unless (member sort '("auto" "desc" "asc" "none") :test #'string=)
                  (refuse-chart "bad_spec" "sort must be auto, desc, asc or none." :where "chart_spec.sort"))
                (when (string-equal sort "auto")
                  (setf sort (if (and (string= key "bar") (= 1 (length series))
                                      (not (time-labels-p (coerce categories 'list))))
                                 "desc" "none")))
                ;; dates first, in time order
                (when (string= key "line")
                  (let ((keys (time-labels-p (coerce categories 'list))))
                    (when keys
                      (let* ((index (loop for i below (length categories) collect i))
                             (order (stable-sort index #'triple<
                                                 :key (lambda (i) (let ((k (nth i keys)))
                                                                    (list (first k) (second k) (third k)))))))
                        (unless (equal order index)
                          (push "The dates were not in order; they were sorted oldest to newest." warnings)
                          (setf categories (map 'vector (lambda (i) (aref categories i)) order))
                          (setf values (nth-value 1 (reorder (coerce categories 'list) values series order)))))))
                  (when (not (string= sort "none"))
                    (push "sort is ignored for line charts; dates stay in time order." warnings)
                    (setf sort "none")))
                (when (member sort '("desc" "asc") :test #'string=)
                  (let* ((order (stable-sort (loop for i below (length categories) collect i)
                                              (if (string= sort "desc") #'> #'<)
                                              :key (lambda (i) (nth i (totals categories values series))))))
                    (setf categories (map 'vector (lambda (i) (aref categories i)) order))
                    (setf values (nth-value 1 (reorder (coerce categories 'list) values series order)))))
                (let* ((category-total (length categories))
                       (top-n (nlk:json-value spec :integer "top_n"))
                       (limit (find-one key +category-limits+)))
                  (when top-n
                    (when (or (minusp top-n) (zerop top-n))
                      (refuse-chart "bad_args" "top_n must be a whole number of 1 or more, such as 10."
                            :where "chart_spec.top_n"))
                    (when (< top-n category-total)
                      (when (and (string= sort "none") (not (string= key "line")))
                        (push "top_n kept the first rows as given; set sort to 'desc' to keep the largest." warnings))
                      (setf categories (subseq categories 0 top-n))
                      (dolist (s series)
                        (setf (gethash s values) (subseq (gethash s values) 0 top-n)))))
                  (when (> (length categories) limit)
                    (refuse-chart "too_many_categories"
                          (format nil "~d categories is more than a ~a chart can label legibly (limit ~d)."
                                  (length categories) key limit)
                          :where "chart_spec.top_n"
                          :fix "Set top_n (for example 10) with sort 'desc', or group the small ones into one 'other' series."))
                  (check-stack key categories series values)
                  (present-plan key orientation categories series values unit source spec output
                                display enc marks percent category-total preset locale
                                warnings inferred table x-name))))))))))

(defun present-plan (key orientation categories series values unit source spec output
                     display enc marks percent category-total preset locale warnings inferred
                     table x-name)
  "The plan's presentation: locale, preset, number style, titles, notes."
  (declare (ignore table))
  (let* ((series (coerce series 'vector))
         (loc (string-downcase (or (nlk:json-value output :string "locale") locale "auto")))
         (loc (if (string-equal loc "auto")
                  (if (japanese-p (format nil "~a ~a ~a ~a" (or (nlk:json-value spec :string "title") "")
                                          unit source
                                          (format nil "~{ ~a~}"
                                                        (loop for i below (min 5 (length categories))
                                                              collect (aref categories i)))))
                      "ja" "en")
                  loc))
         (preset (string-downcase (or (nlk:json-value output :string "preset") preset "mobile")))
         (number-format (string-downcase (or (nlk:json-value output :string "number_format") "auto"))))
    (unless (member loc '("ja" "en") :test #'string=)
      (refuse-chart "bad_spec" "output.locale must be ja, en or auto." :where "output.locale"))
    (unless (find preset +presets+ :key #'first :test #'string=)
      (refuse-chart "bad_spec" (format nil "output.preset must be one of ~{~a~^, ~}." (mapcar #'first +presets+))
            :where "output.preset" :fix "mobile (4:5, for phones), square, or wide (16:9, for slides)."))
    (unless (member number-format '("auto" "plain" "compact") :test #'string=)
      (refuse-chart "bad_spec" "output.number_format must be auto, plain or compact." :where "output.number_format"))
    (let* ((ja (string= loc "ja"))
           (numbers (loop for s across series append (remove nil (gethash s values))))
           (style (if (string= key "share_bar") "plain" (choose-style numbers unit number-format loc)))
           (decimals (if (string= key "share_bar") 1 (decimals-for numbers)))
           (y-zero (let ((value (gethash "y_zero" spec)))
                     (if (null value) t (not (null value)))))
           (highlight (let ((value (or (nlk:json-value spec :array "highlight")
                                       (nlk:json-value spec :string "highlight"))))
                        (cond ((null value) nil)
                              ((stringp value) (list value))
                              (t (loop for each across value collect (cell-text each)))))))
      (setf orientation
            (cond ((string= key "line") "vertical")
                  ((and (string-equal orientation "auto") (string= key "share_bar")) "horizontal")
                  ((string-equal orientation "auto")
                   (let ((longest (loop for c across categories maximize (display-length c)))
                         (narrow (not (string= preset "wide"))))
                     (if (or (> longest 4) (> (length categories) (if narrow 6 12)))
                         "horizontal" "vertical")))
                  (t orientation)))
      (when (and (string= key "share_bar") (string= orientation "vertical"))
        (push "share_bar is always drawn horizontally so the labels stay readable." warnings)
        (setf orientation "horizontal"))
      (when (and (not y-zero) (not (string= key "line")))
        (refuse-chart "truncated_axis" "Bars must start at zero; a cut axis makes small differences big."
              :where "chart_spec.y_zero"
              :fix "Remove y_zero, or use chart_type 'line' if the change over time is the point."))
      (dolist (h highlight)
        (unless (find h categories :test #'string=)
          (refuse-chart "unknown_category" (format nil "highlight names ~s, which is not a category." h)
                :where "chart_spec.highlight"
                :fix (format nil "Use labels exactly as in the data, e.g. ~{~s~^, ~}."
                             (loop for i below (min 3 (length categories)) collect (aref categories i))))))
      (when (and highlight (not (and (string= key "bar") (= 1 (length series)))))
        (refuse-chart "bad_spec" "highlight works on single-series bar charts."
              :where "chart_spec.highlight" :fix "Remove highlight for this chart type."))
      (let* ((x-title (let ((title (encoding-title (nlk:json-value enc :any "x")))
                            (label (gethash (encoding-field enc "x") display)))
                        (cond ((and title (plusp (length title))) title)
                              ((and label (plusp (length (cell-text label)))) (cell-text label))
                              ((plusp (length (encoding-field enc "x"))) (encoding-field enc "x"))
                              ((and x-name (plusp (length (string x-name)))) (string x-name))
                              (t "category"))))
             (series-labels (loop for s across series
                                  collect (cons s (let ((label (gethash s display)))
                                                    (if (and label (plusp (length (cell-text label))))
                                                        (cell-text label) s)))))
             (y-title (if (= 1 (length series))
                          (let ((label (gethash (first (coerce series 'list)) display)))
                            (if (and label (plusp (length (cell-text label)))) (cell-text label) ""))
                          ""))
             (title (string-trim " " (or (nlk:json-value spec :string "title") "")))
             (title (if (plusp (length title))
                        title
                        (let ((what (if (= 1 (length series)) (first (coerce series 'list)) "")))
                          (prog1 (if ja
                                     (if (plusp (length what)) (format nil "~a別の~a" x-title what)
                                         (format nil "~a別" x-title))
                                     (if (plusp (length what)) (format nil "~a by ~a" what x-title)
                                         (format nil "By ~a" x-title)))
                            (push (cons "title" title) inferred))))))
        (let ((notes (loop for note across (or (nlk:json-value spec :array "notes")
                                               (let ((one (nlk:json-value spec :string "note")))
                                                 (if one (vector one) #())))
                           when (plusp (length (string-trim " " (cell-text note))))
                             collect (cell-text note))))
          (let ((top-n (nlk:json-value spec :integer "top_n")))
            (when (and top-n (< top-n category-total))
              (setf notes (append notes (list (if ja
                                                  (format nil "上位~d件を表示（全~d件）" top-n category-total)
                                                  (format nil "Top ~d of ~d shown" top-n category-total)))))))
          (when marks
            (let ((listed (format nil "~{~a~^, ~}" (sort (copy-list marks) #'string<))))
              (setf notes (append notes (list (if ja (format nil "「~a」は値なしとして扱った" listed)
                                                  (format nil "Cells marked ~a are drawn as no value" listed)))))))
          (let ((missing (loop for s across series sum (count nil (gethash s values)))))
            (when (and (plusp missing) (null marks))
              (setf notes (append notes (list (cond ((member key '("bar" "stacked_bar") :test #'string=)
                                                     (if ja "空欄は値なし（棒を描かない）" "Empty cells are drawn as no bar"))
                                                    ((string= key "line")
                                                     (if ja "値のない月・期間は線を切っている" "Gaps in the line are periods with no value"))
                                                    (t (if ja "空欄は値なし" "Empty cells are drawn as no value"))))))))
          (when (and percent (not (member (string-trim " " unit) '("%" "％" "percent" "パーセント") :test #'string=)))
            (push (format nil "Some cells end in % but unit is ~s; check that the unit is right." unit) warnings))
          (when (string= key "share_bar")
            (setf notes (append notes (list (if ja "各行の合計を100%とした構成比" "Each bar adds up to 100%")))))
          (when (not y-zero)
            (setf notes (append notes (list (if ja "縦軸は0から始まっていない" "The value axis does not start at zero")))))
          (multiple-value-bind (width height) (preset-size preset)
            (make-plan :chart-type key :orientation orientation
                       :categories categories :series (coerce series 'vector) :series-labels series-labels
                       :values values :unit unit :source source :title title
                       :subtitle (or (nlk:json-value spec :string "subtitle") "")
                       :x-title (if (plusp (length x-title)) x-title "category")
                       :y-title y-title
                       :locale loc :number-style style :decimals decimals
                       :preset preset :width width :height height
                       :highlight highlight :notes notes
                       :warnings (reverse warnings) :inferred (reverse inferred)
                       :filename (or (nlk:json-value output :string "filename") "")
                       :category-total category-total)))))))

(defun encoding-field (enc channel)
  "The single column CHANNEL names, or the channel's own column name."
  (let ((name (field-name (nlk:json-value enc :any channel))))
    (cond ((stringp name) name)
          ((and (consp name) (stringp (first name))) (first name))
          (t ""))))

