;;;; cell.lisp --- the three chart tools, /charts and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of TakeshiTGAL/hermes-plugin-charts, a Hermes Agent
;;;; plugin that drew a bar, line, stacked or share chart as a PNG, checked
;;;; every bar and printed number against the parsed table before returning
;;;; it, and put the image on the reply with a MEDIA line. The rewrite keeps
;;;; the three tools and the promise - a chart that reads well on a phone, a
;;;; unit and a source always printed, nothing returned that does not match
;;;; the data - and gives the model a Markdown image line instead of a MEDIA
;;;; line, which is how a Nodecode answer carries a picture.

(in-package #:nodecode-charts)

(nlk:define-peripheral charts :not-running t)

;;; --- the schemas the model reads ------------------------------------------------

(defparameter +data-schema+
  (nlk:make-json-object
   "type" "object"
   "description" "The table to draw. Give exactly one of values, columns + rows, text, json or path."
   "properties"
   (nlk:make-json-object
    "values" (nlk:make-json-object "type" "array"
                                    "description" "Rows, e.g. [{\"月\": \"2026-01\", \"売上\": 1200}].")
    "columns" (nlk:make-json-object "type" "array" "description" "Column names, with rows below them.")
    "rows" (nlk:make-json-object "type" "array" "description" "One array a row, in the columns' order.")
    "text" (nlk:make-json-object "type" "string"
                                  "description" "Pasted table text, header row first: CSV, TSV copied from Excel, or a Markdown table.")
    "json" (nlk:make-json-object "type" "string" "description" "A table as JSON text.")
    "path" (nlk:make-json-object "type" "string"
                                  "description" "Absolute path of a local .csv/.tsv/.txt/.json; hidden files and the agent's own home folder are refused.")
    "unit" (nlk:make-json-object "type" "string" "description" "Used when chart_spec.unit is not given.")
    "source" (nlk:make-json-object "type" "string" "description" "Used when chart_spec.source is not given.")))
  "The data member of chart_render and chart_validate.")

(defparameter +encoding-schema+
  (nlk:make-json-object
   "type" "object"
   "properties"
   (nlk:make-json-object
    "field" (nlk:make-json-object "type" "string" "description" "Column name.")
    "fields" (nlk:make-json-object "type" "array"
                                    "description" "y only: several number columns (grouped bars, several lines, or the parts of a stacked/share bar).")
    "title" (nlk:make-json-object "type" "string" "description" "Axis title; defaults to the column name.")))
  "One channel's encoding.")

(defparameter +spec-schema+
  (nlk:make-json-object
   "type" "object"
   "description" "What to draw. x is always the category/date column and y the numbers, for horizontal bars too."
   "properties"
   (nlk:make-json-object
    "chart_type" (nlk:make-json-object
                  "type" "string"
                  "description" "bar: compare categories. line: change over time. stacked_bar: totals and their parts. share_bar: composition in % (use instead of a pie chart)."
                  "enum" (vector "bar" "line" "stacked_bar" "share_bar"))
    "title" (nlk:make-json-object "type" "string" "description" "Headline. Say what the reader should see.")
    "subtitle" (nlk:make-json-object "type" "string" "description" "Optional: who, what and when is measured.")
    "unit" (nlk:make-json-object "type" "string"
                                  "description" "REQUIRED. What one number counts, as given in the data: 人, 万円, 円, %, 件, kWh, index (2020=100).")
    "source" (nlk:make-json-object "type" "string"
                                    "description" "REQUIRED. Publisher and date of the numbers. If the user typed them: 'ユーザー提供データ' or 'Data provided by the user'.")
    "encodings" (nlk:make-json-object
                 "type" "object"
                 "properties" (nlk:make-json-object
                               "x" +encoding-schema+
                               "y" +encoding-schema+
                               "color" (nlk:make-json-object
                                        "type" "object"
                                        "description" "Series column of a long table (one row per category and series)."
                                        "properties" (nlk:make-json-object
                                                      "field" (nlk:make-json-object "type" "string")))))
    "orientation" (nlk:make-json-object "type" "string" "enum" (vector "auto" "horizontal" "vertical"))
    "sort" (nlk:make-json-object "type" "string" "enum" (vector "auto" "desc" "asc" "none")
                                  "description" "auto: bars largest first, dates in time order.")
    "top_n" (nlk:make-json-object "type" "integer"
                                   "description" "Keep the first N categories after sorting; the chart notes 'top N of M'.")
    "highlight" (nlk:make-json-object "type" "array"
                                       "description" "Single-series bar: categories to color; the rest turn grey.")
    "y_zero" (nlk:make-json-object "type" "boolean"
                                    "description" "Line charts only. Default true; false lets the axis start above zero and says so in a note.")
    "notes" (nlk:make-json-object "type" "array" "description" "Footnotes printed under the chart."))
   "required" (vector "chart_type" "unit" "source"))
  "The chart_spec member of chart_render and chart_validate.")

(defparameter +render-schema+
  (list :description
        (concatenate 'string
                     "Draw a bar, line, stacked or share chart as a PNG that reads well on a phone. Every drawn bar "
                     "and every printed number is read back and compared with the input before the image is answered. "
                     "Units, 万/億 number formatting, color-blind-safe colors, an axis title with the unit and a source "
                     "line are built in; bars always start at zero. Use it whenever the user wants to see numbers as a "
                     "graph or needs a chart image for a report or a chat. To show the chart, put the returned markdown "
                     "line (![caption](path)) on a line of your own in the reply, and use alt_text as the caption. "
                     "A share chart shows a percentage, including a stacked bar. On an error, follow error.fix and call "
                     "again. Keywords: chart, graph, plot, visualize, image, report, sales, population, statistics.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "data" +data-schema+
          "chart_spec" +spec-schema+
          "output" (nlk:make-json-object
                    "type" "object"
                    "properties" (nlk:make-json-object
                                  "preset" (nlk:make-json-object "type" "string" "enum" (vector "mobile" "square" "wide")
                                                                  "description" "mobile 1080x1350 (default, for chat apps on phones), square 1080x1080, wide 1600x900 (slides and reports).")
                                  "locale" (nlk:make-json-object "type" "string" "enum" (vector "auto" "ja" "en"))
                                  "number_format" (nlk:make-json-object "type" "string" "enum" (vector "auto" "plain" "compact")
                                                                         "description" "auto writes 1,418万 / 3.2億 (or 1.4M) for large numbers.")
                                  "filename" (nlk:make-json-object "type" "string"
                                                                    "description" "Optional file name (letters, digits, - _).")))
          "field_display_names" (nlk:make-json-object "type" "object"
                                                       "description" "Column name to label to print, e.g. {\"pop\": \"Population\"}."))
         "required" (vector "data")))
  "chart_render, and chart_validate, which takes the same arguments.")

(defparameter +types-schema+
  (list :description
        (concatenate 'string
                     "List the chart types for a bar, line, stacked or share graph: when to use each, the encodings, "
                     "the data formats, the size presets and the limits. Call it when unsure which type a table wants.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "chart_type" (nlk:make-json-object
                                     "type" "string" "enum" (vector "bar" "line" "stacked_bar" "share_bar")
                                     "description" "Optional: only this type."))))
  "chart_types' schema.")

;;; --- the answers ----------------------------------------------------------------

(defun charts-directory ()
  "Where a chart is written: the section's own directory, else a charts folder
under the agent's home."
  (let ((directory (setting :directory)))
    (if (and directory (plusp (length (princ-to-string directory))))
        (uiop:ensure-directory-pathname (princ-to-string directory))
        (output-dir))))

(defun answer (object)
  (nlk:encode-json-object object))

(defun refusal-answer (failure)
  "The refusal as the JSON a model repairs from: the code, the place, the fix."
  (answer (nlk:make-json-object
           "success" :false "ok" :false
           "error_type" (chart-error-code failure)
           "error" (nlk:make-json-object
                    "code" (chart-error-code failure)
                    "message" (chart-error-message failure)
                    "where" (chart-error-where failure)
                    "fix" (chart-error-fix failure)))))

(defun internal-answer (condition)
  (answer (nlk:make-json-object
           "success" :false "ok" :false "error_type" "internal_error"
           "error" (nlk:make-json-object
                    "code" "internal_error"
                    "message" (format nil "~a: ~a" (type-of condition) condition)
                    "at" (with-output-to-string (out)
                           (ignore-errors (sb-debug:print-backtrace :stream out :count 8)))
                    "fix" (concatenate 'string
                                       "This is a bug in the charts cell, not in your data. Call chart_validate with the "
                                       "same arguments; if it passes, the drawing step is at fault.")))))

(defun chart-summary (plan)
  (nlk:make-json-object
   "chart_type" (plan-chart-type plan)
   "orientation" (plan-orientation plan)
   "title" (plan-title plan)
   "categories" (length (plan-categories plan))
   "series" (coerce (loop for s across (plan-series plan) collect s) 'vector)
   "unit" (plan-unit plan)
   "source" (plan-source plan)
   "locale" (plan-locale plan)
   "preset" (plan-preset plan)
   "size_px" (vector (plan-width plan) (plan-height plan))
   "number_format" (if (string-equal (plan-number-style plan) "plain") "plain" "compact")))

(defun planner-arguments (arguments)
  "ARGUMENTS as the tools' own boundary takes them: the decoded table a
provider's call arrives as, refilled from the flat form when a model sent it
that way."
  (let ((spec (or (nlk:json-value arguments :object "chart_spec") (nlk:make-json-object)))
        (folded (nlk:make-json-object)))
    (maphash (lambda (key value) (setf (gethash key folded) value)) spec)
    (dolist (key '("chart_type" "chartType" "type" "unit" "source" "title" "subtitle"
                   "encodings" "orientation" "top_n" "highlight" "y_zero" "notes" "note" "sort"))
      (multiple-value-bind (value present) (gethash key arguments)
        (when (and present value (null (gethash key folded)))
          (setf (gethash key folded) value))))
    (nlk:make-json-object "data" (nlk:json-value arguments :any "data")
                          "chart_spec" folded
                          "output" (nlk:json-value arguments :any "output")
                          "field_display_names" (nlk:json-value arguments :any "field_display_names"))))

;;; --- chart_render ---------------------------------------------------------------

(defun run-render (arguments)
  "chart_render: the plan drawn to a PNG, checked, and the file answered."
  (let ((preset (princ-to-string (setting :preset)))
        (locale (princ-to-string (setting :locale))))
    (handler-case
        (let* ((plan (build-plan (planner-arguments arguments) :preset preset :locale locale))
               (path (png-path (plan-title plan) :directory (charts-directory)
                                                  :filename (plan-filename plan)))
               (drawing (render-plan plan path))
               (check (verify-drawing drawing)))
          (if (string= (getf check :status) "ok")
              (let* ((caption (alt-text drawing))
                     (markdown (format nil "![~a](~a)" caption (drawing-path drawing))))
                (answer
                 (nlk:make-json-object
                  "success" t "ok" t
                  "path" (drawing-path drawing)
                  "markdown" markdown
                  "how_to_show" (concatenate 'string
                                              "Put the markdown line on a line of its own in your reply; it draws the image: "
                                              markdown ". A terminal shows the path.")
                  "alt_text" caption
                  "chart" (chart-summary plan)
                  "verification" (nlk:make-json-object
                                  "status" (getf check :status)
                                  "checked" (getf check :checked)
                                  "how" "every bar's drawn length and every printed number read back against the table")
                  "printed_notes" (coerce (plan-notes plan) 'vector)
                  "warnings" (coerce (plan-warnings plan) 'vector)
                  "inferred" (coerce (loop for (key . value) in (plan-inferred plan)
                                            collect (nlk:make-json-object "member" key "value" (princ-to-string value)))
                                     'vector))))
              (progn
                (ignore-errors (delete-file (drawing-path drawing)))
                (answer (nlk:make-json-object
                         "success" :false "ok" :false "error_type" "verification_failed"
                         "error" (nlk:make-json-object
                                  "code" "verification_failed"
                                  "message" "The drawn chart did not match the input, so it was not returned."
                                  "where" (format nil "~{~a~^; ~}" (getf check :mismatches))
                                  "fix" "This is a bug in the charts cell. Try chart_validate with the same arguments."))))))
      (chart-error (failure) (refusal-answer failure))
      (error (condition) (internal-answer condition)))))

;;; --- chart_validate -------------------------------------------------------------

(defun run-validate (arguments)
  "chart_validate: the table read and reported, nothing drawn."
  (handler-case
      (let* ((plan (build-plan (planner-arguments arguments)
                               :preset (princ-to-string (setting :preset))
                               :locale (princ-to-string (setting :locale))))
             (categories (plan-categories plan))
             (shown (min 20 (length categories)))
             (preview (loop for i below shown
                            collect (nlk:make-json-object
                                     "category" (aref categories i)
                                     "values" (coerce (loop for s across (plan-series plan)
                                                            collect (or (nth i (gethash s (plan-values plan))) :null))
                                                      'vector)))))
        (answer (nlk:make-json-object
                 "success" t "valid" t
                 "chart" (chart-summary plan)
                 "rows_preview" (coerce preview 'vector)
                 "rows_omitted" (max 0 (- (length categories) shown))
                 "printed_notes" (coerce (plan-notes plan) 'vector)
                 "warnings" (coerce (plan-warnings plan) 'vector))))
    (chart-error (failure) (refusal-answer failure))
    (error (condition) (internal-answer condition))))

;;; --- chart_types ----------------------------------------------------------------

(defparameter +guide+
  '(("bar" . "Comparing amounts across categories (prefectures, products, branches). Sorted largest first by default; horizontal when the labels are long or there are many categories, vertical otherwise. x = category column, y = number column (or several for grouped bars); color = optional series column for a long table. Always from zero; at most 40 categories.")
    ("line" . "Change over time (months, quarters, years). Dates are put in time order. x = date/period column, y = number column(s); at most 500 points. From zero by default; set y_zero false only when the change itself is the point, and the chart then says so in a note.")
    ("stacked_bar" . "Totals split into parts, when both the total and the parts matter. x = category, y = several part columns, or y = one number column plus color = part column. At most 30 categories; parts must be zero or more.")
    ("share_bar" . "Composition in percent: one 100% band a row, which is what a pie chart should have been. x = row label, y = several part columns, or y plus color for a long table. At most 15 rows and at least two parts."))
  "When to use each type, as chart_types answers it.")

(defun run-types (arguments)
  "chart_types: what each type is for, the formats and the limits."
  (handler-case
      (let* ((wanted (and (hash-table-p arguments) (nlk:json-value arguments :string "chart_type")))
             (types (if (and wanted (find-one wanted +guide+))
                        (list (cons wanted (find-one wanted +guide+)))
                        +guide+)))
        (answer (nlk:make-json-object
                 "success" t
                 "chart_types" (coerce (loop for (key . text) in types
                                              collect (nlk:make-json-object "chart_type" key "use_for" text))
                                       'vector)
                 "not_offered" (nlk:make-json-object
                                "pie/donut" "use share_bar"
                                "dual axis" "draw two charts"
                                "3d" "use bar")
                 "required" (vector "chart_spec.chart_type" "chart_spec.unit" "chart_spec.source"
                                    "data (values, columns + rows, text, json or path)")
                 "data_formats" (nlk:make-json-object
                                 "values" "[{\"month\": \"2026-01\", \"sales\": 1200}, ...]"
                                 "columns+rows" "column names, then one array a row"
                                 "text" "CSV, TSV copied from Excel, or a Markdown table; title and note lines around the table are skipped"
                                 "path" "absolute path to a local .csv/.tsv/.txt/.json (UTF-8)"
                                 "numbers" "1,234 / １２３４ / 3.5万 / 1億2000万 / △120 (negative) are read; -, …, x, *** mean no value")
                 "output" (nlk:make-json-object
                           "presets" (nlk:make-json-object "mobile" "1080x1350px" "square" "1080x1080px" "wide" "1600x900px")
                           "default_preset" (princ-to-string (setting :preset))
                           "locale" "ja | en | auto"
                           "number_format" "auto | plain | compact (万/億 or K/M)")
                 "limits" (nlk:make-json-object
                           "series" +series-max+ "bar_categories" 40 "stacked_bar_categories" 30
                           "share_bar_categories" 15 "line_categories" 500))))
    (chart-error (failure) (refusal-answer failure))
    (error (condition) (internal-answer condition))))

;;; --- /charts, and the manual ----------------------------------------------------

(defparameter +usage+
  "charts: the chart tools draw a bar, line, stacked or share chart as a PNG that reads
 well on a phone, with the unit, the source and every number checked against the
 table before the file is answered. Ask the model to chart a table, or call
 chart_render from eval: (nodecode-charts:run-render data ...) . /charts types
 lists the four types and when to use each; (help :charts) is this manual."
  "What /charts says when it is typed alone.")

(defun run-slash (args session-id)
  "The /charts command: what the cell draws, or the types it offers."
  (declare (ignore session-id))
  (running-settings)
  (let ((typed (string-downcase (nlk:trimmed (or args "")))))
    (cond ((zerop (length typed)) +usage+)
          ((member typed '("types" "type" "種類") :test #'string=)
           (with-output-to-string (out)
             (format out "charts: four types, and what each is for~%")
             (dolist (entry +guide+)
               (format out "~%  ~a~%    ~a~%" (car entry) (cdr entry)))
             (format out "~%preset ~a, locale ~a. A pie chart is answered with share_bar.~%"
                     (setting :preset) (setting :locale))))
          (t (format nil "~a~%Ask for a chart in words, or call chart_render from eval with data and a chart_spec." +usage+)))))

(defparameter +manual+
  "charts: draws a bar, line, stacked or share chart as a PNG that reads well on a
phone, from a table the model hands over - row objects, columns plus rows, pasted
CSV/TSV/Markdown, JSON text, or an absolute .csv/.tsv/.txt/.json path (hidden files
and the agent's own home folder are refused, 10 MB at most). The tool chart_render
takes data and a chart_spec with chart_type, unit and source; the unit and the
source are always printed, so both are required. Every bar's drawn length and
every printed number are read back against the table before the file is answered,
and a chart that does not match is deleted rather than returned. What is kept of
the original: bars from zero, the share band instead of a pie chart, dates in time
order, a bar sorted largest first, 万/億 number formatting (or K/M in English),
color-blind-safe colors, a note for a top-N cut, for the marks a statistics table
uses for no value, and for a value axis that does not start at zero. chart_validate
checks a table without drawing it; chart_types says what each type is for and the
limits (8 series, 40 bar categories, 30 stacked, 15 share rows, 500 line points).
The section charts holds the default preset (mobile, square or wide) and locale
(auto, ja, en), and the directory a chart is written to; by default a chart goes
to a charts folder under the agent's home, and its markdown line is
![caption](path) - that line draws the image in an answer. The pixel face is five
by seven dots, so a label in Japanese sets as boxes on the picture while the
caption carries it in words."
  "What (help :charts) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell charts
  (:section ("charts")
    (:guide "nothing here is required: charts are drawn at 1080x1350 for a phone, in the language the data is in, into a charts folder under the agent's home; /charts types lists what can be drawn")
    ("preset" :choice :options '("mobile" "square" "wide") :default "mobile"
              :doc "the size a chart is drawn at: mobile 1080x1350 for a phone, square 1080x1080, wide 1600x900 for a slide")
    ("locale" :choice :options '("auto" "ja" "en") :default "auto"
              :doc "the language of the notes and the number units; auto follows the data")
    ("directory" :path
                 :doc "where a chart is written; empty means a charts folder under the agent's home"))
  (:tool "chart_render" +render-schema+ 'run-render)
  (:tool "chart_validate" +render-schema+ 'run-validate)
  (:tool "chart_types" +types-schema+ 'run-types)
  (:command "charts" 'run-slash
            :description "What the chart tools draw, and the four chart types"
            :argument-hint "types")
  (:help :charts "charts: draw a bar, line, stacked or share chart as a PNG - chart_render, chart_types, /charts types"
         +manual+))
