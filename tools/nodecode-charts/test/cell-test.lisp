;;;; cell-test.lisp --- the cell's half: the tools, the command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs the three tools, /charts and
;;;; (help :charts), and takes all of them back on stop; chart_render draws a
;;;; PNG whose every bar and printed number read back against the table;
;;;; chart_validate and chart_types answer through the wire a provider's call
;;;; comes through; a table the cell cannot draw from is refused with the place
;;;; and the next step; the numbers a Japanese spreadsheet writes are read; and
;;;; a preset outside the section's three refuses at start.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "charts"
  (:config)
  (:help :charts)
  (:command "charts")
  (:running
   (is (string= "mobile" (nodecode-charts::setting :preset)) "the section's default preset")
   (is (string= "auto" (nodecode-charts::setting :locale)) "and its default locale"))
  (:refused ("preset" "enormous")))

(deftest charts-cell-draws-a-checked-png ()
  (with-cell-stop ((charts-start))
    (let* ((answer (charts-answer
                    "charts-call-1"
                    "{\"data\": {\"values\": [{\"pref\": \"Aichi\", \"pop\": 7541000}, {\"pref\": \"Tokyo\", \"pop\": 14178000}, {\"pref\": \"Osaka\", \"pop\": 8780000}]},
                      \"chart_spec\": {\"chart_type\": \"bar\", \"title\": \"Population\", \"unit\": \"people\", \"source\": \"Statistics Bureau\",
                                      \"encodings\": {\"x\": {\"field\": \"pref\"}, \"y\": {\"field\": \"pop\"}}}}"))
           (path (charts-member answer "path")))
      (is (eq t (charts-member answer "success")) "the chart was drawn")
      (is (charts-png-p path) "the answer's file is a PNG by its own bytes")
      (is (search "![Bar chart: Population" (charts-member answer "markdown")) "the answer carries a markdown image line")
      (is (search "](/" (charts-member answer "markdown")) "with the file it drew")
      (is (string= "ok" (charts-member (charts-member answer "verification") "status")) "every bar and printed number read back")
      (is (plusp (charts-member (charts-member answer "verification") "checked")) "and something was checked")
      (is (search "Tokyo" (charts-member answer "alt_text")) "the caption names the largest category"))))

(deftest charts-cell-the-share-band-and-the-line-are-drawn ()
  (with-cell-stop ((charts-start))
    (let ((answer (charts-answer
                   "charts-call-2"
                   "{\"data\": {\"columns\": [\"year\", \"home\", \"abroad\"],
                                \"rows\": [[\"2024\", 60, 40], [\"2025\", 72, 28]]},
                     \"chart_spec\": {\"chart_type\": \"share_bar\", \"unit\": \"%, \", \"source\": \"Sales system\",
                                     \"encodings\": {\"x\": {\"field\": \"year\"}, \"y\": {\"fields\": [\"home\", \"abroad\"]}}}}")))
      (is (eq t (charts-member answer "success")) "a share band is drawn")
      (is (string= "share_bar" (charts-member (charts-member answer "chart") "chart_type")) "as the type asked for"))
    (let ((answer (charts-answer
                   "charts-call-3"
                   "{\"data\": {\"text\": \"month,sales\\n2025-03,120\\n2025-01,100\\n2025-02,140\"},
                     \"chart_spec\": {\"chart_type\": \"line\", \"unit\": \"units\", \"source\": \"Finance\",
                                     \"encodings\": {\"x\": {\"field\": \"month\"}, \"y\": {\"field\": \"sales\"}}}}")))
      (is (eq t (charts-member answer "success")) "a line chart is drawn from pasted CSV")
      (is (string= "ok" (charts-member (charts-member answer "verification") "status")) "and its points read back"))))

(deftest charts-cell-a-table-it-cannot-draw-from-is-refused ()
  (with-cell-stop ((charts-start))
    (let ((answer (charts-answer
                   "charts-call-4"
                   "{\"data\": {\"values\": [{\"a\": \"one\", \"b\": 2}]},
                     \"chart_spec\": {\"chart_type\": \"bar\", \"source\": \"x\"}}")))
      (is (string= "missing_unit" (charts-member (charts-member answer "error") "code")) "a chart with no unit is refused, naming the fix"))
    (let ((answer (charts-answer
                   "charts-call-5"
                   "{\"data\": {\"values\": [{\"a\": \"one\", \"b\": 2}]},
                     \"chart_spec\": {\"chart_type\": \"円グラフ\", \"unit\": \"件\", \"source\": \"x\"}}")))
      (is (string= "unsupported_chart" (charts-member (charts-member answer "error") "code")) "a pie chart is answered with the share band instead")
      (is (search "share_bar" (charts-member (charts-member answer "error") "fix")) "and says so"))
    (let ((answer (charts-answer
                   "charts-call-6"
                   "{\"data\": {\"values\": [{\"a\": \"one\", \"b\": \"abc\"}, {\"a\": \"two\", \"b\": 3}]},
                     \"chart_spec\": {\"chart_type\": \"bar\", \"unit\": \"件\", \"source\": \"x\",
                                      \"encodings\": {\"x\": {\"field\": \"a\"}, \"y\": {\"field\": \"b\"}}}}")))
      (is (string= "type_mismatch" (charts-member (charts-member answer "error") "code")) "a cell that is not a number is named by its row"))))

(deftest charts-cell-validate-and-types-answer ()
  (with-cell-stop ((charts-start))
    (let ((answer (charts-answer
                   "charts-call-7"
                   "{\"data\": {\"text\": \"pref,pop\\nA,10\\nB,20\\nC,30\"},
                     \"chart_spec\": {\"chart_type\": \"bar\", \"unit\": \"people\", \"source\": \"Team\"}}"
                   "chart_validate")))
      (is (eq t (charts-member answer "valid")) "chart_validate reads a table it could draw"))
    (let ((answer (charts-answer "charts-call-8" "{}" "chart_types")))
      (is (= 4 (length (charts-member answer "chart_types"))) "chart_types lists the four types")
      (is (search "share_bar" (charts-json (charts-member answer "not_offered"))) "and what a pie chart becomes"))))

(deftest charts-cell-the-numbers-a-japanese-table-writes ()
  (with-cell-stop ((charts-start))
    (multiple-value-bind (number percent) (nodecode-charts:parse-number "1,234")
      (declare (ignore percent))
      (is (= 1234d0 number) "a thousands separator is read"))
    (multiple-value-bind (number percent) (nodecode-charts:parse-number "3.5万")
      (declare (ignore percent))
      (is (= 35000d0 number) "and a 万 multiplier"))
    (multiple-value-bind (number percent) (nodecode-charts:parse-number "△120")
      (declare (ignore percent))
      (is (= -120d0 number) "and the triangle a report writes a minus with"))
    (let ((answer (charts-answer
                   "charts-call-9"
                   "{\"data\": {\"text\": \"pref,pop\\nA,1億2000万\\nB,\\\"3,000万\\\"\"},
                     \"chart_spec\": {\"chart_type\": \"bar\", \"unit\": \"人\", \"source\": \"推計\"},
                     \"output\": {\"locale\": \"ja\"}}")))
      (is (eq t (charts-member answer "success")) "a Japanese unit and multipliers draw a chart")
      (is (search "。" (charts-member answer "alt_text")) "with a Japanese caption"))))

(deftest charts-cell-a-pasted-table-parses ()
  (with-cell-stop ((charts-start))
    (let* ((data (nlk:decode-json "{\"text\": \"month,sales\\n2025-03,120\\n2025-01,100\"}"))
           (table (nodecode-charts:load-table data)))
      (is (= 2 (length (nodecode-charts:table-rows table))) "and its two rows")
      (is (equalp (vector "month" "sales") (nodecode-charts:table-columns table)) "the header row is the columns"))))

(deftest charts-cell-a-bar-is-drawn-where-the-frame-leaves-room ()
  (with-cell-stop ((charts-start))
    (let* ((arguments (nlk:decode-json
                        "{\"data\": {\"text\": \"pref,pop\\nA,1億2000万\\nB,3,000万\"},
                          \"chart_spec\": {\"chart_type\": \"bar\", \"unit\": \"人\", \"source\": \"推計\"}}"))
           (plan (nodecode-charts:build-plan arguments))
           (drawing (nodecode-charts:render-plan
                     plan (nodecode-charts:png-path "probe" :directory (nodecode-charts:output-dir)))))
      (is (string= "vertical" (nodecode-charts:plan-orientation plan)) "two short labels draw upright bars")
      (is (plusp (length (nodecode-charts:drawing-labels drawing))) "the bar carries its number")
      (is (plusp (length (nodecode-charts:drawing-bars drawing))) "a bar was drawn"))))

(deftest charts-cell-the-command-answers-the-operator ()
  (with-cell-stop ((charts-start))
    (is (search "chart_render" (nodecode-charts:run-slash "" nil)) "bare, /charts says what it draws")
    (is (search "share_bar" (nodecode-charts:run-slash "types" nil)) "and `types` lists the four")))
