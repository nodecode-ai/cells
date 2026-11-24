;;;; cell-test.lisp --- the estat cell, end to end over a scripted e-Stat.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- lifecycle, settings and the tools' shape ---------------------------------

(define-cell-lifecycle-tests "estat"
  (:config "app_id" +key+)
  (:help :estat)
  (:command "estat")
  (:running
   (is (nle::find-tool "estat_search_tables") "the search tool is registered")
   (is (nle::find-tool "estat_table_info") "the table tool is registered")
   (is (nle::find-tool "estat_get_data") "the data tool is registered")
   (is (equal +key+ (nodecode-estat::setting :app-id)) "the ID the section carries")
   (is (= 50 (nodecode-estat::setting :max-rows)) "and one answer is bounded"))
  (:stopped
   (is (null (nle::find-tool "estat_search_tables")) "stop takes the search tool back out")
   (is (null (nle::find-tool "estat_get_data")) "and the data tool"))
  (:refused ("app_id" "" "app_id_env" "UNSET_ESTAT_APP_ID")
            ("app_id" +key+ "timeout_seconds" 0)
            ("app_id" +key+ "max_rows" 0))
  (:idle nodecode-estat:estat-error
         (nodecode-estat:search-tables "人口")
         (nodecode-estat:get-data "0003445078")))

(deftest estat-cell-without-an-application-id-refuses ()
  ;; The one setting nobody can work without: a section naming no ID, over a
  ;; variable that holds none, is refused at start, as the original offered
  ;; no tool without ESTAT_APP_ID.
  (let ((condition (signals-error nlk:config-refusal
                                  (estat-start "app_id" ""
                                               "app_id_env" "UNSET_ESTAT_APP_ID"))))
    (is (cl:search "no application ID" (princ-to-string condition)) "the refusal names what is missing")
    (is (null (nle::find-tool "estat_search_tables")) "and installs nothing")))

(deftest estat-cell-a-call-with-no-table-id-refuses ()
  ;; The member every figure call requires: refused before the body runs, so
  ;; the model reads which member to add.
  (with-estat ()
    (is (signals-error nlk:contract-refusal (estat-decode-data-call "area" "東京都")) "a call with no table_id is refused")
    (is (null *calls*) "and nothing was asked of e-Stat")))

;;; --- the search ---------------------------------------------------------------

(deftest estat-cell-search-asks-e-stat-and-lists-tables ()
  (with-estat ()
    (let ((text (estat-call-tool "estat_search_tables" "{\"query\":\"人口 世帯数\"}")))
      (is (= 1 (length *calls*)) "one search is one request")
      (is (equal "getStatsList" (endpoint-of (first *calls*))) "of the listing endpoint")
      (is (equal +key+ (param (first *calls*) "appId")) "carrying the section's ID")
      (is (equal "人口 世帯数" (param (first *calls*) "searchWord")) "and the words asked for")
      (is (cl:search "0003445078" text) "the block names the first table's id")
      (is (cl:search "人口推計" text) "and its survey")
      (is (cl:search "総務省" text) "and who published it")
      (is (cl:search "estat_table_info(table_id)" text) "and what to call next"))))

(deftest estat-cell-search-passes-the-year-and-area-level-through ()
  (with-estat ()
    (estat-call-tool "estat_search_tables"
                      "{\"query\":\"人口\",\"year\":\"令和2年\",\"area_level\":\"prefecture\",\"limit\":2}")
    (is (equal "2020" (param (first *calls*) "surveyYears")) "the era year goes as its western year")
    (is (equal "2" (param (first *calls*) "collectArea")) "and the area level as its code")
    (is (equal "2" (param (first *calls*) "limit")) "and the cap is the call's")))

;;; --- the table -----------------------------------------------------------------

(deftest estat-cell-table-info-names-the-dimensions ()
  (with-estat ()
    (let ((text (estat-call-tool "estat_table_info" "{\"table_id\":\"0003445078\"}")))
      (is (equal "getMetaInfo" (endpoint-of (first *calls*))) "one metadata request")
      (is (cl:search "総務省" text) "the block names the publisher")
      (is (cl:search "地域 (area)" text) "and the area dimension")
      (is (cl:search "東京都" text) "with the names a call may use")
      (is (cl:search "男女 (cat01)" text) "and every other dimension")
      (is (cl:search "estat_get_data(table_id=\"0003445078\"" text) "and a ready call"))))

;;; --- the figures ---------------------------------------------------------------

(deftest estat-cell-get-data-resolves-names-and-cites-e-stat ()
  (with-estat ()
    (let ((text (estat-call-tool "estat_get_data"
                                  "{\"table_id\":\"0003445078\",\"area\":\"東京都\",\"time\":\"2020年\"}")))
      (is (= 2 (length *calls*)) "metadata, then the figures")
      (let ((ask (first *calls*)))
        (is (equal "getStatsData" (endpoint-of ask)) "the second request is the data one")
        (is (equal "13000" (param ask "cdArea")) "the area name became its code")
        (is (equal "2020000000" (param ask "cdTime")) "and the period name its code")
        (is (equal "0003445078" (param ask "statsDataId")) "for the table asked about"))
      (is (cl:search "東京都 | 2020年 | 総数 = 14047594 人" text) "the row carries its labels and its unit")
      (is (cl:search "出典：政府統計の総合窓口(e-Stat)" text) "the citation is there to copy")
      (is (cl:search "国によって保証されたものではありません" text) "with e-Stat's own credit"))))

(deftest estat-cell-get-data-narrows-a-selection-wider-than-the-cap ()
  (with-estat ()
    (setf *answer*
          (lambda (url)
            (if (cl:search "getStatsData" url)
                (values (data-answer (list "@area" "13000" "@time" "2020000000" "$" "1")
                                      (list "@area" "13000" "@time" "2021000000" "$" "2")
                                      (list "@area" "13000" "@time" "2020000000" "$" "3"))
                        200)
                (values +meta-answer+ 200))))
    (let ((text (estat-call-tool "estat_get_data"
                                  "{\"table_id\":\"0003445078\",\"max_rows\":2}")))
      (is (cl:search "showing the first 2 of 3 values" text) "the block says what it left out")
      (is (cl:search "narrow it with area, time or filters" text) "and how to ask again"))))

(deftest estat-cell-get-data-refuses-an-ambiguous-area-with-the-candidates ()
  (with-estat ()
    (let ((text (estat-call-tool "estat_get_data"
                                  "{\"table_id\":\"0003445078\",\"area\":\"府中市\"}")))
      (is (cl:search "ERROR:" text) "a name two items answer to is refused")
      (is (cl:search "closest are" text) "with the items to choose from")
      (is (cl:search "13206" text) "naming their codes"))))

;;; --- the command ---------------------------------------------------------------

(deftest estat-cell-the-command-searches-from-the-conversation ()
  (with-estat ()
    (let ((text (nodecode-estat::run-estat "人口" nil)))
      (is (cl:search "0003445078" text) "/estat answers the same search block"))
    (multiple-value-bind (text dialog) (nodecode-estat::run-estat "" nil)
      (is (stringp text) "bare, it answers a line")
      (is (and dialog t) "and a dialog asking for the words"))))
