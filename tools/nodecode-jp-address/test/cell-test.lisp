;;;; cell-test.lisp --- the jp-address cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- the fixture routes -------------------------------------------------------------

(defparameter +chiyoda+
  (cons "%E5%8D%83%E4%BB%A3%E7%94%B0%E5%8C%BA" (jp-chiyoda-file))
  "Chiyoda's town file, under the percent-encoded municipality name.")

(defparameter +chuo+
  (cons "%E4%B8%AD%E5%A4%AE%E5%8C%BA" (jp-chuo-file))
  "Sapporo Chuo's town file.")

(defun jp-routes ()
  "The list file, and the two town files the tests below reach."
  (list (cons "/api.json" (jp-list-file)) +chiyoda+ +chuo+))

;;; --- lifecycle, settings and the tool's shape ------------------------------------

(define-cell-lifecycle-tests "jp-address"
  (:help :jp-address)
  (:command "jp-address")
  (:running
   (is (nle::find-tool "address_split") "the tool is registered")
   (is (equal "https://japanese-addresses-v2.geoloniamaps.com/api" (nodecode-jp-address::setting :base)) "the lookups read the published address data")
   (is (= 15 (nodecode-jp-address::setting :timeout)) "one file may take fifteen seconds"))
  (:stopped
   (is (null (nle::find-tool "address_split")) "stop takes the tool back out"))
  (:refused ("base_url" "http://example.com/api")
            ("base_url" "https://example.com/api/")
            ("base_url" "https://user@example.com/api")
            ("timeout_seconds" 0))
  (:idle nodecode-jp-address:jp-address-error
         (nodecode-jp-address:address-split-json "東京都千代田区霞が関2-1-2")))

(deftest jp-address-cell-tool-refuses-a-call-with-no-address ()
  ;; The one member the declaration requires: a call without it never reaches
  ;; the body, which is the repair the model reads.
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-jp-address::split-request)
                              (nlk:make-json-object "town" "霞が関")))))

(deftest jp-address-cell-tool-refuses-a-second-argument ()
  ;; The original took only an address string; a call that passes more is
  ;; refused with what to drop rather than half-answered.
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-jp-address::split-request)
                              (nlk:make-json-object "address" "東京都千代田区霞が関2-1-2"
                                                     "pref" "東京都")))))

;;; --- one address, town level -------------------------------------------------------

(deftest jp-address-cell-splits-a-town-level-address ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result "東京都千代田区霞が関2-1-2")))
        (is (= 3 (jp-address-field result "level")) "the town matched")
        (is (equal "東京都" (jp-address-field result "pref")) "the prefecture is named")
        (is (equal "千代田区" (jp-address-field result "city")) "the municipality is named")
        (is (equal "霞が関二丁目" (jp-address-field result "town")) "and the town, in the file's spelling")
        (is (equal "1-2" (jp-address-field result "other")) "the house number is the rest")
        (let ((point (jp-address-field result "point")))
          (is (< (abs (- 35.675551d0 (jp-address-field point "lat"))) 1d-4) "the point is the town's latitude")
          (is (< (abs (- 139.750413d0 (jp-address-field point "lng"))) 1d-4) "and its longitude")
          (is (= 3 (jp-address-field point "level")) "at the town's own level"))
        (is (search "Geolonia" (jp-address-field result "attribution")) "the CC BY 4.0 attribution rides every result")
        (is (= 2 (length calls)) "one list file, then one town file")
        (is (search "/api.json" (first calls)) "the national list is asked for first")
        (is (search "%E6%9D%B1%E4%BA%AC%E9%83%BD/" (second calls)) "the matched prefecture travels percent-encoded in the path")
        (is (not (search "2-1-2" (second calls))) "the house number does not travel")))))

(deftest jp-address-cell-stops-at-the-municipality ()
  ;; The town file has no row without a chome, so the safe answer is the
  ;; municipality, not a guess at 一丁目.
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result "東京都千代田区霞が関")))
        (is (= 2 (jp-address-field result "level")) "the municipality is as far as it goes")
        (is (equal "霞が関" (jp-address-field result "other")) "the town name is returned as the rest")
        (is (= 2 (jp-address-field (jp-address-field result "point") "level")) "with the municipality's point")))))

(deftest jp-address-cell-folds-fullwidth-digits-and-spaces ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result "東京都 千代田区霞が関２-１-２")))
        (is (= 3 (jp-address-field result "level")) "the same address in fullwidth digits and with a space")
        (is (equal "1-2" (jp-address-field result "other")) "reads back in halfwidth")
        (is (= 2 (length calls)) "and is answered as one address")))))

;;; --- the municipality without a prefecture --------------------------------------------

(deftest jp-address-cell-finds-a-unique-municipality ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result "千代田区霞が関2-1-2")))
        (is (= 3 (jp-address-field result "level")) "one Chiyoda in Japan is enough")
        (is (equal "東京都" (jp-address-field result "pref")) "and the prefecture is filled in")
        (is (= 2 (length calls)) "the list answered which prefecture it is")))))

(deftest jp-address-cell-asks-for-the-prefecture-when-a-name-is-shared ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let* ((result (jp-address-result "泊村"))
             (cities (jp-address-field result "municipalities")))
        (is (equal "ambiguous_city" (jp-address-field result "status")) "two Tomari villages cannot be told apart")
        (is (= 2 (length cities)) "both are named")
        (is (find "古宇郡泊村" cities :test #'string=) "the one in Furuu")
        (is (find "国後郡泊村" cities :test #'string=) "and the one in Kunashiri")
        (is (= 1 (length calls)) "the town file is not asked for until one is chosen")))))

(deftest jp-address-cell-answers-an-unknown-address-with-level-0 ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result "1600 Pennsylvania")))
        (is (= 0 (jp-address-field result "level")) "nothing matched, which is not a failure")
        (is (equal "1600Pennsylvania" (jp-address-field result "other")) "the whole address comes back as the rest")
        (is (= 1 (length calls)) "and only the list was read")))))

(deftest jp-address-cell-reports-an-unreadable-file ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (list (cons "/api.json" (jp-list-file))))
      (let ((result (jp-address-result "東京都千代田区霞が関2-1-2")))
        (is (equal "upstream" (jp-address-field result "status")) "a missing town file is upstream, not a guess")
        (is (search "HTTP 404" (jp-address-field result "message")) "with the status, so the operator sees why")
        (is (search "Geolonia" (jp-address-field result "attribution")) "and the attribution the licence asks for")))))

(deftest jp-address-cell-refuses-an-address-that-is-too-long ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result (make-string 401 :initial-element #\東))))
        (is (equal "bad_args" (jp-address-field result "status")) "401 characters is refused")
        (is (null calls) "before any request")))))

;;; --- the town the file spells with a kanji numeral ------------------------------------

(deftest jp-address-cell-reads-an-arabic-street-number ()
  ;; 宮の森1条1丁目1 is the town the file writes as 宮の森一条一丁目: the
  ;; digit run before 条 is read as the numeral it stands for.
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result "北海道札幌市中央区宮の森1条1丁目1")))
        (is (= 3 (jp-address-field result "level")) "the town matched")
        (is (equal "宮の森一条一丁目" (jp-address-field result "town")) "in the file's own spelling")
        (is (equal "1" (jp-address-field result "other")) "the lot is the rest")))))

(deftest jp-address-cell-asks-which-town-a-loose-number-means ()
  ;; 宮ケ丘3-1 could be the bare row or its third chome; only a written 丁目
  ;; picks the chome, so both are listed.
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let* ((result (jp-address-result "北海道札幌市中央区宮ケ丘3-1"))
             (towns (jp-address-field result "towns")))
        (is (equal "ambiguous_town" (jp-address-field result "status")) "the loose number does not choose")
        (is (= 2 (length towns)) "both rows are listed")
        (is (find "宮ケ丘" towns :test #'string=) "the row with no chome")
        (is (find "宮ケ丘三丁目" towns :test #'string=) "and the chome row")
        (is (null (jp-address-field result "level")) "an ambiguous town is not a level")))))

(deftest jp-address-cell-picks-the-chome-when-it-is-written ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((result (jp-address-result "北海道札幌市中央区宮ケ丘3丁目")))
        (is (= 3 (jp-address-field result "level")) "a written 丁目 picks the chome")
        (is (equal "宮ケ丘三丁目" (jp-address-field result "town")) "and names it")
        (is (equal "" (jp-address-field result "other")) "with nothing left over")))))

;;; --- the operator's command ------------------------------------------------------------

(deftest jp-address-command-splits-in-the-conversation ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((text (cell-entry "nodecode-jp-address" "jp-address" "東京都千代田区霞が関2-1-2")))
        (is (search "霞が関二丁目" text) "the town is in the answer")
        (is (search "other 1-2" text) "with the house number as the rest")
        (is (search "CC BY 4.0" text) "and the attribution")
        (is (= 2 (length calls)) "the command reads the same two files the tool does")))))

(deftest jp-address-command-asks-for-an-address-bare ()
  (with-jp-address ()
    (with-scripted-address (calls :routes (jp-routes))
      (let ((text (cell-entry "nodecode-jp-address" "jp-address" "")))
        (is (search "one Japanese address" text) "bare, the command says what it wants")
        (is (null calls) "and reads nothing")))))
