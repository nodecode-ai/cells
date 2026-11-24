;;;; cell-test.lisp --- the lifecycle, the arithmetic, the list, the refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that a start installs the five tools, /freight and
;;;; the manual and a stop takes every one back; that a bill line matched to a
;;;; trip and rated by the rule in force keeps the integer difference and names
;;;; its evidence; that a charge with no rule in force is unresolved with
;;;; unknown, never zero; that waiting rounds up to whole blocks after the free
;;;; seconds and a toll without a verified receipt is missing evidence; that the
;;;; same imported files answer the same summary twice; that an executed trip
;;;; nobody billed is listed; that the export writes the CSV; that a bad amount
;;;; names the line it is on; and that a missing data_dir refuses.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "freight"
  (:help :freight)
  (:command "freight")
  (:running (is (= 20 (nodecode-freight::setting :page-size)) "a page is twenty differences")
            (is (equal "CNY" (nodecode-freight::setting :currency)) "the currency defaults to CNY"))
  (:refused ("data_dir" "") ("data_dir" "lib" "page_size" 0))
  (:idle freight:freight-error
         (freight:cases)
         (freight:summary "november")))

;;; --- the arithmetic ---------------------------------------------------------------

(deftest freight-cell-a-bill-line-keeps-its-difference ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips.csv" +trips+))
    (freight:load-source "november" "rates" (freight-fixture "rates.csv" +base-rates+))
    (freight:load-source "november" "bill" (freight-fixture "bill.csv" +base-bill+))
    (let ((text (freight:reconcile "november")))
      (is-carrying text ("1 bill lines in 1 groups: 1 comparable" "the one line is comparable")
                    ("net difference +100.00 CNY" "the hundred is the difference")
                    ("1 group with issues" "and the group carries the findings")))
    (let ((text (freight:differences "november")))
      (is-carrying text ("billed 1100.00, expected 1000.00, delta +100.00" "billed against contracted")
                    ("AMOUNT_HIGH" "over-billed")
                    ("MISSING_POD" "no delivery evidence was indexed")
                    ("trips#trip=T1" "the trip is the evidence ref")))))

(deftest freight-cell-no-rule-in-force-is-unresolved-not-zero ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips.csv" +trips+))
    (freight:load-source "november" "bill" (freight-fixture "bill.csv" +base-bill+))
    (is-carrying (text (freight:reconcile "november"))
                  ("0 comparable" "nothing could be compared")
                  ("1 unresolved" "a missing rate keeps the group unresolved"))
    (is-carrying (text (freight:differences "november"))
                  ("RATE_NOT_FOUND" "the issue names the missing rule")
                  ("expected unknown" "an unknown rate is never zero"))))

(deftest freight-cell-waiting-rounds-up-after-the-free-seconds ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips.csv" +trips+))
    (freight:load-source "november" "rates"
                         (freight-fixture "wait-rates.csv"
                                          (format nil "rule_id,version,carrier_id,route_id,vehicle_class,charge_code,currency,amount_basis,effective_from,effective_to,formula,rate,free_seconds,block_seconds~%R2,1,C1,R1,V1,WAIT,CNY,,2026-11-01T00:00:00+00:00,2026-12-01T00:00:00+00:00,WAIT_BLOCKS,50.00,600,300~%")))
    (freight:load-source "november" "waiting"
                         (freight-fixture "waiting.csv"
                                          (format nil "trip_id,occurrence_id,elapsed_seconds,verified~%T1,,900,true~%")))
    (freight:load-source "november" "bill"
                         (freight-fixture "wait-bill.csv"
                                          (format nil "line_id,carrier_id,trip_ref,charge_code,currency,amount~%L2,C1,T1,WAIT,CNY,60.00~%")))
    (is-carrying (text (freight:reconcile "november"))
                  ("net difference +10.00 CNY" "900 seconds, 600 free, one 300-second block at 50.00"))))

(deftest freight-cell-a-toll-without-a-verified-receipt-is-missing-evidence ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips.csv" +trips+))
    (freight:load-source "november" "rates"
                         (freight-fixture "toll-rates.csv"
                                          (format nil "rule_id,version,carrier_id,route_id,vehicle_class,charge_code,currency,amount_basis,effective_from,effective_to,formula,rate~%R3,1,C1,R1,V1,TOLL,CNY,,2026-11-01T00:00:00+00:00,2026-12-01T00:00:00+00:00,ACTUAL_RECEIPT,0~%")))
    (freight:load-source "november" "bill"
                         (freight-fixture "toll-bill.csv"
                                          (format nil "line_id,carrier_id,trip_ref,charge_code,currency,amount~%L3,C1,T1,TOLL,CNY,45.00~%")))
    (freight:reconcile "november")
    (is-carrying (text (freight:differences "november"))
                  ("MISSING_RECEIPT" "a toll nobody proved is missing evidence")
                  ("expected unknown" "and its amount stays unknown"))))

(deftest freight-cell-the-same-files-answer-the-same-summary ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips.csv" +trips+))
    (freight:load-source "november" "rates" (freight-fixture "rates.csv" +base-rates+))
    (freight:load-source "november" "bill" (freight-fixture "bill.csv" +base-bill+))
    (let ((first (freight:reconcile "november"))
          (second (freight:reconcile "november")))
      (is (string= first second) "two runs over the same frozen inputs answer the same words"))))

;;; --- the list, the export, the refusal ---------------------------------------------

(deftest freight-cell-a-trip-nobody-billed-is-listed ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips-two.csv" +trips-two+))
    (freight:load-source "november" "rates" (freight-fixture "rates.csv" +base-rates+))
    (freight:load-source "november" "bill" (freight-fixture "bill.csv" +base-bill+))
    (is-carrying (text (freight:reconcile "november")) ("1 trip not billed" "T2 is counted"))
    (is-carrying (text (freight:differences "november" :filter "not-billed"))
                  ("NOT_BILLED" "the filter shows it")
                  ("trip T2" "and names the trip"))))

(deftest freight-cell-the-export-writes-the-difference-list ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips.csv" +trips+))
    (freight:load-source "november" "rates" (freight-fixture "rates.csv" +base-rates+))
    (freight:load-source "november" "bill" (freight-fixture "bill.csv" +base-bill+))
    (freight:reconcile "november")
    (let* ((text (freight:export-review "november"))
           (path (merge-pathnames "exports/november-review.csv"
                                  (uiop:ensure-directory-pathname (freight-test-library))))
           (csv (nlk:read-text path)))
      (is-carrying text ("exported 1 difference row" "one difference is exported"))
      (is (search "group_id,trip_id,charge_code" csv) "with its header")
      (is (search "AMOUNT_HIGH" csv) "and the issue code"))))

(deftest freight-cell-a-bad-amount-names-its-line ()
  (with-freight ()
    (freight-open)
    (freight:load-source "november" "trips" (freight-fixture "trips.csv" +trips+))
    (is (search "line 2"
                (refusal-text freight:freight-error
                              (freight:load-source
                               "november" "bill"
                               (freight-fixture "bad-bill.csv"
                                                (format nil "line_id,carrier_id,trip_ref,charge_code,currency,amount~%L1,C1,T1,BASE,CNY,12.345~%"))))) "a three-place amount is refused where it is")))

(deftest freight-cell-a-case-without-a-library-refuses ()
  (is (signals-error nlk:config-refusal
        (nodecode-freight:start-cell
         (nlk:json-object "freight" (nlk:make-json-object "page_size" 5)))) "a section with no data_dir refuses rather than writing somewhere unexpected"))

(deftest freight-cell-the-tools-do-what-the-verbs-do ()
  (with-freight ()
    (is-carrying (text (nodecode-freight::tool-case
                         (nodecode-freight::decode-freight-case
                          (nlk:make-json-object "case" "october" "carrier" "C1"
                                                 "period_from" "2026-10-01T00:00:00+00:00"
                                                 "period_to" "2026-11-01T00:00:00+00:00"))))
                  ("case october opened" "the tool opens a case"))
    (nodecode-freight::tool-import
     (nodecode-freight::decode-freight-source
      (nlk:make-json-object "case" "october" "kind" "trips"
                             "path" (freight-fixture "trips.csv" +trips+))))
    (is-carrying (text (nodecode-freight::tool-import
                         (nodecode-freight::decode-freight-source
                          (nlk:make-json-object "case" "october" "kind" "rates"
                                                 "path" (freight-fixture "rates.csv" +base-rates+)))))
                  ("imported 1 rates rows" "an import answers its count"))
    (is-carrying (text (freight:cases)) ("october" "the library lists it"))))

(deftest freight-cell-the-command-answers-the-library-and-a-panel ()
  (with-freight ()
    (freight-open)
    (multiple-value-bind (text dialog) (nodecode-freight::run-slash "" nil)
      (is-carrying text ("november" "the command lists the case"))
      (is dialog "and answers a panel"))
    (is (search "no reconciliation yet"
                (refusal-text freight:freight-error (freight:summary "november"))) "an unrun case says so")))
