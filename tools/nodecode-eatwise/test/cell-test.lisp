;;;; cell-test.lisp --- lifecycle, the table, the arithmetic, the consent gates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here leaves the image: the table is compiled in, the arithmetic is
;;;; local, and the store is one JSON file under a scratch home the tests name
;;;; in the section. A tool is called the way the engine's body calls it, with
;;;; the plist made by its contract's decoder.

(in-package #:nodecode.test)

(define-test-slice "eatwise" "EATWISE-CELL-" :start nodecode-eatwise:start-cell)

(defparameter +store+ "eatwise/test-store.json"
  "The store these tests read and write, under the home the run gives them.")

(defun eatwise-call (tool &rest pairs) "TOOL called with PAIRS, the way the engine's body calls it."
  (let ((entry (nle::find-tool tool)))
    (unless entry (error "no tool ~a" tool))
    (funcall (third entry)
             (nlk:contract-decode
              (second entry)
              (apply #'nlk:make-json-object pairs)))))

(defun eatwise-clean-store ()
  (let ((path (nlk:home +store+)))
    (when (probe-file path) (ignore-errors (delete-file path)))))

(defmacro with-eatwise ((&rest settings) &body body)
  "The cell started on the test store, with SETTINGS over it, and stopped after."
  `(progn
     (eatwise-clean-store)
     (with-cell-stop ((apply #'eatwise-start (append (list "history_file" +store+) (list ,@settings))))
       (unwind-protect (progn ,@body)
         (eatwise-clean-store)))))

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "eatwise"
  (:command "eatwise")
  (:running (is (nle::find-tool "eatwise_foods") "the food tool is registered")
            (is (nle::find-tool "eatwise_review_meal") "and the review tool")
            (is (nle::find-tool "eatwise_history") "and the history tool")
            (is (equal 467 nodecode-eatwise::*food-count*) "the table is compiled in whole")
            (is (assoc :eatwise nle::*help-topics*) "(help :eatwise) is on the manual"))
  (:stopped (is (null (nle::find-tool "eatwise_foods")) "stop unregisters the tool")
            (is (null (nle::find-registered-command "eatwise")) "and the command"))
  (:refused ("meal_share_percent" 5)))

(deftest eatwise-cell-refuses-a-share-below-ten (with-saved-globals (nle:*tools*))
  (is (signals-error nlk:config-refusal (eatwise-start "meal_share_percent" 5)) "a meal share under ten percent is refused")
  (is (null (nle::find-tool "eatwise_foods")) "and the refused start installed nothing"))

;;; --- the table ---------------------------------------------------------------

(deftest eatwise-cell-finds-a-food-by-name-and-alias (with-eatwise ()
  (let ((answer (eatwise-call "eatwise_foods" "query" "白饭")))
    (is (search "tw-a0550601" answer) "the rice entry is named by its own name")
    (is (search "1 碗 = 160" answer) "with its serving in grams")
    (is (search "TFND" answer) "and where its numbers come from"))
  (is (search "tw-a0550601" (eatwise-call "eatwise_foods" "query" "白米饭")) "an alias finds the same entry")
  (is (search "No food" (eatwise-call "eatwise_foods" "query" "火星肉")) "a food the table does not hold says so")))

;;; --- targets ------------------------------------------------------------------

(deftest eatwise-cell-targets-are-the-mifflin-arithmetic (with-eatwise ()
  (let ((answer (eatwise-call "eatwise_targets"
                              "gender" "male" "age" 30 "height_cm" 175 "weight_kg" 70
                              "activity_level" "moderate" "goal" "maintain")))
    (is (search "BMR 1649" answer) "Mifflin-St Jeor: 10*70 + 6.25*175 - 5*30 + 5")
    (is (search "TDEE 2556" answer) "times the moderate factor 1.55")
    (is (search "protein 70.0 g" answer) "a maintaining profile keeps 1.0 g/kg of protein")
    (is (search "SAFETY: ok" answer) "and nothing refers"))))

(deftest eatwise-cell-targets-refuse-without-a-profile (with-eatwise ()
  (is (search "ERROR: EATWISE-ERROR: no profile"
              (eatwise-call "eatwise_targets")) "no field and no stored profile is a refusal in words")))

(deftest eatwise-cell-a-minor-is-referred-not-given-numbers (with-eatwise ()
  (let ((answer (eatwise-call "eatwise_targets"
                              "gender" "female" "age" 15 "height_cm" 160 "weight_kg" 50
                              "activity_level" "light" "goal" "fatLoss")))
    (is (search "referral" answer) "a minor is a safety referral")
    (is (search "未成年人" answer) "with the reason the original wrote")
    (is (not (search "DAILY TARGETS" answer)) "and no adult formula target"))))

;;; --- a meal ------------------------------------------------------------------

(deftest eatwise-cell-reviews-confirmed-items (with-eatwise ()
  (let ((answer (eatwise-call "eatwise_review_meal"
                             "confirmed" t
                             "items" (vector (nlk:make-json-object "food_id" "tw-a0550601" "amount" 2)))))
    (is (search "tw-a0550601" answer) "the food is listed")
    (is (search "(320.0 g)" answer) "two bowls of a 160 g serving weigh 320 g")
    (is (search "MEAL: " answer) "and the meal total is given")
    (is (search "未建档" answer) "with no profile, no personal target is claimed"))))

(deftest eatwise-cell-a-meal-nobody-confirmed-is-refused (with-eatwise ()
  (let ((answer (eatwise-call "eatwise_review_meal" "meal_text" "2碗白饭")))
    (is (search "not confirmed" answer) "an unconfirmed meal computes nothing")
    (is (not (search "MEAL: " answer)) "not even a total"))))

(deftest eatwise-cell-an-unknown-food-id-is-refused (with-eatwise ()
  (is (search "is not a food id"
              (eatwise-call "eatwise_review_meal" "confirmed" t
                            "items" (vector (nlk:make-json-object "food_id" "tw-nope" "amount" 1)))) "a made-up id is named back")))

(deftest eatwise-cell-reads-a-meal-from-text (with-eatwise ()
  (let ((answer (eatwise-call "eatwise_review_meal" "confirmed" t "meal_text" "2碗白饭，红烧肉")))
    (is (search "tw-a0550601" answer) "the rice entry is named by its own name")
    (is (search "红烧肉" answer) "and the dish")
    (is (search "默认一份" answer) "a portion nobody wrote is disclosed as the default"))))

;;; --- the store ---------------------------------------------------------------

(deftest eatwise-cell-consent-gates-the-writes (with-eatwise ()
  (is (search "MEAL RECORDING: off" (eatwise-call "eatwise_history" "action" "status")) "an untouched store keeps nothing")
  (is (search "profile memory is off"
              (eatwise-call "eatwise_history" "action" "save_profile"
                            "gender" "male" "age" 30 "height_cm" 175 "weight_kg" 70
                            "activity_level" "light" "goal" "maintain")) "a profile is not written before the person agrees")
  (is (search "meal recording is off"
              (eatwise-call "eatwise_review_meal" "confirmed" t "save" t
                            "local_date" "2026-11-08" "request_id" "test-request-1"
                            "items" (vector (nlk:make-json-object "food_id" "tw-a0550601" "amount" 1)))) "nor is a meal")))

(deftest eatwise-cell-a-consented-meal-is-read-back-by-day (with-eatwise ()
  (eatwise-call "eatwise_history" "action" "configure" "profile_memory" t "meal_recording" t)
  (is (search "PROFILE SAVED"
              (eatwise-call "eatwise_history" "action" "save_profile"
                            "gender" "male" "age" 30 "height_cm" 175 "weight_kg" 70
                            "activity_level" "moderate" "goal" "maintain")) "a consented profile is written")
  (let ((answer (eatwise-call "eatwise_review_meal" "confirmed" t "save" t
                              "local_date" "2026-11-08" "request_id" "test-request-2"
                              "meal_type" "lunch"
                              "items" (vector (nlk:make-json-object "food_id" "tw-a0550601" "amount" 2)))))
    (is (search "SAVED: 2026-11-08 lunch" answer) "a consented, confirmed meal is saved")
    (is (search "DAILY TARGETS" answer) "the stored profile supplies the targets now"))
  (is (search "2026-11-08: 1 recorded meal" (eatwise-call "eatwise_history" "action" "day"
                                                           "local_date" "2026-11-08")) "the day review finds it")
  (is (search "2026-11-08 lunch" (eatwise-call "eatwise_history" "action" "list")) "and the list does too")
  (is (search "DELETED (meals)"
              (eatwise-call "eatwise_history" "action" "delete" "scope" "meals")) "delete removes them")
  (is (search "No meal recorded yet." (eatwise-call "eatwise_history" "action" "list")) "and nothing is left to read")))

(deftest eatwise-cell-the-command-answers (with-eatwise ()
  (is (search "STORE: " (nodecode-eatwise::run-eatwise "" "s1")) "/eatwise alone is the store's state")
  (is (search "tw-a0550601" (nodecode-eatwise::run-eatwise "2碗白饭" "s1")) "and text after it is a review")))
