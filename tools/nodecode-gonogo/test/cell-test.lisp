;;;; cell-test.lisp --- the three verbs against the numbers they must give.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here is stubbed and nothing is dialled: the statistics run
;;;; locally, and every results file is one these tests write into a scratch
;;;; directory of their own.

(in-package #:nodecode.test)

(define-test-slice "gonogo" "GONOGO-CELL-" :start nodecode-gonogo:start-cell)

(define-cell-lifecycle-tests "gonogo"
  (:help :gonogo)
  (:command "gonogo")
  (:refused ("default_target_percent" 10))
  (:running (is (= 95 (nodecode-gonogo::setting :default-target-percent)) "the default target")))

;;; --- scratch files -------------------------------------------------------------

(defun gonogo-dir ()
  "This run's scratch directory."
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "~a/gonogo-cell/" (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun gonogo-file (name text)
  "TEXT written at NAME in the scratch directory => the pathname."
  (let ((path (merge-pathnames name (gonogo-dir))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string text out))
    path))

(defun gonogo-run (name passed total)
  "A results file of TOTAL cases, the first PASSED of which passed."
  (gonogo-file name
                (with-output-to-string (out)
                  (loop for i from 1 to total
                        do (format out "{\"id\": \"c~d\", \"passed\": ~a}~%"
                                   i (if (<= i passed) "true" "false"))))))

(defun gonogo-refusal-p (thunk)
  "Whether THUNK refuses as GONOGO-ERROR."
  (handler-case (progn (funcall thunk) nil)
    (gonogo:gonogo-error () t)))

;;; --- the decision ---------------------------------------------------------------

(deftest gonogo-cell-a-count-under-the-bar-is-assist-only ()
  ;; 47 of 50 reads as 94%, which is under a 95% target, and no confidence
  ;; signal means there is no subset to route to a person.
  (with-cell-stop ((gonogo-start))
    (let ((out (gonogo:decide :passed 47 :total 50 :target 0.95)))
      (is (equal "ASSIST ONLY" (getf out :verdict)))
      (is (not (getf out :can-automate)))
      (is (null (getf out :operating-point)))
      (is (null (getf out :needed-n)))
      (is (< (abs (- 0.838 (getf (getf out :pass-rate) :low))) 0.001) "the interval is the finding")
      (let ((notes (format nil "~{~a~^ ~}" (getf out :notes))))
        (is (search "confidence" notes) "the missing confidence signal is reported")))))

(deftest gonogo-cell-a-wide-interval-is-insufficient-evidence ()
  ;; The headline case: 94% observed against a 90% target, but the interval
  ;; reaches down to 83.8%, so the sample cannot support the claim.
  (with-cell-stop ((gonogo-start))
    (let ((out (gonogo:decide :passed 47 :total 50 :target 0.9)))
      (is (equal "INSUFFICIENT EVIDENCE" (getf out :verdict)))
      (is (> (getf (getf out :pass-rate) :point) 0.9))
      (is (< (getf (getf out :pass-rate) :low) 0.9))
      (is (= 204 (getf out :needed-n)) "how many cases the claim would need")
      (is (search "Needs about 204 cases" (getf out :markdown))))))

(deftest gonogo-cell-clears-a-low-target-and-will-not-save-a-weak-one ()
  (with-cell-stop ((gonogo-start))
    (let ((cleared (gonogo:decide :passed 95 :total 100 :target 0.85))
          (weak (gonogo:decide :passed 3 :total 10)))
      (is (equal "AUTOMATE" (getf cleared :verdict)))
      (is (getf cleared :can-automate))
      (is (equal "DO NOT AUTOMATE" (getf weak :verdict)))
      (let ((text (getf (getf (gonogo:decide :passed 47 :total 50) :pass-rate) :text)))
        (is (equal "94.0%" (subseq text 0 5)) "the point estimate is never the answer")))))

(deftest gonogo-cell-a-results-file-unlocks-an-operating-point ()
  (with-cell-stop ((gonogo-start))
    (let* ((path (gonogo-file
                  "invoices.jsonl"
                  (with-output-to-string (out)
                    (loop for i from 1 to 200
                          do (format out "{\"id\": \"inv-~3,'0d\", \"passed\": ~a, \"confidence\": ~,2f}~%"
                                     i (if (<= i 180) "true" "false")
                                     (if (<= i 180) 0.95 0.4))))))
           (out (gonogo:decide :results-path (namestring path) :target 0.9)))
      (is (equal "AUTOMATE WITH REVIEW" (getf out :verdict)))
      (is (= 200 (getf out :n-cases)))
      (is (getf out :operating-point) "confidence on every case is what unlocks it")
      (is (< (getf (getf out :operating-point) :coverage) 1.0))
      (is (>= (getf (getf out :operating-point) :n-deferred) 20)))))

(deftest gonogo-cell-groups-fold-to-one-trial-each ()
  (with-cell-stop ((gonogo-start))
    (let* ((path (gonogo-file
                  "grouped.jsonl"
                  (format nil "{\"id\": \"g1-a\", \"passed\": true, \"group\": \"g1\"}~%~
{\"id\": \"g1-b\", \"passed\": true, \"group\": \"g1\"}~%~
{\"id\": \"g2-a\", \"passed\": true, \"group\": \"g2\"}~%~
{\"id\": \"g2-b\", \"passed\": false, \"group\": \"g2\"}~%")))
           (out (gonogo:decide :results-path (namestring path) :target 0.5)))
      (is (equal "groups" (getf out :unit)))
      (is (= 2 (getf out :n-trials)) "two groups, not four cases")
      (is (= 4 (getf out :n-cases)) "the case count is context")
      (is (= 3 (getf out :n-passed)))
      (is (= 1 (getf out :n-trials-passed)) "a group passes only if every case in it does")
      (is (search "3/4 cases passed" (getf out :markdown))))))

(deftest gonogo-cell-refuses-what-it-cannot-measure ()
  (with-cell-stop ((gonogo-start))
    (is (gonogo-refusal-p (lambda () (gonogo:decide))) "neither a count nor a file")
    (is (gonogo-refusal-p (lambda () (gonogo:decide :passed 60 :total 50))))
    (is (gonogo-refusal-p (lambda () (gonogo:decide :passed 10 :total 10 :target 1.5))))
    (is (gonogo-refusal-p
         (lambda () (gonogo:decide :results-path "/nonexistent/never.jsonl"))))
    (let ((path (namestring (gonogo-file "bad.jsonl" "{\"id\": \"a\"}~%"))))
      (is (gonogo-refusal-p (lambda () (gonogo:decide :results-path path))) "a row with no passed"))))

;;; --- the report and the paired test ---------------------------------------------

(deftest gonogo-cell-the-report-is-an-artifact-that-reads-back ()
  (with-cell-stop ((gonogo-start))
    (let* ((path (gonogo-file
                  "ninety.jsonl"
                  (with-output-to-string (out)
                    (loop for i from 1 to 100
                          do (format out "{\"id\": \"p~d\", \"passed\": ~a, \"confidence\": ~,2f}~%"
                                     i (if (<= i 90) "true" "false")
                                     (if (<= i 90) 0.9 0.3))))))
           (save (merge-pathnames "out/report.json" (gonogo-dir)))
           (page (merge-pathnames "out/report.html" (gonogo-dir)))
           (out (gonogo:report :results-path (namestring path) :task "invoice routing"
                               :target 0.85 :show-failures 3
                               :save-report (namestring save)
                               :write-html (namestring page))))
      (is (equal "invoice routing" (getf out :task)))
      (is (search "Score report" (getf out :markdown)))
      (is (uiop:string-prefix-p "invoice routing:" (getf out :summary)))
      (is (probe-file (getf out :saved-report)))
      (is (search "<html" (string-downcase (uiop:read-file-string page))))
      (is (search "p91" (getf out :markdown)) "the worst failures are listed")
      (let ((again (gonogo:report :results-path (getf out :saved-report))))
        (is (= (getf out :n-cases) (getf again :n-cases)))
        (is (equal (getf out :verdict) (getf again :verdict)) "a saved report re-measures")))))

(deftest gonogo-cell-compare-calls-a-difference-and-refuses-noise ()
  (with-cell-stop ((gonogo-start))
    (let ((out (gonogo:compare :report-a (gonogo-run "a.jsonl" 60 100)
                                :report-b (gonogo-run "b.jsonl" 90 100))))
      (is (= 100 (getf out :n-shared)))
      (is (getf out :significant))
      (is (getf out :exact))
      (is (< (abs (- 0.3 (getf out :difference))) 0.01)))
    (let ((out (gonogo:compare :report-a (gonogo-run "a60.jsonl" 51 60)
                                :report-b (gonogo-run "b60.jsonl" 54 60))))
      (is (not (getf out :significant)))
      (let ((summary (getf out :summary)))
        (is (search "no detectable difference" summary) "three points on 60 cases is noise")))))

(deftest gonogo-cell-compare-pairs-saved-reports ()
  (with-cell-stop ((gonogo-start))
    (let* ((a (gonogo:report :results-path (namestring (gonogo-run "early.jsonl" 10 20))
                              :save-report (namestring (merge-pathnames "early.json" (gonogo-dir)))))
           (b (gonogo:report :results-path (namestring (gonogo-run "late.jsonl" 19 20))
                              :save-report (namestring (merge-pathnames "late.json" (gonogo-dir)))))
           (out (gonogo:compare :report-a (getf a :saved-report)
                                :report-b (getf b :saved-report))))
      (is (= 20 (getf out :n-shared)))
      (is (plusp (getf out :only-b)))
      (is (search "Agent B pass rate" (getf out :summary))))))
