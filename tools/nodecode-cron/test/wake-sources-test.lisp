;;;; wake-sources-test.lisp --- the outward wake sources: each fact once, in its window.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The pure LINES half of every source, driven with data of the shapes its
;;;; reader returns: a fact shows only when it crosses into (SINCE, NOW], so a
;;;; cursor that moves on admission never repeats it; what a source cannot
;;;; apply to answers NIL.

(in-package #:nodecode.test)

(defparameter +wake-now+ (nlk:iso-universal "2026-11-03T12:00:00Z"))

(defun wake-iso (seconds-before-now)
  "The ISO time SECONDS-BEFORE-NOW before +WAKE-NOW+."
  (nlk:iso-time (- (- +wake-now+ seconds-before-now) nlk:+unix-epoch+) :millis nil))

(deftest cron-cell-wake-sources-registered ()
  (dolist (source '(nodecode-cron::wake-source-red-checks nodecode-cron::wake-source-waits
                    nodecode-cron::wake-source-promises nodecode-cron::wake-source-folders))
    (is (member source nodecode-cron::*wake-sources*)
        (format nil "~a is on *wake-sources*" source))))

(deftest cron-cell-wake-sources-cap ()
  (let ((nodecode-cron::*wake-source-cap* 3))
    (is (equal '("a" "b" "c" "... and 2 more")
               (nodecode-cron::cap-lines '("a" "b" "c" "d" "e"))) "past the cap the rest are counted")
    (is (equal '("a b") (nodecode-cron::cap-lines (list (format nil "a~%b")))) "a line stays one line")))

(deftest cron-cell-wake-sources-promises ()
  (let* ((since (- +wake-now+ 3600))
         (row (lambda (&key (seq 1) (ago 60) (role "assistant") (session "discord-x") (text "I'll keep an eye on the nightly run."))
                (list :session-id session :title "perf thread" :seq seq :at (wake-iso ago)
                      :role role :text text)))
         (lines (nodecode-cron::promise-lines
                 (list (funcall row :seq 1)
                       (funcall row :seq 1)
                       (funcall row :seq 2 :role "user")
                       (funcall row :seq 3 :ago 7200)
                       (funcall row :seq 4 :session "cron-wake")
                       (funcall row :seq 5 :text "You said you would keep an eye on it.")
                       (funcall row :seq 6 :text "I will check back once CI is green."))
                 since +wake-now+)))
    (is (= 2 (length lines)) "one line per promise: user lines, old ones, a cron session's own and a mention are not promises")
    (is (search "perf thread" (first lines)) "the line names the session")
    (is (search "check back" (second lines)) "'I will' counts as well as 'I'll'")
    (is (search ": I will check back" (second lines)) "the line quotes the promise from where it starts")
    (let ((whole (nodecode-cron::promise-lines
                  (list (funcall row :seq 7 :text "...snippet"))
                  since +wake-now+ :body (lambda (row) (declare (ignore row)) "Merged. I'll follow up tomorrow."))))
      (is (equal '("promised in perf thread: I'll follow up tomorrow.") whole) "the promise is matched in the whole answer, not the search snippet"))))

(deftest cron-cell-wake-sources-waits ()
  (let* ((nodecode-cron::*wake-wait-threshold* 7200)
         (since (- +wake-now+ 900))
         (session (lambda (id quiet-ago &optional (waits ""))
                    (list :id id :title id :updated-at (wake-iso quiet-ago) :waits waits)))
         (lines (nodecode-cron::wait-lines
                 (list (funcall session "crossed" 7500 "merge it?")
                       (funcall session "young" 3600)
                       (funcall session "told-before" 9000)
                       (list :id "answered" :title "answered" :updated-at (wake-iso 7500) :waits nil))
                 since +wake-now+)))
    (is (equal 1 (length lines)) "only the wait that crossed the threshold inside the window")
    (is (search "crossed" (first lines)) "the line names the session")
    (is (search "merge it?" (first lines)) "and what it waits on")
    (is (search "2h" (first lines)) "and how long it has waited")))

(deftest cron-cell-wake-sources-red-runs ()
  (let* ((since (- +wake-now+ 3600))
         (run (lambda (conclusion ago &optional pulls)
                (nlk:decode-json
                 (format nil "{\"name\":\"test\",\"head_branch\":\"wake-in-cron\",\"conclusion\":~s,\"updated_at\":~s,\"html_url\":\"https://example.test/run\",\"pull_requests\":[~{{\"number\":~d}~^,~}]}"
                         conclusion (wake-iso ago) pulls))))
         (lines (nodecode-cron::red-run-lines "o/r" (list (funcall run "failure" 60 '(84)) (funcall run "failure" 7200)
                             (funcall run "success" 60))
                 since +wake-now+)))
    (is (= 1 (length lines)) "a failure that finished in the window, nothing else")
    (is (search "o/r test on wake-in-cron #84" (first lines)) "the line names the repository, the workflow, the branch and its PR")))

(deftest cron-cell-wake-sources-folders ()
  (let ((folder (uiop:ensure-directory-pathname
                 (merge-pathnames (format nil "wake-folder-~d/" (random 1000000)) (uiop:temporary-directory)))))
    (ensure-directories-exist folder)
    (unwind-protect
         (progn
           (with-open-file (out (merge-pathnames "T-1.md" folder) :direction :output :if-exists :supersede)
             (write-line "x" out))
           (let ((now (+ (get-universal-time) 5)))
             (is (= 1 (length (nodecode-cron::folder-lines folder (- now 600) now))) "a file written inside the window is reported")
             (is (null (nodecode-cron::folder-lines folder now (+ now 60))) "and not again once the cursor passed it"))
           (let ((nodecode-cron::*wake-watched-folders* '()))
             (is (null (nodecode-cron::wake-source-folders 0)) "no folder named, no lines")))
      (uiop:delete-directory-tree folder :validate t))))

(deftest cron-cell-wake-sources-red-checks-without-prs ()
  (if (find-package "NODECODE-PRS")
      (is t "the prs cell is loaded in this image")
      (is (null (nodecode-cron::wake-source-red-checks 0)) "without the prs cell the source does not apply")))
