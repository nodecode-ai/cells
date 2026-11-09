;;;; cell-test.lisp --- the classifier, the rewrites, the hook, the tool, /headroom.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell reads of the host comes through one parameter,
;;;; *PROC-ROOT*, so the tests fabricate a /proc of their own: memory, PSI,
;;;; a CPU count and a few processes. Nothing here touches the network, and
;;;; nothing spawns a process: the hook's NEXT is a stub.

(in-package #:nodecode.test)

;;; --- a fabricated /proc ----------------------------------------------------------

(defun write-proc (root relative text)
  (let ((path (merge-pathnames relative root)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))))

(defmacro with-fake-proc ((&key (total 7800) (available 2400) (swap-total 2048)
                                         (swap-free 1900) (cpus 4) (rss 1258291))
                                     &body body)
  "Run BODY with *PROC-ROOT* a fabricated tree: TOTAL and AVAILABLE MB,
SWAP-TOTAL and SWAP-FREE, CPUS processors and one process of RSS KB."
  (let ((root (gensym "ROOT")))
    `(let* ((,root (merge-pathnames (format nil "headroom-proc-~d/" (random 1000000))
                                     (uiop:temporary-directory)))
            (nodecode-headroom::*proc-root* (namestring ,root)))
       (ensure-directories-exist ,root)
       (unwind-protect
            (progn
              (write-proc ,root "meminfo"
                          (format nil "MemTotal: ~d kB~%MemFree: 1024 kB~%MemAvailable: ~d kB~%SwapTotal: ~d kB~%SwapFree: ~d kB~%"
                                  ,(* total 1024) ,(* available 1024)
                                  ,(* swap-total 1024) ,(* swap-free 1024)))
              (write-proc ,root "pressure/memory"
                          (format nil "some avg10=1.25 avg60=0.50 avg300=0.10 total=100~%full avg10=0.30 avg60=0.20 avg300=0.05 total=50~%"))
              (write-proc ,root "cpuinfo"
                          (with-output-to-string (out)
                            (loop for i below ,cpus
                                  do (format out "processor~c: ~d~%" #\Tab i))))
              (write-proc ,root "5101/stat"
                          (format nil "5101 (node) S 1 5101 5101 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0~%"))
              (write-proc ,root "5101/status"
                          (format nil "Name:~cnode~%VmRSS: ~d kB~%" #\Tab ,rss))
              (write-proc ,root "5102/stat"
                          (format nil "5102 (make) S 5101 5102 5102 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0~%"))
              (write-proc ,root "5102/status"
                          (format nil "Name:~cmake~%VmRSS: 61440 kB~%" #\Tab))
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,root :validate t))))))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "headroom"
  (:hooks :tool :round)
  (:help :headroom)
  (:command "headroom")
  (:running (is (nle::find-tool "headroom_status") "the tool is registered")
            (is (equal "enforce" (nodecode-headroom::setting :mode)) "the default mode enforces")
            (is (search "memory guard" (nle:help :headroom)) "the manual is up"))
  (:stopped (is (null (nle::find-tool "headroom_status")) "stop unregisters the tool"))
  (:refused ("mode" "sometimes") ("queue_timeout_s" 0)))

;;; --- what a command costs ---------------------------------------------------------

(deftest headroom-cell-knows-a-heavy-command-from-a-light-one ()
  (with-cell-stop ((headroom-start))
    (dolist (heavy '("npm install" "npm run build" "npx jest" "pytest -q" "cargo test"
                     "go build ./..." "make -j8" "docker build ." "ffmpeg -i a.mp4 b.mp4"
                     "firefox https://example.com/"))
      (is (getf (nodecode-headroom::classify-command heavy) :heavy) heavy))
    (dolist (light '("git status" "ls -la" "git commit -m \"npm install the docs\""
                     "make --version" "rg -n todo src"))
      (is (not (getf (nodecode-headroom::classify-command light) :heavy)) light))
    (is (equal '("js-install") (getf (nodecode-headroom::classify-command "npm install") :rules)) "the family that matched is named")
    (is (eq t (getf (nodecode-headroom::classify-command
                      "git commit -m \"x\" && npm run build")
                     :heavy)) "a heavy command later in a list still counts")))

(deftest headroom-cell-lightens-what-it-runs-and-says-how ()
  (with-cell-stop ((headroom-start))
    (multiple-value-bind (text changes)
        (nodecode-headroom::lighten "npm test" :budget-mb 1200 :cpus 4 :low-memory t)
      (is (search "NODE_OPTIONS=" text) "a Node heap cap is prefixed")
      (is (search "--max-old-space-size=840" text) "at 70% of the job's budget")
      (is (= 1 (length changes)) "one line says what changed"))
    (multiple-value-bind (text changes)
        (nodecode-headroom::lighten "npx jest" :budget-mb 1200 :cpus 4 :low-memory t)
      (is (search "--runInBand" text) "a test runner drops to one process")
      (is (some (lambda (change) (search "jest" change)) changes) "the runner change is named"))
    (multiple-value-bind (text changes)
        (nodecode-headroom::lighten "make -j16" :budget-mb 1400 :cpus 4 :low-memory t)
      (declare (ignore changes))
      (is (search " -j2" text) "a build is capped at what the budget feeds"))
    (multiple-value-bind (text changes)
        (nodecode-headroom::lighten "go build ./..." :budget-mb 1200 :cpus 8 :low-memory t)
      (declare (ignore changes))
      (is (search "-p 1" text) "and a Go build drops to one package at a time"))
    (multiple-value-bind (text changes)
        (nodecode-headroom::lighten "git status" :budget-mb 1200 :cpus 4 :low-memory t)
      (is (equal "git status" text) "a light command is untouched")
      (is (null changes)))))

;;; --- the hook ---------------------------------------------------------------------

(deftest headroom-cell-rewrites-the-form-it-hands-on ()
  (with-cell-stop ((headroom-start))
    (let* ((arguments (nlk:json-object "form" "(sh \"npm test\")"))
           (seen nil)
           (answer (funcall (cell-hook "nodecode-headroom" :tool)
                            (list :name "eval" :arguments arguments :call-id "c-heavy")
                            (lambda (op) (setf seen (getf op :arguments)) "the tests passed"))))
      (is (search "NODE_OPTIONS=" (gethash "form" seen)) "the form next runs carries the cap")
      (is (search "the tests passed" answer) "the result is the command's own")
      (is (search "[headroom]" answer) "with one line saying what headroom did")
      (is (not (search "guard error" answer)) (nodecode-headroom::events-text))
      (is (string= "(sh \"npm test\")" (gethash "form" arguments)) "the model's own arguments are not mutated"))))

(deftest headroom-cell-leaves-a-light-call-alone ()
  (with-cell-stop ((headroom-start))
    (let* ((arguments (nlk:json-object "form" "(sh \"git status\")"))
           (seen nil)
           (answer (funcall (cell-hook "nodecode-headroom" :tool)
                            (list :name "eval" :arguments arguments :call-id "c-light")
                            (lambda (op) (setf seen op) "clean"))))
      (is (equal "clean" answer) "the result is untouched")
      (is (eq arguments (getf seen :arguments)) "and so is the call"))))

;;; --- the tool, the command, the manual ---------------------------------------------

(deftest headroom-cell-the-tool-and-the-command-answer-from-the-host ()
  (with-fake-proc (:total 7800 :available 2400)
    (with-cell-stop ((headroom-start))
      (let* ((report (nlk:contract-decode
                      (nlk:contract 'nodecode-headroom::headroom-status)
                      (nlk:json-object "explain" "npm run build")))
             (data (nlk:decode-json (nodecode-headroom::headroom-status-tool report)))
             (memory (gethash "memory" data))
             (budget (gethash "budget" data)))
        (is (equal "enforce" (gethash "mode" data)))
        (is (= 2400 (gethash "available_mb" memory)) "free memory is read from the host")
        (is (= 7800 (gethash "total_mb" memory)))
        (is (= 148 (gethash "swap_used_mb" memory)) "swap used is total minus free")
        (is (search "heavy" (gethash "explain" data)) "explain classifies the command")
        (is (plusp (gethash "next_job_cap_mb" budget)) "a cap is computed")
        (is (>= (gethash "start_needs_available_mb" budget) 1024) "a start needs room")
        (is (vectorp (gethash "top_processes" data))) "the largest processes are listed"
        (is (hash-table-p (gethash "pressure" data)) "pressure present as one object"))
      (is (search "free of" (nodecode-headroom::run-headroom "status" "s1")) "/headroom status is the operator's line")
      (is (search "headroom (enforce)" (nodecode-headroom::run-headroom "status" "s1")))
      (is (search "heavy (js-install)" (nodecode-headroom::run-headroom "explain npm install" "s1")) "/headroom explain says what it would do")
      (is (search "light:" (nodecode-headroom::run-headroom "explain git status" "s1")))
      (is (search "started" (nodecode-headroom::run-headroom "events" "s1")) "/headroom events lists the log"))))

(deftest headroom-cell-a-queued-call-is-refused-when-no-slot-frees ()
  (with-fake-proc (:total 7800 :available 2400)
    (with-cell-stop ((headroom-start "queue_timeout_s" 1 "max_heavy_jobs" 1))
      ;; The cell holds its one slot, as a call in flight would.
      (sb-thread:wait-on-semaphore nodecode-headroom::*slots* :timeout 1)
      (let* ((arguments (nlk:json-object "form" "(sh \"npm install\")"))
             (ran nil)
             (answer (funcall (cell-hook "nodecode-headroom" :tool)
                              (list :name "eval" :arguments arguments :call-id "c-queued")
                              (lambda (op) (declare (ignore op)) (setf ran t) "ran"))))
        (is (null ran) "the command did not run")
        (is (search "ERROR: headroom" answer) "the model is told why")
        (is (search "did not run" answer)))
      (sb-thread:signal-semaphore nodecode-headroom::*slots*))))
