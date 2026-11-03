;;;; spec.lisp --- selffix end to end, in Lisp, on a real organism. SPDX-License-Identifier: MIT
;;;;
;;;; Read by `nodecode trial' (spec.sh starts it): this form is the whole spec.
;;;; A scratch home carries this copy's cron and selffix cells and a planted
;;;; defect (plant/: a generic function with no method for a session id). Real
;;;; turns fail on it through `nodecode rpc'; every check of the cell runs in a
;;;; fresh process of that home through the kernel's own `trial'. The repro is
;;;; checked with a provider key planted in the caller's environment, and the
;;;; plant reports any repro child that sees a key, the network or the home.
;;;; Each line goes to $SELFFIX_SPEC_ROOT/report; the form answers the passes.

(let* ((copy (uiop:ensure-directory-pathname (uiop:getenv "SELFFIX_SPEC_COPY")))
       (root (uiop:ensure-directory-pathname (uiop:getenv "SELFFIX_SPEC_ROOT")))
       (nc (uiop:getenv "SELFFIX_SPEC_NC"))
       (home (merge-pathnames "home/" root))
       (cell (merge-pathnames "tools/nodecode-selffix/" copy))
       (plant (merge-pathnames "cells/selffix-plant/" home))
       (report (merge-pathnames "report" root))
       (passed 0))
  (labels ((say (control &rest args)
             (with-open-file (o report :direction :output :if-exists :append :if-does-not-exist :create)
               (apply #'format o control args)
               (terpri o)))
           (check (ok what &optional why)
             (if ok
                 (progn (incf passed) (say "pass ~a" what))
                 (say "FAIL ~a~@[: ~a~]" what why))
             ok)
           (put (path text)
             (ensure-directories-exist path)
             (with-open-file (o path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-string text o)))
           (text-of (path) (ignore-errors (uiop:read-file-string path)))
           (note (name) (or (text-of (merge-pathnames name root)) "no detail written"))
           (config (selffix)
             (put (merge-pathnames "config.jsonc" home)
                  (format nil "{ \"providers\": { \"stub\": { \"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:9/v1\", \"models\": [\"stub\"] } },~%  \"general\": { \"default_model\": { \"provider\": \"stub\", \"model\": \"stub\" } },~%  \"update\": { \"mode\": \"off\" }~:[~;,~%  \"selffix\": { \"idle_minutes\": 30 }~] }~%"
                          selffix)))
           (child (name form &optional (seconds 900))
             ;; one fresh process of the scratch home; FORM may write its detail to ROOT/NAME
             (multiple-value-bind (n why)
                 (nodecode.kernel::trial
                  (format nil "(flet ((detail (control &rest args) (with-open-file (o ~s :direction :output :if-exists :append :if-does-not-exist :create) (apply (function format) o control args) (terpri o)))) (declare (ignorable (function detail))) (unless (nodecode.kernel::store-open-p) (nodecode.kernel::open-store)) (ignore-errors (nodecode.evolved::restart-cells \"nodecode-selffix\")) (handler-case ~a (error (c) (detail \"error: ~~a\" c) 0)))"
                          (namestring (merge-pathnames name root)) form)
                  :home home :seconds seconds)
               (unless n (say "  ~a: the child gave no score (~a)" name why))
               n))
           (turn (prompt)
             ;; one real turn through `nodecode rpc' => the frame line that ended it, or NIL
             (let* ((process (uiop:launch-program
                              (append (list "env") (nodecode.kernel::throwaway-environment home) (list nc "rpc"))
                              :input :stream :output :stream :error-output nil))
                    (end nil)
                    (watchdog (sb-thread:make-thread
                               (lambda () (sleep 300) (ignore-errors (uiop:terminate-process process :urgent t))))))
               (unwind-protect
                    (let ((in (uiop:process-info-input process)) (out (uiop:process-info-output process)))
                      (format in "{\"type\":\"start_turn\",\"request\":{\"prompt\":~s}}~%" prompt)
                      (finish-output in)
                      (loop for line = (read-line out nil)
                            while line
                            when (or (search "\"turn.failed\"" line) (search "\"turn.completed\"" line)
                                     (search "\"turn.cancelled\"" line))
                              do (setf end line) (loop-finish))
                      (ignore-errors (close in))
                      (ignore-errors (uiop:wait-process process)))
                 (ignore-errors (sb-thread:terminate-thread watchdog))
                 (sleep 2))
               end))
           (findings ()
             ;; .lisp files the cycle wrote outside the cells it was given and its cases
             (remove-if (lambda (p)
                          (let ((s (namestring p)))
                            (or (not (uiop:string-prefix-p (namestring home) s))
                                (search "/cells/selffix-plant/" s) (search "/selffix/cases/" s)
                                (search "/cache/" s))))
                        (directory (merge-pathnames "**/*.lisp" home))))
           (git (dir &rest args)
             (multiple-value-bind (out err code)
                 (uiop:run-program (append (list "git" "-C" (namestring dir)) args)
                                   :output :string :error-output :string :ignore-error-status t)
               (declare (ignore err))
               (values (string-trim '(#\Space #\Newline) out) code)))
           (reproduce (path label)
             (child label (format nil "(let ((w (merge-pathnames \"cells/selffix-plant/window\" (uiop:ensure-directory-pathname (uiop:getenv \"NODECODE_HOME\"))))) (with-open-file (o w :direction :output :if-exists :supersede :if-does-not-exist :create) (write-line \"open\" o)) (unwind-protect (let ((n (nodecode-selffix::reproduce ~s))) (detail \"reproduce answered ~~s\" n) (case n (1 1) (0 0) (t 7))) (ignore-errors (delete-file w))))"
                                  (namestring path)))))
    ;; the home
    (ensure-directories-exist (merge-pathnames "cells/" home))
    (dolist (link (list (cons "nodecode-cron" (merge-pathnames "tools/nodecode-cron/" copy))
                        (cons "nodecode-selffix" cell)))
      (uiop:run-program (list "ln" "-s" (string-right-trim "/" (namestring (cdr link)))
                              (namestring (merge-pathnames (car link) (merge-pathnames "cells/" home))))))
    (dolist (f (directory (merge-pathnames ".github/selffix-spec/lisp/plant/*.*" copy)))
      (put (merge-pathnames (file-namestring f) plant) (uiop:read-file-string f)))
    (put (merge-pathnames "spec-home" plant) (format nil "~a~%" (namestring home)))
    (put (merge-pathnames "spec-root" plant) (format nil "~a~%" (namestring root)))
    (config nil)

    ;; 1. the cell is Lisp
    (let ((foreign (remove-if-not (lambda (p) (member (pathname-type p) '("py" "sh" "in" "bash") :test #'equal))
                                  (directory (merge-pathnames "**/*.*" cell)))))
      (check (null foreign) "the cell is Lisp: no shell or Python under tools/nodecode-selffix"
             (format nil "~{~a~^ ~}" (mapcar #'file-namestring foreign))))

    ;; 2-3. off: a real turn fails on the planted defect, and the cell installs nothing
    (let ((end (turn "SELFFIX-PLANT: tidy the release notes")))
      (check (and end (search "turn.failed" end) (search "NO-APPLICABLE-METHOD" end))
             "a real turn fails on the planted defect with NO-APPLICABLE-METHOD"
             (subseq (or end "no turn ended") 0 (min 300 (length (or end ""))))))
    (let ((files (directory (merge-pathnames "selffix/**/*.*" home))))
      (check (null files) "off (no selffix section): the turn left nothing under selffix/"
             (format nil "~{~a~^ ~}" (mapcar #'file-namestring files))))

    ;; 4. on: the failure is captured as a readable case, its call and arguments
    (config t)
    (turn "SELFFIX-PLANT: tidy the release notes, again")
    (let* ((cases (directory (merge-pathnames "selffix/cases/*.lisp" home)))
           (text (and cases (text-of (first cases)))))
      (check (and text (search "NO-APPLICABLE-METHOD" (string-upcase text))
                  (search "RULES-SECTION" (string-upcase text)) (search ":ARGS" (string-upcase text)))
             "on: the failure is captured as a case: its condition, function and arguments"
             (if text (subseq text 0 (min 300 (length text))) "no selffix/cases/*.lisp")))

    ;; 5. busy: a turn ran a moment ago, so a cycle holds at $0
    (let ((n (child "busy" "(let ((s (nodecode-selffix::cycle))) (detail \"cycle answered ~s\" s) (if (eq s :busy) 1 0))")))
      (check (eql n 1) "busy: a cycle right after a turn holds (:busy) and does nothing" (note "busy")))

    ;; 6-8. idle: the case ranks first, a cycle turns it into a finding, the next moves on
    (let ((n (child "cycle" "(let* ((items (nodecode-selffix::backlog :fixable t)) (top (prin1-to-string (first items)))) (detail \"top: ~a\" (subseq top 0 (min 300 (length top)))) (multiple-value-bind (s1 d1) (nodecode-selffix::cycle :idle-seconds 0) (detail \"first cycle: ~s ~a\" s1 d1) (multiple-value-bind (s2 d2) (nodecode-selffix::cycle :idle-seconds 0) (detail \"next cycle: ~s ~a\" s2 d2) (+ (if (search \"RULES-SECTION\" (string-upcase top)) 1 0) (if (eq s1 :finding) 2 0) (if (member s2 '(:nothing :not-reproduced)) 4 0)))))")))
      (let ((n (or n 0)) (detail (note "cycle")))
        (check (logbitp 0 n) "the backlog ranks the captured case first" detail)
        (check (logbitp 1 n) "an idle cycle turns it into a finding" detail)
        (check (logbitp 2 n) "the next cycle does not take the found case again" detail)))

    ;; 9-10. the finding is a deftest in a .lisp file, committed to git
    (let* ((found (first (findings)))
           (text (and found (text-of found))))
      (check (and text (search "(DEFTEST" (string-upcase text)))
             "the finding is a .lisp file holding a deftest"
             (if found (namestring found) "no .lisp finding outside the cells and cases"))
      (let ((dir (and found (uiop:pathname-directory-pathname found))))
        (check (and dir
                    (eql 0 (nth-value 1 (git dir "ls-files" "--error-unmatch" (file-namestring found))))
                    (string= "" (git dir "status" "--porcelain" "--" (file-namestring found))))
               "the finding is committed to git, its tree clean"
               (if dir (format nil "~a: ~a" (git dir "log" "-1" "--format=%h %s" "--" (file-namestring found))
                               (git dir "status" "--porcelain" "--" (file-namestring found)))
                   "no finding")))

      ;; 11. red: the repro of that finding fails while the defect is there, sealed
      (let ((n (and found (reproduce found "red"))))
        (check (and (eql n 1) (not (probe-file (merge-pathnames "unsealed.txt" root))))
               "the finding's repro fails while the defect is there, in a sealed child (no key, no network, not the home)"
               (format nil "~a~@[; ~a~]" (note "red") (text-of (merge-pathnames "unsealed.txt" root)))))

      ;; 12. green: the defect fixed, the same committed finding passes
      (put (merge-pathnames "fixed" plant) (format nil "fixed~%"))
      (let ((n (and found (reproduce found "green"))))
        (check (and (eql n 0) (equal text (text-of found))
                    (string= "" (git (uiop:pathname-directory-pathname found) "status" "--porcelain" "--" (file-namestring found)))
                    (not (probe-file (merge-pathnames "unsealed.txt" root))))
               "with the defect fixed, the same committed finding passes"
               (format nil "~a~@[; ~a~]" (note "green") (text-of (merge-pathnames "unsealed.txt" root))))))

    ;; 13. the stop sticks across processes
    (child "stop" "(progn (detail \"stop answered ~s\" (nodecode-selffix::selffix :off)) 1)")
    (let ((n (child "stopped" "(let ((s (nodecode-selffix::cycle :idle-seconds 0))) (detail \"cycle after a stop, in a fresh process: ~s\" s) (if (eq s :stopped) 1 0))")))
      (check (eql n 1) "a stop sticks: a fresh process's cycle answers :stopped" (note "stopped")))
    passed))
