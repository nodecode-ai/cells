;;;; check.lisp --- one machine against the whole feed, and the fix for each hit.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-cyber-security)

(defparameter +rank+ '(:vulnerable 0 :error 1 :skipped 2 :manual 3 :ok 4 :absent 5)
  "How a status sorts in a report: what needs a hand first.")

(defparameter +severity-rank+ '("critical" 0 "high" 1 "medium" 2 "low" 3))

(defun verdict (results)
  "An advisory's status from its checks' RESULTS: the worst one that ran."
  (cond ((null results) :manual)
        ((find :vulnerable results :key (lambda (r) (getf r :status))) :vulnerable)
        ((find :error results :key (lambda (r) (getf r :status))) :error)
        ((find :ok results :key (lambda (r) (getf r :status))) :ok)
        ((find :skipped results :key (lambda (r) (getf r :status))) :skipped)
        (t :absent)))

(defun python-package (check)
  "The Python package CHECK's version probe asks about, or NIL."
  (let ((words (and (equal (field check "kind") "version") (python-probe (field check "command")))))
    (and words (car (last words)))))

(defun reads-folder-p (check)
  "CHECK reads a site or project folder: it needs one, or names {dir}."
  (or (eq (needs check) :dir)
      (some (lambda (key)
              (let ((v (field check key)))
                (if (stringp v) (search "{dir}" v)
                    (and (listp v) (some (lambda (w) (and (stringp w) (search "{dir}" w))) v)))))
            '("command" "paths"))))

(defun run-check-everywhere (check &key dir host dirs)
  "CHECK against this machine and, when no DIR is named, every project
folder in DIRS => its results, each carrying :where (a folder, or NIL for
the machine itself). A check that reads a folder runs in each project; a
Python package's version is also read from each project's own virtualenv.
Only what was found is kept: absent everywhere is one :absent result."
  (let ((label (list :label (field check "label") :kind (field check "kind"))))
    (flet ((at (where result) (append label (list :where where) result)))
      (cond
        ((or dir (null dirs) (not (or (reads-folder-p check) (python-package check))))
         (list (at dir (run-check check :dir dir :host host))))
        (t
         (let* ((package (python-package check))
                (machine (and package (run-check check :host host)))
                (found (append
                        (and machine (not (eq (getf machine :status) :absent))
                             (list (list* :label (format nil "~a (system python)" (field check "label"))
                                          (cddr (at nil machine)))))
                        (loop for d in dirs
                              for result = (if package
                                               (let ((v (venv-package-version d package)))
                                                 (when v
                                                   (let ((parsed (parse-version v)))
                                                     (if parsed
                                                         (multiple-value-bind (status why) (judge parsed check)
                                                           (list :status status :found (version-string parsed) :why why))
                                                         (list :status :error :why (format nil "version ~a" v))))))
                                               (run-check check :dir d :host host))
                              when (and result (not (eq (getf result :status) :absent)))
                                collect (at d result)))))
           (or found
               (list (at nil (or (and machine (eq (getf machine :status) :absent)
                                      (list :status :absent
                                            :why (format nil "not on this machine or in its ~d project~:p" (length dirs))))
                                 (list :status :absent
                                       :why (format nil "not in any of the ~d project~:p" (length dirs)))))))))))))

(defun check (&key dir host (fetch nil) (dirs nil dirs-p))
  "Every advisory against this machine. DIR is a site's folder (a WordPress
root, a Node or PHP project), HOST its public name for the certificate
check; both optional. Without DIR, a check that reads a folder runs in every
project this machine holds (DIRS, by default what PROJECTS finds), and a
check that needs a missing one is :SKIPPED.
=> a list of (:id :title :severity :status :results), worst first; each
result names :where it was found (a folder, or NIL for the machine)."
  (let* ((dirs (cond (dir nil) (dirs-p dirs) (t (projects))))
         (rows (loop for a in (advisories :fetch fetch)
                     for results = (loop for c in (append (field a "checks") (field a "file_checks"))
                                         append (run-check-everywhere c :dir dir :host host :dirs dirs))
                     collect (list :id (field a "id") :title (field a "title")
                                   :severity (field a "severity")
                                   :exploited (eq (field a "exploited") t)
                                   :status (verdict results) :results results))))
    (stable-sort rows #'< :key (lambda (r) (+ (* 10 (getf +rank+ (getf r :status)))
                                               (or (second (member (getf r :severity) +severity-rank+
                                                                   :test #'equal))
                                                   9))))))

(defun present-p (row)
  "ROW touches this machine: a check found what the advisory names -- a
vulnerable build, a fixed one, or an error reading it."
  (some (lambda (c)
          (case (getf c :status)
            ((:vulnerable :error) t)
            (:ok (and (getf c :found)
                      (not (member (getf c :kind) '("command" "files") :test #'equal))))))
        (getf row :results)))

(defun report-row (s r)
  (format s "~a ~a~:[~; (exploited)~] ~a -- ~a~%"
          (case (getf r :status)
            (:vulnerable "PATCH") (:error "ERROR") (:skipped "SKIP ")
            (:manual "READ ") (:ok "ok   ") (t "--   "))
          (string-upcase (or (getf r :severity) "")) (getf r :exploited)
          (getf r :id) (getf r :title))
  (flet ((line (c)
           (format s "    ~a~@[ in ~a~]: ~a~@[ ~a~]~%" (getf c :label)
                   (and (getf c :where) (short-folder (getf c :where)))
                   (getf c :why)
                   (and (eq (getf c :status) :vulnerable) (getf c :found)
                        (not (search (getf c :found) (getf c :why)))
                        (getf c :found)))))
    (let* ((results (getf r :results))
           (bad (remove-if-not (lambda (c) (member (getf c :status) '(:vulnerable :error :skipped))) results))
           ;; A fixed build is worth a line; a sign-of-compromise search that found nothing is not.
           (fine (remove-if-not (lambda (c) (and (eq (getf c :status) :ok) (getf c :found)
                                                  (not (member (getf c :kind) '("command" "files") :test #'equal))))
                                results)))
      (mapc #'line (sort (copy-list bad) #'< :key (lambda (c) (getf +rank+ (getf c :status)))))
      (cond ((null fine))
            ((and bad (eq (getf r :status) :vulnerable))
             (format s "    already fixed ~{~a~^, ~}~%"
                     (loop for c in fine
                           collect (if (getf c :where)
                                       (format nil "in ~a (~a)" (short-folder (getf c :where)) (getf c :found))
                                       (format nil "on this machine (~a)" (getf c :found))))))
            (t (mapc #'line fine))))))

(defun report (&key dir host (fetch nil) rows all (dirs nil dirs-p))
  "CHECK's answer as text a person reads: what this machine and its projects
need patched first, then what they run at a fixed version, one line each.
Advisories that name nothing found here fold into one count line; ALL lists
every advisory instead, the way the feed has them."
  (let* ((dirs (cond (dir nil) (dirs-p dirs) (rows nil) (t (projects))))
         (rows (or rows (check :dir dir :host host :fetch fetch :dirs dirs)))
         (shown (if all rows (remove-if-not #'present-p rows)))
         (hidden (if all '() (remove-if #'present-p rows))))
    (with-output-to-string (s)
      (format s "~a~%" (feed-status))
      (format s "checked: this machine~@[, site folder ~a~]~@[, host ~a~]" dir host)
      (when dirs
        (if (<= (length dirs) 8)
            (format s " and ~d project~:p: ~{~a~^, ~}" (length dirs) (mapcar #'short-folder dirs))
            (format s " and its ~d projects (/security projects lists them)" (length dirs))))
      (format s "~%~%")
      (if shown
          (dolist (r shown) (report-row s r))
          (format s "Nothing in the feed touches this machine~:[~; or its projects~].~%" dirs))
      (let ((hits (count :vulnerable shown :key (lambda (r) (getf r :status)))))
        (format s "~%~d to patch~:[~;: (security:show \"ID\") gives each fix~].~%" hits (plusp hits)))
      (when hidden
        (let ((manual (count :manual hidden :key (lambda (r) (getf r :status))))
              (skipped (count :skipped hidden :key (lambda (r) (getf r :status)))))
          (format s "Not shown: ~d more in the feed~@[, ~d naming software found nowhere here~]~@[, ~d with no machine check (a platform or appliance breach: act if you use it)~]~@[, ~d that could not run here (a host name, or a newer copy of this cell)~]. /security all lists every one.~%"
                  (length hidden)
                  (let ((n (- (length hidden) manual skipped))) (and (plusp n) n))
                  (and (plusp manual) manual) (and (plusp skipped) skipped)))))))

(defun commands (id &key dir host)
  "What advisory ID's checks run, as this copy runs them, for a machine the
report cannot reach (a server over ssh): one (:label L :run WORDS) per check
this copy vouches for, or (:label L :not-run WHY). Checks that run no
program (npm, composer, wp-plugin, files) read files: their fields say which."
  (let ((a (advisory id)))
    (loop for c in (and a (field a "checks"))
          when (member (field c "kind") '("version" "command") :test #'equal)
            collect (multiple-value-bind (words why) (runnable (field c "command") dir host)
                      (if words
                          (list :label (field c "label") :run words)
                          (list :label (field c "label") :not-run why))))))

(defun show (id)
  "One advisory in full, as text: what, who, the fix, the stopgap, the signs, the sources."
  (let ((a (advisory id)))
    (unless a
      (return-from show (format nil "no advisory named ~a; (security:advisories) lists them" id)))
    (with-output-to-string (s)
      (format s "~a~%~a, ~a~:[~;, exploited~]~@[, ~{~a~^ ~}~]~%~%~a~%~%Affected: ~a~%"
              (field a "title") (field a "date") (string-upcase (field a "severity" ""))
              (eq (field a "exploited") t) (field a "cves") (field a "summary") (field a "affected"))
      (when (field a "fixed") (format s "Fixed in: ~{~a~^, ~}~%" (field a "fixed")))
      (loop for (key head) in '(("patch" "Patch") ("mitigation" "Until you can patch")
                                ("iocs" "Signs of compromise") ("sources" "Sources"))
            for items = (field a key)
            when items do (format s "~%~a:~%~{- ~a~%~}" head items)))))
