;;;; validate.lisp --- what a feed must be before it is published.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every installed copy reads advisories.json from main the moment it is
;;;; pushed, and the copies already out there read it with the code they were
;;;; installed with. So a feed is published only when FEED-PROBLEMS answers
;;;; NIL for it: well-formed JSON, every field a reader uses present and of
;;;; its type, every check a kind this cell reads with the fields that kind
;;;; needs, every version one PARSE-VERSION reads. The six-hourly update (see
;;;; UPDATING.md) runs it before every push, with :SINCE the feed it replaces,
;;;; so the job can add advisories but never a new command: copies installed
;;;; before commands were pinned (detect.lisp) still run what main names. The
;;;; tests run it on the copy shipped here.

(in-package #:nodecode-cyber-security)

(defparameter +check-kinds+ '("version" "npm" "composer" "wp-plugin" "command" "files")
  "The check kinds RUN-CHECK reads; a feed may use no other.")

(defparameter +file-check-kinds+ '("lockfile" "gh-action" "uses" "npm" "composer" "wp-plugin" "files")
  "The kinds an advisory's FILE_CHECKS may use: each reads files and runs no
program. The first three are newer than CHECKS' kinds, so they live under
their own key, which copies installed before them never read.")

(defparameter +ecosystems+ '("crates.io" "Go" "Maven" "RubyGems" "PyPI" "npm" "Packagist")
  "A lockfile check's ECOSYSTEM, in OSV's names.")

(defparameter +severities+ '("critical" "high" "medium" "low"))

(defun strings-p (value &key (min 1))
  "VALUE is a list (or vector) of at least MIN non-empty strings."
  (let ((items (if (and (vectorp value) (not (stringp value))) (coerce value 'list) value)))
    (and (listp items) (>= (length items) min)
         (every (lambda (s) (and (stringp s) (plusp (length (string-trim " " s))))) items))))

(defun text-p (value) (and (stringp value) (plusp (length (string-trim " " value)))))

(defun check-problems (id n c &key (kinds +check-kinds+) (key "check"))
  "What is wrong with check N of advisory ID; KINDS are the kinds its list
may use, KEY names that list in a problem."
  (let ((kind (field c "kind")) (out '()))
    (flet ((bad (fmt &rest args) (push (format nil "~a: ~a ~d (~a): ~?" id key n kind fmt args) out)))
      (cond
        ((not (member kind kinds :test #'equal))
         (bad "kind is not one of ~{~a~^, ~}" kinds))
        (t
         (when (member kind '("version" "command") :test #'equal)
           (unless (strings-p (field c "command")) (bad "command is not a list of strings")))
         (when (equal kind "npm") (unless (text-p (field c "package")) (bad "package is missing")))
         (when (equal kind "composer") (unless (strings-p (field c "packages")) (bad "packages is not a list of strings")))
         (when (equal kind "wp-plugin") (unless (strings-p (field c "slugs")) (bad "slugs is not a list of strings")))
         (when (equal kind "files") (unless (strings-p (field c "paths")) (bad "paths is not a list of strings")))
         (when (equal kind "lockfile")
           (unless (member (field c "ecosystem") +ecosystems+ :test #'equal)
             (bad "ecosystem is not one of ~{~a~^, ~}" +ecosystems+))
           (unless (text-p (field c "package")) (bad "package is missing"))
           (when (and (equal (field c "ecosystem") "Maven") (text-p (field c "package"))
                      (not (cl-ppcre:scan "^[^:\\s]+:[^:\\s]+$" (field c "package"))))
             (bad "a Maven package is group:artifact")))
         (when (equal kind "gh-action")
           (unless (and (text-p (field c "action"))
                        (cl-ppcre:scan "^[A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+$" (field c "action")))
             (bad "action is not owner/repo")))
         (when (equal kind "uses")
           (unless (text-p (field c "service")) (bad "service is missing"))
           (unless (or (field c "paths") (field c "env") (field c "programs"))
             (bad "names no sign: paths, env or programs"))
           (let ((folder (remove-if-not (lambda (p) (and (stringp p) (search "{dir}" p))) (field c "paths"))))
             (when (and folder (or (field c "env") (field c "programs")
                                   (/= (length folder) (length (field c "paths")))))
               (bad "mixes a folder's signs ({dir}) with the machine's: one check each, since a folder's check runs in every project")))
           (dolist (p (field c "paths"))
             (unless (and (text-p p) (or (uiop:string-prefix-p "~/" p) (uiop:string-prefix-p "{dir}/" p)
                                         (uiop:string-prefix-p "/" p)))
               (bad "path ~s does not start with ~~/, {dir}/ or /" p)))
           (dolist (e (field c "env"))
             (unless (and (stringp e) (cl-ppcre:scan "^[A-Za-z_][A-Za-z0-9_]*$" e))
               (bad "env ~s is not a variable name" e)))
           (dolist (p (field c "programs"))
             (unless (and (stringp p) (cl-ppcre:scan "^[A-Za-z0-9][A-Za-z0-9._+-]*$" p))
               (bad "program ~s is not a file name" p))))
         (when (member kind '("version" "npm" "composer" "wp-plugin" "lockfile" "gh-action") :test #'equal)
           (let ((fixed (field c "fixed")) (badv (field c "bad")))
             (unless (or (strings-p fixed) (strings-p badv))
               (bad "names neither fixed nor bad versions"))
             (dolist (v (append (and (listp fixed) fixed)
                                ;; "*" in bad: every version is the bad one (a poisoned package)
                                (remove "*" (and (listp badv) badv) :test #'equal)
                                (and (field c "min") (list (field c "min")))))
               (unless (and (stringp v) (parse-version v))
                 (bad "version ~s does not parse" v)))))
         (when (field c "backport")
           (unless (strings-p (field c "backport")) (bad "backport is not a list of strings")))
         (when (field c "needs")
           (unless (member (field c "needs") '("dir" "host") :test #'equal) (bad "needs is not dir or host")))
         (when (field c "line")
           (unless (and (integerp (field c "line")) (<= 1 (field c "line") 4)) (bad "line is not 1-4")))
         (when (field c "match")
           (unless (and (stringp (field c "match"))
                        (handler-case (progn (cl-ppcre:create-scanner (field c "match")) t) (error () nil)))
             (bad "match is not a regular expression"))))))
    (nreverse out)))

(defun advisory-problems (a)
  "What is wrong with the advisory hash table A."
  (let* ((id (field a "id")) (name (if (text-p id) id "(no id)")) (out '()))
    (flet ((bad (fmt &rest args) (push (format nil "~a: ~?" name fmt args) out)))
      (unless (and (text-p id) (cl-ppcre:scan "^[a-z0-9][a-z0-9.-]*$" id)) (bad "id is not a lowercase slug"))
      (unless (text-p (field a "title")) (bad "title is missing"))
      (unless (and (stringp (field a "date")) (cl-ppcre:scan "^\\d{4}-\\d{2}-\\d{2}$" (field a "date")))
        (bad "date is not YYYY-MM-DD"))
      (unless (member (field a "severity") +severities+ :test #'equal)
        (bad "severity is not one of ~{~a~^, ~}" +severities+))
      (unless (nth-value 1 (gethash "exploited" a)) (bad "exploited is missing (true or false)"))
      (unless (member (gethash "exploited" a) '(t nil)) (bad "exploited is not true or false"))
      (unless (text-p (field a "summary")) (bad "summary is missing"))
      (unless (text-p (field a "affected")) (bad "affected is missing"))
      (unless (strings-p (field a "patch")) (bad "patch says no fix"))
      (unless (and (strings-p (field a "sources"))
                   (every (lambda (u) (cl-ppcre:scan "^https?://\\S+$" u)) (field a "sources")))
        (bad "sources are not links"))
      (dolist (cve (field a "cves"))
        (unless (and (stringp cve) (cl-ppcre:scan "^(CVE-\\d{4}-\\d{4,}|GHSA(-[0-9a-z]{4}){3})$" cve))
          (bad "~s is not a CVE or GHSA id" cve)))
      (dolist (key '("mitigation" "iocs" "fixed"))
        (let ((v (gethash key a)))
          (when (and v (not (or (stringp v) (strings-p v :min 0))))
            (bad "~a is not a list of strings" key))))
      (loop for c in (field a "checks") for n from 1
            do (if (hash-table-p c)
                   (dolist (p (check-problems name n c)) (push p out))
                   (bad "check ~d is not an object" n)))
      (loop for c in (field a "file_checks") for n from 1
            do (if (hash-table-p c)
                   (dolist (p (check-problems name n c :kinds +file-check-kinds+ :key "file_check"))
                     (push p out))
                   (bad "file_check ~d is not an object" n))))
    (nreverse out)))

(defun new-command-problems (entries since)
  "A problem for every command ENTRIES run that SINCE (the feed text they
replace) does not, the Python-package probe aside."
  (let ((known (command-set (read-feed since))) (out '()))
    (dolist (a entries (nreverse out))
      (loop for c in (field a "checks") for n from 1
            when (hash-table-p c)
              do (dolist (key '("command" "backport"))
                   (let ((words (field c key)))
                     (when (and (consp words) (every #'stringp words)
                                (not (python-probe words)) (not (gethash words known)))
                       (push (format nil "~a: check ~d (~a): its ~a is new; only a person publishes a new command (UPDATING.md, section 5)"
                                     (field a "id") n (field c "kind") key)
                             out))))))))

(defun feed-problems (text &key since)
  "Every reason the feed TEXT (advisories.json's contents) must not be
published, one string each; NIL means it may. SINCE, the feed TEXT
replaces, is the unattended job's gate: a command SINCE does not carry is
then a problem too, so a feed written from web news never adds a program to
run -- a person publishes that, and an installed copy runs it only once a
release pins it (detect.lisp)."
  (let ((obj (handler-case (shasht:read-json text) (error (e) (return-from feed-problems
                                                                  (list (format nil "not JSON: ~a" e)))))))
    (unless (hash-table-p obj) (return-from feed-problems (list "not a JSON object")))
    (let ((entries (gethash "advisories" obj)) (out '()))
      (unless (eql (gethash "format" obj) 1) (push "format is not 1" out))
      (unless (and (stringp (gethash "updated" obj))
                   (cl-ppcre:scan "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}(:\\d{2})?Z$" (gethash "updated" obj)))
        (push "updated is not an ISO time like 2026-10-22T04:00:00Z" out))
      (cond
        ((not (and (vectorp entries) (not (stringp entries)) (plusp (length entries))))
         (push "advisories is not a non-empty list" out))
        (t
         (let ((ids '()))
           (loop for a across entries for n from 1
                 do (cond ((not (hash-table-p a)) (push (format nil "advisory ~d is not an object" n) out))
                          (t (when (member (gethash "id" a) ids :test #'equal)
                               (push (format nil "~a: id is used twice" (gethash "id" a)) out))
                             (push (gethash "id" a) ids)
                             (dolist (p (advisory-problems a)) (push p out))))))
         (when since
           (dolist (p (new-command-problems (coerce entries 'list) since)) (push p out)))))
      (nreverse out))))
