;;;; store.lisp --- the vault: four tab-separated files under one folder.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original keeps SQLite with an FTS5 index; this rewrite keeps the
;;;; same records as plain TSV files, so what the cell knows is readable with
;;;; any tool the user already has and there is no database library to
;;;; bundle. One folder, four files, and the folder is the only setting the
;;;; cell refuses to start without:
;;;;
;;;;   records.tsv    one line per note, medication event, annotation or import
;;;;   genotypes.tsv   rsid, chromosome, position, genotype, one line per marker
;;;;   samples.tsv     metric, day, value, unit: one line per Apple Health day-stat
;;;;   sources.tsv     what was imported, when, and how many rows it gave
;;;;
;;;; Everything here is local. Nothing is uploaded, and no file outside the
;;;; folder is written.

(in-package #:nodecode-caduceus)

;;; --- the folder ---------------------------------------------------------------

(defparameter +records-file+ "records.tsv")
(defparameter +genotypes-file+ "genotypes.tsv")
(defparameter +samples-file+ "samples.tsv")
(defparameter +sources-file+ "sources.tsv")

(defun nonblank (text)
  "TEXT trimmed, or NIL when it is not a non-empty string."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun data-dir ()
  "The vault folder: the data_dir setting, created on first use. A refusal
names the setting and what to put in it."
  (let ((raw (setting :data-dir)))
    (unless (nonblank (if (pathnamep raw) (namestring raw) raw))
      (fail "no data_dir is set: set it to a folder this cell may keep its vault in (for example a folder of your own inside your home), then restart the cell"))
    (let ((dir (uiop:ensure-directory-pathname
                (nlk:expand-home (if (pathnamep raw) (namestring raw) raw)))))
      (ensure-directories-exist (merge-pathnames "vault.txt" dir))
      dir)))

(defun vault-file (name)
  (merge-pathnames name (data-dir)))

;;; --- one TSV shape for all four files ---------------------------------------

(defparameter +tab+ (string #\Tab)
  "The field separator. A tab inside a field is escaped, so a line is always
one record.")

(defun escape-field (text)
  "TEXT with the three characters a line cannot carry escaped."
  (with-output-to-string (out)
    (loop for char across (princ-to-string (or text ""))
          do (case char
               (#\\ (write-string "\\\\" out))
               (#\Tab (write-string "\\t" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (t (write-char char out))))))

(defun unescape-field (text)
  "TEXT with the escapes ESCAPE-FIELD writes turned back into characters."
  (let ((text (or text "")))
    (if (null (position #\\ text))
        text
        (with-output-to-string (out)
          (loop with length = (length text) with index = 0
                while (< index length)
                do (let ((char (char text index)))
                     (cond ((and (char= char #\\) (< (1+ index) length))
                            (let ((next (char text (1+ index))))
                              (write-char (case next
                                            (#\t #\Tab)
                                            (#\n #\Newline)
                                            (#\r #\Return)
                                            (t next))
                                          out))
                            (incf index 2))
                           (t (write-char char out)
                              (incf index)))))))))

(defun split-line (line)
  "LINE's fields, unescaped."
  (loop with fields = '() with start = 0
        for tab = (position #\Tab line :start start)
        do (push (unescape-field (subseq line start tab)) fields)
           (if tab (setf start (1+ tab)) (return (nreverse fields)))))

(defun read-rows (path)
  "PATH's rows, each a list of fields, in the order written. A missing file
is no rows."
  (if (probe-file path)
      (with-open-file (in path :direction :input :external-format :utf-8
                               :if-does-not-exist nil)
        (when in
          (loop for line = (read-line in nil nil)
                while line
                collect (split-line line))))
      '()))

(defun row-line (fields)
  "FIELDS escaped and joined with a tab: one line of a vault file."
  (with-output-to-string (out)
    (loop for field in fields
          for first = t then nil
          do (unless first (write-string +tab+ out))
             (write-string (escape-field field) out))))

(defun append-row (path &rest fields)
  "Append one row to PATH, creating it when absent."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :append
                            :if-does-not-exist :create :external-format :utf-8)
    (write-line (row-line fields) out))
  path)

(defun write-rows (path rows)
  "PATH with exactly ROWS, atomically: a temp file in the same folder, then
a rename. The old copy is replaced only once the new one is whole."
  (ensure-directories-exist path)
  (let* ((temp (merge-pathnames (format nil "~a.part" (file-namestring path)) path)))
    (with-open-file (out temp :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (dolist (row rows)
        (write-line (row-line row) out)))
    (rename-file temp path)
    path))

;;; --- records ------------------------------------------------------------------

(defun record-row (record)
  "RECORD, a plist, as the six fields RECORDS.TSV carries."
  (list (getf record :kind) (getf record :category) (getf record :ref)
        (getf record :title) (getf record :body) (getf record :ts)))

(defun all-records ()
  "Every record in the vault, newest last, as plists."
  (loop for (kind category ref title body ts) in (read-rows (vault-file +records-file+))
        collect (list :kind kind :category category :ref ref :title title :body body :ts ts)))

(defun append-record (record)
  (append-row (vault-file +records-file+) (getf record :kind) (getf record :category)
              (getf record :ref) (getf record :title) (getf record :body) (getf record :ts))
  record)

(defun next-ref (kind)
  "A fresh ref for a record of KIND: the count already stored plus one."
  (format nil "~a:~d" kind
          (1+ (count kind (all-records) :key (lambda (r) (getf r :kind)) :test #'string=))))

;;; --- sources ------------------------------------------------------------------

(defun append-source (path kind rows)
  (append-row (vault-file +sources-file+) (namestring path) kind (princ-to-string rows)
              (iso-now))
  t)

(defun all-sources ()
  (loop for (path kind rows at) in (read-rows (vault-file +sources-file+))
        collect (list :path path :kind kind :rows (parse-integer rows :junk-allowed t) :at at)))

;;; --- time ---------------------------------------------------------------------

(defun iso-now ()
  "Now as the ISO stamp the vault writes."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))

(defun iso-day-p (text)
  "TEXT as YYYY-MM-DD, or NIL when it is not a day."
  (and (stringp text) (>= (length text) 10)
       (loop for index from 0 below 10
             for expect-digit = (not (member index '(4 7)))
             for char = (char text index)
             always (if expect-digit (digit-char-p char) (char= char #\-)))
       (subseq text 0 10)))

(defun day-number (day)
  "DAYS since 1970-01-01 for the ISO DAY, or NIL."
  (when (iso-day-p day)
    (floor (- (encode-universal-time 0 0 0 (parse-integer day :start 8 :end 10)
                                     (parse-integer day :start 5 :end 7)
                                     (parse-integer day :start 0 :end 4) 0)
              (encode-universal-time 0 0 0 1 1 1970 0))
           86400)))

(defun day-from-number (number)
  "The ISO day NUMBER days after 1970-01-01."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ (* number 86400)
                                 (encode-universal-time 0 0 0 1 1 1970 0))
                             0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun day+ (day offset)
  (let ((number (day-number day)))
    (and number (day-from-number (+ number offset)))))

;;; --- privacy: level, categories, what may be shown --------------------------------

(defparameter +categories+
  '("general" "pgx" "trait" "wellness" "records" "medication" "research"
    "alzheimers" "cancer_risk" "carrier" "mental_health" "reproductive")
  "Every category a record may carry. The last five are the sensitive ones:
the right not to know them is the point of the standard level.")

(defparameter +sensitive+
  '("alzheimers" "cancer_risk" "carrier" "mental_health" "reproductive"))

(defun policy ()
  "The settings that decide what the tools may show, as one plist. An
unknown value fails closed: a level that is not one the declaration allows
was already refused at start, and a category nothing names is refused here."
  (list :level (string-downcase (princ-to-string (setting :privacy-level)))
        :hidden (mapcar (lambda (item) (string-downcase (princ-to-string item)))
                        (or (setting :hidden-categories) '()))
        :unlocked (mapcar (lambda (item) (string-downcase (princ-to-string item)))
                           (or (setting :unlocked-categories) '()))))

(defun category-allowed-p (policy category)
  "True when POLICY may show an item of CATEGORY. An unknown category is
refused, never guessed."
  (let ((category (string-downcase (princ-to-string (or category "")))))
    (cond ((not (member category +categories+ :test #'string=)) nil)
          ((member category (getf policy :hidden) :test #'string=) nil)
          ((and (not (string= (getf policy :level) "open"))
                (member category +sensitive+ :test #'string=)
                (not (member category (getf policy :unlocked) :test #'string=)))
           nil)
          (t t))))

(defparameter +kind-base-category+
  '(("observation" . "records") ("medication" . "medication")
    ("research" . "research") ("pgx" . "pgx"))
  "The base category of a kind: hiding `medication' hides every medication,
whatever else the record is.")

(defun visible-p (policy category &key kind)
  "True when a record of CATEGORY, of KIND, may be shown: its own category
and its kind's base category are both allowed."
  (and (category-allowed-p policy category)
       (let ((base (cdr (assoc kind +kind-base-category+ :test #'string=))))
         (or (null base) (category-allowed-p policy base)))))

(defun policy-object ()
  "The policy as the model reads it, with the sensitive categories it shows."
  (let ((policy (policy)))
    (nlk:json-object
     "privacy_level" (getf policy :level)
     "hidden_categories" (coerce (getf policy :hidden) 'vector)
     "unlocked_categories" (coerce (getf policy :unlocked) 'vector)
     "sensitive_categories_shown"
     (coerce (remove-if-not (lambda (category) (category-allowed-p policy category))
                             +sensitive+)
             'vector)
     "categories" (coerce +categories+ 'vector))))

;;; --- the category a drug name implies -------------------------------------------

(defparameter +mental-health-drugs+
  '("citalopram" "escitalopram" "fluoxetine" "fluvoxamine" "paroxetine" "sertraline" "venlafaxine"
    "desvenlafaxine" "duloxetine" "amitriptyline" "clomipramine" "imipramine" "nortriptyline"
    "bupropion" "mirtazapine" "trazodone" "lithium" "atomoxetine" "methylphenidate"
    "lisdexamfetamine" "amphetamine" "buprenorphine" "naltrexone" "aripiprazole" "quetiapine"
    "olanzapine" "risperidone" "clozapine" "zoloft" "lexapro" "prozac" "paxil" "celexa" "effexor"
    "cymbalta" "wellbutrin" "abilify" "seroquel" "adderall" "vyvanse" "concerta" "xanax" "ativan"
    "klonopin" "valium")
  "Drugs prescribed mainly for a condition the standard level keeps private. Used
only for the user's own medications: a gene-drug record about sertraline is
drug-response information, a sertraline prescription says something about its owner.")

(defparameter +reproductive-drugs+
  '("levonorgestrel" "norethindrone" "norgestimate" "desogestrel" "drospirenone" "etonogestrel"
    "medroxyprogesterone" "ulipristal" "mifepristone" "misoprostol" "clomiphene" "sildenafil"
    "tadalafil" "vardenafil" "mirena" "kyleena" "nexplanon" "depo-provera" "nuvaring" "yaz" "yasmin"
    "viagra" "cialis" "clomid"))

(defun words-of (text)
  "TEXT's lowercased words, punctuation dropped."
  (let ((words '()) (current '()))
    (labels ((flush ()
               (when current
                 (push (coerce (nreverse current) 'string) words)
                 (setf current '()))))
      (loop for char across (string-downcase (princ-to-string (or text "")))
            do (if (or (alphanumericp char) (char= char #\-))
                   (push char current)
                   (flush)))
      (flush))
    (nreverse words)))

(defun medication-category (text)
  "The category of one of the user's medications, from its name and any
text the record gives."
  (let ((words (words-of text)))
    (cond ((intersection words +mental-health-drugs+ :test #'string=) "mental_health")
          ((intersection words +reproductive-drugs+ :test #'string=) "reproductive")
          (t "medication"))))
