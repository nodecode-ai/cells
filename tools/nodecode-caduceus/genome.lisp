;;;; genome.lisp --- the DNA half: raw data in, genotypes and what they mean out.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Three things live here, and none of them dials anything: the parse of a
;;;; consumer chip's raw data file (23andMe or AncestryDNA .txt), a small
;;;; evidence pack of common markers with the allele that carries the finding,
;;;; and the gene-drug entries a chip can actually call. The pack is this
;;;; rewrite's own: the original ships a CPIC export and a research watch,
;;;; while a handful of well-known markers is what a raw data file can answer
;;;; on its own. Every answer repeats the two limits the original repeats:
;;;; consumer chips miss rare variants, and none of this is a prescription.

(in-package #:nodecode-caduceus)

(defparameter +chip-limits+
  "Consumer DNA chips test common variants; rare variants can be missed or miscalled. A clinical test confirms anything important."
  "What every genotype answer says about the data it reads.")

(defparameter +pgx-disclaimer+
  "Information with limits, not a diagnosis or a dosing recommendation. Chip data do not replace clinical pharmacogenetic testing. Do not start, stop, switch or dose a drug from this result; talk to your prescriber."
  "What every gene-drug answer says.")

(defparameter +prevalence-note+
  "Markers are reported on the plus strand as consumer chips write them; a chip that reports the other strand shows the complement."
  "Where the allele a finding is keyed on comes from.")

;;; --- the genotypes the vault holds --------------------------------------------

(defvar *genotype-cache* nil
  "The genotypes of the last read file, as (NAMESTRING WRITE-DATE TABLE), so a
call does not re-read a six-hundred-thousand-line raw data file it just read.")

(defun load-genotype-table (path)
  "PATH's rsid-to-genotype table, keys lowercased."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row (read-rows path))
      (destructuring-bind (rsid chromosome position genotype &rest rest) row
        (declare (ignore chromosome position rest))
        (when (and (nonblank rsid) (nonblank genotype)
                   (plusp (length genotype)))
          (setf (gethash (string-downcase (nlk:trimmed rsid)) table)
                (string-upcase (nlk:trimmed genotype))))))
    table))

(defun genotypes ()
  "The vault's genotypes: rsid (lowercase) to the genotype as called."
  (let* ((path (vault-file +genotypes-file+))
         (stamp (and (probe-file path) (file-write-date path))))
    (unless (and *genotype-cache* (equal (first *genotype-cache*) (namestring path))
                 (eql (second *genotype-cache*) stamp))
      (setf *genotype-cache* (list (namestring path) stamp (load-genotype-table path))))
    (third *genotype-cache*)))

(defun genotype-of (rsid)
  "The user's genotype at RSID, or NIL when the chip did not call it."
  (let ((genotype (gethash (string-downcase (nlk:trimmed rsid)) (genotypes))))
    (and genotype (not (string= genotype "--")) (not (string= genotype "00")) genotype)))

(defun genotype-count ()
  (hash-table-count (genotypes)))

(defun call-object (rsid)
  "One marker's call, as the model reads it: its state and its genotype."
  (let* ((key (string-downcase (nlk:trimmed rsid)))
         (raw (gethash key (genotypes))))
    (nlk:json-object
     "rsid" key
     "state" (cond ((null raw) "not_on_chip")
                   ((or (string= raw "--") (string= raw "00")) "no_call")
                   ((= 1 (length raw)) "haploid")
                   (t "called"))
     "genotype" (if (and raw (not (string= raw "--"))) raw :null))))

;;; --- reading a raw data file ----------------------------------------------------

(defun parse-raw-rows (lines)
  "LINES of a 23andMe or AncestryDNA raw data file, as (RSID CHROMOSOME POSITION
GENOTYPE) lists. The format is read off the header, never guessed from the
data: 23andMe writes rsid, chromosome, position, genotype; AncestryDNA writes
rsid, chromosome, position, allele1, allele2."
  (let* ((rows (loop for line in lines
                     when (plusp (length (nlk:trimmed line)))
                       collect (uiop:split-string line :separator '(#\Tab))))
         (header (find-if (lambda (row) (and row (search "rsid" (first row) :test #'char-equal))) rows))
         (fields (mapcar (lambda (field) (string-downcase (nlk:trimmed field))) header))
         (ancestry (not (null (member "allele1" fields :test #'string=))))
         (genotype-index (position "genotype" fields :test #'string=))
         (calls '()))
    (unless header
      (fail "that file has no rsid header line, so it is neither 23andMe nor AncestryDNA raw data"))
    (dolist (row rows)
      (when (and (>= (length row) (if ancestry 5 4))
                 (not (search "#" (first row))))
        (let ((rsid (nlk:trimmed (first row)))
              (genotype (if ancestry
                            (concatenate 'string (nlk:trimmed (fourth row)) (nlk:trimmed (fifth row)))
                            (nlk:trimmed (nth (or genotype-index 3) row)))))
          (when (and (plusp (length rsid)) (plusp (length genotype)))
            (push (list rsid (nlk:trimmed (second row)) (nlk:trimmed (third row))
                        (string-upcase genotype))
                  calls)))))
    (nreverse calls)))

(defun import-raw-data (path kind)
  "Import a parsed raw data file into the vault, merging with what is there:
a marker the new file calls wins, one it does not keep the old call."
  (let* ((rows (parse-raw-rows (read-lines path)))
         (calls (when rows (length rows))))
    (unless calls
      (fail "~a holds no genotype lines this cell recognizes" (file-namestring path)))
    (let ((table (make-hash-table :test #'equal)))
      (loop for (rsid chromosome position genotype) in rows
            when (and (plusp (length rsid)) (plusp (length genotype)))
              do (setf (gethash (string-downcase rsid) table)
                       (list rsid chromosome position genotype)))
      ;; what the vault already held, where the new file is silent
      (dolist (row (read-rows (vault-file +genotypes-file+)))
        (destructuring-bind (rsid chromosome position genotype &rest rest) row
          (declare (ignore rest))
          (unless (gethash (string-downcase rsid) table)
            (setf (gethash (string-downcase rsid) table)
                  (list rsid chromosome position genotype)))))
      (write-rows (vault-file +genotypes-file+)
                  (sort (loop for value being the hash-values of table collect value)
                        #'string< :key #'first))
      (setf *genotype-cache* nil)
      (append-source path (format nil "~a raw data" kind) (hash-table-count table))
      (nlk:json-object
       "imported" (file-namestring path)
       "format" kind
       "markers" (hash-table-count table)
       "called" (genotype-count)
       "note" "the vault merges a new import with what it held: a marker the new file calls wins"))))

(defun read-lines (path)
  "PATH's lines, read one at a time. A .zip is read through the unzip the
user already has, one member at a time, never unpacked to disk."
  (let ((path (pathname path)))
    (if (string-equal (pathname-type path) "zip")
        (let ((output (uiop:run-program
                       (list "unzip" "-p" (namestring path)
                             (or (first (uiop:run-program
                                         (list "unzip" "-Z1" (namestring path))
                                         :output '(:string :stripped t)
                                         :error-output nil))
                                 ""))
                       :output '(:string :stripped t) :error-output nil :ignore-error-status t)))
          (when (or (null output) (zerop (length output)))
            (fail "~a is a zip this cell could not read; unpack it and pass the .txt or .tsv inside"
                  (file-namestring path)))
          (uiop:split-string output :separator '(#\Newline)))
        (with-open-file (in path :direction :input :external-format :utf-8
                                 :if-does-not-exist nil)
          (unless in (fail "no file at ~a" (namestring path)))
          (loop for line = (read-line in nil nil)
                while line collect line)))))

;;; --- the annotation pack ---------------------------------------------------------

(defparameter +annotations+
  '((:rsid "rs4988235" :gene "LCT" :title "Lactose persistence" :category "trait"
     :effect "T" :other "C" :magnitude 2.5 :repute "good"
     :finding "A lactase-persistence allele: lactose is usually still digested in adulthood."
     :plain "No lactase-persistence allele at this marker: lactose can be harder to digest, which is common and not a disease.")
    (:rsid "rs1801133" :gene "MTHFR" :title "MTHFR C677T" :category "wellness"
     :effect "T" :other "C" :magnitude 2.0 :repute "neutral"
     :finding "A C677T allele: the enzyme runs slower, so folate status is worth a blood test rather than a guess."
     :plain "No C677T allele at this marker.")
    (:rsid "rs1801131" :gene "MTHFR" :title "MTHFR A1298C" :category "wellness"
     :effect "C" :other "A" :magnitude 1.5 :repute "neutral"
     :finding "An A1298C allele: one more common MTHFR variant with a mild effect."
     :plain "No A1298C allele at this marker.")
    (:rsid "rs762551" :gene "CYP1A2" :title "Caffeine clearance" :category "wellness"
     :effect "C" :other "A" :magnitude 1.5 :repute "neutral"
     :finding "A slower-caffeine-clearance allele: coffee late in the day may cost more sleep."
     :plain "The fast-clearance allele at this marker.")
    (:rsid "rs671" :gene "ALDH2" :title "Alcohol flush" :category "wellness"
     :effect "A" :other "G" :magnitude 3.0 :repute "bad"
     :finding "The ALDH2*2 allele: acetaldehyde clears slowly, so alcohol brings flushing and a higher risk from regular drinking."
     :plain "No ALDH2*2 allele at this marker.")
    (:rsid "rs1229984" :gene "ADH1B" :title "Alcohol metabolism" :category "wellness"
     :effect "A" :other "G" :magnitude 1.5 :repute "neutral"
     :finding "The fast ADH1B allele: alcohol is turned into acetaldehyde quickly, which many people feel as flushing."
     :plain "The slower ADH1B allele at this marker.")
    (:rsid "rs1815739" :gene "ACTN3" :title "Sprint and power" :category "trait"
     :effect "C" :other "T" :magnitude 1.5 :repute "neutral"
     :finding "The R allele of ACTN3: fast-twitch muscle protein is made, which suits power sports. Plenty of endurance athletes have it too."
     :plain "No R allele at this marker: the classic endurance genotype, and no bar to any sport.")
    (:rsid "rs4680" :gene "COMT" :title "COMT Val158Met" :category "trait"
     :effect "A" :other "G" :magnitude 1.5 :repute "neutral"
     :finding "The Met allele: dopamine clears more slowly in the prefrontal cortex; the popular focus and stress stories about it are weak."
     :plain "The Val allele at this marker.")
    (:rsid "rs12913832" :gene "HERC2" :title "Eye colour" :category "trait"
     :effect "G" :other "A" :magnitude 1.5 :repute "neutral"
     :finding "A blue-eye allele at HERC2; eye colour still runs in families and a chip cannot settle it."
     :plain "The brown-eye allele at HERC2; other genes still shape the colour.")
    (:rsid "rs17822931" :gene "ABCC11" :title "Earwax type" :category "trait"
     :effect "T" :other "C" :magnitude 1.0 :repute "neutral"
     :finding "The dry-earwax allele at ABCC11."
     :plain "The wet-earwax allele at ABCC11, the common one.")
    (:rsid "rs1805007" :gene "MC1R" :title "Red hair" :category "trait"
     :effect "T" :other "C" :magnitude 1.0 :repute "neutral"
     :finding "A red-hair variant of MC1R; it also means paler skin and more care with the sun."
     :plain "No red-hair variant at this MC1R marker.")
    (:rsid "rs9939609" :gene "FTO" :title "Weight tendency" :category "wellness"
     :effect "A" :other "T" :magnitude 2.0 :repute "bad"
     :finding "An FTO allele: a slightly higher tendency to gain weight, worth about a kilogram on average - habits matter more."
     :plain "No FTO risk allele at this marker.")
    (:rsid "rs7903146" :gene "TCF7L2" :title "Type 2 diabetes risk" :category "wellness"
     :effect "T" :other "C" :magnitude 2.5 :repute "bad"
     :finding "A TCF7L2 allele: one of the stronger common type 2 diabetes risk markers; a blood test, not this, says where you stand."
     :plain "No TCF7L2 risk allele at this marker.")
    (:rsid "rs6025" :gene "F5" :title "Factor V Leiden" :category "carrier"
     :effect "A" :other "G" :magnitude 3.5 :repute "bad"
     :finding "The Factor V Leiden allele: a higher clotting risk, which matters for pregnancy, surgery, long flights and hormonal contraception - worth telling a clinician."
     :plain "No Factor V Leiden allele at this marker.")
    (:rsid "rs1800562" :gene "HFE" :title "HFE C282Y" :category "carrier"
     :effect "A" :other "G" :magnitude 3.0 :repute "neutral"
     :finding "The C282Y allele of HFE: iron loads faster if two copies are inherited; ferritin on a blood test shows whether it does."
     :plain "No C282Y allele at this marker.")
    (:rsid "rs334" :gene "HBB" :title "Sickle cell carrier" :category "carrier"
     :effect "T" :other "A" :magnitude 3.5 :repute "bad"
     :finding "The sickle cell variant of HBB: one copy is a carrier state, two copies are the disease. A partner's status matters for children."
     :plain "No sickle cell variant at this marker.")
    (:rsid "rs4149056" :gene "SLCO1B1" :title "SLCO1B1 statin transport" :category "pgx"
     :effect "C" :other "T" :magnitude 3.0 :repute "bad"
     :finding "A reduced-function SLCO1B1 allele: simvastatin (and at higher doses, other statins) reaches muscle more, so the myopathy risk is higher. Ask about a lower dose or another statin."
     :plain "No reduced-function SLCO1B1 allele at this marker.")
    (:rsid "rs4244285" :gene "CYP2C19" :title "CYP2C19*2" :category "pgx"
     :effect "A" :other "G" :magnitude 3.0 :repute "bad"
     :finding "The CYP2C19*2 no-function allele: clopidogrel is activated less, and some antidepressants and proton-pump inhibitors clear more slowly."
     :plain "No CYP2C19*2 allele at this marker.")
    (:rsid "rs4986893" :gene "CYP2C19" :title "CYP2C19*3" :category "pgx"
     :effect "A" :other "G" :magnitude 3.0 :repute "bad"
     :finding "The CYP2C19*3 no-function allele, which is rare outside East Asia."
     :plain "No CYP2C19*3 allele at this marker.")
    (:rsid "rs12248560" :gene "CYP2C19" :title "CYP2C19*17" :category "pgx"
     :effect "T" :other "C" :magnitude 2.0 :repute "neutral"
     :finding "The CYP2C19*17 increased-function allele: some drugs that CYP2C19 clears leave faster, and one does its work too well."
     :plain "No CYP2C19*17 allele at this marker.")
    (:rsid "rs1799853" :gene "CYP2C9" :title "CYP2C9*2" :category "pgx"
     :effect "T" :other "C" :magnitude 2.0 :repute "neutral"
     :finding "The CYP2C9*2 reduced-function allele: warfarin and some other drugs clear more slowly."
     :plain "No CYP2C9*2 allele at this marker.")
    (:rsid "rs1057910" :gene "CYP2C9" :title "CYP2C9*3" :category "pgx"
     :effect "C" :other "A" :magnitude 2.5 :repute "bad"
     :finding "The CYP2C9*3 reduced-function allele, the stronger of the two common CYP2C9 variants."
     :plain "No CYP2C9*3 allele at this marker.")
    (:rsid "rs9923231" :gene "VKORC1" :title "VKORC1 warfarin sensitivity" :category "pgx"
     :effect "T" :other "C" :magnitude 2.5 :repute "neutral"
     :finding "The warfarin-sensitive VKORC1 allele: a lower dose is often enough, which a prescriber can check with INR."
     :plain "The less warfarin-sensitive VKORC1 allele at this marker.")
    (:rsid "rs776746" :gene "CYP3A5" :title "CYP3A5 expressor" :category "pgx"
     :effect "G" :other "A" :magnitude 2.0 :repute "neutral"
     :finding "The CYP3A5*3 allele: the enzyme is not made from that copy, so tacrolimus stays higher. Two copies mean a classic non-expressor."
     :plain "An expressing CYP3A5 allele at this marker.")
    (:rsid "rs1800462" :gene "TPMT" :title "TPMT*2" :category "pgx"
     :effect "G" :other "C" :magnitude 2.0 :repute "bad"
     :finding "A reduced-function TPMT allele: thiopurines such as azathioprine are cleared more slowly."
     :plain "No TPMT*2 allele at this marker.")
    (:rsid "rs116855232" :gene "NUDT15" :title "NUDT15*3" :category "pgx"
     :effect "T" :other "C" :magnitude 2.5 :repute "bad"
     :finding "The NUDT15*3 allele: thiopurine doses usually need to be lower, most often found in East Asian and Hispanic ancestry."
     :plain "No NUDT15*3 allele at this marker.")
    (:rsid "rs2231142" :gene "ABCG2" :title "ABCG2 Q141K" :category "pgx"
     :effect "T" :other "G" :magnitude 2.0 :repute "neutral"
     :finding "A reduced-function ABCG2 allele: urate leaves more slowly, and allopurinol and some statins are handled differently."
     :plain "No reduced-function ABCG2 allele at this marker."))
  "Common markers a consumer chip calls, with the allele that carries the
finding. Not a diagnostic set: it is what a raw data file can answer alone.")

(defun annotation-entry (rsid)
  (find (string-downcase (nlk:trimmed rsid)) +annotations+
        :key (lambda (entry) (string-downcase (getf entry :rsid))) :test #'string=))

(defun annotation-call (entry)
  "One pack ENTRY and the user's call at it: a plist with the genotype, how
many effect alleles were seen, and the finding that fits."
  (let* ((genotype (genotype-of (getf entry :rsid)))
         (effect (char (getf entry :effect) 0))
         (other (char (getf entry :other) 0))
         (usable (and genotype (every (lambda (char) (or (char-equal char effect)
                                                          (char-equal char other)))
                                       genotype)))
         (count (and usable (count effect genotype :test #'char-equal))))
    (list :entry entry
          :rsid (getf entry :rsid)
          :gene (getf entry :gene)
          :title (getf entry :title)
          :category (getf entry :category)
          :magnitude (getf entry :magnitude)
          :repute (getf entry :repute)
          :genotype genotype
          :effect-alleles count
          :state (cond ((null genotype) "not_on_chip")
                       ((not usable) "unsupported_call")
                       (t "called"))
          :finding (cond ((null genotype) "Not on this chip: absent from the answer, not from the genome.")
                         ((not usable) (format nil "The call ~a carries an allele this entry does not describe." genotype))
                         ((plusp count) (getf entry :finding))
                         (t (getf entry :plain))))))

(defun annotation-calls (&key category gene topic min-magnitude repute)
  "Every pack entry the user's chip says something about, filtered."
  (let ((calls '()))
    (dolist (entry +annotations+)
      (let ((call (annotation-call entry)))
        (when (and (member (getf call :state) '("called" "unsupported_call") :test #'string=)
                   (or (null category) (string= category (getf call :category)))
                   (or (null gene) (string= (string-upcase gene) (getf call :gene)))
                   (or (null min-magnitude) (>= (getf call :magnitude) min-magnitude))
                   (or (null repute) (string= repute "any") (string= repute (getf call :repute)))
                   (or (null topic) (search (string-downcase topic)
                                             (string-downcase (format nil "~a ~a ~a"
                                                                      (getf call :title)
                                                                      (getf call :gene)
                                                                      (getf call :finding))))))
          (push call calls))))
    (sort calls #'> :key (lambda (call) (getf call :magnitude)))))

;;; --- derived calls -----------------------------------------------------------------

(defun allele-characters (rsid)
  "RSID's call as a list of allele characters, or NIL."
  (let ((genotype (genotype-of rsid)))
    (when (and genotype (<= 1 (length genotype) 2))
      (coerce (string-upcase genotype) 'list))))

(defparameter +apoe-haplotypes+
  '(("e2" #\T #\C) ("e3" #\T #\T) ("e4" #\C #\T))
  "APOE haplotypes as the two chip markers write them: rs429358, then rs7412.")

(defun same-multiset-p (left right)
  (and left right (= (length left) (length right))
       (null (set-difference left right :test #'char-equal))
       (null (set-difference right left :test #'char-equal))))

(defun apoe-result ()
  "The APOE genotype the two markers allow, with the pairs a phase cannot
separate, or NIL when a marker was not called."
  (let* ((first (allele-characters "rs429358"))
         (second (allele-characters "rs7412"))
         (pairs '()))
    (when (and first second)
      (dolist (one +apoe-haplotypes+)
        (dolist (two +apoe-haplotypes+)
          (when (and (same-multiset-p (list (second one) (second two)) first)
                     (same-multiset-p (list (third one) (third two)) second))
            (let ((name (format nil "~a/~a" (first one) (first two))))
              (unless (find name pairs :test (lambda (a b)
                                               (string= a (format nil "~a/~a"
                                                                  (second (uiop:split-string b :separator "/"))
                                                                  (first (uiop:split-string b :separator "/"))))))
                (push name pairs))))))
      (nreverse pairs))))

(defun apoe-report ()
  (let ((pairs (apoe-result)))
    (nlk:json-object
     "gene" "APOE"
     "markers" (nlk:json-object "rs429358" (call-object "rs429358")
                                 "rs7412" (call-object "rs7412"))
     "genotypes" (coerce (or pairs '()) 'vector)
     "interpretation"
     (cond ((null pairs) "Both APOE markers are needed; one was not called on this chip.")
           ((find "e4/e4" pairs :test #'string=)
            "Two e4 alleles: the strongest common APOE risk for late-onset Alzheimer's, and still only a risk - many e4/e4 people never develop it.")
           ((some (lambda (pair) (search "e4" pair)) pairs)
            "One e4 allele (or a pair a chip cannot phase): a modestly higher late-onset Alzheimer's risk. The categories this vault keeps private unless you unlock them.")
           ((find "e2/e2" pairs :test #'string=)
            "Two e2 alleles: the least common APOE pair, associated with lower Alzheimer's risk and, in some studies, higher triglyceride levels.")
           (t "No e4 allele: the commonest APOE result - no e4 risk, and no guarantee about any disease."))
     "note" "APOE was derived from rs429358 and rs7412; a double heterozygote leaves e2/e4 and e3/e3 indistinguishable without phasing."
     "disclaimer" +chip-limits+)))

(defun enzyme-counts (markers)
  "For each (RSID EFFECT LABEL) in MARKERS, how many effect alleles the chip
called, and the markers it did not."
  (let ((counts '()) (missing '()))
    (dolist (marker markers)
      (destructuring-bind (rsid effect label) marker
        (let ((genotype (genotype-of rsid)))
          (cond ((null genotype) (push (format nil "~a not on the chip" label) missing))
                ((not (every (lambda (char) (or (char-equal char (char effect 0))
                                                (not (char-equal char (char effect 0)))))
                             genotype))
                 (push (format nil "~a: unsupported call" label) missing))
                (t (push (cons label (count (char effect 0) genotype :test #'char-equal)) counts))))))
    (values (nreverse counts) (nreverse missing))))

(defun cyp2c19-result ()
  "CYP2C19 *2, *3 and *17 as a phenotype guess, with what it cannot see."
  (let* ((no-function (+ (or (cdr (assoc "*2" (enzyme-counts '(("rs4244285" "A" "*2"))) :test #'string=)) 0)
                          (or (cdr (assoc "*3" (enzyme-counts '(("rs4986893" "A" "*3"))) :test #'string=)) 0)))
         (increased (or (cdr (assoc "*17" (enzyme-counts '(("rs12248560" "T" "*17"))) :test #'string=)) 0))
         (called (and (genotype-of "rs4244285") (genotype-of "rs4986893"))))
    (when called
      (list :gene "CYP2C19"
            :no-function no-function
            :increased increased
            :phenotype (cond ((>= no-function 2) "likely poor metabolizer")
                             ((= no-function 1) "likely intermediate metabolizer")
                             ((= increased 2) "likely rapid metabolizer")
                             ((= increased 1) "likely normal to rapid metabolizer")
                             (t "likely normal metabolizer"))
            :note (if (zerop increased)
                      "Derived from *2 and *3 only; a chip does not type every no-function allele, so a poor metabolizer can look normal."
                      "Derived from *2, *3 and *17 only; rare alleles are invisible to a chip.")))))

(defun slco1b1-result ()
  (let* ((count (or (cdr (assoc "*5" (enzyme-counts '(("rs4149056" "C" "*5"))) :test #'string=)) 0))
         (genotype (genotype-of "rs4149056")))
    (when genotype
      (list :gene "SLCO1B1"
            :decreased count
            :function (cond ((>= count 2) "poor function")
                            ((= count 1) "decreased function")
                            (t "normal function"))
            :note "Simvastatin exposure is the best-established consequence; statin choice is a conversation, not a verdict."))))

(defun cyp2c9-result ()
  (let* ((star2 (or (cdr (assoc "*2" (enzyme-counts '(("rs1799853" "T" "*2"))) :test #'string=)) 0))
         (star3 (or (cdr (assoc "*3" (enzyme-counts '(("rs1057910" "C" "*3"))) :test #'string=)) 0))
         (reduced (+ star2 star3))
         (genotype (genotype-of "rs1799853")))
    (when genotype
      (list :gene "CYP2C9"
            :reduced reduced
            :phenotype (cond ((>= reduced 2) "poor metabolizer (from *2 and *3)")
                             ((= reduced 1) "intermediate metabolizer (from *2 and *3)")
                             (t "normal metabolizer (from *2 and *3)"))
            :note "Warfarin and phenytoin doses are the usual concern; other rare alleles are not typed."))))

(defun cyp3a5-result ()
  (let ((genotype (genotype-of "rs776746")))
    (when genotype
      (let ((expressors (count #\A genotype :test #'char-equal)))
        (list :gene "CYP3A5"
              :expressing-alleles expressors
              :phenotype (if (plusp expressors) "expressor" "non-expressor")
              :note "The common *3 allele is what this marker calls; tacrolimus dosing is where it is used.")))))

(defun derived-calls ()
  "The calls a chip can combine: APOE and four enzymes."
  (remove nil (list (apoe-report) (cyp2c19-result) (slco1b1-result)
                    (cyp2c9-result) (cyp3a5-result))))

;;; --- gene-drug evidence ------------------------------------------------------------

(defparameter +pgx-entries+
  '((:id "cyp2c19-clopidogrel" :gene "CYP2C19" :drugs ("clopidogrel" "plavix")
     :allele-name "CYP2C19 *2, *3 and *17"
     :markers (("rs4244285" "A" "*2" "no function") ("rs4986893" "A" "*3" "no function")
               ("rs12248560" "T" "*17" "increased function"))
     :function "Clopidogrel is a prodrug that CYP2C19 activates."
     :finding "A no-function CYP2C19 allele: CPIC suggests prasugrel or ticagrelor instead of clopidogrel after a stent, when neither is contraindicated."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for CYP2C19 and clopidogrel" "https://cpicpgx.org/guidelines/"))
     :limits "A chip types the common alleles only; a clinical test is the confirmation.")
    (:id "cyp2d6-codeine" :gene "CYP2D6" :drugs ("codeine" "tramadol")
     :allele-name "CYP2D6 *4 and *10"
     :markers (("rs3892097" "A" "*4" "no function") ("rs1065852" "T" "*10" "decreased function"))
     :function "Codeine is a prodrug that CYP2D6 turns into morphine."
     :finding "A reduced-function CYP2D6 allele: codeine may give less pain relief, and a poor metabolizer gets little from it. Ultrarapid metabolizers are the other risk, and a chip misses the duplications that cause it."
     :evidence "CPIC A" :chip "limited"
     :sources (("CPIC guideline for CYP2D6, codeine and tramadol" "https://cpicpgx.org/guidelines/"))
     :limits "Copy-number variants, which decide ultrarapid status, are invisible to a consumer chip.")
    (:id "cyp2c9-warfarin" :gene "CYP2C9" :drugs ("warfarin" "coumadin" "jantoven")
     :allele-name "CYP2C9 *2 and *3"
     :markers (("rs1799853" "T" "*2" "decreased function") ("rs1057910" "C" "*3" "decreased function"))
     :function "Warfarin is cleared by CYP2C9."
     :finding "A reduced-function CYP2C9 allele: warfarin clears more slowly, so the starting dose is usually lower and INR checks matter more."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for warfarin" "https://cpicpgx.org/guidelines/"))
     :limits "Warfarin dosing also depends on VKORC1, age, weight and diet; INR is the authority.")
    (:id "vkorc1-warfarin" :gene "VKORC1" :drugs ("warfarin" "coumadin" "jantoven")
     :allele-name "VKORC1 -1639G>A"
     :markers (("rs9923231" "T" "-1639A" "increased sensitivity"))
     :function "VKORC1 is the enzyme warfarin inhibits."
     :finding "The warfarin-sensitive VKORC1 allele: a lower starting dose is usually needed."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for warfarin" "https://cpicpgx.org/guidelines/"))
     :limits "One marker of several; INR remains the authority.")
    (:id "slco1b1-simvastatin" :gene "SLCO1B1" :drugs ("simvastatin" "zocor")
     :allele-name "SLCO1B1 *5"
     :markers (("rs4149056" "C" "*5" "decreased function"))
     :function "SLCO1B1 carries statins into the liver; a slower transporter leaves more in muscle."
     :finding "A reduced-function SLCO1B1 allele: the risk of simvastatin muscle pain is higher, and CPIC suggests a lower dose or a statin that does not depend on this transporter."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for SLCO1B1 and statins" "https://cpicpgx.org/guidelines/"))
     :limits "Muscle symptoms are the thing to report; creatine kinase is the test.")
    (:id "cyp3a5-tacrolimus" :gene "CYP3A5" :drugs ("tacrolimus" "astagraf" "envarsus")
     :allele-name "CYP3A5 *3"
     :markers (("rs776746" "G" "*3" "no function"))
     :function "CYP3A5 clears tacrolimus; an expressor needs more of it."
     :finding "The CYP3A5*3 allele: each copy makes less enzyme, so tacrolimus stays higher. Two copies mean the standard non-expressor dose."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for CYP3A5 and tacrolimus" "https://cpicpgx.org/guidelines/"))
     :limits "Transplant dosing is led by trough levels, not by this result.")
    (:id "tpmt-azathioprine" :gene "TPMT" :drugs ("azathioprine" "mercaptopurine" "imuran" "azasan")
     :allele-name "TPMT *2, *3B and *3C"
     :markers (("rs1800462" "G" "*2" "decreased function") ("rs1800460" "A" "*3B" "decreased function")
               ("rs1142345" "C" "*3C" "decreased function"))
     :function "TPMT clears thiopurine drugs."
     :finding "A reduced-function TPMT allele: azathioprine and mercaptopurine are cleared more slowly, so the dose starts lower and blood counts are checked."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for thiopurines" "https://cpicpgx.org/guidelines/"))
     :limits "Three common alleles are typed here; a clinical panel covers the rest.")
    (:id "nudt15-azathioprine" :gene "NUDT15" :drugs ("azathioprine" "mercaptopurine" "imuran")
     :allele-name "NUDT15 *3"
     :markers (("rs116855232" "T" "*3" "decreased function"))
     :function "NUDT15 protects cells from thiopurine metabolites."
     :finding "The NUDT15*3 allele: thiopurine doses are usually much lower, and the allele is common in East Asian and Hispanic ancestry."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for thiopurines" "https://cpicpgx.org/guidelines/"))
     :limits "One marker; the rest of NUDT15 is not typed here.")
    (:id "dpyd-fluorouracil" :gene "DPYD" :drugs ("fluorouracil" "capecitabine" "5-fu")
     :allele-name "DPYD *2A, *13 and c.2846A>T"
     :markers (("rs3918290" "T" "*2A" "no function") ("rs55886062" "C" "*13" "no function")
               ("rs67376798" "T" "c.2846A>T" "decreased function"))
     :function "DPYD clears fluorouracil, the chemotherapy drug."
     :finding "A reduced-function DPYD allele: fluorouracil and capecitabine can be severely toxic, and CPIC suggests a reduced dose."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for DPYD and fluoropyrimidines" "https://cpicpgx.org/guidelines/"))
     :limits "Chemotherapy dosing is set by an oncology team, usually with a clinical DPYD test.")
    (:id "cyp2c19-citalopram" :gene "CYP2C19" :drugs ("citalopram" "escitalopram" "celexa" "lexapro")
     :allele-name "CYP2C19 *2, *3 and *17"
     :markers (("rs4244285" "A" "*2" "no function") ("rs4986893" "A" "*3" "no function")
               ("rs12248560" "T" "*17" "increased function"))
     :function "CYP2C19 clears citalopram and escitalopram."
     :finding "A CYP2C19 no-function allele: the drug stays higher, so CPIC suggests a lower starting dose, especially for escitalopram. A *17/*17 genotype can mean a poorer response."
     :evidence "CPIC A" :chip "reliable"
     :sources (("CPIC guideline for CYP2C19 and SSRIs" "https://cpicpgx.org/guidelines/"))
     :limits "Response to an antidepressant is more than its level; a prescriber decides.")
    (:id "hla-b-abacavir" :gene "HLA-B" :drugs ("abacavir" "ziagen")
     :allele-name "HLA-B*57:01"
     :markers (("rs2395029" "G" "B*57:01" "risk allele"))
     :function "HLA-B*57:01 is the marker of abacavir hypersensitivity."
     :finding "The HLA-B*57:01 tag allele: abacavir can cause a severe hypersensitivity reaction, and the drug is avoided when the allele is present."
     :evidence "CPIC A" :chip "limited"
     :sources (("CPIC guideline for abacavir and HLA-B" "https://cpicpgx.org/guidelines/"))
     :limits "The tag marker is not the allele itself; clinical typing confirms it."))
  "The gene-drug entries a consumer chip can call, with the allele that carries
the effect. Evidence is CPIC's; every one is a conversation with a prescriber,
not a dose.")

(defparameter +drug-aliases+
  '(("zocor" . "simvastatin") ("lipitor" . "atorvastatin") ("crestor" . "rosuvastatin")
    ("plavix" . "clopidogrel") ("coumadin" . "warfarin") ("jantoven" . "warfarin")
    ("imuran" . "azathioprine") ("azasan" . "azathioprine") ("celexa" . "citalopram")
    ("lexapro" . "escitalopram") ("prozac" . "fluoxetine") ("zoloft" . "sertraline")
    ("ziagen" . "abacavir") ("astagraf" . "tacrolimus") ("envarsus" . "tacrolimus")
    ("5-fu" . "fluorouracil") ("xeloda" . "capecitabine") ("ozempic" . "semaglutide")
    ("eliquis" . "apixaban") ("xarelto" . "rivaroxaban"))
  "Brand names, to the generic a gene-drug entry is filed under.")

(defun normalize-drug (name)
  "NAME as the generic a gene-drug entry is filed under, or NIL."
  (let* ((text (string-downcase (princ-to-string (or name ""))))
         (words (remove-if (lambda (word) (or (every #'digit-char-p word)
                                               (member word '("mg" "mcg" "ml" "g" "units" "unit"
                                                              "weekly" "daily" "twice" "once"
                                                              "tablet" "tablets" "capsule" "capsules")
                                                       :test #'string=)))
                            (words-of text)))
         (candidates (append words
                             (loop for (first second) on words while second
                                   collect (format nil "~a ~a" first second)))))
    (or (loop for candidate in candidates
              when (assoc candidate +drug-aliases+ :test #'string=) return (cdr (assoc candidate +drug-aliases+ :test #'string=)))
        (loop for candidate in candidates
              when (find candidate +pgx-entries+
                         :key (lambda (entry) (getf entry :drugs)) :test #'member)
                return candidate))))

(defun pgx-check (drug)
  "Every gene-drug entry for DRUG, each marker called, absent or not on the chip."
  (let ((generic (normalize-drug drug)))
    (unless generic
      (fail "~s is not a drug this pack knows; it covers ~{~a~^, ~}"
            drug (sort (remove-duplicates (loop for entry in +pgx-entries+
                                                when (getf entry :drugs) append (getf entry :drugs))
                                           :test #'string=)
                        #'string<)))
    (let ((entries '()))
      (dolist (entry +pgx-entries+)
        (when (member generic (getf entry :drugs) :test #'string=)
          (let ((calls '()) (reasons '()) (counts '()) (status nil))
            (dolist (marker (getf entry :markers))
              (destructuring-bind (rsid effect label function) marker
                (let ((genotype (genotype-of rsid)))
                  (push (nlk:json-object "rsid" rsid "allele" label "function" function
                                          "state" (if genotype "called" "not_on_chip")
                                          "genotype" (or genotype :null))
                        calls)
                  (cond ((null genotype)
                         (push (format nil "~a not on the chip" rsid) reasons))
                        ((not (every (lambda (char) (find char "ACGT" :test #'char-equal))
                                     genotype))
                         (push (format nil "~a: unsupported call" rsid) reasons))
                        (t (let ((count (count (char effect 0) genotype :test #'char-equal)))
                             (push (cons rsid count) counts)
                             (when (plusp count) (setf status "flag"))))))))
            (setf counts (nreverse counts))
            (push (nlk:json-object
                   "id" (getf entry :id)
                   "gene" (getf entry :gene)
                   "drug" generic
                   "allele" (getf entry :allele-name)
                   "function" (getf entry :function)
                   "evidence" (getf entry :evidence)
                   "chip_reliability" (getf entry :chip)
                   "status" (cond ((null status) (if (or reasons "no_flag") "no_flag" "not_assessed"))
                                  (t status))
                   "effect_alleles" (apply #'nlk:make-json-object
                                             (loop for (rsid . count) in counts
                                                   append (list rsid count)))
                   "markers" (coerce (nreverse calls) 'vector)
                   "reason" (if reasons (format nil "~{~a~^; ~}" (nreverse reasons)) :null)
                   "finding" (if reasons
                                  (format nil "Not assessed in full: ~{~a~^; ~}. ~a" (nreverse reasons) +chip-limits+)
                                  (getf entry :finding))
                   "limits" (getf entry :limits)
                   "sources" (coerce (loop for (title url) in (getf entry :sources)
                                            collect (nlk:json-object "title" title "url" url))
                                     'vector)
                   "disclaimer" +pgx-disclaimer+)
                  entries))))
      (nlk:json-object
       "drug" generic
       "asked_as" drug
       "entries" (coerce (nreverse entries) 'vector)
       "disclaimer" +pgx-disclaimer+))))

;;; --- the report ------------------------------------------------------------------

(defun html-escape (text)
  (with-output-to-string (out)
    (loop for char across (princ-to-string (or text ""))
          do (case char
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (t (write-char char out))))))

(defun write-variant-report ()
  "A local HTML file of what the chip says, under the vault, and its path."
  (let* ((path (merge-pathnames "variant-report.html" (data-dir)))
         (calls (annotation-calls)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (format out "<!doctype html><meta charset=\"utf-8\"><title>Caduceus variant report</title>~%")
      (format out "<h1>Variant report</h1>~%<p>~a</p>~%<p>~a</p>~%"
              (html-escape +chip-limits+) (html-escape (iso-now)))
      (format out "<table border=\"1\" cellpadding=\"4\"><tr><th>Marker</th><th>Gene</th><th>Call</th><th>Finding</th></tr>~%")
      (dolist (call calls)
        (format out "<tr><td>~a</td><td>~a</td><td>~a</td><td>~a</td></tr>~%"
                (html-escape (getf call :rsid)) (html-escape (getf call :gene))
                (html-escape (or (getf call :genotype) "not on chip"))
                (html-escape (getf call :finding))))
      (format out "</table>~%"))
    (list :path (namestring path) :entries (length calls))))
