;;;; corpus.lisp --- the bundled statute pack, read once, and the names for each norm.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The pack that rides with this cell is a compact rendering of the original
;;;; plugin's chile-base corpus: 25 Chilean statutes, one row per article with
;;;; its text hash and its in-force window. Nothing here is fetched: the two
;;;; tables under data/ are the whole corpus, and PACK-HASH names the snapshot
;;;; they came from.

(in-package #:nodecode-dike)

(defparameter +pack-hash+ "af1422d66d1e6eee"
  "The snapshot these tables render: the first 16 hex digits of the SHA-256 of the
pack's unpacked tree, the identifier every answer is committed to.")

(defparameter +data-dir+
  ;; The source flag that ASDF keeps, never the fasl's own cache directory:
  ;; the tables sit beside the sources, wherever they were loaded from.
  (or (ignore-errors (asdf:system-relative-pathname "nodecode-dike" "data/"))
      (merge-pathnames "data/"
                       (make-pathname :name nil :type nil :version nil
                                      :defaults (or *load-truename* *default-pathname-defaults*))))
  "Where data/norms.tsv and data/articles.tsv sit, beside this cell's sources.")

(defstruct (norm (:constructor make-norm (id slug type number year title url aliases)))
  "One statute of the pack."
  id slug type number year title url aliases
  (articles (make-hash-table :test #'equal) :read-only t))

(defstruct (article (:constructor make-article (token sha from to status retrieved eid)))
  "One article: its comparable token, the hash of its text, and its window."
  token sha from to status retrieved eid)

(defparameter *norms* nil "Every statute of the pack, in the pack's order.")
(defparameter *by-slug* nil "Slug => NORM.")
(defparameter *mentions* nil "A folded name => the slugs it can mean.")
(defparameter *mention-keys* nil "The names of *MENTIONS*, longest first.")

;;; --- folding ----------------------------------------------------------------------

(defparameter +fold-table+
  (let ((table (make-array 256 :initial-element #\Nul)))
    (loop for c across "áàäâãéèëêíìïîóòöôõúùüûñçÁÀÄÂÃÉÈËÊÍÌÏÎÓÒÖÔÕÚÙÜÛÑÇ"
          for f across "aaaaaeeeeiiiiooooouuuuncAAAAAEEEEIIIIOOOOOUUUUNC"
          do (setf (aref table (char-code c)) f))
    table)
  "The accents that fold to a plain letter, one character for one.")

(defun fold (text)
  "TEXT lowercased and stripped of accents, the SAME length: an index into the
answer is an index into TEXT, so a name found in the fold is cut from the
original."
  (let ((out (copy-seq text)))
    (dotimes (i (length out) out)
      (let* ((c (char out i)) (code (char-code c))
             (mapped (and (< code 256) (aref +fold-table+ code))))
        (setf (char out i) (if (and mapped (not (char= mapped #\Nul)))
                                mapped
                                (char-downcase c)))))))

;;; --- loading ----------------------------------------------------------------------

(defun split-tabs (line)
  "LINE cut at its tabs."
  (loop with start = 0
        for at = (position #\Tab line :start start)
        collect (subseq line start at)
        while at do (setf start (1+ at))))

(defun designation (type number)
  "How prose names the statute whose pack number is NUMBER: ley 19880, decreto
ley 3500."
  (cond ((string= type "ley") (format nil "ley ~a" number))
        ((string= type "decreto-ley") (format nil "decreto ley ~a" (string-trim "dl-" number)))
        ((string= type "constitucion") "constitucion")
        (t (substitute #\Space #\- number))))

(defparameter +short-names+
  '(("cc" . "codigo-civil") ("ccom" . "codigo-comercio") ("cp" . "codigo-penal")
    ("cpc" . "cpc") ("cpp" . "codigo-procesal-penal") ("ct" . "codigo-del-trabajo")
    ("cot" . "codigo-organico-tribunales") ("cpr" . "constitucion")
    ("codigo de aguas" . "codigo-aguas") ("codigo tributario" . "codigo-tributario")
    ("constitucion politica" . "constitucion")
    ("constitucion politica de la republica" . "constitucion"))
  "Names the pack's own titles do not spell out.")

(defun article-key (eid)
  "The comparable token of an article id: art_0006_a => 6-a, art_t_0005 => t-5,
art_0152_quater_n => 152-quater-n. The same shape the draft's numbers are
normalised to, so the two compare."
  (let* ((raw (if (and (> (length eid) 4) (string= "art_" eid :end2 4)) (subseq eid 4) eid))
         (parts (loop with start = 0
                      for at = (position #\_ raw :start start)
                      collect (subseq raw start at)
                      while at do (setf start (1+ at)))))
    (cond ((and (string= (first parts) "t") (second parts))
           (format nil "t-~d" (parse-integer (second parts))))
          (t (format nil "~{~a~^-~}"
                     (cons (format nil "~d" (parse-integer (first parts)))
                           (mapcar (lambda (s) (fold (string-downcase s))) (rest parts))))))))

(defun load-corpus ()
  "Read the two tables once; answer the list of statutes."
  (or *norms*
      (let ((norms '()) (by-slug (make-hash-table :test #'equal))
            (by-id (make-hash-table :test #'equal)))
        (with-open-file (in (merge-pathnames "norms.tsv" +data-dir+) :external-format :utf-8)
          (loop for line = (read-line in nil nil)
                while line
                do (let* ((row (split-tabs line))
                          (id (first row)) (slug (second row)) (type (third row))
                          (number (fourth row))
                          (norm (make-norm id slug type number (fifth row) (sixth row) (seventh row)
                                           (remove-duplicates
                                            (list* (designation type number) slug
                                                   (remove nil (cdr (assoc slug +short-names+
                                                                           :test #'equal))))
                                            :test #'equal))))
                     (push norm norms)
                     (setf (gethash slug by-slug) norm
                           (gethash id by-id) norm))))
        (with-open-file (in (merge-pathnames "articles.tsv" +data-dir+) :external-format :utf-8)
          (loop for line = (read-line in nil nil)
                while line
                do (let* ((row (split-tabs line)) (norm (gethash (first row) by-id)))
                     (when norm
                       (let ((token (article-key (second row))))
                         (setf (gethash token (norm-articles norm))
                               (make-article token (third row) (fourth row) (fifth row)
                                             (sixth row) (seventh row) (second row))))))))
        (setf *norms* (nreverse norms) *by-slug* by-slug)
        (let ((mentions (make-hash-table :test #'equal)))
          (dolist (norm *norms*)
            (dolist (alias (norm-aliases norm))
              (let ((key (string-trim " " (fold alias))))
                (when (plusp (length key))
                  (setf (gethash key mentions)
                        (adjoin (norm-slug norm) (gethash key mentions) :test #'equal))))))
          (setf *mentions* mentions
                *mention-keys* (sort (loop for key being the hash-keys of mentions collect key)
                                      #'> :key #'length)))
        *norms*)))

(defun norm-by-slug (slug)
  "The statute SLUG names, or NIL."
  (load-corpus)
  (gethash slug *by-slug*))

(defun corpus ()
  "The pack, in lines: the hash it answers under, then one statute a line. Reads
no network and needs no key."
  (load-corpus)
  (with-output-to-string (out)
    (format out "dike: pack ~a, ~d statutes, ~d articles"
            +pack-hash+ (length *norms*)
            (loop for norm in *norms* sum (hash-table-count (norm-articles norm))))
    (dolist (norm *norms*)
      (format out "~%~a (~a ~a, ~d articles) - ~a"
              (norm-slug norm) (norm-type norm) (norm-number norm)
              (hash-table-count (norm-articles norm))
              (subseq (norm-title norm) 0 (min 90 (length (norm-title norm))))))))
