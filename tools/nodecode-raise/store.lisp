;;;; store.lisp --- one JSON file under the home: the pipeline, its ids, its lookups.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The whole desk is one document, raise/pipeline.json under the home: the
;;;; profile, its claims, the investors with their evidence and their events. A
;;;; call reads it, works on the plist, and writes it back once, so nothing is
;;;; half-saved. Keys are snake_case keywords, what NLK:JSON-PLIST and
;;;; NLK:PLIST-JSON round-trip.

(in-package #:nodecode-raise)

;;; --- the file -----------------------------------------------------------------

(defparameter *pipeline* '()
  "The pipeline this call runs on: a keyword plist read off the store file.")

(defun store-file ()
  "Where the pipeline lives: raise/pipeline.json under the home."
  (nlk:home "raise/pipeline.json"))

(defun pipeline-read ()
  "The pipeline on disk, or the empty one."
  (let ((path (store-file)))
    (or (and (probe-file path)
             (ignore-errors (delist (nlk:json-plist (nlk:decode-json (nlk:read-text path))))))
        '())))

(defun jsonify (value)
  "VALUE with every list an array and every plist an object, so the encoder
writes a pipeline of records rather than one record a level."
  (cond ((and (consp value) (keywordp (car value)))
         ;; a NIL member is left out: the encoder writes NIL as [], which
         ;; reads back as an empty array, never as the nothing it was
         (loop for (key inner) on value by #'cddr
               when inner append (list key (jsonify inner))))
        ((listp value) (coerce (mapcar #'jsonify value) 'vector))
        (t value)))

(defun delist (value)
  "VALUE with every JSON array a list, the inverse of JSONIFY over a decoded
document."
  (cond ((and (consp value) (keywordp (car value)))
         (loop for (key inner) on value by #'cddr
               append (list key (delist inner))))
        ((and (vectorp value) (not (stringp value)))
         (map 'list #'delist value))
        ((hash-table-p value)
         (loop for key being the hash-keys of value using (hash-value inner)
               append (list (intern (string-upcase (substitute #\_ #\- key)) :keyword)
                            (delist inner))))
        (t value)))

(defun pipeline-write (state)
  "STATE as the store file. => STATE."
  (nlk:write-file-atomically (store-file)
                              (nlk:encode-json-object (nlk:plist-json (jsonify state)))
                              :mode :private)
  state)

(defmacro with-pipeline ((&key (save t)) &body body)
  "BODY over the pipeline as it stands on disk, written back when BODY answers
unless SAVE is NIL: a read saves nothing and makes no file."
  `(let ((*pipeline* (pipeline-read)))
     (multiple-value-prog1 (progn ,@body)
       (when ,save (pipeline-write *pipeline*)))))

;;; --- ids ----------------------------------------------------------------------

(defun take-id ()
  "The next free id: one counter numbers investors, evidence, claims and events."
  (let ((next (1+ (or (getf *pipeline* :seq) 0))))
    (setf (getf *pipeline* :seq) next)
    next))

;;; --- words --------------------------------------------------------------------

(defun name-key (text)
  "TEXT as a name is compared: lowercase letters and digits, every run of
anything else one space."
  (nlk:trimmed (ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase (or text "")) " ")))

(defun firmish-p (name)
  "Whether NAME reads as a firm's, not a person's."
  (and name (plusp (length name))
       (ppcre:scan "(?i)\\b(capital|ventures?|partners|funds?|vc|holdings|investments?|labs|llc|ltd|inc|gmbh)\\b"
                    name)))

(defun several-p (name)
  "Whether NAME reads as several people in one record."
  (and name (plusp (length name))
       (ppcre:scan "(?i)[;(&/]|\\band\\b" name)))

(defun one-line (text)
  "TEXT with every blank run one space, trimmed."
  (nlk:trimmed (ppcre:regex-replace-all "[ \\t\\r\\n]+" (or text "") " ")))

(defun clip (text limit)
  (let ((text (or text "")))
    (if (> (length text) limit) (format nil "~a..." (subseq text 0 (- limit 3))) text)))

(defun money-text (amount)
  "AMOUNT as a founder says it: 1.5M, 410k, 250."
  (let ((amount (coerce (or amount 0) 'double-float)))
    (cond ((>= (abs amount) 1000000)
           (format nil "~,1fM" (/ (/ amount 100000.0) 10.0)))
          ((>= (abs amount) 1000)
           (format nil "~,0fk" (/ amount 1000.0)))
          (t (format nil "~,0f" amount)))))

;;; --- the investors ---------------------------------------------------------------

(defun investors () (getf *pipeline* :investors))

(defun (setf investors) (value) (setf (getf *pipeline* :investors) value))

(defun investor-label (record)
  "RECORD as a person reads it: the person at the firm, or the firm."
  (let ((name (let ((n (getf record :name))) (and n (plusp (length n)) n)))
        (firm (let ((f (getf record :firm))) (and f (plusp (length f)) f))))
    (cond ((and name firm (not (string= (name-key name) (name-key firm))))
           (format nil "~a at ~a" name firm))
          ((or name firm) (or name firm))
          (t "an unnamed investor"))))

(defun set-investor (record)
  "RECORD in the pipeline in the place of the one with its id. => RECORD."
  (let ((id (getf record :id)))
    (setf (investors)
          (mapcar (lambda (old) (if (eql (getf old :id) id) record old)) (investors))))
  record)

(defun investor-by-id (id)
  (find id (investors) :key (lambda (record) (getf record :id)) :test #'eql))

(defun resolve-investor (ref &key (required t))
  "The investor REF names: an id, an exact label, name or firm, else the one
record whose label holds the words. A refusal names the ref when REQUIRED."
  (let* ((text (nlk:trimmed (princ-to-string (or ref ""))))
         (found
           (and (plusp (length text))
                (or (multiple-value-bind (id end) (parse-integer text :junk-allowed t)
                      (and id (= end (length text)) (investor-by-id id)))
                    (let ((key (name-key text)))
                      (and (plusp (length key))
                           (or (find key (investors) :test #'string=
                                     :key (lambda (r) (name-key (getf r :label))))
                               (find key (investors) :test #'string=
                                     :key (lambda (r) (name-key (getf r :name))))
                               (find key (investors) :test #'string=
                                     :key (lambda (r) (name-key (getf r :firm))))
                               (find-if (lambda (r) (search key (name-key (getf r :label))))
                                        (investors)))))))))
    (cond (found found)
          ((not required) nil)
          (t (fail "no investor matches ~s: raise_focus lists the pipeline, raise_import adds one"
                   text)))))

(defun new-investor (&key name firm firm-kind)
  "A pipeline record for NAME at FIRM, at the start of the pipeline."
  (let* ((id (take-id))
         (record (list :id id :name (or name "") :firm (or firm "")
                       :firm_kind (or firm-kind "vc") :stage "new"
                       :email "" :email_confirmed nil :linkedin "" :connected nil
                       :firm_email "" :firm_form "" :do_not_contact nil :excluded nil
                       :archived nil :score nil :dims nil :ranked_at nil
                       :evidence '() :events '() :warm_paths '()
                       :created_at (nlk:iso-now))))
    (setf (getf record :label) (investor-label record))
    (setf (investors) (append (investors) (list record)))
    record))

(defun evidence-of (record) (getf record :evidence))
(defun events-of (record) (getf record :events))

(defun add-evidence (record &key kind claim source-url date amount stage)
  "One cited fact about RECORD. => its plist."
  (let ((item (list :id (take-id) :kind kind :claim claim :source_url source-url
                     :date date :amount amount :stage stage :at (nlk:iso-now))))
    (setf (getf record :evidence) (append (evidence-of record) (list item)))
    (set-investor record)
    item))

;;; --- claims and the profile -------------------------------------------------------

(defun profile () (getf *pipeline* :profile))

(defun set-profile (plist)
  "The founder's profile: the old one with PLIST's members merged in."
  (let ((merged (copy-list (profile))))
    (loop for (key value) on plist by #'cddr do (setf (getf merged key) value))
    (setf (getf *pipeline* :profile) merged)
    merged))

(defun claims () (getf *pipeline* :claims))

(defun add-claim (text tag &key value source)
  "Record one claim the founder made, TAGGED verified, unverified or projection."
  (let ((claim (list :id (take-id) :text text :tag tag :value value :source source
                     :at (nlk:iso-now))))
    (setf (getf *pipeline* :claims) (append (claims) (list claim)))
    claim))

(defun verified-claims ()
  (remove-if-not (lambda (claim) (equal "verified" (getf claim :tag))) (claims)))

;;; --- the log --------------------------------------------------------------------

(defparameter +stage-moves+ '(('pass . "passed") ("soft_pass" . "passed")
                              ("in_talks" . "in_talks") ("committed" . "committed"))
  "What a logged kind does to the stage it does not do on its own.")

(defun add-event (record &key kind channel direction summary ref)
  "One thing that happened with RECORD, on its own log. => its plist."
  (let ((event (list :seq (take-id) :at (nlk:iso-now) :kind kind :channel channel
                     :direction direction :summary summary :ref ref)))
    (setf (getf record :events) (append (events-of record) (list event)))
    (let ((stage (cdr (assoc kind +stage-moves+ :test #'equal))))
      (when stage (setf (getf record :stage) stage)))
    (set-investor record)
    event))

(defun stage-of (record) (let ((stage (getf record :stage))) (if (and stage (plusp (length stage))) stage "new")))

(defparameter +open-stages+ '("new" "researching" "ready" "contacted" "in_talks"))
(defparameter +individual-kinds+ '("angel" "angel_group"))

(defun individual-p (record) (member (getf record :firm_kind) +individual-kinds+ :test #'equal))

;;; --- time -----------------------------------------------------------------------

(defun now () (nlk:unix-now))

(defun parse-stamp (iso)
  "ISO as unix seconds, or NIL."
  (let ((universal (and iso (nlk:iso-universal iso))))
    (and universal (- universal nlk:+unix-epoch+))))

(defun stamp (unix)
  "UNIX seconds as a UTC ISO stamp, to the second."
  (nlk:iso-time unix :millis nil))

(defun weekday-of (unix)
  "UNIX's weekday, 0 Monday to 6 Sunday."
  (nth-value 6 (decode-universal-time unix 0)))

(defun workday-p (unix) (< (weekday-of unix) 5))

(defun workdays-back (unix count)
  "UNIX moved back COUNT working days."
  (loop while (plusp count)
        do (decf unix 86400)
           (when (workday-p unix) (decf count))
        finally (return unix)))

(defun add-workdays (unix count)
  "UNIX moved on COUNT working days."
  (loop while (plusp count)
        do (incf unix 86400)
           (when (workday-p unix) (decf count))
        finally (return unix)))

(defun day-label (unix)
  "UNIX as a founder reads a date in a sentence: Tue 04 Nov."
  (multiple-value-bind (second minute hour day month year weekday) (decode-universal-time unix 0)
    (declare (ignore second minute hour year))
    (format nil "~a ~2,'0d ~a"
            (subseq "MonTueWedThuFriSatSun" (* 3 weekday) (+ 3 (* 3 weekday)))
            day
            (subseq "JanFebMarAprMayJunJulAugSepOctNovDec" (* 3 (1- month)) (* 3 month)))))

(defun name= (a b)
  "Whether A and B are the same name, punctuation and case aside."
  (string= (name-key a) (name-key b)))

(defun in-pipeline (ref)
  "The investor REF names, read out of the pipeline on disk."
  (with-pipeline (:save nil) (resolve-investor ref)))
