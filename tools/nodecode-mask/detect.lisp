;;;; detect.lisp --- the ten detectors, the overlap rule, the numbered
;;;; placeholders, and the restore they make possible.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original's patterns and its scores, one for one: a detector that
;;;; finds a span, a confidence, and a greedy pass that keeps the earliest
;;;; span and, where two start together, the more confident one. A number is
;;;; an ID card before it is a bank card, and the two are never both masked.
;;;;
;;;; A span is replaced by <LABEL_N>, numbered per label and per run, and the
;;;; same words always come back as the same placeholder: the model reads one
;;;; story across a whole session instead of a fresh number per message. The
;;;; mapping is kept so /mask restore can hand the plaintext back, in a file,
;;;; never in the answer.

(in-package #:nodecode-mask)

(defparameter +all-entities+ '("phone" "email" "id-card" "bank-card" "key" "ip")
  "Every detector this cell has, in the order the original declared them.")

(defparameter +default-entities+ '("phone" "email" "id-card" "bank-card" "key")
  "What a cell with nothing set runs: the original's default, IP left out.")

(defparameter +noun+ '(("PHONE" . "phone") ("EMAIL" . "email") ("ID_CARD" . "ID card")
                        ("BANK_CARD" . "bank card") ("KEY" . "key") ("IP" . "IP address"))
  "What a label is called in an answer.")

(defun noun (label)
  (or (cdr (assoc label +noun+ :test #'string=)) label))

(defstruct (detector (:constructor make-detector (entity label source scanner score)))
  (entity "" :type string :read-only t)
  (label "" :type string :read-only t)
  (source "" :type string :read-only t)
  (scanner nil :read-only t)
  (score 0.0d0 :type double-float :read-only t))

(defun make-detectors (specs)
  "DETECTORs from SPECS, each (ENTITY LABEL PATTERN SCORE), the pattern a
Perl string as the original wrote it."
  (loop for (entity label source score) in specs
        collect (make-detector entity label source (cl-ppcre:create-scanner source)
                               (float score 1.0d0))))

(defparameter +detectors+
  (make-detectors
   '(("phone" "PHONE" "(?<!\\d)1[3-9]\\d{9}(?!\\d)" 0.95)
     ("id-card" "ID_CARD" "(?<!\\d)\\d{17}[\\dXx](?!\\d)" 0.92)
     ("email" "EMAIL" "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}" 0.9)
     ("bank-card" "BANK_CARD" "(?<!\\d)\\d{16,19}(?!\\d)" 0.65)
     ("ip" "IP" "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b" 0.85)
     ("key" "KEY" "sk-[A-Za-z0-9_-]{16,}" 0.95)
     ("key" "KEY" "gh[pousr]_[A-Za-z0-9]{16,}" 0.95)
     ("key" "KEY" "xox[baprs]-[A-Za-z0-9-]{10,}" 0.9)
     ("key" "KEY" "AKIA[0-9A-Z]{16}" 0.9)
     ("key" "KEY" "Bearer[ \t]+[A-Za-z0-9._~+/=-]{8,}" 0.9)))
  "The original's detectors, its order and its scores: a mainland mobile
number, a Chinese ID card, an email address, a bank card, a dotted IPv4
address, and the five shapes of a credential.")

(defvar *entities* (copy-list +default-entities+)
  "The detectors this run has on: set by the start from the section.")

(defstruct (hit (:constructor make-hit (type text start end score)))
  (type "" :type string :read-only t)
  (text "" :type string :read-only t)
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (score 0.0d0 :type double-float :read-only t))

(defun detect-hits (text &key (entities +default-entities+))
  "Every span of TEXT the enabled ENTITIES find, in detector order."
  (when (and (stringp text) (plusp (length text)))
    (loop for detector in +detectors+
          when (member (detector-entity detector) entities :test #'string=)
            append (loop for (start end) on (cl-ppcre:all-matches (detector-scanner detector) text)
                           by #'cddr
                         collect (make-hit (detector-label detector) (subseq text start end)
                                           start end (detector-score detector))))))

(defun resolve-overlaps (hits)
  "HITS with the overlaps taken out: earliest start first, the more confident
where two begin together, and a span that overlaps one already kept is
dropped. The order answered is the order they are replaced in."
  (let ((sorted (stable-sort (copy-list hits)
                              (lambda (a b)
                                (if (/= (hit-start a) (hit-start b))
                                    (< (hit-start a) (hit-start b))
                                    (> (hit-score a) (hit-score b))))))
        (kept '())
        (last-end -1))
    (dolist (hit sorted)
      (when (>= (hit-start hit) last-end)
        (push hit kept)
        (setf last-end (hit-end hit))))
    (nreverse kept)))

;;; --- the placeholders ----------------------------------------------------------

(defstruct (stripper (:conc-name stripper-))
  "One run's numbering: LABEL -> count, placeholder -> the words it stands
for, the words -> their placeholder, and the placeholders in the order they
were made, oldest first, so the oldest can go when the table is full."
  (counters (make-hash-table :test #'equal))
  (places (make-hash-table :test #'equal))
  (values (make-hash-table :test #'equal))
  (order '())
  (total 0)
  (distribution (make-hash-table :test #'equal))
  (max-entries 500 :type fixnum))

(defvar *stripper* (make-stripper)
  "The running cell's numbering, reset by a start.")

(defun reset-stripper (&optional (stripper *stripper*) (max-entries 500))
  "Empty STRIPPER and let it remember MAX-ENTRIES placeholders at once."
  (clrhash (stripper-counters stripper))
  (clrhash (stripper-places stripper))
  (clrhash (stripper-values stripper))
  (clrhash (stripper-distribution stripper))
  (setf (stripper-order stripper) nil
        (stripper-total stripper) 0
        (stripper-max-entries stripper) max-entries)
  stripper)

(defun placeholder-for (stripper label original)
  "ORIGINAL's placeholder, <LABEL_N>, made (and remembered) the first time
these exact words are seen."
  (or (gethash original (stripper-values stripper))
      (let* ((count (incf (gethash label (stripper-counters stripper) 0)))
             (placeholder (format nil "<~a_~d>" label count)))
        (setf (gethash placeholder (stripper-places stripper)) original
              (gethash original (stripper-values stripper)) placeholder)
        (setf (stripper-order stripper) (nconc (stripper-order stripper) (list placeholder)))
        placeholder)))

(defun prune-stripper (stripper)
  "Forget the oldest placeholders past MAX-ENTRIES; their numbers are never
reused, so a placeholder an old message still carries simply stops restoring."
  (loop while (> (hash-table-count (stripper-places stripper)) (stripper-max-entries stripper))
        for oldest = (pop (stripper-order stripper))
        while oldest
        do (let ((original (gethash oldest (stripper-places stripper))))
             (remhash oldest (stripper-places stripper))
             (when original (remhash original (stripper-values stripper))))))

(defun mask-hits (text hits &optional (stripper *stripper*))
  "TEXT with every HIT replaced by its placeholder, numbering and counting as
it goes."
  (let ((parts '())
        (last 0))
    (dolist (hit hits)
      (push (subseq text last (hit-start hit)) parts)
      (push (placeholder-for stripper (hit-type hit) (hit-text hit)) parts)
      (setf last (hit-end hit))
      (incf (stripper-total stripper))
      (incf (gethash (hit-type hit) (stripper-distribution stripper) 0)))
    (push (subseq text last) parts)
    (prune-stripper stripper)
    (apply #'concatenate 'string (nreverse parts))))

(declaim (ftype function note-failure))

(defun mask-text (text &key (entities (or *entities* +default-entities+)) (stripper *stripper*))
  "TEXT as it leaves: every detected span replaced by its placeholder. A
 detector that throws answers TEXT itself, counted, as the original did."
  (if (or (not (stringp text)) (zerop (length text)))
      text
      (handler-case
          (mask-hits text (resolve-overlaps (detect-hits text :entities entities)) stripper)
        (error (condition) (note-failure condition) text))))

;;; --- the way back --------------------------------------------------------------

(defun replace-all-string (text needle replacement)
  "TEXT with every NEEDLE replaced by REPLACEMENT, literally."
  (with-output-to-string (out)
    (loop with start = 0
          for position = (search needle text :start2 start)
          while position
          do (write-string text out :start start :end position)
             (write-string replacement out)
             (setf start (+ position (length needle)))
          finally (write-string text out :start start))))

(defun placeholders-in (text &optional (stripper *stripper*))
  "The placeholders of this run that TEXT carries, longest first."
  (when (stringp text)
    (sort (remove-if-not (lambda (placeholder) (search placeholder text))
                          (stripper-order stripper))
          #'> :key #'length)))

(defun restore-text (text &optional (stripper *stripper*))
  "TEXT with every known placeholder put back to the words it stands for."
  (let ((result text))
    (dolist (placeholder (placeholders-in text stripper) result)
      (let ((original (gethash placeholder (stripper-places stripper))))
        (when original
          (setf result (replace-all-string result placeholder original)))))))

;;; --- what an answer says ----------------------------------------------------------

(defun hit-counts (hits)
  "HITS counted by label, alphabetically: ((LABEL . COUNT) ...)."
  (let ((counts '()))
    (dolist (hit hits)
      (let ((cell (assoc (hit-type hit) counts :test #'string=)))
        (if cell (incf (cdr cell)) (push (cons (hit-type hit) 1) counts))))
    (sort counts #'string< :key #'car)))

(defun found-summary (hits)
  "One line on what HITS found, or that nothing was."
  (if (null hits)
      "nothing found: the text comes back as it was"
      (format nil "masked ~{~a~^, ~}"
              (loop for (label . count) in (hit-counts hits)
                    collect (format nil "~d ~a~a" count (noun label)
                                    (if (= count 1) "" "s"))))))

(defun distribution-words (distribution)
  "DISTRIBUTION, a hash table of LABEL -> COUNT, as one line for a reader."
  (let ((counts (loop for label being the hash-keys of distribution using (hash-value count)
                      collect (cons label count))))
    (if (null counts)
        "(none yet)"
        (format nil "~{~a~^, ~}"
                (loop for (label . count) in (sort counts #'string< :key #'car)
                      collect (format nil "~a=~d" label count))))))
