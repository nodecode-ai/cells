;;;; store.lisp --- the facts, their history, and the one file they live in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A fact is one short standalone sentence, what the person said or what
;;;; the model concluded, with the keywords someone might ask for it by, a
;;;; trust score, an origin and the dates it held. A change keeps the text
;;;; that was there as a version, so 'what did it say before' has an answer;
;;;; FORGET is the only verb that destroys one.
;;;;
;;;; The whole store is a list of FACTS in memory, read from and written
;;;; back to one file of Lisp data (a plist), atomically: the new file is
;;;; written beside the old one and renamed over it. Nothing here dials.

(in-package #:nodecode-holographic)

(defstruct (fact (:conc-name fact-))
  (id 0)
  (content "")
  (keywords '())          ; words someone might ask for it by, never shown as part of it
  (tags "")
  (category "general")
  (trust 0.5)
  (origin "unknown")      ; said | confirmed | inferred | unknown
  (valid-from nil)        ; "YYYY-MM-DD" or NIL
  (valid-to nil)
  (status "current")      ; current | ended
  (created "")
  (updated "")
  (versions '()))         ; oldest first: plists of the text it held before

(defvar *facts* '() "Every fact the store holds, oldest id first.")
(defvar *next-id* 1 "The id the next added fact takes.")
(defvar *loaded-from* nil "The file *FACTS* was read from, or NIL when nothing was read.")
(defvar *path-override* nil "A file the caller names for this store; NIL means the section's.")

(defun today ()
  "The local date as YYYY-MM-DD."
  (multiple-value-bind (second minute hour date month year) (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun store-path ()
  "The file the facts live in: the caller's override, else the section's
db_path when it names one, else one file under this home."
  (or *path-override*
      (let ((chosen (ignore-errors (setting :db-path))))
        (and (stringp chosen)
             (plusp (length (nlk:trimmed chosen)))
             (nlk:trimmed chosen)))
      (namestring (nlk:home "holographic-facts.sexp"))))

;;; --- the numbers a section writes as text ---------------------------------------

(defun number-or (text default)
  "TEXT, a string from the section or a number, as a number between 0 and
1; DEFAULT when it is not one."
  (let ((value (cond ((realp text) text)
                     ((stringp text)
                      (let ((trimmed (nlk:trimmed text)))
                        (and (plusp (length trimmed))
                             (ignore-errors
                              (let ((*read-eval* nil)) (read-from-string trimmed))))))
                     (t nil))))
    (if (and (realp value) (<= 0 value 1)) (coerce value 'single-float) default)))

(defun trust-setting (key default)
  "The section's KEY as a trust between 0 and 1, DEFAULT when it is not one."
  (number-or (ignore-errors (setting key)) default))

;;; --- the file -----------------------------------------------------------------

(defun fact-data (fact)
  "FACT as the plist the file holds."
  (list :id (fact-id fact) :content (fact-content fact) :keywords (fact-keywords fact)
        :tags (fact-tags fact) :category (fact-category fact) :trust (fact-trust fact)
        :origin (fact-origin fact) :valid-from (fact-valid-from fact)
        :valid-to (fact-valid-to fact) :status (fact-status fact)
        :created (fact-created fact) :updated (fact-updated fact)
        :versions (fact-versions fact)))

(defun data-fact (data)
  "The FACT DATA, one entry of the file, describes."
  (make-fact :id (or (getf data :id) 0)
             :content (or (getf data :content) "")
             :keywords (copy-list (getf data :keywords))
             :tags (or (getf data :tags) "")
             :category (or (getf data :category) "general")
             :trust (or (getf data :trust) 0.5)
             :origin (or (getf data :origin) "unknown")
             :valid-from (getf data :valid-from)
             :valid-to (getf data :valid-to)
             :status (or (getf data :status) "current")
             :created (or (getf data :created) "")
             :updated (or (getf data :updated) "")
             :versions (copy-list (getf data :versions))))

(defun store-data ()
  "Everything the file holds."
  (list :version 1 :next-id *next-id* :facts (mapcar #'fact-data *facts*)))

(defun read-store (path)
  "Load PATH into *FACTS*. A file that will not read leaves an empty store
and says so on the way out rather than taking the cell down."
  (let ((facts '()) (next 1))
    (when (probe-file path)
      (handler-case
          (with-open-file (in path :direction :input :if-does-not-exist nil)
            (let ((data (and in (with-standard-io-syntax (read in nil nil)))))
              (when (consp data)
                (dolist (entry (getf data :facts))
                  (push (data-fact entry) facts))
                (setf next (or (getf data :next-id) 1)))))
        (error (condition)
          (warn "holographic: ~a did not read: ~a; starting empty" path condition)
          (setf facts '() next 1))))
    (setf *facts* (nreverse facts)
          *next-id* (max next (1+ (reduce #'max (mapcar #'fact-id *facts*) :initial-value 0)))
          *loaded-from* path)
    *facts*))

(defun write-store ()
  "Write *FACTS* over STORE-PATH, atomically: the new file beside the old,
then renamed onto it."
  (let* ((path (store-path))
         (new (concatenate 'string path ".new")))
    (ensure-directories-exist path)
    (with-open-file (out new :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-readably* t) (*print-pretty* t) (*print-length* nil) (*print-level* nil))
          (write (store-data) :stream out)
          (terpri out))))
    (rename-file new path)
    (setf *loaded-from* path)
    t))

(defun ensure-store ()
  "The facts, read from the configured file when it is not the one in memory."
  (let ((path (store-path)))
    (unless (and *loaded-from* (equal *loaded-from* path))
      (read-store path)))
  *facts*)

;;; --- words ---------------------------------------------------------------------

(defun words (text)
  "TEXT's words, lowercased, each two characters or longer, without repeats."
  (let ((found '()) (current (make-string-output-stream)))
    (labels ((flush ()
               (let ((word (string-downcase (get-output-stream-string current))))
                 (when (>= (length word) 2)
                   (push word found)))))
      (loop for character across (or text "")
            do (if (alphanumericp character)
                   (write-char character current)
                   (flush)))
      (flush))
    (remove-duplicates (nreverse found) :test #'equal)))

(defun same-word-p (asked held)
  "ASKED and HELD are the same word, or one is a form of the other: a
shared stem of four characters, so 'birthday' finds 'birthdays'."
  (or (string= asked held)
      (and (>= (min (length asked) (length held)) 4)
           (or (uiop:string-prefix-p asked held)
               (uiop:string-prefix-p held asked)))))

(defun hit-p (asked words)
  (some (lambda (held) (same-word-p asked held)) words))

(defun coverage (fact asked)
  "The share of ASKED's words FACT holds: its text weighs one, one of its
keywords four fifths. Answers (values COVERAGE SCORE)."
  (let* ((text (words (fact-content fact)))
         (keys (mapcan #'words (fact-keywords fact)))
         (hits 0))
    (dolist (word asked)
      (cond ((hit-p word text) (incf hits 1))
            ((hit-p word keys) (incf hits 4/5))))
    (let ((share (if asked (/ hits (length asked)) 0)))
      (values share (+ share (* 3/10 (fact-trust fact)))))))

(defun fact-when (fact)
  "The dates FACT holds, as one clause for a line, or NIL."
  (cond ((and (fact-valid-from fact) (fact-valid-to fact))
         (format nil "~a to ~a" (fact-valid-from fact) (fact-valid-to fact)))
        ((fact-valid-from fact) (format nil "since ~a" (fact-valid-from fact)))
        ((fact-valid-to fact) (format nil "until ~a" (fact-valid-to fact)))))

(defun fact-line (fact &key source)
  "FACT as one line for the model or the operator."
  (format nil "[~d] ~a (trust ~,2f~@[; ~a~]~@[; ~a~]~@[; ~a~])"
          (fact-id fact) (fact-content fact) (fact-trust fact)
          (let ((origin (fact-origin fact)))
            (and (stringp origin) (plusp (length origin))
                 (not (equal origin "unknown")) origin))
          (fact-when fact)
          source))

(defun facts-text (facts &key source)
  "FACTS as lines, or one sentence when there are none."
  (if facts
      (format nil "~{~a~^~%~}" (mapcar (lambda (fact) (fact-line fact :source source)) facts))
      "No relevant facts found."))

;;; --- dates --------------------------------------------------------------------

(defun held-at-p (fact as-of)
  "FACT held on AS-OF (YYYY-MM-DD): current, or the version that was current then."
  (if as-of
      (and (or (null (fact-valid-from fact)) (not (string-lessp as-of (fact-valid-from fact))))
           (or (null (fact-valid-to fact)) (string-lessp as-of (fact-valid-to fact))))
      (equal (fact-status fact) "current")))

;;; --- the verbs ------------------------------------------------------------------

(defun find-fact (id)
  "The fact ID, or a refusal naming it."
  (or (find id *facts* :key #'fact-id)
      (refuse "no fact #~a in the store; use action list to see the ids" id)))

(defun recall (query &key (limit 10) min-trust as-of)
  "The facts QUERY recalls, best first: by how much of the query each holds,
then by trust => a list of FACTs."
  (ensure-store)
  (let* ((asked (words query))
         (floor (or min-trust (trust-setting :min-trust 0.3)))
         (candidates '()))
    (dolist (fact *facts*)
      (when (and (held-at-p fact as-of)
                 (>= (fact-trust fact) floor))
        (multiple-value-bind (held score) (coverage fact asked)
          (when (plusp held)
            (push (list score (fact-trust fact) (fact-id fact) fact) candidates)))))
    (loop for entry in (sort candidates #'>
                              :key (lambda (entry) (list (first entry) (second entry) (third entry))))
          for fact = (fourth entry)
          repeat limit
          collect fact)))

(defun remember (content &key keywords tags category origin valid-from)
  "Store CONTENT as a new fact => its FACT."
  (ensure-store)
  (let ((text (nlk:trimmed (or content ""))))
    (when (zerop (length text))
      (refuse "add needs content: one short standalone sentence"))
    (let ((fact (make-fact :id (prog1 *next-id* (incf *next-id*))
                           :content text
                           :keywords (clean-keywords keywords)
                           :tags (or tags "")
                           :category (or category "general")
                           :trust (trust-setting :default-trust 0.5)
                           :origin (or (blank-nil origin) "unknown")
                           :valid-from (blank-nil valid-from)
                           :created (today)
                           :updated (today))))
      (setf *facts* (append *facts* (list fact)))
      (write-store)
      fact)))

(defun blank-nil (text)
  "TEXT trimmed, or NIL when it is blank."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun clean-keywords (keywords)
  "KEYWORDS as a list of short trimmed words, at most twelve."
  (loop for keyword in (or keywords '())
        for text = (and (stringp keyword) (nlk:trimmed keyword))
        when (and text (plusp (length text)))
          collect (subseq text 0 (min 40 (length text))) into kept
        finally (return (subseq kept 0 (min 12 (length kept))))))

(defun similar-facts (fact &key (limit 3))
  "Facts that say something close to FACT: the ones sharing at least half
its words."
  (let ((asked (words (fact-content fact))))
    (loop for other in *facts*
          unless (eq other fact)
            when (multiple-value-bind (held) (coverage other asked)
                   (>= held 1/2))
              collect other into close
          finally (return (subseq close 0 (min limit (length close)))))))

(defun change-fact (id content &key keywords valid-from origin)
  "FACT ID now says CONTENT. The text it said is kept as a version; its
keywords go unless given again."
  (let* ((fact (find-fact id))
         (text (nlk:trimmed (or content ""))))
    (when (zerop (length text))
      (refuse "change needs content: the sentence it says now"))
    (setf (fact-versions fact)
          (append (fact-versions fact)
                  (list (list :content (fact-content fact) :keywords (fact-keywords fact)
                              :kind "change" :at (today)
                              :valid-from (fact-valid-from fact) :valid-to (fact-valid-to fact)))))
    (setf (fact-content fact) text
          (fact-keywords fact) (if keywords (clean-keywords keywords) '())
          (fact-valid-from fact) (or (blank-nil valid-from) (fact-valid-from fact))
          (fact-origin fact) (or (blank-nil origin) (fact-origin fact))
          (fact-valid-to fact) nil
          (fact-status fact) "current"
          (fact-updated fact) (today))
    (write-store)
    fact))

(defun end-fact (id &key valid-to)
  "FACT ID stopped being true: it is no longer recalled, and its history keeps it."
  (let ((fact (find-fact id)))
    (setf (fact-versions fact)
          (append (fact-versions fact)
                  (list (list :content (fact-content fact) :keywords (fact-keywords fact)
                              :kind "end" :at (today) :valid-to (or (blank-nil valid-to) (today)))))
          (fact-valid-to fact) (or (blank-nil valid-to) (today))
          (fact-status fact) "ended"
          (fact-updated fact) (today))
    (write-store)
    fact))

(defun forget (id)
  "Take FACT ID out of the store for good, history included => the text it said."
  (let ((fact (find-fact id)))
    (setf *facts* (remove fact *facts*))
    (write-store)
    (fact-content fact)))

(defun history-of (id)
  "FACT ID's versions, newest first, each as one line."
  (let ((fact (find-fact id)))
    (append
     (loop for version in (reverse (fact-versions fact))
           collect (format nil "~a (~a~@[; ~a~]) -> ~a"
                            (or (getf version :at) "?") (or (getf version :kind) "version")
                            (getf version :valid-to) (getf version :content)))
     (list (format nil "~a (current~@[; ~a~]) -> ~a" (fact-updated fact) (fact-when fact)
                   (fact-content fact))))))

(defun feedback (id action)
  "ACTION is how FACT ID did: helpful raises its trust, unhelpful lowers it,
confirmed says the person said it again."
  (let ((fact (find-fact id)))
    (multiple-value-bind (delta origin)
        (cond ((equal action "helpful") (values 1/10 nil))
              ((equal action "unhelpful") (values -1/10 nil))
              ((equal action "confirmed") (values 3/20 "confirmed"))
              (t (refuse "fact_feedback takes helpful, unhelpful or confirmed")))
      (setf (fact-trust fact) (max 0 (min 1 (+ (fact-trust fact) delta)))
            (fact-updated fact) (today))
      (when origin (setf (fact-origin fact) origin))
      (write-store)
      fact)))
