;;;; watch.lisp --- one saved query, the ids it has already reported, and the file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept its watch in a JSON file under the plugin's data
;;;; directory, and advanced seen ids only inside a run it could prove was a
;;;; cron run. A cell has no such proof to read, so the port advances on every
;;;; check -- the operator's own check is the only one there is -- and keeps
;;;; the two states that matter: a baseline is pending until the first read
;;;; after `save', and a page that reads empty is remembered so a quiet watch
;;;; does not repeat itself.
;;;;
;;;; The file is the cell's only durable write, it lives under the Nodecode
;;;; home, and a write that fails leaves the previous file exactly as it was.

(in-package #:nodecode-index-search)

(defparameter +seen-cap+ 500
  "How many ids are remembered; the oldest fall off first, as the original's did.")

(defparameter +msg-baseline+
  "Baseline saved. These ids are not reported as new.")

(defparameter +msg-kept+
  "No unseen id is on this page. Seen ids were kept.")

(defparameter +msg-no-watch+
  "No watch query is saved, so seen ids were not advanced.")

(defparameter +msg-corrupt+
  "The saved watch file is unreadable, so seen ids were not advanced.")

(defparameter +msg-not-saved+
  "The page was read, but the watch file was not written. Seen ids were not advanced.")

(defparameter +msg-overwritten+
  "The saved watch file is unreadable, so it was not overwritten.")

(defparameter +msg-write-failed+
  "The watch file was not written. This call did not search. Seen ids were not changed.")

(defparameter +msg-limit+
  " Only the first 20 hits are watched; if estimatedTotalHits is over 20, matches past that page are never reported.")

(defparameter +schedule-line+
  "On a schedule: have the cron cell run index_watch with action check every 15 minutes."
  "The line a saved watch ends with, naming the one way a cell runs on a schedule.")

;;; --- the file -------------------------------------------------------------------

(defvar *state-file* nil
  "The watch file a test writes instead of the one under the home.")

(defun state-file ()
  "The one file this cell writes: its saved query and the ids it has reported."
  (or *state-file* (nlk:home "index-search/watch.json")))

(defun state-string (state key)
  "STATE's member KEY when it is a string, else the empty string."
  (let ((value (and (hash-table-p state) (gethash key state))))
    (if (stringp value) value "")))

(defun state-flag (state key)
  "STATE's member KEY as a boolean, false when it is anything else."
  (and (hash-table-p state) (eq t (gethash key state))))

(defun seen-ok-p (entry)
  "ENTRY as one remembered id: an object with a kind and a value of that kind."
  (and (hash-table-p entry)
       (let ((kind (nlk:json-value entry :string "t"))
             (value (nlk:json-value entry :any "v")))
         (cond
           ((equal kind "s") (and (stringp value) (plusp (length value))))
           ((equal kind "i") (integerp value))
           (t nil)))))

(defun seen-item (entry)
  "ENTRY as the id the page is compared against: (\"s\" . \"abc\") or (\"i\" . 7)."
  (cons (nlk:json-value entry :string "t") (nlk:json-value entry :any "v")))

(defun normalize-state (data)
  "DATA as the state this cell runs on, or :CORRUPT when a member it needs is not
the shape it promises."
  (let ((seen (nlk:json-value data :array "seen")))
    (if (and seen (not (every #'seen-ok-p seen)))
        :corrupt
        (nlk:json-object
         "index" (state-string data "index")
         "q" (state-string data "q")
         "id_field" (state-string data "id_field")
         "title_field" (state-string data "title_field")
         "baseline_pending" (state-flag-or data "baseline_pending" t)
         "last_empty" (state-flag-or data "last_empty" nil)
         "seen" (coerce (or seen #()) 'vector)))))

(defun state-flag-or (state key default)
  "STATE's member KEY as a boolean, DEFAULT when it was never written."
  (multiple-value-bind (value present) (nlk:json-value state :boolean key)
    (if present (and value t) default)))

(defun watch-state ()
  "The saved watch. => (values KIND STATE), KIND :MISSING, :CORRUPT or :OK."
  (let* ((path (state-file))
         (raw (and (probe-file path) (ignore-errors (uiop:read-file-string path)))))
    (cond
      ((not (probe-file path)) (values :missing nil))
      ((null raw) (values :corrupt nil))
      (t (let ((data (page-json raw)))
           (if (null data)
               (values :corrupt nil)
               (let ((state (normalize-state data)))
                 (if (eq state :corrupt)
                     (values :corrupt nil)
                     (values :ok state)))))))))

(defun save-state (state)
  "STATE written to the watch file in one rename. => T, or NIL when it was not."
  (let* ((path (state-file))
         (folder (uiop:pathname-directory-pathname path))
         (draft (merge-pathnames (format nil ".watch-~d.tmp" (get-universal-time)) folder)))
    (handler-case
        (progn
          (ensure-directories-exist path)
          (with-open-file (out draft :direction :output :if-exists :supersede
                                     :if-does-not-exist :create :external-format :utf-8)
            (write-string (nlk:encode-json-object state) out))
          (uiop:rename-file-overwriting-target draft path)
          t)
      (error () (ignore-errors (delete-file draft)) nil))))

;;; --- what the file's ids mean ---------------------------------------------------

(defun id-entry (item)
  "ITEM as the object the file stores: its kind and its value."
  (nlk:json-object "t" (car item) "v" (cdr item)))

(defun trim-seen (seen)
  "SEEN cut to the newest +SEEN-CAP+ ids."
  (if (> (length seen) +seen-cap+)
      (subseq seen (- (length seen) +seen-cap+))
      seen))

(defun seen-set (state)
  "Every id STATE remembers, as a list."
  (loop for entry across (or (gethash "seen" state) #())
        when (seen-ok-p entry)
          collect (seen-item entry)))

(defun gap-note (state found total)
  "The sentence a full page earns when it shares no id with the saved ones."
  (if (or (<= total +hit-limit+) (state-flag state "baseline_pending"))
      ""
      (let ((known (seen-set state))
            (page (mapcar #'car found)))
        (if (and known (null (intersection known page :test #'equal)))
            +msg-gap+
            ""))))

;;; --- the three verbs --------------------------------------------------------------

(defun describe-watch (state)
  "STATE as the sentence a person or the model reads."
  (format nil "A watch query is saved. Index: ~a. Query: ~a. Id field: ~a. Title field: ~a. Seen ids: ~d.~%~a"
          (state-string state "index") (state-string state "q")
          (state-string state "id_field")
          (let ((title (state-string state "title_field")))
            (if (plusp (length title)) title "(none)"))
          (length (seen-set state))
          +schedule-line+))

(defun watch-status ()
  "The saved watch as text, or a line saying there is none. => TEXT."
  (multiple-value-bind (kind state) (watch-state)
    (case kind
      (:corrupt +msg-corrupt+)
      (:missing "No watch query is saved.")
      (t (let ((problem (query-problem (state-string state "index")
                                       (state-string state "q")
                                       (state-string state "id_field")
                                       (state-string state "title_field"))))
           (if problem
               (format nil "A watch is saved, but it cannot run: ~a" problem)
               (describe-watch state)))))))

(defun watch-zero (state)
  "A page with no hits at all, read against STATE. => TEXT."
  (let ((baseline (state-flag state "baseline_pending")))
    (setf (gethash "baseline_pending" state) nil
          (gethash "last_empty" state) t)
    (when baseline (setf (gethash "seen" state) #()))
    (if (save-state state)
        (format nil "~a~a" (if baseline +msg-baseline+ +msg-zero+) (total-suffix 0))
        (format nil "ERROR: ~a" +msg-not-saved+))))

(defun watch-hits (hits total state)
  "A page of HITS read against STATE; the ids it has not reported are the answer."
  (multiple-value-bind (found message cause) (ids-of hits (state-string state "id_field")
                                                                (state-string state "title_field"))
    (declare (ignore cause))
    (if (null found)
        (format nil "ERROR: ~a" message)
        (let ((baseline (state-flag state "baseline_pending")))
          (if baseline
              (progn
                (setf (gethash "baseline_pending" state) nil
                      (gethash "last_empty" state) nil
                      (gethash "seen" state)
                      (trim-seen (map 'vector #'id-entry (mapcar #'car found))))
                (if (save-state state)
                    (format nil "~a~a" +msg-baseline+ (total-suffix total))
                    (format nil "ERROR: ~a" +msg-not-saved+)))
              (let* ((known (seen-set state))
                     (fresh (remove-if (lambda (pair) (member (car pair) known :test #'equal))
                                       found))
                     (gap (gap-note state found total)))
                (setf (gethash "last_empty" state) nil
                      (gethash "seen" state)
                      (trim-seen (concatenate 'vector
                                              (or (gethash "seen" state) #())
                                              (map 'vector #'id-entry (mapcar #'car fresh)))))
                (when (null fresh) (setf (gethash "last_empty" state) t))
                (let ((text (if fresh
                                (format nil "New ids: ~a.~a~a" (public-text fresh)
                                        (total-suffix total) gap)
                                (format nil "~a~a" +msg-kept+ (total-suffix total)))))
                  (if (save-state state)
                      text
                      (format nil "ERROR: ~a" +msg-not-saved+)))))))))

(defun watch-page (page state)
  "PAGE read against STATE: the text to answer, and the file written beside it."
  (case (first page)
    (:hits (watch-hits (second page) (third page) state))
    (:zero (watch-zero state))
    (t (format nil "ERROR: ~a" (second page)))))

(defun watch-check ()
  "One check of the saved query. => TEXT."
  (multiple-value-bind (kind state) (watch-state)
    (cond
      ((eq kind :corrupt) (format nil "ERROR: ~a" +msg-corrupt+))
      ((eq kind :missing) (format nil "ERROR: ~a" +msg-no-watch+))
      (t
       (let ((problem (query-problem (state-string state "index")
                                      (state-string state "q")
                                      (state-string state "id_field")
                                      (state-string state "title_field"))))
         (if problem
             (format nil "ERROR: ~a" problem)
             (multiple-value-bind (origin host-problem) (origin-of (setting :host))
               (if host-problem
                   (format nil "ERROR: ~a" host-problem)
                   (let ((key (setting :api-key)))
                     (if (zerop (length key))
                         (format nil "ERROR: ~a" +msg-key-required+)
                         (watch-page (call-search origin (state-string state "index")
                                                           (state-string state "q") key)
                                     state)))))))))))

(defun watch-save (index query id-field title-field)
  "One saved query, in the file. => TEXT."
  (let ((problem (query-problem index query id-field title-field)))
    (if problem
        (format nil "ERROR: ~a" problem)
        (multiple-value-bind (kind previous) (watch-state)
          (if (eq kind :corrupt)
              (format nil "ERROR: ~a" +msg-overwritten+)
              (save-watch-state index query id-field title-field previous))))))

(defun save-watch-state (index query id-field title-field previous)
  "The new state for INDEX, QUERY and the two fields, written over PREVIOUS."
  (let* ((same (and previous
                     (equal (state-string previous "index") index)
                     (equal (state-string previous "q") query)
                     (equal (state-string previous "id_field") id-field)))
         (title (if (plusp (length title-field))
                    title-field
                    (if same (state-string previous "title_field") "")))
         (state (nlk:json-object
                 "index" index
                 "q" query
                 "id_field" id-field
                 "title_field" title
                 "baseline_pending" (if same (state-flag previous "baseline_pending") t)
                 "last_empty" (if same (state-flag previous "last_empty") nil)
                 "seen" (if previous (or (gethash "seen" previous) #()) #()))))
    (if (null (save-state state))
        (format nil "ERROR: ~a" +msg-write-failed+)
        (format nil "Watch query saved. Index: ~a. Query: ~a. Id field: ~a. Title field: ~a. This call did not search.~a~a~%~a"
                index query id-field (if (plusp (length title)) title "(none)")
                (cond
                  ((null previous) "")
                  ((not same) " The next check saves a new baseline. Seen ids were kept.")
                  ((not (equal (state-string previous "title_field") title))
                   " The title field was updated. Seen ids were kept.")
                  (t " Seen ids were kept."))
                +msg-limit+
                +schedule-line+))))
