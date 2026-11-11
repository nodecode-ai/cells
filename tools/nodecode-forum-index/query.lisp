;;;; query.lisp --- the list read four ways: by tier, by category, by name, by words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network: the whole point of the cell is that the
;;;; directory answers with no request, no key and no account. A link is a
;;;; string a person opens themselves, in a browser or through Tor.

(in-package #:nodecode-forum-index)

;;; --- reading one entry ----------------------------------------------------------

(defun forums ()
  "Every forum in the list, in the poster's order."
  +forums+)

(defun forum-name (forum) (getf forum :name))
(defun forum-tier (forum) (getf forum :tier))
(defun forum-categories (forum) (getf forum :categories))
(defun forum-urls (forum) (getf forum :urls))
(defun forum-onion (forum) (getf forum :onion))
(defun forum-note (forum) (getf forum :note))

(defun forum-links (forum)
  "Every link the entry carries, the onion ones last."
  (append (forum-urls forum)
          (let ((onion (forum-onion forum))) (and onion (list onion)))))

(defun tier-name (tier)
  "A tier as the list spells it."
  (string-downcase (string tier)))

(defun category-name (category)
  "A category as the list spells it."
  (string-downcase (string category)))

(defun forum-category-text (forum)
  "The entry's categories as one line."
  (format nil "~{~a~^, ~}" (mapcar #'category-name (forum-categories forum))))

(defun tier-sentence (tier)
  (cdr (assoc tier +tiers+)))

(defun category-sentence (category)
  (cdr (assoc category +legend+ :test #'string-equal)))

;;; --- matching -------------------------------------------------------------------

(defun contains-p (needle haystack)
  "Whether NEEDLE appears in HAYSTACK, ignoring case; NIL when either is empty."
  (and (stringp haystack)
       (plusp (length haystack))
       (stringp needle)
       (plusp (length needle))
       (not (null (search (string-downcase needle) (string-downcase haystack))))))

(defun forum-mentions-p (forum text)
  "Whether TEXT appears in anything the entry says."
  (or (contains-p text (forum-name forum))
      (contains-p text (tier-name (forum-tier forum)))
      (some (lambda (url) (contains-p text url)) (forum-links forum))
      (some (lambda (category) (contains-p text (category-name category))) (forum-categories forum))
      (contains-p text (forum-note forum))))

(defun forums-matching (&key category tier text)
  "The forums that satisfy every given filter. CATEGORY and TIER are strings as
the list spells them (\"fraud\", \"established\"); TEXT is matched against the name,
the links, the categories and the note. With no filter, every forum."
  (remove-if-not
   (lambda (forum)
     (and (or (null category)
              (member (string-downcase category)
                      (mapcar #'category-name (forum-categories forum)) :test #'string=))
          (or (null tier)
              (string-equal tier (tier-name (forum-tier forum))))
          (or (null text)
              (zerop (length text))
              (forum-mentions-p forum text))))
   (forums)))

(defun forum-by-name (name)
  "The one forum NAME most nearly names: the same name, else one starting with
it, else one containing it; a bare string that names none is NIL."
  (let ((wanted (string-downcase (or name ""))))
    (when (plusp (length wanted))
      (or (find wanted (forums) :test #'string= :key (lambda (f) (string-downcase (forum-name f))))
          (find wanted (forums) :test #'string= :key (lambda (f) (let ((n (string-downcase (forum-name f))))
                                                                   (subseq n 0 (min (length n) (length wanted))))))
          (find-if (lambda (f) (contains-p wanted (forum-name f))) (forums))))))

;;; --- what a person reads ----------------------------------------------------------

(defun forum-block (forum)
  "One forum as the block a person reads: what it is, where it is, and the caveat."
  (format nil "~a (~a)~%  what: ~a~%  where: ~{~a~^~%         ~}~%  note: ~a~%  tier: ~a"
          (forum-name forum)
          (forum-category-text forum)
          (format nil "~{~a~^; ~}"
                  (mapcar (lambda (category)
                            (or (category-sentence category) (category-name category)))
                          (forum-categories forum)))
          (forum-links forum)
          (or (forum-note forum) "none")
          (tier-sentence (forum-tier forum))))

(defun forum-row (forum)
  "One forum as the line a listing gives: name, categories, the first link."
  (format nil "  ~va ~va ~a"
          (max 18 (length (forum-name forum))) (forum-name forum)
          22 (forum-category-text forum)
          (or (first (forum-links forum)) "(onion only)")))

(defun forum-list-text (forums &key (heading nil))
  "FORUMS as the lines of one listing, HEADING above them."
  (if (null forums)
      "no forum in the list matches that."
      (format nil "~@[~a (~d)~%~]~{~a~^~%~}"
              heading (length forums) (mapcar #'forum-row forums))))

(defun count-by (key forums)
  "How many of FORUMS have each value KEY returns, biggest first."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (forum forums)
      (let ((value (funcall key forum)))
        (dolist (one (if (listp value) value (list value)))
          (when one (incf (gethash (string-downcase (string one)) counts 0))))))
    (sort (loop for value being the hash-keys of counts using (hash-value n)
                collect (cons value n))
          #'string< :key #'car)))

(defun summary-text ()
  "The whole list in one answer: how many, by tier and by category, and where it came from."
  (format nil "~d forums, ~d of them established and ~d with concerns raised; the rest are listed.~%~
Source: ~a.~%~
  by category: ~{~a ~d~^, ~}~%~
  by tier: ~{~a ~d~^, ~}~%~
Ask for one by name, or narrow it: a category (fraud, leaks, hacking, cracking, malware, seo, development, accounts) or a tier (established, rising, other)."
          (length (forums))
          (length (forums-matching :tier "established"))
          (length (forums-matching :tier "rising"))
          +source+
          (loop for (category . n) in (count-by #'forum-categories (forums)) collect category collect n)
          (loop for (tier . n) in (count-by (lambda (f) (tier-name (forum-tier f))) (forums)) collect tier collect n)))

(defun category-text ()
  "The legend: every category the list uses, and the tier sentences."
  (format nil "categories~%~{~a~}~%tiers~%~{~a~}"
          (loop for (category . sentence) in +legend+
                collect (format nil "  ~va ~a~%" (max 12 (length category)) category sentence))
          (loop for (tier . sentence) in +tiers+
                collect (format nil "  ~va ~a~%" (max 12 (length (string tier))) (tier-name tier) sentence))))

(defun source-line ()
  "Where the list comes from, said once."
  (format nil "Source: ~a. A snapshot: the list is the poster's, and no forum on it is vouched for here." +source+))
