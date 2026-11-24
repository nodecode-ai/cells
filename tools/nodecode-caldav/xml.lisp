;;;; xml.lisp --- just enough XML to read a CalDAV multistatus.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original parsed the answer with Python's expat: element names with
;;;; their prefixes ignored, character data gathered per element, a depth
;;;; limit, and a refusal of anything whose root is not a multistatus. This
;;;; is the same reading, written small: one pass over the text into a tree
;;;; of elements, then a walk that picks the elevenths the caller wants.
;;;; Nothing here resolves an entity from outside the document or reads a
;;;; file, so a hostile answer can only be a string.

(in-package #:nodecode-caldav)

(defparameter +max-depth+ 100
  "How deep an answer may nest, as the original allowed.")

(defstruct (xml-node (:constructor make-xml-node (local)))
  (local "" :type string)
  (text '() :type list)
  (children '() :type list))

(defun local-name (name)
  "NAME's local part, the way the original dropped a namespace prefix: the
element name of `D:response x=...' is `response'."
  (let* ((tag (subseq name 0 (or (position-if (lambda (character)
                                                 (member character '(#\Space #\Tab #\Newline #\Return)))
                                               name)
                                   (length name))))
         (colon (position #\: tag)))
    (string-downcase (if colon (subseq tag (1+ colon)) tag))))

(defun xml-children (node)
  "NODE's children in document order, they being gathered newest-first."
  (reverse (xml-node-children node)))

(defun xml-text (node)
  "NODE's own character data, children's excluded, as one string."
  (apply #'concatenate 'string (nreverse (xml-node-text node))))

(defun scan-xml (text)
  "TEXT read as one element tree, its root answered. A document that is not
one well-formed element tree is not_xml, and one nested past +MAX-DEPTH+
levels is too_deep, as the original refused them."
  (let ((stack '())
        (root nil)
        (index 0)
        (size (length text)))
    (labels ((add-text (chunk)
               (when stack (push chunk (xml-node-text (first stack)))))
             (open-element (name self-closing)
               (when (>= (length stack) +max-depth+) (calendar-failed "too_deep"))
               (let ((node (make-xml-node (local-name name))))
                 (cond (stack (push node (xml-node-children (first stack))))
                       (root (calendar-failed "not_xml"))
                       (t (setf root node)))
                 (unless self-closing (push node stack))))
             (close-element ()
               (when stack (pop stack))))
      (loop while (< index size)
            do (let ((character (char text index)))
                 (cond
                   ((char/= character #\<)
                    (let ((next (or (position #\< text :start index) size)))
                      (add-text (subseq text index next))
                      (setf index next)))
                   ((and (<= (+ index 9) size) (string= text "<![CDATA[" :start1 index :end1 (+ index 9)))
                    (let ((end (search "]]>" text :start2 (+ index 9))))
                      (unless end (calendar-failed "not_xml"))
                      (add-text (subseq text (+ index 9) end))
                      (setf index (+ end 3))))
                   ((and (<= (+ index 4) size) (string= text "<!--" :start1 index :end1 (+ index 4)))
                    (let ((end (search "-->" text :start2 (+ index 4))))
                      (unless end (calendar-failed "not_xml"))
                      (setf index (+ end 3))))
                   ((and (<= (+ index 2) size) (string= text "<?" :start1 index :end1 (+ index 2)))
                    (let ((end (search "?>" text :start2 (+ index 2))))
                      (unless end (calendar-failed "not_xml"))
                      (setf index (+ end 2))))
                   ((and (<= (+ index 9) size) (string= text "<!DOCTYPE" :start1 index :end1 (+ index 9)))
                    (let ((end (position #\> text :start index)))
                      (unless end (calendar-failed "not_xml"))
                      (setf index (1+ end))))
                   ((and (<= (+ index 2) size) (string= text "</" :start1 index :end1 (+ index 2)))
                    (let ((end (position #\> text :start index)))
                      (unless end (calendar-failed "not_xml"))
                      (close-element)
                      (setf index (1+ end))))
                   (t (let ((end (position #\> text :start index)))
                        (unless end (calendar-failed "not_xml"))
                        (let ((body (subseq text (1+ index) end)))
                          (if (and (plusp (length body)) (char= (char body (1- (length body))) #\/))
                              (open-element (subseq body 0 (1- (length body))) t)
                              (open-element body nil))
                          (setf index (1+ end))))))))
      (unless (and root (null stack)) (calendar-failed "not_xml"))
      root)))

(defun collect-texts (node local &optional (found '()))
  "Every element's own character data under NODE whose local name is LOCAL,
in document order, in ONE string each."
  (when (string= (xml-node-local node) local)
    (setf found (nconc found (list (xml-text node)))))
  (dolist (child (xml-children node) found)
    (setf found (collect-texts child local found))))

(defun status-code (text)
  "The three-digit status line TEXT carries (`HTTP/1.1 200 OK' => 200), or NIL."
  (let* ((trimmed (nlk:trimmed text))
         (start (search "HTTP/" trimmed))
         (space (and start (position #\Space trimmed :start (+ start 5)))))
    (and space
         (ignore-errors (parse-integer (subseq trimmed (1+ space)
                                                (min (length trimmed) (+ space 4))))))))
