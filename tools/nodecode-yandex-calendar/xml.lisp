;;;; xml.lisp --- the smallest XML reader the CalDAV answers need.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A CalDAV server answers one document at a time and this cell reads four
;;;; things out of it: an href, a display name, a resourcetype's calendar
;;;; child, and the iCalendar text in a calendar-data element. None of that
;;;; needs namespaces resolved, attributes typed, or a schema, so this is one
;;;; pass over the text building a tree of local names.
;;;;
;;;; The answers are untrusted, and a calendar is someone else's text: the
;;;; document is capped in size and nesting, a DTD is refused where it starts
;;;; (which leaves no entity to expand and no external fetch to make), and a
;;;; document that closes an element it never opened is refused rather than
;;;; guessed at. An entity other than the five XML defines and a numeric
;;;; character reference is left as it was written.

(in-package #:nodecode-yandex-calendar)

(defconstant +max-xml-chars+ (* 2 1024 1024)
  "The largest document, in characters, that PARSE-XML accepts.")
(defconstant +max-xml-depth+ 32
  "The deepest nesting PARSE-XML accepts: CalDAV's answers reach six.")

(defstruct (xnode (:constructor make-xnode (name &optional attrs)))
  "One element: its local NAME, its CHILDREN, and the character data it holds."
  (name "" :type string)
  (attrs '() :type list)
  (children '() :type list)
  (text '() :type list))

(defun xml-text (node)
  "The character data NODE holds, its children's included, as one string."
  (with-output-to-string (out)
    (dolist (chunk (reverse (xnode-text node)))
      (write-string chunk out))
    (dolist (child (reverse (xnode-children node)))
      (write-string (xml-text child) out))))

(defun xml-children (node name)
  "NODE's direct children called NAME, in document order."
  (loop for child in (reverse (xnode-children node))
        when (string= name (xnode-name child)) collect child))

(defun xml-find (node name)
  "The first element called NAME at any depth under NODE, or NIL."
  (if (string= name (xnode-name node))
      node
      (loop for child in (reverse (xnode-children node))
            for found = (xml-find child name)
            when found return found)))

(defun xml-find-all (node name)
  "Every element called NAME at any depth under NODE, in document order."
  (append (when (string= name (xnode-name node)) (list node))
          (loop for child in (reverse (xnode-children node))
                append (xml-find-all child name))))

(defun xml-entity (name)
  "What the entity NAME stands for, or NIL when it is not one this reader decodes."
  (cond ((string= name "lt") "<")
        ((string= name "gt") ">")
        ((string= name "amp") "&")
        ((string= name "quot") "\"")
        ((string= name "apos") "'")
        ((and (> (length name) 1) (char= #\# (char name 0)))
         (let ((code (ignore-errors
                       (if (and (> (length name) 2) (member (char name 1) '(#\x #\X)))
                           (parse-integer name :start 2 :radix 16)
                           (parse-integer name :start 1)))))
           (and code (code-char code) (string (code-char code)))))))

(defun xml-unescape (text)
  "TEXT with the five XML entities and numeric character references decoded."
  (if (null (position #\& text))
      text
      (with-output-to-string (out)
        (loop with i = 0 with n = (length text)
              for amp = (position #\& text :start i)
              do (cond ((null amp) (write-string text out :start i) (return))
                       (t (write-string text out :start i :end amp)
                          (let ((semi (and (< amp (1- n)) (position #\; text :start (1+ amp)))))
                            (if (and semi (< (- semi amp) 12))
                                (let ((replacement (xml-entity (subseq text (1+ amp) semi))))
                                  (if replacement
                                      (progn (write-string replacement out) (setf i (1+ semi)))
                                      (progn (write-char #\& out) (setf i (1+ amp)))))
                                (progn (write-char #\& out) (setf i (1+ amp)))))))))))

(defun xml-local-name (tag)
  "TAG's name without its namespace prefix, lower-cased."
  (string-downcase (subseq tag (1+ (or (position #\: tag) -1)))))

(defun parse-xml (text)
  "TEXT, one XML document, as its root XNODE; CALDAV-ERROR for one this reader refuses."
  (let ((length (length text)))
    (when (> length +max-xml-chars+)
      (caldav-fail "Refusing to parse the response: it is ~a characters, more than the ~
limit of ~a." length +max-xml-chars+))
    (let ((stack '())
          (root nil)
          (i 0))
      (labels ((top () (first stack))
               (finish (node)
                 (if stack
                     (push node (xnode-children (top)))
                     (setf root node)))
               (emit (chunk)
                 (when (and (top) (plusp (length chunk)))
                   (push chunk (xnode-text (top))))))
        (loop while (< i length)
              do (let ((lt (position #\< text :start i)))
                   (when (null lt)
                     (emit (subseq text i))
                     (return))
                   (emit (xml-unescape (subseq text i lt)))
                   (let ((gt (position #\> text :start lt)))
                     (unless gt
                       (caldav-fail "Could not parse the response: an element is never closed."))
                     (let ((tag (subseq text (1+ lt) gt)))
                       (setf i (1+ gt))
                       (cond
                         ((zerop (length tag)) nil)
                         ((char= #\? (char tag 0)) nil)
                         ((and (>= (length tag) 3) (string= tag "!--" :end1 3))
                          (unless (and (>= (length tag) 5) (string= tag "--" :start1 (- (length tag) 2)))
                            (let ((end (search "-->" text :start2 i)))
                              (unless end
                                (caldav-fail "Could not parse the response: a comment is never closed."))
                              (setf i (+ end 3)))))
                         ((and (>= (length tag) 8) (string= tag "![CDATA[" :end1 8))
                          (let ((end (search "]]>" text :start2 i)))
                            (unless end
                              (caldav-fail "Could not parse the response: CDATA is never closed."))
                            (emit (subseq text i end))
                            (setf i (+ end 3))))
                         ((char= #\! (char tag 0))
                          (when (uiop:string-prefix-p "!DOCTYPE" (string-upcase tag))
                            (caldav-fail "Rejected the response: it declares a DTD.")))
                         ((char= #\/ (char tag 0))
                          (let ((name (xml-local-name (string-trim " " (subseq tag 1)))))
                            (let ((node (pop stack)))
                              (unless (and node (string= name (xnode-name node)))
                                (caldav-fail "Could not parse the response: ~a closes an element ~
that is not open." name))
                              (finish node))))
                         (t (let* ((closing (and (plusp (length tag))
                                                  (char= #\/ (char tag (1- (length tag))))))
                                   (body (string-trim " " (if closing (subseq tag 0 (1- (length tag))) tag)))
                                   (space (position #\Space body))
                                   (node (make-xnode (xml-local-name (if space (subseq body 0 space) body)))))
                              (when (> (1+ (length stack)) +max-xml-depth+)
                                (caldav-fail "Rejected the response: its elements are nested ~
more than ~a deep." +max-xml-depth+))
                              (cond (closing (finish node))
                                    (t (push node stack)))))))))))
      (when stack
        (caldav-fail "Could not parse the response: it ends with ~a still open."
                     (xnode-name (top))))
      (or root (caldav-fail "Could not parse the response: it holds no element.")))))
