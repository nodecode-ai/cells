;;;; xml.lisp --- the Event Log's XML, read by the eighty lines it needs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Windows renders every event record as XML in one namespace:
;;;;   <Event xmlns='http://schemas.microsoft.com/win/2004/08/events/event'>
;;;;     <System><Provider Name='...'/>...<EventID>1000</EventID>...</System>
;;;;     <EventData><Data Name='...'>value</Data></EventData>
;;;;   </Event>
;;;; wevtutil writes those documents one after another, so this reads a
;;;; stream of them: elements, attributes, text and the five predefined
;;;; entities, nothing else. A real record carries no DTD and no CDATA, and
;;;; one default namespace, so no namespace machinery is needed: a prefixed
;;;; element name is kept by its local name.
;;;;
;;;; A node is (NAME ATTRS CHILDREN): ATTRS an alist of (NAME . VALUE), the
;;;; name and the value as written, CHILDREN a list of nodes and strings in
;;;; document order. The readers are case-insensitive on names, because
;;;; Windows providers disagree with themselves between builds ("Faulting
;;;; Module Name" beside "Faulting module name").

(in-package #:nodecode-windows-eventlog)

;;; --- a node -------------------------------------------------------------------

(defstruct (xnode (:constructor %xnode (name attrs children))
                   (:copier nil)
                   (:predicate xnode-p))
  "One XML element: NAME, ATTRS an alist, CHILDREN nodes and strings."
  (name "" :type string :read-only t)
  (attrs '() :read-only t)
  (children '() :read-only t))

(defun name-equal (left right)
  "XML names compared the way Windows providers spell them: case-blind."
  (and (stringp left) (stringp right) (string-equal left right)))

(defun xattr (node name)
  "NODE's attribute NAME, or NIL."
  (cdr (assoc name (xnode-attrs node) :test #'name-equal)))

(defun xchild (node name)
  "NODE's first child element called NAME, or NIL."
  (when node
    (find-if (lambda (child) (and (xnode-p child) (name-equal (xnode-name child) name)))
             (xnode-children node))))

(defun xchildren (node name)
  "NODE's child elements called NAME, in document order."
  (when node
    (remove-if-not (lambda (child) (and (xnode-p child) (name-equal (xnode-name child) name)))
                   (xnode-children node))))

(defun xtext (node)
  "Every string under NODE, joined and trimmed; the empty string for none."
  (let ((parts '()))
    (labels ((walk (item)
               (cond ((stringp item) (push item parts))
                     ((xnode-p item) (mapc #'walk (xnode-children item))))))
      (when node (walk node)))
    (string-trim '(#\Space #\Tab #\Newline #\Return #\Null)
                 (format nil "~{~a~}" (nreverse parts)))))

(defun xdescribe (node)
  "NODE rendered back as text: the provider's own schema, for the one payload
Windows nests in UserData, kept as the text it spells."
  (if (null node)
      ""
      (with-output-to-string (out)
        (labels ((walk (item)
                   (cond
                     ((stringp item) (write-string item out))
                     ((xnode-p item)
                      (format out "<~a" (xnode-name item))
                      (dolist (pair (xnode-attrs item))
                        (format out " ~a='~a'" (car pair) (cdr pair)))
                      (if (xnode-children item)
                          (progn (write-string ">" out)
                                 (mapc #'walk (xnode-children item))
                                 (format out "</~a>" (xnode-name item)))
                          (write-string "/>" out))))))
          (walk node)))))

;;; --- entities -------------------------------------------------------------------

(defparameter +entities+ '(("lt" . "<") ("gt" . ">") ("amp" . "&") ("quot" . "\"") ("apos" . "'"))
  "The five predefined entities, which is all an event record uses.")

(defun character-reference (body)
  "BODY as `#NN' or `#xNN' spells it, or NIL."
  (when (and (> (length body) 1) (char= #\# (char body 0)))
    (handler-case
        (if (find (char body 1) "xX")
            (parse-integer body :start 2 :radix 16)
            (parse-integer body :start 1 :radix 10))
      (error () nil))))

(defun decode-entities (text)
  "TEXT with its entity and character references resolved."
  (if (not (find #\& text))
      text
      (with-output-to-string (out)
        (let ((index 0) (length (length text)))
          (loop while (< index length)
                do (let* ((character (char text index))
                          (end (and (char= character #\&)
                                    (position #\; text :start index :end (min length (+ index 12))))))
                     (if (null end)
                         (progn (write-char character out) (incf index))
                         (let* ((body (subseq text (1+ index) end))
                                (named (cdr (assoc body +entities+ :test #'string=)))
                                (code (and (not named) (character-reference body))))
                           (cond (named (write-string named out))
                                 ((and code (< 0 code) (< code #x110000) (not (<= #xD800 code #xDFFF)))
                                  (write-char (code-char code) out))
                                 (t (write-string text out :start index :end (1+ end))))
                           (setf index (1+ end))))))))))

;;; --- the reader ------------------------------------------------------------------

(defparameter +space-chars+ '(#\Space #\Tab #\Newline #\Return)
  "What XML calls markup-ignorable whitespace.")

(defun local-name (text)
  "TEXT's element name without its namespace prefix."
  (let ((colon (position #\: text :from-end t)))
    (if colon (subseq text (1+ colon)) text)))

(defun skip-blank (text index end)
  (loop while (and (< index end) (find (char text index) +space-chars+))
        do (incf index))
  index)

(defun read-attributes (text index end)
  "The attributes in TEXT between INDEX and END, as an alist."
  (let ((attributes '()))
    (loop
      (setf index (skip-blank text index end))
      (when (>= index end) (return (nreverse attributes)))
      (let* ((name-end (or (position-if (lambda (c) (or (find c +space-chars+) (char= c #\=)))
                                         text :start index :end end)
                            end))
             (name (local-name (subseq text index name-end)))
             (value "")
             (equals (skip-blank text name-end end)))
        (cond
          ((and (< equals end) (char= #\= (char text equals)))
           (let* ((start (skip-blank text (1+ equals) end))
                  (quote (and (< start end) (char text start))))
             (if (and quote (find quote '(#\" #\')))
                 (let* ((stop (or (position quote text :start (1+ start) :end end) end))
                        (raw (subseq text (1+ start) stop)))
                   (setf value (decode-entities raw)
                         index (min end (1+ stop))))
                 (let* ((stop (or (position-if (lambda (c) (find c +space-chars+))
                                               text :start start :end end)
                                  end))
                        (raw (subseq text start stop)))
                   (setf value (decode-entities raw)
                         index stop)))))
          (t (setf index (max (1+ name-end) (1+ index)))))
        (when (plusp (length name))
          (push (cons name value) attributes))))))

(defun skip-declaration (text index)
  "The index after the comment or declaration starting at INDEX."
  (if (and (>= (- (length text) index) 4) (string= "<!--" text :start2 index :end2 (+ index 4)))
      (let ((end (search "-->" text :start2 index)))
        (if end (+ end 3) (length text)))
      (let ((end (position #\> text :start index)))
        (if end (1+ end) (length text)))))

(defun parse-xml-elements (text &optional wanted)
  "Every top-level element in TEXT, in document order; with WANTED, only the
elements whose name it equals. Comments, processing instructions, a DOCTYPE
and the whitespace between documents are skipped."
  (let ((index 0) (length (length text)) (stack '()) (top '()))
    (labels ((collect (node)
               ;; A finished node goes into the frame around it, or to the top.
               (if stack
                   (push node (third (first stack)))
                   (push node top)))
             (finish (frame)
               (collect (%xnode (first frame) (second frame) (nreverse (third frame))))))
      (loop while (< index length)
            do (let ((character (char text index)))
                 (cond
                   ((char= character #\<)
                    (let ((next (and (< (1+ index) length) (char text (1+ index)))))
                      (cond
                        ((eql next #\!)
                         (setf index (skip-declaration text index)))
                        ((eql next #\?)
                         (let ((end (search "?>" text :start2 index)))
                           (setf index (if end (+ end 2) length))))
                        ((eql next #\/)
                         (let ((end (or (position #\> text :start index) (1- length))))
                           (when stack (finish (pop stack)))
                           (setf index (1+ end))))
                        (t
                         (let* ((end (or (position #\> text :start index) (1- length)))
                                (empty (and (> end (1+ index)) (char= #\/ (char text (1- end)))))
                                (inner (if empty (1- end) end))
                                (name-end (or (position-if (lambda (c) (find c +space-chars+))
                                                           text :start (1+ index) :end inner)
                                              inner))
                                (name (local-name (subseq text (1+ index) name-end)))
                                (attrs (read-attributes text name-end inner)))
                           (if empty
                               (collect (%xnode name attrs '()))
                               (push (list name attrs '()) stack))
                           (setf index (1+ end)))))))
                   (t
                    (let ((next (position #\< text :start index)))
                      ;; Text between documents (the newline wevtutil writes
                      ;; after every record) belongs to no element: a frame
                      ;; that is open is the only place text goes.
                      (when (and stack (> (or next length) index))
                        (push (decode-entities (subseq text index next))
                              (third (first stack))))
                      (setf index (or next length))))))))
    (let ((nodes (nreverse top)))
      (if wanted
          (remove-if-not (lambda (node) (name-equal (xnode-name node) wanted)) nodes)
          nodes))))
