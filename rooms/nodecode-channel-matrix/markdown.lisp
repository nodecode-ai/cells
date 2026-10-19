;;;; markdown.lisp --- the Markdown a model writes, as Matrix's HTML. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Matrix message carries its words twice: `body', the plain text every
;;;; client can show, and `formatted_body' in the org.matrix.custom.html
;;;; format, the HTML subset the spec lets a client draw (spec.matrix.org,
;;;; Client-Server API, m.room.message msgtypes). The model writes the
;;;; Markdown Discord draws; MATRIX-HTML says the same in that subset: headings,
;;;; paragraphs whose line breaks stay, lists, quotes, fenced code, tables,
;;;; rules, and inline bold, italics, strike, code and links. Every character
;;;; outside a tag is escaped, so nothing a model writes opens a tag; a link
;;;; is drawn only for http, https and mailto addresses.

(in-package #:nodecode-channel-matrix)

(defun html-escape (text)
  "TEXT with the four characters HTML reads as markup spelled as entities."
  (with-output-to-string (out)
    (loop for ch across text
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char ch out))))))

(defun inline-plain (text)
  "TEXT, a run outside code spans, escaped and with its emphasis and links
 drawn."
  (let ((html (html-escape text)))
    (setf html (ppcre:regex-replace-all
                "!?\\[([^\\]]+)\\]\\(((?:https?://|mailto:)[^)\\s]+)\\)" html
                "<a href=\"\\2\">\\1</a>"))
    (setf html (ppcre:regex-replace-all "\\*\\*(\\S(?:.*?\\S)?)\\*\\*" html "<strong>\\1</strong>"))
    (setf html (ppcre:regex-replace-all "(?<![\\w_])__(\\S(?:.*?\\S)?)__(?![\\w_])" html "<strong>\\1</strong>"))
    (setf html (ppcre:regex-replace-all "~~(\\S(?:.*?\\S)?)~~" html "<del>\\1</del>"))
    (setf html (ppcre:regex-replace-all "(?<![\\w*])\\*(?![\\s*])([^*]*?[^\\s*])\\*(?![\\w*])" html "<em>\\1</em>"))
    (ppcre:regex-replace-all "(?<![\\w_])_(?![\\s_])([^_]*?[^\\s_])_(?![\\w_])" html "<em>\\1</em>")))

(defun inline-html (text &aux (size (length text)))
  "One line's inline Markdown as HTML: code spans as written, escaped; the
runs between them through INLINE-PLAIN."
  (with-output-to-string (out)
    (do ((at 0)) ((>= at size))
      (let ((tick (position #\` text :start at)))
        (if (null tick)
            (progn (write-string (inline-plain (subseq text at)) out)
                   (setf at size))
            (let* ((run-end (or (position #\` text :start tick :test-not #'char=) size))
                   (run (- run-end tick))
                   (close (search (make-string run :initial-element #\`) text :start2 run-end)))
              (write-string (inline-plain (subseq text at tick)) out)
              (if (and close (or (= (+ close run) size) (char/= #\` (char text (+ close run)))))
                  (progn
                    (format out "<code>~a</code>" (html-escape (nlk:trimmed (subseq text run-end close))))
                    (setf at (+ close run)))
                  (progn (write-string (subseq text tick run-end) out)
                         (setf at run-end)))))))))

(defun blank-p (line)
  (every (lambda (ch) (member ch '(#\Space #\Tab))) line))

(defun fence-open (line)
  "(values MARK LANGUAGE) when LINE opens a fenced code block, else NIL."
  (ppcre:register-groups-bind (mark language) ("^\\s{0,3}(`{3,}|~{3,})\\s*([^`\\s]*)" line)
    (values mark language)))

(defun list-item (line)
  "(values KIND START TEXT) when LINE is a list item: :UL, or :OL and its
number."
  ;; A COND, not an OR: OR passes on only its last form's values.
  (let ((bullet (nth-value 1 (ppcre:scan-to-strings "^\\s{0,3}[-*+]\\s+(.*)$" line)))
        (numbered (nth-value 1 (ppcre:scan-to-strings "^\\s{0,3}(\\d{1,9})[.)]\\s+(.*)$" line))))
    (cond (bullet (values :ul nil (aref bullet 0)))
          (numbered (values :ol (parse-integer (aref numbered 0)) (aref numbered 1))))))

(defun table-cells (line)
  "The cells of one table row, its outer pipes dropped."
  (let ((row (string-trim " " line)))
    (when (and (plusp (length row)) (char= #\| (char row 0))) (setf row (subseq row 1)))
    (when (and (plusp (length row)) (char= #\| (char row (1- (length row)))))
      (setf row (subseq row 0 (1- (length row)))))
    (mapcar #'nlk:trimmed (uiop:split-string row :separator "|"))))

(defun table-rule-p (line)
  (and (find #\| line) (find #\- line)
       (ppcre:scan "^\\s*\\|?\\s*:?-+:?\\s*(\\|\\s*:?-+:?\\s*)*\\|?\\s*$" line)
       t))

(defun block-start-p (line next)
  "Whether LINE opens a block of its own, ending the paragraph before it."
  (or (fence-open line)
      (ppcre:scan "^\\s{0,3}(#{1,6})\\s" line)
      (ppcre:scan "^\\s{0,3}>" line)
      (list-item line)
      (ppcre:scan "^\\s{0,3}([-*_])(\\s*\\1){2,}\\s*$" line)
      (and next (find #\| line) (table-rule-p next))))

(defun matrix-html (text)
  "TEXT, Markdown as a chat platform draws it, as org.matrix.custom.html."
  (let ((lines (coerce (uiop:split-string (remove #\Return (or text "")) :separator '(#\Newline))
                       'vector))
        (index 0))
    (flet ((line (i) (and (< i (length lines)) (aref lines i))))
      (with-output-to-string (out)
        (loop while (< index (length lines))
              do (let ((line (line index))
                       (next (line (1+ index))))
                   (cond
                     ((blank-p line) (incf index))
                     ((fence-open line)
                      (multiple-value-bind (mark language) (fence-open line)
                        (let ((code '()))
                          (incf index)
                          (loop for body = (line index)
                                while (and body
                                           (not (ppcre:scan (format nil "^\\s{0,3}~a~a*\\s*$"
                                                                    (ppcre:quote-meta-chars mark)
                                                                    (ppcre:quote-meta-chars (subseq mark 0 1)))
                                                            body)))
                                do (push body code) (incf index))
                          (incf index)
                          (format out "<pre><code~@[ class=\"language-~a\"~]>~a</code></pre>"
                                  (and (plusp (length language)) (html-escape language))
                                  (html-escape (format nil "~{~a~^~%~}" (reverse code)))))))
                     ((ppcre:scan "^\\s{0,3}(#{1,6})\\s" line)
                      (ppcre:register-groups-bind (hashes words) ("^\\s{0,3}(#{1,6})\\s+(.*?)\\s*#*\\s*$" line)
                        (format out "<h~d>~a</h~2:*~d>" (length hashes) (inline-html words)))
                      (incf index))
                     ((ppcre:scan "^\\s{0,3}([-*_])(\\s*\\1){2,}\\s*$" line)
                      (write-string "<hr>" out)
                      (incf index))
                     ((ppcre:scan "^\\s{0,3}>" line)
                      (let ((quoted '()))
                        (loop for body = (line index)
                              while (and body (ppcre:scan "^\\s{0,3}>" body))
                              do (push (ppcre:regex-replace "^\\s{0,3}> ?" body "") quoted)
                                 (incf index))
                        (format out "<blockquote>~a</blockquote>"
                                (matrix-html (format nil "~{~a~^~%~}" (reverse quoted))))))
                     ((list-item line)
                      (multiple-value-bind (kind start) (list-item line)
                        (format out (if (and (eq kind :ol) start (/= start 1))
                                        "<ol start=\"~d\">" "<~*~a>")
                                start (if (eq kind :ol) "ol" "ul"))
                        (loop for body = (line index)
                              while body
                              do (multiple-value-bind (item-kind item-start words) (list-item body)
                                   (declare (ignore item-start))
                                   (unless (eq item-kind kind) (return))
                                   (format out "<li>~a</li>" (inline-html words))
                                   (incf index)))
                        (format out "</~a>" (if (eq kind :ol) "ol" "ul"))))
                     ((and next (find #\| line) (table-rule-p next))
                      (format out "<table><thead><tr>~{<th>~a</th>~}</tr></thead><tbody>"
                              (mapcar #'inline-html (table-cells line)))
                      (incf index 2)
                      (loop for body = (line index)
                            while (and body (find #\| body) (not (blank-p body)))
                            do (format out "<tr>~{<td>~a</td>~}</tr>" (mapcar #'inline-html (table-cells body)))
                               (incf index))
                      (write-string "</tbody></table>" out))
                     (t
                      (let ((paragraph (list line)))
                        (incf index)
                        (loop for body = (line index)
                              while (and body (not (blank-p body))
                                         (not (block-start-p body (line (1+ index)))))
                              do (push body paragraph) (incf index))
                        (format out "<p>~{~a~^<br>~}</p>"
                                (mapcar (lambda (l) (inline-html (string-right-trim " " l)))
                                        (reverse paragraph))))))))))))
