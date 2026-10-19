;;;; markdown.lisp --- the Markdown a model writes, as an email's HTML part. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An answer goes out twice in one email (multipart/alternative): the
;;;; Markdown as typed, which every client can show, and MARKDOWN-HTML's
;;;; document, which most draw. Mail clients drop <style> blocks and classes
;;;; often enough that every element carries its look inline. Drawn:
;;;; headings, paragraphs whose line breaks stay, lists (nested by indent),
;;;; quotes, fenced code, tables, rules, and inline bold, italics, strike,
;;;; code and links (bare http/https addresses included). Every character
;;;; outside a tag is escaped, so nothing a model writes opens a tag, and a
;;;; link is drawn only for http, https and mailto addresses.

(in-package #:nodecode-channel-email)

(defparameter +html-styles+
  '((:body . "font-family:-apple-system,'Segoe UI',Helvetica,Arial,sans-serif;font-size:14px;line-height:1.5;color:#1f2328")
    (:p . "margin:0 0 12px")
    (:h . "margin:18px 0 8px;line-height:1.25")
    (:list . "margin:0 0 12px;padding-left:24px")
    (:pre . "margin:0 0 12px;padding:10px 12px;background:#f6f8fa;border-radius:6px;overflow-x:auto;font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:13px;line-height:1.45;white-space:pre")
    (:code . "padding:1px 4px;background:#eff1f3;border-radius:4px;font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:13px")
    (:quote . "margin:0 0 12px;padding:0 12px;border-left:3px solid #d0d7de;color:#59636e")
    (:table . "margin:0 0 12px;border-collapse:collapse")
    (:cell . "padding:4px 10px;border:1px solid #d0d7de;text-align:left;vertical-align:top")
    (:hr . "margin:16px 0;border:0;border-top:1px solid #d0d7de"))
  "The inline style each drawn element wears.")

(defun style (kind)
  (cdr (assoc kind +html-styles+)))

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
    ;; A bare address is a link too, unless it already sits in one.
    (setf html (ppcre:regex-replace-all
                "(?<![\"'>=/\\w])(https?://[^\\s<]*[^\\s<.,;:!?)\\]'\"*_~])" html
                "<a href=\"\\1\">\\1</a>"))
    (setf html (ppcre:regex-replace-all "\\*\\*(\\S(?:.*?\\S)?)\\*\\*" html "<strong>\\1</strong>"))
    (setf html (ppcre:regex-replace-all "(?<![\\w_])__(\\S(?:.*?\\S)?)__(?![\\w_])" html "<strong>\\1</strong>"))
    (setf html (ppcre:regex-replace-all "~~(\\S(?:.*?\\S)?)~~" html "<del>\\1</del>"))
    (setf html (ppcre:regex-replace-all "(?<![\\w*])\\*(?![\\s*])([^*]*?[^\\s*])\\*(?![\\w*])" html "<em>\\1</em>"))
    (ppcre:regex-replace-all "(?<![\\w_/])_(?![\\s_])([^_]*?[^\\s_])_(?![\\w_])" html "<em>\\1</em>")))

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
                    (format out "<code style=\"~a\">~a</code>" (style :code)
                            (html-escape (trim (subseq text run-end close))))
                    (setf at (+ close run)))
                  (progn (write-string (subseq text tick run-end) out)
                         (setf at run-end)))))))))

(defun blank-p (line)
  (every (lambda (ch) (member ch '(#\Space #\Tab))) line))

(defun indent-of (line)
  "LINE's leading columns, a tab counted as four."
  (loop for ch across line
        while (member ch '(#\Space #\Tab))
        sum (if (char= ch #\Tab) 4 1)))

(defun dedent (line columns)
  "LINE without up to COLUMNS of its leading blanks."
  (let ((at 0) (seen 0))
    (loop while (and (< at (length line)) (< seen columns) (member (char line at) '(#\Space #\Tab)))
          do (incf seen (if (char= (char line at) #\Tab) 4 1))
             (incf at))
    (subseq line at)))

(defun fence-open (line)
  "(values MARK LANGUAGE) when LINE opens a fenced code block, else NIL."
  (ppcre:register-groups-bind (mark language) ("^\\s{0,3}(`{3,}|~{3,})\\s*([^`\\s]*)" line)
    (values mark language)))

(defun list-item (line)
  "(values KIND START TEXT) when LINE is a list item at any indent: :UL, or
:OL and its number."
  ;; A COND, not an OR: OR passes on only its last form's values.
  (let ((bullet (nth-value 1 (ppcre:scan-to-strings "^\\s*[-*+]\\s+(.*)$" line)))
        (numbered (nth-value 1 (ppcre:scan-to-strings "^\\s*(\\d{1,9})[.)]\\s+(.*)$" line))))
    (cond (bullet (values :ul nil (aref bullet 0)))
          (numbered (values :ol (parse-integer (aref numbered 0)) (aref numbered 1))))))

(defun rule-p (line)
  (and (ppcre:scan "^\\s{0,3}([-*_])(\\s*\\1){2,}\\s*$" line) t))

(defun table-cells (line)
  "The cells of one table row, its outer pipes dropped."
  (let ((row (string-trim " " line)))
    (when (and (plusp (length row)) (char= #\| (char row 0))) (setf row (subseq row 1)))
    (when (and (plusp (length row)) (char= #\| (char row (1- (length row)))))
      (setf row (subseq row 0 (1- (length row)))))
    (mapcar #'trim (uiop:split-string row :separator "|"))))

(defun table-rule-p (line)
  (and (find #\| line) (find #\- line)
       (ppcre:scan "^\\s*\\|?\\s*:?-+:?\\s*(\\|\\s*:?-+:?\\s*)*\\|?\\s*$" line)
       t))

(defun block-start-p (line next)
  "Whether LINE opens a block of its own, ending the paragraph before it."
  (or (fence-open line)
      (ppcre:scan "^\\s{0,3}(#{1,6})\\s" line)
      (ppcre:scan "^\\s{0,3}>" line)
      (and (list-item line) (not (rule-p line)))
      (rule-p line)
      (and next (find #\| line) (table-rule-p next))))

(defun list-html (lines start out)
  "Draw the list that opens at LINES[START] to OUT: its items at the first
item's indent, each with the lines indented under it drawn inside it (a
nested list, a code block). => the index after the list."
  (let* ((base (indent-of (aref lines start)))
         (kind (list-item (aref lines start)))
         (number (nth-value 1 (list-item (aref lines start))))
         (index start)
         (tag (if (eq kind :ol) "ol" "ul")))
    (format out "<~a~@[ start=\"~d\"~] style=\"~a\">" tag (and number (/= number 1) number) (style :list))
    (loop while (< index (length lines))
          do (let ((line (aref lines index)))
               (multiple-value-bind (item-kind item-start words) (list-item line)
                 (declare (ignore item-start))
                 (unless (and item-kind (eq item-kind kind) (<= (indent-of line) (1+ base))
                              (not (rule-p line)))
                   (return))
                 (incf index)
                 ;; The item's own lines: anything indented past its marker,
                 ;; blank lines between them included when more follows.
                 (let ((inner '()))
                   (loop while (< index (length lines))
                         do (let ((body (aref lines index)))
                              (cond ((and (not (blank-p body)) (> (indent-of body) (1+ base)))
                                     (push (dedent body (+ base 2)) inner) (incf index))
                                    ((and (blank-p body) (< (1+ index) (length lines))
                                          (> (indent-of (aref lines (1+ index))) (1+ base))
                                          (not (blank-p (aref lines (1+ index)))))
                                     (push "" inner) (incf index))
                                    (t (return)))))
                   (let ((inner (reverse inner)))
                     (format out "<li>~a~@[~a~]</li>" (inline-html words)
                             (and inner (blocks-html (coerce inner 'vector) :tight t))))))))
    (format out "</~a>" tag)
    index))

(defun blocks-html (lines &key tight)
  "LINES, a vector of Markdown lines, as HTML blocks. TIGHT draws each
paragraph without a paragraph's margin, as inside a list item."
  (let ((index 0))
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
                          (format out "<pre style=\"~a\"><code~@[ class=\"language-~a\"~]>~a</code></pre>~%"
                                  (style :pre)
                                  (and (plusp (length language)) (html-escape language))
                                  (html-escape (format nil "~{~a~^~%~}" (reverse code)))))))
                     ((ppcre:scan "^\\s{0,3}(#{1,6})\\s" line)
                      (ppcre:register-groups-bind (hashes words) ("^\\s{0,3}(#{1,6})\\s+(.*?)\\s*#*\\s*$" line)
                        (format out "<h~d style=\"~a;font-size:~dpx\">~a</h~d>~%"
                                (length hashes) (style :h)
                                (nth (1- (length hashes)) '(22 19 17 15 14 13))
                                (inline-html words) (length hashes)))
                      (incf index))
                     ((rule-p line)
                      (format out "<hr style=\"~a\">~%" (style :hr))
                      (incf index))
                     ((ppcre:scan "^\\s{0,3}>" line)
                      (let ((quoted '()))
                        (loop for body = (line index)
                              while (and body (ppcre:scan "^\\s{0,3}>" body))
                              do (push (ppcre:regex-replace "^\\s{0,3}> ?" body "") quoted)
                                 (incf index))
                        (format out "<blockquote style=\"~a\">~a</blockquote>~%" (style :quote)
                                (blocks-html (coerce (reverse quoted) 'vector)))))
                     ((list-item line)
                      (setf index (list-html lines index out))
                      (terpri out))
                     ((and next (find #\| line) (table-rule-p next))
                      (format out "<table style=\"~a\"><thead><tr>~{<th style=\"~a\">~a</th>~}</tr></thead><tbody>"
                              (style :table)
                              (loop for cell in (table-cells line) append (list (style :cell) (inline-html cell))))
                      (incf index 2)
                      (loop for body = (line index)
                            while (and body (find #\| body) (not (blank-p body)))
                            do (format out "<tr>~{<td style=\"~a\">~a</td>~}</tr>"
                                       (loop for cell in (table-cells body)
                                             append (list (style :cell) (inline-html cell))))
                               (incf index))
                      (format out "</tbody></table>~%"))
                     (t
                      (let ((paragraph (list line)))
                        (incf index)
                        (loop for body = (line index)
                              while (and body (not (blank-p body))
                                         (not (block-start-p body (line (1+ index)))))
                              do (push body paragraph) (incf index))
                        (let ((words (format nil "~{~a~^<br>~%~}"
                                             (mapcar (lambda (l) (inline-html (trim l)))
                                                     (reverse paragraph)))))
                          (if tight
                              (format out "<div>~a</div>~%" words)
                              (format out "<p style=\"~a\">~a</p>~%" (style :p) words))))))))))))

(defun markdown-html (text)
  "TEXT, Markdown as a chat platform draws it, as a whole HTML document for
an email's text/html part."
  (format nil "<!DOCTYPE html>~%<html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width\"></head>~%<body style=\"~a\">~%~a</body></html>~%"
          (style :body)
          (blocks-html (coerce (uiop:split-string (remove #\Return (or text "")) :separator '(#\Newline))
                               'vector))))
