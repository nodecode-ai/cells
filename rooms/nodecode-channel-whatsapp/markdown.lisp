;;;; markdown.lisp --- Markdown as WhatsApp spells it. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WhatsApp formats with its own marks: *bold*, _italic_, ~strikethrough~,
;;;; `inline code`, ```monospace``` blocks, "> " quotes, "- " and "1. "
;;;; lists. The model writes Markdown, so an answer is respelled before it
;;;; posts: **bold** and __bold__ become *bold*, a single *italic* becomes
;;;; _italic_, ~~struck~~ becomes ~struck~, a heading becomes a bold line, a
;;;; link its words with the address beside them, a table a monospace block
;;;; so its columns stay lined up. Code keeps its characters: nothing inside
;;;; a fence or a backtick span is touched.

(in-package #:nodecode-channel-whatsapp)

(defparameter +bold-mark+ (code-char #xE000)
  "A private-use character standing for a bold mark while italics are
 respelled, so a bold's * is never read as an italic's.")

(defun respell-inline (text)
  "TEXT's inline Markdown in WhatsApp's marks, code spans kept as written."
  (with-output-to-string (out)
    (loop with start = 0
          for tick = (position #\` text :start start)
          for close = (and tick (position #\` text :start (1+ tick)))
          do (write-string (respell-prose (subseq text start (if close tick (length text)))) out)
             (unless close (return))
             (write-string text out :start tick :end (1+ close))
             (setf start (1+ close)))))

(defun respell-prose (text &aux (bold (string +bold-mark+)))
  "TEXT, with no code in it, in WhatsApp's marks."
  (let ((s text))
    ;; A backslash escape is the character itself, kept out of every rule.
    (setf s (ppcre:regex-replace-all "\\\\([*_~`#>\\[\\]()|-])" s
                                     (lambda (match char)
                                       (declare (ignore match))
                                       (string (code-char (+ #xF0000 (char-code (char char 0))))))
                                     :simple-calls t))
    ;; Images and links first: their brackets hold words to respell too.
    (setf s (ppcre:regex-replace-all "!\\[([^\\]]*)\\]\\(([^)\\s]+)[^)]*\\)" s
                                     (lambda (match alt url)
                                       (declare (ignore match))
                                       (if (plusp (length alt)) (format nil "~a (~a)" alt url) url))
                                     :simple-calls t))
    (setf s (ppcre:regex-replace-all "\\[([^\\]]+)\\]\\(([^)\\s]+)[^)]*\\)" s
                                     (lambda (match words url)
                                       (declare (ignore match))
                                       (if (or (string= words url)
                                               (string= words (ppcre:regex-replace "^https?://" url "")))
                                           url
                                           (format nil "~a (~a)" words url)))
                                     :simple-calls t))
    (setf s (ppcre:regex-replace-all "<(https?://[^>\\s]+)>" s "\\1"))
    ;; Bold, under a placeholder, then italics, then the placeholder as *.
    (setf s (ppcre:regex-replace-all "\\*\\*(\\S(?:.*?\\S)?)\\*\\*" s (format nil "~a\\1~a" bold bold)))
    (setf s (ppcre:regex-replace-all "(?<![\\w])__(\\S(?:.*?\\S)?)__(?![\\w])" s (format nil "~a\\1~a" bold bold)))
    (setf s (ppcre:regex-replace-all "(?<![\\w*])\\*(\\S(?:[^*]*?\\S)?)\\*(?![\\w*])" s "_\\1_"))
    (setf s (ppcre:regex-replace-all "~~(\\S(?:.*?\\S)?)~~" s "~\\1~"))
    (setf s (substitute #\* +bold-mark+ s))
    (map 'string (lambda (char)
                   (if (<= #xF0000 (char-code char) #xF00FF)
                       (code-char (- (char-code char) #xF0000))
                       char))
         s)))

(defun table-line-p (line)
  (let ((trim (nlk:trimmed line)))
    (and (> (length trim) 1) (char= #\| (char trim 0)))))

(defun table-separator-p (line)
  (ppcre:scan "^\\s*\\|?\\s*:?-{2,}:?\\s*(\\|\\s*:?-{2,}:?\\s*)*\\|?\\s*$" line))

(defun table-block (lines)
  "A Markdown table's LINES as a monospace block: its cells padded into
columns, the separator row a rule."
  (let* ((rows (loop for line in lines
                     unless (table-separator-p line)
                       collect (mapcar #'nlk:trimmed
                                       (ppcre:split "\\|" (string-trim "| " (nlk:trimmed line))))))
         (columns (reduce #'max rows :key #'length :initial-value 0))
         (widths (loop for column below columns
                       collect (reduce #'max rows
                                       :key (lambda (row) (length (or (nth column row) "")))
                                       :initial-value 0))))
    (with-output-to-string (out)
      (format out "```~%")
      (loop for row in rows
            for index from 0
            do (format out "~{~a~^  ~}~%"
                       (loop for width in widths
                             for column from 0
                             collect (let ((cell (or (nth column row) "")))
                                       (format nil "~va" width cell))))
               (when (and (zerop index) (rest rows))
                 (format out "~{~a~^  ~}~%"
                         (loop for width in widths collect (make-string width :initial-element #\-)))))
      (format out "```"))))

(defun whatsapp-text (markdown)
  "MARKDOWN as a WhatsApp message spells it."
  (let ((lines (uiop:split-string (or markdown "") :separator '(#\Newline)))
        (out '())
        (fence nil))
    (loop while lines
          for line = (pop lines)
          do (cond
               ;; A fence opens and closes as itself; its language is dropped,
               ;; since WhatsApp would show the word as the first line of code.
               ((ppcre:scan "^\\s*```" line)
                (push (if fence "```" "```") out)
                (setf fence (not fence)))
               (fence (push line out))
               ((table-line-p line)
                (let ((table (list line)))
                  (loop while (and lines (table-line-p (first lines)))
                        do (push (pop lines) table))
                  (push (table-block (nreverse table)) out)))
               ((ppcre:scan "^\\s*#{1,6}\\s+" line)
                (let ((title (nlk:trimmed (ppcre:regex-replace "^\\s*#{1,6}\\s+" line ""))))
                  (setf title (ppcre:regex-replace-all "\\*\\*" title ""))
                  (push (format nil "*~a*" (respell-inline (string-right-trim "#" title))) out)))
               ((ppcre:scan "^\\s*([-*_])\\s*(\\1\\s*){2,}$" line)
                (push "───" out))
               (t
                ;; A list item WhatsApp draws as one: "- ", keeping its indent.
                (let ((line (ppcre:regex-replace "^(\\s*)[*+]\\s+" line "\\1- ")))
                  (push (respell-inline line) out)))))
    (when fence (push "```" out))
    (format nil "~{~a~^~%~}" (nreverse out))))
