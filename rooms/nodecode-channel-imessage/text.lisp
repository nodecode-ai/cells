;;;; text.lisp --- Markdown as the plain text Messages shows. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Messages draws no Markdown: a bubble shows its characters as sent, and
;;;; finds links in them on its own. The model writes Markdown, so an answer is
;;;; spelled as a person would type it: emphasis marks dropped, a heading its
;;;; words, a list item a bullet, a link its words and its address, a fenced
;;;; block its lines as they are.

(in-package #:nodecode-channel-imessage)

(defun replace-all (regex text replacement)
  "TEXT with every match of REGEX replaced: REPLACEMENT a string (with \\N
registers) or a function of the match and its registers."
  (if (functionp replacement)
      (ppcre:regex-replace-all regex text replacement :simple-calls t)
      (ppcre:regex-replace-all regex text replacement)))

(defun link-plain (text url)
  "A link's words and its address as a person types them: the address alone
when the words are the address."
  (let ((bare (ppcre:regex-replace "^(mailto:|tel:)" url "")))
    (if (or (string= text url) (string= text bare) (zerop (length (nlk:trimmed text))))
        bare
        (format nil "~a (~a)" text bare))))

(defun emphasis-plain (text)
  "TEXT outside code with its links spelled out and its emphasis marks dropped."
  (let ((s text))
    (setf s (replace-all "!\\[([^\\]]*)\\]\\(\\s*<?([^)\\s>]+)>?(?:\\s+\"[^\"]*\")?\\s*\\)" s
                         (lambda (match alt url) (declare (ignore match alt)) url)))
    (setf s (replace-all "\\[([^\\]]+)\\]\\(\\s*<?([^)\\s>]+)>?(?:\\s+\"[^\"]*\")?\\s*\\)" s
                         (lambda (match words url) (declare (ignore match)) (link-plain words url))))
    (setf s (replace-all "<((?:https?|mailto):[^>\\s]+)>" s "\\1"))
    (setf s (replace-all "\\*\\*(.+?)\\*\\*" s "\\1"))
    (setf s (replace-all "(?<![\\w])__(.+?)__(?![\\w])" s "\\1"))
    (setf s (replace-all "~~(.+?)~~" s "\\1"))
    (setf s (replace-all "(?<![\\w*])\\*(?![\\s*])(.+?)(?<![\\s*])\\*(?![\\w*])" s "\\1"))
    (setf s (replace-all "(?<![\\w_])_(?![\\s_])(.+?)(?<![\\s_])_(?![\\w_])" s "\\1"))
    s))

(defun inline-plain (line)
  "One LINE's inline Markdown spelled plain: code spans kept as they are,
without their backticks, everything between them EMPHASIS-PLAIN."
  (let ((parts (ppcre:split "`" line :with-registers-p nil :omit-unmatched-p nil :sharedp nil)))
    (if (evenp (count #\` line))
        (format nil "~{~a~}" (loop for part in parts
                                   for code-p = nil then (not code-p)
                                   collect (if code-p part (emphasis-plain part))))
        (emphasis-plain line))))

(defun block-line-plain (line)
  "One LINE outside a fence: a heading its words, a list item a bullet, a
rule a dash, a table's separator row nothing (NIL), its inline marks plain."
  (cond ((ppcre:scan "^\\s{0,3}([-*_])(\\s*\\1){2,}\\s*$" line) (string (code-char #x2014)))
        ((and (find #\| line) (ppcre:scan "^\\s*\\|?\\s*:?-{3,}:?\\s*(\\|\\s*:?-{3,}:?\\s*)*\\|?\\s*$" line)) nil)
        (t
         (let ((line (replace-all "^\\s{0,3}#{1,6}\\s+(.*?)\\s*#*\\s*$" line "\\1")))
           (setf line (replace-all "^(\\s*)[-*+]\\s+(\\[[ xX]\\]\\s+)?" line
                                   (lambda (match indent box)
                                     (declare (ignore match))
                                     (format nil "~a~a " indent
                                             (cond ((null box) (string (code-char #x2022)))
                                                   ((find #\x box :test #'char-equal) (string (code-char #x2611)))
                                                   (t (string (code-char #x2610))))))))
           (inline-plain line)))))

(defun markdown-plain (text)
  "TEXT, Markdown, as the plain text a Messages bubble shows."
  (let ((lines '()) (fence nil))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((opener (nth-value 1 (ppcre:scan-to-strings "^\\s*(`{3,}|~{3,})" line))))
        (cond ((and opener (null fence)) (setf fence (subseq (aref opener 0) 0 3)))
              ((and opener fence (uiop:string-prefix-p fence (aref opener 0))) (setf fence nil))
              (fence (push line lines))
              (t (nlk:when-let (plain (block-line-plain line)) (push plain lines))))))
    (nlk:trimmed (replace-all "\\n{3,}" (format nil "~{~a~^~%~}" (nreverse lines))
                              (format nil "~%~%")))))
