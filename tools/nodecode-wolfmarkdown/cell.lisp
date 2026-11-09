;;;; cell.lisp --- three tools, one command, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of WolfMarkTools/WolfMarkDown, which was a Hermes
;;;; Agent skill plus a Node toolchain: doctor, install, scaffold, format and
;;;; verify scripts over markdownlint, Prettier and a GFM parser. The rewrite
;;;; keeps what it gave a user -- a deterministic answer to "is this markdown
;;;; file clean, and did my edit lose anything the source carried?" -- and
;;;; drops the runtime: the checks are Lisp over the text, so there is no
;;;; package to install, no node to keep current, and nothing to fail behind
;;;; a closed network.
;;;;
;;;; Three tools, because a document is checked in three ways: against its
;;;; own shape (markdown_verify), cleaned into the canonical form
;;;; (markdown_clean, which refuses to write when the source's protected
;;;; tokens would not survive), and taken apart for a rewrite to plan from
;;;; (markdown_outline, the source map the original called a scaffold).

(in-package #:nodecode-wolfmarkdown)

(defun max-issues ()
  "How many findings a report prints, from the section when the cell runs."
  (or (ignore-errors (setting :max-issues)) 20))

(defun write-by-default-p ()
  "Whether markdown_clean writes the file when the call does not say."
  (let ((value (ignore-errors (setting :write-by-default))))
    (and value t)))

(defun resolve-path (path)
  "PATH as a pathname, a leading ~ read as the home directory."
  (let ((name (if (and (plusp (length path)) (char= (char path 0) #\~))
                  (concatenate 'string (or (uiop:getenv "HOME") "") (subseq path 1))
                  path)))
    (uiop:parse-native-namestring name)))

(defun file-text (path)
  "What PATH holds, or a refusal naming it."
  (let ((truename (resolve-path path)))
    (unless (probe-file truename)
      (fail "~a is not there" path))
    (uiop:read-file-string truename)))

(defun text-of (path text)
  "(VALUES TEXT LABEL): the document a call is about, from TEXT when it
carries one and from the file PATH otherwise; LABEL names it in a report."
  (let ((path (and path (nlk:trimmed path)))
        (text (or text "")))
    (cond ((and path (plusp (length path))) (values (file-text path) path))
          ((plusp (length (nlk:trimmed text))) (values text nil))
          (t (fail "give path (a .md file) or text (the Markdown itself)")))))

(defun report-tail (report)
  "REPORT from its Result line on, so a summary can stand above it."
  (let ((at (search "Result:" report)))
    (if at (subseq report at) report)))

;;; --- the arguments of the three tools ------------------------------------------

(nlk:define-contract verify-arguments
  (:doc "Check a Markdown document without changing it. Answers a report: PASS or FAIL, one line per check, then every finding with the line it is on and how to fix it. Given source -- the untouched snapshot the document was rewritten from -- it also proves content integrity: the URLs, versions, hashes, dates, paths, environment names and code spans the source carried, the ordered items that must keep their positions, and the ledger records that must stay on their own lines. A PASS is Markdown quality proven against that source; it is not a claim that the source is true.")
  ("path" :string :default "" :doc "the .md file to check")
  ("text" :string :default "" :doc "the Markdown itself, when it is not in a file")
  ("source" :string :default "" :doc "the untouched snapshot to prove content integrity against"))

(nlk:define-contract clean-arguments
  (:doc "Rewrite a Markdown document into the cell's canonical form: no trailing whitespace but a two-space hard break, one blank line at most, a blank line around headings, fences and lists, one unordered marker throughout, a dot after every ordered one, a space between adjacent numeric citations, and exactly one newline at the end. Fenced code is kept byte for byte. With source, the cleaned text is checked against the source's protected tokens first, and nothing is written when one of them would be lost. Answers the change summary, then the verification of the result.")
  ("path" :string :default "" :doc "the .md file to clean")
  ("text" :string :default "" :doc "the Markdown itself, when it is not in a file")
  ("source" :string :default "" :doc "the untouched snapshot, so a loss is caught before anything is written")
  ("write_file" :boolean :default nil :doc "write the result to path; without it nothing on disk moves"))

(nlk:define-contract outline-arguments
  (:doc "Take a Markdown document apart for a rewrite to plan from: its headings in order, its fenced blocks, its table lines, its ordered lists, its ledger record runs, and the protected tokens of every class with examples. Answers the source map; it changes nothing and judges nothing.")
  ("path" :string :default "" :doc "the .md file to outline")
  ("text" :string :default "" :doc "the Markdown itself, when it is not in a file"))

;;; --- the tools --------------------------------------------------------------------

(defun run-verify (arguments)
  "The markdown_verify tool: the document checked, and its source too when
one was given."
  (let ((source (nlk:trimmed (verify-arguments-source arguments))))
    (multiple-value-bind (text label)
        (text-of (verify-arguments-path arguments) (verify-arguments-text arguments))
      (verify-text text
                   :source (and (plusp (length source)) (file-text source))
                   :path label
                   :max-issues (max-issues)))))

(defun changed-line-count (before after)
  "How many lines of BEFORE differ from AFTER, position by position."
  (let ((left (lines-of before))
        (right (lines-of after)))
    (loop for index from 0 below (max (length left) (length right))
          count (not (equal (and (< index (length left)) (aref left index))
                            (and (< index (length right)) (aref right index)))))))

(defun lost-tokens (source cleaned)
  "The source tokens CLEANED no longer carries, each (TOKEN CLASS)."
  (multiple-value-bind (missing mutated decreased)
      (compare-tokens (token-index source) (token-index cleaned))
    (append missing
            (mapcar (lambda (entry) (list (first entry) (third entry))) mutated)
            (mapcar (lambda (entry) (list (first entry) (second entry))) decreased))))

(defun run-clean (arguments)
  "The markdown_clean tool: the document in canonical form, written only
when the source's protected tokens survive it."
  (let* ((path (nlk:trimmed (clean-arguments-path arguments)))
         (source (nlk:trimmed (clean-arguments-source arguments)))
         (write-p (or (clean-arguments-write-file arguments) (write-by-default-p))))
    (multiple-value-bind (text label)
        (text-of path (clean-arguments-text arguments))
      (multiple-value-bind (cleaned fixes) (clean-text text)
        (let* ((source-text (and (plusp (length source)) (file-text source)))
               (lost (and source-text (lost-tokens source-text cleaned)))
               (wrote (and write-p (plusp (length path)) (not lost))))
          (when wrote
            (nlk:write-file-atomically (resolve-path path) cleaned :parents t))
          (multiple-value-bind (report pass) (verify-text cleaned :path label :max-issues (max-issues))
            (declare (ignore pass))
            (with-output-to-string (out)
              (format out "WolfMarkDown~%Operation: Clean~@[~%File: ~a~]~%" label)
              (format out "Lines changed: ~d~%" (changed-line-count text cleaned))
              (format out "Citations separated: ~d~%" fixes)
              (format out "Content integrity: ~a~%"
                      (cond ((null source-text) "Skipped")
                            (lost "Fail")
                            (t "Pass")))
              (dolist (entry lost)
                (format out "- lost from the source: ~(~a~) ~a~%" (second entry) (first entry)))
              (format out "Wrote: ~a~%"
                      (cond (wrote path)
                            (lost "no (the source's tokens would not survive)")
                            (t "no (say write_file true to put it on disk)")))
              (format out "~a" (report-tail report))
              (unless (or wrote lost)
                (format out "~%~%~a" cleaned)))))))))

(defun run-outline (arguments)
  "The markdown_outline tool: the document's source map."
  (multiple-value-bind (text label)
      (text-of (outline-arguments-path arguments) (outline-arguments-text arguments))
    (inventory-text text label)))

;;; --- /markdown -----------------------------------------------------------------

(defparameter +manual+
  "markdown: checks, cleans and proves Markdown files without leaving the
image. markdown_verify, markdown_clean and markdown_outline are the tools;
/markdown verify PATH, /markdown clean PATH and /markdown outline PATH are
the same three from the command line, and /markdown PATH checks that file.
A check reads the document's own shape: an unclosed fence, a skipped heading
level, a table without a delimiter row, trailing whitespace, more than one
blank line, a list marker the document does not use throughout, an adjacent
citation pair ([6][26], which GFM reads as one link), and whether the file
is already in the cell's canonical form and formats to itself. Given the
untouched snapshot as source, it also proves content integrity: every URL,
version, hash, date, percent, money amount, path, environment name and code
span the source carried is still there, no value changed into another of its
class, every ordered item kept its position, and every ledger record came
through on a line of its own in order. Clean writes the canonical form, and
with a source it writes nothing when a protected token would be lost.
A PASS is Markdown quality, never a claim that the content is true."
  "What (help :markdown) answers while the cell runs.")

(defun verify-file (path)
  "PATH checked, as /markdown verify wants it."
  (when (zerop (length path))
    (fail "name a file: /markdown verify PATH"))
  (verify-text (file-text path) :path path :max-issues (max-issues)))

(defun clean-file (path)
  "PATH cleaned and written, then verified; as /markdown clean wants it."
  (when (zerop (length path))
    (fail "name a file: /markdown clean PATH"))
  (multiple-value-bind (cleaned fixes) (clean-text (file-text path))
    (declare (ignore fixes))
    (nlk:write-file-atomically (resolve-path path) cleaned :parents t)
    (multiple-value-bind (report pass) (verify-text cleaned :path path :max-issues (max-issues))
      (declare (ignore pass))
      (format nil "~a: written in canonical form.~%~a" path (report-tail report)))))

(defun outline-file (path)
  "PATH taken apart, as /markdown outline wants it."
  (when (zerop (length path))
    (fail "name a file: /markdown outline PATH"))
  (inventory-text (file-text path) path))

(defun standby-text ()
  "What /markdown says on its own: the settings in force and the words to use."
  (format nil "markdown: verify, clean and prove .md files~%
               (list marker ~s, ~d findings printed, ~:[clean does not write~;clean writes~])~%
               /markdown verify PATH  · /markdown clean PATH  · /markdown outline PATH~%
               /markdown PATH checks that file. The tools markdown_verify, markdown_clean and~%
               markdown_outline carry the same three; pass source to prove content integrity~%
               against an untouched snapshot. A PASS is Markdown quality, not factual truth."
          (list-style) (max-issues) (write-by-default-p)))

(defun run-slash (args session-id)
  "/markdown: bare, the settings and the words; verify, clean or outline,
the same verb on the file named after it; any other text, the file to
check. => the answer's text."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (verb (string-downcase (or (first words) "")))
         (rest (nlk:trimmed (format nil "~{~a~^ ~}" (rest words)))))
    (cond ((null words) (standby-text))
          ((string= verb "verify") (verify-file rest))
          ((string= verb "clean") (clean-file rest))
          ((string= verb "outline") (outline-file rest))
          (t (verify-file (nlk:trimmed (or args "")))))))

;;; --- the declaration ---------------------------------------------------------------

(defvar *ready* nil "Whether this process has the cell started: the one thing its start installs, so every contribution has an inverse.")

(nle:define-cell wolfmarkdown
  (:section ("wolfmarkdown")
    (:guide "nothing here is required: the checks read the document, and a PASS is Markdown quality, not factual truth. list_style is the unordered marker clean writes; max_issues caps how many findings one report prints; write_by_default is whether markdown_clean touches the file without being told to")
    ("list_style" :choice :options '("-" "*" "+") :default "-"
                  :doc "the unordered list marker markdown_clean writes and markdown_verify expects")
    ("max_issues" :integer :default 20 :min 1
                  :doc "how many findings one report prints before it counts the rest")
    ("write_by_default" :boolean :default nil
                         :doc "whether markdown_clean writes the file when the call does not say"))
  (:start (lambda ()
            (setf *ready* t)
            (nle:on-stop (lambda () (setf *ready* nil)))))
  (:tool "markdown_verify" (nlk:contract 'verify-arguments) 'run-verify)
  (:tool "markdown_clean" (nlk:contract 'clean-arguments) 'run-clean)
  (:tool "markdown_outline" (nlk:contract 'outline-arguments) 'run-outline)
  (:command "markdown" 'run-slash
            :description "Verify, clean or outline a Markdown file"
            :argument-hint "verify PATH | clean PATH | outline PATH | PATH")
  (:help :markdown "markdown: verify, clean and prove .md files; /markdown PATH checks one"
         +manual+))
