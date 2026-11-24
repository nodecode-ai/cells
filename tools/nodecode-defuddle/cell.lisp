;;;; cell.lisp --- clean article markdown from a URL, by the defuddle program.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is the Hermes plugin annalinneajohansson82/hermes-defuddle: a
;;;; web-extract provider that registered itself with Hermes' web_extract tool,
;;;; which ran `defuddle parse --json URL' once per URL and read the JSON it
;;;; printed -- contentMarkdown, title, description, domain, site, language,
;;;; wordCount, author, published -- with stdin at /dev/null and a minute's
;;;; deadline. Hermes picked its extract backend from config; Nodecode ships a
;;;; websearch cell whose web:fetch reads a page itself, and has no extract
;;;; registry a cell may join, so the same reader is here a call the model
;;;; makes: (defuddle:extract URL), the defuddle_extract tool, and /defuddle
;;;; for the operator -- the same program on PATH, the same argv, the same
;;;; reading of its answer.
;;;;
;;;; Config, a top-level key of its own:
;;;;   "defuddle": {"program": "defuddle", "timeout_seconds": 60}

(in-package #:nodecode-defuddle)

;;; --- the words --------------------------------------------------------------------

(defparameter *primer*
  "defuddle reads a web page as a clean article: its text with navigation, sidebars and
ads stripped, as markdown, with the page's own title and metadata. It runs on this
machine, by the defuddle program (npm i -g defuddle): no key, no service, and the only
traffic is the page request itself. One call in the defuddle: package, through eval,
answers a string:
  (defuddle:extract \"https://example.org/an-article\")
      the title and the address, one line of what the page says about itself (site,
      language, words, byline, publish date) when it says it, the description, then
      the article. ERROR: DEFUDDLE-ERROR names why a page came back empty -- the
      program's own message, a page that is no article, one that needed JavaScript,
      or a read past timeout_seconds.
It reads a page; it does not find one: search with web:search (the websearch cell),
then read here the pages worth reading whole. The defuddle_extract tool is the same
call from the model's side, and /defuddle URL is the operator's own read."
  "What (help :defuddle) answers while the cell runs.")

(defparameter *untitled* "Untitled"
  "What a page with no title of its own is called in the answer.")

;;; --- the program ------------------------------------------------------------------

(defun program-path (name)
  "NAME as a program: the path itself when it names one, else the first file of
that name on PATH; NIL when there is none. A directory of that name is no
program."
  (when (and (stringp name) (plusp (length name)))
    (flet ((use (path)
             (when (and path (not (uiop:directory-pathname-p path)))
               (uiop:native-namestring path))))
      (if (or (find #\/ name) (find #\\ name))
          (use (ignore-errors (probe-file name)))
          (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                               :separator '(#\: #\;))
                for path = (ignore-errors
                             (probe-file
                              (merge-pathnames name
                                               (uiop:ensure-directory-pathname
                                                (if (plusp (length dir)) dir ".")))))
                thereis (use path))))))

(defvar *serial* 0
  "A counter that keeps one read's temporary files apart from the next's.")

(defun temporary (kind)
  "A pathname in the system's temporary directory that nothing else is using."
  (merge-pathnames (format nil "defuddle-~d-~d.~a"
                            (get-universal-time) (incf *serial*) kind)
                   (uiop:temporary-directory)))

(defun file-text (path)
  "PATH's contents as text, or \"\" when it holds none."
  (if (probe-file path)
      (handler-case (uiop:read-file-string path :external-format :utf-8)
        (error () ""))
      ""))

(defun program-run (program args timeout)
  "Run PROGRAM with ARGS and standard input at /dev/null, for at most TIMEOUT
seconds. => (values STDOUT STDERR EXIT-CODE TIMED-OUT-P). The streams are
read back whole from files, and a program past its deadline is killed, so a page
that never answers cannot hold the read forever."
  (let ((out (temporary "out")) (err (temporary "err")))
    (unwind-protect
         (let* ((process (sb-ext:run-program program args
                                              :search nil
                                              :input nil
                                              :output out
                                              :error err
                                              :if-output-exists :supersede
                                              :wait nil))
                (deadline (+ (get-internal-real-time)
                             (* timeout internal-time-units-per-second)))
                (timed-out nil))
           (loop
             (unless (sb-ext:process-alive-p process) (return))
             (when (>= (get-internal-real-time) deadline)
               (setf timed-out t)
               (ignore-errors (sb-ext:process-kill process 15))
               (sleep 0.1)
               (when (sb-ext:process-alive-p process)
                 (ignore-errors (sb-ext:process-kill process 9)))
               (return))
             (sleep 0.02))
           (sb-ext:process-wait process)
           (values (file-text out) (file-text err)
                   (ignore-errors (sb-ext:process-exit-code process))
                   timed-out))
      (ignore-errors (delete-file out))
      (ignore-errors (delete-file err)))))

(defvar *run* #'program-run
  "The one seam that runs the program: (PROGRAM ARGS TIMEOUT) => (values
STDOUT STDERR EXIT-CODE TIMED-OUT-P). Tests replace it; nothing else does.")

;;; --- what defuddle says ------------------------------------------------------------

(defun nonblank (text)
  "TEXT without its surrounding whitespace, or NIL when nothing is left."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun address (url)
  "URL as the address to read, checked to be http(s) before it reaches the
program: defuddle takes the address as its own argument, so anything else
could be read as one of its flags."
  (let ((url (nonblank url)))
    (unless url (fail "url must be a non-empty string, got ~s" url))
    (unless (or (uiop:string-prefix-p "http://" (string-downcase url))
                (uiop:string-prefix-p "https://" (string-downcase url)))
      (fail "url must be an http:// or https:// address, got ~s" url))
    url))

(defun article (url)
  "URL read by the program => a plist of :TITLE, :DESCRIPTION, :MARKDOWN and
the :METADATA the page carried. Signals DEFUDDLE-ERROR when it cannot be read,
as the plugin answered an error for the same reasons."
  (let* ((program (setting :program))
         (timeout (setting :timeout-seconds))
         (found (program-path program)))
    (unless found
      (fail "defuddle.program is ~s: no such program on PATH -- install it with \"npm i -g defuddle\", or give its path here"
            program))
    (multiple-value-bind (stdout stderr exit timed-out)
        (funcall *run* found (list "parse" "--json" url) timeout)
      (cond
        (timed-out
         (fail "defuddle did not finish within ~as reading ~a" timeout url))
        ((not (eql 0 exit))
         (fail "defuddle exited ~a reading ~a~@[: ~a~]"
               exit url (nonblank (nlk:one-line (or stderr "")))))
        ((null (nonblank stdout))
         (fail "defuddle printed nothing for ~a" url))
        (t
         ;; DECODE-JSON signals on anything that is no JSON at all, so the
         ;; answer is read under a guard and both cases refused the same way.
         (let ((data (handler-case (nlk:decode-json stdout) (error () nil))))
           (unless (hash-table-p data)
             (fail "defuddle's answer for ~a is not JSON: ~a" url
                   (nlk:clip (nlk:one-line stdout) 200)))
           (let ((markdown (nlk:json-value data :string "contentMarkdown")))
             ;; contentMarkdown is the clean markdown; the plugin refused to put
             ;; raw HTML in its place, and so does this, saying why.
             (unless (nonblank markdown)
               (fail "defuddle found no article in ~a: the page has no markdown content (it may be no article, or one that needed JavaScript)"
                     url))
             (list :title (or (nonblank (nlk:json-value data :string "title")) *untitled*)
                   :description (or (nonblank (nlk:json-value data :string "description")) "")
                   :markdown markdown
                   :metadata (list :site (nonblank (nlk:json-value data :string "site"))
                                   :domain (nonblank (nlk:json-value data :string "domain"))
                                   :language (nonblank (nlk:json-value data :string "language"))
                                   :words (nlk:json-value data :integer "wordCount")
                                   :author (nonblank (nlk:json-value data :string "author"))
                                   :published (nonblank (nlk:json-value data :string "published")))))))))))

;;; --- what the model reads ----------------------------------------------------------

(defun quoted (text)
  "TEXT as one blockquote: every line carries its own '> ', so a multi-line
description stays a blockquote, as the plugin wrote it."
  (with-output-to-string (out)
    (loop for line in (uiop:split-string text :separator '(#\Newline))
          for first = t then nil
          do (unless first (format out "~%"))
             (format out "> ~a" line))))

(defun metadata-line (metadata)
  "One line naming what the page says about itself, only the parts it says."
  (let ((parts (remove nil
                        (list (or (getf metadata :site) (getf metadata :domain))
                              (getf metadata :language)
                              (let ((words (getf metadata :words)))
                                (and (integerp words) (plusp words)
                                     (format nil "~:d words" words)))
                              (let ((author (getf metadata :author)))
                                (and author (format nil "by ~a" author)))
                              (let ((published (getf metadata :published)))
                                (and published (format nil "published ~a" published)))))))
    (when parts (format nil "~{~a~^ · ~}" parts))))

(defun rendered (url article max-chars)
  "URL's ARTICLE as the text the model reads, clipped at MAX-CHARS when that
is a positive number, with the cut said out loud."
  (let* ((title (getf article :title))
         (description (getf article :description))
         (text (with-output-to-string (out)
                 (format out "~a~%~a~%" title url)
                 (let ((line (metadata-line (getf article :metadata))))
                   (when line (format out "~a~%" line)))
                 (when (and (nonblank description) (not (equal description title)))
                   (format out "~%~a" (quoted description)))
                 (format out "~%~a" (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                                        (getf article :markdown))))))
    (if (and (integerp max-chars) (plusp max-chars) (> (length text) max-chars))
        (format nil "~a~%[defuddle: clipped at ~:d of ~:d characters; call again with max_chars 0 for all of it]"
                (subseq text 0 max-chars) max-chars (length text))
        text)))

;;; --- the verb --------------------------------------------------------------------

(define-verb extract (url &key max-chars)
  "Read URL as a clean article: its title and address, one line of metadata,
its description, then the article as markdown, navigation and ads stripped.
MAX-CHARS, when positive, clips the answer. Reads the page here, by the local
defuddle program; no key and no service. ERROR: DEFUDDLE-ERROR says why a page
could not be read."
  (let ((url (address url)))
    (rendered url (article url) max-chars)))

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract defuddle-article
  (:doc "Read a web page as a clean article: its text with navigation, sidebars and ads stripped, as markdown, with the page's own title, byline, publish date and word count. Use it when a page's plain text is too noisy to read and the article itself is what matters. The page is read here, by the local defuddle program: no key, no service.")
  ("url" :text :required t
         :doc "the page's address, http:// or https://")
  ("max_chars" :integer :default 0
               :doc "clip the answer at this many characters; 0, the default, keeps the whole article"))

(defun extract-tool (call)
  "The defuddle_extract tool: the page CALL names, as the article it carries."
  (extract (defuddle-article-url call)
           :max-chars (defuddle-article-max-chars call)))

;;; --- /defuddle --------------------------------------------------------------------

(defun run-slash (args session-id)
  "/defuddle URL: the operator's own read of a page, answered as the text the
model reads."
  (declare (ignore session-id))
  (let ((url (nlk:trimmed (or args ""))))
    (if (zerop (length url))
        (format nil "defuddle: type a URL -- /defuddle https://example.org/an-article. ~
                     The cell also gives the model (defuddle:extract ...) and the defuddle_extract tool.")
        (extract url))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell defuddle
  (:section ("defuddle")
    (:guide "no key and no service: the defuddle program (npm i -g defuddle) reads the page on this machine; name its path in program when it is not on PATH, and timeout_seconds bounds one read of a slow or hanging page")
    ("program" :string :default "defuddle"
               :doc "the defuddle program: its name on PATH, or a path to it")
    ("timeout_seconds" :integer :default 60 :min 1
                       :doc "how long one page may take to be read, in seconds"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((program (getf values :program)))
                 (unless (program-path program)
                   (nlk:config-error
                    "defuddle.program is ~s: no such program on PATH -- install it with \"npm i -g defuddle\", or give its path here"
                    program))
                 values)))
  (:tool "defuddle_extract" (nlk:contract 'defuddle-article) 'extract-tool)
  (:command "defuddle" 'run-slash
            :catches 'defuddle-error
            :description "Read a web page as a clean article"
            :argument-hint "URL")
  (:help :defuddle "defuddle: read a URL as clean article markdown by the local defuddle CLI - (defuddle:extract \"https://…\")"
         *primer*))
