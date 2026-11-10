;;;; cell.lisp --- one sentence in, a working watch-or-poll cell out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of dolepee's Hermes plugin dolepee/hermes-plugin-foundry. The
;;;; original turned a narrow request into a Hermes plugin and ran its tests in
;;;; a subprocess; this does the same five things for Nodecode cells: PLAN
;;;; reads the request into a spec, BUILD writes the folder, VERIFY scans and
;;;; reads what was written, LIST says what is under the one root, and CATALOG
;;;; reads a draft. The root is one directory (the foundry/generated under
;;;; this home unless the section names another), symlinks followed, so a path
;;;; outside it refuses before anything is written.
;;;;
;;;; Two archetypes, the original's: a folder watcher for one file type, and
;;;; a JSON API poller; each is one cell with a section, a tool, a /command, a
;;;; worker and a state file of what it has already reported.

(in-package #:nodecode-foundry)

(nlk:define-peripheral foundry :not-running t)

;;; --- the words ------------------------------------------------------------------

(defparameter *default-folder* "~/Downloads"
  "Where a request that names no folder is understood to point.")

(defparameter *default-interval* 15
  "Minutes between looks when the request asks for no interval.")

(defparameter *type-words*
  '(("pdf" . "pdf") ("csv" . "csv") ("txt" . "txt") ("text" . "txt")
    ("md" . "md") ("markdown" . "md") ("png" . "png") ("image" . "png")
    ("photo" . "png") ("jpg" . "jpg") ("jpeg" . "jpeg") ("docx" . "docx")
    ("word" . "docx"))
  "The file types a watcher may be asked for, as the word and the extension.")

(defparameter *markers*
  '(("#." . "dynamic code") ("run-program" . "process launch")
    ("launch-program" . "process launch") ("sb-posix" . "posix call")
    ("usocket" . "socket"))
  "What VERIFY looks for in a generated file: a plain reading of its text, so a
reader may see a marker the code never runs. A finding is a question, not a
verdict.")

;;; --- text -------------------------------------------------------------------------

(defun replace-all (text old new)
  "TEXT with every OLD written as NEW."
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old text :start2 start)
          do (cond (at
                    (write-string text out :start start :end at)
                    (write-string new out)
                    (setf start (+ at (length old))))
                   (t
                    (write-string text out :start start)
                    (return))))))

(defun render (template table)
  "TEMPLATE with every {{key}} written as TABLE's value for KEY."
  (reduce (lambda (text pair)
            (replace-all text (format nil "{{~a}}" (car pair)) (cdr pair)))
          table :initial-value template))

(defun tokens (text)
  "TEXT's words, split on whitespace."
  (let ((words '()) (start nil) (size (length text)))
    (loop for index below size
          for space = (find (char text index) '(#\Space #\Tab #\Newline #\Return))
          do (cond (space (when start (push (subseq text start index) words) (setf start nil)))
                   ((null start) (setf start index)))
          finally (when start (push (subseq text start) words)))
    (nreverse words)))

(defun clean-token (word)
  "WORD without the punctuation a sentence puts around it."
  (string-right-trim ".,;:)]}'\"!" (string-left-trim "([{'\"<" word)))

(defun slugify (text)
  "TEXT as a slug: lowercase letters and digits, runs of anything else written
as one dash."
  (let ((chars '()) (pending nil))
    (flet ((emit (char) (push char chars)))
      (loop for char across (string-downcase text)
            do (if (and (alphanumericp char) (< (char-code char) 128))
                   (progn (when pending (emit #\-) (setf pending nil))
                          (emit char))
                   (when chars (setf pending t)))))
    (let ((slug (coerce (nreverse chars) 'string)))
      (if (plusp (length slug)) slug "cell"))))

(defun title-case (text)
  "TEXT's words, split on dashes and underscores, each capitalised."
  (let ((words '()) (start 0))
    (loop for index from 0 to (length text)
          when (or (= index (length text)) (find (char text index) "-_ "))
            do (let ((word (subseq text start index)))
                 (when (plusp (length word)) (push (string-capitalize word) words))
                 (setf start (1+ index))))
    (format nil "~{~a~^ ~}" (nreverse words))))

;;; --- reading the request ------------------------------------------------------------

(defun url-in (text)
  "The one http or https URL TEXT names, or NIL."
  (loop for word in (tokens text)
        for token = (clean-token word)
        when (or (uiop:string-prefix-p "http://" token)
                 (uiop:string-prefix-p "https://" token))
          return token))

(defun folder-in (text)
  "The one folder TEXT names as ~, /, ./ or ../, or NIL."
  (loop for word in (tokens text)
        for token = (clean-token word)
        when (and (plusp (length token))
                  (or (char= #\~ (char token 0))
                      (char= #\/ (char token 0))
                      (uiop:string-prefix-p "./" token)
                      (uiop:string-prefix-p "../" token)))
          return token))

(defun interval-in (text)
  "Minutes between looks, from `every N minutes|hours|days', else the default."
  (let ((words (mapcar (lambda (word) (string-downcase (clean-token word)))
                        (tokens text))))
    (or (loop for tail on words
              when (and (equal (first tail) "every") (second tail) (third tail))
                do (let ((value (ignore-errors (parse-integer (second tail) :junk-allowed t))))
                     (when value
                       (return (cond ((uiop:string-prefix-p "hour" (third tail)) (* value 60))
                                     ((uiop:string-prefix-p "day" (third tail)) (* value 1440))
                                     (t value))))))
        *default-interval*)))

(defun extension-in (text)
  "The one file type TEXT names, as its extension; a request naming none, or
two, is refused rather than guessed at."
  (let* ((words (mapcar (lambda (word) (string-downcase (clean-token word)))
                         (tokens text)))
         (found (remove-duplicates
                 (loop for word in words
                       for hit = (cdr (assoc word *type-words* :test #'equal))
                       when hit collect hit)
                 :test #'equal)))
    (cond ((null found)
           (fail "the request names no file type: say one of ~{~a~^, ~}"
                 (remove-duplicates (mapcar #'cdr *type-words*) :test #'equal)))
          ((rest found)
           (fail "the request names more than one file type: ~{~a~^, ~}" found))
          (t (first found)))))

(defun host-of (url)
  "URL's host, lowercased."
  (let* ((after (or (search "://" url) -1))
         (rest (if (< after 0) url (subseq url (+ after 3))))
         (end (or (position-if (lambda (char) (find char "/?#")) rest) (length rest))))
    (string-downcase (subseq rest 0 end))))

(defun poller-name (url)
  "The name a poller of URL is known by."
  (let* ((host (host-of url))
         (first (subseq host 0 (or (position #\. host) (length host)))))
    (cond ((search "github" host) "GitHub Poller")
          ((or (search "hn.algolia.com" host) (search "news.ycombinator.com" host))
           "Hacker News Poller")
          ((search "reddit" host) "Reddit Poller")
          (t (format nil "~@(~a~) Poller" (if (plusp (length first)) first "api"))))))

(defun folder-name (folder)
  "FOLDER's last component."
  (let* ((trimmed (string-right-trim "/" folder))
         (slash (position #\/ trimmed :from-end t))
         (base (if slash (subseq trimmed (1+ slash)) trimmed)))
    (if (plusp (length base)) base "downloads")))

(defun watcher-name (folder)
  "The name a watcher of FOLDER is known by."
  (let ((base (folder-name folder)))
    (if (member (string-downcase base) '("downloads" "documents" "files") :test #'equal)
        "File Digest"
        (format nil "~a Digest" (title-case base)))))

(defun tool-name (slug suffix)
  "SLUG's tool name: underscores, and SUFFIX as the verb it answers."
  (format nil "~a_~a" (substitute #\_ #\- slug) suffix))

(defun spec-for (request)
  "REQUEST read as the one cell it describes: a plist of what BUILD writes."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or request ""))))
    (when (< (length text) 3)
      (fail "a request is required: one sentence naming a folder to watch or a JSON URL to poll"))
    (let ((url (url-in text)))
      (if url
          (let* ((name (poller-name url))
                 (slug (slugify name))
                 (source (host-of url)))
            (list :request text :archetype "sqlite_api_poller" :name name :slug slug
                  :source url :source-name source :tool (tool-name slug "poll")
                  :state (format nil "~a/seen.txt" slug) :interval (interval-in text)
                  :reads (format nil "~a" source)
                  :short (format nil "New items from ~a, as a tool and a command" source)
                  :what (format nil "Reports the items ~a lists that no earlier call reported, and remembers them." source)))
          (let* ((folder (or (folder-in text) *default-folder*))
                 (extension (extension-in text))
                 (name (watcher-name folder))
                 (slug (slugify name))
                 (source (folder-name folder)))
            (list :request text :archetype "sqlite_folder_watcher" :name name :slug slug
                  :source folder :source-name source :ext extension
                  :tool (tool-name slug "scan")
                  :state (format nil "~a/seen.txt" slug) :interval (interval-in text)
                  :reads (format nil "the ~a files in ~a" extension source)
                  :short (format nil "New ~a files in ~a, as a tool and a command" extension source)
                  :what (format nil "Reports the ~a files in ~a that no earlier call reported, and remembers them." extension source)))))))

;;; --- the root, and the one fence ----------------------------------------------------

(defparameter *test-root* nil
  "The directory a test writes into instead of the section's own, or NIL.")

(defun root ()
  "The one directory BUILD and VERIFY write and read under."
  (let ((wanted (or *test-root*
                    (ignore-errors (setting :generated-dir))
                    (nlk:home "foundry/generated/"))))
    (ensure-directories-exist (pathname wanted))
    (truename (pathname wanted))))

(defun confine (path root)
  "PATH as a directory under ROOT, every symlink on the way followed; a path
that resolves outside ROOT refuses."
  (let* ((wanted (uiop:ensure-directory-pathname (pathname path)))
         (existing (loop for dir = wanted then (uiop:pathname-parent-directory-pathname dir)
                         for found = (probe-file dir)
                         when found return (truename dir)
                         when (uiop:pathname-equal
                               dir (uiop:pathname-parent-directory-pathname dir))
                           return nil)))
    (unless existing
      (fail "~a is not there, and neither is a folder above it" (namestring wanted)))
    (unless (uiop:string-prefix-p (namestring root) (namestring existing))
      (fail "~a is outside the foundry root (~a)" (namestring wanted) (namestring root)))
    (uiop:ensure-directory-pathname
     (if (probe-file wanted) (truename wanted) wanted))))

;;; --- writing it -------------------------------------------------------------------

(defun cell-files (spec)
  "The cell SPEC describes, as (RELATIVE-NAME . TEXT) pairs."
  (let* ((extension (getf spec :ext))
         (table
           (list (cons "slug" (getf spec :slug))
                 (cons "upper" (string-upcase (getf spec :slug)))
                 (cons "name" (getf spec :name))
                 (cons "archetype" (getf spec :archetype))
                 (cons "tool" (getf spec :tool))
                 (cons "state" (getf spec :state))
                 (cons "interval" (format nil "~d" (getf spec :interval)))
                 (cons "limit" (format nil "~d" (setting :max-items)))
                 (cons "request" (getf spec :request))
                 (cons "reads" (getf spec :reads))
                 (cons "short" (getf spec :short))
                 (cons "what" (getf spec :what))
                 (cons "ext" (or extension "json"))
                 (cons "source" (getf spec :source))
                 (cons "source-name" (getf spec :source-name))
                 (cons "key" (if extension "folder" "url"))
                 (cons "key-value" (getf spec :source))
                 (cons "source-note"
                       (if extension
                           (format nil "the folder watched (~a unless changed)" (getf spec :source))
                           (format nil "the JSON endpoint polled (~a unless changed)" (getf spec :source))))))
         (body (if extension *watcher-cell* *poller-cell*))
         (slug (getf spec :slug)))
    (list (cons (format nil "nodecode-~a.asd" slug) (render *asd* table))
          (cons "cell.lisp" (render body table))
          (cons "test/cell-test.lisp" (render *test* table))
          (cons "README.md" (render *readme* table))
          (cons "config.example.jsonc" (render *config* table))
          (cons "CATALOG.md" (render *catalog* table)))))

(defun write-cell (spec directory)
  "Write the cell SPEC describes under DIRECTORY; answer the file names written."
  (loop for (relative . text) in (cell-files spec)
        collect (let ((path (merge-pathnames relative directory)))
                  (ensure-directories-exist path)
                  (with-open-file (out path :direction :output :if-exists :supersede
                                            :if-does-not-exist :create)
                    (write-string text out))
                  relative)))

(defun build (request &key output)
  "Write the cell REQUEST describes under the foundry root, then check it.
=> a plist: :spec, :directory, :files, :checked."
  (let* ((spec (spec-for request))
         (root (root))
         (slug (getf spec :slug))
         (directory (confine (merge-pathnames
                              (or (and output (plusp (length output)) output)
                                  (format nil "nodecode-~a/" slug))
                              root)
                             root)))
    (when (probe-file (merge-pathnames "CATALOG.md" directory))
      (fail "~a already holds a generated cell; name another folder"
            (namestring directory)))
    (let ((files (write-cell spec directory)))
      (list :spec spec :directory (namestring directory) :files files
            :checked (verify directory)))))

;;; --- checking it --------------------------------------------------------------------

(defun lisp-files (directory)
  "Every .lisp and .asd file under DIRECTORY."
  (remove-if-not (lambda (file)
                   (member (pathname-type file) '("lisp" "asd") :test #'string-equal))
                 (uiop:directory* (merge-pathnames "**/*.*" directory))))

(defun scan (directory)
  "DIRECTORY's Lisp files read for *MARKERS*: (FILE FINDING) pairs, or NIL."
  (loop for file in (lisp-files directory)
        for text = (uiop:read-file-string file)
        nconc (loop for (marker . label) in *markers*
                    when (search marker text)
                      collect (list (enough-namestring file directory) label))))

(defun reader-package (directory)
  "The package a generated cell's files name, made here when this image does not
hold it yet, so a file that names its own package still reads."
  (let* ((folder (string-upcase (or (car (last (pathname-directory directory))) "cell")))
         (name (if (uiop:string-prefix-p "NODECODE-" folder)
                   folder
                   (format nil "NODECODE-~a" folder))))
    (or (find-package name)
        (let ((package (ignore-errors (make-package name))))
          (when package
            ;; The file names the cell's verbs with one colon, so the package
            ;; we made for reading exports the ones the templates write.
            (dolist (verb '("START-CELL" "STOP-CELL" "RUN-COMMAND"
                            "NEW-FILES" "NEW-ITEMS"))
              (ignore-errors (export (intern verb package) package))))
          package))))

(defun read-forms (file &optional package)
  "How many forms FILE holds, read as data with the reader's evaluation off,
inside PACKAGE when one is given."
  (handler-case
      (let ((*read-eval* nil)
            (*package* (or package *package*)))
        (with-open-file (in file :element-type 'character)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                count 1)))
    (error () 0)))

(defun forms-report (directory)
  "One (FILE FORMS) pair a Lisp file of DIRECTORY, the ones that would not read
with zero forms."
  (let ((package (reader-package directory)))
    (loop for file in (lisp-files directory)
          collect (list (enough-namestring file directory)
                        (read-forms file package)))))

(defun verify (path)
  "PATH read as a generated cell: the static scan and whether every Lisp file
reads. => a plist: :directory, :findings, :forms, :ok."
  (let* ((root (root))
         (directory (confine (merge-pathnames (pathname path) root) root))
         (findings (scan directory))
         (forms (forms-report directory)))
    (unless forms
      (fail "~a holds no Lisp file" (namestring directory)))
    (list :directory (namestring directory) :findings findings :forms forms
          :ok (and (null findings) (every (lambda (entry) (plusp (second entry))) forms)))))

;;; --- what is under the root ---------------------------------------------------------

(defun catalog-field (line)
  "The (KEY . VALUE) a draft's `**Key:** value' line carries, or NIL."
  (when (uiop:string-prefix-p "**" line)
    (let ((end (search ":**" line)))
      (when end
        (cons (string-downcase (subseq line 2 end))
              (string-trim '(#\Space #\Tab) (subseq line (+ end 3))))))))

(defun record-of (directory)
  "DIRECTORY's catalog draft as a plist of what LIST answers, or NIL when it
holds no draft."
  (let ((catalog (merge-pathnames "CATALOG.md" directory)))
    (when (probe-file catalog)
      (let ((fields (loop for line in (ignore-errors (uiop:read-file-lines catalog))
                          for field = (catalog-field line)
                          when field collect field)))
        (when fields
          (list :folder (car (last (pathname-directory directory)))
                :name (cdr (assoc "name" fields :test #'equal))
                :slug (cdr (assoc "slug" fields :test #'equal))
                :archetype (cdr (assoc "archetype" fields :test #'equal))
                :tool (cdr (assoc "tool" fields :test #'equal))
                :request (cdr (assoc "request" fields :test #'equal))))))))

(defun listing ()
  "Every cell this foundry wrote under its root, as plists."
  (loop for directory in (uiop:subdirectories (root))
        for record = (record-of directory)
        when record collect record))

(defun catalog-text (path)
  "The catalog draft of the generated cell at PATH."
  (let* ((root (root))
         (directory (confine (merge-pathnames (pathname path) root) root))
         (catalog (merge-pathnames "CATALOG.md" directory)))
    (unless (probe-file catalog)
      (fail "~a holds no CATALOG.md" (namestring directory)))
    (uiop:read-file-string catalog)))

;;; --- the words the tools and the command answer ---------------------------------------

(defun plan-text (request)
  "The spec REQUEST reads as, one line a fact."
  (let ((spec (spec-for request)))
    (with-output-to-string (out)
      (format out "archetype: ~a~%" (getf spec :archetype))
      (format out "name: ~a~%" (getf spec :name))
      (format out "slug: ~a~%" (getf spec :slug))
      (format out "tool: ~a~%" (getf spec :tool))
      (format out "source: ~a~%" (getf spec :source))
      (format out "interval_minutes: ~d~%" (getf spec :interval))
      (format out "state: ~a~%" (getf spec :state)))))

(defun build-text (request output)
  "The report of writing REQUEST, from BUILD's own answer."
  (let* ((result (build request :output output))
         (checked (getf result :checked))
         (findings (getf checked :findings)))
    (with-output-to-string (out)
      (format out "generated ~a (~a)~%" (getf (getf result :spec) :slug)
              (getf (getf result :spec) :archetype))
      (format out "folder: ~a~%" (getf result :directory))
      (format out "files: ~{~a~^, ~}~%" (getf result :files))
      (if findings
          (format out "static scan: ~{~a/~a~^, ~}~%"
                  (loop for (file label) in findings append (list file label)))
          (format out "static scan: clean~%"))
      (format out "forms read: ~{~a ~d~^, ~}~%"
              (loop for (file forms) in (getf checked :forms) append (list file forms)))
      (format out "next: read cell.lisp, then put the folder under ~a and it loads at the next boot~%"
              "~/.nodecode/cells/"))))

(defun verify-text (path)
  "The report of checking PATH."
  (let* ((result (verify path))
         (findings (getf result :findings)))
    (with-output-to-string (out)
      (format out "checked: ~a~%" (getf result :directory))
      (if findings
          (format out "static scan: ~a~%" findings)
          (format out "static scan: clean~%"))
      (format out "forms read: ~{~a ~d~^, ~}~%" (loop for (file forms) in (getf result :forms)
                                                      append (list file forms)))
      (format out "readable: ~a~%" (if (getf result :ok) "yes" "no")))))

(defun listing-text ()
  "One line a generated cell, or the word that none is."
  (let ((records (listing)))
    (if (null records)
        "Nothing generated yet: /foundry build <one sentence> writes the first one."
        (with-output-to-string (out)
          (dolist (record records)
            (format out "~a  ~a  ~a  ~a~%" (getf record :slug) (getf record :archetype)
                    (getf record :tool) (getf record :folder)))))))

;;; --- the boundary --------------------------------------------------------------------

(nlk:define-contract foundry-request
  (:doc "Plan or build one narrow Nodecode cell from one sentence: a folder to watch, or a JSON URL to poll. Write only under the foundry root; installing the folder is a separate, human step.")
  ("request" :text :required t
             :doc "the cell asked for, in one sentence: \"watch ~/Invoices for new PDF files\" or \"poll https://example.com/items.json every 30 minutes\"")
  ("output" :string :default ""
             :doc "a folder name under the foundry root to write into, or empty for one named from the request"))

(nlk:define-contract foundry-path
  (:doc "A folder this foundry wrote, by name or by path, inside the foundry root.")
  ("path" :text :required t :doc "the generated folder"))

(nlk:define-contract foundry-listing
  (:doc "List the cells this foundry wrote under its one root.")
  ("limit" :integer :default 20 :doc "how many to list at most"))

(defun plan-tool (record)
  "The foundry_plan tool: what a request would build, writing nothing."
  (plan-text (foundry-request-request record)))

(defun build-tool (record)
  "The foundry_build tool: write the cell, check it, answer what was written."
  (let ((output (foundry-request-output record)))
    (build-text (foundry-request-request record)
                (if (plusp (length output)) output nil))))

(defun test-tool (record)
  "The foundry_test tool: scan a generated folder and read its Lisp files."
  (verify-text (foundry-path-path record)))

(defun catalog-tool (record)
  "The foundry_catalog tool: a generated folder's catalog draft."
  (catalog-text (foundry-path-path record)))

(defun list-tool (record)
  "The foundry_list tool: the cells under the root."
  (let ((records (listing))
        (limit (foundry-listing-limit record)))
    (declare (ignorable limit))
    (if (null records)
        "Nothing generated yet: foundry_build writes the first one."
        (with-output-to-string (out)
          (dolist (entry (subseq records 0 (min (length records) limit)))
            (format out "~a  ~a  ~a  ~a~%" (getf entry :slug) (getf entry :archetype)
                    (getf entry :tool) (getf entry :folder)))))))

;;; --- the manual ----------------------------------------------------------------------

(defparameter +usage+
  "/foundry plan <one sentence> | build <one sentence> | list | test <slug> | catalog <slug>"
  "What /foundry takes.")

(defparameter +manual+
  "foundry: one sentence in, a working Nodecode cell out — a folder watched for
one file type, or a JSON URL polled for new items.

The tool and /foundry both do it, and both write only under one directory:
this home's foundry/generated, or the section's generated_dir. A path outside
it refuses before anything is written; the symlinks on the way are followed.
  /foundry plan <sentence>      what it would build, writing nothing
  /foundry build <sentence>     write nodecode-<slug>/ there, scan it, read it
  /foundry list                 what is under the root
  /foundry test <slug>          scan and read one again
  /foundry catalog <slug>       its catalog draft
The generated cell carries a section, a tool, a /command, a worker that looks
every interval_minutes, and a state file of what it has already reported, so
nothing is reported twice. It is written, not installed: read cell.lisp, then
put the folder under ~/.nodecode/cells/ — it loads at the next boot, or
(restart-cells) starts it now. The scanner reads the generated text for
markers (#. and process launches) and the reader is run over every file with
evaluation off; nothing generated is ever executed here, so neither the check
nor the scan is proof — a person reads the code before installing it."
  "What (help :foundry) answers while the cell runs.")

(defun run-command (args session-id)
  "The /foundry command: the five verbs of the original, for cells."
  (declare (ignore session-id))
  (let* ((text (string-trim '(#\Space #\Tab #\Newline) (or args "")))
         (space (position #\Space text))
         (sub (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (string-trim '(#\Space #\Tab) (subseq text (1+ space))) "")))
    (handler-case
        (cond ((member sub '("" "list") :test #'equal)
               (format nil "~a~%under ~a" (listing-text) (namestring (root))))
              ((equal sub "plan") (plan-text rest))
              ((equal sub "build") (build-text rest nil))
              ((equal sub "test") (verify-text rest))
              ((equal sub "catalog") (catalog-text rest))
              ((member sub '("help" "?") :test #'equal) +usage+)
              (t (format nil "Unknown /foundry subcommand ~s.~%~a" sub +usage+)))
      (foundry-error (condition) (format nil "ERROR: ~a" condition)))))

;;; --- the cell ------------------------------------------------------------------------

(nle:define-cell foundry
  (:section ("foundry")
    (:guide "generated_dir is where a scaffold goes, empty for this home's foundry/generated; max_items bounds what a generated tool reports; /foundry build <one sentence> writes one")
    ("generated_dir" :path
                     :doc "where a generated cell is written; empty = this home's foundry/generated")
    ("max_items" :integer :default 20 :min 1
                 :doc "how many new items a generated tool reports at most"))
  (:tool "foundry_plan" (nlk:contract 'foundry-request) #'plan-tool)
  (:tool "foundry_build" (nlk:contract 'foundry-request) #'build-tool)
  (:tool "foundry_test" (nlk:contract 'foundry-path) #'test-tool)
  (:tool "foundry_list" (nlk:contract 'foundry-listing) #'list-tool)
  (:tool "foundry_catalog" (nlk:contract 'foundry-path) #'catalog-tool)
  (:help :foundry "foundry: one sentence in, a working watch-or-poll cell out" +manual+)
  (:command "foundry" #'run-command
            :description "Write a working cell from one sentence"
            :argument-hint "plan <sentence> | build <sentence> | list | test <slug> | catalog <slug>"))
