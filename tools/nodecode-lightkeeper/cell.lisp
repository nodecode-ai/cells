;;;; cell.lisp --- the vault, its notes, and the pointers they hand the turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A vault is a folder of Obsidian-compatible markdown notes kept outside the
;;;; repo: one note per module, flow, trap or decision, each saying what the
;;;; area is, where it lives, how it works, and what bites. This cell reads
;;;; that folder and hands it to the turn three ways:
;;;;
;;;;   lightkeeper_find   words -> ranked notes (name, tag, then body lines),
;;;;                      or file paths -> the notes that own them, each hit
;;;;                      with its one-hop links and the reason for each
;;;;   lightkeeper_note   one note by name or path, whole or one section
;;;;   lightkeeper_prime  what the vault covers, how many notes, how to query
;;;;
;;;; and by itself: the primer on a session's first round, and, after a write,
;;;; the note that owns the file, once per session per note. Nothing here can
;;;; block a write or a turn: a hook that fails answers with the result it was
;;;; given.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "lightkeeper": {"vault": "/path/to/the vault", "repo": "/path/to/checkout",
;;;;                   "max_notes": 5}

(in-package #:nodecode-lightkeeper)

;;; --- the manual ---------------------------------------------------------------
;;; (help :lightkeeper) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "A knowledge vault is available through the nodecode-lightkeeper cell: a folder of
markdown notes kept outside the repo, one per module, flow, trap or decision, each
with What it is, Where it lives (the files it owns), How it works, Gotchas and
Connects to (every link carries its reason: that list is the blast radius of a
change). Three tools read it:
  lightkeeper_find   {\"query\": \"books chapters\"} for a topic, or
      {\"paths\": [\"app/Models/Book.php\"]} for the notes that own files. Each hit
      answers its one line, where the note is, and its reasoned neighbours.
  lightkeeper_note   {\"note\": \"Books\"} reads one note; add
      {\"section\": \"Gotchas\"} for one heading.
  lightkeeper_prime  what the vault covers and how to query it.
Doctrine: before major or cross-module work, query the vault instead of
re-exploring code you already wrote down; read a hit's How it works and Gotchas
before changing that area. After a meaningful change, update the affected note:
that is part of the definition of done. A trap learned while debugging belongs in
a Gotcha even when no file changed. The primer arrives on a session's first
round, and a write names the note that owns the file once per session."
  "What (help :lightkeeper) answers while the cell runs.")

;;; --- settings ------------------------------------------------------------------

(defun folder-namestring (path)
  "PATH as a folder namestring, a trailing slash on it."
  (namestring (uiop:ensure-directory-pathname path)))

(defun read-settings (values table)
  "The settings the cell runs on, from the `lightkeeper' declaration (VALUES)
and its own object (TABLE, unused: the section is flat)."
  (declare (ignore table))
  (let ((vault (getf values :vault))
        (repo (getf values :repo)))
    (unless (and vault (uiop:directory-exists-p (uiop:ensure-directory-pathname vault)))
      (nlk:config-error "vault ~s is not a folder: point it at the markdown vault this cell reads" vault))
    (when (and repo (plusp (length repo))
               (not (uiop:directory-exists-p (uiop:ensure-directory-pathname repo))))
      (nlk:config-error "repo ~s is not a folder: it is the checkout the vault covers" repo))
    (list :vault (folder-namestring (truename (uiop:ensure-directory-pathname vault)))
          :repo (if (and repo (plusp (length repo)))
                    (folder-namestring (truename (uiop:ensure-directory-pathname repo)))
                    nil)
          :max-notes (getf values :max-notes))))

(defun vault-root ()
  "The vault folder as a namestring with its trailing slash, or a refusal."
  (or (setting :vault) (fail "no vault is set: put its folder in lightkeeper.vault")))

;;; --- small text tools ------------------------------------------------------------

(defun join (strings separator)
  "STRINGS joined by SEPARATOR, NILs and the empty string left out."
  (with-output-to-string (out)
    (loop for (string . rest) on (remove-if (lambda (s) (or (null s) (zerop (length s)))) strings)
          do (write-string string out)
             (when rest (write-string separator out)))))

(defun split-lines (text)
  "TEXT as a list of lines."
  (loop with start = 0 with out = '()
        for newline = (position #\Newline text :start start)
        for end = (or newline (length text))
        do (push (subseq text start end) out)
        while newline
        do (setf start (1+ newline))
        finally (return (nreverse out))))

(defun split-on (text char)
  "TEXT split on CHAR."
  (loop with start = 0 with out = '()
        for position = (position char text :start start)
        do (push (subseq text start position) out)
           (if position (setf start (1+ position)) (return (nreverse out)))))

(defun trim-to (text limit)
  "TEXT cut at a word boundary under LIMIT characters, an ellipsis on the cut."
  (if (or (null text) (<= (length text) limit))
      text
      (let* ((head (subseq text 0 limit))
             (cut (position #\Space head :from-end t)))
        (concatenate 'string (if cut (subseq head 0 cut) head) "…"))))

(defun read-text (file)
  "FILE's text, or the empty string when it cannot be read."
  (or (ignore-errors (uiop:read-file-string file :external-format :utf-8)) ""))

(defun tokens (text)
  "TEXT's lowercase tokens: runs of letters and digits, anything else a break."
  (let ((out '()) (current (make-string-output-stream)))
    (flet ((flush ()
             (let ((token (string-downcase (get-output-stream-string current))))
               (when (plusp (length token)) (push token out)))))
      (loop for char across (string-downcase text)
            do (if (alphanumericp char) (write-char char current) (flush)))
      (flush))
    (nreverse out)))

(defparameter *stopwords*
  '("a" "about" "after" "all" "an" "and" "any" "are" "as" "at" "be" "before" "but" "by"
    "can" "do" "does" "for" "from" "has" "have" "how" "if" "in" "into" "is" "it" "its"
    "not" "of" "on" "one" "or" "our" "out" "over" "so" "that" "the" "this" "to" "two"
    "was" "we" "what" "when" "where" "which" "who" "why" "with" "you")
  "Words a note name is never matched on.")

(defun significant (text)
  "TEXT's tokens without the stopwords, a trailing plural s folded away."
  (loop for token in (tokens text)
        unless (member token *stopwords* :test #'string=)
          collect (if (and (> (length token) 3) (char= #\s (char token (1- (length token)))))
                      (subseq token 0 (1- (length token)))
                      token)))

;;; --- reading the vault -------------------------------------------------------------

(defparameter *skip-directories* '(".git" ".obsidian" ".trash" "_templates" "node_modules")
  "Folders that hold the vault's own machinery, never a note.")

(defun folder-component (pathname)
  "PATHNAME's last directory component, downcased, or NIL."
  (let ((directory (pathname-directory pathname)))
    (and directory (string-downcase (car (last directory))))))

(defun note-files (vault)
  "Every .md file under VAULT, the vault's own folders skipped."
  (let ((found '()))
    (labels ((walk (directory)
               (dolist (file (ignore-errors (uiop:directory-files directory)))
                 (when (equal "md" (string-downcase (or (pathname-type file) "")))
                   (push file found)))
               (dolist (sub (ignore-errors (uiop:subdirectories directory)))
                 (unless (member (folder-component sub) *skip-directories* :test #'string=)
                   (walk sub)))))
      (walk (uiop:ensure-directory-pathname vault)))
    (sort found #'string< :key #'namestring)))

(defun frontmatter (lines)
  "The flat `key: value` frontmatter of LINES, as an alist of lowercase keys."
  (when (and lines (string= (string-trim '(#\Space #\Return) (first lines)) "---"))
    (loop for line in (rest lines)
          until (string= (string-trim '(#\Space #\Return) line) "---")
          for colon = (position #\: line)
          when colon
            collect (cons (string-downcase (string-trim '(#\Space #\Return) (subseq line 0 colon)))
                          (string-trim '(#\Space #\Return) (subseq line (1+ colon)))))))

(defun field (frontmatter key)
  "The value of KEY in FRONTMATTER, or NIL."
  (cdr (assoc key frontmatter :test #'string=)))

(defun section-body (text heading)
  "The lines under `## HEADING` in TEXT, up to the next heading, or NIL."
  (let ((start (search (format nil "~%## ~a" heading) text)))
    (when start
      (let* ((newline (position #\Newline text :start (+ start 1)))
             (body (and newline (split-lines (subseq text (1+ newline)))))
             (kept '()))
        (dolist (line body)
          (when (or (uiop:string-prefix-p "## " line) (uiop:string-prefix-p "# " line))
            (return))
          (push line kept))
        (join (nreverse kept) (string #\Newline))))))

(defun code-spans (text)
  "The `code spans` of TEXT."
  (loop with start = 0
        for open = (position #\` text :start start)
        for close = (and open (position #\` text :start (1+ open)))
        while close
        do (setf start (1+ close))
        collect (subseq text (1+ open) close)))

(defun strip-anchor (span)
  "SPAN without a trailing :12 or :12-34 line anchor."
  (let ((colon (position #\: span :from-end t)))
    (if (and colon (< (1+ colon) (length span))
             (every (lambda (char) (or (digit-char-p char) (char= char #\-))) (subseq span (1+ colon))))
        (subseq span 0 colon)
        span)))

(defun claimed-paths (text)
  "The code paths and folders the `## Where it lives` table of TEXT claims."
  (let ((body (section-body text "Where it lives"))
        (paths '()) (dirs '()))
    (when body
      (dolist (line (split-lines body))
        (when (uiop:string-prefix-p "|" (string-left-trim " " line))
          (dolist (span (code-spans line))
            (let ((value (strip-anchor (string-trim '(#\Space #\Return #\Tab) span))))
              (cond ((zerop (length value)))
                    ((or (char= #\/ (char value (1- (length value))))
                         (uiop:string-prefix-p "./" value))
                     (let ((dir (string-right-trim "/" (if (uiop:string-prefix-p "./" value)
                                                           (subseq value 2) value))))
                       (when (plusp (length dir))
                         (pushnew dir dirs :test #'string=)
                         (pushnew dir paths :test #'string=))))
                    ((find #\/ value)
                     (pushnew value paths :test #'string=))
                    ((find #\. value :start (max 0 (- (length value) 8)))
                     (pushnew value paths :test #'string=))))))))
    (values paths dirs)))

(defun reason-of (line)
  "The reason a [[link]] carries on LINE, or NIL."
  (let ((dash (or (search "—" line) (search "–" line) (search " - " line))))
    (when dash
      (let ((after (string-trim '(#\Space #\Tab)
                                 (subseq line (+ dash (if (char= #\Space (char line dash)) 3 1))))))
        (trim-to after 120)))))

(defun wiki-links (text)
  "TEXT's [[links]] with the reason each carries on its line, as (TARGET . REASON)."
  (let ((out '()) (start 0))
    (loop
      (let ((open (search "[[" text :start2 start)))
        (unless open (return))
        (let ((close (search "]]" text :start2 (+ open 2))))
          (unless close (return))
          (let* ((inner (subseq text (+ open 2) close))
                 (bar (position #\| inner))
                 (target (string-trim '(#\Space) (subseq inner 0 (or bar (length inner)))))
                 (line-end (or (position #\Newline text :start close) (length text)))
                 (line (subseq text (+ close 2) (min line-end (+ close 200)))))
            (push (cons target (reason-of line)) out))
          (setf start (+ close 2)))))
    (nreverse out)))

(defun summary-of (text)
  "The first sentence of TEXT's `## What it is`, cleaned of markdown, under 240 characters."
  (let ((body (section-body text "What it is")))
    (when body
      (let ((paragraph (find-if (lambda (line) (plusp (length (string-trim '(#\Space #\Tab) line))))
                                (split-lines body))))
        (when paragraph
          (let* ((clean (string-trim '(#\Space #\Tab #\>) paragraph))
                 (cut (search ". " clean))
                 (sentence (if cut (subseq clean 0 (1+ cut)) clean)))
            (trim-to (remove-markup sentence) 240)))))))

(defun remove-markup (text)
  "TEXT without **bold**, [[links|alias]] and `code` marks."
  (let ((out (with-output-to-string (stream)
               (loop with index = 0
                     while (< index (length text))
                     do (cond ((and (< (+ index 1) (length text)) (char= #\[ (char text index) (char text (1+ index))))
                               (let ((close (search "]]" text :start2 (+ index 2))))
                                 (if close
                                     (let* ((inner (subseq text (+ index 2) close))
                                            (bar (position #\| inner)))
                                       (write-string (if bar (subseq inner (1+ bar)) inner) stream)
                                       (setf index (+ close 2)))
                                     (progn (write-char #\[ stream) (incf index)))))
                              ((and (< (+ index 1) (length text)) (char= #\* (char text index) (char text (1+ index))))
                               (incf index 2))
                              ((char= #\` (char text index)) (incf index))
                              (t (write-char (char text index) stream) (incf index)))))))
    out))

;;; --- a note, and the vault of them -------------------------------------------------

(defstruct (note (:constructor make-note))
  "One vault note: its frontmatter, its one line, what it claims, and its links."
  (stem "" :read-only t)
  (path "" :read-only t)
  (file nil :read-only t)
  (type nil :read-only t)
  (status nil :read-only t)
  (depth nil :read-only t)
  (updated nil :read-only t)
  (summary nil :read-only t)
  (tags '() :read-only t)
  (paths '() :read-only t)
  (dirs '() :read-only t)
  (links '() :read-only t)
  (body "" :read-only t))

(defun make-note-from (file vault)
  "The NOTE FILE is, relative to VAULT."
  (let* ((text (read-text file))
         (lines (split-lines text))
         (front (frontmatter lines)))
    (multiple-value-bind (paths dirs) (claimed-paths text)
      (make-note :stem (or (pathname-name file) "")
                 :path (enough-namestring file vault)
                 :file file
                 :type (field front "type")
                 :status (field front "status")
                 :depth (field front "depth")
                 :updated (field front "updated")
                 :summary (summary-of text)
                 :tags (let ((raw (field front "tags")))
                         (when raw
                           (mapcar (lambda (tag) (string-downcase (string-trim '(#\Space #\[ #\]) tag)))
                                   (split-on raw #\,))))
                 :paths paths
                 :dirs dirs
                 :links (wiki-links text)
                 :body text))))

(defun vault-notes ()
  "Every note in the vault, freshly read."
  (let ((vault (vault-root)))
    (unless (uiop:directory-exists-p vault)
      (fail "the vault folder ~a is gone" (string-right-trim "/" vault)))
    (mapcar (lambda (file) (make-note-from file vault)) (note-files vault))))

(defun file-name (path)
  "PATH's last segment."
  (car (last (split-on path #\/))))

(defun path-tail-p (short long)
  "Whether SHORT names the tail of LONG at a path boundary."
  (or (string= short long)
      (and (< (length short) (length long))
           (let ((start (- (length long) (length short))))
             (and (string= short long :start2 start)
                  (char= #\/ (char long (1- start))))))))

(defun owners (notes path)
  "The notes that own PATH, best first: (STEM VIA PRIORITY), 0 exact, 1 folder, 2 basename."
  (let ((normalized (string-right-trim "/" (strip-anchor (string-trim '(#\Space) path))))
        (found '()))
    (when (plusp (length normalized))
      ;; the exact path a note claims, or the file a claim ends with
      (dolist (note notes)
        (dolist (claim (note-paths note))
          (when (path-tail-p claim normalized)
            (pushnew (list (note-stem note) claim 0) found :test #'string= :key #'first))))
      ;; the longest folder above it that a note claims
      (let ((parts (split-on normalized #\/)))
        (loop for count downfrom (1- (length parts)) to 1
              while (null found)
              for directory = (join (subseq parts 0 count) "/")
              do (dolist (note notes)
                   (dolist (dir (note-dirs note))
                     (when (path-tail-p dir directory)
                       (pushnew (list (note-stem note) (concatenate 'string dir "/") 1)
                                found :test #'string= :key #'first))))))
      ;; the same file name, when no path claims it
      (when (null found)
        (let ((base (file-name normalized)))
          (dolist (note notes)
            (dolist (claim (note-paths note))
              (when (string-equal (file-name claim) base)
                (pushnew (list (note-stem note) base 2) found :test #'string= :key #'first)))))))
    (sort found #'< :key #'third)))

(defun neighbours (note notes &key (limit 6))
  "NOTE's one-hop links among NOTES, each as (DIRECTION OTHER REASON)."
  (let ((out '()) (seen '()))
    (dolist (link (note-links note))
      (let ((other (find (car link) notes :key #'note-stem :test #'string-equal)))
        (when (and other (not (member (note-stem other) seen :test #'string=))
                   (< (length out) limit))
          (push (note-stem other) seen)
          (push (list "→" (note-stem other) (cdr link)) out))))
    (dolist (other notes)
      (when (and (not (eq other note)) (< (length out) limit)
                 (not (member (note-stem other) seen :test #'string=)))
        (let ((link (find (note-stem note) (note-links other) :key #'car :test #'string-equal)))
          (when link
            (push (note-stem other) seen)
            (push (list "←" (note-stem other) (cdr link)) out)))))
    (nreverse out)))

;;; --- rendering a hit ----------------------------------------------------------------

(defun render-hit (note notes &key via matched-lines)
  "NOTE as the lines a find answers with: who it is, where, and its blast radius."
  (let* ((meta (remove nil (list (note-type note) (note-status note) (note-depth note)
                                  (and (note-updated note) (format nil "updated ~a" (note-updated note))))))
         (head (format nil "~a (~a)" (note-stem note) (join meta ", ")))
         (lines (list (if via (format nil "~a   [~a]" head via) head))))
    (when (note-summary note) (push (format nil "  ~a" (note-summary note)) lines))
    (push (format nil "  → ~a" (note-path note)) lines)
    (dolist (line matched-lines)
      (push (format nil "  match: ~a" (trim-to line 200)) lines))
    (let ((links (neighbours note notes)))
      (when links
        (push "  connects:" lines)
        (dolist (link links)
          (push (format nil "    ~a ~a~@[ — ~a~]" (first link) (second link) (third link)) lines))))
    (format nil "~a" (join (nreverse lines) (string #\Newline)))))

(defun term-scores (notes query)
  "The notes QUERY names, by name, then tag, then body, best first: (NOTE VIA SCORE LINES)."
  (let ((wanted (significant query))
        (hits '()))
    (when wanted
      (dolist (note notes)
        (let* ((stem-tokens (significant (note-stem note)))
               (overlap (count-if (lambda (token) (member token wanted :test #'string=)) stem-tokens))
               (full (and (plusp (length stem-tokens)) (= overlap (length stem-tokens))
                          (or (> (length stem-tokens) 1) (>= (length (first stem-tokens)) 3))))
               (tags (loop for tag in (note-tags note)
                           for tokens = (significant tag)
                           when (some (lambda (token) (member token wanted :test #'string=)) tokens)
                             collect tag)))
          (cond (full (push (list note "name" (+ 3.0 (length stem-tokens)) nil) hits))
                ((and (<= (length wanted) 3) (plusp overlap))
                 (push (list note "name-part" (+ 1.0 (/ overlap (length stem-tokens))) nil) hits))
                (tags (push (list note (format nil "tag:~a" (first tags)) 2.0 nil) hits))))))
    (when (null hits)
      (let ((body-wanted (remove-if (lambda (token) (< (length token) 2)) wanted)))
        (when body-wanted
          (dolist (note notes)
            (let ((lines (matching-lines note body-wanted)))
              (when lines (push (list note "content" (length lines) lines) hits)))))))
    (sort hits #'> :key #'third)))

(defun matching-lines (note wanted)
  "The lines of NOTE that carry the most of WANTED, the whole note required: the top two."
  (let ((body (string-downcase (note-body note))))
    (when (every (lambda (token) (search token body)) wanted)
      (let ((scored '()))
        (dolist (line (split-lines (note-body note)))
          (let ((low (string-downcase line)) (count 0))
            (dolist (token wanted) (when (search token low) (incf count)))
            (when (plusp count) (push (list count (string-trim '(#\Space) line)) scored))))
        (mapcar #'second (subseq (sort scored #'> :key #'first) 0 (min 2 (length scored))))))))

;;; --- the verbs ----------------------------------------------------------------------

(defun find-notes (query &key paths (limit nil))
  "The vault's answer for QUERY words or PATHS: the notes that match, best first."
  (unless *lightkeeper* (fail "not running: start the lightkeeper cell, then set its vault"))
  (let ((limit (max 1 (or limit (setting :max-notes) 5)))
        (notes (vault-notes)))
    (cond
      (paths
       (let ((lines '()) (seen '()) (unknown '()))
         (dolist (path (mapcar (lambda (p) (princ-to-string p)) paths))
           (let ((owned (owners notes path)))
             (if (null owned)
                 (pushnew path unknown :test #'string=)
                 (dolist (entry owned)
                   (unless (member (first entry) seen :test #'string=)
                     (push (first entry) seen)
                     (when (< (length lines) limit)
                       (push (render-hit (find (first entry) notes :key #'note-stem :test #'string=)
                                         notes :via (second entry))
                             lines)))))))
         (join (cons (if unknown
                         (format nil "no note claims ~{~a~^, ~} — if that area is real, it may need a note"
                                 (reverse unknown))
                         nil)
                     (append (list nil) (nreverse lines)))
               (string #\Newline))))
      ((and query (plusp (length (string-trim '(#\Space) query))))
       (let ((hits (term-scores notes query)))
         (if (null hits)
             (format nil "no note matches ~s — try fewer or other words, or a file path" query)
             (format nil "~a~@[~%~%~a~]"
                     (join (loop for (note via score lines) in (subseq hits 0 (min limit (length hits)))
                                 collect (render-hit note notes :via via :matched-lines lines))
                           (string #\Newline))
                     nil))))
      (t (fail "give words to search, or paths to look up")))))

(defun read-note (name &key section (chars 6000))
  "The note NAME answers, whole or one SECTION of it, cut at CHARS."
  (unless *lightkeeper* (fail "not running: start the lightkeeper cell, then set its vault"))
  (let* ((notes (vault-notes))
         (wanted (string-trim '(#\Space) (or name "")))
         (note (and (plusp (length wanted))
                    (or (find wanted notes :key #'note-stem :test #'string-equal)
                        (find wanted notes :key #'note-path :test #'string-equal)
                        (find (format nil "~a.md" wanted) notes :key #'note-path :test #'string-equal)))))
    (unless note
      (fail "no note named ~s: call lightkeeper_find with words from the area first" wanted))
    (let* ((text (note-body note))
           (body (if (and section (plusp (length section)))
                     (or (section-body text section)
                         (fail "note ~a has no section ~s" (note-stem note) section))
                     text))
           (cut (> (length body) (max 500 chars)))
           (shown (if cut (subseq body 0 (max 500 chars)) body)))
      (join (list (format nil "~a — ~a" (note-stem note) (note-path note))
                  ""
                  shown
                  (and cut (format nil "… truncated at ~d characters; the whole note is ~a"
                                   chars (note-path note))))
            (string #\Newline)))))

(defun vault-name ()
  "The vault's own name: its folder's."
  (let ((directory (pathname-directory (uiop:ensure-directory-pathname (vault-root)))))
    (or (and directory (car (last directory))) "vault")))

(defun prime (&optional repo)
  "What the vault covers and how to query it, as one text."
  (declare (ignore repo))
  (unless *lightkeeper* (fail "not running: start the lightkeeper cell, then set its vault"))
  (let* ((notes (vault-notes))
         (home (find "home" notes :key #'note-stem :test #'string-equal))
         (by-type (make-hash-table :test #'equal)))
    (dolist (note notes)
      (incf (gethash (or (note-type note) "note") by-type 0)))
    (join (list (format nil "~a — ~d note~:p in the vault at ~a" (vault-name) (length notes)
                         (string-right-trim "/" (vault-root)))
                 (format nil "  ~a" (join (loop for key being the hash-keys of by-type using (hash-value count)
                                                 collect (format nil "~a: ~d" key count))
                                           ", "))
                 (and home (format nil "~%~a" (note-summary home)))
                 ""
                 "lightkeeper_find takes words or file paths; lightkeeper_note reads one note or one section. Query the vault before cross-module work; update the note a change touches.")
            (string #\Newline))))

(defun check-vault (&optional limit)
  "The vault's own state: the stubs, and the claimed paths that are not in the checkout."
  (unless *lightkeeper* (fail "not running: start the lightkeeper cell, then set its vault"))
  (let* ((limit (or limit 8))
         (notes (vault-notes))
         (repo (setting :repo))
         (stubs (remove-if-not (lambda (note) (member (note-depth note) '("stub") :test #'string-equal)) notes))
         (missing '()))
    (when repo
      (dolist (note notes)
        (dolist (path (note-paths note))
          (unless (probe-file (merge-pathnames path repo))
            (push (format nil "~a: ~a" (note-stem note) path) missing)))))
    (join (list (format nil "~a — ~d note~:p; ~d still a stub~@[ (e.g. ~{~a~^, ~})~]"
                         (vault-name) (length notes) (length stubs)
                         (and stubs (mapcar #'note-stem (subseq stubs 0 (min 3 (length stubs))))))
                 (and repo (format nil "~d claimed path~:p not in the checkout~@[ (e.g. ~{~a~^, ~})~]"
                                   (length missing) (and missing (subseq (nreverse missing) 0 (min limit (length missing))))))
                 (and (null repo) "set lightkeeper.repo to have claimed paths checked against the checkout"))
          (string #\Newline))))

;;; --- the tools ----------------------------------------------------------------------

(nlk:define-contract find-query
  (:doc "Look in the project's knowledge vault: words find notes by topic, paths find the notes that own files. Each hit answers its one line, where the note is, and its linked neighbours with the reason for each link.")
  ("query" :text :default ""
           :doc "words naming a module, a flow, a trap or a decision")
  ("paths" :array :of :string
           :doc "file paths (or folders), each answering the notes that own it")
  ("limit" :integer :default 5
           :doc "how many notes to answer with"))

(nlk:define-contract note-query
  (:doc "Read one note of the project's knowledge vault, whole or one of its sections (What it is, Where it lives, How it works, Gotchas, Connects to).")
  ("note" :text :required t
          :doc "the note's name as a find hit printed it, e.g. \"Books\", or its path in the vault")
  ("section" :text :default ""
             :doc "one heading to read instead of the whole note, e.g. \"Gotchas\"")
  ("chars" :integer :default 6000
           :doc "how much of the note to answer with"))

(nlk:define-contract prime-query
  (:doc "The vault's primer: what it covers, how many notes, and how to query it. Call it once when you start work in an unfamiliar area.")
  ("repo" :text :default ""
          :doc "which checkout to prime for; empty means this one"))

(defun find-tool (arguments)
  "The lightkeeper_find tool."
  (find-notes (find-query-query arguments)
               :paths (coerce (find-query-paths arguments) 'list)
               :limit (find-query-limit arguments)))

(defun note-tool (arguments)
  "The lightkeeper_note tool."
  (read-note (note-query-note arguments)
             :section (note-query-section arguments)
             :chars (note-query-chars arguments)))

(defun prime-tool (arguments)
  "The lightkeeper_prime tool."
  (prime (prime-query-repo arguments)))

;;; --- the hooks: the primer once a session, the pointer after a write ------------------

(defvar *written* (make-hash-table :test #'equal)
  "Session -> the files it wrote this turn, newest first.")

(defvar *shown* (make-hash-table :test #'equal)
  "Session -> the notes it has already been pointed at.")

(defvar *primed* (make-hash-table :test #'equal)
  "The sessions whose first round has heard the primer.")

(defvar *hook-lock* (bt2:make-lock :name "lightkeeper")
  "One lock over the three tables above: hooks run on the turn's thread and the store's.")

(defun forget-session (session)
  "Drop what the tables hold for SESSION."
  (bt2:with-lock-held (*hook-lock*)
    (remhash session *written*)
    (remhash session *shown*)
    (remhash session *primed*)))

(defun remember-write (session path)
  "Record that SESSION wrote PATH, once."
  (when (and session path)
    (bt2:with-lock-held (*hook-lock*)
      (pushnew path (gethash session *written*) :test #'string=))))

(defun take-written (session)
  "SESSION's recorded files, the record emptied."
  (bt2:with-lock-held (*hook-lock*)
    (prog1 (gethash session *written*) (remhash session *written*))))

(defun record-write (next path text)
  "WRITE-FILE-TEXT advice: once the write has landed, note the file for the
turn it was written for. Never fails the write."
  (multiple-value-prog1 (funcall next path text)
    (ignore-errors
      (when *lightkeeper*
        (remember-write (getf (nle:turn) :session-id)
                        (ignore-errors (namestring (probe-file path))))))))

(defun inside-vault-p (path)
  "Whether PATH is one of the vault's own files: never point at a note while editing the vault."
  (let ((vault (ignore-errors (setting :vault))))
    (and vault (uiop:string-prefix-p vault path))))

(defun pointer-line (note via)
  "The one line a write earns: the note that owns the file, and what to do with it."
  (if (string-equal (note-depth note) "stub")
      (format nil "`~a` belongs to vault note \"~a\", still a stub: ~a. If you learn how this area works or hit a trap, write it there."
              via (note-stem note) (note-path note))
      (format nil "`~a` is documented by vault note \"~a\" (updated ~a) — read its Gotchas before going further: ~a"
              via (note-stem note) (or (note-updated note) "?") (note-path note))))

(defun pointer-block (session)
  "The note pointers SESSION has not been shown yet for the files it wrote, or NIL."
  (let ((files (take-written session)))
    (when files
      (let ((notes (ignore-errors (vault-notes)))
            (shown (bt2:with-lock-held (*hook-lock*) (copy-list (gethash session *shown*))))
            (fresh '())
            (lines '()))
        (when notes
          (dolist (file files)
            (unless (inside-vault-p file)
              (dolist (entry (owners notes file))
                (let ((stem (first entry)))
                  (when (and (< (third entry) 3)
                             (not (member stem shown :test #'string=))
                             (not (member stem fresh :test #'string=))
                             (< (length fresh) 2))
                    (push stem fresh)
                    (let ((note (find stem notes :key #'note-stem :test #'string=)))
                      (push (pointer-line note (second entry)) lines))))))))
        (when fresh
          (bt2:with-lock-held (*hook-lock*)
            (setf (gethash session *shown*) (append fresh shown)))
          (join (append (nreverse lines)
                        (list "If this change alters behavior, updating the note afterwards is part of the definition of done."))
                (string #\Newline)))))))

(defun note-writes (op next)
  "The :TOOL advice: the result of a call that wrote files gains the notes that
own them. Fails open: the result's own text is never lost."
  (let ((text (funcall next op))
        (session (or (getf (nle:turn) :session-id) (getf op :session-id))))
    (if (and (stringp text) session *lightkeeper*)
        (handler-case
            (let ((block (pointer-block session)))
              (if block (format nil "~a~&~%~a" text block) text))
          (nlk:turn-cancelled-condition (condition) (error condition))
          (error () text))
        text)))

(defun round-line ()
  "The one reminder a session's first round carries: the vault exists, and the verbs."
  (format nil "~a knowledge vault: ~d note~:p at ~a — module notes with How it works and Gotchas, kept outside the repo. Query it before cross-module work (lightkeeper_find), read the note a hit names before changing that area, and update the note after a change. (help :lightkeeper) has the verbs."
          (vault-name) (length (vault-notes)) (string-right-trim "/" (vault-root))))

(defun prime-round (op next)
  "The :ROUND advice: on a session's first round, the vault's one-line primer."
  (let ((session (getf op :session-id))
        (turn (getf op :turn))
        (text nil))
    (when (and session turn *lightkeeper*
               (or (null (setting :repo))
                   (let ((directory (ignore-errors (nle::ambient-session-directory))))
                     (and directory (uiop:string-prefix-p (setting :repo) (namestring directory))))))
      (let ((fresh (bt2:with-lock-held (*hook-lock*)
                     (unless (gethash session *primed*)
                       (setf (gethash session *primed*) t)))))
        (when fresh
          (setf text (ignore-errors (round-line)))
          (when text
            (ignore-errors (nlk:record-turn-context-prompt turn :text text))))))
    (funcall next op)
    text))

;;; --- the command --------------------------------------------------------------------

(defun run-slash (args session-id)
  "/lightkeeper alone or `status': the vault's state; `find WORDS'; `note NAME [SECTION]'."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (string-downcase (if space (subseq text 0 space) text)))
         (rest (and space (nlk:trimmed (subseq text (1+ space))))))
    (handler-case
        (cond ((member verb '("" "status" "check") :test #'string=) (check-vault))
              ((string= verb "find") (if (and rest (plusp (length rest)))
                                          (find-notes rest)
                                          "usage: /lightkeeper find WORDS"))
              ((string= verb "note")
               (if (and rest (plusp (length rest)))
                   (let ((split (position #\Space rest)))
                     (read-note (if split (subseq rest 0 split) rest)
                                :section (or (and split (nlk:trimmed (subseq rest (1+ split)))) "")))
                   "usage: /lightkeeper note NAME [SECTION]"))
              (t (format nil "/lightkeeper: ~a is not one of status, find, note" verb)))
      (lightkeeper-error (condition) (format nil "~a" condition)))))

;;; --- the one declaration ------------------------------------------------------------

(nle:define-cell lightkeeper
  (:section ("lightkeeper")
    (:guide "vault is the folder of markdown notes this cell reads (required); repo is the checkout it covers, so the primer stays silent elsewhere and claimed paths are checked against it; max_notes bounds one find")
    ("vault" :path :required t
             :doc "the folder of markdown notes; an Obsidian vault works as it is")
    ("repo" :path
            :doc "the checkout the vault covers: the primer stays silent outside it")
    ("max_notes" :integer :default 5 :min 1
                 :doc "how many notes one find answers with"))
  (:settings #'read-settings)
  (:tool "lightkeeper_find" (nlk:contract 'find-query) 'find-tool)
  (:tool "lightkeeper_note" (nlk:contract 'note-query) 'note-tool)
  (:tool "lightkeeper_prime" (nlk:contract 'prime-query) 'prime-tool)
  (:help :lightkeeper "lightkeeper: a vault of notes on how the codebase works - lightkeeper_find, lightkeeper_note, lightkeeper_prime" +primer+)
  (:hook 'nle::write-file-text #'record-write)
  (:hook :tool #'note-writes)
  (:hook :round #'prime-round)
  (:command "lightkeeper" 'run-slash
            :description "The knowledge vault: its state, a find, one note"
            :argument-hint "[status | find WORDS | note NAME [SECTION]]"))
