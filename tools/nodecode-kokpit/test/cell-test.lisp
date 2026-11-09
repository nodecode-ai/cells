;;;; cell-test.lisp --- lifecycle, the vault formats, capture, search, the night pass.
;;;;
;;;; SPDX-LICENSE-IDENTIFIER: MIT
;;;;
;;;; What is proved here is the cell's own wiring, offline: the section (a
;;;; healthy start on a vault, a missing vault refused in words, an integer
;;;; under its floor refused), each tool through the seam the model calls it
;;;; by (the brief, search, threads, capture and the night pass), and /kokpit.
;;;; Every vault is a folder of its own under the scratch home, so no test
;;;; touches another's notes, and nothing reaches the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun kokpit-vault (&key (threads nil threads-given) (todo nil todo-given) (last-session nil cards-given))
  "A vault of its own for one test, holding THREADS, TODO and LAST-SESSION (or the defaults)."
  (let* ((dir (merge-pathnames (format nil "kokpit-test-~d-~d/"
                                       (get-universal-time) (random 1000000))
                                (nlk:home)))
         (files (list (cons "Threads.md" (if threads-given threads *threads*))
                      (cons "To-Do.md" (if todo-given todo *todo*))
                      (cons "Last-Session.md" (if cards-given last-session *cards*)))))
    (ensure-directories-exist dir)
    (dolist (pair files)
      (let ((path (merge-pathnames (car pair) dir)))
        (ensure-directories-exist path)
        (with-open-file (stream path :direction :output :if-exists :supersede
                                     :if-does-not-exist :create :external-format :utf-8)
          (write-string (cdr pair) stream))))
    (ensure-directories-exist (merge-pathnames "notes/" dir))
    (namestring dir)))

(defun iso-days-ago (days)
  "TODAY minus DAYS as an ISO date; a test's vault dates never depend on the day it runs."
  (multiple-value-bind (s m h d mo y) (decode-universal-time (- (get-universal-time) (* days 86400)))
    (declare (ignore s m h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d)))

(defun iso-days-ahead (days) (iso-days-ago (- days)))

(defun kokpit-today () (iso-days-ago 0))

(defvar *threads*
  (format nil "# Threads~%~%## Active Threads~%~%### Kitchen shelf~%~a~%Where we left off: waiting on the boards.~%Open: on you: pick the paint; Bende (last week): order the brackets~%Source: [[notes/kitchen]]~%~%### Garden gate~%~a~%Open: on you: decide on the hinge~%~%## Closed Threads~%~%### Paid the bill~%~a~%Open: on you: nothing here~%"
          (iso-days-ago 30) (iso-days-ago 2) (iso-days-ago 40))
  "A Threads.md: one stale thread, one fresh thread, one closed thread.")

(defvar *todo*
  (format nil "# To-Do~%~%## Open~%- [ ] call the printer (due: ~a)~%- [ ] water the plants~%- [x] pay the rent~%~%## Done~%- [ ] a task in the done section~%"
          (iso-days-ahead 2))
  "A To-Do.md: two open tasks, one ticked, one task in the Done section.")

(defvar *cards*
  (format nil "# Last session~%~%## ~a 09:00 · Printer and paint · s-1~%- Did: asked the printer for a quote~%- Open: pay the deposit~%"
          (iso-days-ago 1))
  "A Last-Session.md with one card.")

(defmacro with-kokpit-vault ((&rest args) &body body)
  "BODY with the cell started on a vault of its own, made from ARGS."
  `(let ((vault (kokpit-vault ,@args)))
     (with-cell-stop ((kokpit-start "vault" vault))
       ,@body)))

(defun kokpit-tool (name &rest pairs)
  "Run the registered tool NAME with PAIRS as its decoded arguments, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (let ((arguments (apply #'nlk:make-json-object
                            (loop for (key value) on pairs by #'cddr
                                  nconc (list key value)))))
      (funcall (third entry) arguments))))

(defun vault-text (vault name)
  (or (nlk:read-text (merge-pathnames name vault)) ""))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "kokpit" "KOKPIT-CELL-" :start nodecode-kokpit:start-cell)

(define-cell-lifecycle-tests "kokpit"
  (:config "vault" (kokpit-vault))
  (:help :kokpit)
  (:command "kokpit")
  (:running (is (nle::find-tool "kokpit_brief") "kokpit_brief is registered")
            (is (nle::find-tool "kokpit_brain_search"))
            (is (nle::find-tool "kokpit_threads"))
            (is (nle::find-tool "kokpit_capture"))
            (is (nle::find-tool "kokpit_night"))
            (is (= 7 (nodecode-kokpit::setting :stale-days)) "stale_days defaults to a week")
            (is (string= "inbox" (nodecode-kokpit::setting :inbox-dir)) "the inbox defaults to inbox"))
  (:stopped (is (null (nle::find-tool "kokpit_brief")) "stop unregisters the tools"))
  (:refused ("stale_days" 0) ("brief_chars" 10))
  (:idle nodecode-kokpit:kokpit-error (nodecode-kokpit::status-line)))

(deftest kokpit-cell-a-missing-vault-is-refused-in-words ()
  (let ((unset (refusal-text nlk:config-refusal (kokpit-start)))
        (gone (refusal-text nlk:config-refusal
                             (kokpit-start "vault" (namestring (merge-pathnames "no-such-vault-xyzzy/" (nlk:home)))))))
    (is (search "vault" (string-downcase unset)) "an unset vault is named")
    (is (search "set vault" unset) "and the refusal says what to set")
    (is (search "not a folder" gone) "a folder that is not there says so")))

;;; --- the brief and the threads -----------------------------------------------------

(deftest kokpit-cell-the-brief-reads-the-vault ()
  (with-kokpit-vault ()
    (let ((text (kokpit-tool "kokpit_brief")))
      (is (search (format nil "Kokpit brief ~a" (kokpit-today)) text) "the brief is dated")
      (is (search "Tasks: 2 open, 0 overdue" text) "the open tasks are counted, the Done section left out")
      (is (search "call the printer" text) "an open task is named")
      (is (search "water the plants" text))
      (is (search "On you (2):" text) "the thread items marked on you, the Done ones left out")
      (is (search "Kitchen shelf: pick the paint" text) "an owned item")
      (is (search "Garden gate: decide on the hinge" text) "an unowned item with a deciding word")
      (is (search "Printer and paint" text) "the last session card")
      (is (search "Open: pay the deposit" text) "and what it left open"))))

(deftest kokpit-cell-the-threads-tool-lists-where-we-left-off ()
  (with-kokpit-vault ()
    (let ((text (kokpit-tool "kokpit_threads")))
      (is (search "Kitchen shelf" text))
      (is (search "waiting on the boards" text) "the status line")
      (is (search "pick the paint" text) "an open item")
      (is (search "me: order the brackets" text) "the owner, read off its Bende mark")
      (is (search "on you" text) "and the mark the brief counts")
      (is (null (search "Paid the bill" text)) "a closed thread is not an open one"))))

;;; --- capture: append only -------------------------------------------------------------

(deftest kokpit-cell-a-note-becomes-a-new-file ()
  (with-kokpit-vault ()
    (let ((answer (kokpit-tool "kokpit_capture" "text" "hedge trimmer needs oil" "kind" "note")))
      (is (search "inbox/" answer) "the answer names the file")
      (let* ((vault (nodecode-kokpit::vault-directory))
             (files (uiop:directory-files (merge-pathnames "inbox/" vault))))
        (is (= 1 (length files)) "one note, one file")
        (is (search "hedge trimmer needs oil" (nlk:read-text (first files))) "the words are in it")
        (is (search "source: kokpit quick capture" (nlk:read-text (first files))) "with the receipt line")))))

(deftest kokpit-cell-a-task-is-one-line-under-open ()
  (with-kokpit-vault ()
    (let ((answer (kokpit-tool "kokpit_capture" "text" "call the glazier" "kind" "task"
                                "due" (iso-days-ahead 5))))
      (is (search "- [ ] call the glazier (due: " answer) "the line as it was written")
      (let* ((vault (nodecode-kokpit::vault-directory))
             (text (vault-text vault "To-Do.md"))
             (lines (nlk:lines text))
             (task (position-if (lambda (line) (search "call the glazier" line)) lines))
             (done (position-if (lambda (line) (search "## Done" line)) lines)))
        (is (search "call the printer" text) "the tasks that were there stay")
        (is (< task done) "the new line is above the Done heading")
        (is (search "a task in the done section" text) "and nothing under Done moved")))))

(deftest kokpit-cell-a-journal-line-is-appended-to-the-days-file ()
  (with-kokpit-vault ()
    (kokpit-tool "kokpit_capture" "text" "watered at six" "kind" "journal")
    (kokpit-tool "kokpit_capture" "text" "pruned the hedge" "kind" "journal")
    (let* ((vault (nodecode-kokpit::vault-directory))
           (path (merge-pathnames (format nil "daily/~a.md" (kokpit-today)) vault))
           (text (nlk:read-text path)))
      (is (probe-file path) "the day's file exists")
      (is (search (format nil "# ~a" (kokpit-today)) text) "it starts with its date")
      (is (search "watered at six" text) "the first line")
      (is (search "pruned the hedge" text) "and the second, appended after it"))))

(deftest kokpit-cell-a-receipt-leaves-one-card-and-one-file ()
  (with-kokpit-vault ()
    (kokpit-tool "kokpit_capture" "text" "bought the brackets" "kind" "note")
    (let ((answer (kokpit-tool "kokpit_capture" "kind" "receipt"
                                "text" "Ordered the brackets, note saved")))
      (is (search "Last-Session.md" answer))
      (let* ((vault (nodecode-kokpit::vault-directory))
             (cards (vault-text vault "Last-Session.md"))
             (receipts (uiop:directory-files (merge-pathnames "receipts/" vault))))
        (is (search "Ordered the brackets" cards) "the card says what was done")
        (is (search "Did: Ordered the brackets" cards))
        (is (search "Files: inbox/" cards) "and where it lives")
        (is (search "Open: pay the deposit" cards) "and what the last session left open")
        (is (search "Printer and paint" cards) "the older card is kept")
        (is (= 1 (length receipts)) "one receipt")
        (is (search "refs: inbox/" (nlk:read-text (first receipts))) "pointing at the file the session wrote")))))

;;; --- search ------------------------------------------------------------------------

(deftest kokpit-cell-a-search-finds-a-note-by-its-words ()
  (with-kokpit-vault ()
    (let ((path (merge-pathnames "notes/kiln.md" (nodecode-kokpit::vault-directory))))
      (with-open-file (stream path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create :external-format :utf-8)
        (write-string "# Kiln schedule

cone 6 is 1200 degrees
cone 7 is 1240 degrees
" stream))
      (let ((answer (kokpit-tool "kokpit_brain_search" "query" "kiln cone")))
        (is (search "notes/kiln.md" answer) "the path is vault-relative")
        (is (search "Kiln schedule" answer) "with the note's title")
        (is (search "1 note" answer) "one note carried both words"))
      (is (search "No note matched" (kokpit-tool "kokpit_brain_search" "query" "zeppelin"))
          "a word the vault never says matches nothing")
      (is (search "no word" (refusal-text nodecode-kokpit:kokpit-error
                                          (kokpit-tool "kokpit_brain_search" "query" "a")))
          "a query of one-letter words refuses in words"))))

;;; --- the night pass ------------------------------------------------------------------

(deftest kokpit-cell-the-night-pass-quotes-a-line-and-accepting-appends ()
  (with-kokpit-vault ()
    (let ((run (kokpit-tool "kokpit_night" "action" "run")))
      (is (search "stale" run) "the stale thread is a proposal")
      (is (search "due" run) "and so is the task due in two days")
      (is (search "Open: on you: pick the paint" run) "the thread proposal quotes the line it came from")
      (is (search "source Threads.md:" run) "naming the file")
      (let* ((open (nodecode-kokpit::open-proposals (nodecode-kokpit::load-night)))
             (stale (find-if (lambda (p) (string= "stale" (nlk:json-value p :string "kind"))) open))
             (due (find-if (lambda (p) (string= "due" (nlk:json-value p :string "kind"))) open))
             (source (nlk:json-value stale :object "source"))
             (vault (nodecode-kokpit::vault-directory)))
        (is (= 2 (length open)) "two proposals, both open")
        (is (search (nlk:json-value source :string "quote")
                    (vault-text vault (nlk:json-value source :string "path")))
            "the quote is in the file word for word")
        (let ((accepted (kokpit-tool "kokpit_night" "action" "accept"
                                      "id" (subseq (nlk:json-value stale :string "id") 0 6))))
          (is (search "Accepted" accepted))
          (is (search "- [ ] Kitchen shelf: pick the paint" (vault-text vault "To-Do.md"))
              "accepting writes one To-Do line and edits nothing"))
        (let ((dismissed (kokpit-tool "kokpit_night" "action" "dismiss"
                                       "id" (nlk:json-value due :string "id"))))
          (is (search "Dismissed" dismissed)))
        (is (= 0 (length (nodecode-kokpit::open-proposals (nodecode-kokpit::load-night))))
            "one accepted and one dismissed leave nothing open")
        (is (search "call the printer" (vault-text vault "To-Do.md")) "the tasks are untouched")))))

;;; --- /kokpit ---------------------------------------------------------------------------

(deftest kokpit-cell-the-command-speaks-and-acts ()
  (with-kokpit-vault ()
    (is (search "Kokpit vault" (nodecode-kokpit::run-command "" "s-1")) "no argument shows the vault")
    (is (search "Kokpit brief" (nodecode-kokpit::run-command "brief" "s-1")))
    (is (search "pick the paint" (nodecode-kokpit::run-command "threads" "s-1")))
    (is (search "call the glazier"
                (nodecode-kokpit::run-command (format nil "todo call the glazier due:~a"
                                                      (iso-days-ahead 3))
                                               "s-1"))
        "todo takes a due date off the end")
    (is (search "(due: " (vault-text (nodecode-kokpit::vault-directory) "To-Do.md"))
        "and writes it as the To-Do reader reads it")
    (is (search "Wrote inbox/" (nodecode-kokpit::run-command "note buy salt" "s-1")))
    (is (search "Night pass" (nodecode-kokpit::run-command "night run" "s-1")))
    (is (search "Kokpit does not know" (nodecode-kokpit::run-command "wat" "s-1"))
        "an unknown subcommand says so")))
