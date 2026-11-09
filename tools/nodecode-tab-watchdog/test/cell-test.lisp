;;;; cell-test.lisp --- the cell over a fake DevTools endpoint and a walking clock.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the cell's one network seam answers the two
;;;; DevTools routes it knows and closes the tab it is told to, and the clock
;;;; is a number a test moves, so an hour of a tab's life is one call. The
;;;; tools are called through the real dispatch seam and the command through
;;;; CELL-ENTRY, so what is proved is the declaration a provider is shown and
;;;; the body that runs it.

(in-package #:nodecode.test)

(define-test-slice "tab-watchdog" "TAB-WATCHDOG-CELL-"
  :start nodecode-tab-watchdog:start-cell)

;;; --- the fake browser -------------------------------------------------------

(defvar *tabs-open* '()
  "The tabs the fake endpoint lists, one (:id :url :title) plist each.")

(defvar *tabs-closed* '()
  "The ids the fake endpoint closed, newest first.")

(defvar *tabs-requests* '()
  "Every request the fake endpoint took, newest first: (METHOD PATH).")

(defvar *tabs-clock* 1000000
  "The clock the cell reads, in universal time.")

(defvar *tabs-calls* 0
  "A counter for wire call ids.")

(defun tabs-tab (id title url)
  "One fake tab, as the endpoint lists it."
  (list :id id :title title :url url))

(defun tabs-json (tabs)
  "TABS as the JSON array /json/list answers."
  (nlk:encode-json-object
   (coerce (loop for tab in tabs
                 collect (nlk:make-json-object "id" (getf tab :id) "type" "page"
                                                "title" (or (getf tab :title) "")
                                                "url" (or (getf tab :url) "")))
           'vector)))

(defun tabs-path (url)
  "URL's DevTools path: from /json/ on."
  (let ((at (search "/json/" url)))
    (if at (subseq url at) url)))

(defun tabs-fake-endpoint ()
  "A stand-in for NLK:HTTP over the fake browser's own tabs."
  (lambda (method url &key content &allow-other-keys)
    (declare (ignore content))
    (let ((path (tabs-path url)))
      (push (list method path) *tabs-requests*)
      (cond
        ((equal path "/json/list") (values (tabs-json *tabs-open*) 200 nil nil))
        ((uiop:string-prefix-p "/json/close/" path)
         (let ((id (subseq path (length "/json/close/"))))
           (push id *tabs-closed*)
           (setf *tabs-open* (remove id *tabs-open* :key (lambda (tab) (getf tab :id))
                                               :test #'equal))
           (values "Target is closing" 200 nil nil)))
        (t (values "{\"error\":\"no such route\"}" 404 nil nil))))))

(defmacro with-fake-tabs ((&key open (now 1000000) ledger) &body body)
  "The cell over a fake browser open on OPEN, its clock at NOW, its ledger
under LEDGER (in memory when NIL)."
  `(let ((*tabs-open* (copy-tree (or ,open '())))
         (*tabs-closed* '())
         (*tabs-requests* '())
         (*tabs-clock* ,now)
         (nodecode-tab-watchdog::*http* (tabs-fake-endpoint))
         (nodecode-tab-watchdog::*now* (lambda () *tabs-clock*))
         (nodecode-tab-watchdog::*ledger-file* ,ledger))
     ,@body))

(defun tabs-advance (seconds)
  "Move the fake clock forward."
  (incf *tabs-clock* seconds))

(defun tabs-request-p (path)
  "Whether the fake endpoint took a request whose path is PATH."
  (and (find path *tabs-requests* :key #'second :test #'equal) t))

(defun tabs-field (object key)
  "OBJECT's KEY, whether it is a hash keyed by string or keyword, or a plist."
  (cond ((hash-table-p object)
         (or (gethash key object)
             (gethash (string-downcase (string key)) object)
             (gethash (intern (string-upcase (string key)) :keyword) object)))
        ((consp object)
         (getf object (if (keywordp key) key (intern (string-upcase key) :keyword))))
        (t nil)))

(defun tabs-answer-text (answer)
  "The text of a tool's wire answer: its content, a string or typed parts."
  (let ((content (tabs-field answer :content)))
    (cond ((stringp content) content)
          ((or (hash-table-p content) (consp content))
           (or (tabs-field content :text) (princ-to-string content)))
          ((vectorp content)
           (with-output-to-string (out)
             (loop for part across content
                   do (let ((text (tabs-field part :text)))
                        (when text (write-string text out))))))
          (t (princ-to-string answer)))))

(defun tabs-tool-text (name &rest arguments)
  "The text the tool NAME answers, called as a provider's call is."
  (tabs-answer-text
   (execute-wire-call (format nil "tabs-call-~d" (incf *tabs-calls*)) name
                       (nlk:encode-json-object
                        (apply #'nlk:make-json-object arguments)))))

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "tab-watchdog"
  (:fixture with-fake-tabs ())
  (:config "auto" t)
  (:help :tab-watchdog)
  (:command "tabs")
  (:running (is (equal "http://127.0.0.1:9222" (nodecode-tab-watchdog::endpoint)) "the endpoint defaults to a loopback browser")
            (is (= 30 (nodecode-tab-watchdog::setting :blank-minutes)) "blank_minutes defaults to thirty")
            (is (nle::find-tool "tabs_inspect") "the report tool is registered")
            (is (nle::find-tool "tabs_sweep") "the sweep tool is registered")
            (is nodecode-tab-watchdog::*worker* "auto starts the sweep worker"))
  (:stopped (is (null nodecode-tab-watchdog::*worker*) "stop takes the worker down"))
  (:refused ("endpoint" "http://example.com:9222"))
  (:idle nodecode-tab-watchdog::tab-watchdog-error
         (nodecode-tab-watchdog:report)
         (nodecode-tab-watchdog:sweep :apply t)))

;;; --- the report -------------------------------------------------------------

(deftest tab-watchdog-cell-a-fresh-browser-has-nothing-to-close ()
  (with-fake-tabs (:open (list (tabs-tab "A" "Docs" "https://docs.example/")))
    (with-cell-stop ((tab-watchdog-start))
      (is-carrying (text (nodecode-tab-watchdog:report))
        ("1 page tab, 0 stale") ("Nothing to close."))
      (is (null *tabs-closed*) "the report closes nothing")
      (is (tabs-request-p "/json/list") "and it only asks for the tab list"))))

(deftest tab-watchdog-cell-a-blank-tab-is-closed-once-it-is-old ()
  (with-fake-tabs (:open (list (tabs-tab "A" "Mail" "https://mail.example/")
                                (tabs-tab "B" "" "about:blank")))
    (with-cell-stop ((tab-watchdog-start))
      (is-carrying (text (nodecode-tab-watchdog:report)) ("2 page tabs, 0 stale"))
      (tabs-advance (* 31 60))
      (is-carrying (text (nodecode-tab-watchdog:report))
        ("1 stale") ("blank for 31m") ("about:blank"))
      (is (null *tabs-closed*) "the report still closes nothing")
      (let ((answer (nodecode-tab-watchdog:sweep :apply t)))
        (is-carrying (text answer) ("closed 1") ("about:blank"))
        (is (equal '("B") *tabs-closed*) "the blank tab was closed")
        (is (equal '("A") (mapcar (lambda (tab) (getf tab :id)) *tabs-open*)) "the page it was opened beside stays")))))

(deftest tab-watchdog-cell-the-last-page-is-never-closed ()
  (with-fake-tabs (:open (list (tabs-tab "A" "Mail" "https://mail.example/")))
    (with-cell-stop ((tab-watchdog-start))
      (nodecode-tab-watchdog:report)
      (tabs-advance (* 9 3600))
      (is-carrying (text (nodecode-tab-watchdog:sweep :apply t)) ("A sweep would close none of them"))
      (is (null *tabs-closed*) "one page, and no sweep closes it"))))

(deftest tab-watchdog-cell-the-newer-copy-of-a-url-is-the-one-that-goes ()
  (with-fake-tabs (:open (list (tabs-tab "OLD" "Docs" "https://docs.example/")
                                (tabs-tab "NEW" "Docs" "https://docs.example/")
                                (tabs-tab "KEEP" "Mail" "https://mail.example/")))
    (with-cell-stop ((tab-watchdog-start))
      (nodecode-tab-watchdog:report)
      (tabs-advance (* 121 60))
      (let ((answer (nodecode-tab-watchdog:sweep :apply t)))
        (is-carrying (text answer) ("closed 1") ("duplicate for"))
        (is (search "(duplicate for" answer) (format nil "the sweep said: ~s" answer)))
      (is (equal '("NEW") *tabs-closed*) "the second copy closes, the one that showed the url first stays"))))

(deftest tab-watchdog-cell-a-tab-nothing-changes-in-is-closed ()
  (with-fake-tabs (:open (list (tabs-tab "A" "Mail" "https://mail.example/")
                                (tabs-tab "B" "News" "https://news.example/")))
    (with-cell-stop ((tab-watchdog-start))
      (nodecode-tab-watchdog:report)
      (tabs-advance (* 6 3600))
      (is-carrying (text (nodecode-tab-watchdog:sweep :apply t))
        ("closed 1") ("idle for"))
      (is (equal '("A") *tabs-closed*) "the tab nothing has changed in for six hours goes, the other stays"))))

(deftest tab-watchdog-cell-a-tab-that-changes-is-not-idle ()
  (with-fake-tabs (:open (list (tabs-tab "A" "Mail" "https://mail.example/")
                                (tabs-tab "B" "News" "https://news.example/")))
    (with-cell-stop ((tab-watchdog-start))
      (nodecode-tab-watchdog:report)
      (tabs-advance (* 3 3600))
      (setf *tabs-open* (list (tabs-tab "A" "Mail" "https://mail.example/")
                               (tabs-tab "B" "Breaking" "https://news.example/")))
      (nodecode-tab-watchdog:report)
      (tabs-advance (* 4 3600))
      (is-carrying (text (nodecode-tab-watchdog:report))
        ("1 stale") ("idle for"))
      (is (equal '("A") (mapcar (lambda (candidate) (getf candidate :id))
                                  (nodecode-tab-watchdog::candidates
                                   (nodecode-tab-watchdog::observe
                                    (nodecode-tab-watchdog::page-tabs))))) "a title that changed restarts the clock, so the news tab is not idle"))))

(deftest tab-watchdog-cell-the-ledger-outlives-a-restart ()
  (let ((path (merge-pathnames "tab-watchdog-cell-test/ledger.json" (nlk:home))))
    (unwind-protect
         (progn
           (when (probe-file path) (delete-file path))
           (with-fake-tabs (:ledger path
                            :open (list (tabs-tab "A" "Docs" "https://docs.example/")
                                        (tabs-tab "B" "Mail" "https://mail.example/")))
             (with-cell-stop ((tab-watchdog-start))
               (nodecode-tab-watchdog:report))
             (is (probe-file path) "the ledger a run leaves behind is written"))
           (with-fake-tabs (:ledger path :now (+ 1000000 (* 7 3600))
                            :open (list (tabs-tab "A" "Docs" "https://docs.example/")
                                        (tabs-tab "B" "Mail" "https://mail.example/")))
             (with-cell-stop ((tab-watchdog-start))
               (is-carrying (text (nodecode-tab-watchdog:report))
                 ("2 stale") ("idle for")))))
      (when (probe-file path) (delete-file path)))))

;;; --- the tools and the command ------------------------------------------------

(deftest tab-watchdog-cell-the-tools-and-the-command-answer ()
  (with-fake-tabs (:open (list (tabs-tab "A" "Mail" "https://mail.example/")
                                (tabs-tab "B" "" "about:blank")))
    (with-cell-stop ((tab-watchdog-start))
      (nodecode-tab-watchdog:report)
      (tabs-advance (* 45 60))
      (is-carrying (text (tabs-tool-text "tabs_inspect"))
        ("2 page tabs, 1 stale") ("about:blank"))
      (is-carrying (text (tabs-tool-text "tabs_sweep")) ("A sweep would close 1"))
      (is (null *tabs-closed*) "tabs_sweep without apply closes nothing")
      (is-carrying (text (tabs-tool-text "tabs_sweep" "apply" t)) ("closed 1"))
      (is (equal '("B") *tabs-closed*) "and with apply the blank tab goes")
      (multiple-value-bind (answer dialog)
          (cell-entry "nodecode-tab-watchdog" "tabs" "" nil)
        (is-carrying (text answer) ("1 page tab, 0 stale"))
        (is (null dialog) "a browser with nothing stale opens no dialog"))
      (tabs-advance (* 7 3600))
      (multiple-value-bind (answer dialog)
          (cell-entry "nodecode-tab-watchdog" "tabs" "" nil)
        (is-carrying (text answer) ("1 stale"))
        (is (equal "Tabs" (nlk:json-value dialog :string "title")) "a stale tab opens the tabs dialog"))
      (is-carrying (text (cell-entry "nodecode-tab-watchdog" "tabs" "sweep" nil))
        ("A sweep would close none of them")))))
