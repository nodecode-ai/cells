;;;; cell.lisp --- the report, the sweep, the ledger, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One Chromium browser's DevTools HTTP endpoint is the whole world of this
;;;; cell: /json/list says which pages are open, /json/close closes one.
;;;; Every look records each page in a ledger under the home -- when the cell
;;;; first saw it, when its url or title last changed -- and a tab is stale
;;;; when it has been blank for blank_minutes, when it is a second copy of a
;;;; url another tab already shows and has sat for duplicate_hours, or when
;;;; nothing about it has changed for idle_hours. SWEEP closes at most
;;;; max_close of them after a second look, never the last page, never every
;;;; blank tab, and looks once more to confirm each one is gone.
;;;;
;;;; The original ran a proxy every browser tool dialled, a CDP observer
;;;; session with a focus script, and per-tool leases. Nodecode's browser
;;;; tools do not pass through this cell, so the ages here are the cell's own
;;;; observations and there is nothing to lease. See NOTICE.

(in-package #:nodecode-tab-watchdog)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "A browser keeps a tab for every page it was driven to, and a browser tool leaves its blanks and
its duplicates behind. This cell watches one Chromium browser's DevTools HTTP endpoint and says
which tabs are stale: a blank tab left for blank_minutes, a second copy of a url another tab
already shows and left for duplicate_hours, or a tab nothing has changed in for idle_hours.
  (tab-watchdog:report)          every page tab, and which ones a sweep would close; touches none.
  (tab-watchdog:sweep :apply t)  close them: at most max_close, never the last page, never every
                                  blank tab, and each one is looked for again to confirm it is gone.
  (tab-watchdog:tabs)            the browser's own /json/list answer, as a list of plists.
(help :tab-watchdog) is this text; the tabs_inspect, tabs_sweep tools and the /tabs command do
the same. Ages come from the cell's own looks (first seen, last change), not from Chrome: a page
being read whose title never changes looks idle, so read the report before a scheduled sweep.
The endpoint is the browser's --remote-debugging-port address, http://127.0.0.1:9222 by default."
  "What (help :tab-watchdog) answers while the cell runs.")

;;; --- the seams a test rebinds ------------------------------------------------

(defvar *http* nil
  "The cell's one network seam: (METHOD URL &key timeout connect-timeout) => (values BODY STATUS).")

(defvar *now* #'get-universal-time
  "The clock, in universal time; a test rebinds it to walk hours in a step.")

(defvar *ledger-file* :default
  "Where the ledger is kept: :DEFAULT under the home, NIL in memory only.")

(defvar *seen* (make-hash-table :test #'equal)
  "The ledger: target id -> (:first-seen :changed :url :title), in universal time.")

(defvar *worker* nil
  "The sweep worker when auto is on, or NIL.")

;;; --- small text -------------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, or NIL when it is empty or not a string."
  (let ((trimmed (and (stringp text) (nlk:trimmed text))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun shorten (text limit)
  "TEXT cut to LIMIT characters, its tail said with an ellipsis."
  (let ((text (or text "")))
    (if (<= (length text) limit) text (format nil "~a..." (subseq text 0 (- limit 3))))))

(defun duration (seconds)
  "SECONDS as the shortest phrase that still says the size: 45m, 2h 10m, 3d 4h."
  (let ((minutes (max 0 (floor (or seconds 0) 60))))
    (cond ((< minutes 60) (format nil "~dm" minutes))
          ((< minutes 1440) (format nil "~dh ~02dm" (floor minutes 60) (mod minutes 60)))
          (t (format nil "~dd ~02dh" (floor minutes 1440) (floor (mod minutes 1440) 60))))))

;;; --- the ledger, across a restart --------------------------------------------

(defun ledger-path ()
  "Where the ledger of this home lives, or NIL when it is kept in memory only."
  (cond ((eq *ledger-file* :default) (nlk:home "tab-watchdog/ledger.json"))
        (t *ledger-file*)))

(defun load-ledger ()
  "The ledger an earlier run wrote, as a hash; an unreadable file is an empty ledger."
  (let ((table (make-hash-table :test #'equal))
        (path (ledger-path)))
    (when (and path (probe-file path))
      (ignore-errors
        (let ((entries (shasht:read-json (uiop:read-file-string path))))
          (loop for entry across (if (vectorp entries) entries #())
                for id = (nlk:json-value entry :string "id")
                when id
                  do (setf (gethash id table)
                           (list :first-seen (nlk:json-value entry :integer "first_seen")
                                 :changed (nlk:json-value entry :integer "changed")
                                 :url (or (nlk:json-value entry :string "url") "")
                                 :title (or (nlk:json-value entry :string "title") "")))))))
    table))

(defun save-ledger ()
  "Write the ledger where an earlier run reads it; failing to write is not failing to watch."
  (let ((path (ledger-path)))
    (when path
      (ignore-errors
        (ensure-directories-exist path)
        (let* ((text (nlk:encode-json-object
                      (coerce (loop for id being the hash-keys of *seen*
                                       using (hash-value entry)
                                     collect (nlk:make-json-object
                                              "id" id
                                              "first_seen" (or (getf entry :first-seen) 0)
                                              "changed" (or (getf entry :changed) 0)
                                              "url" (or (getf entry :url) "")
                                              "title" (or (getf entry :title) "")))
                              'vector)))
               (temporary (merge-pathnames (format nil "ledger-~d.tmp" (random 1000000)) path)))
          (with-open-file (out temporary :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
            (write-string text out))
          (rename-file temporary path))))))

;;; --- the browser's DevTools HTTP routes ---------------------------------------

(defun endpoint ()
  "The browser's CDP HTTP address, without a trailing slash."
  (let ((text (or (nonblank (setting :endpoint)) "http://127.0.0.1:9222")))
    (string-right-trim "/" (if (search "://" text) text (format nil "http://~a" text)))))

(defun check-config ()
  "Refuse an endpoint that is not a loopback HTTP address, naming the member to set."
  (let ((text (string-downcase (endpoint))))
    (unless (or (uiop:string-prefix-p "http://127.0.0.1" text)
                (uiop:string-prefix-p "http://localhost" text)
                (uiop:string-prefix-p "http://[::1]" text))
      (error 'nlk:config-refusal
             :detail (format nil "tab-watchdog: endpoint is ~s, which is not a loopback HTTP ~
address; set endpoint in the tab-watchdog section to http://127.0.0.1:9222, or to the port your ~
browser's --remote-debugging-port uses"
                              (setting :endpoint))))))

(defun call (route &key (method :get))
  "One DevTools HTTP exchange at ROUTE: (values BODY STATUS), an unreachable endpoint as 0."
  (handler-case
      (multiple-value-bind (body status)
          (funcall *http* method (format nil "~a~a" (endpoint) route) :timeout 3 :connect-timeout 2)
        (values body (or status 0)))
    (error () (values nil 0))))

(defun json-at (route)
  "The decoded answer at ROUTE, or NIL when it does not answer or is not JSON."
  (multiple-value-bind (body status) (call route)
    (when (and (integerp status) (< 0 status 300) (stringp body) (plusp (length body)))
      (ignore-errors (shasht:read-json body)))))

(defun url-encode (text)
  "TEXT with everything but a URL's own reserved characters percent-escaped."
  (with-output-to-string (out)
    (loop for character across text
          do (if (or (alphanumericp character)
                     (find character "-._~:/?&=" :test #'char=))
                 (write-char character out)
                 (format out "%~2,'0X" (char-code character))))))

(defun page-tabs ()
  "Every open page target the endpoint answers, as a plist of :ID :URL :TITLE."
  (let ((answer (json-at "/json/list")))
    (unless answer
      (fail "~a answered no tab list: is a Chromium browser running with --remote-debugging-port?"
            (endpoint)))
    (loop for tab across (if (vectorp answer) answer #())
          when (equal "page" (or (nlk:json-value tab :string "type") "page"))
            collect (list :id (nlk:json-value tab :string "id")
                          :url (or (nlk:json-value tab :string "url") "")
                          :title (or (nlk:json-value tab :string "title") "")))))

;;; --- the ledger, one look at a time ------------------------------------------

(defun observe (tabs)
  "Record TABS in the ledger and answer them with :FIRST-SEEN and :CHANGED attached."
  (let ((now (funcall *now*)))
    (dolist (tab tabs)
      (let* ((id (getf tab :id))
             (entry (gethash id *seen*)))
        (setf (gethash id *seen*)
              (if entry
                  (list :first-seen (or (getf entry :first-seen) now)
                        :changed (if (and (equal (getf entry :url) (getf tab :url))
                                          (equal (getf entry :title) (getf tab :title)))
                                     (or (getf entry :changed) now)
                                     now)
                        :url (getf tab :url)
                        :title (getf tab :title))
                  (list :first-seen now :changed now
                        :url (getf tab :url) :title (getf tab :title))))))
    ;; A tab the endpoint no longer lists is gone: forget it.
    (let ((open (loop for tab in tabs collect (getf tab :id))))
      (loop for id being the hash-keys of *seen*
            unless (member id open :test #'equal) do (remhash id *seen*)))
    (loop for tab in tabs
          for entry = (gethash (getf tab :id) *seen*)
          collect (append (list :id (getf tab :id) :url (getf tab :url) :title (getf tab :title))
                          (list :first-seen (getf entry :first-seen)
                                :changed (getf entry :changed))))))

;;; --- what is stale ------------------------------------------------------------

(defparameter +blank-urls+
  '("" "about:blank" "about:newtab" "chrome://newtab/" "chrome://new-tab-page/"
    "edge://newtab/" "brave://newtab/")
  "The pages a browser shows with nothing in them.")

(defun blank-url-p (url)
  "Whether URL is a page with nothing in it."
  (member (string-downcase (or url "")) +blank-urls+ :test #'equal))

(defun age-of (entry now)
  "How long ENTRY has been open, in seconds."
  (- now (or (getf entry :first-seen) now)))

(defun quiet-of (entry now)
  "How long since anything about ENTRY changed, in seconds."
  (- now (or (getf entry :changed) now)))

(defun duplicate-losers (entries)
  "The ids of the tabs that repeat a url another tab of ENTRIES has shown longer."
  (let ((now (funcall *now*))
        (groups (make-hash-table :test #'equal))
        (losers '()))
    (dolist (entry entries)
      (unless (blank-url-p (getf entry :url))
        (push entry (gethash (getf entry :url) groups))))
    (maphash (lambda (url group)
               (declare (ignore url))
               (let ((sorted (stable-sort (reverse group) #'<
                                           :key (lambda (entry) (or (getf entry :first-seen) 0)))))
                 (dolist (entry (rest sorted))
                   (when (<= (* 3600 (setting :duplicate-hours)) (age-of entry now))
                     (push (getf entry :id) losers)))))
             groups)
    losers))

(defun candidates (entries)
  "ENTRIES that are stale, oldest first, each with :REASON (:BLANK, :DUPLICATE or :IDLE)."
  (let ((now (funcall *now*))
        (losers (duplicate-losers entries)))
    (sort (loop for entry in entries
                for id = (getf entry :id)
                for reason = (cond ((member id losers :test #'equal) :duplicate)
                                   ((and (blank-url-p (getf entry :url))
                                         (<= (* 60 (setting :blank-minutes)) (age-of entry now)))
                                    :blank)
                                   ((<= (* 3600 (setting :idle-hours)) (quiet-of entry now)) :idle))
                when reason collect (append entry (list :reason reason)))
          #'< :key (lambda (entry) (or (getf entry :first-seen) 0)))))

(defun keep-one-blank (candidates)
  "CANDIDATES without the newest blank one when a sweep would close every blank tab."
  (let ((blanks (remove-if-not (lambda (candidate) (blank-url-p (getf candidate :url)))
                                candidates)))
    (if (> (length blanks) 1)
        (let ((newest (first (sort (copy-list blanks) #'>
                                    :key (lambda (candidate) (or (getf candidate :first-seen) 0))))))
          (remove newest candidates))
        candidates)))

(defun closeable (entries candidates)
  "CANDIDATES a sweep may close now: at most max_close, never the last page, never every blank tab."
  (let ((budget (min (setting :max-close) (1- (length entries))))
        (wanted (keep-one-blank candidates)))
    (when (plusp budget)
      (subseq wanted 0 (min budget (length wanted))))))

;;; --- the texts ---------------------------------------------------------------

(defun tab-line (entry candidate now)
  "One report line: the tab, and why it is stale or how long it has been quiet."
  (format nil "  ~:[~;!~] ~a  ~a  ~a"
          (and candidate t)
          (shorten (or (nonblank (getf entry :title)) "(untitled)") 48)
          (shorten (or (nonblank (getf entry :url)) "(no url)") 64)
          (if candidate
              (format nil "~(~a~) for ~a"
                      (getf candidate :reason)
                      (duration (if (eq (getf candidate :reason) :idle)
                                    (quiet-of entry now)
                                    (age-of entry now))))
              (format nil "quiet for ~a" (duration (quiet-of entry now))))))

(defun report-text (entries candidates &key all)
  "The report /tabs and the tabs_inspect tool answer."
  (let* ((now (funcall *now*))
         (shown (if all entries (remove-if-not (lambda (entry)
                                                 (find (getf entry :id) candidates
                                                       :key (lambda (candidate) (getf candidate :id))
                                                       :test #'equal))
                                               entries)))
         (lines (list (format nil "~a -- ~d page tab~:p, ~d stale."
                              (endpoint) (length entries) (length candidates)))))
    (dolist (entry (subseq shown 0 (min (length shown) 20)))
      (push (tab-line entry (find (getf entry :id) candidates
                                  :key (lambda (candidate) (getf candidate :id)) :test #'equal)
                       now)
            lines))
    (when (> (length shown) 20)
      (push (format nil "  ... and ~d more" (- (length shown) 20)) lines))
    (cond ((null entries) (push "Nothing is open." lines))
          ((null candidates) (push "Nothing to close." lines))
          (t (let ((open (length (closeable entries candidates))))
               (push (if (plusp open)
                         (format nil "A sweep would close ~d of them." open)
                         "A sweep would close none of them: a browser keeps at least one page.")
                     lines))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun sweep-text (closed failed lingering kept remaining &key all)
  "The text a sweep answers: what went, what would not, and what is left."
  (let ((now (funcall *now*))
        (lines '()))
    (flet ((line (control &rest args) (push (apply #'format nil control args) lines)))
      (line "~a -- ~d page tab~:p, closed ~d." (endpoint) remaining (length closed))
      (dolist (candidate closed)
        (line "  closed  ~a  ~a  (~(~a~) for ~a)"
              (shorten (or (nonblank (getf candidate :title)) "(untitled)") 48)
              (shorten (or (nonblank (getf candidate :url)) "(no url)") 64)
              (getf candidate :reason) (duration (age-of candidate now))))
      (dolist (pair failed)
        (line "  stayed  ~a  ~a  (http ~a)"
              (shorten (or (nonblank (getf (car pair) :title)) "(untitled)") 48)
              (shorten (or (nonblank (getf (car pair) :url)) "(no url)") 64)
              (cdr pair)))
      (dolist (id lingering)
        (line "  still listed after closing: ~a" id))
      (when (null closed)
        (line "  nothing closed~@[ (~d stayed)~]" (and failed (length failed))))
      (when all
        (dolist (entry keep)
          (line "  kept    ~a  ~a"
                (shorten (or (nonblank (getf entry :title)) "(untitled)") 48)
                (shorten (or (nonblank (getf entry :url)) "(no url)") 64)))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

;;; --- the verbs ---------------------------------------------------------------

(defun raw-tabs (&optional (entries (page-tabs)))
  "ENTRIES as a list of (:id :url :title) plists."
  (loop for entry in entries
        collect (list :id (getf entry :id) :url (getf entry :url) :title (getf entry :title))))

(defun report (&key all)
  "Every page tab of the watched browser, and which ones a sweep would close; touches nothing."
  (running-settings)
  (with-tab-watchdog-lock
    (let* ((entries (observe (page-tabs)))
           (candidates (candidates entries)))
      (save-ledger)
      (values (report-text entries candidates :all all) (length candidates)))))

(defun sweep (&key apply all)
  "Close the stale tabs when APPLY; answers the text, and the ids it closed as a second value."
  (running-settings)
  (with-tab-watchdog-lock
    (let* ((entries (observe (page-tabs)))
           (candidates (candidates entries))
           (doomed (closeable entries candidates)))
      (if (or (null doomed) (not apply))
          (progn (save-ledger)
                 (values (report-text entries candidates :all all) nil))
          (let ((closed '())
                (failed '()))
            ;; A second look: the tab is there, with the same url, before it goes.
            (let* ((again (observe (page-tabs)))
                   (still (loop for candidate in doomed
                                for entry = (find (getf candidate :id) again
                                                  :key (lambda (one) (getf one :id))
                                                  :test #'equal)
                                when (and entry (equal (getf entry :url) (getf candidate :url)))
                                  collect candidate)))
              (dolist (candidate still)
                (multiple-value-bind (body status)
                    (call (format nil "/json/close/~a" (url-encode (getf candidate :id))))
                  (declare (ignore body))
                  (if (and (integerp status) (< 0 status 300))
                      (push candidate closed)
                      (push (cons candidate status) failed)))))
            (setf closed (nreverse closed)
                  failed (nreverse failed))
            (let* ((after (observe (page-tabs)))
                   (lingering (loop for candidate in closed
                                    when (find (getf candidate :id) after
                                               :key (lambda (one) (getf one :id))
                                               :test #'equal)
                                      collect (getf candidate :id))))
              (save-ledger)
              (values (sweep-text closed failed lingering after (length after) :all all)
                      (mapcar (lambda (candidate) (getf candidate :id)) closed))))))))

(defun tabs ()
  "The watched browser's own /json/list answer, one plist of :id :url :title per page."
  (running-settings)
  (raw-tabs))

;;; --- the tools and the command -----------------------------------------------

(nlk:define-contract inspect-arguments
  (:doc "Look at the open tabs of the browser this cell watches, and say which ones a sweep would\nclose: a blank tab left for blank_minutes, a second copy of a url another tab already shows and\nleft for duplicate_hours, or a tab nothing has changed in for idle_hours. Read-only: nothing is\nclosed. Run it before tabs_sweep, and whenever a sweep's list looks wrong.")
  ("all" :boolean :default nil :doc "list every tab, not only the ones a sweep would close"))

(nlk:define-contract sweep-arguments
  (:doc "Close the stale tabs of the browser this cell watches: the report first, then the\ncandidates go, at most max_close of them, never the last page and never every blank tab.\nEach tab is looked for twice before it is closed and once after, so what it answers is what\nhappened. With apply false it only reports what a sweep would close.")
  ("apply" :boolean :default nil :slot applied
   :doc "close them; when false, only report what a sweep would close")
  ("all" :boolean :default nil :doc "list every tab, including the ones kept"))

(defun run-inspect (arguments)
  "The tabs_inspect tool: the report, one value of text."
  (let ((text (report :all (inspect-arguments-all arguments))))
    text))

(defun run-sweep (arguments)
  "The tabs_sweep tool: close the stale tabs, or say what a sweep would close."
  (let ((text (sweep :apply (sweep-arguments-applied arguments)
                      :all (sweep-arguments-all arguments))))
    text))

(defun panel (entries candidates)
  "The dialog bare /tabs answers: every open tab, the stale ones marked, and one row to sweep."
  (nle:list-dialog
   :title "Tabs"
   :context (format nil "~d open · ~d stale" (length entries) (length candidates))
   :rows (append
          (when candidates
            (list (list :mark "!" :label (format nil "Close ~d stale tab~:p" (length candidates))
                        :detail "enter sweeps the ones marked here"
                        :run (list :enter "/tabs sweep"))))
          (loop for entry in entries
                for candidate = (find (getf entry :id) candidates
                                      :key (lambda (one) (getf one :id)) :test #'equal)
                collect (list :mark (if candidate "!" " ")
                              :label (shorten (or (nonblank (getf entry :title)) "(untitled)") 48)
                              :detail (shorten (or (nonblank (getf entry :url)) "(no url)") 64)
                              :suffix (and candidate
                                            (string-downcase
                                             (string (getf candidate :reason)))))))
   :hint "enter sweeps  esc close"))

(defun run-command (arguments session-id)
  "The /tabs command: the report, or `sweep`, and the dialog the report's list opens."
  (declare (ignore session-id))
  (let ((argument (string-downcase (or (nlk:trimmed (or arguments "")) ""))))
    (if (member argument '("sweep" "close" "apply") :test #'equal)
        (values (sweep :apply t :all t) nil)
        (let (entries candidates)
          (running-settings)
          (with-tab-watchdog-lock
            (setf entries (observe (page-tabs))
                  candidates (candidates entries))
            (save-ledger))
          (values (report-text entries candidates :all (equal argument "all"))
                  (and candidates (panel entries candidates)))))))

;;; --- start and stop -----------------------------------------------------------

(defun worker-step ()
  "One scheduled look: close what may go, and say so when something did."
  (handler-case
      (let ((closed (nth-value 1 (sweep :apply t))))
        (when closed
          (nle:notice (format nil "tab-watchdog: closed ~d stale tab~:p" (length closed))
                      :key "nodecode-tab-watchdog")))
    (error () nil)))

(defun start ()
  "Read the ledger an earlier run left, and sweep on a schedule when auto is on."
  (check-config)
  (setf *seen* (load-ledger)
        *worker* nil)
  (when (setting :auto)
    (setf *worker* (nlk:worker-start "tab-watchdog" #'worker-step
                                     :wake (* 60 (setting :sweep-minutes)))))
  (nle:on-stop (lambda ()
                 (when *worker*
                   (nlk:worker-stop *worker*)
                   (setf *worker* nil))
                 (save-ledger))))

(setf *http* #'nlk:http)

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell tab-watchdog
  (:section ("tab-watchdog")
    (:guide "endpoint is a Chromium browser's DevTools address, on a browser started with --remote-debugging-port=9222; blank_minutes, duplicate_hours and idle_hours say when a tab is stale, max_close bounds one sweep, and auto sweeps every sweep_minutes; then (tab-watchdog:report) from eval, the tabs_inspect tool, or /tabs")
    ("endpoint" :string :default "http://127.0.0.1:9222"
     :doc "the browser's CDP HTTP endpoint, on loopback")
    ("blank_minutes" :integer :default 30 :min 5
     :doc "how long a blank tab is left alone before it is stale")
    ("idle_hours" :integer :default 6 :min 1
     :doc "how long a tab nothing has changed in is stale")
    ("duplicate_hours" :integer :default 2 :min 1
     :doc "how long a second copy of a url must sit before it is stale")
    ("max_close" :integer :default 5 :min 1
     :doc "the most tabs one sweep closes")
    ("auto" :boolean :default nil
     :doc "sweep every sweep_minutes on its own")
    ("sweep_minutes" :integer :default 15 :min 1
     :doc "how often an automatic sweep looks"))
  (:help :tab-watchdog
         "tab-watchdog: close stale blank, idle and duplicate Chrome tabs"
         +primer+)
  (:tool "tabs_inspect" (nlk:contract 'inspect-arguments) 'run-inspect)
  (:tool "tabs_sweep" (nlk:contract 'sweep-arguments) 'run-sweep)
  (:command "tabs" 'run-command
            :description "Stale Chrome tabs: the report, and the sweep"
            :argument-hint "[sweep|all]")
  (:start #'start))
