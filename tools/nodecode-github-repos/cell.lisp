;;;; cell.lisp --- your GitHub repositories as text: a list, a total, a command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes dashboard plugin: a statusbar chip, a palette
;;;; command and a dashboard tab that list the repositories of the account the
;;;; gh CLI is signed in as. This cell keeps what the tab computes and gives
;;;; it to the model and to the person typing, instead of a window: the same
;;;; REST endpoint (/user/repos, the original's own), the same per-repository
;;;; fields, the same sort keys, the same visibility filter, the same
;;;; 60-second cache and the same 500-repository ceiling, answered as text.
;;;;
;;;; Every read is one `gh api' call. gh is the user's own, already signed
;;;; in, so the cell holds no token and reads no credential file: the only
;;;; thing that leaves the machine is the request gh makes to the GitHub API.
;;;;
;;;; Tests rebind *RUN*, the one process seam, so nothing here needs gh.

(in-package #:nodecode-github-repos)

(nlk:define-peripheral github-repos :not-running t)

(defparameter +sorts+ '("pushed" "stars" "forks" "created" "name")
  "The orders a list can be read in, as the original tab offered them.")

(defparameter +visibilities+ '("all" "public" "private" "fork" "archived")
  "What a list can be narrowed to.")

(defparameter +per-page+ 100
  "The page size the GitHub REST API allows; the original read pages of 100.")

(defparameter +description-chars+ 120
  "How much of one description a line shows.")

(defvar *run* nil
  "The one process seam: (PROGRAM ARGS TIMEOUT) => (values OUTPUT EXIT-CODE).
NIL means the real subprocess, REAL-RUN. Tests rebind it.")

(defvar *cache* nil
  "Fetched repository lists: (ACCOUNT SECONDS . REPOS), newest first.")

;;; --- the process seam ---------------------------------------------------------------

(defun real-run (program args timeout)
  "Run PROGRAM with ARGS, its standard error with its output, its output
bounded by a file and its time by TIMEOUT. => (values OUTPUT EXIT-CODE),
EXIT-CODE NIL when it outlived TIMEOUT and was stopped."
  (let ((path (merge-pathnames (format nil "github-repos-~d-~d.txt"
                                        (get-universal-time) (random 1000000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (unwind-protect
         (handler-case
             (let* ((process (uiop:launch-program (cons program args)
                                                   :output path
                                                   :error-output :output
                                                   :if-output-exists :supersede
                                                   :external-format :utf-8))
                    (deadline (+ (get-internal-real-time)
                                 (* (max 1 timeout) internal-time-units-per-second)))
                    (code nil))
               (loop while (uiop:process-alive-p process)
                     do (if (> (get-internal-real-time) deadline)
                            (progn (ignore-errors (uiop:terminate-process process :urgent t))
                                   (return))
                            (sleep 0.02)))
               (setf code (ignore-errors (uiop:wait-process process)))
               (values (if (probe-file path)
                           (uiop:read-file-string path :external-format :utf-8)
                           "")
                       code))
           (error (e)
             (fail "cannot run ~a: ~a" program e)))
      (ignore-errors (delete-file path)))))

(defun run-gh (&rest args)
  "One gh command. => (values OUTPUT EXIT-CODE)."
  (funcall (or *run* #'real-run) (setting :gh) args (setting :timeout-seconds)))

(defun one-line (text)
  "TEXT as one line, short enough to sit in a refusal."
  (let* ((text (or text ""))
         (newline (position #\Newline text)))
    (when newline (setf text (subseq text 0 newline)))
    (when (> (length text) 240)
      (setf text (concatenate 'string (subseq text 0 240) "...")))
    (nlk:trimmed text)))

(defun gh-api (path)
  "PATH as the JSON `gh api' answers, or a refusal carrying gh's own words."
  (multiple-value-bind (output code) (run-gh "api" path)
    (unless (eql 0 code)
      (fail "gh api ~a failed: ~a" path
            (let ((text (one-line output)))
              (if (plusp (length text)) text "no answer, and gh is not signed in?"))))
    (handler-case (nlk:decode-json (or output ""))
      (error (e) (fail "gh api ~a answered no JSON: ~a" path (one-line (princ-to-string e)))))))

;;; --- what a section and a call name ------------------------------------------------

(defun checked-settings (values table)
  "VALUES as the cell runs on them, or a refusal naming the member it cannot honour."
  ;; A blank :STRING member reads as its default, so the member is read here
  ;; off the section's own object: gh named and then emptied is a section that
  ;; names no program.
  (multiple-value-bind (gh present) (and (hash-table-p table) (gethash "gh" table))
    (when (and present (stringp gh) (zerop (length (nlk:trimmed gh))))
      (error 'nlk:config-refusal
             :detail (concatenate
                      'string
                      "github-repos: \"gh\" is the GitHub CLI every read runs; "
                      "name the program, gh when it is on PATH"))))
  values)

(defun account-arg (account)
  "ACCOUNT when the call names one, else the section's, with only a login's characters."
  (let ((account (nlk:trimmed (or account ""))))
    (when (zerop (length account))
      (setf account (nlk:trimmed (or (setting :account) ""))))
    (unless (zerop (length account))
      (unless (every (lambda (char) (or (alphanumericp char)
                                        (member char '(#\- #\_ #\.))))
                     account)
        (fail "~s is no GitHub login" account)))
    account))

(defun sort-arg (sort)
  "SORT when it names an order, else the section's."
  (or (first (member (nlk:trimmed (or sort "")) +sorts+ :test #'string-equal))
      (setting :sort)))

(defun visibility-arg (visibility)
  "VISIBILITY when it names one of the filters, else all."
  (or (first (member (nlk:trimmed (or visibility "")) +visibilities+ :test #'string-equal))
      "all"))

;;; --- reading the repositories ------------------------------------------------------

(defun repos-path (account page)
  "The REST path for page PAGE of ACCOUNT's repositories, or this account's own."
  (format nil "~a?per_page=~d&page=~d&sort=pushed~a"
          (if (plusp (length account))
              (format nil "/users/~a/repos" account)
              "/user/repos")
          +per-page+ page
          (if (plusp (length account)) "" "&affiliation=owner")))

(defun fetch-page (account page)
  "One page of ACCOUNT's repositories, as decoded JSON."
  (let ((batch (gh-api (repos-path account page))))
    (coerce (if (vectorp batch) batch #()) 'list)))

(defun fetch-repos (account)
  "Every repository ACCOUNT owns, up to the section's max_pages pages of 100."
  (let ((repos '())
        (page 1)
        (pages (setting :max-pages)))
    (loop while (<= page pages)
          do (let ((batch (fetch-page account page)))
               (setf repos (nconc repos batch))
               (if (< (length batch) +per-page+)
                   (return)
                   (incf page))))
    repos))

(defun cached-repos (account)
  "ACCOUNT's repositories, re-read once the section's cache_seconds have passed."
  (let* ((ttl (setting :cache-seconds))
         (hit (assoc account *cache* :test #'string=)))
    (if (and hit (plusp ttl) (< (- (get-universal-time) (second hit)) ttl))
        (cddr hit)
        (let ((repos (fetch-repos account)))
          (when (plusp ttl)
            (setf *cache* (cons (list* account (get-universal-time) repos)
                                (remove account *cache* :key #'first :test #'string=))))
          repos))))

;;; --- what one repository says -------------------------------------------------------

(defun repo-string (repo key)
  (or (nlk:json-value repo :string key) ""))

(defun repo-number (repo key)
  (or (nlk:json-value repo :integer key) 0))

(defun repo-flag (repo key)
  (and (nlk:json-value repo :boolean key) t))

(defun repo-name (repo)
  (repo-string repo "full_name"))

(defun login-of (account repos)
  "The login ACCOUNT names, else the owner of REPOS' first entry, else empty."
  (if (plusp (length account))
      account
      (let ((name (repo-name (first repos))))
        (if (plusp (length name))
            (subseq name 0 (or (position #\/ name) (length name)))
            ""))))

(defun visible-p (repo visibility)
  "True when REPO passes the VISIBILITY filter."
  (cond ((string= visibility "public") (not (repo-flag repo "private")))
        ((string= visibility "private") (repo-flag repo "private"))
        ((string= visibility "fork") (repo-flag repo "fork"))
        ((string= visibility "archived") (repo-flag repo "archived"))
        (t t)))

(defun matches-p (repo query)
  "True when QUERY is in REPO's name, description or language."
  (let ((query (string-downcase query)))
    (or (zerop (length query))
        (some (lambda (key) (search query (string-downcase (repo-string repo key))))
              '("name" "description" "language")))))

(defun sort-key (repo sort)
  "REPO's value for SORT."
  (cond ((string= sort "stars") (repo-number repo "stargazers_count"))
        ((string= sort "forks") (repo-number repo "forks_count"))
        ((string= sort "name") (string-downcase (repo-string repo "name")))
        ((string= sort "created") (repo-string repo "created_at"))
        (t (repo-string repo "pushed_at"))))

(defun sort-repos (repos sort)
  "REPOS in SORT's order: the counts high to low, the dates and the names ascending."
  (let ((rows (copy-list repos)))
    (cond ((member sort '("stars" "forks") :test #'string=)
           (sort rows #'> :key (lambda (repo) (sort-key repo sort))))
          ((string= sort "name")
           (sort rows #'string< :key (lambda (repo) (sort-key repo sort))))
          (t (sort rows #'string> :key (lambda (repo) (sort-key repo sort)))))))

(defparameter +months+ 30
  "How many days a month is taken to be in an age.")

(defun date-days (iso)
  "The universal time of ISO's date part at midnight UTC, or NIL."
  (let ((iso (nlk:trimmed (or iso ""))))
    (when (>= (length iso) 10)
      (let ((year (parse-integer iso :start 0 :end 4 :junk-allowed t))
            (month (parse-integer iso :start 5 :end 7 :junk-allowed t))
            (day (parse-integer iso :start 8 :end 10 :junk-allowed t)))
        (when (and year month day)
          (ignore-errors (encode-universal-time 0 0 0 day month year 0)))))))

(defun short-age (iso)
  "ISO as an age a person reads: today, yesterday, 3d ago, 5mo ago, or the date."
  (let ((then (date-days iso)))
    (if (null then)
        "?"
        (let ((days (max 0 (floor (- (get-universal-time) then) 86400))))
          (cond ((zerop days) "today")
                ((= days 1) "yesterday")
                ((< days +months+) (format nil "~dd ago" days))
                ((< days 365) (format nil "~dmo ago" (floor days +months+)))
                (t (subseq (nlk:trimmed iso) 0 10)))))))

(defun clipped (text limit)
  "TEXT cut to LIMIT characters, saying so."
  (let ((text (nlk:trimmed (or text ""))))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "..."))))

(defun repo-line (repo)
  "One repository as one line: what it is, how it stands, when it last moved."
  (let* ((language (repo-string repo "language"))
         (issues (repo-number repo "open_issues_count"))
         (pushed (repo-string repo "pushed_at"))
         (description (repo-string repo "description")))
    (with-output-to-string (out)
      (format out "~a~@[ [private]~]~@[ [fork]~]~@[ [archived]~]  stars ~d  forks ~d"
              (repo-name repo)
              (repo-flag repo "private")
              (repo-flag repo "fork")
              (repo-flag repo "archived")
              (repo-number repo "stargazers_count")
              (repo-number repo "forks_count"))
      (when (plusp (length language)) (format out "  ~a" language))
      (when (plusp issues) (format out "  ~d issues" issues))
      (when (plusp (length pushed)) (format out "  pushed ~a" (short-age pushed)))
      (when (plusp (length description))
        (format out " - ~a" (clipped description +description-chars+))))))

;;; --- the answers -------------------------------------------------------------------

(defun kept-repos (repos visibility query)
  "REPOS that pass the VISIBILITY filter and hold QUERY."
  (remove-if-not (lambda (repo)
                   (and (visible-p repo visibility) (matches-p repo query)))
                 repos))

(defun render-repos (&key query visibility sort limit account)
  "The list QUERY, VISIBILITY, SORT and LIMIT ask for, as text."
  (let* ((account (account-arg account))
         (repos (cached-repos account))
         (sort (sort-arg sort))
         (visibility (visibility-arg visibility))
         (query (nlk:trimmed (or query "")))
         (limit (if (and (integerp limit) (plusp limit)) limit (setting :limit)))
         (kept (kept-repos repos visibility query))
         (rows (sort-repos kept sort))
         (shown (subseq rows 0 (min (length rows) limit)))
         (login (login-of account repos)))
    (with-output-to-string (out)
      (format out "GitHub repositories~@[ of ~a~]: ~d~%" login (length repos))
      (format out "  ~d match~:p, ~d shown, sorted by ~a~@[, ~a only~]~@[, query \"~a\"~]~%"
              (length kept) (length shown) sort
              (unless (string= visibility "all") visibility)
              (and (plusp (length query)) query))
      (if (plusp (length repos))
          (dolist (repo shown) (write-line (repo-line repo) out))
          (format out "no repositories: is gh signed in? (gh auth login)~%"))
      (when (> (length rows) (length shown))
        (format out "[~d more; narrow them with query or visibility, or raise limit]~%"
                (- (length rows) (length shown)))))))

(defun language-counts (repos)
  "REPOS' languages, the most used first: (NAME . COUNT) ..."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (repo repos)
      (let ((language (repo-string repo "language")))
        (when (plusp (length language))
          (incf (gethash language counts 0)))))
    (let ((pairs '()))
      (maphash (lambda (name count) (push (cons name count) pairs)) counts)
      (subseq (sort pairs #'> :key #'cdr) 0 (min 8 (length pairs))))))

(defun render-summary (&key account)
  "The totals of ACCOUNT's repositories, as text."
  (let* ((account (account-arg account))
         (repos (cached-repos account))
         (login (login-of account repos))
         (languages (language-counts repos)))
    (with-output-to-string (out)
      (format out "GitHub repositories~@[ of ~a~]: ~d~%" login (length repos))
      (format out "  ~d public, ~d private, ~d forks, ~d archived~%"
              (count-if-not (lambda (repo) (repo-flag repo "private")) repos)
              (count-if (lambda (repo) (repo-flag repo "private")) repos)
              (count-if (lambda (repo) (repo-flag repo "fork")) repos)
              (count-if (lambda (repo) (repo-flag repo "archived")) repos))
      (format out "  ~d stars, ~d open issues~%"
              (reduce #'+ repos :key (lambda (repo) (repo-number repo "stargazers_count"))
                      :initial-value 0)
              (reduce #'+ repos :key (lambda (repo) (repo-number repo "open_issues_count"))
                      :initial-value 0))
      (when languages
        (format out "  languages: ~{~a (~d)~^, ~}~%"
                (loop for (name . count) in languages append (list name count))))
      (when (zerop (length repos))
        (format out "  is gh signed in? (gh auth login)~%")))))

;;; --- the boundaries -----------------------------------------------------------------

(nlk:define-contract repos-request
  (:doc "List the GitHub repositories of the account gh is signed in as, or of another account: one line each with visibility, stars, forks, language and when it last moved.")
  ("query" :string :default "" :doc "words to find in a repository's name, description or language; empty lists every repository")
  ("visibility" :text :default "all" :options (list "all" "public" "private" "fork" "archived")
   :doc "all, or only public, private, forked or archived repositories")
  ("sort" :string :default "" :slot sort-order :options (list "pushed" "stars" "forks" "created" "name")
   :doc "the order to answer in: last pushed (the default), most stars, most forks, newest or name; empty reads the section's sort")
  ("limit" :integer :default 0 :doc "how many repositories to answer, at most; 0 reads the section's limit")
  ("account" :string :default "" :doc "the GitHub login whose repositories to list; empty lists the signed-in account's own"))

(nlk:define-contract stats-request
  (:doc "Total the GitHub repositories of an account: how many are public, private, forks and archived, the stars and open issues across them, and the languages they are written in.")
  ("account" :string :default "" :doc "the GitHub login to total up; empty totals the signed-in account's own"))

(defun repos-tool (request)
  "The GITHUB_REPOS tool: the list REQUEST asks for, as text."
  (handler-case
      (progn
        (running-settings)
        (render-repos :query (repos-request-query request)
                      :visibility (repos-request-visibility request)
                      :sort (repos-request-sort-order request)
                      :limit (repos-request-limit request)
                      :account (repos-request-account request)))
    (github-repos-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

(defun stats-tool (request)
  "The GITHUB_REPO_STATS tool: the totals REQUEST asks for, as text."
  (handler-case
      (progn
        (running-settings)
        (render-summary :account (stats-request-account request)))
    (github-repos-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

;;; --- the verbs and /repos ----------------------------------------------------------

(define-verb repos (&key query visibility sort limit account)
  "List repositories, for eval: (github-repos:repos :sort \"stars\"). It
answers the same text the github_repos tool answers."
  (render-repos :query query :visibility visibility :sort sort :limit limit
                :account account))

(define-verb repo-summary (&key account)
  "Total repositories, for eval: (github-repos:repo-summary). It answers the
same text the github_repo_stats tool answers."
  (render-summary :account account))

(defun repos-command (args session-id)
  "One /repos [QUERY]: the account's repositories, narrowed to QUERY."
  (declare (ignore session-id))
  (handler-case
      (render-repos :query (nlk:trimmed (or args "")))
    (github-repos-error (e) (format nil "repos: ~a" e))))

;;; --- the primer --------------------------------------------------------------------

(defparameter +primer+
  "GitHub repositories are readable through two tools and /repos, all through
the gh CLI the person is already signed in with (gh auth login) -- the cell
holds no token:
  github_repos       query (words in a name, description or language),
      visibility all | public | private | fork | archived, sort pushed (the
      default) | stars | forks | created | name, limit (rows; 0 reads the
      section's), account (another login; empty is the signed-in one). It
      answers a header, one line per repository with its stars, forks,
      language, open issues and how long ago it last moved, and how many were
      cut.
  github_repo_stats  account (as above). It answers the totals: public and
      private, forks and archived, stars and open issues, and the languages
      most used.
/repos [query] lists them for the person typing it, in the section's sort.
Listings are cached for cache_seconds and are read at most max_pages pages of
100 repositories, so an account with more than that is only partly seen. The
cell answers text; clicking a repository open, the tab and the status chip of
the original stay in the dashboard, so quote the full name and let the person
open it."
  "What (help :github-repos) answers while the cell runs.")

(nle:define-cell github-repos
  (:section ("github-repos")
    (:guide "nothing here is required once gh is signed in (gh auth login); sort and limit are what a list answers with when a call does not say; account is empty for the signed-in one")
    ("gh" :string :default "gh" :doc "the GitHub CLI every read runs; its path when it is not on PATH")
    ("account" :string :default "" :doc "the GitHub login to list by default; empty lists the signed-in account's own repositories")
    ("sort" :choice :options '("pushed" "stars" "forks" "created" "name") :default "pushed"
     :doc "the order lists answer in: last pushed, most stars, most forks, newest or name")
    ("limit" :integer :default 40 :min 1 :doc "how many repositories one answer shows at most")
    ("cache_seconds" :integer :default 60 :min 0 :doc "how long a fetched list is reused; 0 reads gh on every call")
    ("max_pages" :integer :default 5 :min 1 :doc "how many pages of 100 repositories one read follows")
    ("timeout_seconds" :integer :default 30 :min 1 :doc "one gh call's ceiling"))
  (:settings #'checked-settings)
  (:tool "github_repos" (nlk:contract 'repos-request) 'repos-tool)
  (:tool "github_repo_stats" (nlk:contract 'stats-request) 'stats-tool)
  (:help :github-repos
         "github-repos: your GitHub repositories as text (github_repos), with the totals across them (github_repo_stats), read through the gh CLI"
         +primer+)
  (:command "repos" 'repos-command
            :description "List your GitHub repositories: /repos [query]"
            :argument-hint "QUERY")
  (:start (lambda () (setf *cache* nil))))
