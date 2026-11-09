;;;; cell.lisp --- PR Studio: the pull-request inbox, and a review that ends in the chat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes Desktop pane over the user's own gh login. This
;;;; cell keeps what the pane computes and gives it to the model instead of a
;;;; window: the three inbox searches (needs your review, your pull requests,
;;;; involves you), one PR read whole -- header, description, checks, commits
;;;; and conversation -- its unified diff with a per-file summary, its review
;;;; threads with the comment ids a reply needs, and the three writes: a
;;;; comment, a reply inside a thread, and a review (comment, approve,
;;;; request changes) carrying comments on lines of the diff.
;;;;
;;;; Every call is the user's own gh CLI, already signed in: gh search prs,
;;;; gh pr view, gh pr diff and gh api. Argument lists only, never a shell,
;;;; one timeout per command. The cell holds no token, reads none, and
;;;; contacts nothing itself: gh authenticates itself.
;;;;
;;;; Tests rebind *RUN*, the one process seam, so nothing here needs gh to be
;;;; installed to be proved.

(in-package #:nodecode-pr-studio)

(nlk:define-peripheral pr-studio :not-running t)

;;; --- the process seam -------------------------------------------------------

(defvar *run* nil
  "The one process seam: (DIR PROGRAM ARGS TIMEOUT) => (values OUTPUT EXIT-CODE).
NIL means the real subprocess, REAL-RUN; the tests rebind it.")

(defun real-run (dir program args timeout)
  "Run PROGRAM with ARGS in DIR, its standard error with its output, its
output bounded by a file and its time by TIMEOUT. => (values OUTPUT EXIT-CODE),
EXIT-CODE NIL when it outlived TIMEOUT and was stopped."
  (let ((path (merge-pathnames (format nil "pr-studio-~d-~d.txt"
                                        (get-universal-time) (random 1000000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (unwind-protect
         (handler-case
             (let* ((process (uiop:launch-program (cons program args)
                                                   :directory (uiop:ensure-directory-pathname dir)
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
               (values (if (probe-file path) (uiop:read-file-string path) "") code))
           (error (e)
             (fail "cannot run ~a: ~a" program e)))
      (ignore-errors (delete-file path)))))

(defun gh-command (args)
  "The gh CLI with ARGS, from this process's own directory.
=> (values OUTPUT EXIT-CODE)."
  (funcall (or *run* #'real-run) (uiop:getcwd) (setting :gh) args
           (setting :timeout-seconds)))

(defun gh (args)
  "OUTPUT of one gh call, refusing when gh fails."
  (multiple-value-bind (output code) (gh-command args)
    (unless (eql 0 code)
      (fail "gh ~{~a~^ ~} failed~@[ (exit ~a)~]: ~a" args code
            (one-line (or output ""))))
    (values (or output "") code)))

(defun json-slice (text)
  "TEXT from its first JSON value: gh writes warnings to its standard error,
which the runner merges into the one output."
  (let* ((text (or text ""))
         (brace (position #\{ text))
         (bracket (position #\[ text))
         (start (cond ((and brace bracket) (min brace bracket))
                      (brace brace)
                      (bracket bracket))))
    (if start (subseq text start) text)))

(defun gh-json (args &key (what "gh"))
  "The one JSON value gh answers for ARGS, decoded; a refusal when it does not."
  (multiple-value-bind (text code) (gh args)
    (handler-case
        (or (shasht:read-json (json-slice text))
            (fail "~a answered nothing for ~a" what
                  (one-line (format nil "~{~a~^ ~}" args))))
      (pr-studio-error (e) (error e))
      (error ()
        (fail "~a did not answer JSON for ~a (exit ~a): ~a" what
              (one-line (format nil "~{~a~^ ~}" args)) code (one-line text))))))

;;; --- text ------------------------------------------------------------------

(defun trimmed (text)
  (nlk:trimmed (or text "")))

(defun head-of (text limit)
  (if (> (length text) limit) (subseq text 0 limit) text))

(defun one-line (text)
  "TEXT as one line, short enough to sit in a refusal."
  (let* ((text (or text ""))
         (newline (position #\Newline text)))
    (when newline (setf text (subseq text 0 newline)))
    (if (> (length text) 200)
        (concatenate 'string (subseq text 0 200) "...")
        text)))

(defun cut (text limit)
  "TEXT, cut to LIMIT characters with a note saying so when it was longer."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a~%[... cut at ~d of ~d characters]"
                (subseq text 0 limit) limit (length text)))))

(defun joined (strings separator)
  (format nil (format nil "~~{~~a~~^~a~~}" separator) strings))

(defun indented (text prefix)
  "TEXT with PREFIX at the head of every line."
  (joined (mapcar (lambda (line) (format nil "~a~a" prefix line))
                  (uiop:split-string (or text "") :separator '(#\Newline)))
          (format nil "~%~a" prefix)))

(defun age-text (iso)
  "ISO-8601 stamp as an age short enough for a row: 3h, 2d, 4mo."
  (let ((universal (ignore-errors (nlk:iso-universal iso))))
    (if (null universal)
        ""
        (let ((seconds (- (get-universal-time) universal)))
          (cond ((minusp seconds) "now")
                ((< seconds 60) "now")
                ((< seconds 3600) (format nil "~dm" (floor seconds 60)))
                ((< seconds 86400) (format nil "~dh" (floor seconds 3600)))
                ((< seconds 2592000) (format nil "~dd" (floor seconds 86400)))
                (t (format nil "~dmo" (floor seconds 2592000))))))))

;;; --- what a call names ------------------------------------------------------

(defparameter +repo-chars+ "-._"
  "What a repository's owner and name are made of, besides letters and digits.")

(defun check-repo (repo)
  "REPO as owner/name. => (values OWNER NAME "owner/name"), or a refusal."
  (let* ((repo (trimmed repo))
         (slash (position #\/ repo)))
    (unless (and slash (plusp slash) (< slash (1- (length repo))))
      (fail "~s is not a repository: write it as owner/name" repo))
    (unless (and (every (lambda (c)
                          (or (alphanumericp c)
                              (and (find c +repo-chars+) t)))
                        (subseq repo 0 slash))
                 (every (lambda (c)
                          (or (alphanumericp c)
                              (and (find c +repo-chars+) t)))
                        (subseq repo (1+ slash)))
                 (not (find #\/ repo :start (1+ slash))))
      (fail "~s is not a repository: write it as owner/name" repo))
    (values (subseq repo 0 slash) (subseq repo (1+ slash)) repo)))

(defun check-number (number)
  (unless (and (integerp number) (plusp number) (< number 100000000))
    (fail "~s is not a pull-request number" number))
  number)

(defun check-comment-id (id)
  (unless (and (integerp id) (plusp id))
    (fail "~s is not a comment id; pr_threads prints the one a reply needs" id))
  id)

(defun check-body (text what)
  (let ((text (trimmed text)))
    (when (zerop (length text))
      (fail "~a needs a body" what))
    (when (> (length text) 60000)
      (fail "~a is longer than 60000 characters" what))
    text))

(defun check-writes (what)
  (when (setting :read-only)
    (fail "read_only is on: set it off in the pr-studio section to ~a" what)))

(defun with-temp-file (text function)
  "Call FUNCTION on a file holding TEXT, and remove the file afterwards."
  (let ((path (merge-pathnames (format nil "pr-studio-~d-~d.txt"
                                       (get-universal-time) (random 1000000))
                               (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede
                                     :if-does-not-exist :create :external-format :utf-8)
             (write-string text out))
           (funcall function (uiop:native-namestring path)))
      (ignore-errors (delete-file path)))))

(defun safe-json (text)
  "TEXT decoded when it is JSON, NIL when it is anything else."
  (handler-case (shasht:read-json (json-slice text))
    (error () nil)))

;;; --- the inbox --------------------------------------------------------------

(defparameter +search-fields+
  (joined '("number" "title" "isDraft" "state" "url" "author" "repository"
             "reviewDecision" "updatedAt" "commentsCount") ",")
  "The gh search prs fields one inbox row reads.")

(defparameter +groups+
  '(("review" "Needs your review" "--review-requested=@me")
    ("mine" "Your pull requests" "--author=@me")
    ("involved" "Involves you" "--involves=@me"))
  "The three searches the inbox makes, in the order a review queue reads.")

(defun search-prs (qualifier limit repo)
  "One search of open pull requests, as a list of decoded rows."
  (let ((rows (gh-json (append (list "search" "prs" qualifier "--state=open"
                                      (format nil "--limit=~d" limit)
                                      (format nil "--json=~a" +search-fields+))
                                (when (plusp (length repo))
                                  (list (format nil "--repo=~a" repo))))
                        :what "gh search prs")))
    (if (vectorp rows) (coerce rows 'list) '())))

(defun repository-name (pr)
  "The owner/name a search row carries, however gh spelled it."
  (or (nlk:json-value pr :string "repository" "nameWithOwner")
      (let ((owner (nlk:json-value pr :string "repository" "owner" "login"))
            (name (nlk:json-value pr :string "repository" "name")))
        (and owner name (format nil "~a/~a" owner name)))
      "?"))

(defun inbox-identity (pr)
  (cons (repository-name pr) (nlk:json-value pr :integer "number")))

(defun inbox-row (pr)
  "One pull request as one line of the inbox."
  (let ((repo (repository-name pr))
        (number (nlk:json-value pr :integer "number"))
        (author (nlk:json-value pr :string "author" "login"))
        (review (nlk:json-value pr :string "reviewDecision"))
        (comments (nlk:json-value pr :integer "commentsCount")))
    (format nil "  ~a#~a  ~a~@[  @~a~]~@[  [draft]~]~@[  [review ~a]~]~@[  ~d comments~]~%      ~a~%"
            repo (or number "?")
            (age-text (nlk:json-value pr :string "updatedAt"))
            author
            (nlk:json-value pr :boolean "isDraft")
            (and review (string-downcase review))
            comments
            (one-line (nlk:json-value pr :string "title")))))

(defun inbox-groups (&key repo limit)
  "The three groups, each a plist of :id :label :pulls; a pull request appears
only in the first group that holds it."
  (let ((seen '())
        (groups '()))
    (dolist (group +groups+ (nreverse groups))
      (let ((rows '()))
        (dolist (pr (search-prs (third group) limit repo))
          (let ((identity (inbox-identity pr)))
            (unless (member identity seen :test #'equal)
              (push identity seen)
              (push pr rows))))
        (push (list :id (first group) :label (second group) :pulls (nreverse rows))
              groups)))))

(defun render-inbox (&key repo (limit (setting :limit)))
  (let* ((repo (if (plusp (length (trimmed repo))) (nth-value 2 (check-repo repo)) ""))
         (out (make-string-output-stream)))
    (format out "pull requests~@[ in ~a~]~%" (and (plusp (length repo)) repo))
    (dolist (group (inbox-groups :repo repo :limit limit))
      (let ((pulls (getf group :pulls)))
        (format out "~%~a (~d)~%" (getf group :label) (length pulls))
        (if (null pulls)
            (format out "  nothing~%")
            (dolist (pr pulls) (format out "~a" (inbox-row pr))))))
    (cut (get-output-stream-string out) (setting :max-chars))))

;;; --- one pull request -------------------------------------------------------

(defparameter +view-fields+
  (joined '("number" "title" "state" "isDraft" "author" "baseRefName" "headRefName"
             "headRefOid" "additions" "deletions" "changedFiles" "body" "comments"
             "reviews" "commits" "reviewDecision" "mergeable" "mergeStateStatus"
             "url" "createdAt" "updatedAt" "statusCheckRollup" "labels")
           ",")
  "The gh pr view fields one read of a pull request uses.")

(defparameter +failing-states+
  '("FAILURE" "ERROR" "TIMED_OUT" "CANCELLED" "ACTION_REQUIRED")
  "The conclusions and states a check a person must look at wears.")

(defun check-text (contexts)
  "One line on the checks: how many, and which ones failed."
  (let ((total 0) (failing '()))
    (loop for context across (or contexts #())
          for name = (or (nlk:json-value context :string "name")
                         (nlk:json-value context :string "context")
                         "a check")
          for state = (or (nlk:json-value context :string "conclusion")
                          (nlk:json-value context :string "state")
                          "")
          do (incf total)
             (when (member (string-upcase state) +failing-states+ :test #'equal)
               (push (format nil "~a (~a)" name (string-downcase state)) failing)))
    (cond ((zerop total) "checks: none")
          (failing (format nil "checks: ~d, ~d failing -- ~a"
                            total (length failing) (joined (nreverse failing) ", ")))
          (t (format nil "checks: ~d, none failing" total)))))

(defun label-text (labels)
  (let ((names (loop for label across (or labels #())
                     for name = (nlk:json-value label :string "name")
                     when name collect name)))
    (if names (format nil "labels: ~a" (joined names ", ")) "labels: none")))

(defun conversation-entries (pr)
  "The comments and the reviews worth reading, oldest first."
  (let* ((comments (coerce (or (nlk:json-value pr :array "comments") #()) 'list))
         (reviews (coerce (or (nlk:json-value pr :array "reviews") #()) 'list))
         (reviews (remove-if-not
                   (lambda (review)
                     (or (plusp (length (trimmed (nlk:json-value review :string "body"))))
                         (member (nlk:json-value review :string "state")
                                 '("APPROVED" "CHANGES_REQUESTED") :test #'equal)))
                   reviews))
         (entries (append (mapcar (lambda (c)
                                    (list (or (nlk:json-value c :string "createdAt") "")
                                          "comment" c))
                                  comments)
                          (mapcar (lambda (r)
                                    (list (or (nlk:json-value r :string "submittedAt") "")
                                          "review" r))
                                  reviews))))
    (sort entries #'string< :key #'first)))

(defun conversation-text (entries limit)
  (let ((entries (if (> (length entries) limit) (last entries limit) entries)))
    (with-output-to-string (out)
      (dolist (entry entries)
        (destructuring-bind (at kind node) entry
          (format out "  @~a  ~a~@[ [~a]~]~%~a~%"
                  (or (nlk:json-value node :string "author" "login") "someone")
                  (age-text at)
                  (and (string= kind "review") (nlk:json-value node :string "state"))
                  (indented (cut (trimmed (nlk:json-value node :string "body")) 900)
                            "      ")))))))

(defun commits-text (commits limit)
  (let ((commits (coerce (or commits #()) 'list)))
    (with-output-to-string (out)
      (dolist (commit (if (> (length commits) limit) (last commits limit) commits))
        (format out "  ~a  ~a  (~a)~%"
                (head-of (or (nlk:json-value commit :string "oid") "") 7)
                (one-line (or (nlk:json-value commit :string "messageHeadline") ""))
                (age-text (nlk:json-value commit :string "committedDate")))))))

(defun render-show (repo number &key (events (setting :events)))
  (check-repo repo)
  (let* ((number (check-number number))
         (pr (gh-json (list "pr" "view" (princ-to-string number) "-R" repo
                             "--json" +view-fields+)
                       :what (format nil "gh pr view ~d" number)))
         (out (make-string-output-stream))
         (comments (nlk:json-value pr :array "comments")))
    (format out "~a#~d  ~a~%" repo number (nlk:json-value pr :string "title"))
    (format out "  ~@[@~a  ~]~a~@[ (draft)~]  +~a -~a in ~a files  ~a -> ~a  head ~a~%"
            (nlk:json-value pr :string "author" "login")
            (string-downcase (or (nlk:json-value pr :string "state") "open"))
            (nlk:json-value pr :boolean "isDraft")
            (or (nlk:json-value pr :integer "additions") 0)
            (or (nlk:json-value pr :integer "deletions") 0)
            (or (nlk:json-value pr :integer "changedFiles") 0)
            (or (nlk:json-value pr :string "headRefName") "?")
            (or (nlk:json-value pr :string "baseRefName") "?")
            (head-of (or (nlk:json-value pr :string "headRefOid") "") 7))
    (format out "  review ~a  mergeable ~a~@[ (~a)~]  ~a~%"
            (or (nlk:json-value pr :string "reviewDecision") "none")
            (or (nlk:json-value pr :string "mergeable") "unknown")
            (nlk:json-value pr :string "mergeStateStatus")
            (if comments (format nil "~d comments" (length comments)) "no comments"))
    (format out "  ~a~%  ~a~%" (label-text (nlk:json-value pr :array "labels"))
            (check-text (nlk:json-value pr :array "statusCheckRollup")))
    (format out "  ~a~%" (nlk:json-value pr :string "url"))
    (format out "~%description:~%~a~%"
            (indented (cut (trimmed (nlk:json-value pr :string "body")) 6000) "  "))
    (format out "~%commits:~%~a" (commits-text (nlk:json-value pr :array "commits") 30))
    (format out "~%conversation:~%~a"
            (conversation-text (conversation-entries pr) events))
    (cut (get-output-stream-string out) (setting :max-chars))))

;;; --- the diff ----------------------------------------------------------------

(defun patch-files (patch)
  "PATCH's per-file churn, one line each: the hunks are read, not a second call."
  (let ((rows '()))
    (dolist (line (uiop:split-string (or patch "") :separator '(#\Newline)))
      (cond ((uiop:string-prefix-p "diff --git " line)
             (let* ((rest (subseq line (length "diff --git ")))
                    (mid (search " b/" rest :from-end t))
                    (name (if mid (subseq rest (+ mid 3)) rest)))
               (push (list :name name :additions 0 :deletions 0) rows)))
            ((null rows))
            ((uiop:string-prefix-p "+++" line))
            ((uiop:string-prefix-p "---" line))
            ((uiop:string-prefix-p "+" line)
             (incf (getf (first rows) :additions)))
            ((uiop:string-prefix-p "-" line)
             (incf (getf (first rows) :deletions)))))
    (nreverse rows)))

(defun render-diff (repo number &key path)
  (check-repo repo)
  (let* ((number (check-number number))
         (patch (gh (list "pr" "diff" (princ-to-string number) "-R" repo)))
         (files (patch-files patch))
         (path (trimmed path)))
    (when (plusp (length path))
      (setf patch
            (with-output-to-string (out)
              (let ((included nil))
                (dolist (line (uiop:split-string patch :separator '(#\Newline)))
                  (when (uiop:string-prefix-p "diff --git " line)
                    (setf included (search (format nil " b/~a" path) line)))
                  (when included (format out "~a~%" line)))))))
    (cut (format nil "diff of ~a#~d~@[ (~a)~]~%~{~a~%~}~%~a~%"
                 repo number (and (plusp (length path)) path)
                 (mapcar (lambda (file)
                           (format nil "  +~d -~d  ~a" (getf file :additions)
                                   (getf file :deletions) (getf file :name)))
                         files)
                 patch)
         (setting :max-chars))))

;;; --- the review threads ------------------------------------------------------

(defparameter +threads-query+
  (joined '("query($owner:String!,$name:String!,$number:Int!){"
             "repository(owner:$owner,name:$name){pullRequest(number:$number){"
             "reviewThreads(first:100){nodes{id isResolved isOutdated path line originalLine"
             "diffSide comments(first:50){nodes{id databaseId body createdAt url diffHunk"
             "author{login}}}}}}}}") " ")
  "The one GraphQL read of a pull request's review threads: each thread's
place in the diff, and each comment's database id, which a reply needs.")

(defun review-threads (repo number)
  (multiple-value-bind (owner name) (check-repo repo)
    (let ((answer (gh-json (list "api" "graphql"
                                 "-f" (format nil "query=~a" +threads-query+)
                                 "-f" (format nil "owner=~a" owner)
                                 "-f" (format nil "name=~a" name)
                                 "-F" (format nil "number=~d" number))
                           :what "gh api graphql")))
      (coerce (or (nlk:json-value answer :array "data" "repository" "pullRequest"
                                   "reviewThreads" "nodes")
                  #())
              'list))))

(defun thread-text (thread index)
  (with-output-to-string (out)
    (format out "  ~d. ~a~@[:~a~]~@[ (~a)~]~@[ resolved~]~@[ outdated~]~%"
            index
            (or (nlk:json-value thread :string "path") "(the conversation)")
            (nlk:json-value thread :integer "line")
            (let ((side (nlk:json-value thread :string "diffSide")))
              (and side (string-downcase side)))
            (nlk:json-value thread :boolean "isResolved")
            (nlk:json-value thread :boolean "isOutdated"))
    (dolist (comment (coerce (or (nlk:json-value thread :array "comments" "nodes") #())
                              'list))
      (format out "     @~a  ~a  reply with comment_id ~a~%~a~%"
              (or (nlk:json-value comment :string "author" "login") "someone")
              (age-text (nlk:json-value comment :string "createdAt"))
              (or (nlk:json-value comment :integer "databaseId")
                  (nlk:json-value comment :string "id") "?")
              (indented (cut (trimmed (nlk:json-value comment :string "body")) 1200)
                        "       ")))))

(defun render-threads (repo number &key only-unresolved)
  (let* ((threads (review-threads repo number))
         (shown (if only-unresolved
                    (remove-if (lambda (thread)
                                 (nlk:json-value thread :boolean "isResolved"))
                               threads)
                    threads))
         (open (count-if-not (lambda (thread)
                               (nlk:json-value thread :boolean "isResolved"))
                             threads))
         (out (make-string-output-stream)))
    (format out "review threads in ~a#~d: ~d, ~d unresolved~%"
            repo number (length threads) open)
    (if (null shown)
        (format out "  none~%")
        (loop for thread in shown
              for index from 1
              do (format out "~a" (thread-text thread index))))
    (cut (get-output-stream-string out) (setting :max-chars))))

;;; --- the writes --------------------------------------------------------------

(defun post-comment (repo number body &key reply-to)
  (check-repo repo)
  (let ((number (check-number number))
        (text (check-body body "a comment")))
    (check-writes "comment on a pull request")
    (if (and (integerp reply-to) (plusp reply-to))
        (let ((id (check-comment-id reply-to)))
          (with-temp-file (nlk:encode-json-object (nlk:make-json-object "body" text))
            (lambda (file)
              (let ((answer (gh-json (list "api" "-X" "POST"
                                           (format nil "repos/~a/pulls/~d/comments/~d/replies"
                                                   repo number id)
                                           "--input" file)
                                     :what "gh api replies")))
                (format nil "replied in the thread of comment ~d: ~a" id
                        (or (nlk:json-value answer :string "html_url") "done"))))))
        (with-temp-file text
          (lambda (file)
            (let ((output (gh (list "pr" "comment" (princ-to-string number) "-R" repo
                                    "--body-file" file))))
              (format nil "commented on ~a#~d: ~a" repo number
                      (or (first (remove "" (uiop:split-string (trimmed output)
                                                                 :separator '(#\Newline))))
                          "done"))))))))

(defun comment-object (comment)
  "One inline COMMENT of a review as the JSON GitHub takes, or a refusal."
  (let* ((path (trimmed (pr-review-comment-path comment)))
         (line (pr-review-comment-line comment))
         (side (string-upcase (or (pr-review-comment-side comment) "RIGHT")))
         (body (check-body (pr-review-comment-body comment) "an inline comment"))
         (start (pr-review-comment-start-line comment)))
    (unless (and (plusp (length path))
                 (not (char= (char path 0) #\/))
                 (not (search ".." path)))
      (fail "~s is not a repository-relative path" path))
    (unless (and (integerp line) (plusp line))
      (fail "~s is not a line of the diff" line))
    (unless (member side '("LEFT" "RIGHT") :test #'equal)
      (fail "~s is not a side: left or right" side))
    (if (and (integerp start) (plusp start) (< start line))
        (nlk:make-json-object "path" path "line" line "side" side
                               "start_line" start "start_side" side "body" body)
        (nlk:make-json-object "path" path "line" line "side" side "body" body))))

(defun review-payload (event body comments commit-id)
  (let ((pairs (list "event" event)))
    (when (plusp (length body))
      (setf pairs (append pairs (list "body" body))))
    (when comments
      (setf pairs (append pairs (list "comments"
                                      (coerce (mapcar #'comment-object comments) 'vector)))))
    (when (plusp (length commit-id))
      (setf pairs (append pairs (list "commit_id" commit-id))))
    (apply #'nlk:make-json-object pairs)))

(defun submit-review (repo number event body comments commit-id)
  (check-repo repo)
  (let* ((number (check-number number))
         (event (string-upcase (trimmed event)))
         (body (trimmed body))
         (commit-id (trimmed commit-id)))
    (unless (member event '("COMMENT" "APPROVE" "REQUEST_CHANGES") :test #'equal)
      (fail "~s is not a review event: comment, approve or request changes" event))
    (when (and (string= event "REQUEST_CHANGES") (zerop (length body)) (null comments))
      (fail "request changes needs a summary or at least one comment on a line"))
    (check-writes "submit a review")
    (with-temp-file (nlk:encode-json-object
                     (review-payload event body comments commit-id))
      (lambda (file)
        (let* ((output (gh (list "api" "-X" "POST"
                                 (format nil "repos/~a/pulls/~d/reviews" repo number)
                                 "--input" file)))
               (answer (safe-json output)))
          (format nil "review ~a on ~a#~d: ~a"
                  (string-downcase event) repo number
                  (or (nlk:json-value answer :string "html_url") (one-line output))))))))

;;; --- the boundaries -----------------------------------------------------------

(nlk:define-contract pr-inbox-request
  (:doc "The pull-request inbox: the open pull requests that need your review, that are yours, and that involve you, one line each, most recently updated first. Call it when someone asks what needs their attention on GitHub.")
  ("repo" :string :default "" :doc "owner/name to search one repository only; empty searches all of GitHub")
  ("limit" :integer :default 0 :doc "rows per group; 0 takes the section's limit"))

(nlk:define-contract pr-show-request
  (:doc "One pull request whole: its header, description, labels, checks, commits and conversation, as text. Read it before reviewing the PR or answering on it.")
  ("repo" :text :required t :doc "owner/name of the repository")
  ("number" :integer :required t :slot num :doc "the pull request's number")
  ("events" :integer :default 25 :doc "the most recent conversation entries to show; at least 1"))

(nlk:define-contract pr-diff-request
  (:doc "The pull request's unified diff, a churn summary of every file first, then the patch. Read it before commenting on a line: the line numbers a review comment takes are the new file's, as the hunk headers count them.")
  ("repo" :text :required t :doc "owner/name of the repository")
  ("number" :integer :required t :slot num :doc "the pull request's number")
  ("path" :string :default "" :doc "one changed file to show alone; empty shows them all"))

(nlk:define-contract pr-threads-request
  (:doc "The review threads already on a pull request: where each one sits (file, line, side), whether it is resolved or outdated, and every comment with the id a reply takes.")
  ("repo" :text :required t :doc "owner/name of the repository")
  ("number" :integer :required t :slot num :doc "the pull request's number")
  ("only_unresolved" :boolean :default nil :doc "show only the threads nobody has resolved"))

(nlk:define-contract pr-comment-request
  (:doc "Say something on a pull request: a comment on the conversation, or -- with reply_to -- a reply inside an existing review thread. It is visible to everyone who can see the PR, so post only what the person asked for.")
  ("repo" :text :required t :doc "owner/name of the repository")
  ("number" :integer :required t :slot num :doc "the pull request's number")
  ("body" :text :required t :doc "the comment, as markdown")
  ("reply_to" :integer :default 0 :doc "the comment id whose thread to reply in, as pr_threads prints it; 0 comments on the conversation"))

(nlk:define-contract pr-review-comment
  (:doc "One comment on a line of the diff, submitted with the review.")
  ("path" :text :required t :doc "the file's path, as the diff names it")
  ("line" :integer :required t :doc "the line in the new file on the right side, or the old one on the left, as the hunk header counts")
  ("body" :text :required t :doc "what to say about that line, as markdown")
  ("side" :text :default "RIGHT" :options '("LEFT" "RIGHT") :doc "which side of the diff the line is on")
  ("start_line" :integer :default 0 :doc "the first line of a comment spanning several; 0 comments on one line"))

(nlk:define-contract pr-review-request
  (:doc "Submit a review on a pull request: comment, approve, or request changes, with optional comments on lines of the diff. Do it only when the person asks; a review is visible to the author.")
  ("repo" :text :required t :doc "owner/name of the repository")
  ("number" :integer :required t :slot num :doc "the pull request's number")
  ("event" :text :required t :options '("COMMENT" "APPROVE" "REQUEST_CHANGES") :doc "comment, approve or request changes")
  ("body" :string :default "" :doc "the review's summary, as markdown")
  ("comments" :array :of pr-review-comment :doc "comments on lines of the diff, submitted with the review")
  ("commit_id" :string :default "" :doc "the head commit the review was read against; empty reviews the PR's head now"))

;;; --- the tools ----------------------------------------------------------------

(defun inbox-tool (request)
  "The PR_INBOX tool: the review queue REQUEST names, as text."
  (handler-case
      (let ((limit (pr-inbox-request-limit request))
            (repo (pr-inbox-request-repo request)))
        (render-inbox :repo repo :limit (if (and (integerp limit) (plusp limit))
                                            limit (setting :limit))))
    (pr-studio-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

(defun show-tool (request)
  "The PR_SHOW tool: one pull request, as text."
  (handler-case
      (let ((events (pr-show-request-events request)))
        (render-show (pr-show-request-repo request) (pr-show-request-num request)
                      :events (if (and (integerp events) (plusp events))
                                  events (setting :events))))
    (pr-studio-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

(defun diff-tool (request)
  "The PR_DIFF tool: one pull request's patch, as text."
  (handler-case
      (render-diff (pr-diff-request-repo request) (pr-diff-request-num request)
                    :path (pr-diff-request-path request))
    (pr-studio-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

(defun threads-tool (request)
  "The PR_THREADS tool: the review threads of one pull request, as text."
  (handler-case
      (render-threads (pr-threads-request-repo request) (pr-threads-request-num request)
                       :only-unresolved (pr-threads-request-only-unresolved request))
    (pr-studio-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

(defun comment-tool (request)
  "The PR_COMMENT tool: a comment or a thread reply, as its receipt."
  (handler-case
      (post-comment (pr-comment-request-repo request) (pr-comment-request-num request)
                    (pr-comment-request-body request)
                    :reply-to (pr-comment-request-reply-to request))
    (pr-studio-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

(defun review-tool (request)
  "The PR_REVIEW tool: one submitted review, as its receipt."
  (handler-case
      (submit-review (pr-review-request-repo request) (pr-review-request-num request)
                     (pr-review-request-event request) (pr-review-request-body request)
                     (coerce (pr-review-request-comments request) 'list)
                     (pr-review-request-commit-id request))
    (pr-studio-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

;;; --- the verbs, for eval and for /prs -----------------------------------------

(define-verb inbox (&key repo limit)
  "The pull-request inbox, for eval: (pr-studio:inbox :repo \"o/r\")."
  (render-inbox :repo (or repo "") :limit (or limit (setting :limit))))

(define-verb show (repo number &key events)
  "One pull request, for eval: (pr-studio:show \"o/r\" 12)."
  (render-show repo number :events (or events (setting :events))))

(define-verb diff (repo number &key path)
  "One pull request's patch, for eval: (pr-studio:diff \"o/r\" 12 :path \"src/a.lisp\")."
  (render-diff repo number :path path))

(define-verb threads (repo number &key only-unresolved)
  "The review threads of one pull request, for eval."
  (render-threads repo number :only-unresolved only-unresolved))

(define-verb comment (repo number body &key reply-to)
  "Comment on a pull request or reply in a thread, for eval."
  (post-comment repo number body :reply-to reply-to))

(define-verb review (repo number event &key body comments commit-id)
  "Submit a review, for eval: (pr-studio:review \"o/r\" 12 \"COMMENT\" :body \"LGTM\")."
  (submit-review repo number event body comments commit-id))

(defun prs-command (args session-id)
  "One /prs [owner/name]: the inbox this session's person would see."
  (declare (ignore session-id))
  (handler-case
      (render-inbox :repo (trimmed args))
    (pr-studio-error (e) (format nil "prs: ~a" e))))

;;; --- the primer ---------------------------------------------------------------

(defparameter +primer+
  "Pull requests are read and answered through the nodecode-pr-studio cell: six
tools over the user's own gh CLI, already signed in. Nothing is stored, and
no token is read.
  pr_inbox   repo (owner/name, empty = all of GitHub), limit. The three groups:
      needs your review, your pull requests, involves you, one row a PR with its
      age, author, title, state and review decision.
  pr_show    repo, number, events. Header, size, branches, review decision,
      mergeable state, labels, checks with the failing ones named, description,
      commits and conversation.
  pr_diff    repo, number, path (one file, empty = all). A churn line a file,
      then the unified patch, cut at max_chars: read one file at a time when it
      is long. The line numbers a review comment takes are the new file's.
  pr_threads repo, number, only_unresolved. Each existing thread: file, line,
      side, resolved or outdated, its comments, and the comment id a reply takes.
  pr_comment repo, number, body, reply_to (0 = the conversation, else the
      comment id pr_threads printed). Posts at once.
  pr_review  repo, number, event comment|approve|request_changes, body,
      comments (path, line, side, body, start_line), commit_id. Posts one review.
/prs [owner/name] reads the inbox, for the person typing it.
A write is the person's decision: comment, reply and review only when asked,
and say what was posted. With read_only set, every write is refused."
  "What (help :pr-studio) answers while the cell runs.")

(nle:define-cell pr-studio
  (:section ("pr-studio")
    (:guide "gh on PATH, signed in (gh auth status), is all it needs; nothing here is required; limit bounds each inbox group; read_only refuses every write")
    ("gh" :string :default "gh" :doc "the gh program to run: its name on PATH, or a path to it")
    ("limit" :integer :default 10 :min 1 :doc "rows per inbox group")
    ("events" :integer :default 25 :min 1 :doc "conversation entries pr_show shows")
    ("max_chars" :integer :default 30000 :min 2000 :doc "the most text one call answers")
    ("read_only" :boolean :default nil :doc "refuse every comment, reply and review")
    ("timeout_seconds" :integer :default 30 :min 1 :doc "one gh call's ceiling"))
  (:tool "pr_inbox" (nlk:contract 'pr-inbox-request) 'inbox-tool)
  (:tool "pr_show" (nlk:contract 'pr-show-request) 'show-tool)
  (:tool "pr_diff" (nlk:contract 'pr-diff-request) 'diff-tool)
  (:tool "pr_threads" (nlk:contract 'pr-threads-request) 'threads-tool)
  (:tool "pr_comment" (nlk:contract 'pr-comment-request) 'comment-tool)
  (:tool "pr_review" (nlk:contract 'pr-review-request) 'review-tool)
  (:help :pr-studio
         "pr-studio: review GitHub pull requests through gh (pr_inbox, pr_show, pr_diff, pr_threads), then comment, reply and review (pr_comment, pr_review)"
         +primer+)
  (:command "prs" 'prs-command
            :description "The pull-request inbox: /prs [owner/name]"
            :argument-hint "OWNER/NAME"))
