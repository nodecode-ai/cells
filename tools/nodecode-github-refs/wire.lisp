;;;; wire.lisp --- one anonymous request to api.github.com, the follow-ups it
;;;; names, and the block they add up to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original read public repositories through GitHub's REST API without a
;;;; credential: the issue, then for a pull request its details, then the last
;;;; page or two of comments, using the URLs GitHub returned for that issue and
;;;; only when they point back at https://api.github.com/. This is the same
;;;; three requests through the image's own HTTP call, every status a value,
;;;; and every refusal a GITHUB-REFS-ERROR a model can read.

(in-package #:nodecode-github-refs)

(defparameter +api-host+ "api.github.com"
  "The one host this cell talks to.")

(defparameter +api-root+ "https://api.github.com/"
  "The one address prefix a URL taken from an answer may carry.")

(defparameter +api-version+ "2022-11-28"
  "The REST API version the original asked for.")

(defparameter +user-agent+ "nodecode-github-refs/0.1.0"
  "What the requests say they are; GitHub refuses a request without one.")

(defparameter +ratelimit-note+
  "GitHub's anonymous API allows 60 requests an hour per IP address, shared by
everything on it."
  "What a used-up limit says, in the original's words.")

;;; --- the one seam ---------------------------------------------------------------

(defun http (method url &key headers timeout)
  "The cell's one network call; a test stubs this name. => (values BODY STATUS
HEADERS), every status a value."
  (nlk:http method url :headers headers :timeout (or timeout 10)))

(defun request-headers ()
  "What every request carries: the media type, the version and a User-Agent.
No credential: the cell reads public repositories anonymously."
  (list (cons "accept" "application/vnd.github+json")
        (cons "user-agent" +user-agent+)
        (cons "x-github-api-version" +api-version+)))

(defun header-value (headers name)
  "NAME's value in the response HEADERS nlk:http answers, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun decode-body (body)
  "BODY as decoded JSON, or NIL when it is not JSON."
  (and (stringp body)
       (plusp (length body))
       (ignore-errors (nlk:decode-json body))))

(defun server-message (data)
  "The sentence the API put in DATA's `message' member, at most 300 characters."
  (let ((text (and (hash-table-p data) (one-line (jstr data "message")))))
    (and text (plusp (length text)) (subseq text 0 (min 300 (length text))))))

(defun api-url (value)
  "VALUE when it is a URL that points back at the API root and carries no
fragment, else NIL: the check the original made on every URL it took from an
answer."
  (and (stringp value)
       (string-head-p +api-root+ value)
       (not (find #\# value))
       value))

(defun reset-time (headers)
  "When the anonymous limit resets, in words, from the answer's own header."
  (let ((reset (header-value headers "x-ratelimit-reset")))
    (if (and reset (plusp (length reset)) (every #'digit-char-p reset))
        (multiple-value-bind (second minute hour)
            (decode-universal-time (+ 2208988800 (parse-integer reset)) 0)
          (declare (ignore second))
          (format nil "at ~2,'0d:~2,'0d UTC" hour minute))
        "within the hour")))

(defun refuse-status (status headers data requested missing)
  "Signal the refusal STATUS and HEADERS mean, saying what GitHub said."
  (cond
    ((= status 404) (fail "~a" missing))
    ((and (member status '(403 429))
          (equal "0" (header-value headers "x-ratelimit-remaining")))
     (fail "~a The limit is used up for ~a; it resets ~a"
           +ratelimit-note+ requested (reset-time headers)))
    ((and (member status '(403 429))
          (let ((after (header-value headers "retry-after")))
            (and after (plusp (length after)) (every #'digit-char-p after))))
     (fail "~a asked to slow down (secondary rate limit); retry in ~as"
           +api-host+ (header-value headers "retry-after")))
    (t
     (let ((message (server-message data)))
       (if message
           (fail "~a answered HTTP ~d: ~a" +api-host+ status message)
           (fail "~a answered HTTP ~d" +api-host+ status))))))

(defun read-answer (url timeout requested &optional (missing "not found"))
  "URL's decoded answer, or the refusal its status means."
  (multiple-value-bind (body status headers) (http :get url :headers (request-headers) :timeout timeout)
    (if (= status 200)
        (or (decode-body body)
            (fail "unreadable answer from ~a for ~a" +api-host+ requested))
        (refuse-status status headers (decode-body body) requested missing))))

;;; --- the deadline, and the pages ---------------------------------------------------

(defun seconds-left (deadline)
  "What is left of DEADLINE, in seconds, never less than a tenth."
  (max 0.1 (/ (- deadline (get-internal-real-time)) (float internal-time-units-per-second))))

(defun comment-pages (total max-comments)
  "Which pages of comments carry the newest MAX-COMMENTS of TOTAL: the last,
and the one before it when the last does not begin at a page boundary."
  (let ((last-page (ceiling total max-comments)))
    (if (and (plusp (mod total max-comments)) (> last-page 1))
        (list (1- last-page) last-page)
        (list last-page))))

;;; --- the verb -------------------------------------------------------------------

(defun expand (target &key max-chars max-comments timeout)
  "TARGET, a public GitHub issue or pull request, as the block a model reads:
title and url, state, labels, the description, the latest comments, and for a
pull request its branches, merge state and change size."
  (multiple-value-bind (owner repo number) (parse-reference target)
    (let* ((max-chars (max 1 (or max-chars (setting :max-chars))))
           (max-comments (max 0 (or max-comments (setting :max-comments))))
           (timeout (or timeout (setting :timeout-seconds)))
           (requested (format nil "~a/~a#~d" owner repo number))
           (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
           (issue (read-answer (format nil "~arepos/~a/~a/issues/~d" +api-root+ owner repo number)
                                timeout requested
                                (format nil "no issue or pull request ~a is visible without signing in (it does not exist, or the repository is private)"
                                        requested))))
      (unless (hash-table-p issue)
        (fail "unexpected answer from ~a for ~a" +api-host+ requested))
      (let* ((total (or (jint issue "comments") 0))
             (pull-url (let ((pull (jobj issue "pull_request")))
                         (and pull (api-url (jstr pull "url")))))
             (comments-url (and (plusp total) (plusp max-comments) (api-url (jstr issue "comments_url"))))
             (pages (and comments-url (comment-pages total max-comments)))
             (notes '())
             (pull nil)
             (gathered '()))
        (if (and (or pull-url pages) (< (seconds-left deadline) 0.5))
            (push (if pull-url
                      "Pull request details and comments not read: the first request used up the time limit."
                      "Comments not read: the first request used up the time limit.")
                  notes)
            (progn
              (when pull-url
                (handler-case (setf pull (read-answer pull-url (seconds-left deadline) requested))
                  (github-refs-error (condition)
                    (push (format nil "Pull request details not read: ~a" (github-refs-error-detail condition))
                          notes))))
              (dolist (page pages)
                (handler-case
                    (let ((answer (read-answer (format nil "~a?per_page=~d&page=~d" comments-url max-comments page)
                                                (seconds-left deadline) requested)))
                      (loop for comment across (if (vectorp answer) answer #())
                            when (hash-table-p comment)
                              do (setf gathered (append gathered (list comment)))))
                  (github-refs-error (condition)
                    (push (format nil "Comments not read: ~a" (github-refs-error-detail condition))
                          notes))))))
        (let ((comments (last gathered (min (length gathered) max-comments))))
          (render issue (and (hash-table-p pull) pull) comments total requested
                  (remove-duplicates (nreverse notes) :test #'string=)
                  max-chars))))))

(define-verb reference (target &key max-chars max-comments timeout)
  "Fetch a public GitHub issue or pull request and answer it as a block a model
reads. TARGET is \"owner/repo#123\", or a github.com issue or pull request
URL; MAX-CHARS bounds the description (the section's max_chars when absent),
MAX-COMMENTS how many of the newest comments are included (the section's
max_comments), TIMEOUT how long the whole reference may take. => title, url,
state, labels, description, comments, and for a pull request its branches,
merge state and change size."
  (expand target :max-chars max-chars :max-comments max-comments :timeout timeout))

;;; --- the tool the model calls -----------------------------------------------------

(defun github-reference-tool (request)
  "The github_reference tool: the reference one call carries, answered as the
verb answers it."
  (reference (github-reference-request-reference request)
             :max-chars (github-reference-request-max-chars request)
             :max-comments (github-reference-request-max-comments request)))

;;; --- the operator's own door ------------------------------------------------------

(defun run-slash (args session-id)
  "/gh REFERENCE: the operator's own reference, answered as the model's tool is."
  (declare (ignore session-id))
  (let ((target (nlk:trimmed (or args ""))))
    (if (plusp (length target))
        (expand target)
        (format nil "gh: type a reference, e.g. /gh owner/repo#123 or /gh https://github.com/owner/repo/pull/12. ~
This cell also gives the model the github_reference tool. It reads public repositories ~
anonymously, so ~a" +ratelimit-note+))))
