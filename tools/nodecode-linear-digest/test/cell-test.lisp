;;;; cell-test.lisp --- the cell against a stubbed Linear.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Linear, no key is
;;;; needed and nothing is written. What is proved is the cell's own wiring:
;;;; the section (a healthy start, and a start without a key refused), the
;;;; linear_digest tool through the dispatch the model calls it by -- the one
;;;; POST, the header the key rides in, the pages it follows, the four groups
;;;; and their caps -- and the /linear-digest command.

(in-package #:nodecode.test)

(define-test-slice "linear-digest" "LINEAR-DIGEST-CELL-" :start nodecode-linear-digest:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, newest first, as (METHOD URL OPTIONS).")

(defparameter *pages* nil
  "The payloads the stub answers, in order; an empty page once they run out.")

(defparameter *status* 200
  "The status every stubbed answer carries.")

(defparameter +key+ "lin_api_example")

(defun today ()
  (nodecode-linear-digest::today-string))

(defun issue-node (identifier title &key (state "In Progress") (type "started") due updated url)
  (nlk:json-object "identifier" identifier
                    "title" title
                    "url" (or url (format nil "https://linear.app/acme/issue/~a" identifier))
                    "dueDate" due
                    "updatedAt" (or updated "2030-01-01T00:00:00.000Z")
                    "state" (nlk:json-object "name" state "type" type)))

(defun page-payload (nodes &key next)
  (nlk:json-object "data"
                    (nlk:json-object "viewer"
                                     (nlk:json-object
                                      "assignedIssues"
                                      (nlk:json-object "nodes" (coerce nodes 'vector)
                                                       "pageInfo"
                                                       (nlk:json-object
                                                        "hasNextPage" (and next t)
                                                        "endCursor" next))))))

(defun stub-wire (method url options)
  "NLK:HTTP: answer the next queued page; nothing leaves the machine."
  (push (list method url options) *calls*)
  (values (nlk:encode-json-object (or (pop *pages*) (page-payload '())))
          *status* nil nil))

(defmacro with-linear-wire (&body body)
  "BODY with NLK:HTTP stubbed and the environment fallback silent, so the
fixture's own variables are the ones in play."
  `(let ((*calls* '())
         (*pages* '())
         (*status* 200))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (stub-wire method url options))
       (with-stubbed-fdefinition (nodecode-linear-digest::environment-value (name)
                                  (declare (ignore name)) nil)
         ,@body))))

(defun digest-called (&optional (arguments "{}"))
  "linear_digest called the way the model calls it, as the text it reads back."
  (nle::message-content (execute-wire-call "linear-digest-call" "linear_digest" arguments)))

(defun request-body (call)
  "CALL's JSON body, decoded."
  (nlk:decode-json (getf (third call) :content)))

(defun after-of (call)
  "The cursor CALL's body carried, or NIL."
  (nlk:json-value (request-body call) :string "variables" "after"))

(defun header-of (call name)
  (cdr (assoc name (getf (third call) :headers) :test #'string-equal)))

;;; --- lifecycle --------------------------------------------------------------------

(define-cell-lifecycle-tests "linear-digest"
  (:fixture with-linear-wire)
  (:config "api_key" +key+)
  (:help :linear-digest)
  (:command "linear-digest")
  (:running
   (is (nle::find-tool "linear_digest") "the digest tool is registered")
   (is (= 1 (count-if (lambda (entry) (eql 0 (search "linear_digest" (first entry))))
                       nle::*tools*)) "one of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "linear_digest")) "stop takes it back"))
  (:refused ()
            ("stale_days" 0)))

(deftest linear-digest-cell-a-refusal-names-the-member-to-set (with-linear-wire)
  (let ((text (refusal-text nlk:config-refusal (linear-digest-start))))
    (is-carrying text "api_key" "LINEAR_API_KEY"))
  (is (null (nle::find-tool "linear_digest")) "nothing was installed"))

;;; --- the list ---------------------------------------------------------------------

(deftest linear-digest-cell-the-list-is-the-four-groups (with-linear-wire)
  (setf *pages*
        (list (page-payload
               (list (issue-node "LIN-1" "Pay the invoice" :due "2020-01-01")
                     (issue-node "LIN-2" "Ship the release" :due (today))
                     (issue-node "LIN-3" "Review the draft" :updated "2020-01-01T00:00:00.000Z")
                     (issue-node "LIN-4" "Write the notes" :state "Todo" :type "unstarted")
                     (issue-node "LIN-9" "Already done" :state "Done" :type "completed")))))
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (let ((answer (digest-called)))
      (is-carrying answer
                   "Overdue (1)"
                   "Due today (1)"
                   "Not updated for 7 days (1)"
                   "Other (1)"
                   (:absent "LIN-9")
                   "LIN-1	Pay the invoice	In Progress	due 2020-01-01	https://linear.app/acme/issue/LIN-1")
      (is (search (format nil "~%~%Due today") answer) "a blank line separates the groups"))))

(deftest linear-digest-cell-one-post-carries-the-key-and-the-query (with-linear-wire)
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (digest-called)
    (let ((call (first (last *calls*))))
      (is (eq :post (first call)) "one POST")
      (is (equal "https://api.linear.app/graphql" (second call)) "to the one origin")
      (is (equal +key+ (header-of call "authorization")) "the key as the whole header value")
      (is (null (header-of call "user-agent")) "and nothing else of this machine's")
      (is (search "assignedIssues" (nlk:json-value (request-body call) :string "query")) "the read-only query goes in the body")
      (is (null (after-of call)) "with no cursor on the first page"))))

(deftest linear-digest-cell-a-second-page-is-read-by-its-cursor (with-linear-wire)
  (setf *pages*
        (list (page-payload (list (issue-node "LIN-1" "First page")) :next "cursor-2")
              (page-payload (list (issue-node "LIN-2" "Second page")))))
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (let ((answer (digest-called)))
      (is (= 2 (length *calls*)) "two pages, two POSTs")
      (is (equal "cursor-2" (after-of (first *calls*))) "the cursor a page answered is sent back")
      (is-carrying answer "First page" "Second page" "Other (2)")
      (is (not (search "not the full list" answer)) "and the list is finished"))))

(deftest linear-digest-cell-an-unread-page-says-so (with-linear-wire)
  (setf *pages*
        (list (page-payload (list (issue-node "LIN-1" "First page")) :next "same")
              (page-payload (list (issue-node "LIN-2" "Second page")) :next "same")))
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (is (search "Later pages were not read, so this is not the full list." (digest-called)) "a page loop stops and says the list is partial")))

(deftest linear-digest-cell-the-show-limit-counts-issue-lines (with-linear-wire)
  (setf *pages*
        (list (page-payload
               (list (issue-node "LIN-1" "One")
                     (issue-node "LIN-2" "Two")
                     (issue-node "LIN-3" "Three")))))
  (with-cell-stop ((linear-digest-start "api_key" +key+ "show_limit" 2))
    (let ((answer (digest-called)))
      (is-carrying answer "Other (3)" "LIN-1" "LIN-2" (:absent "LIN-3") "+1 more"))))

(deftest linear-digest-cell-nothing-assigned-is-an-empty-answer (with-linear-wire)
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (is (equal "" (digest-called)) "nothing to say, and nothing said")))

(deftest linear-digest-cell-a-url-and-a-title-are-tamed (with-linear-wire)
  (setf *pages*
        (list (page-payload
               (list (issue-node "LIN-1" "Two
lines here"
                                 :url "http://linear.example/acme/issue/LIN-1")
                     (issue-node "LIN-2" "No due" :due "tomorrow")))))
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (let ((answer (digest-called)))
      (is-carrying answer "LIN-1	Two lines here	" "	url refused" "due none" "due unreadable"))))

(deftest linear-digest-cell-a-rejected-key-says-what-to-do (with-linear-wire)
  (setf *status* 401)
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (let ((answer (digest-called)))
      (is-carrying answer "ERROR: Linear refused the key (HTTP 401)" "Nothing was fetched" "restart-cells"))))

(deftest linear-digest-cell-an-argument-is-refused (with-linear-wire)
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (is (search "Unknown arguments were rejected"
                (digest-called "{\"stale_days\": 3}")) "the tool takes no arguments, as the original's did")
    (is (null *calls*) "and nothing is fetched")))

;;; --- the operator's command ---------------------------------------------------------

(deftest linear-digest-cell-the-command-prints-the-list (with-linear-wire)
  (setf *pages* (list (page-payload (list (issue-node "LIN-1" "Pay the invoice")))))
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (is (search "Pay the invoice" (nodecode-linear-digest:run-command "" nil)) "no argument prints the morning list")
    (setf *pages* nil)
    (is (search "No open Linear issue is assigned"
                (nodecode-linear-digest:run-command "" nil)) "and an empty list says so in words")))

(deftest linear-digest-cell-the-command-says-how-it-is-set-up (with-linear-wire)
  (with-cell-stop ((linear-digest-start "api_key" +key+))
    (let ((answer (nodecode-linear-digest:run-command "status" nil)))
      (is-carrying answer "https://api.linear.app/graphql" "the api_key member" (:absent +key+))
      (is (null *calls*) "with no request to Linear"))))

(deftest linear-digest-cell-a-key-in-the-environment-is-read-once (with-linear-wire)
  (with-stubbed-fdefinition (nodecode-linear-digest::environment-value (name)
                             (and (equal name "LINEAR_API_KEY") +key+))
    (with-cell-stop ((linear-digest-start))
      (setf *pages* (list (page-payload (list (issue-node "LIN-1" "Pay the invoice")))))
      (digest-called)
      (is (equal +key+ (header-of (first *calls*) "authorization")) "the variable's value rides the header")
      (is (search "the environment variable api_key_env names"
                  (nodecode-linear-digest:run-command "status" nil)) "and the command names where the key came from"))))
