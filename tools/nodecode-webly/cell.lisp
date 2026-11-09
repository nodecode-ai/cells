;;;; cell.lisp --- the primer, the section, three tools, and /webly.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :webly) answers the primer below, and every request's help
;;;; section carries one line naming it. The tools are the publishing
;;;; itself; /webly is the operator's own switch.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "webly": {"api_url": "https://api.webly.ai", "timeout": 60, "name": ""}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-webly)

(defparameter +primer+
  "Webly hosts what agents make: publish a folder or an HTML file at a live URL
in seconds, with no account, through the nodecode-webly cell (three tools, and
the webly: functions behind them, callable through eval).
  webly_publish   a folder or one .html file becomes this machine's anonymous
      site at https://<name>.webly.site. The first publish creates it; every
      later one updates the same site at the same URL. It is live at once for
      24 hours and can be claimed into an account for 7 days. Limits: 25 files,
      1 MiB each, 5 MiB in all; hidden files and node_modules are left out.
      Give the site a name it would be recognized by (the folder's own name is
      used when that says something, e.g. 'portfolio' and not 'dist').
  webly_share     one or more files or folders become public links for 24 hours
      (500 MiB in all per machine). Folder links keep their structure; a large
      folder sits beside the site and never touches it.
  webly_site      the saved site: its URL, when it goes offline, when it can
      still be claimed, and what the API says is next. Ask before publishing a
      second time if something is already live that the person may want to keep.
The operator claims a site from /webly claim, which opens the page that signs
them in; publishing again after that needs the account (the API says so).
ERROR: WEBLY-ERROR names the problem, with the API's own reason."
  "What (help :webly) answers while the cell runs.")

;;; --- what the model calls --------------------------------------------------------

(nlk:define-contract webly-publish
  (:doc "Publish a folder or one HTML file at a live Webly URL with no account: the first publish creates this machine's site, later ones update it at the same URL. It is live for 24 hours and claimable for 7 days.")
  ("path" :text :required t
   :doc "the folder or .html file to publish, as an absolute path (the folder's index.html is the site's front page)")
  ("name" :text :default ""
   :doc "what to call the site on its dashboard and claim page; the folder's name is used when it says something, and a generic name like 'dist' is refused"))

(nlk:define-contract webly-share
  (:doc "Share files or folders as public links that work for 24 hours, with or without a live site.")
  ("paths" :array :of :text :required t
   :doc "the files or folders to share: a folder keeps its structure, and nothing is sent until you call this")
  ("name" :text :default ""
   :doc "what to call the shared folder; 'Shared files' when empty"))

(nlk:define-contract webly-site
  (:doc "The Webly site this machine has published: its URL, when it goes offline, when it can still be claimed into an account, and what the API says to do next.")
  ("refresh" :boolean :default t
   :doc "ask the API for the site's state now; false reports only what is saved on this machine"))

(defun publish-tool (request)
  "WEBLY_PUBLISH: a folder or file at a live URL, or the same site updated."
  (let ((name (webly-publish-name request)))
    (deploy (webly-publish-path request)
            (if (plusp (length (nlk:trimmed (or name "")))) name (setting :name)))))

(defun share-tool (request)
  "WEBLY_SHARE: public file links for a day."
  (share-links (coerce (webly-share-paths request) 'list)
               (webly-share-name request)))

(defun site-tool (request)
  "WEBLY_SITE: where this machine's site stands."
  (status-text :refresh (webly-site-refresh request)))

;;; --- what the operator calls ------------------------------------------------------

(define-verb publish (path &key (name ""))
  "Publish PATH (a folder or an .html file) at a live URL => the summary text."
  (deploy path (if (plusp (length (nlk:trimmed name))) name (setting :name))))

(define-verb share (paths &key (name ""))
  "Share PATHS, files or folders, as public links for 24 hours => the links."
  (share-links (uiop:ensure-list paths) name))

(define-verb status (&key (refresh t))
  "Where this machine's site stands => the summary text."
  (status-text :refresh refresh))

(define-verb claim-link ()
  "The page that claims this machine's site into an account => its URL."
  (claim-text))

;;; --- /webly ---------------------------------------------------------------------

(defun run-command (args session-id)
  "One /webly invocation => the answer's text, and the dialog it opens
without arguments: the site's state, with a row that publishes a folder and
one that opens the claim page."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond ((member verb '("publish" "deploy") :test #'string=)
           (if (plusp (length rest))
               (publish rest)
               "Usage: /webly publish PATH"))
          ((member verb '("share" "upload") :test #'string=)
           (if (plusp (length rest))
               (share rest)
               "Usage: /webly share PATH [PATH ...]"))
          ((string= verb "claim") (claim-link))
          ((string= verb "status") (status))
          (t (let ((summary (status)))
               (values summary
                       (nle:list-dialog
                        :title "Webly"
                        :summary summary
                        :rows (list (nle:ask-row "Publish a folder" "Folder" "/webly publish"
                                                  :placeholder "./site" :verb "publish")
                                    (nle:run-row "Share a file or folder" "/webly share "
                                                 :detail "public links for 24 hours")
                                    (nle:run-row "Open the claim page" "/webly claim"
                                                 :detail "sign in and keep this site")))))))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell webly
  (:section ("webly")
    (:guide "nothing is required: publishing without an account works at once; api_url is Webly's own API unless you run one; name is the site name a publish uses when its folder name says nothing")
    ("api_url" :string :default "https://api.webly.ai"
     :doc "the Webly API this cell talks to; https unless it is a local one")
    ("timeout" :integer :default 60 :min 1
     :doc "seconds one request may take")
    ("name" :string :default ""
     :doc "the site name a publish uses when the folder's own says nothing (dist, build, out)"))
  (:tool "webly_publish" (nlk:contract 'webly-publish) 'publish-tool)
  (:tool "webly_share" (nlk:contract 'webly-share) 'share-tool)
  (:tool "webly_site" (nlk:contract 'webly-site) 'site-tool)
  (:help :webly "webly: publish a folder at a live webly.site URL, or share files as links" +primer+)
  (:command "webly" 'run-command
            :description "Publish a site, or share files, with Webly"
            :argument-hint "[publish PATH | share PATH | claim]"))
