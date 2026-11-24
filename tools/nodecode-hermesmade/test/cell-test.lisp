;;;; cell-test.lisp --- lifecycle, the three tools and /hermesmade, against a stubbed shelf.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches hermesmade.lol and
;;;; no test needs a network. What is proved is the cell's own wiring: the
;;;; section, the three tools through the seam the model calls them by - the
;;;; url, the body, the answer - the operator's command, and the two refusals
;;;; that matter: a package that is not a bot's, and a folder holding a key.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values BODY STATUS).")

(defmacro with-shelf (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials the shelf."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected hermesmade request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       (ignore-errors (delete-file (nodecode-hermesmade::state-path)))
       ,@body)))

(defun shelf-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun octets (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun test-package (&optional (files '(("SOUL.md" . "You are a chef who plans a week of dinners.")
                                        ("config.yaml" . "agent: {}\n")
                                        ("skills/chef/SKILL.md" . "# Chef\nPlans dinners."))))
  "A bot package as the shelf serves one: a gzipped tar of FILES under chef/."
  (nodecode-hermesmade::gzip-bytes
   (nodecode-hermesmade::tar-bytes
    (mapcar (lambda (file) (cons (car file) (octets (cdr file)))) files)
    "chef/")))

(defun chef-listing (&optional (package (test-package)))
  "The listing JSON the shelf answers for bot 7, carrying PACKAGE's own digest."
  (format nil "{\"bot\": {\"id\": \"7\", \"name\": \"chef\", \"title\": \"Chef\", ~
\"version\": \"1.0.0\", \"sha256\": ~s}}"
          (subseq (nlk:sha256-text package) 7)))

(defun serve-chef (&optional package)
  "Serve the listing and PACKAGE for bot 7, and answer JSON 404 otherwise."
  (let ((package (or package (test-package))))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method args))
            (cond ((search "/package" url) (values package 200))
                  ((search "/api/bots/7" url) (values (chef-listing package) 200))
                  (t (values "{\"error\": \"Bot not found\"}" 404)))))))

(defun shelf-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "hermesmade-call" name arguments)))

(defun bot-dir ()
  "The scratch bots directory this test's section names."
  (nlk:home "hermesmade-test-bots/"))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "hermesmade" "HERMESMADE-CELL-" :start nodecode-hermesmade:start-cell)

(define-cell-lifecycle-tests "hermesmade"
  (:fixture with-shelf)
  (:config "handle" "tester")
  (:help :hermesmade)
  (:command "hermesmade")
  (:running
   (is (nle::find-tool "hermesmade_preview") "a listing can be read")
   (is (nle::find-tool "hermesmade_add") "a bot can be added")
   (is (nle::find-tool "hermesmade_share") "and a folder shared")
   (is (= 3 (count-if (lambda (entry) (eql 0 (search "hermesmade_" (first entry)))) nle::*tools*)) "three of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "hermesmade_preview")) "and stop takes every one back")
   (is (null (nle::find-tool "hermesmade_add")))
   (is (null (nle::find-tool "hermesmade_share"))))
  (:refused ("timeout_seconds" 0)
            ("max_bytes" 10)))

;;; --- what a listing is -------------------------------------------------------------

(deftest hermesmade-cell-preview-reads-the-package-before-anything-installs (with-shelf)
  (serve-chef)
  (with-cell-stop ((hermesmade-start "bots_dir" (namestring (bot-dir))))
    (let ((answer (shelf-call "hermesmade_preview" "{\"bot\": \"https://hermesmade.lol/bots/7-chef\"}")))
      (is (= 2 (length (shelf-asked))) "one listing and one package, and nothing else")
      (is (member :get (mapcar #'first (shelf-asked))) "both are reads")
      (is (search "Chef" answer) "the title")
      (is (search "installs as `chef`" answer) "the name it installs as")
      (is (search "SOUL.md" answer) "every file it ships")
      (is (search "skills/chef/SKILL.md" answer) "the skill's own file")
      (is (search "Skills: chef" answer) "the skill, named")
      (is (null (uiop:directory-exists-p (bot-dir))) "and nothing was written"))))

(deftest hermesmade-cell-a-link-and-a-bare-id-are-one-bot (with-shelf)
  (serve-chef)
  (with-cell-stop ((hermesmade-start))
    (shelf-call "hermesmade_preview" "{\"bot\": \"7\"}")
    (is (equal "https://hermesmade.lol/api/bots/7" (second (first (shelf-asked)))) "a bare id reaches the same listing")
    (let ((answer (shelf-call "hermesmade_preview" "{\"bot\": \"nothing here\"}")))
      (is (search "ERROR: " answer) "a reference with no id in it is the model's refusal")
      (is (search "bot id" answer) "naming what it wanted"))))

;;; --- adding ----------------------------------------------------------------------

(deftest hermesmade-cell-add-writes-the-allowlisted-package-under-the-bots-directory (with-shelf)
  (serve-chef)
  (with-cell-stop ((hermesmade-start "bots_dir" (namestring (bot-dir))))
    (let ((answer (shelf-call "hermesmade_add" "{\"bot\": \"7\"}")))
      (is (search "Added Chef as bot `chef`" answer) "it says what landed and where")
      (is (search "Skills: chef" answer) "with the skill it brought")
      (let ((soul (merge-pathnames "chef/SOUL.md" (bot-dir)))
            (skill (merge-pathnames "chef/skills/chef/SKILL.md" (bot-dir))))
        (is (uiop:file-exists-p soul) "SOUL.md is on disk")
        (is (search "plans a week of dinners" (uiop:read-file-string soul)) "with its text")
        (is (uiop:file-exists-p skill) "and the skill is a file beside it")))))

(deftest hermesmade-cell-a-package-that-is-not-a-bot-is-refused (with-shelf)
  (serve-chef (test-package '(("SOUL.md" . "You are a chef who plans a week of dinners.")
                               ("hooks/evil.py" . "print(1)"))))
  (with-cell-stop ((hermesmade-start "bots_dir" (namestring (bot-dir))))
    (let ((answer (shelf-call "hermesmade_add" "{\"bot\": \"7\"}")))
      (is (search "ERROR: " answer) "the install is refused")
      (is (search "hooks/evil.py" answer) "naming the file a bot is not made of")
      (is (null (uiop:directory-exists-p (bot-dir))) "and nothing was written"))))

(deftest hermesmade-cell-a-package-that-does-not-match-its-checksum-is-refused (with-shelf)
  (let ((package (test-package)))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method args))
            (if (search "/package" url)
                (values package 200)
                (values (format nil "{\"bot\": {\"id\": \"7\", \"name\": \"chef\", \"sha256\": ~s}}"
                                "0000000000000000000000000000000000000000000000000000000000000000")
                        200))))
    (with-cell-stop ((hermesmade-start "bots_dir" (namestring (bot-dir))))
      (let ((answer (shelf-call "hermesmade_add" "{\"bot\": \"7\"}")))
        (is (search "checksum" answer) "the listing's digest is checked before it is unpacked")
        (is (null (uiop:directory-exists-p (bot-dir))) "and nothing was written")))))

;;; --- sharing ------------------------------------------------------------------------

(defun a-bot-folder (&optional (soul "You are a chef who plans a week of dinners."))
  "A folder this test shares: SOUL.md, and one skill of its own."
  (let* ((root (merge-pathnames "chef/" (nlk:home "hermesmade-test-share/")))
         (skill (merge-pathnames "skills/chef/" root)))
    (ensure-directories-exist (merge-pathnames "x" skill))
    (with-open-file (out (merge-pathnames "SOUL.md" root) :direction :output
                                                          :if-exists :supersede :if-does-not-exist :create)
      (write-string soul out))
    (with-open-file (out (merge-pathnames "SKILL.md" skill) :direction :output
                                                               :if-exists :supersede :if-does-not-exist :create)
      (write-string "# Chef\nPlans dinners." out))
    (namestring root)))

(deftest hermesmade-cell-share-packs-scans-and-uploads-under-the-handle (with-shelf)
  (with-cell-stop ((hermesmade-start "handle" "tester"))
    (setf *reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (values (format nil "{\"ok\": true, \"id\": \"12\", \"url\": \"https://hermesmade.lol/bots/12-chef\", \"manage_token\": \"kept\", \"message\": \"Thanks - a person reviews it next.\"}")
                            200)))
    (let ((answer (shelf-call "hermesmade_share"
                              (format nil "{\"folder\": ~s, \"title\": \"Chef\"}"
                                      (a-bot-folder)))))
      (let ((request (first (shelf-asked))))
        (is (eq :post (first request)) "one POST")
        (is (equal "https://hermesmade.lol/api/bots" (second request)) "to the shelf's upload endpoint")
        (is (search "multipart/form-data; boundary=" (cdr (assoc "content-type" (getf (cddr request) :headers)
                                                                  :test #'string-equal))) "as a multipart body")
        (is (null (cdr (assoc "x-manage-token" (getf (cddr request) :headers) :test #'string-equal))) "with no manage token: this is a first share")
        (let ((body (coerce (getf (cddr request) :content) '(vector (unsigned-byte 8)))))
          (is (search "name=\"handle\"" (map 'string #'code-char body)) "carrying the handle")
          (is (search "tester" (map 'string #'code-char body)) "the section's own")))
      (is (search "Thanks - a person reviews it next." answer) "the shelf's own words")
      (is (search "#12" answer) "the listing it became")
      (is (equal "tester" (getf (nodecode-hermesmade::record-for "chef") :handle)) "and the record this machine keeps holds the handle")
      (is (equal "12" (getf (nodecode-hermesmade::record-for "chef") :id)) "and the listing's id, so the next share updates it"))))

(deftest hermesmade-cell-a-folder-holding-a-key-is-refused-before-anything-is-sent (with-shelf)
  (with-cell-stop ((hermesmade-start "handle" "tester"))
    (let ((answer (shelf-call "hermesmade_share"
                              (format nil "{\"folder\": ~s}"
                                      (a-bot-folder "You are a chef. key: sk-abcdefghijklmnop123456")))))
      (is (search "ERROR: " answer) "the share is refused")
      (is (search "SOUL.md" answer) "naming the file")
      (is (search "an API key" answer) "and what it looked like")
      (is (null (shelf-asked)) "and nothing was uploaded"))))

(deftest hermesmade-cell-a-share-without-a-handle-refuses (with-shelf)
  (with-cell-stop ((hermesmade-start))
    (let ((answer (shelf-call "hermesmade_share" (format nil "{\"folder\": ~s}" (a-bot-folder)))))
      (is (search "handle" answer) "the refusal names the handle it needs")
      (is (null (shelf-asked)) "and nothing was uploaded"))))

;;; --- the operator's command ----------------------------------------------------------

(deftest hermesmade-cell-the-command-reads-previews-and-lists (with-shelf)
  (serve-chef)
  (with-cell-stop ((hermesmade-start "handle" "tester"))
    (is (search "shared nothing yet" (nodecode-hermesmade::run-command "" nil)) "no argument says what this machine has shared")
    (is (search "Chef" (nodecode-hermesmade::run-command "preview 7" nil)) "preview reads the listing")
    (is (null (uiop:directory-exists-p (bot-dir))) "and writes nothing")
    (let ((answer (nodecode-hermesmade::run-command "preview nothing" nil)))
      (is (search "ERROR: " answer) "a refusal comes back as the command's answer"))))
