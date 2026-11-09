;;;; cell-test.lisp --- lifecycle; a publish; an update; sharing; the refusals.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here touches the network: NLK:HTTP is stubbed for every test and
;;;; records what each call carried, so what is proved is the cell's own
;;;; wiring -- the credential it saves, the body it publishes, the site it
;;;; updates, the uploads it finishes -- and not Webly's answers.

(in-package #:nodecode.test)

(define-test-slice "webly" "WEBLY-CELL-" :start nodecode-webly:start-cell)

(define-cell-lifecycle-tests "webly"
  (:help :webly)
  (:command "webly")
  (:running (is (string= "https://api.webly.ai" (nodecode-webly::setting :api-url)) "api_url defaults to Webly's own API")
            (is (= 60 (nodecode-webly::setting :timeout))))
  (:refused ("timeout" 0) ("api_url" 5))
  (:idle webly:webly-error (webly:status)))

;;; --- fixtures ----------------------------------------------------------------------

(defparameter +site-live+
  "{\"id\":\"ws_1\",\"name\":\"demo\",\"kind\":\"static\",\"status\":\"live\",\"headVersion\":7,\"urls\":{\"published\":\"https://anon-demo.webly.site/\"},\"liveUntil\":\"2026-11-09T10:00:00Z\",\"claimUntil\":\"2026-11-15T10:00:00Z\",\"claimPage\":\"https://app.webly.ai/claim\",\"next\":{\"message\":\"live; claim it to keep it\",\"actions\":[\"claim\"]}}"
  "A site as the API answers it, with one published URL and both deadlines.")

(defparameter +site-created+
  "{\"id\":\"ws_1\",\"name\":\"demo\",\"kind\":\"static\",\"status\":\"live\",\"headVersion\":1,\"urls\":{\"published\":\"https://anon-demo.webly.site/\"},\"liveUntil\":\"2026-11-09T10:00:00Z\",\"claimUntil\":\"2026-11-15T10:00:00Z\",\"claimPage\":\"https://app.webly.ai/claim\",\"next\":{\"message\":\"live\"}}"
  "The same site a first publish answers.")

(defparameter +uploads+
  "{\"folderUrl\":\"https://anon-demo.webly.site/f/abc\",\"zipUrl\":\"https://anon-demo.webly.site/f/abc.zip\",\"expiresAt\":\"2026-11-09T10:00:00Z\",\"files\":[{\"id\":\"o1\",\"name\":\"notes.txt\",\"upload\":{\"url\":\"https://storage.example/u1\",\"headers\":{\"content-type\":\"text/plain\"}}}]}"
  "The upload slots the API hands back for one shared file.")

(defparameter +finalized+
  "{\"files\":[{\"id\":\"o1\",\"name\":\"notes.txt\",\"ok\":true,\"url\":\"https://anon-demo.webly.site/f/abc/notes.txt\",\"size\":5}]}"
  "The finished upload, with its public link.")

(defmacro with-webly-cell ((&rest config) &body body)
  "BODY with the cell started on CONFIG and no credential saved yet."
  `(with-cell-stop ((webly-start ,@config))
     (ignore-errors (delete-file (nlk:home "webly/state.json")))
     ,@body))

(defmacro with-webly-api ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL fragment: ROUTES are (FRAGMENT FORM),
FORM answering (VALUES TEXT STATUS). ASKED collects (METHOD URL CONTENT
HEADERS TIMEOUT CONNECT-TIMEOUT), newest first."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
       (push (list method url content headers timeout connect-timeout) ,asked)
       (cond ,@(loop for (fragment form) in routes
                     collect `((search ,fragment url) ,form))
             (t (values "{\"message\":\"no route in this test\"}" 500 nil nil))))
       ,@body)))

(defun webly-post-to (asked fragment)
  "The one request in ASKED that went to a URL ending with FRAGMENT."
  (find-if (lambda (row) (uiop:string-suffix-p (second row) fragment)) asked))

(defun webly-site-dir ()
  "A fresh folder holding one index.html."
  (let ((dir (merge-pathnames (format nil "webly-~36r/" (random most-positive-fixnum))
                               (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "index.html" dir))
    (with-open-file (out (merge-pathnames "index.html" dir)
                          :direction :output :if-exists :supersede)
      (write-string "<h1>hello</h1>" out))
    dir))

(defmacro with-site-dir ((dir) &body body)
  "BODY with DIR a fresh site folder, removed afterwards."
  `(let ((,dir (webly-site-dir)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore))))

;;; --- publishing ---------------------------------------------------------------------

(deftest webly-cell-a-first-publish-saves-a-credential-and-answers-the-url ()
  (with-site-dir (dir)
    (with-webly-cell ()
      (with-webly-api (asked
                       ("/credentials" (values "{\"token\":\"wa_demo\"}" 200 nil nil))
                       ("/sites/current" (values "{\"message\":\"none\"}" 404 nil nil))
                       ("/sites" (values +site-created+ 200 nil nil)))
        (let ((text (webly:publish dir :name "demo")))
          (is (search "https://anon-demo.webly.site/" text) "the live URL")
          (is (search "Claim it until 2026-11-15" text) "the claim deadline")
          (let ((post (webly-post-to asked "/public/v1/anonymous/sites")))
            (is (eq :post (first post)) "a create, not an update")
            (is (search "\"path\":\"/index.html\"" (or (third post) "")) "the page's path")
            (is (search "hello" (or (third post) "")) "the page's bytes"))
          (is (search "wa_demo" (uiop:read-file-string (nlk:home "webly/state.json"))) "the credential is saved for the next publish"))))))

(deftest webly-cell-a-second-publish-updates-the-same-site ()
  (with-site-dir (dir)
    (with-webly-cell ()
      (nodecode-webly::save-token "wa_demo")
      (with-webly-api (asked
                       ("/sites/current" (values +site-live+ 200 nil nil))
                       ("/sites/ws_1" (values +site-live+ 200 nil nil)))
        (let ((text (webly:publish dir :name "demo")))
          (is (search "Updated" text) "the same site, updated in place")
          (let ((put (find :put asked :key #'first)))
            (is put "an update goes to the saved site, not a new one")
            (is (search "expectedHeadVersion\":7" (or (third put) "")) "the version it read")))))))

;;; --- sharing -------------------------------------------------------------------------

(deftest webly-cell-sharing-a-file-uploads-it-and-answers-the-link ()
  (with-webly-cell ()
    (let ((file (merge-pathnames (format nil "webly-~36r.txt" (random most-positive-fixnum))
                                  (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (out file :direction :output :if-exists :supersede)
               (write-string "notes" out))
             (with-webly-api (asked
                              ("/credentials" (values "{\"token\":\"wa_demo\"}" 200 nil nil))
                              ("/objects/finalize" (values +finalized+ 200 nil nil))
                              ("/objects/uploads" (values +uploads+ 200 nil nil))
                              ("/sites/storage" (values "{\"message\":\"none\"}" 404 nil nil))
                              ("storage.example" (values "" 200 nil nil))
                              ("/sites" (values "{\"id\":\"ws_share\",\"kind\":\"storage\"}" 200 nil nil)))
               (let ((text (webly:share file)))
                 (is (search "https://anon-demo.webly.site/f/abc/notes.txt" text) "the file's link")
                 (is (search "Shared 1 file" text) "one file, counted")
                 (is (search (file-namestring file) (or (third (webly-post-to asked "/objects/uploads")) "")) "the file's name")
                 (let ((put (find-if (lambda (row) (search "storage.example" (second row))) asked)))
                   (is (eq :put (first put)) "the bytes go to the URL the API handed back")
                   (is (equalp #(110 111 116 101 115) (third put)) "and they are the file's")))))
        (ignore-errors (delete-file file))))))

;;; --- refusals, and the slash command ------------------------------------------------

(deftest webly-cell-a-publish-without-a-path-is-refused ()
  (is (handler-case (progn (nodecode-webly::decode-webly-publish (nlk:make-json-object)) nil) (nlk:contract-refusal () t)) "the one required member, path, refused its absence"))

(deftest webly-cell-the-slash-command-answers-the-site-and-its-rows ()
  (with-webly-cell ()
    (nodecode-webly::save-token "wa_demo")
    (with-webly-api (asked ("/sites/current" (values +site-live+ 200 nil nil)))
      (multiple-value-bind (text dialog) (webly::run-command "" nil)
        (is (search "anon-demo.webly.site" text) "the site's URL")
        (is (search "live; claim it to keep it" text) "what the API says is next")
        (is (search "anon-demo.webly.site" (princ-to-string dialog)) "the dialog repeats it")
        (is (search "/webly claim" (princ-to-string dialog)) "and offers the claim page"))
      (is (search "https://app.webly.ai/claim#token=wa_demo" (webly:claim-link)) "the claim page carries the token in the fragment"))))
