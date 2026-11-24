;;;; client.lisp --- Webly's anonymous API: a credential, a site, files, links.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every byte here goes through NLK:HTTP, to the API in the section's
;;;; api_url and to nothing else but the presigned upload URLs that API
;;;; returns. No helper script, no MCP client, no Node.
;;;;
;;;; The anonymous credential the API issues is saved under the home, mode
;;;; 0600, holds the API it belongs to, and rides every request as a Bearer
;;;; token to that one API. A machine has one anonymous site; publishing
;;;; again updates it in place, at the same URL.

(in-package #:nodecode-webly)

;;; --- what the API keeps, mirrored so nothing too big is sent ------------------

(defparameter +max-files+ 25 "Files one anonymous site takes.")
(defparameter +max-file-bytes+ (* 1024 1024) "One file's ceiling.")
(defparameter +max-total-bytes+ (* 5 1024 1024) "A whole site's ceiling.")

(defparameter +generic-names+
  '("dist" "build" "out" "public" "site" "www" "_site" "index" "src" "app"
    "web" "website" "html" "tmp" "temp" "untitled" "new folder")
  "Folder names that say nothing about the site, so a publish asks for one.")

;;; --- the one network call ----------------------------------------------------

(defun request (method url &key headers content)
  "The cell's one network call: NLK:HTTP under the section's timeout.
A test stubs NLK:HTTP itself, so this stays the real seam."
  (let ((timeout (setting :timeout)))
    (nlk:http method url :headers headers :content content
                        :timeout timeout :connect-timeout (min 10 timeout))))

(defun api-base ()
  "The API this cell talks to, from the section, with its trailing / off."
  (let ((base (string-right-trim "/" (setting :api-url))))
    (unless (or (uiop:string-prefix-p "https://" base)
                (uiop:string-prefix-p "http://localhost" base)
                (uiop:string-prefix-p "http://127.0.0.1" base))
      (fail "api_url ~s is no Webly API: use https, or http://localhost for a local one" base))
    base))

(defun api-call (method path &key token content)
  "One anonymous-API call METHOD on PATH => (VALUES BODY STATUS), BODY the
decoded JSON object or NIL."
  (let* ((url (format nil "~a/public/v1/anonymous~a" (api-base) path))
         (body (typecase content
                 (null nil)
                 (string content)
                 (t (nlk:encode-json-object content))))
         (headers (append (when body '(("content-type" . "application/json")))
                          (when token
                            (list (cons "authorization" (format nil "Bearer ~a" token)))))))
    (multiple-value-bind (text status) (request method url :headers headers :content body)
      (values (and text (plusp (length text)) (ignore-errors (nlk:decode-json text)))
              status))))

(defun api-refusal (body status fallback)
  "Signal the API's own words about a failed call."
  (declare (ignore status))
  (let ((message (nlk:json-value body :string "message"))
        (reason (nlk:json-value body :string "details" "reason")))
    (fail "~a~@[ [~a]~]" (or message fallback) reason)))

;;; --- the credential, under the home -------------------------------------------

(defun state-path ()
  "Where this machine's Webly credential lives."
  (nlk:home "webly/state.json"))

(defun read-state ()
  "The saved state object, or NIL."
  (let ((text (ignore-errors (uiop:read-file-string (state-path)))))
    (and text
         (let ((object (ignore-errors (nlk:decode-json text))))
           (and (hash-table-p object) object)))))

(defun saved-token ()
  "The credential saved for this API, or NIL."
  (let* ((state (read-state))
         (api (nlk:json-value state :string "api"))
         (token (nlk:json-value state :string "token")))
    (and api token
         (string= api (api-base))
         (uiop:string-prefix-p "wa_" token)
         token)))

(defun save-token (token)
  "Write TOKEN, with the API it belongs to, mode 0600."
  (let ((path (state-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence (nlk:encode-json-octets
                       (nlk:make-json-object "api" (api-base) "token" token))
                      out))
    (ignore-errors (sb-posix:chmod (namestring path) #o600))
    token))

(defun ensure-token ()
  "The saved credential, issuing and saving one the first time."
  (or (saved-token)
      (multiple-value-bind (body status) (api-call :post "/credentials")
        (unless (and (<= 200 status 299) body)
          (api-refusal body status (format nil "could not get a site credential (HTTP ~a)" status)))
        (let ((token (nlk:json-value body :string "token")))
          (unless (and token (uiop:string-prefix-p "wa_" token))
            (fail "the API answered a credential this cell does not recognize"))
          (save-token token)))))

;;; --- the site ------------------------------------------------------------------

(defun current-site (token)
  "This machine's anonymous site => (VALUES SITE STATUS); SITE is NIL when
nothing was published yet."
  (multiple-value-bind (body status) (api-call :get "/sites/current" :token token)
    (cond ((and (<= 200 status 299) (hash-table-p body)) (values body status))
          ((= status 404) (values nil status))
          (t (api-refusal body status (format nil "could not read this machine's site (HTTP ~a)" status))))))

(defun site-url (site)
  "The address SITE answers at."
  (nlk:json-value site :string "urls" "published"))

(defun site-lines (site)
  "A few lines about SITE: what it is, where it is, how long it stays."
  (let ((name (nlk:json-value site :string "name"))
        (status (nlk:json-value site :string "status"))
        (url (site-url site))
        (live (nlk:json-value site :string "liveUntil"))
        (claim (nlk:json-value site :string "claimUntil"))
        (next (nlk:json-value site :string "next" "message")))
    (format nil "~@[~a~]~@[ (~a)~]~@[~%Live: ~a~]~@[~%Online until ~a.~]~@[~%Claim it until ~a to keep it for good (/webly claim opens the page).~]~@[~%Next: ~a~]"
            name status url live claim next)))

(defun site-name (path name)
  "The site's name: NAME, else the file's or folder's, refusing a generic one."
  (let* ((given (nlk:trimmed (or name "")))
         (fallback (if (uiop:file-exists-p path)
                       (pathname-name path)
                       (car (last (pathname-directory
                                   (uiop:ensure-directory-pathname path))))))
         (candidate (if (plusp (length given)) given (princ-to-string fallback))))
    (when (and (not (plusp (length given)))
               (member (string-downcase candidate) +generic-names+ :test #'string=))
      (fail "~s says nothing about the site: pass a name (name), the one its dashboard and claim page show (the web address is assigned for you)"
            candidate))
    candidate))

;;; --- the files a publish sends ------------------------------------------------

(defun read-bytes (path)
  "PATH's bytes."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun utf-8-text (octets)
  "OCTETS as a string when they are UTF-8 text, else NIL."
  (and (not (find 0 octets))
       (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
         (error () nil))))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The standard base64 alphabet.")

(defun base64 (octets)
  "OCTETS as standard base64 with padding."
  (let ((length (length octets)))
    (with-output-to-string (out)
      (loop for i from 0 below length by 3
            for a = (aref octets i)
            for b = (and (< (1+ i) length) (aref octets (1+ i)))
            for c = (and (< (+ i 2) length) (aref octets (+ i 2)))
            do (write-char (char +base64-alphabet+ (ash a -2)) out)
               (write-char (char +base64-alphabet+
                                 (logand 63 (ash (logior (ash a 8) (or b 0)) -4))) out)
               (write-char (if b
                               (char +base64-alphabet+
                                     (logand 63 (ash (logior (ash (or b 0) 8) (or c 0)) -6)))
                               #\=)
                           out)
               (write-char (if c (char +base64-alphabet+ (logand 63 c)) #\=) out)))))

(defun file-object (wire path)
  "One file as the API takes it: its text, or its bytes in base64."
  (let* ((bytes (read-bytes path))
         (text (utf-8-text bytes)))
    (if text
        (nlk:make-json-object "path" wire "content" text)
        (nlk:make-json-object "path" wire "content" (base64 bytes) "encoding" "base64"))))

(defun dir-name (path)
  "The last name of the directory PATH."
  (car (last (pathname-directory (uiop:ensure-directory-pathname path)))))

(defun folder-files (dir prefix found)
  "FOUND gains (WIRE . PATH) for every file under DIR, hidden entries and
node_modules left out."
  (dolist (file (uiop:directory-files dir))
    (let ((name (file-namestring file)))
      (unless (char= #\. (char name 0))
        (push (cons (format nil "~a/~a" prefix name) file) found))))
  (dolist (sub (uiop:subdirectories dir))
    (let ((name (dir-name sub)))
      (unless (or (string= name "node_modules") (char= #\. (char name 0)))
        (setf found (folder-files sub (format nil "~a/~a" prefix name) found)))))
  found)

(defun site-files (path)
  "PATH's files as (WIRE . PATH), the anonymous limits applied."
  (let ((found (cond ((uiop:file-exists-p path)
                      (list (cons (if (equal "html" (string-downcase (or (pathname-type path) "")))
                                      "/index.html"
                                      (format nil "/~a" (file-namestring path)))
                                  path)))
                     ((uiop:directory-exists-p path)
                      (sort (folder-files (uiop:ensure-directory-pathname path) "" '())
                            #'string< :key #'car))
                     (t (fail "no such file or folder: ~a" path)))))
    (unless found (fail "no files to publish in ~a" path))
    (when (> (length found) +max-files+)
      (fail "~d files; an anonymous site takes at most ~d: publish a built folder, or sign in at webly.ai for a bigger one"
            (length found) +max-files+))
    (loop with total = 0
          for (wire . file) in found
          for size = (with-open-file (in file :element-type '(unsigned-byte 8))
                       (file-length in))
          do (when (> size +max-file-bytes+)
               (fail "~a is larger than 1 MiB, the anonymous per-file limit" wire))
             (incf total size)
          finally (when (> total +max-total-bytes+)
                    (fail "the files add up to more than 5 MiB, the anonymous per-site limit")))
    found))

;;; --- publishing ----------------------------------------------------------------

(defun deploy (path name)
  "PATH as this machine's live anonymous site => the summary text."
  (let* ((token (ensure-token))
         (files (site-files path))
         (form (nlk:make-json-object
                "kind" "static"
                "files" (coerce (mapcar (lambda (entry) (file-object (car entry) (cdr entry)))
                                        files)
                                'vector))))
    (multiple-value-bind (site status) (current-site token)
      (declare (ignore status))
      (if (and site (not (equal "storage" (nlk:json-value site :string "kind"))))
          (progn
            (setf (gethash "expectedHeadVersion" form)
                  (nlk:json-value site :integer "headVersion"))
            (multiple-value-bind (body code)
                (api-call :put (format nil "/sites/~a" (nlk:json-value site :string "id"))
                          :token token :content form)
              (unless (and (<= 200 code 299) body)
                (api-refusal body code (format nil "the site was not published (HTTP ~a)" code)))
              (format nil "Updated this machine's site.~%~a" (site-lines body))))
          (progn
            (setf (gethash "name" form) (site-name path name))
            (multiple-value-bind (body code) (api-call :post "/sites" :token token :content form)
              (unless (and (<= 200 code 299) body)
                (api-refusal body code (format nil "the site was not published (HTTP ~a)" code)))
              (format nil "Published.~%~a" (site-lines body))))))))

;;; --- sharing files -------------------------------------------------------------

(defparameter +mime+
  '(("pdf" . "application/pdf") ("png" . "image/png") ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg") ("gif" . "image/gif") ("webp" . "image/webp")
    ("svg" . "image/svg+xml") ("mp4" . "video/mp4") ("webm" . "video/webm")
    ("mov" . "video/quicktime") ("mp3" . "audio/mpeg") ("m4a" . "audio/mp4")
    ("wav" . "audio/wav") ("ogg" . "audio/ogg") ("txt" . "text/plain")
    ("md" . "text/markdown") ("csv" . "text/csv") ("json" . "application/json")
    ("zip" . "application/zip") ("html" . "text/html"))
  "The types a browser needs named; anything else goes up as octets.")

(defun mime-of (path)
  (or (cdr (assoc (string-downcase (or (pathname-type path) "")) +mime+ :test #'string=))
      "application/octet-stream"))

(defun share-entries (paths)
  "Every file PATHS names, as (NAME . PATH), a folder keeping its structure."
  (let ((entries '()))
    (dolist (path paths)
      (cond ((uiop:file-exists-p path)
             (push (cons (file-namestring path) path) entries))
            ((uiop:directory-exists-p path)
             (let ((prefix (and (> (length paths) 1) (dir-name path))))
               (dolist (pair (folder-files (uiop:ensure-directory-pathname path) "" '()))
                 (push (cons (if prefix
                                  (format nil "~a/~a" prefix (string-left-trim "/" (car pair)))
                                  (string-left-trim "/" (car pair)))
                             (cdr pair))
                       entries))))
            (t (fail "no such file or folder: ~a" path))))
    (when (zerop (length entries))
      (fail "no files to share in ~a" (format nil "~{~a~^, ~}" paths)))
    (nreverse entries)))

(defun header-alist (object)
  "OBJECT, a decoded JSON object of headers, as an alist."
  (when (hash-table-p object)
    (loop for key being the hash-keys of object using (hash-value value)
          when (stringp value) collect (cons key value))))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

(defun storage-site (token name)
  "The machine's shared-files site, created on first use."
  (multiple-value-bind (site status) (api-call :get "/sites/storage" :token token)
    (if (and (<= 200 status 299) site
             (not (equal "offline" (nlk:json-value site :string "status"))))
        site
        (multiple-value-bind (body code)
            (api-call :post "/sites" :token token
                      :content (nlk:make-json-object
                                "kind" "storage"
                                "name" (if (plusp (length (nlk:trimmed (or name ""))))
                                           name "Shared files")))
          (unless (and (<= 200 code 299) body)
            (api-refusal body code (format nil "could not open a place to share files (HTTP ~a)" code)))
          body))))

(defun share-links (paths name)
  "PATHS as public links for 24 hours => the summary text."
  (let* ((token (ensure-token))
         (entries (share-entries paths))
         (site (storage-site token name))
         (id (nlk:json-value site :string "id"))
         (form (nlk:make-json-object
                "files" (coerce (mapcar (lambda (entry)
                                           (nlk:make-json-object
                                            "name" (car entry)
                                            "byteSize" (file-size (cdr entry))
                                            "mimeType" (mime-of (cdr entry))))
                                         entries)
                                'vector))))
    (multiple-value-bind (begun status)
        (api-call :post (format nil "/sites/~a/objects/uploads" id) :token token :content form)
      (unless (and (<= 200 status 299) begun)
        (api-refusal begun status (format nil "the upload was refused (HTTP ~a)" status)))
      (let ((slots (nlk:json-value begun :array "files")))
        (loop for slot across slots
              for entry in entries
              for upload = (nlk:json-value slot :object "upload")
              for url = (nlk:json-value upload :string "url")
              when url
                do (let ((headers (header-alist (nlk:json-value upload :object "headers"))))
                     (when (uiop:string-prefix-p (api-base) url)
                       (push (cons "authorization" (format nil "Bearer ~a" token)) headers))
                     (multiple-value-bind (body code)
                         (request :put url :headers headers :content (read-bytes (cdr entry)))
                       (declare (ignore body))
                       (unless (<= 200 code 299)
                         (fail "uploading ~a failed (HTTP ~a)" (car entry) code)))))
        (multiple-value-bind (finished code)
            (api-call :post (format nil "/sites/~a/objects/finalize" id) :token token
                      :content (nlk:make-json-object
                                "ids" (coerce (loop for slot across slots
                                                     collect (nlk:json-value slot :string "id"))
                                              'vector)))
          (unless (and (<= 200 code 299) finished)
            (api-refusal finished code (format nil "the upload did not finish (HTTP ~a)" code)))
          (share-summary begun finished))))))

(defun share-summary (begun finished)
  "The links the finished uploads came back with, and when they stop working."
  (with-output-to-string (out)
    (let ((folder (nlk:json-value begun :string "folderUrl"))
          (expires (nlk:json-value begun :string "expiresAt"))
          (files (nlk:json-value finished :array "files")))
      (format out "Shared ~d file~:p." (length files))
      (when expires (format out " The links work until ~a." expires))
      (when folder (format out "~%All of them: ~a" folder))
      (loop for file across files
            for url = (nlk:json-value file :string "url")
            for name = (nlk:json-value file :string "name")
            when url do (format out "~%~@[~a: ~]~a" name url)))))

;;; --- status and the claim page ------------------------------------------------

(defun status-text (&key (refresh t))
  "What this machine has on Webly: the site, its deadlines, the next step."
  (let ((token (saved-token)))
    (if (null token)
        "No Webly site is saved on this machine yet: (webly:publish \"/path/to/site\") publishes a folder at a live webly.site URL for 24 hours, claimable for 7 days."
        (multiple-value-bind (site status) (current-site token)
          (cond ((and (not refresh) (<= 200 status 299)) "A Webly credential is saved for this machine.")
                ((null site) "A Webly credential is saved, but nothing is published with it yet.")
                (t (site-lines site)))))))

(defun claim-text ()
  "The page that claims this machine's site, its token in the fragment."
  (let ((token (saved-token)))
    (unless token
      (fail "nothing is saved to claim: publish a site first"))
    (multiple-value-bind (site status) (current-site token)
      (unless (and (<= 200 status 299) site)
        (api-refusal site status (format nil "there is no claimable site (HTTP ~a)" status)))
      (let ((page (or (nlk:json-value site :string "claimPage")
                      (format nil "~a/claim" (api-base)))))
        (format nil "Open this page to sign in and claim the site into an account; it carries the site's token in the fragment, so do not paste it anywhere else:~%~a#token=~a~%~a"
                page token (site-lines site))))))
