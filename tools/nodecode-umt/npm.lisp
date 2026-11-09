;;;; npm.lisp --- the npm registry: a search, one package, its versions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; registry.npmjs.org answers a package's metadata document at /NAME (every
;;;; version, the dist-tags and the publish times) and a text search at
;;;; /-/v1/search; both are public and keyless. A scoped name such as
;;;; @scope/name is percent-encoded as one path segment, which the registry
;;;; accepts.

(in-package #:nodecode-umt)

(defparameter +npm-limit+ 50 "How many search results or versions one call may answer with.")
(defparameter +npm-search-limit+ 50)

(defun npm-registry-base () (string-right-trim "/" (setting :npm-registry-base)))

(defun npm-metadata (name)
  "NAME's registry document, decoded; a name the registry does not know is a
refusal that says so."
  (let ((name (nlk:trimmed (princ-to-string name))))
    (unless (plusp (length name)) (fail "a package name is required"))
    (let ((payload (http-json (format nil "~a/~a" (npm-registry-base) (url-encode name))
                               :what "The npm registry")))
      (unless (hash-table-p payload)
        (fail "The npm registry answered no metadata for ~s" name))
      payload)))

(defun npm-tags (payload)
  "PAYLOAD's dist-tags as an alist of (tag . version), newest tag first."
  (let ((tags '())
        (table (nlk:json-value payload :object "dist-tags")))
    (when (hash-table-p table)
      (maphash (lambda (tag version)
                 (when (and (stringp tag) (stringp version))
                   (push (cons tag version) tags)))
               table))
    (sort tags #'string< :key #'car)))

(defun npm-pairs (tags)
  "TAGS' (tag . version) as the flat list FORMAT's ~{~a=~a~} reads."
  (loop for (tag . version) in tags append (list tag version)))

(defun npm-versions (payload)
  "PAYLOAD's versions, newest first, each (version published-at deprecated-p)."
  (let ((versions (nlk:json-value payload :object "versions"))
        (times (nlk:json-value payload :object "time"))
        (rows '()))
    (when (hash-table-p versions)
      (maphash (lambda (version detail)
                 (when (stringp version)
                   (push (list version
                               (and (hash-table-p times) (nlk:json-value times :string version))
                               (and (hash-table-p detail)
                                    (plusp (length (or (nlk:json-value detail :string "deprecated") "")))))
                         rows)))
               versions))
    (sort rows (lambda (left right)
                 (string> (or (second left) "") (or (second right) ""))))))

(defun npm-size (bytes)
  "BYTES as KB or MB when it is an integer, else NIL."
  (when (and (integerp bytes) (plusp bytes))
    (if (>= bytes (* 1024 1024))
        (format nil "~,1f MB" (/ bytes (* 1024.0 1024.0)))
        (format nil "~,0f KB" (/ bytes 1024.0)))))

;;; --- the rendering ---------------------------------------------------------------

(defun render-search (heading payload)
  "HEADING, then one package a line: name@version, its date, its description,
and its npm page."
  (let ((objects (nlk:json-array payload "objects")))
    (if (zerop (length objects))
        (format nil "~a: nothing found." heading)
        (with-output-to-string (out)
          (format out "~a (~d):~%" heading (length objects))
          (loop for object across objects
                for package = (nlk:json-value object :object "package")
                for index from 1
                when package
                  do (format out "~d. ~a@~a~%" index
                             (or (nlk:json-value package :string "name") "?")
                             (or (nlk:json-value package :string "version") "?"))
                     (let ((description (nlk:json-value package :string "description")))
                       (when description
                         (format out "   ~a~%" (nlk:clip (nlk:one-line description :cap 200) 200 :ellipsis "..."))))
                     (let ((date (nlk:json-value package :string "date")))
                       (when date (format out "   published ~a~%" date)))
                     (let ((links (nlk:json-value package :object "links")))
                       (when links
                         (format out "   ~@[~a~]~@[ - ~a~]~%"
                                 (nlk:json-value links :string "npm")
                                 (nlk:json-value links :string "repository")))))))))

(defun render-metadata (payload)
  "One package: what it is, who publishes it, and how to reach its source."
  (with-output-to-string (out)
    (format out "~a~@[ - ~a~]~%" (or (nlk:json-value payload :string "name") "?")
            (nlk:json-value payload :string "description"))
    (let ((latest (nlk:json-value (nlk:json-value payload :object "dist-tags") :string "latest")))
      (format out "latest ~a~@[ (published ~a)~]~%"
              (or latest "unknown")
              (and latest
                   (nlk:json-value (nlk:json-value payload :object "time") :string latest))))
    (format out "license ~a; ~d versions~%"
            (or (nlk:json-value payload :string "license") "unknown")
            (length (npm-versions payload)))
    (let ((tags (npm-tags payload)))
      (when tags
        (format out "dist-tags: ~{~a=~a~^, ~}~%" (npm-pairs tags))))
    (let ((homepage (nlk:json-value payload :string "homepage"))
          (repository (nlk:json-value (nlk:json-value payload :object "repository") :string "url")))
      (when homepage (format out "homepage: ~a~%" homepage))
      (when repository (format out "repository: ~a~%" repository)))
    (let ((names (loop for maintainer across (nlk:json-array payload "maintainers")
                        for name = (nlk:json-value maintainer :string "name")
                        when name collect name)))
      (when names (format out "maintainers: ~{~a~^, ~}~%" names)))))

(defun render-versions (payload limit)
  "One package's newest LIMIT versions, newest first, with dates."
  (let ((rows (npm-versions payload)))
    (with-output-to-string (out)
      (format out "~a: ~d versions~@[, newest ~a~]~%"
              (or (nlk:json-value payload :string "name") "?")
              (length rows)
              (first (first rows)))
      (let ((tags (npm-tags payload)))
        (when tags (format out "dist-tags: ~{~a=~a~^, ~}~%" (npm-pairs tags))))
      (loop for (version published deprecated) in (subseq rows 0 (min limit (length rows)))
            do (format out "~a~@[ - ~a~]~:[~; [deprecated]~]~%" version published deprecated)))))

;;; --- the verbs --------------------------------------------------------------------

(define-verb search-packages (query &key (limit 10) (from 0))
  "Search the npm registry for packages matching QUERY: LIMIT results (10 by
default, 50 at most) from offset FROM, each with its name, latest version,
publish date, description and links."
  (let ((query (nlk:trimmed (princ-to-string query))))
    (unless (plusp (length query)) (fail "a search needs a query"))
    (let ((url (format nil "~a/-/v1/search?text=~a&size=~d&from=~d"
                        (npm-registry-base) (url-encode query)
                        (bounded limit 1 +npm-search-limit+)
                        (max 0 (if (integerp from) from 0)))))
      (render-search (format nil "npm packages matching ~s" query)
                     (http-json url :what "The npm registry")))))

(define-verb package-metadata (name)
  "One npm package's registry metadata by NAME: its latest version and when it
was published, its licence, how many versions exist, its dist-tags, its
homepage, its repository and its maintainers."
  (render-metadata (npm-metadata name)))

(define-verb package-versions (name &key (limit 20))
  "An npm package's versions, newest first: at most LIMIT (20 by default, 50
at most), each with its publish date and whether it is deprecated, after the
package's dist-tags."
  (render-versions (npm-metadata name) (bounded limit 1 +npm-limit+)))
