;;;; feed-test.lisp --- the bundled feed is whole; the live one wins and a failure says so.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest security-patches-the-bundled-feed-is-whole ()
  (let* ((entries (nodecode-security-patches::read-feed
                   (uiop:read-file-string (nodecode-security-patches::packaged-path))))
         (ids (mapcar (lambda (a) (gethash "id" a)) entries)))
    (is (>= (length entries) 20) "the feed carries the fortnight's advisories")
    (is (= (length ids) (length (remove-duplicates ids :test #'equal))) "ids are unique")
    (dolist (a entries)
      (let ((id (gethash "id" a)))
        (is (stringp (gethash "title" a)) (format nil "~a has a title" id))
        (is (cl-ppcre:scan "^\\d{4}-\\d{2}-\\d{2}$" (gethash "date" a)) (format nil "~a has a date" id))
        (is (member (gethash "severity" a) '("critical" "high" "medium" "low") :test #'equal)
            (format nil "~a has a severity" id))
        (is (plusp (length (nodecode-security-patches:field a "patch"))) (format nil "~a says how to fix it" id))
        (is (every (lambda (u) (uiop:string-prefix-p "http" u)) (nodecode-security-patches:field a "sources"))
            (format nil "~a's sources are links" id))
        (is (plusp (length (nodecode-security-patches:field a "sources"))) (format nil "~a has a source" id))
        (dolist (c (nodecode-security-patches:field a "checks"))
          (is (member (gethash "kind" c) '("version" "npm" "composer" "wp-plugin" "command" "files") :test #'equal)
              (format nil "~a's check kind ~a is one the cell reads" id (gethash "kind" c))))))))

(deftest security-patches-the-live-feed-wins-and-a-failure-says-so (with-stubbed-security)
  (let ((live "{\"format\":1,\"updated\":\"2026-10-23T00:00:00Z\",\"advisories\":[{\"id\":\"only\",\"title\":\"One\",\"checks\":[]}]}"))
    ;; never answered: the bundled copy, and the reason
    (nodecode-security-patches:load-feed :fetch t)
    (is (eq :bundled (getf nodecode-security-patches::*feed-state* :source)))
    (is (>= (length nodecode-security-patches::*feed*) 20))
    (is (search "live fetch failed" (nodecode-security-patches:feed-status)) "the status says the live feed failed")
    ;; answered: the live copy is the authority
    (setf *sp-fetch-answer* (list live 200))
    (nodecode-security-patches:load-feed :fetch t)
    (is (eq :remote (getf nodecode-security-patches::*feed-state* :source)))
    (is (equal '("only") (mapcar (lambda (a) (gethash "id" a)) nodecode-security-patches::*feed*)))
    (is (search "live, written 2026-10-23" (nodecode-security-patches:feed-status)))
    ;; answered before, failing now: that copy stays, and the status says so
    (setf *sp-fetch-answer* '("not json" 200))
    (nodecode-security-patches:load-feed :fetch t)
    (is (eq :stale (getf nodecode-security-patches::*feed-state* :source)))
    (is (equal '("only") (mapcar (lambda (a) (gethash "id" a)) nodecode-security-patches::*feed*)))
    (is (search "is not a feed" (nodecode-security-patches:feed-status)))))
