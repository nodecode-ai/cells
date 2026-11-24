;;;; cell-test.lisp --- the tray's rules, at the tool point and through the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the lifecycle the declaration owes (install, stop,
;;;; veto, a refused section); that a watched call's answer leaves the URLs
;;;; it carried, with their titles, once each, and a call not named in the
;;;; section leaves none; that a URL already on the tray keeps its place and
;;;; takes the newer entry; that the cap keeps the newest and drops the
;;;; oldest; that each session has its own list; and that both tools and
;;;; /sources answer from the same list and empty it.
;;;;
;;;; A real store and real hooks: the only fixture is a scratch home, and no
;;;; call here dials anything.

(in-package #:nodecode.test)

(define-test-slice "source-tray" "SOURCE-TRAY-CELL-" :start nodecode-source-tray:start-cell)

(define-cell-lifecycle-tests "source-tray"
  (:hooks :tool)
  (:help :sources)
  (:command "sources")
  (:refused ("max_items" 0) ("max_items" "many"))
  (:running (is (= 40 (nodecode-source-tray::setting :max-items)) "the default cap is the original's 40")
            (is (nodecode-source-tray::watched-p "web_search") "a web search is read on the defaults")
            (is (not (nodecode-source-tray::watched-p "eval")) "the eval tool is not")))

(defmacro with-tray ((session &rest config) &body body)
  "BODY in SESSION's evaluation, over a temp store, the cell started on
CONFIG."
  `(with-temp-store ()
     (ensure-durable-session ,session)
     (with-cell-stop ((source-tray-start ,@config))
       (with-durable-turn (turn ,session :current t)
         (let ((nlk:*scribe-session-id* ,session))
           ,@body)))))

(defun tray-of (session)
  "SESSION's tray, for a test to read."
  (nodecode-source-tray:tray-items session))

(defun tray-urls (session)
  (mapcar (lambda (item) (getf item :url)) (tray-of session)))

(defun call-tool (name answer)
  "Call tool NAME through the :TOOL point with ANSWER as its return, as a
turn's dispatch would."
  (nlk:call-hooked :tool
                   (list :name name
                         :arguments (nlk:make-json-object "query" "lisp cells")
                         :call-id "call-1")
                   (lambda (op) (declare (ignore op)) answer)))

(defun call-list (&optional arguments)
  (funcall (third (nle::find-tool "source_tray_list"))
           (or arguments (make-hash-table :test #'equal))))

(defun call-clear ()
  (funcall (third (nle::find-tool "source_tray_clear")) (make-hash-table :test #'equal)))

;;; --- the seam ------------------------------------------------------------------

(deftest source-tray-cell-records-the-urls-a-watched-answer-carried ()
  (with-tray ("tray-answer")
    (let ((answer (format nil "{\"results\":[{\"title\":\"Cells at a glance\",~@
\"url\":\"https://Example.COM/docs/cells#intro\"},~@
{\"title\":\"Second\",\"url\":\"http://example.org/a\"}]}")))
      (is (equal answer (call-tool "web_search" answer)) "the call itself answers as it would unadvised")
      (let ((items (tray-of "tray-answer")))
        (is (equal '("https://example.com/docs/cells" "http://example.org/a")
                   (mapcar (lambda (item) (getf item :url)) items)) "each URL is on the tray, its host lowercased and its fragment gone")
        (is (equal "Cells at a glance" (getf (first items) :title)) "with the title its result carried")
        (is (equal "web_search" (getf (first items) :tool)) "and the call that opened it")
        (is (equal "lisp cells" (getf (first items) :query)) "and that call's query")))
    ;; plain text, no JSON: the URLs in it are read, each titled by its host
    (call-tool "web_extract" "see https://example.net/x, and http://example.net/y.")
    (is (equal '("https://example.com/docs/cells" "http://example.org/a"
                 "https://example.net/x" "http://example.net/y")
               (tray-urls "tray-answer")) "a page's text is read for URLs, the trailing punctuation dropped")
    (is (equal "example.net" (getf (third (tray-of "tray-answer")) :title)) "a URL found in text is titled by its host")
    ;; a call the section does not name leaves nothing
    (call-tool "eval" "https://not-a-source.example/ignored")
    (is (= 4 (length (tray-of "tray-answer"))) "an unwatched call leaves the tray as it was")
    (call-tool "web_fetch" "https://another.example")
    (is (equal "https://another.example/" (car (last (tray-urls "tray-answer")))) "a watched call naming no path keeps the URL with one")))

;;; --- one URL's place, the cap, and two sessions ---------------------------------

(deftest source-tray-cell-keeps-urls-unique-and-capped ()
  (with-tray ("tray-cap" "max_items" 3)
    (is (= 3 (nodecode-source-tray::setting :max-items)) "the section's cap is the one read")
    (dotimes (index 5)
      (call-tool "web_search"
                  (format nil "{\"results\":[{\"title\":\"t~d\",\"url\":\"https://example.com/~d\"}]}"
                          index index)))
    (is (equal '("https://example.com/2" "https://example.com/3" "https://example.com/4")
               (tray-urls "tray-cap")) "past the cap the oldest URLs leave, newest last")
    (call-tool "web_search"
                "{\"results\":[{\"title\":\"again\",\"url\":\"https://example.com/2\"}]}")
    (let ((items (tray-of "tray-cap")))
      (is (equal '("https://example.com/2" "https://example.com/3" "https://example.com/4")
                 (tray-urls "tray-cap")) "a URL already on the tray keeps its place")
      (is (equal "again" (getf (first items) :title)) "and takes the newer title, as the original did"))
    (is (= 3 (nodecode-source-tray:clear-sources "tray-cap")) "clearing answers how many URLs went")))

(deftest source-tray-cell-keeps-one-list-a-session ()
  (with-tray ("tray-a")
    (ensure-durable-session "tray-b")
    (call-tool "web_search" "https://example.com/only-a")
    (is (equal '("https://example.com/only-a") (tray-urls "tray-a")))
    (is (null (tray-of "tray-b")) "another session's tray holds none of it")))

;;; --- /sources and the two tools ------------------------------------------------

(deftest source-tray-cell-answers-through-the-tools-and-the-command ()
  (with-tray ("tray-tools")
    (call-tool "web_search" "https://example.com/one")
    (let ((json (call-list)))
      (is (search "https://example.com/one" json) "the tool answers the URL")
      (is (search "\"count\":1" json) "with a count")
      (is (search "\"markdown\"" json) "and a markdown list, by default")
      (is (search "1. [" json) "whose first line is the entry"))
    (let ((arguments (make-hash-table :test #'equal)))
      (setf (gethash "markdown" arguments) nil)
      (is (not (search "\"markdown\"" (call-list arguments))) "a call that says markdown false gets the items alone"))
    (multiple-value-bind (text dialog) (source-tray:run-sources "" "tray-tools")
      (is (search "example.com/one" text) "bare /sources answers the list")
      (is (not (null dialog)) "and the panel whose row clears it"))
    (is (search "example.com/one" (source-tray:run-sources "list" "tray-tools")) "/sources list answers it too")
    (is (search "Usage" (source-tray:run-sources "wat" "tray-tools")) "an unknown word answers the usage")
    (is (search "cleared" (source-tray:run-sources "clear" "tray-tools")))
    (is (null (tray-of "tray-tools")) "and the tray is empty")
    (call-tool "web_search" "https://example.com/two")
    (is (search "\"ok\":true" (call-clear)) "source_tray_clear answers ok")
    (is (null (tray-of "tray-tools")) "and empties the same list")
    (is (search "empty" (call-list)) "which the list then says is empty")))

;;; --- a URL, as it is kept ------------------------------------------------------

(deftest source-tray-cell-normalizes-a-url ()
  (is (equal "https://example.com/a"
             (nodecode-source-tray:normalize-url "  HTTPS://Example.com/a#top  ")) "the host is lowercased and the fragment dropped")
  (is (equal "https://example.com/" (nodecode-source-tray:normalize-url "https://example.com")) "a URL naming no path gets one")
  (is (null (nodecode-source-tray:normalize-url "ftp://example.com/a")) "ftp is no URL here")
  (is (null (nodecode-source-tray:normalize-url "example.com/a")) "nor is a bare host")
  (is (null (nodecode-source-tray:normalize-url "https://")) "nor one with no host")
  (is (null (nodecode-source-tray:normalize-url "")) "nor is nothing")
  (is (null (nodecode-source-tray:normalize-url
             (concatenate 'string "https://example.com/" (make-string 500 :initial-element #\a)))) "nor one past 500 characters"))
