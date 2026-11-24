;;;; bridge.lisp --- handing work to a coworker that is not this process.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original supports two ways out and no third (bridge.py): a webhook
;;;; routine the user runs, POSTed as JSON, and a shared folder the coworker
;;;; reads -- handoffs/<id>.json written, results/<id>.json|md|txt read back.
;;;; Both fail closed: with neither configured, a handoff is a clear refusal
;;;; with instructions, never a call into nothing.
;;;;
;;;; Its guards come too: https only (plain http for a loopback host), no user
;;;; info in the URL, the URL shown as scheme://host/... so a token in its path
;;;; or query is never printed, a strict job-id pattern before any path is
;;;; built from one, no symlinked result file, and a 2 MB ceiling on one.

(in-package #:nodecode-staff)

(defconstant +max-result-bytes+ 2000000 "One inbox result's ceiling, as the original bounds it.")

(defparameter +setup-hint+
  "No handoff path is configured. Set `webhook_url` (a webhook routine you run yourself gets the job POSTed as JSON) or `inbox_dir` (a shared folder the coworker reads) in the staff section of config.jsonc or in /settings, then (restart-cells \"nodecode-staff\")."
  "What a handoff says when neither path is set.")

;;; --- the URL --------------------------------------------------------------

(defun url-parts (url)
  "URL as (values SCHEME AUTHORITY), or NIL when it is not scheme://authority..."
  (let ((mark (search "://" url)))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (char) (member char '(#\/ #\? #\#))) rest)
                      (length rest))))
        (values scheme (subseq rest 0 end))))))

(defun url-host (authority)
  "AUTHORITY's host: its user info and its port taken off."
  (let* ((at (position #\@ authority))
         (host (if at (subseq authority (1+ at)) authority)))
    (if (and (plusp (length host)) (char= #\[ (char host 0)))
        (subseq host 0 (min (length host) (1+ (or (position #\] host) (1- (length host))))))
        (subseq host 0 (or (position #\: host) (length host))))))

(defun loopback-host-p (host)
  "Whether HOST names this machine: localhost, 127.0.0.0/8, or ::1."
  (let ((host (string-downcase host)))
    (or (string= host "localhost")
        (string= host "::1")
        (string= host "[::1]")
        (and (> (length host) 3) (string= "127." (subseq host 0 4))))))

(defun validate-webhook-url (url)
  "NIL when URL is a webhook the cell may POST to, else why not."
  (let ((url (trim url)))
    (cond
      ((zerop (length url)) "webhook URL is empty")
      (t
       (multiple-value-bind (scheme authority) (url-parts url)
         (cond
           ((null scheme) "webhook URL does not parse")
           ((find #\@ authority) "webhook URL must not contain user info")
           ((null (url-host authority)) "webhook URL has no host")
           ((string= scheme "https") nil)
           ((and (string= scheme "http") (loopback-host-p (url-host authority))) nil)
           (t "webhook URL must be https (plain http is allowed only for localhost)")))))))

(defun redact-url (url)
  "URL as scheme://host/...: its path and query, which carry tokens, never shown."
  (multiple-value-bind (scheme authority) (url-parts (trim url))
    (if (and scheme authority (plusp (length (url-host authority))))
        (format nil "~a://~a/..." scheme (url-host authority))
        "(invalid)")))

;;; --- the POST -------------------------------------------------------------

(defun post-webhook (url payload timeout)
  "PAYLOAD, one JSON string, POSTed to URL. => (values OK DETAIL)."
  (let ((wrong (validate-webhook-url url)))
    (if wrong
        (values nil wrong)
        (handler-case
            (multiple-value-bind (answer status)
                (nlk:http :post (trim url)
                          :headers '(("content-type" . "application/json")
                                     ("user-agent" . "nodecode-staff/0.1"))
                          :content payload
                          :timeout (max 1 (min (or timeout 15) 120)))
              (declare (ignore answer))
              (if (<= 200 status 299)
                  (values t (format nil "HTTP ~d" status))
                  (values nil (format nil "webhook answered HTTP ~d" status))))
          (error (condition)
            (values nil (format nil "webhook unreachable: ~a" condition)))))))

;;; --- the shared folder ----------------------------------------------------

(defun inbox-root (dir)
  "DIR as a directory pathname, its `~' expanded."
  (uiop:ensure-directory-pathname (path-from-string (or dir ""))))

(defun safe-id-p (id)
  "Whether ID is a job id a path may be built from."
  (and (stringp id)
       (plusp (length id))
       (<= (length id) 64)
       (every (lambda (char) (or (alphanumericp char) (char= char #\-) (char= char #\_))) id)))

(defun write-inbox-handoff (dir payload id)
  "PAYLOAD, one JSON string, as DIR/handoffs/ID.json. => (values OK DETAIL)."
  (cond
    ((not (safe-id-p id)) (values nil "invalid job id"))
    (t
     (handler-case
         (let ((file (merge-pathnames (format nil "handoffs/~a.json" id) (inbox-root dir))))
           (ensure-directories-exist file)
           (ensure-directories-exist (merge-pathnames "results/" (inbox-root dir)))
           (nlk:write-file-atomically file payload)
           (values t (format nil "~a" file)))
       (error (condition)
         (values nil (format nil "could not write inbox file: ~a" condition)))))))

(defun symlink-p (pathname)
  "Whether PATHNAME itself is a symbolic link, not what it points at."
  (handler-case
      (= (logand (sb-posix:stat-mode (sb-posix:lstat (uiop:native-namestring pathname)))
                 sb-posix:s-ifmt)
         sb-posix:s-iflnk)
    (error () nil)))

(defun file-bytes (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (file-length in)))

(defun result-text (pathname)
  "PATHNAME's text, or NIL when it is absent, a link, or over the ceiling. A
.json result answers its `result' or `text' member, else the file itself."
  (when (probe-file pathname)
    (unless (or (symlink-p pathname) (> (file-bytes pathname) +max-result-bytes+))
      (let ((text (ignore-errors (uiop:read-file-string pathname))))
        (when text
          (if (string= "json" (string-downcase (pathname-type pathname)))
              (let ((object (ignore-errors (nlk:decode-json text))))
                (if (hash-table-p object)
                    (or (field object "result") (field object "text") text)
                    text))
              text))))))

(defun read-inbox-result (dir id)
  "The answer ID's job got in DIR, read off results/ID.json, .md or .txt, or
NIL when the coworker has not answered yet."
  (when (and (plusp (length (trim (or dir "")))) (safe-id-p id))
    (let ((root (merge-pathnames "results/" (inbox-root dir))))
      (loop for type in '("json" "md" "txt")
            thereis (result-text (merge-pathnames (format nil "~a.~a" id type) root))))))
