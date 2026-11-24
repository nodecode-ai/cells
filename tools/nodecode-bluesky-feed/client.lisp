;;;; client.lisp --- one HTTPS GET to the public AppView, and what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin sent one GET to public.api.bsky.app, with no key and
;;;; no Authorization header, to two read methods, and read the same two
;;;; answers this does. A failure is BLUESKY-FEED-ERROR carrying the words the model
;;;; or the operator reads, never a half-empty result.

(in-package #:nodecode-bluesky-feed)

(defparameter +api-base+ "https://public.api.bsky.app"
  "The public AppView: the one host this cell talks to. It asks for no key.")

(defparameter +profile-path+ "/xrpc/app.bsky.actor.getProfile"
  "One account's public profile.")

(defparameter +feed-path+ "/xrpc/app.bsky.feed.getAuthorFeed"
  "One account's recent posts and reposts, a page at a time.")

(defparameter +page-size+ 10
  "Items one page asks for. A page can come back shorter than this.")

(defparameter +user-agent+ "nodecode-bluesky-feed/0.1 (+https://nodecode.ai)"
  "What the AppView sees; no product's name but this cell's own.")

(defparameter +text-cap+ 300
  "How much of one post's text an answer carries.")

(defparameter +label-cap+ 20
  "How many content labels one post reports.")

;;; --- the request ---------------------------------------------------------------------

(defun build-url (path pairs)
  "PATH on the public AppView, with PAIRS as its query. Only the two read
methods are allowed, so no other path is ever dialled."
  (unless (member path (list +profile-path+ +feed-path+) :test #'string=)
    (fail "~s is not one of the two AppView reads this cell sends." path))
  (format nil "~a~a~@[?~a~]"
          +api-base+ path
          (and pairs
               (format nil "~{~a~^&~}"
                       (loop for (key . value) in pairs
                             collect (format nil "~a=~a"
                                             (quri:url-encode (string key) :encoding :utf-8)
                                             (quri:url-encode (or value "") :encoding :utf-8)))))))

(defun api-error-text (text)
  "The AppView's own error and message in TEXT, or NIL."
  (let ((data (ignore-errors (nlk:decode-json text))))
    (when (hash-table-p data)
      (let ((error (nlk:json-value data :text "error"))
            (message (nlk:json-value data :text "message")))
        (when (or (and error (plusp (length error)))
                  (and message (plusp (length message))))
          (format nil "~@[~a~]~@[: ~a~]" error message))))))

(defun answer-data (body status)
  "BODY from a STATUS answer as the decoded JSON object. A failed status, an
empty body, a body that is not a JSON object and a JSON object carrying an
error are all refusals in words."
  (let ((text (cond ((stringp body) body)
                    ((streamp body) "")
                    ((null body) "")
                    (t ""))))
    (cond
      ((not (and (integerp status) (<= 200 status 299)))
       (fail "The public AppView answered HTTP ~a~@[: ~a~]." status (api-error-text text)))
      ((zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
       (fail "The public AppView answered HTTP 200 with an empty body."))
      (t
       (let ((data (ignore-errors (nlk:decode-json text))))
         (unless (hash-table-p data)
           (fail "The public AppView answered HTTP 200 with something that was not a JSON object."))
         (let ((error (nlk:json-value data :text "error")))
           (when (and error (plusp (length error)))
             (fail "The public AppView reported ~@[~a~]~@[: ~a~]."
                   error (nlk:json-value data :text "message"))))
         data)))))

(defvar *http* #'nlk:http
  "The one network seam: NLK:HTTP, or a stub a test binds.")

(defun get-json (path pairs &key timeout)
  "One GET to PATH with PAIRS; the decoded JSON object."
  (let ((url (build-url path pairs))
        (timeout (or timeout (setting :timeout-seconds))))
    (handler-case
        (multiple-value-bind (body status)
            (funcall *http* :get url
                      :timeout timeout
                      :headers (list (cons "Accept" "application/json")
                                     (cons "User-Agent" +user-agent+)))
          (answer-data body status))
      (bluesky-feed-error (condition) (error condition))
      (error (condition)
        (fail "The public AppView could not be read: ~a"
              (nlk:one-line (princ-to-string condition)))))))

;;; --- reading an answer ---------------------------------------------------------------

(defun json-text (object key)
  "OBJECT's KEY as a string, or \"\"."
  (let ((value (and object (nlk:json-value object :text key))))
    (or value "")))

(defun json-count (object key)
  "OBJECT's KEY as an integer, or 0."
  (or (and object (nlk:json-value object :integer key)) 0))

(defun json-array (object key)
  "OBJECT's KEY as a list, whatever the decoder made of the array."
  (let ((value (and object (nlk:json-value object :array key))))
    (cond ((vectorp value) (coerce value 'list))
          ((listp value) value)
          (t '()))))

(defun profile-of (object)
  "One profile as a plist: the fields the plugin kept, and no image address."
  (list :did (json-text object "did")
        :handle (json-text object "handle")
        :name (json-text object "displayName")
        :followers (json-count object "followersCount")
        :follows (json-count object "followsCount")
        :posts (json-count object "postsCount")))

(defun reason-kind (item)
  "What ITEM says it is a repost or a pin of, as \"repost\", \"pin\" or NIL."
  (let ((reason (nlk:json-value item :object "reason")))
    (when (hash-table-p reason)
      (let ((type (json-text reason "$type")))
        (cond ((search "reasonRepost" type) "repost")
              ((search "reasonPin" type) "pin")
              (t "other"))))))

(defun labels-of (post)
  "The content labels POST carries, at most LABEL-CAP of them."
  (loop for label in (json-array post "labels")
        for text = (if (stringp label) label (json-text label "val"))
        when (plusp (length text))
          collect (nlk:clip text 100) into kept
        finally (return (subseq kept 0 (min +label-cap+ (length kept))))))

(defun post-of (item)
  "One feed row as a plist. A row with no post answers the empties."
  (let* ((post (nlk:json-value item :object "post"))
         (author (and post (nlk:json-value post :object "author")))
         (record (and post (nlk:json-value post :object "record")))
         (embed (and post (nlk:json-value post :object "embed")))
         (text (or (and record (nlk:json-value record :text "text")) "")))
    (list :uri (json-text post "uri")
          :kind (or (reason-kind item) "post")
          :text (nlk:clip text +text-cap+)
          :truncated (> (length text) +text-cap+)
          :created (json-text record "createdAt")
          :replied (and record (nlk:json-value record :object "reply") t)
          :author-did (json-text author "did")
          :author-handle (json-text author "handle")
          :author-name (json-text author "displayName")
          :embed-type (if (and (zerop (length text)) (hash-table-p embed))
                          (json-text embed "$type")
                          "")
          :labels (labels-of post))))

(defun feed-page (actor &key cursor)
  "One page of ACTOR's feed: (VALUES ROWS NEXT-CURSOR). Replies are left
out by the request itself, so a thread continuation is never a row."
  (let* ((data (get-json +feed-path+
                          (append (list (cons "actor" actor)
                                        (cons "limit" (princ-to-string +page-size+))
                                        (cons "filter" "posts_no_replies"))
                                  (and cursor (list (cons "cursor" cursor))))))
         (feed (nlk:json-value data :array "feed")))
    (unless (or (vectorp feed) (listp feed))
      (fail "The public AppView answered without a feed array."))
    (let ((cursor (json-text data "cursor")))
      (values (mapcar #'post-of (coerce feed 'list))
              (and (plusp (length cursor)) cursor)))))
