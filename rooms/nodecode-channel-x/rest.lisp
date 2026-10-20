;;;; rest.lisp --- the conversation book, the plans, the executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A plan is a bridge op: method BRIDGE, the op as its path, its arguments
;;;; as its body (bridge.lisp runs it; tests put a scripted executor there).
;;;;
;;;; Posts. An answer is a reply under the ask, and an answer longer than one
;;;; post goes on as a reply to its own previous post, numbered, up to the
;;;; section's max_posts. A post that would reply to nothing is refused: this
;;;; cell never tweets on its own. X cannot edit a post, so the turn's card
;;;; is never posted and a settled card changes nothing. Every post counts
;;;; against daily_posts, which refuses the rest of the UTC day.

(in-package #:nodecode-channel-x)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution))

;;; --- the conversation book -----------------------------------------------------------

(defparameter +x-book-cap+ 20000
  "Entries any one table holds before it starts over.")

(defstruct (x-book (:copier nil))
  ;; tweet id -> T for a post the bot made.
  (mine (make-hash-table :test #'equal :synchronized t))
  ;; ask id -> the bot's latest post answering it: where the next part goes.
  (chain (make-hash-table :test #'equal :synchronized t))
  ;; Posts made today, and the UTC day (yyyy-mm-dd) they count for.
  (day "" :type string)
  (posts 0 :type integer)
  (daily-posts 50 :type integer)
  (max-posts 4 :type integer)
  (lock (sb-thread:make-mutex :name "x book")))

(nlk:access (book x-book))

(defun book-note (table key value)
  (when (>= (hash-table-count table) +x-book-cap+)
    (clrhash table))
  (setf (gethash key table) value))

(defun bot-tweet-p (book id)
  "Whether ID is one of the bot's posts."
  (and id (gethash id book.mine) t))

(defun utc-day (&optional (time (get-universal-time)))
  (multiple-value-bind (s mi h d mo y) (decode-universal-time time 0)
    (declare (ignore s mi h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d)))

(defun take-post (book)
  "Count one post against today's allowance: T, or NIL once it is spent."
  (sb-thread:with-mutex (book.lock)
    (let ((today (utc-day)))
      (unless (string= today book.day)
        (setf book.day today book.posts 0))
      (when (< book.posts book.daily-posts)
        (incf book.posts)
        t))))

(defun give-post-back (book)
  "A post that was refused does not count."
  (sb-thread:with-mutex (book.lock)
    (when (plusp book.posts) (decf book.posts))))

;;; --- plans -------------------------------------------------------------------------

(sb-ext:defglobal **card-counter** (list 0)
  "How many card ids this process has handed out.")

(defun bridge-plan (op label timeout-seconds &rest pairs)
  "The plan of bridge OP with PAIRS as its arguments."
  (rest-plan "BRIDGE" op label nil timeout-seconds
             (apply #'nlk:make-json-object "op" op pairs)))

(defun noop-plan (label timeout-seconds &optional body)
  "A plan that reaches nothing: what a card's post, edit and typing are."
  (rest-plan "NOOP" "none" label nil timeout-seconds
             (list* :id (format nil "card-~36r" (sb-ext:atomic-incf (car **card-counter**))) body)))

(defun card-id-p (id)
  "Whether ID names a card, which was never posted."
  (and (stringp id) (uiop:string-prefix-p "card-" id)))

(defun fail-plan (label timeout-seconds why)
  (rest-plan "FAIL" "none" label nil timeout-seconds (list :error why)))

(defparameter +image-types+ '("png" "jpg" "jpeg" "gif" "webp")
  "The file types X shows as a picture.")

(defun post-image (files)
  "The first of FILES X takes as a picture, as a native path, or NIL."
  (loop for file in files
        when (member (string-downcase (or (pathname-type (pathname file)) "")) +image-types+
                     :test #'string=)
          return (uiop:native-namestring file)))

(defun part-text (words index shown cut-p)
  "One part's text: WORDS, numbered INDEX/SHOWN when there is more than one
part, and marked when the parts past it were cut."
  (cond ((= shown 1) words)
        (cut-p (format nil "~a ~d/~d …" words index shown))
        (t (format nil "~a ~d/~d" words index shown))))

(defun message-plan (book target chunk &key reply-to card files (timeout-seconds 60)
                     &allow-other-keys)
  "One post of CHUNK (a kit text-chunk) as a reply under REPLY-TO, the
first part replying to the ask and each later one to the part before it; a
part past the book's max_posts is dropped and the last one kept marked as
cut. A turn's CARD is never posted; a post that answers nothing is refused."
  (declare (ignore target))
  (let ((index (text-chunk-index chunk))
        (total (text-chunk-total chunk))
        (most book.max-posts))
    (cond (card (noop-plan "x_card" timeout-seconds))
          ((null reply-to)
           (fail-plan "send_post" timeout-seconds
                      "the X channel posts only replies under an ask; it never tweets on its own"))
          ((> index most) (noop-plan "x_post_dropped" timeout-seconds))
          (t
           (let ((shown (min total most))
                 (image (and (= index 1) (post-image files))))
             (apply #'bridge-plan "reply"
                    (if (> total 1) (format nil "send_post_~a_of_~a" index (min total most)) "send_post")
                    timeout-seconds
                    "text" (part-text (markdown-plain (text-chunk-text chunk)) index shown
                                      (and (= index most) (> total most)))
                    "reply_to" reply-to
                    (append (and (> index 1) (list "__chain" t))
                            (and image (list "image" image)))))))))

(defun edit-plan (book target message-id text &key (timeout-seconds 60) &allow-other-keys)
  "Nothing: X cannot edit a post, and the card was never posted."
  (declare (ignore book target text))
  (noop-plan "x_edit" timeout-seconds (list :id message-id)))

(defun delete-plan (book target message-id &key (timeout-seconds 60) &allow-other-keys)
  "A card's retirement, which reaches nothing; a post of the bot's deleted."
  (declare (ignore target))
  (cond ((card-id-p message-id) (noop-plan "x_card" timeout-seconds))
        ((bot-tweet-p book message-id)
         (bridge-plan "delete" "delete_post" timeout-seconds "id" message-id))
        (t (fail-plan "delete_post" timeout-seconds "the X channel deletes only its own posts"))))

(defun typing-plan (book target &key (timeout-seconds 30) &allow-other-keys)
  "Nothing: X shows no one typing."
  (declare (ignore book target))
  (noop-plan "x_typing" timeout-seconds))

(defun file-plan (book target pathname &key content reply-to (timeout-seconds 120)
                  &allow-other-keys)
  "PATHNAME posted as a picture in a reply under REPLY-TO, CONTENT its words."
  (declare (ignore book target))
  (let ((image (post-image (list pathname))))
    (cond ((null reply-to)
           (fail-plan "send_file" timeout-seconds
                      "the X channel posts only replies under an ask; it never tweets on its own"))
          ((null image)
           (fail-plan "send_file" timeout-seconds "X posts pictures only: png, jpg, gif or webp"))
          (t (bridge-plan "reply" "send_file" timeout-seconds
                          "text" (markdown-plain (or content "")) "reply_to" reply-to "image" image)))))

(defun mentions-plan (since &key (limit 40))
  (bridge-plan "mentions" "read_mentions" 60 "since" (princ-to-string (or since 0)) "limit" limit))

(defun x-message-id (body)
  "The id of the post a send made, or the id a card stands under."
  (if (consp body)
      (getf body :id)
      (nlk:json-value body :string "id")))

;;; --- the executor ----------------------------------------------------------------

(defun bare-reply (plan book)
  "PLAN, a reply, as the bridge takes it: a later part aimed at the bot's
latest post under the same ask, the private __chain member taken off."
  (let ((body (alexandria:copy-hash-table plan.body)))
    (when (gethash "__chain" body)
      (nlk:when-let (previous (gethash (gethash "reply_to" body) book.chain))
        (setf (gethash "reply_to" body) previous))
      (remhash "__chain" body))
    (rest-plan plan.method plan.path plan.audit-label plan.retry-server-errors
               plan.timeout-seconds body plan.headers)))

(defun note-post (book plan result)
  "What one post leaves in BOOK: its id the bot's, and the next part of its
answer aimed at it."
  (let ((id (and result.ok-p (x-message-id result.body))))
    (when id
      (book-note book.mine id t)
      (book-note book.chain (gethash "reply_to" plan.body) id))
    result))

(defun wrap-x-executor (inner &key book)
  "An executor speaking X in the kit's terms: a NOOP plan done and a FAIL
plan refused without a call, a reply counted against the day's posts and
aimed down its chain, every post noted in BOOK."
  (make-plan-executor
   :run (lambda (plan)
          (cond
            ((null plan) (make-execution :ok-p t :status 200 :attempts 0))
            ((string= "NOOP" plan.method)
             (make-execution :ok-p t :status 200 :attempts 0 :body plan.body))
            ((string= "FAIL" plan.method)
             (make-execution :ok-p nil :status 0 :attempts 0 :error (getf plan.body :error)))
            (t
             (let ((posting (string= "reply" (gethash "op" plan.body))))
               (if (and posting (not (take-post book)))
                   (make-execution :ok-p nil :status 429 :attempts 0
                                   :error (format nil "x send_post refused: the day's ~d posts are spent"
                                                  book.daily-posts))
                   (let ((result (execute-plan inner (if posting (bare-reply plan book) plan))))
                     (when (and posting (not (execution-ok-p result))) (give-post-back book))
                     (if (and book (execution-ok-p result) posting)
                         (note-post book plan result)
                         result)))))))))
