;;;; feed.lisp --- the client header, and the feed read into the listing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; api.cline.bot answers a free model (cline-free/...) only to a request
;;;; whose X-CLIENT-TYPE names a client: without the header it is a 403, "only
;;;; available via Cline product surfaces"; with any value it streams
;;;; (2026-09-22, rev, X-CLIENT-TYPE nodecode: 200). So the header names
;;;; nodecode — the section's client_type — and nothing here claims to be
;;;; Cline. The Pass models (cline-pass/...) and the free ones are absent from
;;;; its /models listing, which is an OpenRouter catalog; Cline's own clients
;;;; read them off the public recommended-models feed, and so does this.
;;;;
;;;; Two advices, and the kernel knows neither: RESOLVE-PROVIDER-CREDENTIAL's
;;;; answer for the served provider carries the header (the request path
;;;; applies a credential's :headers, whatever tier the key came from), and
;;;; LIST-PROVIDER-MODELS's answer for it gains the feed's rows. Every other
;;;; provider passes through untouched.

(in-package #:nodecode-cline)

(defun served-p (provider)
  "True when PROVIDER is the lane this add-on serves."
  (and (stringp provider) (string-equal provider (setting :provider))))

(defun client-headers ()
  "The headers that name this client to Cline's API."
  (list (cons "x-client-type" (setting :client-type))
        (cons "x-client-version" nle::*user-agent-version*)))

(defun identify (next provider &rest keys &aux (credential (apply next provider keys)))
  "RESOLVE-PROVIDER-CREDENTIAL advice: PROVIDER's credential, carrying the
client headers when PROVIDER is the served lane."
  (if (served-p provider)
      (let ((attributes (copy-list (nle:credential-attributes credential))))
        (setf (getf attributes :headers)
              (append (getf attributes :headers) (client-headers)))
        (nle:make-credential (nle:credential-key credential)
                             (nle:credential-source credential)
                             attributes))
      credential))

(defun feed-rows-from (text buckets &aux (seen (make-hash-table :test #'equal))
                                          (rows '()))
  "The listing rows (:id :display) the feed body TEXT carries under BUCKETS,
in bucket order and each id once; NIL for a body that is not the feed."
  ;; A row's display is the feed's name when it says more than the id; the
  ;; free bucket's rows say they are free, which is the one fact the picker
  ;; cannot learn from the id — once: a name's own :free suffix goes (the
  ;; feed names laguna-s-2.1:free so).
  (let ((feed (ignore-errors (nlk:decode-json text))))
    (dolist (bucket buckets (nreverse rows))
      (loop for entry across (or (nlk:json-value feed :array bucket) #())
            for id = (nlk:json-value entry :text "id")
            for name = (let ((name (nlk:json-value entry :text "name")))
                         (when (and name (string= bucket "free"))
                           (setf name (ppcre:regex-replace ":free\\z" name "")))
                         (and name (string/= name id) name))
            when (and id (not (gethash id seen)))
              do (setf (gethash id seen) t)
                 (push (list :id id
                             :display (if (string= bucket "free")
                                          (format nil "~@[~a ~](free)" name)
                                          name))
                       rows)))))

(defun fetch-feed ()
  "The feed's body, or NIL when it cannot be read within one listing's
deadline."
  ;; The feed is public: no key rides it. HTTP-FETCH is the one bounded GET
  ;; whose failures, a deadline's included, are values: the listing worker
  ;; (provider.lisp LIST-PROVIDER-MODELS) never unwinds through here.
  (values (nle:http-fetch (setting :feed) :headers '(("accept" . "application/json"))
                          :timeout nle::*provider-models-fetch-timeout-seconds*)))

(defun list-with-feed (next provider)
  "LIST-PROVIDER-MODELS advice: the served lane's listing gains the feed's
rows ahead of its own; any other lane's passes through."
  ;; => (values ROWS ERROR), NEXT's contract. The listing's error stands only
  ;; when nothing at all was listed: the cache keeps rows beside an error, and
  ;; the picker shows an error only for a lane with no rows.
  (multiple-value-bind (rows error) (funcall next provider)
    (if (served-p provider)
        (let* ((feed (let ((body (fetch-feed)))
                       (and body (feed-rows-from body (coerce (setting :buckets) 'list)))))
               (merged (append feed
                               (remove-if (lambda (row)
                                            (find (getf row :id) feed
                                                  :key (lambda (fed) (getf fed :id))
                                                  :test #'equal))
                                          rows))))
          (values merged (and (null merged) error)))
        (values rows error))))
