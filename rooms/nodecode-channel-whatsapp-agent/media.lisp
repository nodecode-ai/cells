;;;; media.lisp --- files in and out: Meta's media calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A file has no words, so the platform carries it apart from them: a
;;;; message names its media by id, and an inbound file is reached by
;;;; GET /media/{id} for a download url on Meta's own media host. Outward a
;;;; file is uploaded first (POST /media, multipart) and the message carries
;;;; the id it got -- two calls, where PLAN-FILE owes one, so the upload is
;;;; made here and the send is the plan. The key rides on each call and
;;;; never in a message body.

(in-package #:nodecode-channel-whatsapp-agent)

;; The executor a host holds.
(nlk:access (host channel-host))

(defparameter +media-host+ "lookaside.fbsbx.com"
  "The one host the platform's download urls were seen on.")

(defparameter +media-id-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-"
  "What a media id may be made of: Meta's are UUIDs.")

(defparameter +kind-extensions+
  '(("image" "jpg" "jpeg" "png" "gif" "webp" "heic" "heif")
    ("video" "mp4" "3gp" "mov" "m4v" "webm")
    ("audio" "mp3" "m4a" "aac" "ogg" "oga" "opus" "amr" "wav")
    ("sticker" "webp"))
  "A file's extension and the kind of media message that carries it. Any
other file goes as a document.")

(defparameter +mime-of-extension+
  (list (cons "jpg" "image/jpeg") (cons "jpeg" "image/jpeg")
        (cons "png" "image/png") (cons "gif" "image/gif")
        (cons "webp" "image/webp") (cons "heic" "image/heic")
        (cons "heif" "image/heif") (cons "mp4" "video/mp4")
        (cons "mov" "video/quicktime") (cons "m4v" "video/x-m4v")
        (cons "3gp" "video/3gpp") (cons "webm" "video/webm")
        (cons "mp3" "audio/mpeg") (cons "m4a" "audio/mp4")
        (cons "aac" "audio/aac") (cons "ogg" "audio/ogg")
        (cons "oga" "audio/ogg") (cons "opus" "audio/opus")
        (cons "amr" "audio/amr") (cons "wav" "audio/wav")
        (cons "pdf" "application/pdf") (cons "txt" "text/plain")
        (cons "md" "text/markdown") (cons "csv" "text/csv")
        (cons "json" "application/json") (cons "zip" "application/zip"))
  "What a file is uploaded as, by its extension; a file of no known type
goes as bytes.")

(defun extension-of (pathname)
  "PATHNAME's extension in lower case, or the empty string."
  (let* ((name (if (typep pathname 'pathname) (file-namestring pathname) pathname))
         (dot (and name (position #\. name :from-end t))))
    (string-downcase (if dot (subseq name (1+ dot)) ""))))

(defun media-kind (pathname)
  "The kind of media message that carries PATHNAME: image, video, audio or
document, by its extension."
  (let ((extension (extension-of pathname)))
    (or (loop for (kind . extensions) in +kind-extensions+
              when (member extension extensions :test #'string=) return kind)
        "document")))

(defun media-mime (pathname)
  "The type PATHNAME is uploaded as."
  (or (cdr (assoc (extension-of pathname) +mime-of-extension+ :test #'string=))
      "application/octet-stream"))

(defun media-id-p (id)
  "Whether ID is safe to put in a path: the platform's own alphabet, and no
dot-dot."
  (and (stringp id) (plusp (length id)) (<= (length id) 128)
       (every (lambda (character) (find character +media-id-chars+)) id)
       (not (search ".." id))))

(defun media-authorization (key)
  "The one header a media call carries."
  (list (cons "authorization" (format nil "bearer ~a" key))))

(defun host-of-base (base)
  "The host BASE names, for the allowlist when a proxy serves the API."
  (let* ((stripped (cond ((uiop:string-prefix-p "https://" base) (subseq base 8))
                         ((uiop:string-prefix-p "http://" base) (subseq base 7))
                         (t base)))
         (slash (or (position #\/ stripped) (length stripped))))
    (subseq stripped 0 slash)))

(defun trusted-media-url (url base)
  "URL, when it is one worth fetching: https, port 443, no credentials, and a
host the platform's media lives on -- Meta's, or the one the API root names.
Otherwise an error naming what was wrong, never the url itself."
  (flet ((refuse (why) (error "the media url ~a" why)))
    (when (or (not (stringp url)) (zerop (length url)) (> (length url) 4096))
      (refuse "is not a url"))
    (unless (uiop:string-prefix-p "https://" url)
      (refuse "is not an https url"))
    (let* ((rest (subseq url 8))
           (slash (or (position #\/ rest) (length rest)))
           (host (subseq rest 0 slash)))
      (when (or (position #\: host) (position #\@ host))
        (refuse "names a port or credentials"))
      (unless (member host (list +media-host+ (host-of-base base)) :test #'string-equal)
        (refuse "is on no host the platform serves media from")))
    url))

(defun media-info (media-id key &key (base +api-base+))
  "GET /media/{id}: what the platform says the file is -- its download url,
type, size and sha256 -- or an error saying the call did not answer."
  (unless (media-id-p media-id)
    (error "the platform named a media id nothing of the alphabet could be"))
  (multiple-value-bind (body status)
      (nlk:http :get (format nil "~a/media/~a" (string-right-trim "/" base) media-id)
                :headers (media-authorization key) :timeout 30)
    (unless (eql 200 status)
      (error "the media lookup answered ~a" status))
    (let ((info (and (stringp body) (ignore-errors (nlk:decode-json body)))))
      (unless (and (hash-table-p info) (nlk:json-value info :string "url"))
        (error "the media lookup answered no download url"))
      info)))

(defun media-download (media-id key &key (base +api-base+) (max-bytes 100000000))
  "MEDIA-ID's bytes, fetched: the lookup, then the file itself from the
platform's media host."
  (let* ((info (media-info media-id key :base base))
         (url (trusted-media-url (nlk:json-value info :string "url") base))
         (declared (nlk:json-value info :integer "file_size")))
    (when (and (integerp declared) (> declared max-bytes))
      (error "the file is ~:d bytes, over the ceiling" declared))
    (multiple-value-bind (octets status)
        (nlk:http :get url :binary t :headers (media-authorization key) :timeout 120)
      (unless (eql 200 status)
        (error "the media download answered ~a" status))
      (unless (typep octets '(vector (unsigned-byte 8)))
        (error "the media download carried no bytes"))
      octets)))

(defun media-fetch-thunk (media-id key &key (base +api-base+))
  "A thunk that returns MEDIA-ID's bytes when the kit reads the attachment:
built on the poll thread, called on the delivery pool."
  (let ((key key) (base base))
    (lambda () (media-download media-id key :base base))))

(defun attach-media-fetchers (adapter candidate)
  "CANDIDATE's media each gain the thunk that fetches its bytes, when the
section carries files (media_enabled, on by default)."
  (when (and (wa-adapter-media-enabled adapter) candidate)
    (loop for attachment across (nlk:json-array candidate "attachments")
          for media-id = (and (hash-table-p attachment) (gethash "download" attachment))
          when (stringp media-id)
            do (setf (gethash "fetch" attachment)
                     (media-fetch-thunk media-id (wa-adapter-key adapter)
                                         :base (wa-adapter-base adapter)))))
  candidate)

(defun upload-plan (pathname &key (mime "application/octet-stream") (timeout 120))
  "POST /media: PATHNAME as form-data, answered with the id the message
carries."
  (rest-plan "POST" "media" "media upload" t timeout
             (list (cons "messaging_product" "whatsapp")
                   (cons "type" mime)
                   (cons "file" pathname))))

(defun media-part (media-id kind filename caption)
  "The message's media object: the id, and only the words it may carry -- a
document's filename, or a caption."
  (let ((part (nlk:json-object "id" media-id)))
    (when (and caption (plusp (length caption)))
      (setf (gethash "caption" part) caption))
    (when (and filename (string= kind "document"))
      (setf (gethash "filename" part) filename))
    part))

(defun send-media-plan (to media-id kind &key reply-to filename caption (timeout 60))
  "One media message to TO, carrying the uploaded MEDIA-ID of KIND."
  ;; The media part's key is the kind itself -- "image", "document" -- the
  ;; one wire field whose name is decided at run time.
  (let ((message (nlk:make-json-object
                  "messaging_product" "whatsapp"
                  "recipient_type" "individual"
                  "to" to
                  "type" kind
                  kind (media-part media-id kind filename caption))))
    (when reply-to
      (setf (gethash "context" message) (nlk:json-object "message_id" reply-to)))
    (rest-plan "POST" "messages" "send media" t timeout message)))

(defun execute-plan-or-error (executor plan &key (what "the upload"))
  "PLAN's answer, or an error saying WHAT the platform refused."
  (let ((result (execute-plan executor plan)))
    (unless (nck:execution-ok-p result)
      (error "~a was refused: ~a" what (or (nck:execution-error result) "no reason given")))
    result))

(defun file-plan (target pathname &key host executor reply-to caption
                                 (media-id nil) (mime nil) (filename nil)
                                 &allow-other-keys)
  "PATHNAME out: the upload, then the one message that carries it to TARGET.
MEDIA-ID, when given, is a file already uploaded; no upload is made then."
  (let ((executor (or executor (and host (host-executor host)))))
    (unless executor
      (error "no executor to upload the file with"))
    (let* ((kind (media-kind pathname))
           (name (or filename
                     (if (typep pathname 'pathname) (file-namestring pathname) pathname)))
           (id (or media-id
                    (nlk:json-value
                     (nck:execution-body
                      (execute-plan-or-error executor
                                             (upload-plan pathname
                                                          :mime (or mime (media-mime pathname)))
                                             :what "the file"))
                     :string "id"))))
      (when (or (not (stringp id)) (zerop (length id)))
        (error "the upload answered no media id"))
      (send-media-plan (source-field target "channel-id") id kind
                       :reply-to reply-to :filename name :caption caption))))
