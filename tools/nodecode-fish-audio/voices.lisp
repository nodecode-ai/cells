;;;; voices.lisp --- the voice library: search, this account's own, one voice,
;;;; and a clone from local samples.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; GET {api_base_url}/model lists and finds voices (self=true lists this
;;;; account's), GET /model/{id} shows one, POST /model as a multipart upload
;;;; clones one from samples. Reading is free; a clone is billed, so the
;;;; speaker's consent is required and a sample is checked before it is sent.

(in-package #:nodecode-fish-audio)

(defun voice-id-p (value)
  "A Fish Audio voice id: 8 to 128 characters of letters, digits, - and _."
  (and (stringp value) (<= 8 (length value) 128)
       (every (lambda (char) (or (alphanumericp char) (find char "-_" :test #'char=))) value)
       t))

(defun checked-voice-id (value)
  (unless (voice-id-p value)
    (fail "voice_id is ~s; a Fish Audio voice id is 8 to 128 letters, digits, - and _ (search first, then pass the id)"
          (or value "empty")))
  value)

(defun row-languages (item)
  (let ((languages (nlk:json-array item "languages")))
    (and (plusp (length languages))
         (format nil " [~{~a~^,~}]" (coerce languages 'list)))))

(defun voice-row (item)
  "One voice as a line: its id (what a call takes), its title, languages,
likes, uses and author."
  (format nil "~a  ~a~a  ~d likes  ~d uses~@[  by ~a~]"
          (or (nlk:json-value item :string "_id")
              (nlk:json-value item :string "id")
              "?")
          (or (nlk:json-value item :string "title") "(untitled)")
          (or (row-languages item) "")
          (or (nlk:json-value item :integer "like_count") 0)
          (or (nlk:json-value item :integer "task_count") 0)
          (nlk:json-value item :string "author" "nickname")))

(defun list-voices (request mine)
  "fish_voices action search (the public library) or mine (this account's
voices), one page at a time."
  (let* ((page (or (fish-voices-page request) 1))
         (size (or (fish-voices-page-size request) 20))
         (sort (or (fish-voices-order request) "score"))
         (query (trimmed-text (fish-voices-query request)))
         (language (trimmed-text (fish-voices-language request))))
    (unless (and (integerp page) (plusp page))
      (fail "page must be a positive integer"))
    (unless (and (integerp size) (<= 1 size 20))
      (fail "page_size is ~a; Fish Audio takes 1 to 20" size))
    (unless (member sort '("score" "task_count" "created_at") :test #'string=)
      (fail "sort is ~s; use score, task_count or created_at" sort))
    (let* ((key (api-key))
           (params (append (list (cons "page_number" page) (cons "page_size" size)
                                 (cons "sort_by" sort))
                           (when mine (list (cons "self" "true")))
                           (when query (list (cons "title" query)))
                           (when language (list (cons "language" language))))))
      (multiple-value-bind (payload status body)
          (json-answer :get "/model" :key key :params params)
        (ensure-ok status body)
        (let* ((items (coerce (nlk:json-array payload "items") 'list))
               (total (nlk:json-value payload :integer "total"))
               (more (nlk:json-value payload :boolean "has_more")))
          (with-output-to-string (out)
            (format out "~a~@[: \"~a\"~]: ~d shown~@[, ~d total~]~@[, page ~d~]~%"
                    (if mine "This account's voices" "Voices") query (length items) total
                    (unless (= page 1) page))
            (if items
                (dolist (item items) (format out "  ~a~%" (voice-row item)))
                (format out "  none~%"))
            (when more
              (format out "more: ask for page ~d~%" (1+ page)))))))))

(defun get-voice (request)
  "fish_voices action get: one voice in full, its id first."
  (let* ((key (api-key))
         (id (checked-voice-id (fish-voices-voice-id request))))
    (multiple-value-bind (payload status body)
        (json-answer :get (format nil "/model/~a" id) :key key)
      (ensure-ok status body)
      (let ((samples (nlk:json-array payload "samples")))
        (with-output-to-string (out)
          (format out "~a~%" (voice-row payload))
          (let ((description (nlk:json-value payload :string "description")))
            (when (and description (plusp (length description)))
              (format out "~a~%" (subseq description 0 (min 400 (length description))))))
          (let ((tags (nlk:json-array payload "tags")))
            (when (plusp (length tags))
              (format out "tags: ~{~a~^, ~}~%" (coerce tags 'list))))
          (format out "visibility: ~a~@[, state ~a~]~%"
                  (or (nlk:json-value payload :string "visibility") "?")
                  (nlk:json-value payload :string "state"))
          (when (plusp (length samples))
            (format out "sample texts:~%")
            (loop for sample across samples
                  for i from 1 to 5
                  do (format out "  ~d. ~a~%" i (or (nlk:json-value sample :string "text") "(no text)"))))
          (format out "speak with it: fish_speak with voice ~a~%" id))))))

(defun clone-voice (request)
  "fish_voices action clone: local samples become a voice on this account.
Fish Audio bills the upload, so consent and every sample are checked first."
  (unless (fish-voices-consent request)
    (fail "cloning needs the speaker's permission: ask the user to confirm it, then call again with consent true"))
  (let* ((title (trimmed-text (fish-voices-title request)))
         (paths (fish-voices-sample-paths request))
         (visibility (or (fish-voices-visibility request) "private"))
         (description (trimmed-text (fish-voices-description request))))
    (unless (and title (plusp (length title)))
      (fail "title is required: the voice is saved under it"))
    (unless (and paths (<= 1 (length paths) 20))
      (fail "sample_paths takes 1 to 20 local audio files"))
    (unless (member visibility '("private" "unlist") :test #'string=)
      (fail "visibility is ~s; clone as private or unlist, and publish from the Fish Audio website" visibility))
    (let ((samples (loop for name across (coerce paths 'vector)
                         collect (checked-input name :max-bytes +max-sample-bytes+)))
          (key (api-key)))
      (multiple-value-bind (payload status body)
          (json-answer :post "/model" :key key :timeout 600
                                     :content (append (list (cons "title" title)
                                                            (cons "type" "tts")
                                                            (cons "train_mode" "fast")
                                                            (cons "visibility" visibility))
                                                      (when description (list (cons "description" description)))
                                                      (loop for sample in samples
                                                            collect (cons "voices" sample))))
        (ensure-ok status body)
        (let ((id (or (nlk:json-value payload :string "_id")
                      (nlk:json-value payload :string "id"))))
          (format nil "cloned voice ~a \"~a\" (~a)~@[, state ~a~]: speak with it by passing voice ~a~%~
                       Copy the samples back to the speaker: the voice reads only what they own."
                  id (or (nlk:json-value payload :string "title") title) visibility
                  (nlk:json-value payload :string "state") id))))))

(defun voices-now (request)
  (let ((action (fish-voices-action request)))
    (cond ((string= action "search") (list-voices request nil))
          ((string= action "mine") (list-voices request t))
          ((string= action "get") (get-voice request))
          ((string= action "clone") (clone-voice request))
          (t (fail "unknown action ~s; use search, mine, get or clone" action)))))

(defun voices (request)
  "The fish_voices tool, answering its refusals as text."
  (handler-case (voices-now request)
    (fish-audio-error (condition)
      (nle:failure (format nil "ERROR: ~a" (fish-audio-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))
