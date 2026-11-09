;;;; tools.lisp --- the operations, the verbs that call them, and the three model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original declared fifteen MCP tools and fifteen Skills: a Skill per
;;;; operation, each saying what it takes and what to keep. Here the operations
;;;; are one table, the arguments are the names its query.json examples used,
;;;; and three tools stand in front of them: FLASHDATA_CALL for any operation
;;;; by name, FLASHDATA_SEARCH for the six Google verticals, and
;;;; FLASHDATA_YOUTUBE for YouTube's. Every one of them is also a verb the
;;;; operator and the model can call through eval, under the same names.

(in-package #:nodecode-flashdata)

;;; --- what the hosted server answers -------------------------------------------------

(defparameter +operations+
  '("google_search" "google_news" "google_images" "google_shopping" "google_videos"
    "google_places" "youtube_search" "youtube_search_extended" "youtube_metadata"
    "youtube_transcript" "youtube_captions" "youtube_trainability" "youtube_channel"
    "youtube_suggestions" "youtube_download" "get_pricing" "get_job" "get_result")
  "Every operation the hosted server names, as its own schema spells them.")

(defparameter +search-sources+
  '(("web" . "google_search") ("news" . "google_news") ("images" . "google_images")
    ("videos" . "google_videos") ("shopping" . "google_shopping")
    ("places" . "google_places"))
  "The six Google verticals one tool reaches, and the operation each is.")

(defparameter +market-sources+ '("google_search" "google_news" "google_shopping")
  "The operations that take a country and a language: the others answer in the query's own.")

(defparameter +youtube-operations+
  '(("search" . "youtube_search") ("extended_search" . "youtube_search_extended")
    ("metadata" . "youtube_metadata") ("transcript" . "youtube_transcript")
    ("captions" . "youtube_captions") ("trainability" . "youtube_trainability")
    ("channel" . "youtube_channel") ("suggestions" . "youtube_suggestions"))
  "The YouTube operations one tool reaches, and the wire name each is.")

;;; --- arguments ----------------------------------------------------------------------

(defun bolt (&rest pairs)
  "A call's arguments from PAIRS (NAME VALUE ...), leaving out every NIL and empty string."
  (let ((object (nlk:make-json-object)))
    (loop for (name value) on pairs by #'cddr
          when (and value (not (and (stringp value) (zerop (length (nlk:trimmed value))))))
            do (setf (gethash name object) value))
    object))

(defun arguments-object (source)
  "SOURCE -- a hash table, or an alist of names -- as the arguments object a call carries."
  (cond ((hash-table-p source) source)
        ((consp source)
         (let ((object (nlk:make-json-object)))
           (loop for (key . value) in source
                 do (setf (gethash (string-downcase (string key)) object) value))
           object))
        (t (nlk:make-json-object))))

(defun required-text (value name)
  "VALUE as non-empty text, or a refusal naming NAME."
  (or (trimmed-text value) (fail "~a is required" name)))

(defun bounded (value default ceiling)
  "VALUE as a count from 1 to CEILING, or DEFAULT when the call named none."
  (max 1 (min (or value default) ceiling)))

(defun run (operation arguments &key full)
  "One operation on the hosted server, as the model reads it. FULL keeps the whole answer."
  (render-result operation (tools-call operation arguments)
                 (and (not full) (characters-limit))))

;;; --- the verbs: one per operation, called through eval ------------------------------

(define-verb search (query &key (num 10) gl hl full)
  "Search Google's web index. => title, url and snippet per result, numbered.
NUM is 1 to 20 (10), GL a two-letter country (us), HL a language (en)."
  (run "google_search" (bolt "query" (required-text query "query")
                             "num" (bounded num 10 20) "gl" gl "hl" hl)
       :full full))

(define-verb news (query &key (num 10) gl hl full)
  "Search Google News for recent reporting. => publication, date, title, url, snippet."
  (run "google_news" (bolt "query" (required-text query "query")
                           "num" (bounded num 10 20) "gl" gl "hl" hl)
       :full full))

(define-verb images (query &key (num 20) full)
  "Search Google Images. => the image's title, its page, its thumbnail, its size."
  (run "google_images" (bolt "query" (required-text query "query") "num" (bounded num 20 100))
       :full full))

(define-verb shopping (query &key (num 10) gl hl full)
  "Search Google Shopping. => products, merchants, prices: a snapshot, not a stock level."
  (run "google_shopping" (bolt "query" (required-text query "query")
                               "num" (bounded num 10 20) "gl" gl "hl" hl)
       :full full))

(define-verb places (query &key (num 20) full)
  "Search Google Places for businesses and places. => name, address, rating, contact details."
  (run "google_places" (bolt "query" (required-text query "query") "num" (bounded num 20 100))
       :full full))

(define-verb videos (query &key (num 10) full)
  "Search Google's video results. => the video's title, its page, its source, its length."
  (run "google_videos" (bolt "query" (required-text query "query") "num" (bounded num 10 100))
       :full full))

(define-verb youtube-search (query &key (max-results 10) full)
  "Search YouTube. => title, url, channel, duration, views: at most 20 results."
  (run "youtube_search" (bolt "query" (required-text query "query")
                               "max_results" (bounded max-results 10 20))
       :full full))

(define-verb youtube-extended-search (query &key (max-results 100) full)
  "Search YouTube further: up to 700 results, at a different rate, no count guaranteed."
  (run "youtube_search_extended" (bolt "query" (required-text query "query")
                                        "max_results" (bounded max-results 100 700))
       :full full))

(define-verb metadata (video-id &key formats chapters full)
  "One video's record: title, channel, duration, views, formats and chapters on request."
  (run "youtube_metadata" (bolt "video_id" (required-text video-id "video_id")
                                 "include_formats" (and formats t)
                                 "include_chapters" (and chapters t))
       :full full))

(define-verb transcript (video-id &key language (auto t) full)
  "A video's existing transcript. AUTO keeps YouTube's generated track in play; no ASR is run."
  (run "youtube_transcript" (bolt "video_id" (required-text video-id "video_id")
                                   "language" language "auto" (and auto t))
       :full full))

(define-verb captions (video-id &key language (auto nil) full)
  "A video's uploaded captions. Uncued: the original defaults AUTO to false here."
  (run "youtube_captions" (bolt "video_id" (required-text video-id "video_id")
                                 "language" language "auto" (and auto t))
       :full full))

(define-verb trainability (video-id &key full)
  "A video's Creative Commons label: whether YouTube marks it reusable. Not a grant of rights."
  (run "youtube_trainability" (bolt "video_id" (required-text video-id "video_id")) :full full))

(define-verb channel (handle &key (video-limit 10) full)
  "One channel by handle (@name) or id: its record and its newest videos."
  (run "youtube_channel" (bolt "handle" (required-text handle "handle")
                                "video_limit" (bounded video-limit 10 100))
       :full full))

(define-verb suggestions (query &key language region full)
  "What YouTube's own search box suggests for QUERY, which is what people type next."
  (run "youtube_suggestions" (bolt "query" (required-text query "query")
                                    "language" language "region" region)
       :full full))

(define-verb pricing (source &key (count 1) (full t))
  "The current rate for one source and request count: a free read, not permission to spend."
  (run "get_pricing" (bolt "source" (required-text source "source")
                            "request_count" (bounded count 1 1000))
       :full t))

(define-verb job (job-id &key full)
  "An asynchronous submission's state: pending and processing are not completion."
  (run "get_job" (bolt "job_id" (required-text job-id "job_id")) :full full))

(define-verb result (result-id &key offset (full t))
  "One chunk of a long answer; concatenate chunks before reading them as JSON."
  (run "get_result" (bolt "result_id" (required-text result-id "result_id")
                           "next_offset" offset)
       :full t))

(define-verb call (operation arguments &key full)
  "Any operation by its wire name, with the flat arguments it takes.
ARGUMENTS is an alist of (\"query\" . \"battery recycling\") or a JSON object."
  (unless (member operation +operations+ :test #'string=)
    (fail "~a is not an operation; one of ~{~a~^, ~}" operation +operations+))
  (run operation (arguments-object arguments) :full full))

;;; --- the model's tools -------------------------------------------------------------

(nlk:define-contract flashdata-call
  (:doc "Call one FlashData operation on the operator's own account: Google web, news, image,
shopping, video and places search, YouTube search, metadata, transcript, captions, channel,
suggestions and trainability, and the job, result and pricing reads. Every data query spends
the account's FlashData Credits, so make one call per question you actually have.")
  ("operation" :text :required t :options +operations+
   :doc "the operation, as its wire name: google_search, google_news, google_images,
 google_shopping, google_videos, google_places, youtube_search, youtube_search_extended,
 youtube_metadata, youtube_transcript, youtube_captions, youtube_trainability,
 youtube_channel, youtube_suggestions, youtube_download, get_pricing, get_job, get_result")
  ("arguments" :object
   :doc "the operation's own flat arguments, as its schema names them: google_search takes
 query, num, gl, hl; youtube_search query, max_results; youtube_transcript video_id, language,
 auto; youtube_channel handle, video_limit; get_pricing source, request_count; get_job
 job_id; get_result result_id, next_offset")
  ("full" :boolean :doc "keep the whole answer; otherwise it is cut to the section's max_chars"))

(nlk:define-contract flashdata-search
  (:doc "Search Google through FlashData: the web, news, images, videos, shopping or places.
One call answers one query with ranked results, their source links and their snippets. It
spends the operator's FlashData Credits.")
  ("query" :text :required t :doc "the search query, in the words the user used")
  ("source" :text :default "web" :options '("web" "news" "images" "videos" "shopping" "places")
   :doc "which vertical: web (default), news, images, videos, shopping or places")
  ("num" :integer :doc "how many results to keep, 1 to 20; 10 by default")
  ("gl" :string :doc "the two-letter country to search from, such as us or gb (web, news, shopping)")
  ("hl" :string :doc "the language code, such as en or de (web, news, shopping)")
  ("full" :boolean :doc "keep the whole answer; otherwise it is cut to the section's max_chars"))

(nlk:define-contract flashdata-youtube
  (:doc "Research YouTube through FlashData: search, a video's metadata, its transcript or its
captions, a channel's record and newest videos, the search box's suggestions, and a video's
Creative Commons label. It spends the operator's FlashData Credits.")
  ("operation" :text :default "search"
   :options '("search" "extended_search" "metadata" "transcript" "captions" "channel"
              "suggestions" "trainability")
   :doc "search (default) | extended_search | metadata | transcript | captions | channel |
 suggestions | trainability")
  ("query" :string :doc "the words, for search, extended_search and suggestions")
  ("video_id" :string :doc "the video's id, for metadata, transcript, captions and trainability")
  ("handle" :string :doc "the channel's @handle or id, for channel")
  ("max_results" :integer :doc "how many results: 10 by default, at most 20 for search, 700 extended")
  ("language" :string :doc "a language code, such as en; the track to read when several exist")
  ("region" :string :doc "a two-letter region for suggestions, such as US")
  ("auto" :boolean :doc "read YouTube's generated track as well as an uploaded one (transcript and
 captions); left out, the server's own default stands")
  ("full" :boolean :doc "keep the whole answer; otherwise it is cut to the section's max_chars"))

(defun tool-flashdata-call (call)
  "flashdata_call: any operation by name, with the flat arguments it takes."
  (let ((operation (flashdata-call-operation call)))
    (unless (member operation +operations+ :test #'string=)
      (fail "operation must be one of ~{~a~^, ~}" +operations+))
    (run operation (arguments-object (flashdata-call-arguments call))
         :full (flashdata-call-full call))))

(defun tool-flashdata-search (call)
  "flashdata_search: one query against one of the six Google verticals."
  (let* ((source (or (trimmed-text (flashdata-search-source call)) "web"))
         (operation (or (cdr (assoc source +search-sources+ :test #'string=))
                        (fail "source must be one of web, news, images, videos, shopping, places")))
         (market (member operation +market-sources+ :test #'string=)))
    (run operation
         (bolt "query" (required-text (flashdata-search-query call) "query")
               "num" (bounded (flashdata-search-num call) 10 (if market 20 100))
               "gl" (and market (trimmed-text (flashdata-search-gl call)))
               "hl" (and market (trimmed-text (flashdata-search-hl call))))
         :full (flashdata-search-full call))))

(defun tool-flashdata-youtube (call)
  "flashdata_youtube: the eight YouTube operations behind one tool."
  (let* ((operation (or (trimmed-text (flashdata-youtube-operation call)) "search"))
         (wire (cdr (assoc operation +youtube-operations+ :test #'string=)))
         (query (trimmed-text (flashdata-youtube-query call)))
         (video (trimmed-text (flashdata-youtube-video-id call)))
         (handle (trimmed-text (flashdata-youtube-handle call)))
         (language (trimmed-text (flashdata-youtube-language call)))
         (region (trimmed-text (flashdata-youtube-region call)))
         (max (flashdata-youtube-max-results call)))
    (unless wire
      (fail "operation must be one of ~{~a~^, ~}" (mapcar #'car +youtube-operations+)))
    (flet ((needs (value name)
             (or value (fail "~a is required for ~a" name operation))))
      (run wire
           (cond ((string= operation "search")
                  (bolt "query" (needs query "query") "max_results" (bounded max 10 20)))
                 ((string= operation "extended_search")
                  (bolt "query" (needs query "query") "max_results" (bounded max 100 700)))
                 ((string= operation "suggestions")
                  (bolt "query" (needs query "query") "language" language "region" region))
                 ((string= operation "channel")
                  (bolt "handle" (needs handle "handle") "video_limit" (bounded max 10 100)))
                 ((string= operation "metadata")
                  (bolt "video_id" (needs video "video_id")))
                 ((string= operation "trainability")
                  (bolt "video_id" (needs video "video_id")))
                 ((string= operation "transcript")
                  (bolt "video_id" (needs video "video_id") "language" language
                        "auto" (and (flashdata-youtube-auto call) t)))
                 ((string= operation "captions")
                  (bolt "video_id" (needs video "video_id") "language" language
                        "auto" (and (flashdata-youtube-auto call) t)))
                 (t (fail "~a is not a YouTube operation" operation)))
           :full (flashdata-youtube-full call)))))
