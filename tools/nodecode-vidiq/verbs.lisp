;;;; verbs.lisp --- the vidIQ tools as Lisp verbs, and the arguments they take.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Hermes registered vidIQ's tools under their own names, vidiq_channel_videos
;;;; and the rest, with a JSON schema each; here each one is a function in the
;;;; vidiq package, and its schema is its keyword arguments: :channel-id is
;;;; the service's channelId, :max-results its maxResults, and a keyword the
;;;; tool does not know is sent anyway (the service ignores unknown keys).
;;;; CALL-TOOL reaches any tool the account lists, including ones this file
;;;; names not at all, and TOOLS lists them.

(in-package #:nodecode-vidiq)

(defun capitalize-first (text)
  "TEXT with its first character upcased: the tail of a camelCase name."
  (if (plusp (length text))
      (concatenate 'string (string-upcase (subseq text 0 1)) (subseq text 1))
      text))

(defun wire-key (keyword)
  "KEYWORD's name as the service spells it: :channel-id is \"channelId\"."
  (let ((parts (uiop:split-string (string-downcase (symbol-name keyword)) :separator "-")))
    (format nil "~a~{~a~}" (first parts) (mapcar #'capitalize-first (rest parts)))))

(defun wire-value (value)
  "VALUE as the JSON a tool argument holds: lists and vectors are arrays, an
object stays one, a number and a string go as they are, anything else is its
printed name."
  (cond ((hash-table-p value) value)
        ((and (vectorp value) (not (stringp value)))
         (map 'vector #'wire-value value))
        ((consp value) (coerce (mapcar #'wire-value value) 'vector))
        ((or (stringp value) (numberp value)) value)
        ((eq value t) t)
        (t (princ-to-string value))))

(defun arguments-from (pairs)
  "PAIRS, alternating keywords and values, as a JSON object. A NIL value is absent."
  (let ((flat '()))
    (loop for (key value) on pairs by #'cddr
          when value
            do (setf flat (list* (wire-value value) (wire-key key) flat)))
    (apply #'nlk:make-json-object (nreverse flat))))

(defmacro define-tool-verb (name tool lambda-list documentation &rest field-forms)
  "A verb calling TOOL. LAMBDA-LIST is the required fields; FIELD-FORMS, each
a list of the fields it needs, go in front of the verb's &REST ARGUMENTS."
  `(define-verb ,name ,lambda-list ,documentation
     (call-tool ,tool (arguments-from (append ,@field-forms arguments)))))

(define-verb user-channels ()
  "The YouTube channels this vidIQ account authorized, with their ids: where every
channel-specific workflow starts."
  (call-tool "vidiq_user_channels"))

(define-verb balance ()
  "The vidIQ credits left on this account and what the month has used."
  (call-tool "vidiq_balance"))

(define-tool-verb channel-search "vidiq_channel_search" (query &rest arguments)
  "Channels matching QUERY: subscriber bounds, breakout signals and the evidence
that a niche has an opening."
  (list :query query))

(define-tool-verb channel-videos "vidiq_channel_videos" (channel-id &rest arguments)
  "One channel's videos with their numbers: the evidence a channel audit and a
new-upload review start from."
  (list :channel-id channel-id))

(define-tool-verb channel-analytics "vidiq_channel_analytics" (channel-id &rest arguments)
  "Private analytics for an authorized channel over a date range, long-form and
Shorts separable."
  (list :channel-id channel-id))

(define-tool-verb channel-trends "vidiq_channel_performance_trends" (channel-id &rest arguments)
  "How an authorized channel's performance moved across a period, one series a
metric."
  (list :channel-id channel-id))

(define-tool-verb outliers "vidiq_outliers" (&rest arguments)
  "Videos beating their channel's own baseline: what a niche rewards, independent
of subscriber count."
  (list))

(define-tool-verb outlier-search "vidiq_instagram_tiktok_outlier_search" (&rest arguments)
  "Short-form outliers on Instagram and the available TikTok surface."
  (list))

(define-tool-verb trending-videos "vidiq_trending_videos" (&rest arguments)
  "What is trending now in a category or country, with velocity."
  (list))

(define-tool-verb keyword-research "vidiq_keyword_research" (keyword &rest arguments)
  "Search demand for KEYWORD: volume, competition and growth, in its research or
rising mode."
  (list :keyword keyword))

(define-tool-verb video-comments "vidiq_video_comments" (video-id &rest arguments)
  "A video's comments, ordered as asked: the audience's own words."
  (list :video-id video-id))

(define-tool-verb video-transcript "vidiq_video_transcript" (video-id &rest arguments)
  "A video's transcript, for aligning retention changes with what was said."
  (list :video-id video-id))

(define-tool-verb video-stats "vidiq_video_stats" (video-id &rest arguments)
  "One video's public and authorized numbers."
  (list :video-id video-id))

(define-tool-verb similar-channels "vidiq_similar_channels" (channel-id &rest arguments)
  "Channels like this one: the adjacent audience a creator can enter."
  (list :channel-id channel-id))

(define-verb videos-by-ids (ids &rest arguments)
  "Several videos at once, IDS a list of ids: the service wants them as its
filters field, video==ID1,ID2, at most 150 a call."
  (call-tool "vidiq_get_videos_by_ids"
              (arguments-from (list* :filters
                                     (format nil "video==~{~a~^,~}"
                                             (if (listp ids) ids (list ids)))
                                     arguments))))

(define-tool-verb job-poll "vidiq_job_poll" (job-id &rest arguments)
  "Poll one asynchronous job by the ID its submission answered."
  (list :job-id job-id))

(define-tool-verb jobs-list "vidiq_jobs_list" (&rest arguments)
  "The account's asynchronous jobs: how interrupted work is recovered instead of
resubmitted."
  (list))

(define-tool-verb generate-titles "vidiq_generate_titles" (&rest arguments)
  "Titles generated for a concept, a script or a published video; an
asynchronous job answers its ID, which job-poll follows."
  (list))

(define-tool-verb generate-thumbnail "vidiq_generate_thumbnail" (&rest arguments)
  "A thumbnail generated for a concept or a video; signed URLs it returns must be
passed on untouched."
  (list))

(define-tool-verb score-title "vidiq_score_title" (&rest arguments)
  "A title scored against comparable evidence for the same channel and format."
  (list))

(define-tool-verb score-thumbnail "vidiq_score_thumbnail" (&rest arguments)
  "A thumbnail scored for a real published video, its intended title and an
image; a score is not a performance verdict."
  (list))

(define-tool-verb update-competitors "vidiq_update_competitors" (&rest arguments)
  "Change the competitors tracked in the user's vidIQ account: confirm the exact
additions and removals before calling it."
  (list))
