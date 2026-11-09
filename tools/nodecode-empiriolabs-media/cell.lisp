;;;; cell.lisp --- the primer, the three tool contracts, /empiriolabs-media, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :empiriolabs-media) answers the primer below, and every session's
;;;; help section carries one line naming it. The same three calls are tools
;;;; the model invokes and Lisp functions an eval snippet calls, so a
;;;; conversation and a script reach one wire.

(in-package #:nodecode-empiriolabs-media)

(defparameter *primer*
  "EmpirioLabs AI makes images, videos and speech, through the
nodecode-empiriolabs-media cell: three tools, and the same three Lisp functions called
through eval. Each answers a string naming the file it saved; give the operator that
path so they can open it.
  empiriolabs_image {prompt, aspect_ratio?, image?}
      one image. aspect_ratio is square (1:1), landscape (16:9) or portrait (9:16).
      image is an http(s) URL, a data:image URI or a local PNG, JPEG, WebP or GIF file
      to edit or to reference; leave it out for a plain generation.
  empiriolabs_video {prompt, image?, aspect_ratio?, duration?, resolution?}
      a few seconds of video from the prompt, or from an image you name. Send duration
      in seconds, aspect_ratio and resolution as the model spells them; a member you
      leave out is the model's own default. Generation is slow: the tool waits up to the
      section's video_timeout_seconds, and when the job outlasts it the answer carries the
      job id and the URL to read it from.
  empiriolabs_speech {text, voice?}
      the text spoken into an audio file (MP3).
  (empiriolabs-media:models :kind \"image\"|\"video\"|\"speech\")  the live model catalog,
      one line a model; use it when the operator asks which models there are.
Every call is billed to the EmpirioLabs AI account the key belongs to, and nothing asks for
approval first: say what one costs before a long video. The prompts, the source images
and the text to speak go to api.empiriolabs.ai, and nowhere else. ERROR:
EMPIRIOLABS-MEDIA-ERROR names the problem; when it says no key, tell the operator to set
EMPIRIOLABS_API_KEY, or to write it in this cell's api_key."
  "What (help :empiriolabs-media) answers while the cell runs.")

;;; --- /empiriolabs-media -------------------------------------------------------

(defun status-line ()
  "What the command answers with no argument: where the cell points, what it
would ask for, and whether it can."
  (format nil "EmpirioLabs AI media at ~a: image ~a, video ~a, speech ~a~@[, voice ~a~]; ~:[NO KEY (set EMPIRIOLABS_API_KEY or this cell's api_key)~;key set~]. Files land in ~a. /empiriolabs-media models [image|video|speech] lists the catalog; /empiriolabs-media image PROMPT makes one."
          (base-url) (setting :image-model) (setting :video-model) (setting :tts-model)
          (nonblank (setting :voice))
          (and (api-key) t)
          (namestring (nlk:home +folder+))))

(defun run-command (args session-id)
  "/empiriolabs-media alone: where the cell points and whether it has a key.
`models [image|video|speech]`: the live catalog. `image PROMPT`: one image,
made now, which takes a while and is billed."
  (declare (ignore session-id))
  (running-settings)
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text)))
    (cond ((null space) (status-line))
           (t (let ((head (string-downcase (subseq text 0 space)))
                    (rest (nlk:trimmed (subseq text (1+ space)))))
                (cond ((zerop (length rest)) (status-line))
                      ((string= head "models") (models :kind rest))
                      ((string= head "image") (image rest))
                      (t (fail "~s is not a thing this command does: try `models [image|video|speech]` or `image PROMPT`"
                               head))))))))

;;; --- the tools' boundaries ----------------------------------------------------

(nlk:define-contract media-image
  (:doc "Generate one image from a prompt, or edit one, through EmpirioLabs AI. The answer names the file it saved.")
  ("prompt" :text :required t
            :doc "what the image should show; for an edit, what to change")
  ("aspect_ratio" :string :options '("square" "landscape" "portrait") :default "square"
                  :doc "square (1:1), landscape (16:9) or portrait (9:16)")
  ("image" :string :default ""
           :doc "an http(s) URL, a data:image URI or a local PNG, JPEG, WebP or GIF to edit or reference; empty to generate"))

(nlk:define-contract media-video
  (:doc "Generate a few seconds of video from a prompt, or animate an image, through EmpirioLabs AI. Generation is slow: the tool waits, and answers a job id when the wait runs out. The answer names the file it saved.")
  ("prompt" :text :required t
            :doc "what should happen in the video")
  ("image" :string :default ""
           :doc "an http(s) URL, a data:image URI or a local image file to animate; empty for text-to-video")
  ("aspect_ratio" :string :default ""
                  :doc "the ratio the model spells, such as 16:9; empty for its default")
  ("duration" :integer :default 0
              :doc "seconds of video; 0 for the model's own default")
  ("resolution" :string :default ""
                :doc "the resolution the model spells, such as 720p; empty for its default"))

(nlk:define-contract media-speech
  (:doc "Speak text through EmpirioLabs AI. The answer names the audio file it saved.")
  ("text" :text :required t :doc "the words to speak")
  ("voice" :string :default ""
           :doc "the voice the model offers; empty for the section's voice, then the model's own"))

(defun image-tool (call)
  "The empiriolabs_image tool: CALL's image, made."
  (image (media-image-prompt call)
         :aspect (media-image-aspect-ratio call)
         :source (media-image-image call)))

(defun video-tool (call)
  "The empiriolabs_video tool: CALL's video, made."
  (video (media-video-prompt call)
         :source (media-video-image call)
         :aspect (media-video-aspect-ratio call)
         :duration (media-video-duration call)
         :resolution (media-video-resolution call)))

(defun speech-tool (call)
  "The empiriolabs_speech tool: CALL's text, spoken."
  (speech (media-speech-text call) :voice (media-speech-voice call)))

;;; --- the declaration ----------------------------------------------------------

(nle:define-cell empiriolabs-media
  (:section ("empiriolabs-media")
    (:guide "a key is what every call needs -- EMPIRIOLABS_API_KEY, or api_key written here; the model members default to EmpirioLabs AI's release defaults; base_url moves every call behind your own proxy; then ask for an image, a video or speech")
    ("api_key" :secret
               :doc "the EmpirioLabs AI API key, written here (optional)")
    ("api_key_env" :env
                   :doc "the environment variable holding the key, when it is not written here (EMPIRIOLABS_API_KEY is read either way)")
    ("base_url" :string :default "https://api.empiriolabs.ai/v1"
                :doc "the API root")
    ("image_model" :string :default "flux-2-klein-4b"
                   :doc "the model an image asks for")
    ("video_model" :string :default "hunyuan-video-1-5"
                   :doc "the model a video asks for")
    ("tts_model" :string :default "tts-2-flash"
                 :doc "the model speech asks for")
    ("voice" :string
             :doc "the voice speech uses when the call names none; empty for the model's own")
    ("timeout_seconds" :integer :default 300 :min 1
                       :doc "one request's ceiling, in seconds")
    ("video_timeout_seconds" :integer :default 900 :min 1
                             :doc "how long one video call waits for its job"))
  (:settings #'read-settings)
  (:help :empiriolabs-media
         "empiriolabs-media: images, videos and speech from EmpirioLabs AI (empiriolabs_image, empiriolabs_video, empiriolabs_speech)"
         *primer*)
  (:tool "empiriolabs_image" (nlk:contract 'media-image) #'image-tool)
  (:tool "empiriolabs_video" (nlk:contract 'media-video) #'video-tool)
  (:tool "empiriolabs_speech" (nlk:contract 'media-speech) #'speech-tool)
  (:command "empiriolabs-media" 'run-command
            :catches 'empiriolabs-media-error
            :description "Images, videos and speech from EmpirioLabs AI"
            :argument-hint "[models [image|video|speech]] | image PROMPT"))
