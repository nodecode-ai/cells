;;;; cell.lisp --- the section, the eight tools, the manual, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every tool is registered against a hand-written schema, because a
;;;; composition plan's chunks are a union two declarations deep (new audio, or
;;;; a kept slice of a stored song) and the JSON-Schema projection of a
;;;; contract has no union in it. The bodies read the decoded argument table
;;;; and refuse before anything billed goes out.
;;;;
;;;; Config, a top-level key:
;;;;   "elevenlabs-music": {"api_key": "...", "music_model": "music_v2_5",
;;;;                        "output_format": "mp3_44100_128",
;;;;                        "store_for_editing": true, "wait_seconds": 240,
;;;;                        "output_dir": "", "api_base_url": "https://api.elevenlabs.io"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-elevenlabs-music)

(defparameter +primer+
  (concatenate
   'string
   "ElevenLabs Music is available through the nodecode-elevenlabs-music cell: tools called with
"
   "JSON arguments, each answering JSON text whose media_tag lines are MEDIA:<path> -- put each one
"
   "on a line of its own in the reply to deliver the audio. Billed to the operator's ElevenLabs
"
   "account by length; draft_plan and finetunes are free.
"
   "  elevenlabs_music_compose        a song or instrumental from a prompt, 3 s to 10 min
"
   "  elevenlabs_music_draft_plan    free: ElevenLabs writes a plan (sections, lyrics, styles)
"
   "  elevenlabs_music_compose_plan  make music from a plan; also edits, extends or loops an
"
   "      earlier song by keeping its slices and regenerating the rest
"
   "  elevenlabs_music_upload        store the operator's own audio so a plan can edit or sample it
"
   "  elevenlabs_music_stems          split a track into two or six stems
"
   "  elevenlabs_music_from_video     score 1-10 videos, optionally with sound effects
"
   "  elevenlabs_music_finetunes      free: the Music finetunes this account can use
"
   "  elevenlabs_music_job             check on, or wait for, a generation still running
"
   "Answer genre, mood, instruments, tempo, key and era in a prompt, and put a length the user
"
   "names in length_seconds: an omitted length lets ElevenLabs pick one. A generation takes
"
   "seconds to minutes; when a tool answers {status: running, job_id: ...} the song is still being
"
   "made -- call elevenlabs_music_job with that id rather than starting the request again, which
"
   "would bill twice. Tracks are kept in the operator's cache/generated/audio/ and listed in the
"
   "cell's tracks.jsonl.")
  "What (help :elevenlabs-music) answers while the cell runs.")

;;; --- the schemas ----------------------------------------------------------

(defparameter +source+
  (nlk:json-object
   "track_id" (nlk:json-object "type" "string"
                                 "description" "A track_id from an earlier result of these tools.")
   "file_path" (nlk:json-object "type" "string"
                                  "description" "An audio file on this machine, absolute or relative to the working directory."))
  "The two ways a tool is pointed at audio; exactly one of them.")

(defun schema (description properties &optional required)
  "The registration plist REGISTER-TOOL takes: a description and the
JSON-Schema object of the arguments."
  (list :description description
        :parameters (nlk:make-json-object
                     "type" "object" "properties" properties
                     "required" (coerce required 'vector))))

(defparameter +compose-schema+
  (schema
   (concatenate 'string
                "Make a song or instrumental with ElevenLabs Music from a text prompt (3 s to 10 min, "
                "vocals in many languages or instrumental). Answer genre, mood, instruments, tempo "
                "(BPM), key and era in the prompt; lyrics may go in it too. A length the user names "
                "goes in length_seconds. Returns the file, its title, lyrics and elevenlabs_song_id "
                "(for later edits). " +deliver+)
   (nlk:json-object
    "prompt" (nlk:json-object "type" "string" "maxLength" 4100
                               "description" "What the music is.")
    "length_seconds" (nlk:json-object "type" "number" "minimum" 3 "maximum" 600
                                       "description" "Length in seconds; omitted, ElevenLabs picks one.")
    "instrumental" (nlk:json-object "type" "boolean" "description" "Guarantee no vocals.")
    "model" (nlk:json-object "type" "string" "enum" (coerce +models+ 'vector)
                              "description" "Omit for the operator's setting.")
    "finetune_id" (nlk:json-object "type" "string"
                                    "description" "A Music finetune id from elevenlabs_music_finetunes."))
   '("prompt")))

(defparameter +compose-plan-chunks+
  (nlk:json-object
   "text" (nlk:json-object "type" "string"
                             "description" "New audio for one section: the section name in brackets, then lyric lines separated by newlines; inline cues in braces ({guitar solo}), sung sounds in parentheses ((ooh)).")
   "duration_ms" (nlk:json-object "type" "integer" "minimum" 3000 "maximum" 120000)
   "positive_styles" (nlk:json-object "type" "array" "items" (nlk:json-object "type" "string")
                                        "maxItems" 50
                                        "description" "Styles to include; the first chunk's set the song (6 or more).")
   "negative_styles" (nlk:json-object "type" "array" "items" (nlk:json-object "type" "string")
                                        "maxItems" 50)
   "context_adherence" (nlk:json-object "type" "string" "enum" #("low" "medium" "high"))
   "conditioning_ref" (nlk:json-object "type" "object"
                                         "description" "Up to 30 s of a stored song to sound like: {song_id, range}.")
   "condition_strength" (nlk:json-object "type" "string"
                                           "enum" #("low" "medium" "high" "xhigh"))
   "song_id" (nlk:json-object "type" "string"
                                "description" "With range: a slice of a stored song, kept unchanged.")
   "range" (nlk:json-object "type" "object"
                              "description" "{start_ms, end_ms} of a kept slice."))
  "One chunk of a composition plan: new audio, or a kept slice of a stored song.")

(defparameter +compose-plan-schema+
  (schema
   (concatenate 'string
                "Make music from a composition plan: an ordered list of chunks, each either new audio "
                "(text with section name and lyrics, duration, styles) or a kept slice of a stored song "
                "(song_id + range). Use it for exact lyrics and structure, and to edit, extend, loop or "
                "re-style a song from an earlier result: keep its slices, regenerate the rest. 1-30 "
                "chunks, 3 s to 10 min in total. " +deliver+)
   (nlk:json-object
    "chunks" (nlk:json-object "type" "array" "minItems" 1 "maxItems" 30
                                "items" +compose-plan-chunks+)
    "model" (nlk:json-object "type" "string" "enum" (coerce +plan-models+ 'vector))
    "seed" (nlk:json-object "type" "integer" "minimum" 0 "maximum" 2147483647)
    "finetune_id" (nlk:json-object "type" "string"))
   '("chunks")))

(defparameter +draft-plan-schema+
  (schema
   (concatenate 'string
                "Free (no credits): have ElevenLabs write a composition plan from a prompt, with "
                "sections, lyrics, durations and styles. Show or edit it, then pass its chunks to "
                "elevenlabs_music_compose_plan. source_chunks revises an existing plan instead.")
   (nlk:json-object
    "prompt" (nlk:json-object "type" "string" "maxLength" 4100)
    "length_seconds" (nlk:json-object "type" "number" "minimum" 3 "maximum" 600)
    "model" (nlk:json-object "type" "string" "enum" (coerce +plan-models+ 'vector))
    "source_chunks" (nlk:json-object "type" "array" "maxItems" 30
                                        "items" (nlk:json-object "type" "object")
                                        "description" "A plan's chunks to rework according to the prompt."))
   '("prompt")))

(defparameter +upload-schema+
  (schema
   (concatenate 'string
                "Store an existing song (the operator's own audio file) at ElevenLabs so "
                "elevenlabs_music_compose_plan can edit, extend or sample it. Returns its "
                "elevenlabs_song_id and, optionally, its composition plan and lyrics. Billed like "
                "generating a song of that length; uploads are screened for copyright.")
   (nlk:json-object
    "track_id" (nlk:json-value +source+ :object "track_id")
    "file_path" (nlk:json-value +source+ :object "file_path")
    "extract_plan" (nlk:json-object "type" "boolean"
                                     "description" "Return the song's composition plan (default true)."))))

(defparameter +stems-schema+
  (schema
   (concatenate 'string
                "Split a song into stems with ElevenLabs: two (vocals, accompaniment) or six "
                "(vocals, drums, bass, guitar, piano, other). Works on a track from these tools or "
                "any audio file. " +deliver+)
   (nlk:json-object
    "track_id" (nlk:json-value +source+ :object "track_id")
    "file_path" (nlk:json-value +source+ :object "file_path")
    "stems" (nlk:json-object "type" "string" "enum" #("two" "six")
                              "description" "Default six."))))

(defparameter +from-video-schema+
  (schema
   (concatenate 'string
                "Score a video with ElevenLabs: background music timed to 1-10 video files on this "
                "machine (joined in order, 200 MB and 10 min in total). An optional description and "
                "style tags steer it; add_sfx adds sound effects for on-screen events. " +deliver+)
   (nlk:json-object
    "video_paths" (nlk:json-object "type" "array" "minItems" 1 "maxItems" 10
                                     "items" (nlk:json-object "type" "string"))
    "description" (nlk:json-object "type" "string" "maxLength" 4100)
    "tags" (nlk:json-object "type" "array" "maxItems" 10
                             "items" (nlk:json-object "type" "string"))
    "model" (nlk:json-object "type" "string" "enum" (coerce +models+ 'vector))
    "add_sfx" (nlk:json-object "type" "boolean")
    "separate_tracks" (nlk:json-object "type" "boolean"
                                         "description" "Music and sound effects as separate files.")
    "use_audio_as_reference" (nlk:json-object "type" "boolean"
                                                "description" "Match the genre, tempo and mood of the video's own audio."))
   '("video_paths")))

(defparameter +finetunes-schema+
  (schema
   "Free: list the Music finetunes this ElevenLabs account can use (own, workspace, ElevenLabs-curated) with their ids."
   (nlk:json-object
    "created_by" (nlk:json-object "type" "string" "enum" #("self" "workspace" "elevenlabs")))))

(defparameter +job-schema+
  (schema
   "Check a music job these tools started in the background (by job_id), optionally waiting up to wait_seconds for it to finish. A finished job returns the same result as the tool that started it."
   (nlk:json-object
    "job_id" (nlk:json-object "type" "string")
    "wait_seconds" (nlk:json-object "type" "number" "minimum" 0 "maximum" 240
                                      "description" "Default 0."))
   '("job_id")))

;;; --- the declaration -------------------------------------------------------

(defun start ()
  "A fresh process answers no earlier job; a stop forgets the ones this one ran."
  (clrhash *jobs*)
  (nle:on-stop (lambda () (clrhash *jobs*))))

(nle:define-cell elevenlabs-music
  (:section ("elevenlabs-music")
    (:guide "set api_key to an ElevenLabs key with Music access (the Music API needs a paid plan); music_model and output_format are what a generation asks for; wait_seconds is how long a tool waits before it answers with a job id")
    ("api_key" :secret
     :doc "your ElevenLabs API key from https://elevenlabs.io/app/settings/api-keys; ELEVENLABS_API_KEY in the environment is read when this is empty")
    ("music_model" :choice :options '("music_v2_5" "music_v2" "music_v1") :default "music_v2_5"
     :doc "the model a generation asks for; plans, edits and audio references need v2 or v2.5")
    ("output_format" :choice
     :options '("mp3_44100_128" "mp3_44100_192" "mp3_48000_192" "mp3_48000_320"
               "opus_48000_128" "opus_48000_192")
     :default "mp3_44100_128"
     :doc "what a song is saved as; the Music API has no WAV container")
    ("store_for_editing" :boolean :default t
     :doc "ask ElevenLabs to keep each song so a later request can edit, extend or sample it (no extra cost)")
    ("wait_seconds" :integer :default 240 :min 0
     :doc "how long a tool waits for a generation before it answers with a job id")
    ("output_dir" :string :default ""
     :doc "where tracks are written; empty means the organism's cache/generated/audio/")
    ("api_base_url" :string :default "https://api.elevenlabs.io"
     :doc "change only for an ElevenLabs data-residency endpoint"))
  (:tool "elevenlabs_music_compose" +compose-schema+ 'compose)
  (:tool "elevenlabs_music_compose_plan" +compose-plan-schema+ 'compose-plan)
  (:tool "elevenlabs_music_draft_plan" +draft-plan-schema+ 'draft-plan)
  (:tool "elevenlabs_music_upload" +upload-schema+ 'upload)
  (:tool "elevenlabs_music_stems" +stems-schema+ 'stems)
  (:tool "elevenlabs_music_from_video" +from-video-schema+ 'from-video)
  (:tool "elevenlabs_music_finetunes" +finetunes-schema+ 'finetunes)
  (:tool "elevenlabs_music_job" +job-schema+ 'job-tool)
  (:help :elevenlabs-music
         "songs, edits, stems and video scores from the ElevenLabs Music API"
         +primer+)
  (:start #'start))
