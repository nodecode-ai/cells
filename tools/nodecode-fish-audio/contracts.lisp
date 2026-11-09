;;;; contracts.lisp --- the three tools' one boundary, declared.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Each contract is the tool's schema and the record its body reads, off one
;;;; declaration: the model sees these docs, and an argument outside a declared
;;;; type or a missing required one is refused before a billed call is made.

(in-package #:nodecode-fish-audio)

(nlk:define-contract fish-speech
  (:doc "Turn one text into a spoken audio file with Fish Audio, billed to the operator's account: expressive, multi-speaker, with emotion cues in square brackets such as [excited] or [whispering]. The answer names the saved file -- give that path to the operator. For plain read-aloud of a short sentence this is still a paid call.")
  ("text" :text :required t
          :doc "what to say, in the language it is written in; emotion cues go in square brackets, up to 3000 characters")
  ("voice" :string
           :doc "a Fish Audio voice id (search with fish_voices first); omitted, the section's voice, else Fish Audio's default")
  ("model" :string :options +tts-models+
           :doc "omit for the section's tts_model; drama-3 is the one for multi-speaker scenes")
  ("format" :string :options '("mp3" "ogg" "wav") :slot format-name
            :doc "mp3 unless set: mp3 plays anywhere, ogg is Ogg Opus (small, for voice notes), wav is for editing")
  ("speed" :number :slot pace :doc "0.5 to 2.0; 1.0 unless sent")
  ("language" :string
              :doc "a BCP-47 tag such as en, es-MX or zh-CN: it fixes how numbers, dates and units are read, not the voice")
  ("speakers" :array :of :string
                      :doc "2 to 4 voice ids, for a conversation between voices; only the S2 and drama models read it"))

(nlk:define-contract fish-voices
  (:doc "Fish Audio's voice library, billed to the operator's account only for a clone: search the public voices by words, language and sort; list this account's own voices; get one voice in full; or clone a voice from 1 to 20 local audio samples, with the speaker's consent. Read first, then pass a voice id to fish_speak.")
  ("action" :string :required t :options '("search" "mine" "get" "clone")
            :doc "search the library, list this account's voices, get one, or clone one")
  ("query" :string :doc "search: words in the voice's title")
  ("language" :string :doc "search: a language tag such as en or zh")
  ("sort" :string :options '("score" "task_count" "created_at") :slot order
          :doc "search: score (the default), task_count or created_at")
  ("page" :integer :doc "search and mine: 1 unless sent")
  ("page_size" :integer :doc "search and mine: 1 to 20, 20 unless sent")
  ("voice_id" :string :doc "get: the voice's id")
  ("title" :string :doc "clone: the name the new voice is saved under")
  ("description" :string :doc "clone: one line about the voice")
  ("visibility" :string :options '("private" "unlist")
                :doc "clone: private (the default) or unlist; publishing is done on the Fish Audio website")
  ("sample_paths" :array :of :string
                          :doc "clone: 1 to 20 local audio files of the speaker (mp3, wav, ogg, webm, flac, m4a), at most 20 MB each")
  ("consent" :boolean
             :doc "clone: true only once the user has confirmed the speaker gave permission"))

(nlk:define-contract fish-transcript
  (:doc "Transcribe one local audio or video file with Fish Audio, billed to the operator's account: the text, speaker turns and language when transcribe-1-pro reads them. With srt true a subtitle file is written beside the audio. The file is uploaded, so pass a path the operator named.")
  ("file_path" :text :required t
                    :doc "the audio file: absolute, or relative to the folder Nodecode runs in; mp3, wav, ogg, webm, flac, m4a or aac, at most 50 MB")
  ("model" :string :options +stt-models+
                                :doc "omit for the section's stt_model; transcribe-1-pro reads WebM and speaker turns")
  ("language" :string :doc "the spoken language, when it is known: a tag such as en or zh")
  ("timestamps" :boolean :default t :doc "word timings; on unless turned off")
  ("diarize" :string :options '("auto" "true" "false") :default "auto"
             :doc "who spoke when: auto unless the audio has one voice")
  ("num_speakers" :integer :doc "the exact number of speakers, when it is known")
  ("min_speakers" :integer :doc "the fewest speakers to look for")
  ("max_speakers" :integer :doc "the most speakers to look for")
  ("tag_audio_events" :boolean :default t
                      :doc "mark laughter, applause and the like in the text")
  ("srt" :boolean :doc "write a .srt subtitle file next to the audio"))
