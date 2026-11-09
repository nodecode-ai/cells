;;;; contracts.lisp --- the five tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from the declaration, the value the body runs on is
;;;; materialized from it, and a call that does not satisfy it is refused
;;;; before the body sees it.

(in-package #:nodecode-acedatacloud-media)

(nlk:define-contract image-request
  (:doc "Make pictures with Ace Data Cloud: Nano Banana (generation and reference-image editing), GPT Image, Seedream, FLUX, Qwen Image or Midjourney. One call spends Ace Data Cloud credits on the operator's key, and the costly options (a pro model, 4K, four pictures) spend more. The answer names the picture URLs, or the task id to hand to acedatacloud_task when the render outlived the call's wait.")
  ("prompt" :text :required t
   :doc "what to draw, in words; a Nano Banana edit describes the change")
  ("service" :string :options '("nano-banana" "gpt-image" "seedream" "flux" "qwen-image" "midjourney")
   :doc "which Ace Data Cloud image service: nano-banana (default), gpt-image, seedream, flux, qwen-image or midjourney")
  ("model" :string
   :doc "the service's own model id, when it takes one: nano-banana-2-lite, gpt-image-2, flux-dev... Omit for the service's default")
  ("size" :string
   :doc "the picture's shape or size as that service writes it: 16:9, 1:1 or 9:16 on Nano Banana; 1024x1024 on GPT Image and FLUX; 2K on Seedream; 1024*1024 on Qwen Image")
  ("resolution" :string
   :doc "Nano Banana only: 1K, 2K or 4K")
  ("count" :integer :slot copies
   :doc "how many pictures, 1 to 4, where the service counts them")
  ("image_urls" :array :of :string
   :doc "Nano Banana reference images to edit, as https URLs: one to four. Omit to generate from the prompt alone")
  ("wait_seconds" :integer
   :doc "how long this call waits for the render before answering its task id: 0 answers at once, and 60 is the ceiling. Omit for the operator's setting"))

(nlk:define-contract video-request
  (:doc "Make a video with Ace Data Cloud: Seedance, HappyHorse, Veo, Kling, MiniMax, Wan or Grok Imagine. One call spends Ace Data Cloud credits on the operator's key, and the costly options (4K, a longer clip) spend more. The answer names the video URL, or the task id to hand to acedatacloud_task when the render outlived the call's wait.")
  ("prompt" :text :required t
   :doc "the shot: subject, action, setting, light, camera")
  ("service" :string :options '("seedance" "happyhorse" "veo" "kling" "minimax" "wan" "grok")
   :doc "which Ace Data Cloud video service: seedance (default), happyhorse, veo, kling, minimax, wan or grok")
  ("model" :string
   :doc "the service's own model id, when it takes one. Omit for the service's default")
  ("ratio" :string :slot frame-ratio
   :doc "the frame's shape as that service writes it: 16:9, 9:16, 1:1, 4:3, 21:9 or adaptive")
  ("resolution" :string
   :doc "480p, 720p, 1080p or 4k, where the service offers it")
  ("duration" :integer
   :doc "the clip's length in seconds, within what the service renders")
  ("image" :string
   :doc "a first frame to animate, as an https URL: Veo, Seedance and MiniMax take one. Omit for text-to-video")
  ("wait_seconds" :integer
   :doc "how long this call waits for the render before answering its task id: 0 answers at once, and 60 is the ceiling. Omit for the operator's setting"))

(nlk:define-contract audio-request
  (:doc "Make music or speech with Ace Data Cloud: Suno for a song from a description or your own lyric, Fish Audio for text to speech. One call spends Ace Data Cloud credits on the operator's key. The answer names the audio URL, or the task id to hand to acedatacloud_task when the render outlived the call's wait.")
  ("prompt" :string
   :doc "Suno: the song's description, or the lyric when custom is true. Fish Audio: the text to say. One of the two is required")
  ("service" :string :options '("suno" "fish")
   :doc "suno for music (default), fish for text to speech")
  ("model" :string
   :doc "the service's own model: chirp-v6, chirp-v5... on Suno; s2-pro on Fish Audio. Omit for the service's default")
  ("title" :string
   :doc "Suno custom mode: the song's title")
  ("style" :string
   :doc "Suno custom mode: the style, as tags or a phrase")
  ("lyric" :string
   :doc "Suno custom mode: the words to sing; omit with instrumental true for an instrumental")
  ("custom" :boolean
   :doc "Suno: take the title, style and lyric as given instead of writing a song from the description")
  ("instrumental" :boolean
   :doc "Suno: no vocals")
  ("duration" :integer
   :doc "the song's length in seconds, 10 to 360")
  ("wait_seconds" :integer
   :doc "how long this call waits for the render before answering its task id: 0 answers at once, and 60 is the ceiling. Omit for the operator's setting"))

(nlk:define-contract task-request
  (:doc "Read one Ace Data Cloud task by the id a generate call answered. A task that is still running answers its state; one that has finished answers the media URLs. It never submits a new job, so asking again costs nothing.")
  ("service" :text :required t
   :doc "the service the task belongs to, as the generate call named it: nano-banana, seedance, suno...")
  ("task_id" :text :required t
   :doc "the task id a generate call or an earlier acedatacloud_task answered")
  ("wait_seconds" :integer
   :doc "how long this call may wait for a task that is still running: 0 answers its state at once, and 60 is the ceiling"))

(nlk:define-contract lookup-request
  (:doc "Ace Data Cloud's other paid requests: Google web or news search with links and snippets, or face landmark analysis of one image. Each spends Ace Data Cloud credits on the operator's key.")
  ("mode" :string :options '("web" "news" "face")
   :doc "web or news for a Google search, face for landmark analysis of one image")
  ("query" :string
   :doc "the search words; required for web and news")
  ("image_url" :string
   :doc "face: the picture to analyse, as an https URL")
  ("number" :integer :slot amount
   :doc "how many results, 1 to 20")
  ("page" :integer
   :doc "which page of results, from 1")
  ("country" :string
   :doc "the search's country code, such as us")
  ("language" :string
   :doc "the search's language code, such as en"))
