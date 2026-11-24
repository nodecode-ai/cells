;;;; data.lisp --- the archive, the hierarchy, the timeline, and the quoted words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every string under :text here is quoted from one document in
;;;; elder-plinius/CL4R1T4S and carries both the document and the line it stands
;;;; on. Nothing is paraphrased in a quote; the paraphrase lives in :note, which
;;;; is ours.

(in-package #:nodecode-model-tiers)

(defparameter +archive+
  (list :name "elder-plinius/CL4R1T4S"
        :url "https://github.com/elder-plinius/CL4R1T4S"
        :head "93a584765460c2fb0ad58380bba476f2149df138"
        :head-date "9 Nov 2026, 23:48:58Z"
        :read "11 Nov 2026")
  "The archive the documents come from, at the commit they were read at.")

(defparameter +captured+ "11 Nov 2026"
  "The day the documents were read for this cell.")

(defparameter +files+
  (list
   (list :key "fable-5.2-code"
         :path "ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt"
         :vendor "Anthropic"
         :document "the Claude Code system prompt, as Fable 5.2 answers it"
         :bytes 957315 :lines 7832
         :sha256 "d54e74e3ffd318c3cd0df3ea71f3e54efdd862ee5d988c03206bff10834e3522"
         :says "names the Mythos tier above Opus, and says Fable 5.2 and Mythos 5.2 share one model")
   (list :key "fable-5.2-web"
         :path "ANTHROPIC/CLAUDE-WEB-APP-FABLE-5.2.md"
         :vendor "Anthropic"
         :document "the Claude web app system prompt, the same model"
         :bytes 427612 :lines 2291
         :sha256 "f6c62d2bdf182bb53e8c05f74ff14803c492c89f4715b2290ed04db3c018145d"
         :says "the same product answers in the chat app, with the same product section")
   (list :key "fable-5.1"
         :path "ANTHROPIC/Claude-Fable-5.1.md"
         :vendor "Anthropic"
         :document "the previous generation's prompt, when the model was Fable 5.1"
         :bytes 275723 :lines 2195
         :sha256 "c57de521ca050e24634697b388fe840350481f238d3080cb2ef223cb8571196a"
         :says "says the same thing a generation earlier: a Mythos-class tier above Opus, one model, extra safety on the Fable one")
   (list :key "opus-5.5"
         :path "ANTHROPIC/CLAUDE-OPUS-5.5.md"
         :vendor "Anthropic"
         :document "the system prompt as Opus 5.5 answers it, the largest document here"
         :bytes 1953216 :lines 21531
         :sha256 "1c92a8c39c6a8b24746150c7ad7934423e2e364069370bb03bcc0b69cc365ff2"
         :says "the tier below Mythos, in its own words")
   (list :key "opus-5"
         :path "ANTHROPIC/OPUS-5.md"
         :vendor "Anthropic"
         :document "the system prompt as Opus 5 answered it"
         :bytes 202762 :lines 2049
         :sha256 "f8bc6dee5b23b926d3cb87a5b2dfc197feea6bfd82119f4a796b767a8da2ec41"
         :says "the same tier one generation back")
   (list :key "gpt-6-astra-tools"
         :path "OPENAI/Codex_Desktop/GPT-6-Astra_Tools.json"
         :vendor "OpenAI"
         :document "Codex on GPT-6 Astra: the tool schemas, as JSON"
         :bytes 1177481 :lines 27868
         :sha256 "de74ff1eedd97be3cab7bdb7a144a649fddec7b4ce5b894d4a95704051b7ac0e"
         :says "what a rival agent is allowed to call, schema by schema")
   (list :key "gpt-6-astra-prompts"
         :path "OPENAI/Codex_Desktop/GPT-6-Astra_Prompts.md"
         :vendor "OpenAI"
         :document "Codex on GPT-6 Astra: the prompt templates"
         :bytes 338952 :lines 5051
         :sha256 "150822d0da1303b5c80180a0368bbb643a6943e1d755570c9069d51f487cfea0"
         :says "an agent based on GPT-6, with no model ladder named at all")
   (list :key "sol-5.6"
         :path "OPENAI/Codex_Desktop/5.6-Sol_SystemPrompt.md"
         :vendor "OpenAI"
         :document "Codex 5.6 Sol: the system prompt"
         :bytes 300534 :lines 4270
         :sha256 "b247f30e23380fc48794756f3ee0ee7e370d008967bca7ae2a13efe3f160c51e"
         :says "says it is based on GPT-5, while the file named GPT-6 Sol says GPT-6")
   (list :key "gpt-6-sol-prompts"
         :path "OPENAI/Codex_Desktop/GPT-6-Sol_Prompts.txt"
         :vendor "OpenAI"
         :document "Codex on GPT-6 Sol: the instruction templates"
         :bytes 161166 :lines 1902
         :sha256 "abb7a285f0894aa95c006ed34ea4515a27fed234aaef046105e54d4bd3b683ab"
         :says "the same agent one model later")
   (list :key "zcode-prompts"
         :path "ZAI/ZCode/Prompts.md"
         :vendor "Z.ai"
         :document "ZCode, an interactive coding agent"
         :bytes 131706 :lines 1843
         :sha256 "3b934c243059d6cff99aeb9f98eafa2951a6477e7e628821dd7ac968ef777fec"
         :says "its security clause names what it refuses and what it will not")
   (list :key "muse-spark"
         :path "META/Muse_Spark_Apr-08-26.txt"
         :vendor "Meta"
         :document "Meta AI on Muse Spark"
         :bytes 49487 :lines 286
         :sha256 "0beefdb67e978f366c67fba6bb039c346d8b2d04c870d07a927fcf2e8c9cb788"
         :says "a different shape entirely: no tools, no ladder, one reply")
   (list :key "cursor-2"
         :path "CURSOR/Cursor_2.0_Sys_Prompt.txt"
         :vendor "Cursor"
         :document "the Cursor 2.0 coding assistant"
         :bytes 23082 :lines 432
         :sha256 "d2ff487e11606752852743eb7294a639e69c2f5129e70616788790489b90dd58"
         :says "an IDE assistant, and what it says about the user's state")
   (list :key "cline"
         :path "CLINE/Cline.md"
         :vendor "Cline"
         :document "Cline, the coding agent"
         :bytes 47221 :lines 576
         :sha256 "cd85fb35ac1091210d27e9a893d6f7bb47a7b341e54c291d19980e3599970783"
         :says "tools and approvals, spelled out one at a time"))
  "Every document this cell can quote: what it is, who wrote it, its size and its digest.")

(defparameter +primary+ "fable-5.2-code"
  "The document the hierarchy and the timeline are read from.")

(defparameter +tiers+
  '((:mythos . "Anthropic's newest tier, and the archive puts it above Opus. Its current models are Fable 5.2 and Mythos 5.2, which share one underlying model.")
    (:opus . "The tier the Mythos tier sits above. Opus 5.5 is the current one.")
    (:sonnet . "The middle of the public ladder. Sonnet 5.5 is the current one.")
    (:haiku . "The smallest and fastest of the public ladder. Haiku 5.5 is the current one."))
  "The tiers the archive names, from the top down, in its own sense.")

(defparameter +tier-order+
  '(:mythos :opus :sonnet :haiku)
  "The ladder as the archive states it: Mythos above Opus, Opus above Sonnet, Sonnet above Haiku.")

(defparameter +models+
  (list
   (list :name "Claude Fable 5.2"
         :tier :mythos
         :api "claude-fable-5-2"
         :availability "public, and the model whose prompt this is"
         :since "as Fable 5, released 9 June 2026"
         :note "The archive calls this one its most intelligent and advanced model for chat and knowledge work. It is a Mythos-tier model with extra safety measures for biology and cybersecurity added on top of Mythos 5.2."
         :quotes (list
                  (list :file "fable-5.2-code" :line 97
                        :text "The currently selected version of Claude is Claude Fable 5.2. Claude Fable 5.2 is our most intelligent and advanced model for chat and knowledge work.")
                  (list :file "fable-5.2-code" :line 117
                        :text "The current generation of Mythos-tier models are Claude Mythos 5.2 and Claude Fable 5.2. They share the same underlying model, but the latter has additional safety measures for biology and cybersecurity.")
                  (list :file "fable-5.2-code" :line 119
                        :text "The training of Fable 5.2 and Mythos 5.2 included measures to limit advances in a narrow set of capabilities that could most directly accelerate AI development, like accelerator kernel optimization.")
                  (list :file "fable-5.1" :line 5
                        :text "This iteration of Claude is Claude Fable 5.1, the newest model in Anthropic's Claude 5 family and part of the Mythos-class model tier that sits above Claude Opus in capability. Claude Fable 5.1 and Claude Mythos 5.1 share the same underlying model.")))

   (list :name "Claude Mythos 5.2"
         :tier :mythos
         :availability "public, one of the two current Mythos-tier models"
         :since "Mythos 5, 9 June 2026"
         :note "The same underlying model as Fable 5.2, without the extra biology and cybersecurity measures. The archive gives it no API string of its own."
         :quotes (list
                  (list :file "fable-5.2-code" :line 117
                        :text "The current generation of Mythos-tier models are Claude Mythos 5.2 and Claude Fable 5.2. They share the same underlying model, but the latter has additional safety measures for biology and cybersecurity.")))

   (list :name "Claude Mythos Preview"
         :tier :mythos
         :availability "not public: a small number of trusted organizations"
         :since "the first Mythos-class model"
         :note "The first Mythos-class model, and not sold: the archive names anthropic.com/glasswing as where to read about it."
         :quotes (list
                  (list :file "fable-5.2-code" :line 117
                        :text "The first Mythos-class model, Claude Mythos Preview, is not currently available to the public. It is currently being used by a small number of trusted organizations.")
                  (list :file "fable-5.1" :line 5
                        :text "Claude Mythos 5.1 is available without those measures to only approved organizations.")))

   (list :name "Claude Opus 5.5"
         :tier :opus
         :api "claude-opus-5-5"
         :availability "public"
         :since nil
         :note "The top of the public ladder below the Mythos tier."
         :quotes (list
                  (list :file "fable-5.2-code" :line 99
                        :text "The most recent publicly available models are Claude Fable 5.2 (the currently selected model), Claude Opus 5.5, Claude Sonnet 5.5, and Claude Haiku 5.5.")
                  (list :file "opus-5.5" :line 91
                        :text "The currently selected version of Claude is Claude Opus 5.5. Claude Opus 5.5 is a powerful model for complex challenges.")))

   (list :name "Claude Sonnet 5.5"
         :tier :sonnet
         :api "claude-sonnet-5-5"
         :availability "public"
         :since nil
         :note "The middle of the public ladder."
         :quotes (list
                  (list :file "fable-5.2-code" :line 99
                        :text "The most recent publicly available models are Claude Fable 5.2 (the currently selected model), Claude Opus 5.5, Claude Sonnet 5.5, and Claude Haiku 5.5.")))

   (list :name "Claude Haiku 5.5"
         :tier :haiku
         :api "claude-haiku-5-5"
         :availability "public"
         :since nil
         :note "The smallest of the public ladder."
         :quotes (list
                  (list :file "fable-5.2-code" :line 99
                        :text "The most recent publicly available models are Claude Fable 5.2 (the currently selected model), Claude Opus 5.5, Claude Sonnet 5.5, and Claude Haiku 5.5."))))
  "Every model the archive names, with where it sits, whether a person can have it, and the lines that say so.")

(defparameter +events+
  '((:when "9 June 2026"
     :what "Claude Fable 5 and Claude Mythos 5 are released."
     :file "fable-5.2-code" :line 121)
    (:when "12 June 2026"
     :what "Anthropic suspends access to both, to comply with U.S. Department of Commerce export controls."
     :file "fable-5.2-code" :line 121)
    (:when "30 June 2026"
     :what "The Department lifts the controls."
     :file "fable-5.2-code" :line 121)
    (:when "1 July 2026"
     :what "Anthropic restores access."
     :file "fable-5.2-code" :line 121)
    (:when "end of June 2026"
     :what "The model's own reliable knowledge cutoff, as the archive states it."
     :file "fable-5.2-code" :line 320)
    (:when "9 November 2026"
     :what "The archive's last commit before this cell read it."
     :file nil :line nil))
  "The dates the archive gives, in order, with the document and line each stands on.")

(defparameter +facts+
  (list
   (list :question "what is above opus"
         :file "fable-5.2-code" :line 117
         :answer "The Mythos tier. The archive says so in one sentence: \"Above Opus sits Anthropic's new Mythos tier.\"")
   (list :question "is fable the same model as mythos"
         :file "fable-5.2-code" :line 117
         :answer "Yes. Fable 5.2 and Mythos 5.2 share one underlying model; Fable adds safety measures for biology and cybersecurity. The generation before says the same of 5.1.")
   (list :question "what models exist"
         :file "fable-5.2-code" :line 99
         :answer "Fable 5.2, Opus 5.5, Sonnet 5.5 and Haiku 5.5 are the public ones, and Mythos 5.2 sits above them. Mythos Preview is not public.")
   (list :question "what was the export control about"
         :file "fable-5.2-code" :line 121
         :answer "U.S. Department of Commerce controls suspended access to Fable 5 and Mythos 5 from 12 June 2026; they were lifted 30 June and access restored 1 July 2026.")
   (list :question "what is the knowledge cutoff"
         :file "fable-5.2-code" :line 320
         :answer "The end of June 2026, in the archive's own words.")
   (list :question "does it refuse things"
         :file "fable-5.2-code" :line 134
         :answer "The refusal section opens with \"Claude can discuss virtually any topic factually and objectively\", then lists where it is careful: child safety, weapons, medical and legal advice.")
   (list :question "what is the api model string"
         :file "fable-5.2-code" :line 103
         :answer "claude-fable-5-2, claude-opus-5-5, claude-sonnet-5-5 and claude-haiku-5-5.")
   (list :question "what does openai say its agent is"
         :file "gpt-6-astra-prompts" :line 7
         :answer "\"You are Codex, an agent based on GPT-6.\" No model ladder, no tiers, no dates. The 5.6 Sol prompt says GPT-5, and the file named GPT-6 Sol says GPT-6.")
   (list :question "how does cursor describe itself"
         :file "cursor-2" :line 3
         :answer "\"You are an advanced AI coding assistant powered by Cursor. You operate exclusively in Cursor, the world's best IDE.\"")
   (list :question "what does zcode refuse"
         :file "zcode-prompts" :line 5
         :answer "Destructive techniques, DoS attacks, mass targeting, supply-chain compromise and detection evasion, unless the context is authorized testing, defence or CTF."))
  "Answers to the questions the archive is opened for, each on a document and a line a person can check.")
