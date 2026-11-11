;;;; data.lisp --- the hierarchy, the timeline and the model's own words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every string under :text here is quoted from one file and carries the line
;;;; of that file it stands on. Nothing is paraphrased in a quote; the paraphrase
;;;; lives in :note, which is ours. The file is ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt
;;;; in elder-plinius/CL4R1T4S.

(in-package #:nodecode-model-tiers)

(defparameter +source+
  "elder-plinius/CL4R1T4S, ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt, committed 9 Nov 2026"
  "Where the file comes from, as a person would cite it.")

(defparameter +captured+ "11 Nov 2026"
  "The day the file was read for this cell.")

(defparameter +file+
  (list :path "ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt"
        :bytes 957315
        :lines 7832
        :sha256 "d54e74e3ffd318c3cd0df3ea71f3e54efdd862ee5d988c03206bff10834e3522"
        :commit "84cd1bf98a8bc395cdcd44b9f420484d0d60881a"
        :committed "9 Nov 2026, 21:02:55Z"
        :url "https://github.com/elder-plinius/CL4R1T4S/blob/main/ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt"
        :raw "https://raw.githubusercontent.com/elder-plinius/CL4R1T4S/main/ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt")
  "The one file every answer stands on: its size, its lines, and the digest that says it is still that file.")

(defparameter +tiers+
  '((:mythos . "Anthropic's newest tier, and the file puts it above Opus. Its current models are Fable 5.2 and Mythos 5.2, which share one underlying model.")
    (:opus . "The tier the Mythos tier sits above. Opus 5.5 is the current one.")
    (:sonnet . "The middle of the public ladder. Sonnet 5.5 is the current one.")
    (:haiku . "The smallest and fastest of the public ladder. Haiku 5.5 is the current one."))
  "The tiers the file names, from the top down, in its own sense.")

(defparameter +tier-order+
  '(:mythos :opus :sonnet :haiku)
  "The ladder as the file states it: Mythos above Opus, Opus above Sonnet, Sonnet above Haiku.")

(defparameter +models+
  (list
   (list :name "Claude Fable 5.2"
         :tier :mythos
         :api "claude-fable-5-2"
         :availability "public, and the model whose prompt this is"
         :since "as Fable 5, released 9 June 2026"
         :note "The file calls this one its most intelligent and advanced model for chat and knowledge work. It is a Mythos-tier model with extra safety measures for biology and cybersecurity added on top of Mythos 5.2."
         :quotes (list
                  (cons 97 "The currently selected version of Claude is Claude Fable 5.2. Claude Fable 5.2 is our most intelligent and advanced model for chat and knowledge work.")
                  (cons 117 "The current generation of Mythos-tier models are Claude Mythos 5.2 and Claude Fable 5.2. They share the same underlying model, but the latter has additional safety measures for biology and cybersecurity.")
                  (cons 119 "The training of Fable 5.2 and Mythos 5.2 included measures to limit advances in a narrow set of capabilities that could most directly accelerate AI development, like accelerator kernel optimization.")))

   (list :name "Claude Mythos 5.2"
         :tier :mythos
         :api nil
         :availability "public, one of the two current Mythos-tier models"
         :since "Mythos 5, 9 June 2026"
         :note "The same underlying model as Fable 5.2, without the extra biology and cybersecurity measures. The file does not give it an API string of its own."
         :quotes (list
                  (cons 117 "The current generation of Mythos-tier models are Claude Mythos 5.2 and Claude Fable 5.2. They share the same underlying model, but the latter has additional safety measures for biology and cybersecurity.")))

   (list :name "Claude Mythos Preview"
         :tier :mythos
         :api nil
         :availability "not public: a small number of trusted organizations"
         :since "the first Mythos-class model"
         :note "The first Mythos-class model, and not sold: the file names anthropic.com/glasswing as where to read about it."
         :quotes (list
                  (cons 117 "The first Mythos-class model, Claude Mythos Preview, is not currently available to the public. It is currently being used by a small number of trusted organizations.")
                  (cons 117 "For further information on this topic, Claude can direct the person to 'https://www.anthropic.com/glasswing'.")))

   (list :name "Claude Opus 5.5"
         :tier :opus
         :api "claude-opus-5-5"
         :availability "public"
         :since nil
         :note "The top of the public ladder below the Mythos tier."
         :quotes (list (cons 99 "The most recent publicly available models are Claude Fable 5.2 (the currently selected model), Claude Opus 5.5, Claude Sonnet 5.5, and Claude Haiku 5.5.")))

   (list :name "Claude Sonnet 5.5"
         :tier :sonnet
         :api "claude-sonnet-5-5"
         :availability "public"
         :since nil
         :note "The middle of the public ladder."
         :quotes (list (cons 99 "The most recent publicly available models are Claude Fable 5.2 (the currently selected model), Claude Opus 5.5, Claude Sonnet 5.5, and Claude Haiku 5.5.")))

   (list :name "Claude Haiku 5.5"
         :tier :haiku
         :api "claude-haiku-5-5"
         :availability "public"
         :since nil
         :note "The smallest of the public ladder."
         :quotes (list (cons 99 "The most recent publicly available models are Claude Fable 5.2 (the currently selected model), Claude Opus 5.5, Claude Sonnet 5.5, and Claude Haiku 5.5."))))
  "Every model the file names, with where it sits, whether a person can have it, and the lines that say so.")

(defparameter +events+
  '((:when "9 June 2026"
     :what "Claude Fable 5 and Claude Mythos 5 are released."
     :line 121)
    (:when "12 June 2026"
     :what "Anthropic suspends access to both, to comply with U.S. Department of Commerce export controls."
     :line 121)
    (:when "30 June 2026"
     :what "The Department lifts the controls."
     :line 121)
    (:when "1 July 2026"
     :what "Anthropic restores access."
     :line 121)
    (:when "end of June 2026"
     :what "The model's own reliable knowledge cutoff, as the file states it."
     :line 320)
    (:when "9 November 2026"
     :what "This file is committed to CL4R1T4S by its maintainer."
     :line nil))
  "The dates the file gives, in order, with the line each stands on.")

(defparameter +facts+
  (list
   (list :question "what is above opus"
         :lines 117
         :answer "The Mythos tier. The file says so in one sentence: \"Above Opus sits Anthropic's new Mythos tier.\"")
   (list :question "is fable the same model as mythos"
         :lines 117
         :answer "Yes. Fable 5.2 and Mythos 5.2 share one underlying model; Fable adds safety measures for biology and cybersecurity.")
   (list :question "what models exist"
         :lines 99
         :answer "Fable 5.2, Opus 5.5, Sonnet 5.5 and Haiku 5.5 are the public ones, and Mythos 5.2 sits above them. Mythos Preview is not public.")
   (list :question "what was the export control about"
         :lines 121
         :answer "U.S. Department of Commerce controls suspended access to Fable 5 and Mythos 5 from 12 June 2026; they were lifted 30 June and access restored 1 July 2026.")
   (list :question "what is the knowledge cutoff"
         :lines 320
         :answer "The end of June 2026, in the file's own words.")
   (list :question "does it refuse things"
         :lines 134
         :answer "The file opens its refusal section with \"Claude can discuss virtually any topic factually and objectively\", then lists the places it is careful: child safety, weapons, medical and legal advice.")
   (list :question "what is the api model string"
         :lines 103
         :answer "claude-fable-5-2, claude-opus-5-5, claude-sonnet-5-5 and claude-haiku-5-5, per line 103."))
  "Answers to the questions this file is opened for, each on a line a person can check.")
