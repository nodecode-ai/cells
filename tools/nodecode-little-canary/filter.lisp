;;;; filter.lisp --- what a text does before any model sees it.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Two readings, both cheap and both local. The first reads the untrusted
;;;; text for the shapes an injection takes: an instruction that overrides
;;;; earlier ones, a demand for the system prompt, a persona switch, a
;;;; delimiter forgery, a lifted constraint, a credential demand, a URL to
;;;; send something to. The second reads the canary's ANSWER for the residue
;;;; of having been affected: a persona it adopted, its own instructions
;;;; discussed, its limits declared lifted. Neither reading decides anything
;;;; on its own: each answers a list of (SIGNAL . WEIGHT) and the pipeline
;;;; adds them up against the section's threshold.
;;;;
;;;; Weights, not booleans, because one "ignore previous instructions" in a
;;;; quoted email is a FLAG and the same sentence with a system-prompt demand
;;;; and a persona switch beside it is a BLOCK.

(in-package #:nodecode-little-canary)

(defparameter +structural-rules+
  '(("instruction-override" 45
     "\\b(?:ignore|disregard|forget|override)\\b[^.\\n]{0,40}\\b(?:previous|prior|above|earlier|all|any)\\b[^.\\n]{0,20}\\b(?:instructions?|prompts?|rules?|directions?|guidelines?)\\b")
    ("system-prompt-demand" 40
     "\\b(?:reveal|print|show|repeat|output|dump|display|tell\\s+me)\\b[^.\\n]{0,30}\\b(?:system\\s+prompt|system\\s+message|initial\\s+instructions|your\\s+(?:instructions|rules|configuration))\\b")
    ("persona-switch" 30
     "\\b(?:you\\s+are\\s+now|from\\s+now\\s+on\\s+you|act\\s+as|pretend\\s+to\\s+be|roleplay\\s+as|respond\\s+as)\\b")
    ("delimiter-forgery" 30
     "<\\|?\\s*(?:system|im_start|im_end|endoftext)|\\[/?inst\\]|<<\\s*/?sys\\s*>>|###\\s*(?:system|instruction)")
    ("constraint-lift" 30
     "\\b(?:no\\s+(?:longer\\s+)?(?:restrictions|limits|rules)|without\\s+any\\s+(?:restrictions|limits)|unrestricted|jailbreak|developer\\s+mode|dan\\s+mode|do\\s+anything\\s+now)\\b")
    ("credential-demand" 25
     "\\b(?:reveal|print|show|send|give|paste|repeat)\\b[^.\\n]{0,25}\\b(?:api\\s+key|password|secret|token|credentials?|ssh\\s+key)\\b")
    ("exfiltrate-to-url" 25
     "\\b(?:send|post|upload|forward|encode|append)\\b[^.\\n]{0,40}\\bhttps?://"))
  "The structural family: (ID WEIGHT PATTERN), matched case-insensitively
against the screened text, one line at a time. The weights are the original
plugin's block_threshold arithmetic: the structural layer alone can reach a
BLOCK, so a plain injection costs no model call.")

(defparameter *structural-scanners*
  (loop for (id weight pattern) in +structural-rules+
        collect (list id weight pattern
                      (ppcre:create-scanner pattern :case-insensitive-mode t)))
  "The rules compiled once, at load.")

(defparameter +canary-rules+
  '(("adopted-the-persona" 40
     "\\b(?:i\\s+am|i'm|as)\\s+(?:dan\\b|an?\\s+(?:unrestricted|unfiltered)|now\\s+unrestricted)"
     "the canary answered as the persona the text asked for")
    ("discussed-its-instructions" 35
     "\\b(?:my|the)\\s+(?:system\\s+prompt|initial\\s+instructions|instructions\\s+(?:are|say|tell))\\b|\\bsystem\\s+prompt\\s*:"
     "the canary talked about its own instructions")
    ("declared-limits-lifted" 35
     "\\b(?:filters?|limits?|restrictions?)\\s+(?:are|have\\s+been|were)\\s+(?:off|disabled|lifted|removed)\\b|\\bno\\s+(?:restrictions|limits|filters)\\b|\\bunrestricted\\b"
     "the canary claimed its limits were lifted")
    ("obeyed-the-demand" 40
     "\\b(?:certainly|sure|of\\s+course)[,.!]?\\s+(?:here\\s+(?:is|are)|my)\\s+(?:the\\s+)?(?:system\\s+prompt|instructions)"
     "the canary obeyed a demand to hand over its instructions")
    ("leaked-a-credential-shape" 15
     "\\b(?:sk-[a-z0-9]{8,}|api[\\s_-]?key\\s*[:=]|password\\s*[:=]|BEGIN\\s+(?:RSA|OPENSSH)\\s+PRIVATE\\s+KEY)\\b"
     "the canary's answer carries a credential shape"))
  "The behavioural family: (ID WEIGHT PATTERN REASON), read against the
canary's ANSWER. A small model given the text and no tools has nothing to
lose, so what it says about itself is evidence of what the text did.")

(defparameter *canary-scanners*
  (loop for (id weight pattern reason) in +canary-rules+
        collect (list id weight reason
                      (ppcre:create-scanner pattern :case-insensitive-mode t)))
  "The behavioural rules compiled once, at load.")

(defparameter +evidence-chars+ 60
  "How much of what matched a rule the signal quotes back.")

(defparameter +max-signals+ 6
  "How many signals one verdict carries, heaviest first.")

(defun matched-text (scanner text)
  "What SCANNER matched in TEXT, one line of it, or NIL."
  (multiple-value-bind (start end) (ppcre:scan scanner text)
    (when start
      (nlk:one-line (subseq text start (min end (+ start +evidence-chars+)))))))

(defun structural-signals (text)
  "The structural family's reading of TEXT => ((ID WEIGHT SENTENCE) ...)."
  (loop for (id weight pattern scanner) in *structural-scanners*
        for match = (matched-text scanner text)
        when match
          collect (list id weight (format nil "~a: ~s" id match))))

(defun canary-signals (answer)
  "The behavioural family's reading of the canary's ANSWER => ((ID WEIGHT SENTENCE) ...)."
  (loop for (id weight reason scanner) in *canary-scanners*
        when (matched-text scanner answer)
          collect (list id weight (format nil "~a: ~a" id reason))))

(defun weigh (signals)
  "The risk SIGNALS add up to, capped at 100."
  (min 100 (loop for (nil weight nil) in signals sum weight)))
