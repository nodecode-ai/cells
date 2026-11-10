;;;; promise.lisp --- the sentence that promises work, and the ones that do not.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from the plugin's _PROMISE, _CONDITIONAL, _NEGATED, _SPLIT and
;;;; _CLAUSE patterns (__init__.py:30-91), alternative for alternative, with
;;;; the same behaviour on the same text: a sentence shorter than 12
;;;; characters or ending in "?" is left alone, a sentence that hands the
;;;; next step to the person is left alone, a negated clause is left alone,
;;;; and the first sentence with a clause that promises the agent's own
;;;; not-yet-done work is the one a nudge quotes.

(in-package #:nodecode-follow-through)

(defparameter +promise-pattern+
  (concatenate 'string
               "\\bI(?:'ve| have) (?:now )?(?:started|begun|kicked off|reopened|launched|initiated|set (?:it )?in motion)\\b"
               "|\\bI(?:'m| am) (?:now |already )?(?:starting|digging|researching|studying|working on|building|running|"
               "preparing|putting together|continuing|going deeper|looking into|pulling|gathering|collecting)\\b"
               "|\\bI(?:'ll| will) (?:now |next |then |also |still |keep |go on to )?(?:start|dig|research|study|build|work|"
               "continue|keep|go|prepare|make|create|write|run|test|send|report|come back|get back|deliver|bring|return|put|"
               "look|check|review|analy[sz]e|finish|complete|do|turn|compile|draft|record|map|learn|practi[cs]e|master|"
               "open|hand|pass|launch|kick off|spin up|set up|fix|land|ship|push|update|patch|wire|follow up)\\b"
               "|\\bI(?:'ll| will)\\b"
               "|\\b(?:running now|started|starting now|already started)\\b"
               "|\\bI(?:'m| am) going to\\b"
               "|\\b(?:next|then|after that),? I(?:'ll| will)\\b"
               "|\\b(?:keep|keeps) (?:digging|going|working|climbing|studying|learning)\\b"
               "|\\b(?:report|come|get) back (?:to you|when|once|with|as soon)\\b"
               "|\\bho (?:già )?(?:iniziato|cominciato|avviato|ripreso|lanciato)\\b"
               "|\\bsto (?:lavorando|studiando|cercando|preparando|costruendo|analizzando|scavando|raccogliendo)\\b"
               "|\\b(?:inizio|comincio|continuo|procedo|preparo|costruisco|approfondisco|torno|riprendo)"
               " (?:ora|adesso|subito|a|con|il|la|lo|i|le|gli)\\b"
               "|\\bti (?:aggiorno|scrivo|mando|porto|riporto|faccio sapere|riscrivo)\\b")
  "Work the agent says has started or will happen (English and Italian).")

(defparameter +conditional-pattern+
  (concatenate 'string
               "\\b(?:if you|if that|se vuoi|se ti va|when you|quando (?:vuoi|mi dici)|tell me|dimmi|your call|should I|"
               "do you want|want me to|vuoi che|on your (?:go|word|yes)|once you|as soon as you|appena (?:mi|tu)|say go|"
               "whenever you|unless you|until you)\\b")
  "A sentence that hands the next step to the person is not a promise of the agent's own work.")

(defparameter +negated-pattern+
  "\\b(?:I won'?t|I will not|I'?m not going to|I am not going to|non (?:lo |la )?(?:farò|faccio))\\b"
  "A sentence that says the work will not happen is not a promise either.")

(defparameter +sentence-split-pattern+ "(?<=[.!?])\\s+|\\n+"
  "One sentence per piece: after terminal punctuation and whitespace, or a newline.")

(defparameter +clause-split-pattern+ ";\\s+|\\s+[—–]\\s+|\\s*—\\s*"
  "One clause per piece: a semicolon, and a dash standing between words.")

(defparameter +promise+ (cl-ppcre:create-scanner +promise-pattern+ :case-insensitive-mode t))
(defparameter +conditional+ (cl-ppcre:create-scanner +conditional-pattern+ :case-insensitive-mode t))
(defparameter +negated+ (cl-ppcre:create-scanner +negated-pattern+ :case-insensitive-mode t))
(defparameter +sentence-split+ (cl-ppcre:create-scanner +sentence-split-pattern+))
(defparameter +clause-split+ (cl-ppcre:create-scanner +clause-split-pattern+))

(defparameter +blank+ (list #\Space #\Tab #\Newline #\Return #\Page)
  "What a strip takes off both ends, as Python's str.strip() does.")

(defun straight-quotes (text)
  "TEXT with the typographic apostrophes folded to the straight one, so the
patterns need one apostrophe and the operator's keyboard needs no other."
  (substitute #\' (code-char #x2019) (substitute #\' (code-char #x2018) (or text ""))))

(defun promise-sentence (text)
  "The first sentence of TEXT that promises the agent's own not-yet-done work,
verbatim, or NIL when every sentence either says something else or hands the
next step to the person."
  (let ((content (straight-quotes text)))
    (dolist (piece (cl-ppcre:split +sentence-split+ content))
      (let ((sentence (string-trim +blank+ piece)))
        (when (and (>= (length sentence) 12)
                   (not (char= #\? (char sentence (1- (length sentence)))))
                   (not (cl-ppcre:scan +conditional+ sentence)))
          (dolist (clause (cl-ppcre:split +clause-split+ sentence))
            (when (and (cl-ppcre:scan +promise+ clause)
                       (not (cl-ppcre:scan +negated+ clause)))
              (return-from promise-sentence sentence))))))
    nil))
