;;;; rules.lisp --- brand profiles and the deterministic linter that checks copy against them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure functions, no store and no hook: what passes is always decided here.
;;;; A verdict is reproducible -- the same text against the same profile always
;;;; scores the same -- because every rule is a pattern, a count or a limit,
;;;; never a judgement. A rewrite of onbrand/rules.py in Mhiah/On-Brand.
;;;;
;;;; A profile is a plist, the same shape the store keeps and the tools edit:
;;;;   (:name "nimbus" :display-name "Nimbus Protocol" :person "third"
;;;;    :tone "calm" :banned ("game-changer") :preferred (("NIMBUS" . "Nimbus"))
;;;;    :required ("NFA") :packs ("crypto") :max-chars 280 :max-hashtags 1
;;;;    :max-emojis 1 :max-exclamations 0 :examples ("...") :notes "")
;;;; A verdict is a plist:
;;;;   (:score 62 :passed nil (:violations ((:rule "banned" :severity "hard"
;;;;    :detail "banned word 'LFG'") ...)))

(in-package #:nodecode-on-brand)

;;; --- the two severities -----------------------------------------------------------

(defparameter +hard+ "hard" "A draft with one does not pass.")
(defparameter +soft+ "soft" "A draft with only soft ones passes, lower on the scale.")

;;; --- scanners, made once -----------------------------------------------------------
;;; A linter that compiles its patterns on every call spends its time in the
;;; regex compiler; these keep one scanner per phrase, for the life of the image.

(defvar *phrase-scanners* (make-hash-table :test #'equal)
  "PHRASE -> the scanner that finds it, case-insensitively, on word edges.")

(defun phrase-scanner (phrase &aux (phrase (nlk:trimmed (or phrase ""))))
  "PHRASE as a scanner matching it case-insensitively between word edges, or
NIL for a blank one."
  (unless (plusp (length phrase))
    (return-from phrase-scanner nil))
  (or (gethash phrase *phrase-scanners*)
      (setf (gethash phrase *phrase-scanners*)
            (cl-ppcre:create-scanner
             (concatenate 'string "(?<!\\w)" (cl-ppcre:quote-meta-chars phrase) "(?!\\w)")
             :case-insensitive-mode t))))

(defvar *spelling-scanners* (make-hash-table :test #'equal)
  "WRONG -> the case-sensitive scanner that finds the misspelling on word edges.")

(defun spelling-scanner (wrong)
  "WRONG as a case-sensitive scanner between word edges, or NIL."
  (let ((wrong (or wrong "")))
    (when (plusp (length (nlk:trimmed wrong)))
      (or (gethash wrong *spelling-scanners*)
          (setf (gethash wrong *spelling-scanners*)
                (cl-ppcre:create-scanner
                 (concatenate 'string "(?<!\\w)" (cl-ppcre:quote-meta-chars wrong) "(?!\\w)")))))))

(defun scan (scanner text)
  "TEXT matches SCANNER (a scanner or a pattern string) => T."
  (and scanner (plusp (length (or text "")))
       (not (null (cl-ppcre:scan scanner text)))))

(defparameter +first-person-plural+
  (cl-ppcre:create-scanner "\\b(we|we're|we've|we'll|we'd|us|our|ours|ourselves)\\b"
                           :case-insensitive-mode t)
  "The words a third-person brand never says.")

(defparameter +first-person-singular+
  (cl-ppcre:create-scanner "\\b(i|i'm|i've|i'll|i'd|me|my|mine)\\b"
                           :case-insensitive-mode t)
  "The words a brand speaking as 'we' does not say.")

(defparameter +hashtag+
  (cl-ppcre:create-scanner "(?:^|[^\\w#])#[a-z0-9_]+" :case-insensitive-mode t)
  "One hashtag: a '#' that starts a word.")

(defparameter +negation+
  (cl-ppcre:create-scanner
   "(?:\\b(?:not|never|nothing|none|isn't|aren't|without|nor|cannot|can't|won't)\\b[^.!?\\n]{0,25})$"
   :case-insensitive-mode t)
  "A negator in the words just before a claim: 'not risk-free' warns, it does not claim.")

;;; --- emoji, without a Unicode table ------------------------------------------------
;;; The ranges every emoji font draws from, as code points; a variation
;;; selector or a joiner is part of the emoji before it, not one of its own.

(defun emoji-p (character)
  "CHARACTER is drawn from an emoji block."
  (let ((code (char-code character)))
    (or (<= #x1F300 code #x1FAFF)
        (<= #x1F1E6 code #x1F1FF)
        (<= #x2600 code #x27BF)
        (<= #x2B00 code #x2BFF)
        (<= #x1F000 code #x1F2FF))))

(defun count-emojis (text)
  "How many emoji TEXT draws: one per drawn character, its modifiers not counted."
  (count-if (lambda (character)
               (and (emoji-p character)
                    (not (member (char-code character) '(#xFE0F #x200D #xFE0E)))))
            (or text "")))

;;; --- the compliance packs ----------------------------------------------------------
;;; Each pack: forbidden claims, always hard, and the disclaimer that becomes
;;; required as soon as the copy talks about the pack's subject.

(defparameter +packs+
  '((:name "crypto"
     :label "Crypto / Web3 promotion"
     :forbidden (("\\bguarantee(d|s)?\\b[^.!?\\n]{0,40}\\b(returns?|profits?|gains?|yield|apy|income)\\b"
                  . "promises guaranteed returns")
                 ("\\b(risk[- ]free|no[- ]risk|can'?t lose|cannot lose|zero risk)\\b"
                  . "claims no risk")
                 ("\\b\\d{2,4}\\s?x\\b(?!\\s?(faster|cheaper|more efficient|speed))"
                  . "promises a price multiple")
                 ("\\b(to the moon|moon(ing)?\\s+soon|next 100x|get rich|life[- ]changing (money|gains))\\b"
                  . "hype price prediction")
                 ("\\b(buy|ape|load up)\\s+(now|today|before it'?s too late)\\b"
                  . "pushes an urgent buy")
                 ("(\\b\\d+(\\.\\d+)?\\s?%\\s?(returns?|apy|apr|yield|profits?|gains?|interest|rewards?)\\b|\\b(returns?|apy|apr|yield|rewards?)\\s+(of|at|around|near|up to)\\s+\\d+(\\.\\d+)?\\s?%)"
                  . "promises a specific return rate"))
     :triggers "\\b(price|token|invest(ing|ment)?|returns?|apy|apr|yield|airdrop|presale|staking rewards?|profit|pump|mint price|floor price)\\b"
     :disclaimers ("NFA" "not financial advice" "DYOR" "do your own research")
     :hint "add 'NFA' or 'DYOR' when talking about price, tokens or returns")
    (:name "health"
     :label "Health and fitness claims"
     :forbidden (("\\b(cures?|heals?|treats?)\\b[^.!?\\n]{0,30}\\b(disease|cancer|diabetes|anxiety|depression)\\b"
                  . "medical cure claim")
                 ("\\b(lose|drop)\\s+\\d+\\s?(kg|lbs?|pounds)\\s+in\\s+\\d+\\s+(days?|weeks?)\\b"
                  . "specific rapid weight-loss promise")
                 ("\\bguaranteed results?\\b" . "guaranteed results"))
     :triggers "\\b(supplement|diet|weight|detox|cure|treatment|symptom)\\b"
     :disclaimers ("not medical advice" "consult a doctor" "consult your doctor")
     :hint "add 'not medical advice' when making health claims"))
  "The compliance packs, as the original's PACKS dict has them.")

(defun pack (name)
  "The pack called NAME, or NIL."
  (find (string-downcase (or name "")) +packs+
        :key (lambda (spec) (getf spec :name)) :test #'equal))

;;; --- names ------------------------------------------------------------------------

(defun brand-slug (name)
  "NAME as a profile's: lowercase letters, digits and single dashes, at most 48,
`brand' when nothing is left."
  (let* ((down (string-downcase (nlk:trimmed (princ-to-string (or name "")))))
         (slug (string-trim "-" (cl-ppcre:regex-replace-all "[^a-z0-9]+" down "-"))))
    (if (plusp (length slug)) (subseq slug 0 (min 48 (length slug))) "brand")))

;;; --- profile fields, read safe -----------------------------------------------------

(defun profile-field (profile key &optional default)
  "PROFILE's KEY, DEFAULT when it has none."
  (let ((value (getf profile key :+absent+)))
    (if (eq value :+absent+) default value)))

(defun string-list (value)
  "VALUE as a list of non-empty trimmed strings, whatever it arrived as."
  (let ((items (typecase value
                 (null nil)
                 (string (list value))
                 (vector (loop for item across value collect item))
                 (list (loop for item in value collect item))
                 (t (list (princ-to-string value))))))
    (remove-if-not (lambda (text) (plusp (length text)))
                   (mapcar (lambda (item)
                             (nlk:trimmed (if (stringp item) item (princ-to-string item))))
                           items))))

(defun pair-list (value)
  "VALUE -- a hash table of wrong -> right, an alist, or a plist -- as an
alist of (WRONG . RIGHT), the pairs whose two sides differ."
  (let ((pairs '()))
    (cond ((hash-table-p value)
           (maphash (lambda (wrong right)
                      (when (and (stringp wrong) (stringp right) (not (equal wrong right)))
                        (push (cons wrong right) pairs)))
                    value))
          ((and (listp value) (every #'consp value))
           (dolist (pair value)
             (when (and (stringp (car pair)) (stringp (cdr pair)) (not (equal (car pair) (cdr pair))))
               (push (cons (car pair) (cdr pair)) pairs))))
          ((and (listp value) (evenp (length value)))
           (loop for (wrong right) on value by #'cddr
                 when (and (stringp wrong) (stringp right) (not (equal wrong right)))
                   do (push (cons wrong right) pairs))))
    (nreverse pairs)))

(defun int-or (value default)
  "VALUE as an integer, DEFAULT when it is not one."
  (cond ((integerp value) value)
        ((and (stringp value) (every #'digit-char-p value) (plusp (length value)))
         (parse-integer value))
        (t default)))

(defun make-profile (&rest data)
  "DATA -- the rules a tool, a command or the store carries -- as one
normalized profile, its name a slug."
  (let ((profile (list :name (brand-slug (or (getf data :name) "brand"))
                        :display-name (nlk:trimmed (princ-to-string (or (getf data :display-name) "")))
                        :person (let ((person (getf data :person)))
                                  (if (member person '("third" "first" "any") :test #'equal)
                                      person "any"))
                        :tone (princ-to-string (or (getf data :tone) ""))
                        :banned (string-list (getf data :banned))
                        :preferred (pair-list (getf data :preferred))
                        :required (string-list (getf data :required))
                        :packs (remove-if-not #'pack (mapcar #'string-downcase
                                                              (string-list (getf data :packs))))
                        :max-chars (max 0 (int-or (getf data :max-chars) 0))
                        :max-hashtags (max -1 (int-or (getf data :max-hashtags) -1))
                        :max-emojis (max -1 (int-or (getf data :max-emojis) -1))
                        :max-exclamations (max -1 (int-or (getf data :max-exclamations) -1))
                        :examples (string-list (getf data :examples))
                        :notes (princ-to-string (or (getf data :notes) "")))))
    profile))

(defun profile-label (profile)
  "PROFILE as a person reads it."
  (let ((display (profile-field profile :display-name "")))
    (if (plusp (length display)) display (profile-field profile :name "brand"))))

;;; --- the brief: the rules as a block the model reads every turn ----------------

(defun profile-brief (profile)
  "PROFILE's rules as a compact instruction block, the text the cell puts in
every request while the brand is active."
  (let ((lines (list (format nil "Brand: ~a." (profile-label profile))))
        (person (profile-field profile :person "any"))
        (limits '()))
    (cond ((equal person "third")
           (push "Write about the brand in the THIRD person (use the brand name, 'the team', 'they'). Never 'we', 'us' or 'our'." lines))
          ((equal person "first")
           (push "Write as the brand in the first person plural ('we', 'our')." lines)))
    (let ((tone (profile-field profile :tone "")))
      (when (plusp (length tone))
        (push (format nil "Tone: ~a." tone) lines)))
    (push "Use only facts the user gave you. Never invent features, audits, partnerships, numbers or proof points. NFA and DYOR are disclaimers (not financial advice / do your own research), not products or tokens." lines)
    (let ((banned (profile-field profile :banned)))
      (when banned
        (push (format nil "Never use: ~{~s~^, ~}." banned) lines)))
    (let ((preferred (profile-field profile :preferred)))
      (when preferred
        (push (format nil "Spell these exactly: ~{~a~^, ~}."
                      (loop for (wrong . right) in preferred collect (format nil "~a (not ~a)" right wrong)))
              lines)))
    (let ((required (profile-field profile :required)))
      (when required
        (push (format nil "Every draft must include: ~{~s~^, ~}." required) lines)))
    (when (plusp (profile-field profile :max-chars 0))
      (push (format nil "at most ~d characters per draft" (profile-field profile :max-chars 0)) limits))
    (when (>= (profile-field profile :max-hashtags -1) 0)
      (push (format nil "at most ~d hashtags" (profile-field profile :max-hashtags -1)) limits))
    (when (>= (profile-field profile :max-emojis -1) 0)
      (push (format nil "at most ~d emoji" (profile-field profile :max-emojis -1)) limits))
    (when (>= (profile-field profile :max-exclamations -1) 0)
      (push (format nil "at most ~d exclamation marks" (profile-field profile :max-exclamations -1))
            limits))
    (when limits
      (push (format nil "Limits: ~{~a~^; ~}." (nreverse limits)) lines))
    (dolist (name (profile-field profile :packs))
      (let ((spec (pack name)))
        (when spec
          (push (format nil "Compliance (~a): no claim the pack forbids; ~a."
                        (getf spec :label) (getf spec :hint))
                lines))))
    (let ((examples (profile-field profile :examples)))
      (when examples
        (push (format nil "Tone examples (copy the voice only; never reuse their facts, numbers or claims, and never state anything the user did not give you): ~{~a~^ | ~}."
                      (subseq examples 0 (min 3 (length examples))))
              lines)))
    (let ((notes (profile-field profile :notes "")))
      (when (plusp (length notes))
        (push (format nil "Notes: ~a" notes) lines)))
    (format nil "~{~a~^ ~}" (nreverse lines))))

;;; --- the linter -------------------------------------------------------------------

(defun affirmed-p (pattern text)
  "PATTERN matches TEXT somewhere it is not negated: 'not risk-free' warns,
'risk-free' claims."
  (let ((positions (cl-ppcre:all-matches pattern text)))
    (loop for (start end) on positions by #'cddr
          for segment-start = (let ((dot (position #\. text :end start :from-end t))
                                    (newline (position #\Newline text :end start :from-end t)))
                                (1+ (max (or dot -1) (or newline -1))))
          unless (scan +negation+ (subseq text segment-start start))
            return t)))

(defun lint (text profile)
  "TEXT against PROFILE: a verdict plist, score 0-100, PASSED false when a
hard rule is broken."
  (let ((text (or text ""))
        (found '())
        (preferred (profile-field profile :preferred)))
    (flet ((add (rule severity detail)
             (push (list :rule rule :severity severity :detail detail) found)))
      ;; Voice and person.
      (let ((person (profile-field profile :person "any")))
        (cond ((equal person "third")
               (let ((hits (remove-duplicates
                            (cl-ppcre:all-matches-as-strings +first-person-plural+ text)
                            :test #'string-equal)))
                 (when hits
                   (add "person" +hard+
                        (format nil "first person (~{~a~^, ~}) in a third-person brand"
                                (mapcar #'string-downcase hits))))))
              ((equal person "first")
               (when (scan +first-person-singular+ text)
                 (add "person" +soft+ "singular 'I/me/my' in a brand that speaks as 'we'")))))
      ;; Vocabulary.
      (dolist (word (profile-field profile :banned))
        (when (scan (phrase-scanner word) text)
          (add "banned" +hard+ (format nil "banned word '~a'" word))))
      (dolist (pair preferred)
        (when (scan (spelling-scanner (car pair)) text)
          (add "spelling" +soft+ (format nil "'~a' should be '~a'" (car pair) (cdr pair)))))
      (dolist (phrase (profile-field profile :required))
        (unless (scan (phrase-scanner phrase) text)
          (add "required" +hard+ (format nil "missing required '~a'" phrase))))
      ;; Shape limits.
      (let ((max-chars (profile-field profile :max-chars 0)))
        (when (and (plusp max-chars) (> (length text) max-chars))
          (add "length" +hard+ (format nil "~d characters, limit is ~d" (length text) max-chars))))
      (let ((max (profile-field profile :max-hashtags -1)))
        (when (>= max 0)
          (let ((n (/ (length (cl-ppcre:all-matches +hashtag+ text)) 2)))
            (when (> n max)
              (add "hashtags" +soft+
                   (format nil "~d hashtag~:p, limit is ~d" n max))))))
      (let ((max (profile-field profile :max-emojis -1)))
        (when (>= max 0)
          (let ((n (count-emojis text)))
            (when (> n max)
              (add "emoji" +soft+ (format nil "~d emoji, limit is ~d" n max))))))
      (let ((max (profile-field profile :max-exclamations -1)))
        (when (>= max 0)
          (let ((n (count #\! text)))
            (when (> n max)
              (add "exclamations" +soft+
                   (format nil "~d exclamation mark~:p, limit is ~d" n max))))))
      ;; Compliance packs.
      (dolist (name (profile-field profile :packs))
        (let ((spec (pack name)))
          (when spec
            (dolist (claim (getf spec :forbidden))
              (when (affirmed-p (car claim) text)
                (add "compliance" +hard+ (format nil "~a: ~a" (getf spec :label) (cdr claim)))))
            (when (scan (getf spec :triggers) text)
              (unless (some (lambda (disclaimer) (scan (phrase-scanner disclaimer) text))
                            (getf spec :disclaimers))
                (add "disclaimer" +hard+
                     (format nil "~a: ~a" (getf spec :label) (getf spec :hint)))))))))
    (let* ((violations (nreverse found))
           (hard (count +hard+ violations :key (lambda (violation) (getf violation :severity))))
           (soft (count +soft+ violations :key (lambda (violation) (getf violation :severity))))
           (score (max 0 (- 100 (* 22 hard) (* 8 soft)))))
      (list :score score :passed (zerop hard) :violations violations))))

(defun verdict-summary (verdict)
  "VERDICT in one line, as /on-brand check answers it."
  (let ((violations (getf verdict :violations)))
    (if (null violations)
        (format nil "~d/100, on brand" (getf verdict :score))
        (format nil "~d/100: ~{~a~^; ~}" (getf verdict :score)
                (mapcar (lambda (violation) (getf violation :detail)) violations)))))

(defun mechanical-fix (text profile)
  "TEXT with the fixes that need no judgement: every exact spelling of the
brand's own names and terms."
  (let ((text (or text "")))
    (dolist (pair (profile-field profile :preferred))
      (let ((scanner (spelling-scanner (car pair))))
        (when scanner
          (setf text (cl-ppcre:regex-replace-all scanner text (cdr pair) :simple-calls t)))))
    text))
