;;;; guard.lisp --- the rules: dashes, AI phrases, du/Sie. Text in, a report out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reads a setting or touches the organism, which is why the
;;;; tests can read every rule directly. The plugin's guard.py is the
;;;; authority for what counts: code fences, inline code, URLs, markdown link
;;;; targets, MEDIA lines and (optionally) quoted lines are replaced by
;;;; placeholders before any rule runs and put back afterwards, so nothing
;;;; inside them is counted and nothing inside them is changed.

(in-package #:nodecode-writing-guard)

;;; --- the dashes, and the spans no rule may touch -----------------------------

(defparameter +dash-chars+ (list (code-char #x2012) (code-char #x2013)
                                  (code-char #x2014) (code-char #x2015))
  "Figure dash, en dash, em dash and horizontal bar: what the original counted.")

(defparameter +dash-class+ (format nil "[~{~a~}]" +dash-chars+))

(defparameter +word-class+ "0-9A-Za-zÄÖÜäöüßÀ-ÿ"
  "What the original called a word character: a dash between two of them with
no space around it is a range (Mo-Fr) or a compound, not a pause.")

(defparameter +placeholder-open+ (code-char #xe000))
(defparameter +placeholder-close+ (code-char #xe001))

(defparameter +max-chars+ 200000
  "Longer text is not checked, as the original did: an answer that long is a
document, and the rules would cost more than they say.")

(defparameter +fence-re+ (cl-ppcre:create-scanner "^[ ]{0,3}(`{3,}|~{3,})"))
(defparameter +inline-code-re+ (cl-ppcre:create-scanner "(`+)(?!`).+?(?<!`)\\1"))
(defparameter +md-link-target-re+ (cl-ppcre:create-scanner "\\]\\([^()\\s]+\\)"))
(defparameter +url-re+ (cl-ppcre:create-scanner
                         "(?:https?://|www\\.)[^\\s<>()\\[\\]]+|<[^<>\\s@]+@[^<>\\s]+>"))
(defparameter +placeholder-re+
  (cl-ppcre:create-scanner (format nil "~c(\\d+)~c" +placeholder-open+ +placeholder-close+)))

(defun split-lines (text)
  "TEXT as its lines, each keeping the newline it ended with."
  (let ((lines '()) (start 0))
    (dotimes (i (length text))
      (when (char= (char text i) #\Newline)
        (push (subseq text start (1+ i)) lines)
        (setf start (1+ i))))
    (when (< start (length text)) (push (subseq text start) lines))
    (nreverse lines)))

(defun line-body (line)
  "LINE without its trailing newline."
  (string-right-trim (list #\Newline #\Return) line))

(defun placeholder (index)
  (format nil "~c~d~c" +placeholder-open+ index +placeholder-close+))

(defun mask (text &key (skip-quotes t))
  "TEXT with every protected span replaced by a placeholder.
=> (values MASKED SAVED), SAVED the spans in the order they were taken out."
  (let ((saved '()))
    (labels ((keep (string)
               (push string saved)
               (placeholder (1- (length saved)))))
      (let ((out (make-string-output-stream)) (fence nil))
        (dolist (line (split-lines text))
          (let* ((body (line-body line))
                 (newline (subseq line (length body)))
                 (open (let ((registers (nth-value 1 (cl-ppcre:scan-to-strings +fence-re+ body))))
                         (and registers (aref registers 0)))))
            (cond
              ;; a fenced block is kept whole, its closing marker with it
              (fence
               (write-string (keep body) out)
               (when (and open
                          (char= (char open 0) (char fence 0))
                          (>= (length open) (length fence))
                          (zerop (length (nlk:trimmed (subseq body (length open))))))
                 (setf fence nil)))
              ((zerop (length (nlk:trimmed body)))
               (write-string body out))
              (open
               (setf fence open)
               (write-string (keep body) out))
              (t
               (let ((stripped (nlk:trimmed body)))
                 (if (or (uiop:string-prefix-p "MEDIA:" stripped)
                         (and skip-quotes (plusp (length stripped))
                              (char= #\> (char stripped 0))))
                     (write-string (keep body) out)
                     ;; cl-ppcre calls a replacement function with the whole target and the
                     ;; match's bounds: the span is a subseq of the target, not an argument.
                     (let ((working (cl-ppcre:regex-replace-all
                                     +inline-code-re+ body
                                     (lambda (target target-start target-end match-start match-end
                                               &rest rest)
                                       (declare (ignore target-start target-end rest))
                                       (keep (subseq target match-start match-end))))))
                       ;; a markdown link's target is a URL: keep the bracket, drop the rest
                       (setf working
                             (cl-ppcre:regex-replace-all
                              +md-link-target-re+ working
                              (lambda (target target-start target-end match-start match-end
                                        &rest rest)
                                (declare (ignore target-start target-end rest))
                                (concatenate 'string "]"
                                             (keep (subseq target (1+ match-start) match-end))))))
                       (setf working
                             (cl-ppcre:regex-replace-all
                              +url-re+ working
                              (lambda (target target-start target-end match-start match-end
                                        &rest rest)
                                (declare (ignore target-start target-end rest))
                                (keep (subseq target match-start match-end)))))
                       (write-string working out))))))
            (write-string newline out)))
        (values (get-output-stream-string out) (nreverse saved))))))

(defun unmask (masked saved)
  "MASKED with its placeholders put back as SAVED holds them."
  (with-output-to-string (out)
    (loop with start = 0
          for (from to starts ends) =
            (multiple-value-list (cl-ppcre:scan +placeholder-re+ masked :start start))
          while from
          do (write-string masked out :start start :end from)
             (let ((digits (if starts (subseq masked (aref starts 0) (aref ends 0)) "")))
               (write-string (if (plusp (length digits))
                                 (or (nth (parse-integer digits) saved) "")
                                 "")
                             out))
             (setf start to)
          finally (write-string masked out :start start))))

(defun prose-of (masked)
  "MASKED with every placeholder read as a neutral word, for detection only."
  (cl-ppcre:regex-replace-all +placeholder-re+ masked " x "))

;;; --- the language ------------------------------------------------------------

(defparameter +de-words+
  '("der" "die" "das" "und" "ist" "nicht" "ich" "du" "sie" "wir" "ihr" "es" "ein" "eine"
    "einen" "dem" "den" "des" "zu" "mit" "für" "auf" "von" "im" "in" "sind" "war" "hat"
    "haben" "wird" "werden" "kann" "können" "auch" "noch" "nur" "oder" "aber" "wie" "wenn"
    "dass" "sich" "bei" "nach" "aus" "um" "über" "unter" "vor" "dir" "dich" "dein" "deine"
    "ihnen" "ihre" "mein" "meine" "uns" "an" "einem" "einer" "eines" "kein" "keine" "man"
    "so" "schon" "sehr" "hier" "jetzt" "dann" "also" "doch" "mal" "gibt" "bitte" "danke"
    "heute" "seite" "seiten" "uhr" "bis" "zum" "zur" "am" "vom")
  "The words the original counted towards German.")

(defparameter +en-words+
  '("the" "and" "is" "are" "was" "were" "be" "been" "to" "of" "in" "on" "for" "with"
    "that" "this" "it" "you" "your" "we" "they" "he" "she" "not" "but" "or" "as" "at"
    "by" "from" "have" "has" "had" "will" "would" "can" "could" "should" "an" "a" "which"
    "what" "there" "their" "our" "my" "me")
  "The words the original counted towards English.")

(defun words-of (text)
  "TEXT's words, letters and apostrophes only, as the original's pattern read them."
  (let ((words '()) (start nil))
    (dotimes (i (length text))
      (if (or (alpha-char-p (char text i)) (char= (char text i) #\'))
          (unless start (setf start i))
          (when start (push (subseq text start i) words) (setf start nil))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun detect-language (text)
  "The language of TEXT, \"de\" or \"en\", by the original's word count: a
German word or an umlaut is worth two points towards German."
  (let* ((words (words-of (string-downcase text)))
         (de (+ (count-if (lambda (word) (member word +de-words+ :test #'string=)) words)
                (* 2 (count-if (lambda (word) (some (lambda (c) (find c word)) "äöüß")) words))))
         (en (count-if (lambda (word) (member word +en-words+ :test #'string=)) words)))
    (if (and (>= de 2) (> de en)) "de" "en")))

;;; --- the dashes --------------------------------------------------------------

(defun count-dashes (text)
  "How many dashes TEXT carries."
  (count-if (lambda (character) (find character +dash-chars+)) text))

(defun scanner (pattern)
  (cl-ppcre:create-scanner pattern :multi-line-mode t))

(defparameter +bullet-re+ (scanner (format nil "^([ \\t]*)~a[ \\t]+" +dash-class+)))
(defparameter +lead-re+ (scanner (format nil "^([ \\t]*)~a(?=\\S)" +dash-class+)))
(defparameter +cell-re+ (scanner (format nil "(?<=\\|)[ \\t]{0,16}~a[ \\t]{0,16}(?=\\|)" +dash-class+)))
(defparameter +num-range-re+
  (scanner (format nil "(?<=\\d)[ \\t]{0,16}~a[ \\t]{0,16}(?=\\d)" +dash-class+)))
(defparameter +range-re+
  (scanner (format nil "(?<=[~a.])[~c~c](?=[~a])"
                    +word-class+ (code-char #x2012) (code-char #x2013) +word-class+)))
(defparameter +end-re+ (scanner (format nil "[ \\t]{0,16}~a{1,16}[ \\t]{0,16}$" +dash-class+)))
(defparameter +before-punct-re+
  (scanner (format nil "[ \\t]{0,16}~a{1,16}[ \\t]{0,16}(?=[,.;:!?)\\]\\\"“”»])" +dash-class+)))
(defparameter +after-open-re+
  (scanner (format nil "(?<=[(\\[„“\\\"«])[ \\t]{0,16}~a{1,16}[ \\t]{0,16}" +dash-class+)))
(defparameter +spaced-re+ (scanner (format nil "[ \\t]{0,16}~a{1,16}[ \\t]{0,16}" +dash-class+)))

(defun replace-spaced (text separator)
  "Every remaining dash in TEXT replaced by SEPARATOR, or by one space where
the dash stood after a comma, a semicolon, a colon or a full stop."
  (with-output-to-string (out)
    (loop with start = 0
          for (from to) = (multiple-value-list (cl-ppcre:scan +spaced-re+ text :start start))
          while from
          do (write-string text out :start start :end from)
             (let ((before (string-right-trim
                            (list #\Space #\Tab)
                            (subseq text (max 0 (- from 8)) from))))
               (write-string (if (and (plusp (length before))
                                       (find (char before (1- (length before))) ",;:.!?"))
                                 " "
                                 separator)
                             out))
             (setf start to)
          finally (write-string text out :start start))))

(defun fix-dashes (text &key (style "comma"))
  "TEXT with every dash replaced by rule: a list bullet becomes \"- \", a
number range (10-20) or a compound (Mo-Fr) keeps a hyphen, a table cell
becomes \" - \", and a dash between words becomes STYLE's separator, a comma
and a space by default or a spaced hyphen.
=> (values FIXED COUNT), COUNT the dashes there were."
  (let ((count (count-dashes text)))
    (if (zerop count)
        (values text 0)
        (let ((separator (if (string= style "hyphen") " - " ", "))
              (working (cl-ppcre:regex-replace-all +bullet-re+ text "\\1- ")))
          (setf working (cl-ppcre:regex-replace-all +lead-re+ working "\\1"))
          (setf working (cl-ppcre:regex-replace-all +cell-re+ working " - "))
          (setf working (cl-ppcre:regex-replace-all +num-range-re+ working "-"))
          (setf working (cl-ppcre:regex-replace-all +range-re+ working "-"))
          (setf working (cl-ppcre:regex-replace-all +end-re+ working ""))
          (setf working (cl-ppcre:regex-replace-all +before-punct-re+ working ""))
          (setf working (cl-ppcre:regex-replace-all +after-open-re+ working ""))
          (setf working (replace-spaced working separator))
          (values working count)))))

;;; --- the stock phrases -------------------------------------------------------

(defparameter +phrases+
  '(;; German
    ("de.wichtig_zu_beachten" "de" "\\bes ist (?:wichtig|erwähnenswert),? zu (?:beachten|betonen|erwähnen)\\b")
    ("de.zusammenfassend" "de" "\\b(?:zusammenfassend|abschließend) (?:lässt sich|kann man) sagen\\b")
    ("de.heutige_welt" "de" "\\bin der heutigen (?:schnelllebigen |digitalen |modernen |vernetzten )?(?:welt|zeit|gesellschaft)\\b")
    ("de.hoffe_hilft" "de" "\\bich hoffe,? (?:das|dies|diese (?:informationen|antwort|übersicht))(?: \\w+)? (?:hilft|helfen)\\b")
    ("de.lass_mich_wissen" "de" "\\b(?:lass(?:t)? mich|lassen sie mich) (?:gerne |einfach )?wissen,? (?:ob|wenn|falls)\\b")
    ("de.gerne_helfe_ich" "de" "\\b(?:gerne helfe ich|ich helfe (?:dir|ihnen|euch) gerne)\\b")
    ("de.eintauchen" "de" "\\b(?:tauchen wir (?:ein|tiefer|gemeinsam)|lass(?:t)? uns (?:tiefer |gemeinsam )?eintauchen|eintauchen in die (?:welt|thematik))\\b")
    ("de.gute_frage" "de" "\\b(?:das ist eine )?(?:sehr |wirklich )?(?:gute|großartige|tolle|spannende|ausgezeichnete) frage[!.]")
    ("de.als_ki" "de" "\\bals (?:eine? )?(?:ki|künstliche intelligenz|ki-sprachmodell|sprachmodell)(?:-assistent)?,? (?:kann|habe|bin) ich\\b")
    ("de.nahtlos" "de" "\\bnahtlos(?:e|en|er|es)?\\b")
    ("de.game_changer" "de" "\\b(?:ein |der |echter )*game[- ]?changer\\b")
    ("de.naechstes_level" "de" "\\bauf (?:ein neues|das nächste) (?:level|niveau)\\b|\\bauf die nächste stufe (?:heben|bringen)\\b")
    ("de.volles_potenzial" "de" "\\b(?:das|ihr|sein|dein) (?:volles|ganzes) potenzial (?:entfalten|ausschöpfen|freisetzen)\\b")
    ("de.spannende_reise" "de" "\\b(?:auf (?:dieser|deiner|ihrer|einer) (?:spannenden )?reise|eine spannende reise)\\b")
    ("de.entscheidende_rolle" "de" "\\bspielt eine (?:entscheidende|zentrale|wichtige|maßgebliche) rolle\\b")
    ("de.von_bedeutung" "de" "\\bvon (?:entscheidender|zentraler|großer) bedeutung\\b")
    ("de.revolution" "de" "\\b(?:revolutioniert|revolutionieren|revolutionär(?:e|en|er|es)?)\\b")
    ("de.im_folgenden" "de" "\\bim folgenden (?:werde ich|erkläre ich|zeige ich|gehe ich)\\b")
    ("de.zweifellos" "de" "\\b(?:zweifellos|zweifelsohne|ohne zweifel)\\b")
    ("de.nicht_vergessen" "de" "\\bnicht zu vergessen\\b")
    ("de.ganzheitlich" "de" "\\bganzheitlich(?:e|en|er|es)? (?:ansatz|lösung|betrachtung)\\b")
    ("de.mehrwert_bieten" "de" "\\b(?:echten |einen )?mehrwert (?:bieten|liefern|schaffen)\\b")
    ("de.hier_ist_ueberblick" "de" "\\bhier (?:ist|sind) (?:ein )?(?:umfassende[rs]?|detaillierte[rs]?|ausführliche[rs]?) (?:überblick|übersicht|leitfaden|anleitung)\\b")
    ("de.zu_guter_letzt" "de" "\\bzu guter letzt\\b")
    ("de.wertvolle_einblicke" "de" "\\bwertvolle (?:einblicke|erkenntnisse|tipps)\\b")
    ;; English
    ("en.important_to_note" "en" "\\bit(?:'s| is) (?:important|worth|crucial) (?:to note|noting|to remember|to mention)\\b")
    ("en.todays_world" "en" "\\bin today'?s (?:fast-paced |digital |ever-changing |modern |rapidly evolving )?(?:world|landscape|age|era)\\b")
    ("en.hope_helps" "en" "\\bi hope (?:this|that|these|this information) (?:helps|was helpful|is helpful)\\b")
    ("en.let_me_know" "en" "\\b(?:let me know if you(?:'d| would)? (?:like|have|need|want)|feel free to (?:ask|reach out|let me know))\\b")
    ("en.great_question" "en" "\\b(?:what a |that'?s a |such a )?(?:great|excellent|fantastic|good) question[!.]")
    ("en.certainly" "en" "^[ \\t]*(?:certainly|absolutely|of course)!")
    ("en.as_an_ai" "en" "\\bas an ai(?: language model| assistant)?\\b")
    ("en.delve" "en" "\\bdelv(?:e|es|ed|ing)\\b")
    ("en.tapestry" "en" "\\b(?:rich |vibrant |intricate )?tapestry\\b")
    ("en.testament" "en" "\\b(?:is |stands as )?a testament to\\b")
    ("en.navigate" "en" "\\bnavigat(?:e|ing) the (?:complexities|complex|intricacies|landscape|world)\\b")
    ("en.game_changer" "en" "\\bgame[- ]?changer\\b")
    ("en.unlock_potential" "en" "\\b(?:unlock|unleash|harness)(?:ing|es|s)? the (?:full )?(?:power|potential)\\b")
    ("en.seamless" "en" "\\bseamless(?:ly)?\\b")
    ("en.in_conclusion" "en" "\\b(?:in conclusion|to sum up|in summary),")
    ("en.dive_in" "en" "\\blet'?s dive (?:in|into|deeper)\\b|\\b(?:deep|deeper) dive into\\b")
    ("en.crucial_role" "en" "\\bplays? a (?:crucial|pivotal|vital|key|significant) role\\b")
    ("en.ever_evolving" "en" "\\bever-(?:evolving|changing|growing) (?:landscape|world|field)\\b")
    ("en.embark" "en" "\\bembark(?:ing)? on (?:a|an|your|this)\\b")
    ("en.elevate" "en" "\\belevate (?:your|the|our)\\b")
    ("en.happy_to_help" "en" "\\bi'?d be (?:happy|glad|delighted) to help\\b")
    ("en.realm" "en" "\\bin the realm of\\b")
    ("en.moreover" "en" "(?:^|\\n|\\. )(?:moreover|furthermore),")
    ("en.not_only_but" "en" "\\bit'?s not (?:just|only) about \\w+(?: \\w+)?,? it'?s about\\b")
    ("en.robust_solution" "en" "\\b(?:robust|comprehensive|holistic) (?:solution|approach|framework) (?:that|to)\\b")
    ("en.valuable_insights" "en" "\\bvaluable (?:insights|tips|information)\\b")
    ("en.here_is_overview" "en" "\\bhere(?:'s| is) a (?:comprehensive|detailed|thorough) (?:overview|guide|breakdown)\\b")
    ("en.underscore" "en" "\\bunderscores? the (?:importance|need)\\b")
    ("en.at_the_end_of_day" "en" "\\bat the end of the day\\b"))
  "The stock phrases the original carried: id, language and pattern. A
pattern runs case-insensitively, multi-line, on the prose only -- code, URLs,
MEDIA lines and quotes are masked out first. Keep one specific: a phrase
people say in normal writing teaches the reader to ignore the report.")

(defparameter +compiled-phrases+
  (mapcar (lambda (rule)
            (list (first rule) (second rule)
                  (cl-ppcre:create-scanner (third rule)
                                            :case-insensitive-mode t
                                            :multi-line-mode t)))
          +phrases+)
  "The table compiled once, at load: the check runs on every answer.")

(defun find-phrases (prose &key extra ignore)
  "What in PROSE reads as a stock phrase: a list of (:id ID :text TEXT :count N).
EXTRA are the operator's own phrases, plain text, matched whole and case
insensitively; IGNORE holds rule ids or the exact phrase."
  (let ((ignored (remove "" (mapcar (lambda (word) (string-downcase (nlk:trimmed word)))
                                    (or ignore '()))
                         :test #'string=))
        (hits (make-hash-table :test #'equal))
        (order '()))
    (labels ((scan (id pattern)
               (unless (member (string-downcase id) ignored :test #'string=)
                 (cl-ppcre:do-matches (start end pattern prose)
                   (let ((found (string-trim " ,.!:"
                                              (subseq prose start (min end (length prose))))))
                     (unless (member (string-downcase found) ignored :test #'string=)
                       (let ((hit (gethash id hits)))
                         (if hit
                             (incf (getf hit :count))
                             (progn (setf (gethash id hits)
                                          (list :id id
                                                :text (subseq found 0 (min 60 (length found)))
                                                :count 1))
                                    (push id order))))))))))
      (dolist (rule +compiled-phrases+) (scan (first rule) (third rule)))
      (loop for phrase in (or extra '())
            for index from 1
            for clean = (nlk:trimmed phrase)
            when (plusp (length clean))
              do (scan (format nil "custom.~d" index)
                       (cl-ppcre:create-scanner
                        (format nil "(?<!\\w)~a(?!\\w)" (cl-ppcre:quote-meta-chars clean))
                        :case-insensitive-mode t))))
    (loop for id in (nreverse order) collect (gethash id hits))))

;;; --- du and Sie --------------------------------------------------------------

(defparameter +informal-re+
  (cl-ppcre:create-scanner
   "\\b(?:du|dich|dir|dein|deine|deinem|deinen|deiner|deines|euch|euer|eure|eurem|euren|eurer|eures)\\b"
   :case-insensitive-mode t)
  "The informal forms, in any case: \"Du\" and \"du\" both address.")

(defparameter +formal-re+
  (cl-ppcre:create-scanner "\\b(?:Sie|Ihnen|Ihr|Ihre|Ihrem|Ihren|Ihrer|Ihres)\\b")
  "The formal forms, capitalised as German spells them.")

(defparameter +lead-chars+ " \t*_\"'„“”»«‚‘(#>-•+")

(defparameter +meta-re+
  (cl-ppcre:create-scanner
   (concatenate 'string
                "\\b(?:du|Du|DU)\\s*(?:/|oder|und|bzw\\.?|vs\\.?|statt|or|and)\\s*(?:Sie|SIE)\\b"
                "|\\b(?:Sie|SIE)\\s*(?:/|oder|und|bzw\\.?|vs\\.?|statt|or|and)\\s*(?:du|Du|DU)\\b"
                "|\\b(?:du|Du|Sie|Ihr|Ihre|Ihnen)-(?=\\w)"
                "|\\b(?:per|das|beim|ins|zum|aufs)\\s+(?:du|Du|Sie)\\b"
                "|[„“\\\"'‚‘»«](?:du|Du|Sie|Ihnen|Ihr|dein|dich|dir)[“”\\\"'‘’«»]"))
  "Talking about the forms is not using them: \"du/Sie\", \"Sie-Form\", a
quoted „du` and \"per du\" are set aside before anything is counted.")

(defparameter +inline-quote-re+
  (cl-ppcre:create-scanner "„[^“”\n]{1,300}[“”]|\\\"[^\\\"\n]{1,300}\\\"|»[^«\n]{1,300}«|«[^»\n]{1,300}»")
  "Someone else's words, quoted inline, are not the answer's own address.")

(defun sentence-start-p (text position)
  "Whether POSITION in TEXT stands where a sentence starts, which is where a
capital \"Sie\" is the pronoun and not the formal address."
  (let* ((start (max 0 (- position 80)))
         (before (string-right-trim +lead-chars+ (subseq text start position))))
    (cond
      ((zerop (length before)) (zerop start))
      (t
       (let ((trimmed (string-right-trim (list #\Space #\Tab) before)))
         (if (zerop (length trimmed))
             t
             (or (not (null (cl-ppcre:scan "(?m)(?:^|\\n)[ \\t]*\\d+[.)]$" trimmed)))
                 (not (null (find (char trimmed (1- (length trimmed))) ".!?:|"))))))))))

(defun address-counts (prose)
  "How many times PROSE addresses informally and formally => (values DU SIE)."
  (let ((text (cl-ppcre:regex-replace-all
               +inline-quote-re+
               (cl-ppcre:regex-replace-all +meta-re+ prose " ")
               " "))
        (du 0)
        (sie 0))
    (cl-ppcre:do-matches (start end +informal-re+ text)
      (declare (ignore end))
      (incf du))
    (cl-ppcre:do-matches (start end +formal-re+ text)
      (declare (ignore end))
      (unless (sentence-start-p text start) (incf sie)))
    (values du sie)))

(defun mixed-paragraphs (prose)
  "The paragraphs of PROSE that use both forms, as a list of (DU . SIE)."
  (let ((mixed '()))
    (dolist (paragraph (cl-ppcre:split "\\n[ \\t]*\\n" prose))
      (multiple-value-bind (du sie) (address-counts paragraph)
        (when (and (plusp du) (plusp sie)) (push (cons du sie) mixed))))
    (nreverse mixed)))

;;; --- the report --------------------------------------------------------------

(defstruct (report (:constructor make-report))
  (language "en")
  (dashes 0)
  (dashes-fixed 0)
  (phrases nil)
  (du 0)
  (sie 0)
  (mixed nil)
  (form nil)
  (expected nil)
  (switched-from nil)
  (text nil))

(defun report-wrong-form (report)
  "How many addresses are the wrong one, when the operator fixed the form."
  (let ((expected (report-expected report)))
    (cond ((equal expected "du") (report-sie report))
          ((equal expected "sie") (report-du report))
          (t 0))))

(defun report-findings-p (report)
  "Whether REPORT has anything to say."
  (not (null (or (plusp (report-dashes report))
                 (plusp (report-dashes-fixed report))
                 (report-phrases report)
                 (report-mixed report)
                 (plusp (report-wrong-form report))
                 (report-switched-from report)))))

(defun check-text (text &key fix (dash-style "comma") (check-dashes t) (check-phrases t)
                              (check-address t) (address "auto") extra-phrases ignore-phrases
                              (skip-quotes t) previous-form (track-switches t))
  "TEXT checked the way the plugin checked an answer => a REPORT. With FIX
the dashes are replaced by rule and the fixed text is the report's TEXT."
  (let ((text (or text "")))
    (if (> (length text) +max-chars+)
        (make-report)
        (multiple-value-bind (masked saved) (mask text :skip-quotes skip-quotes)
          (let ((prose (prose-of masked))
                (report (make-report :language (detect-language (prose-of masked)))))
            (when check-dashes
              (if fix
                  (multiple-value-bind (fixed count) (fix-dashes masked :style dash-style)
                    (setf (report-dashes-fixed report) count)
                    (when (plusp count)
                      (setf (report-text report) (unmask fixed saved)
                            prose (prose-of fixed))))
                  (setf (report-dashes report) (count-dashes masked))))
            (when check-phrases
              (setf (report-phrases report)
                    (find-phrases prose :extra extra-phrases :ignore ignore-phrases)))
            (when (and check-address (string= (report-language report) "de"))
              (multiple-value-bind (du sie) (address-counts prose)
                (setf (report-du report) du (report-sie report) sie
                      (report-mixed report) (mixed-paragraphs prose)
                      (report-form report) (cond ((and (plusp du) (plusp sie)) "mixed")
                                                 ((plusp du) "du")
                                                 ((plusp sie) "sie"))))
              (when (member address '("du" "sie") :test #'string=)
                (setf (report-expected report) address))
              (when (and track-switches
                         (member previous-form '("du" "sie") :test #'string=)
                         (member (report-form report) '("du" "sie") :test #'string=)
                         (not (string= (report-form report) previous-form)))
                (setf (report-switched-from report) previous-form)))
            report)))))

;;; --- the one line the reader sees --------------------------------------------

(defun plural (count one many)
  (format nil "~d ~a" count (if (= count 1) one many)))

(defun quoted (text language)
  (let ((clean (string-trim " " (substitute #\Space #\_ (remove #\* text)))))
    (if (string= language "de")
        (format nil "„~a“" clean)
        (format nil "\"~a\"" clean))))

(defun footer-line (report &key language)
  "One italic line summarising REPORT, or \"\" when there is nothing to say.
LANGUAGE names the report's language; the answer's own when NIL."
  (if (not (report-findings-p report))
      ""
      (let* ((language (or language (report-language report)))
             (de (string= language "de"))
             (parts '()))
        (when (plusp (report-dashes-fixed report))
          (push (if de
                    (plural (report-dashes-fixed report) "Gedankenstrich ersetzt"
                            "Gedankenstriche ersetzt")
                    (plural (report-dashes-fixed report) "dash replaced" "dashes replaced"))
                parts))
        (when (and (zerop (report-dashes-fixed report)) (plusp (report-dashes report)))
          (push (if de (plural (report-dashes report) "Gedankenstrich" "Gedankenstriche")
                    (plural (report-dashes report) "dash" "dashes"))
                parts))
        (when (report-phrases report)
          (let* ((all (report-phrases report))
                 (total (reduce #'+ all :key (lambda (hit) (getf hit :count))))
                 (shown (format nil "~{~a~^, ~}"
                                (loop for hit in (subseq all 0 (min 3 (length all)))
                                      collect (quoted (getf hit :text) language))))
                 (more (- (length all) 3)))
            (when (plusp more) (setf shown (format nil "~a +~d" shown more)))
            (push (format nil "~a (~a)"
                          (if de (plural total "KI-Floskel" "KI-Floskeln")
                              (plural total "AI phrase" "AI phrases"))
                          shown)
                  parts)))
        (when (report-mixed report)
          (let ((du (reduce #'+ (report-mixed report) :key #'car))
                (sie (reduce #'+ (report-mixed report) :key #'cdr)))
            (push (if de
                      (format nil "du und Sie im selben Absatz gemischt (du ~d, Sie ~d)" du sie)
                      (format nil "du and Sie mixed in one paragraph (du ~d, Sie ~d)" du sie))
                  parts)))
        (let ((wrong (report-wrong-form report)))
          (cond
            ((and (plusp wrong) (report-expected report))
             (let ((wanted (if (string= (report-expected report) "du") "du" "Sie"))
                   (other (if (string= (report-expected report) "du") "Sie" "du")))
               (push (if de (format nil "~dx ~a statt ~a" wrong other wanted)
                         (format nil "~dx ~a instead of ~a" wrong other wanted))
                     parts)))
            ((and (report-switched-from report) (null (report-mixed report)))
             (let ((before (if (string= (report-switched-from report) "du") "du" "Sie"))
                   (now (if (string= (or (report-form report) "du") "du") "du" "Sie")))
               (push (if de (format nil "Anrede gewechselt: vorher ~a, jetzt ~a" before now)
                         (format nil "form of address switched: ~a before, ~a now" before now))
                     parts)))))
        (format nil "*writing-guard: ~{~a~^; ~}.*" (nreverse parts)))))
