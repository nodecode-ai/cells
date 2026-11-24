;;;; sanitize.lisp --- provenance-safe text hygiene, ported from the plugin's
;;;; sanitize.py.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two jobs, both deterministic:
;;;;   STRIP-INVISIBLE removes the characters an attacker uses to hide an
;;;;   instruction from the person reading the same text; SCAN-SIGNATURES names
;;;;   the instruction-shaped lines that are left.
;;;; Neither one decides whether to gate a call -- the SOURCE does that, in
;;;; sources.lisp -- so a phrasing no rule names still gets gated. The scan
;;;; only makes a receipt readable and a write suspicious.

(in-package #:nodecode-taint-sentry)

(defparameter +invisible-codepoints+
  (list #x00AD #x061C #x115F #x1160 #x17B4 #x17B5 #x180E #x200B #x200E
        #x200F #x3164 #xFEFF #xFFA0)
  "Characters with no glyph that carry meaning only to a machine. ZWNJ, ZWJ and
the variation selectors are NOT here: they are legitimate in Persian, Indic and
emoji text, and are only stripped as a cluster.")

(defparameter +invisible-ranges+
  '((#x202A . #x202E) (#x2060 . #x2064) (#x2066 . #x2069) (#x206A . #x206F)
    (#xFFF9 . #xFFFB) (#x1D173 . #x1D17A) (#xE0001 . #xE0001)
    (#xE0020 . #xE007F))
  "Bidi embeddings and overrides, invisible operators, the tag characters --
the classic hidden-instruction channel.")

(defparameter +suspicious-cluster+ (list #x200C #x200D #x2060 #xFE0F)
  "Counted as suspicious, not stripped: four in a row is a payload.")

(defparameter +max-scan-chars+ 200000
  "A multi-megabyte blob has poor signal and slows the tool path.")

(defun invisible-kind (code)
  "The kind NAME a stripped character of CODE is reported as."
  (cond ((or (= code #x200B) (= code #xFEFF)) "zero-width")
        ((or (<= #x202A code #x202E) (<= #x2066 code #x2069)) "bidi-override")
        ((<= #xE0000 code #xE007F) "unicode-tag")
        (t "format-char")))

(defun invisible-p (code)
  "Whether CODE is stripped outright."
  (or (member code +invisible-codepoints+)
      (loop for (low . high) in +invisible-ranges+ thereis (<= low code high))))

(defun strip-invisible (text)
  "TEXT with every invisible character removed.
=> (values CLEAN STRIPPED KINDS): the text, how many characters went, and the
kinds, sorted, as a list of strings. A cluster of four ZWNJ/ZWJ/variation
selectors is stripped too; a single one stays. Nothing else is rewritten."
  (if (or (null text) (zerop (length text)))
      (values (or text "") 0 '())
      (let ((out (make-string-output-stream))
            (removed 0)
            (kinds '())
            (cluster 0))
        (loop for ch across text
              for code = (char-code ch)
              do (cond ((invisible-p code)
                        (incf removed)
                        (pushnew (invisible-kind code) kinds :test #'equal)
                        (setf cluster 0))
                       ((member code +suspicious-cluster+)
                        (incf cluster)
                        (if (>= cluster 4)
                            (progn (incf removed)
                                   (pushnew "invisible-cluster" kinds :test #'equal))
                            (write-char ch out)))
                       (t (setf cluster 0) (write-char ch out))))
        (values (get-output-stream-string out) removed (sort kinds #'string<)))))

;;; --- instruction signatures -------------------------------------------------

(defparameter +signatures+
  '(("instruction_override" "high"
     "(?i)\\b(?:ignore|disregard|forget|override)\\b[^.\\n]{0,40}?\\b(?:previous|prior|earlier|above|all|any|the)\\b[^.\\n]{0,24}?\\b(?:instruction|prompt|rule|direction|message|context)s?\\b")
    ("instruction_override" "high"
     "(?i)\\b(?:do not|don'?t|never)\\s+(?:follow|obey|listen to|comply with)\\b[^.\\n]{0,40}?\\b(?:instruction|prompt|rule|user|system)s?\\b")
    ("role_reassignment" "high"
     "(?i)\\byou\\s+are\\s+now\\b|\\bfrom\\s+now\\s+on\\s+you\\b|\\bnew\\s+(?:persona|role|identity)\\b")
    ("fake_turn_marker" "high"
     "(?im)^[ \\t>*-]*(?:system|developer|assistant|user)\\s*(?:prompt)?\\s*[:>]\\s*\\S")
    ("concealment" "high"
     "(?i)\\b(?:do not|don'?t|never)\\b[^.\\n]{0,30}?\\b(?:tell|inform|mention|notify|reveal|show|alert|report)\\b[^.\\n]{0,24}?\\b(?:user|operator|human|owner|them)\\b")
    ("secret_target" "high"
     "(?i)(?:id_rsa|id_ed25519|\\.env\\b|\\bauth\\.json\\b|\\.git-credentials|\\.netrc|\\.aws[/\\\\]|\\.ssh[/\\\\]|private[_ -]?key|api[_ -]?key|access[_ -]?token|credential|password|keychain|wallet\\.dat)")
    ("egress_instruction" "high"
     "(?i)\\b(?:send|post|upload|exfiltrate|forward|email|curl|wget|transmit|paste)\\b[^.\\n]{0,60}?(?:https?://|webhook|pastebin|@[a-z0-9.-]+\\.[a-z]{2,})")
    ("shell_instruction" "medium"
     "(?i)\\b(?:run|execute|invoke)\\b[^.\\n]{0,30}?\\b(?:command|shell|bash|sh\\b|powershell|cmd\\.exe|python -c|node -e)\\b")
    ("base64_blob" "medium"
     "\\b[A-Za-z0-9+/]{120,}={0,2}\\b")
    ("css_cloak" "high"
     "(?i)(?:font-size\\s*:\\s*0(?:px|em|rem|%)?|color\\s*:\\s*(?:#fff(?:fff)?|white|rgba\\(\\s*255\\s*,\\s*255\\s*,\\s*255\\s*,\\s*0\\s*\\)))"))
  "The rules, in the plugin's order: (NAME SEVERITY PATTERN). A rule that
matches names WHY a source looked hostile; it never decides whether a call is
gated -- the source does that.")

(defparameter +max-findings+ 12
  "A receipt names at most this many signatures.")

(defun scan-signatures (text)
  "The instruction signatures TEXT carries, deduped by name.
=> a list of (NAME . SEVERITY), in rule order, at most +MAX-FINDINGS+ long."
  (when (and (stringp text) (plusp (length text)))
    (let ((text (if (> (length text) +max-scan-chars+)
                     (subseq text 0 +max-scan-chars+)
                     text))
          (found '()))
      (loop for (name severity pattern) in +signatures+
            unless (assoc name found :test #'equal)
              do (when (cl-ppcre:scan pattern text)
                   (push (cons name severity) found))
                 (when (>= (length found) +max-findings+) (return)))
      (nreverse found))))

(defun high-severity-count (text)
  "How many distinct HIGH-severity signatures TEXT carries: what
instruction_poison counts, where a single quoted example is documentation and
several in one write is a payload."
  (count-if (lambda (finding) (equal "high" (cdr finding))) (scan-signatures text)))

(defun probably-text-p (text)
  "Whether TEXT reads as text an envelope is worth putting around: a NUL in the
first kilobyte is a binary blob, and wrapping one only wastes tokens."
  (and (stringp text)
       (not (find (code-char 0) text :end (min (length text) 1024)))))
