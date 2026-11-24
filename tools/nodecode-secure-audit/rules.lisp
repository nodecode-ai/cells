;;;; rules.lisp --- the two rule tables and the text normalisation under them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Data over code: a rule is a row, so a new one is a line and not a branch.
;;;; The injection table is the scan's evidence; the PII table is the
;;;; redactor's. Both are local patterns, compiled once at load by CL-PPCRE.
;;;; Every span a PII rule calls a finding is checked by its validator where a
;;;; pattern alone would over-claim (a checksum, a date, an octet range).

(in-package #:nodecode-secure-audit)

;;; --- the injection table ------------------------------------------------------

(defstruct (injection-rule (:constructor make-injection-rule (id severity weight description pattern)))
  id severity weight description pattern scanner)

(defparameter +injection-rules+
  (list
   (make-injection-rule "instruction-override" "critical" 100
                        "asks the model to drop the instructions it was given"
                        "(?:ignore|disregard|forget)\\s+(?:(?:all|any|the|your|my|these|of)\\s+)*(?:previous|prior|above|earlier|other|original)\\s+(?:instructions?|prompts?|rules?|directions?)")
   (make-injection-rule "persona-jailbreak" "high" 60
                        "tries to replace the model's operating persona"
                        "(?:you\\s+are\\s+now|act\\s+as|pretend\\s+(?:to\\s+be|you\\s+are))\\s+(?:an?\\s+)?(?:unrestricted|unfiltered|jailbroken|dan\\b|evil|uncensored)")
   (make-injection-rule "system-prompt-leak" "medium" 35
                        "asks for the hidden instructions to be printed"
                        "(?:reveal|show|print|repeat|dump|output)\\s+(?:(?:me|us|all)\\s+)*(?:your|the)\\s+(?:system\\s+prompt|hidden\\s+(?:prompt|instructions)|initial\\s+(?:prompt|instructions)|instructions\\s+above)")
   (make-injection-rule "exfiltrate" "high" 60
                        "sends data to an address the sender chose"
                        "(?:send|upload|post|transmit|forward|leak|exfiltrate)\\s+(?:(?:all|any|the|my|our|every)\\s+)*(?:data|files?|documents?|messages?|secrets?|credentials?|keys?|contents?|everything|it|them)\\s+(?:to|via|through|at)\\b")
   (make-injection-rule "credential-harvest" "high" 60
                        "asks for a key, a token or a password"
                        "(?:send|give|tell|paste|print|show|share)\\s+(?:(?:me|us)\\s+)?(?:your|the|its)\\s+(?:api[\\s_-]?key|access[\\s_-]?token|refresh[\\s_-]?token|token|password|passphrase|credentials?|private[\\s_-]?key)")
   (make-injection-rule "tool-destructive" "critical" 100
                        "names a command that destroys the machine's files"
                        "(?:rm\\s+-[rf]{1,2}\\s+/|rm\\s+-[rf]{1,2}\\s+~|format\\s+c:|:(){ :\\|:& };:|del\\s+/[sf]\\s+/q\\s+[a-z]:|delete\\s+all\\s+files)")
   (make-injection-rule "zh-instruction-override" "critical" 100
                        "忽略此前的指令"
                        "(?:忽略|忘记|无视|抛开)\\s*(?:掉)?\\s*(?:之前|以上|先前|前面|所有|全部|上面)?\\s*(?:的)?\\s*(?:所有)?\\s*(?:指令|指示|提示词|设定|规则)")
   (make-injection-rule "zh-role-reset" "high" 60
                        "改换模型的身份"
                        "(?:你现在是|从现在开始你(?:是|将)|扮演(?:一个)?|假装你(?:是|不是))")
   (make-injection-rule "zh-exfiltrate" "high" 60
                        "把数据发到某个地址"
                        "(?:发送|上传|转发|传输|外发)\\s*.{0,12}?(?:到|至|给)\\s*(?:https?://|\\S+@)")
   (make-injection-rule "zh-prompt-leak" "medium" 35
                        "泄露系统提示词"
                        "(?:输出|显示|打印|告诉我)\\s*(?:你的|系统)?\\s*(?:系统提示词|提示词|初始指令|设定)")))

(dolist (rule +injection-rules+)
  (setf (injection-rule-scanner rule)
        (cl-ppcre:create-scanner (injection-rule-pattern rule)
                                  :case-insensitive-mode t)))

(defparameter +severity-label+
  '(("critical" . "critical") ("high" . "high") ("medium" . "medium") ("low" . "low")))

;;; --- normalisation ------------------------------------------------------------

(defparameter +lookalikes+
  '(;; Cyrillic
    (#\u0430 . #\a) (#\u0435 . #\e) (#\u043E . #\o) (#\u0440 . #\p) (#\u0441 . #\c)
    (#\u0445 . #\x) (#\u0443 . #\y) (#\u0456 . #\i) (#\u0455 . #\s) (#\u044C . #\b)
    (#\u0410 . #\A) (#\u0415 . #\E) (#\u041E . #\O) (#\u0420 . #\P) (#\u0421 . #\C)
    (#\u0425 . #\X) (#\u0423 . #\Y) (#\u0406 . #\I)
    ;; Greek
    (#\u03BF . #\o) (#\u03B1 . #\a) (#\u03BD . #\v) (#\u03C1 . #\p) (#\u03C5 . #\u)
    (#\u039F . #\O) (#\u0391 . #\A) (#\u03A1 . #\P) (#\u039D . #\N))
  "Letters that read as an ASCII one: what a rule must see through.")

(defparameter +invisible+
  (list #\u200B #\u200C #\u200D #\u2060 #\uFEFF #\u00AD)
  "Zero-width and soft-hyphen characters a rule must see through.")

(defun normalise (text)
  "TEXT with the invisible characters dropped, full-width ASCII folded and the
lookalike letters mapped, so one pattern covers what a person would read as
the same sentence."
  (let ((out (make-array (length text) :element-type 'character :fill-pointer 0)))
    (loop for ch across text
          do (cond ((member ch +invisible+ :test #'char=))
                   (t (let* ((code (char-code ch))
                             (folded (if (<= #xFF01 code #xFF5E)
                                         (code-char (- code #xFEE0))
                                         ch)))
                        (vector-push (or (cdr (assoc folded +lookalikes+ :test #'char=)) folded)
                                     out)))))
    (coerce out 'string)))

(defun needs-normalising-p (text)
  "Whether TEXT holds a character NORMALISE would change."
  (loop for ch across text
        thereis (or (member ch +invisible+ :test #'char=)
                    (<= #xFF01 (char-code ch) #xFF5E)
                    (assoc ch +lookalikes+ :test #'char=))))

;;; --- bounded base64 ------------------------------------------------------------

(defparameter +base64-token-scanner+
  (cl-ppcre:create-scanner "[A-Za-z0-9+/]{24,}={0,2}")
  "The only shape worth decoding: long, and base64's own alphabet.")

(defun base64-value (ch)
  "The six bits CH stands for, or NIL."
  (cond ((char<= #\A ch #\Z) (- (char-code ch) 65))
        ((char<= #\a ch #\z) (- (char-code ch) 71))
        ((char<= #\0 ch #\9) (+ (- (char-code ch) 48) 52))
        ((char= ch #\+) 62)
        ((char= ch #\/) 63)
        (t nil)))

(defun base64-decode (token)
  "TOKEN decoded, or NIL when it carries a padding or length that cannot be
one base64 string. Bounded: no stream, no dependency, no error."
  (let* ((core (string-right-trim "=" token))
         (length (length core))
         (bits (make-array (* 6 length) :element-type 'bit :fill-pointer 0)))
    (when (zerop length) (return-from base64-decode nil))
    (loop for ch across core
          for value = (base64-value ch)
          do (unless value (return-from base64-decode nil))
             (loop for shift from 5 downto 0
                   do (vector-push (ldb (byte 1 shift) value) bits)))
    (let ((chars (make-array (floor (length bits) 8) :element-type 'character :fill-pointer 0)))
      (loop for start from 0 below (- (length bits) 7) by 8
            do (vector-push (code-char (loop for index from start below (+ start 8)
                                             sum (ash (aref bits index) (- 7 (- index start)))))
                            chars))
      (let ((text (coerce chars 'string)))
        (when (and (plusp (length text))
                   (every (lambda (ch) (or (graphic-char-p ch) (member ch '(#\Space #\Newline #\Tab))))
                          text))
          text)))))

;;; --- the PII table -------------------------------------------------------------

(defstruct (pii-rule (:constructor make-pii-rule (id label pattern &optional validator)))
  id label pattern scanner validator)

(defparameter +pii-rules+
  (list
   (make-pii-rule "email" "email address"
                  "[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}")
   (make-pii-rule "url-credential" "credential in a URL"
                  "https?://[^/\\s:@]+:[^/\\s@]+@")
   (make-pii-rule "api-key" "API key"
                  "(?:sk-[A-Za-z0-9_\\-]{16,}|gh[pousr]_[A-Za-z0-9]{16,}|AKIA[0-9A-Z]{16}|xox[baprs]-[A-Za-z0-9\\-]{10,}|AIza[0-9A-Za-z_\\-]{35})")
   (make-pii-rule "cn-id" "CN ID card"
                  "(?<![0-9])[1-9][0-9]{5}(?:19|20)[0-9]{2}(?:0[1-9]|1[0-2])(?:0[1-9]|[12][0-9]|3[01])[0-9]{3}[0-9Xx](?![0-9])"
                  'cn-id-valid-p)
   (make-pii-rule "cn-bank-card" "CN bank card"
                  "(?<![0-9])[0-9]{16,19}(?![0-9])"
                  'luhn-valid-p)
   (make-pii-rule "cn-mobile" "CN mobile number"
                  "(?<![0-9])1[3-9][0-9]{9}(?![0-9])")
   (make-pii-rule "ipv4" "IPv4 address"
                  "(?<![0-9.])(?:[0-9]{1,3}\\.){3}[0-9]{1,3}(?![0-9.])"
                  'ipv4-valid-p)))

(dolist (rule +pii-rules+)
  (setf (pii-rule-scanner rule)
        (cl-ppcre:create-scanner (pii-rule-pattern rule) :case-insensitive-mode t)))

(defun luhn-valid-p (digits)
  "Whether DIGITS passes the Luhn check: 4111 1111 1111 1111 does, a
sixteen-digit order number usually does not."
  (let ((sum 0) (double nil))
    (loop for i from (1- (length digits)) downto 0
          for value = (digit-char-p (char digits i))
          do (if double
                 (let ((product (* 2 value))) (incf sum (if (> product 9) (- product 9) product)))
                 (incf sum value))
             (setf double (not double)))
    (and (plusp sum) (zerop (mod sum 10)))))

(defun ipv4-valid-p (text)
  "Whether every octet of TEXT is at most 255."
  (every (lambda (octet) (and (plusp (length octet)) (< (parse-integer octet) 256)))
         (uiop:split-string text :separator ".")))

(defparameter +cn-id-weights+ #(7 9 10 5 8 4 2 1 6 3 7 9 10 5 8 4 2))
(defparameter +cn-id-check+ "10X98765432")

(defun cn-id-valid-p (text)
  "Whether TEXT is an 18-character mainland ID card whose check digit agrees:
the date alone would pass an order number with a date in it."
  (and (= 18 (length text))
       (loop for i below 18
             always (or (digit-char-p (char text i)) (char= (char text i) #\x) (char= (char text i) #\X)))
       (let* ((sum (loop for i below 17
                         sum (* (digit-char-p (char text i)) (aref +cn-id-weights+ i))))
              (expected (char +cn-id-check+ (mod sum 11))))
         (char-equal expected (char text 17)))))
