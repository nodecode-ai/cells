;;;; normalize.lisp --- the folding that runs before any request.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is pure and offline: the 13-digit corporate number and
;;;; its check digit, company-name folding (width, spaces, the legal-form
;;;; words 株式会社 / (株) / ㈱), prefecture lookup and address folding. An
;;;; input that fails these checks never reaches gBizINFO, and nothing here
;;;; needs the network or a running cell.

(in-package #:nodecode-jp-corporate)

;;; --- text ----------------------------------------------------------------------

(defparameter +nfkc+
  (and (find-package "SB-UNICODE") (find-symbol "NORMALIZE-STRING" "SB-UNICODE"))
  "The Lisp's compatibility-composition function, when it has one.")

(defun nfkc (text)
  "TEXT in compatibility-composed form: full-width letters and digits to ASCII,
half-width katakana to full-width, ㈱ to (株), ㍿ to 株式会社. TEXT itself when
this Lisp normalises nothing."
  (if +nfkc+
      (handler-case (funcall +nfkc+ text :nfkc) (error () text))
      text))

(defparameter +blanks+ '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)
  "The characters folding drops: gBizINFO's own names sometimes carry them.")

(defun unblanked (text)
  "TEXT without whitespace, which no name comparison here cares about."
  (remove-if (lambda (character) (member character +blanks+)) text))

(defun fold (text)
  "Width- and space-insensitive form: NFKC, no whitespace, upper case."
  (string-upcase (unblanked (nfkc (princ-to-string (or text ""))))))

(defun replace-all (text from to)
  "TEXT with every FROM replaced by TO."
  (let ((out (make-string-output-stream))
        (size (length from)))
    (loop with index = 0
          while (< index (length text))
          do (if (and (plusp size)
                      (<= (+ index size) (length text))
                      (string= from text :start2 index :end2 (+ index size)))
                 (progn (write-string to out) (incf index size))
                 (progn (write-char (char text index) out) (incf index))))
    (get-output-stream-string out)))

(defun has-prefix-p (prefix text)
  "Whether TEXT begins with PREFIX."
  (and (<= (length prefix) (length text)) (string= prefix text :end2 (length prefix))))

(defun reversed (text)
  "TEXT backwards."
  (reverse text))

;;; --- company names -------------------------------------------------------------

(defparameter +legal-forms+
  '("特定非営利活動法人" "有限責任事業組合" "国立研究開発法人" "独立行政法人" "医療法人社団"
    "医療法人財団" "一般社団法人" "一般財団法人" "公益社団法人" "公益財団法人" "社会福祉法人"
    "株式会社" "有限会社" "合同会社" "合名会社" "合資会社" "相互会社" "医療法人" "学校法人"
    "宗教法人" "協同組合" "外国会社" "NPO法人")
  "The legal-form words a name can carry, longest first so a compound wins over its parts.")

(defparameter +abbreviations+
  '(("(株)" . "株式会社") ("(有)" . "有限会社") ("(同)" . "合同会社") ("(名)" . "合名会社")
    ("(資)" . "合資会社") ("(相)" . "相互会社") ("(一社)" . "一般社団法人") ("(一財)" . "一般財団法人")
    ("(公社)" . "公益社団法人") ("(公財)" . "公益財団法人") ("(特非)" . "特定非営利活動法人")
    ("(医)" . "医療法人") ("(福)" . "社会福祉法人") ("(学)" . "学校法人") ("(宗)" . "宗教法人")
    ("(独)" . "独立行政法人"))
  "What NFKC leaves of the parenthesised short forms, and the words each stands for.")

(defun spelled-out (text)
  "TEXT (already folded) with every short form spelled out."
  (reduce (lambda (name pair) (replace-all name (car pair) (cdr pair)))
          +abbreviations+ :initial-value text))

(defun full-name (text)
  "Folded name with abbreviations such as (株) spelled out as 株式会社."
  (spelled-out (fold text)))

(defun core-name (text)
  "Folded name without its legal-form words: '(株)ﾄﾖﾀ自動車' answers 'トヨタ自動車'."
  (reduce (lambda (name form) (replace-all name form ""))
          +legal-forms+ :initial-value (full-name text)))

(defun query-term (text)
  "The string to send as gBizINFO's partial-match name search.
Width differences are left to the API, but the legal-form words are removed,
because '(株)トヨタ自動車' is not a substring of 'トヨタ自動車株式会社'. An
official name may contain a space ('ＫＤＤＩ　ｘＧ　Ｎｅｔｗｏｒｋｓ'), so only the
longest space-free piece is sent and the whole name is matched afterwards."
  (let* ((name (reduce (lambda (name form) (replace-all name form " "))
                       +legal-forms+ :initial-value (spelled-out (nfkc (princ-to-string (or text ""))))))
         (pieces (remove ""
                         (loop for start = 0 then (1+ end)
                               for end = (or (position #\Space name :start start) (length name))
                               collect (string-trim '(#\Space #\Tab) (subseq name start end))
                               while (< end (length name)))
                         :test #'string=)))
    (if (null pieces)
        ""
        (reduce (lambda (best piece) (if (> (length piece) (length best)) piece best)) pieces))))

;;; --- the corporate number (法人番号) --------------------------------------------

(defparameter +number-junk+ '(#\- #\ー #\/ #\‐ #\‑ #\‒ #\– #\— #\―)
  "Characters an invoice or a form puts inside a corporate number.")

(defun check-digit (base12)
  "The check digit of a 12-digit corporate-number base (National Tax Agency
rule): digits weigh 1, 2, 1, 2, ... from the rightmost, and the digit is 9
minus the weighted sum modulo 9."
  (let ((total 0))
    (loop for index from 0 below (length base12)
          do (incf total (* (if (evenp index) 1 2)
                            (digit-char-p (char base12 (- (length base12) 1 index))))))
    (- 9 (mod total 9))))

(defun parse-corporate-number (raw)
  "RAW as a clean 13-digit corporate number, or a refusal that says what is
wrong with it. Accepts full-width digits, spaces and hyphens, and the leading
T of an invoice registration number (適格請求書発行事業者登録番号)."
  (let ((text (remove-if (lambda (character) (member character +number-junk+))
                          (unblanked (nfkc (string-trim '(#\Space #\Tab #\Newline) (or raw "")))))))
    (when (and (plusp (length text)) (member (char text 0) '(#\T #\t)))
      (setf text (subseq text 1)))
    (cond
      ((zerop (length text))
       (fail "corporate_number is empty: give the 13-digit 法人番号, e.g. 1180301018771."))
      ((not (every #'digit-char-p text))
       (fail "corporate_number ~s has characters other than digits: give the 13-digit 法人番号 (an invoice number such as T1180301018771 is also accepted)." raw))
      ((/= (length text) 13)
       (fail "corporate_number ~s has ~d digits; a 法人番号 has exactly 13. Check for a missing or extra digit, or find the number from the name with jp_corp_search." raw (length text)))
      ((not (= (digit-char-p (char text 0)) (check-digit (subseq text 1))))
       ;; The expected digit is deliberately not shown: "fixing" the first digit
       ;; would turn a typo anywhere into a valid-looking number of another company.
       (fail "corporate_number ~s fails the 法人番号 check digit, so at least one digit is wrong. Re-check it against the invoice, contract or registry, or search by name instead. No request was sent." raw))
      (t text))))

;;; --- prefectures and addresses --------------------------------------------------

(defparameter +prefectures+
  #(("北海道" . "hokkaido") ("青森県" . "aomori") ("岩手県" . "iwate") ("宮城県" . "miyagi")
    ("秋田県" . "akita") ("山形県" . "yamagata") ("福島県" . "fukushima") ("茨城県" . "ibaraki")
    ("栃木県" . "tochigi") ("群馬県" . "gunma") ("埼玉県" . "saitama") ("千葉県" . "chiba")
    ("東京都" . "tokyo") ("神奈川県" . "kanagawa") ("新潟県" . "niigata") ("富山県" . "toyama")
    ("石川県" . "ishikawa") ("福井県" . "fukui") ("山梨県" . "yamanashi") ("長野県" . "nagano")
    ("岐阜県" . "gifu") ("静岡県" . "shizuoka") ("愛知県" . "aichi") ("三重県" . "mie")
    ("滋賀県" . "shiga") ("京都府" . "kyoto") ("大阪府" . "osaka") ("兵庫県" . "hyogo")
    ("奈良県" . "nara") ("和歌山県" . "wakayama") ("鳥取県" . "tottori") ("島根県" . "shimane")
    ("岡山県" . "okayama") ("広島県" . "hiroshima") ("山口県" . "yamaguchi") ("徳島県" . "tokushima")
    ("香川県" . "kagawa") ("愛媛県" . "ehime") ("高知県" . "kochi") ("福岡県" . "fukuoka")
    ("佐賀県" . "saga") ("長崎県" . "nagasaki") ("熊本県" . "kumamoto") ("大分県" . "oita")
    ("宮崎県" . "miyazaki") ("鹿児島県" . "kagoshima") ("沖縄県" . "okinawa"))
  "Japan's 47 prefectures in JIS order: index 0 is code 01 (北海道).")

(defun prefecture-name (code)
  "The Japanese name of prefecture CODE (1-47), or NIL."
  (and code (<= 1 code (length +prefectures+)) (car (aref +prefectures+ (1- code)))))

(defun prefecture-code (name)
  "The code of the prefecture whose Japanese name is NAME, or NIL."
  (loop for index from 0 below (length +prefectures+)
        when (string= name (car (aref +prefectures+ index)))
          do (return (1+ index))))

(defun romaji-stem (text)
  "TEXT lower-cased with a romaji prefecture suffix ('-ken', ' to', 'fu',
'prefecture') taken off."
  (let ((lowered (string-downcase text)))
    (dolist (suffix '("prefecture" "pref." "pref" "ken" "to" "fu") lowered)
      (when (and (> (length lowered) (length suffix))
                 (has-prefix-p (reversed suffix) (reversed lowered)))
        (return (string-right-trim '(#\- #\Space)
                                   (subseq lowered 0 (- (length lowered) (length suffix)))))))))

(defun strip-postal-code (text)
  "TEXT without a leading postal code (〒123-4567, 1234567, 郵便番号:123-4567)."
  (let ((start (cond ((and (plusp (length text)) (char= (char text 0) #\〒)) 1)
                     ((has-prefix-p "郵便番号:" text) 6)
                     ((has-prefix-p "郵便番号" text) 5)
                     (t 0))))
    (let ((digits (subseq text (min start (length text)))))
      (flet ((digit-at (index) (and (< index (length digits)) (digit-char-p (char digits index)))))
        (cond
          ((and (digit-at 0) (digit-at 1) (digit-at 2)
                (member (and (< 3 (length digits)) (char digits 3)) +number-junk+)
                (digit-at 4) (digit-at 5) (digit-at 6) (digit-at 7))
           (subseq digits 8))
          ((and (digit-at 0) (digit-at 1) (digit-at 2) (digit-at 3) (digit-at 4) (digit-at 5) (digit-at 6))
           (subseq digits 7))
          (t digits))))))

(defparameter +kanji-digits+ "〇一二三四五六七八九")

(defun kanji-number->digits (text)
  "TEXT — kanji digits such as 三, 十二, 二十 — as figures: '3', '12', '20'."
  (if (not (find #\十 text))
      (map 'string (lambda (character)
                     (if (find character +kanji-digits+)
                         (digit-char (position character +kanji-digits+))
                         character))
           text)
      (let* ((ten (position #\十 text))
             (tens (subseq text 0 ten))
             (ones (subseq text (1+ ten)))
             (value (+ (* 10 (if (zerop (length tens))
                                 1
                                 (position (char tens 0) +kanji-digits+)))
                       (if (zerop (length ones))
                           0
                           (position (char ones 0) +kanji-digits+)))))
        (princ-to-string value))))

(defun kanji-numbers (text)
  "TEXT with each run of kanji digits turned into figures where a 丁目, 番 or
号 follows it."
  (with-output-to-string (out)
    (loop with index = 0
          with size = (length text)
          while (< index size)
          do (let ((end (loop for scan from index below size
                              while (find (char text scan) +kanji-digits+)
                              finally (return scan))))
               (if (and (> end index) (< end size) (find (char text end) "丁目番号"))
                   (progn (write-string (kanji-number->digits (subseq text index end)) out)
                          (setf index end))
                   (progn (write-char (char text index) out) (incf index)))))))

(defun collapse-dashes (text)
  "TEXT with runs of '-' kept only where a figure follows them."
  (with-output-to-string (out)
    (loop with index = 0
          with size = (length text)
          while (< index size)
          do (if (char= (char text index) #\-)
                 (progn (loop while (and (< index size) (char= (char text index) #\-))
                               do (incf index))
                        (when (and (< index size) (digit-char-p (char text index)))
                          (write-char #\- out)))
                 (progn (write-char (char text index) out) (incf index))))))

(defun fold-address (text)
  "Address form that ignores width, spaces, a leading postal code, kanji
numerals, ヶ/ケ/が ('霞ヶ関' = '霞が関') and 丁目/番地/号 against hyphens."
  (let* ((folded (strip-postal-code (fold text)))
         (mapped (map 'string (lambda (character)
                                (cond ((member character '(#\ヶ #\ケ #\が #\ガ)) #\ケ)
                                      ((member character '(#\‐ #\‑ #\‒ #\– #\— #\― #\ー)) #\-)
                                      (t character)))
                      (kanji-numbers folded))))
    (dolist (mark '("丁目" "番地の" "番地" "番" "号"))
      (setf mapped (replace-all mapped mark "-")))
    (string-trim "-" (collapse-dashes mapped))))

(defun split-prefecture (address)
  "ADDRESS split into (values CODE REST): CODE is the prefecture's number
(1-47) or NIL, REST the address without it. '愛知県豊田市' => 23 and '豊田市';
'愛知' => 23 and ''; 'Tokyo' => 13 and ''; '豊田市' => NIL and '豊田市'. A
leading postal code is ignored."
  (let* ((text (strip-postal-code (fold address)))
         (stem (and (plusp (length text)) (romaji-stem text))))
    (when (and stem (plusp (length stem)))
      (loop for index from 0 below (length +prefectures+)
            when (string= stem (cdr (aref +prefectures+ index)))
              do (return-from split-prefecture (values (1+ index) ""))))
    (loop for index from 0 below (length +prefectures+)
          for name = (car (aref +prefectures+ index))
          when (has-prefix-p name text)
            do (return-from split-prefecture (values (1+ index) (subseq text (length name)))))
    (loop for index from 1 below (length +prefectures+)
          for name = (car (aref +prefectures+ index))
          for bare = (subseq name 0 (1- (length name)))
          when (has-prefix-p bare text)
            do (let ((rest (subseq text (length bare))))
                 ;; '愛知豊田市' is Aichi, but '京都市', '福島区' and '千葉市' are cities.
                 (unless (and (plusp (length rest)) (find (char rest 0) "市区町村郡"))
                   (return-from split-prefecture (values (1+ index) rest)))))
    (values nil text)))
