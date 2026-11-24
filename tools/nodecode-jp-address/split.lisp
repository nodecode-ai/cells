;;;; split.lisp --- a Japanese address, split against Geolonia's address files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything the cell knows about addresses: one normalization, the two
;;;; files Geolonia publishes over HTTPS, the longest-prefix match through
;;;; prefecture, municipality and town, and the result object one call
;;;; answers. CELL.LISP is the only caller.
;;;;
;;;; The peer is the address data host and nothing else. A call sends at
;;;; most two GETs: the national list of prefectures and municipalities,
;;;; then the one town file for the municipality that matched. Only the
;;;; matched prefecture and municipality names go into a URL path; the house
;;;; number, the lot and the building name never travel. No key is read, no
;;;; credential is sent, nothing is written to disk, nothing is spawned.

(in-package #:nodecode-jp-address)

;;; --- what every result carries -------------------------------------------------

(defparameter +attribution+
  (concatenate 'string
               "Geolonia 住所データ（CC BY 4.0 https://creativecommons.org/licenses/by/4.0/）。"
               "本データは、「アドレス・ベース・レジストリ」（デジタル庁）をもとに"
               "株式会社 Geolonia が作成したものです。"
               "「アドレス・ベース・レジストリ」（デジタル庁）"
               "（https://dataset.address-br.digital.go.jp）をもとに株式会社 Geolonia が作成し、"
               "jp-address が町丁目までを選択した。")
  "The attribution every result carries, verbatim from the original plugin: the
address data is Geolonia's, made from Japan's Digital Agency base registry,
under CC BY 4.0, and the town-level choice is this cell's.")

(defparameter +msg-extra+
  "address_split takes only an address string. Drop every other argument and pass address, for example 東京都千代田区霞が関2-1-2.")

(defparameter +msg-length+
  "The address must be a string of 400 characters or fewer. Pass address as one string, for example 東京都千代田区霞が関2-1-2.")

(defparameter +msg-script+
  "Write the address in Japanese and include the prefecture, for example 東京都千代田区霞が関2-1-2.")

(defparameter +msg-city+
  "This municipality name is in more than one prefecture. Include the prefecture.")

(defparameter +msg-muni+
  "This name matches more than one municipality. Include the county.")

(defparameter +msg-town+
  "More than one town matches the same span.")

(defparameter +msg-town-length+
  "More than one town matches this address at different lengths.")

(defparameter +msg-upstream+ "The address file could not be read.")

;;; --- the bounds ----------------------------------------------------------------

(defparameter +max-chars+ 400
  "The longest address one call takes, before normalization.")

(defparameter +max-bytes+ (* 8 1000 1000)
  "The most one address file may be. The national list is a third of a
megabyte and a town file is under one, so a peer that sends more is cut
rather than waited on.")

(defparameter +user-agent+
  "nodecode-jp-address/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "Who the data host sees asking. It names the cell and where it comes from,
and nothing about the operator.")

(defparameter +timeout+ 15
  "Seconds one address file may take. The cell's section sets the value the
call uses; this is what the code falls back on.")

;;; --- normalization -------------------------------------------------------------

(defun whitespace-code-p (code)
  "T when CODE is a character a person meant as a separator: the ASCII
controls Python calls whitespace, the no-break forms, and the ideographic
space."
  (or (<= 9 code 13)
      (= code 32) (= code 133) (= code 160) (= code 5760)
      (<= 8192 code 8202)
      (= code 8232) (= code 8233) (= code 8239) (= code 8287)
      (= code 12288)))

(defun fold-code (code)
  "CODE as the character NFKC folds it to, for the forms an address is
written in: fullwidth digits, letters, the fullwidth hyphen and the
fullwidth plus."
  (cond ((<= #xFF10 code #xFF19) (+ (- code #xFF10) (char-code #\0)))
        ((<= #xFF21 code #xFF3A) (+ (- code #xFF21) (char-code #\A)))
        ((<= #xFF41 code #xFF5A) (+ (- code #xFF41) (char-code #\a)))
        ((= code #xFF0D) (char-code #\-))
        ((= code #xFF0B) (char-code #\+))
        (t code)))

(defun normalize-text (text)
  "TEXT with every separator dropped and the fullwidth forms an address is
written in folded. Compatibility kanji are not folded to their modern
forms, and neither is kana: 澤 stays 澤, ヶ stays ヶ."
  (coerce (loop for ch across text
                for code = (char-code ch)
                unless (whitespace-code-p code)
                  collect (code-char (fold-code code)))
          'string))

(defun ascii-letters-p (text)
  "T when TEXT is wholly ASCII letters: an address written in the Latin
alphabet, which this parser cannot split."
  (and (plusp (length text))
       (every (lambda (ch) (or (char<= #\a ch #\z) (char<= #\A ch #\Z))) text)))

;;; --- json, read leniently ------------------------------------------------------

(defun json-field (object key)
  "OBJECT's member KEY when OBJECT is a JSON object, else NIL."
  (and (hash-table-p object) (gethash key object)))

(defun json-text (object key)
  "OBJECT's member KEY as text, or NIL when it is absent, null or not text."
  (let ((value (json-field object key)))
    (and (stringp value) value)))

(defun array-value-p (value)
  "T when VALUE is a JSON array: a vector that is not a string."
  (and (vectorp value) (not (stringp value))))

(defun json-array (value)
  "VALUE as a list: a JSON array decodes to a vector, and anything else is
no list at all."
  (if (array-value-p value) (coerce value 'list) nil))

(defun point-of (value)
  "The file's point for a row, [longitude, latitude], as (LAT . LNG); NIL
when it is absent or out of range, so no coordinate is ever invented."
  (when (and (array-value-p value) (= 2 (length value)))
    (let ((lng (aref value 0))
          (lat (aref value 1)))
      (when (and (realp lng) (realp lat)
                 (<= -90 lat 90) (<= -180 lng 180))
        (cons lat lng)))))

(defun point-object (pair level)
  "PAIR as the result's point member at LEVEL, or NIL when there is none."
  (when pair
    (list :lat (car pair) :lng (cdr pair) :level level)))

;;; --- the two files -------------------------------------------------------------

(defun url-segment (text)
  "TEXT as one UTF-8 percent-encoded path segment."
  (quri:url-encode text :encoding :utf-8))

(defun list-url (base)
  "The national list of prefectures and municipalities under BASE."
  (concatenate 'string base ".json"))

(defun town-url (base pref city)
  "The one municipality's town file under BASE."
  (format nil "~a/~a/~a.json" base (url-segment pref) (url-segment city)))

(defun http-body (url timeout)
  "One GET: URL's body as text and its status. Tests stub this name and
never touch the network."
  (multiple-value-bind (body status)
      (nlk:http :get url
                :headers `(("user-agent" . ,+user-agent+)
                           ("accept" . "application/json"))
                :timeout timeout
                :max-bytes +max-bytes+)
    (values (if (stringp body) body (nlk:decode-octets body)) status)))

(defun fetch-json (url timeout)
  "URL as one decoded JSON object. A status that is not 200, or a body that
is not an object, is a refusal the caller renders as upstream."
  (multiple-value-bind (body status) (http-body url timeout)
    (unless (eql status 200)
      (fail "~a HTTP ~a" +msg-upstream+ status))
    (let ((json (ignore-errors (nlk:decode-json body))))
      (unless (hash-table-p json)
        (fail "~a" +msg-upstream+))
      json)))

;;; --- the list file -------------------------------------------------------------

(defun parse-city (row)
  "One municipality row of the list: its official name, the names it
matches with and without a listed county, and its point."
  (let ((city (json-text row "city")))
    (unless (and city (plusp (length city)))
      (fail "~a" +msg-upstream+))
    (let* ((county (or (json-text row "county") ""))
           (ward (or (json-text row "ward") ""))
           (official (concatenate 'string county city ward))
           (short (concatenate 'string city ward)))
      (list :official official
            :names (if (string= official short) (list official) (list official short))
            :point (point-of (json-field row "point"))))))

(defun parse-prefs (json)
  "The list file as prefectures, each with its municipalities."
  (let ((rows (json-array (json-field json "data"))))
    (unless rows
      (fail "~a" +msg-upstream+))
    (mapcar (lambda (row)
              (let ((name (json-text row "pref"))
                    (cities (json-field row "cities")))
                (unless (and name (plusp (length name)) (array-value-p cities))
                  (fail "~a" +msg-upstream+))
                (list :name name
                      :point (point-of (json-field row "point"))
                      :cities (mapcar #'parse-city (json-array cities)))))
            rows)))

;;; --- the town file -------------------------------------------------------------

(defparameter +boundary-kanji+ "〇零一二三四五六七八九十百千"
  "A kanji numeral after a town name is a lot number: the boundary holds.")

(defparameter +num-suffixes+ '("丁目" "地割" "番町" "条" "線" "号")
  "The suffixes a place-name numeral is written before. Longer first, so
丁目 is not read as 丁.")

(defun part-text (row key)
  "ROW's member KEY, normalized the way the input is."
  (normalize-text (or (json-text row key) "")))

(defun row-name (row)
  "The town name ROW spells, normalized: the oaza, then its chome, then
the koaza; a row with no oaza is named by its koaza alone."
  (let ((oaza (part-text row "oaza_cho"))
        (chome (part-text row "chome"))
        (koaza (part-text row "koaza")))
    (if (plusp (length oaza))
        (concatenate 'string oaza chome koaza)
        koaza)))

(defun row-head (row)
  "ROW's name with its chome removed: the oaza and its koaza."
  (let ((oaza (part-text row "oaza_cho"))
        (koaza (part-text row "koaza")))
    (if (plusp (length oaza)) (concatenate 'string oaza koaza) koaza)))

(defun town-display (row)
  "ROW's town name in the file's own spelling, which is what a result shows."
  (let ((oaza (or (json-text row "oaza_cho") ""))
        (chome (or (json-text row "chome") ""))
        (koaza (or (json-text row "koaza") "")))
    (if (plusp (length oaza))
        (concatenate 'string oaza chome koaza)
        koaza)))

(defun chome-number (row)
  "ROW's chome as the integer an Arabic form may be read against, or NIL."
  (let ((value (json-field row "chome_n")))
    (when (and (integerp value) (<= 1 value 99)) value)))

(defun digit-char-p* (ch)
  (and ch (char<= #\0 ch #\9)))

(defun boundary-ok-p (text length)
  "T when a town name ending at LENGTH in TEXT is followed by a lot marker
or the end: a digit, a kanji numeral, 番, 号, a hyphen, 字 or 丁."
  (and (plusp length) (<= length (length text))
       (or (= length (length text))
           (let ((next (char text length)))
             (or (digit-char-p* next)
                 (find next "番号-字丁" :test #'char=)
                 (find next +boundary-kanji+ :test #'char=))))))

(defun arabic-to-kanji (number)
  "NUMBER from 0 to 99 as a place-name numeral, or NIL above 99: 0 is 零,
10 is 十, 21 is 二十一."
  (let ((ones "一二三四五六七八九"))
    (when (<= 0 number 99)
      (cond ((zerop number) "零")
            ((< number 10) (string (char ones (1- number))))
            (t (multiple-value-bind (tens rest) (floor number 10)
                 (concatenate 'string
                              (if (= tens 1)
                                  "十"
                                  (concatenate 'string (string (char ones (1- tens))) "十"))
                              (if (zerop rest) "" (string (char ones (1- rest)))))))))))

(defun expanded-units (text)
  "TEXT as (ORIGINAL-END . EXPANDED) units: a run of ASCII digits right
before 丁目, 地割, 番町, 条, 線 or 号 is read as the kanji numeral and that
suffix, so 宮の森1条 is compared as 宮の森一条 while the index stays one
into the original. Digits before a hyphen stay digits."
  (let ((units '())
        (index 0)
        (size (length text)))
    (loop while (< index size) do
      (let* ((digits-end (and (digit-char-p* (char text index))
                               (let ((end (1+ index)))
                                 (loop while (and (< end size) (digit-char-p* (char text end)))
                                       do (incf end))
                                 end)))
             (suffix (and digits-end
                          (find-if (lambda (candidate)
                                     (and (<= (+ digits-end (length candidate)) size)
                                          (string= candidate text
                                                   :start2 digits-end
                                                   :end2 (+ digits-end (length candidate)))))
                                   +num-suffixes+)))
             (kanji (and suffix (arabic-to-kanji
                                 (parse-integer text :start index :end digits-end)))))
        (if (and suffix kanji)
            (let ((stop (+ digits-end (length suffix))))
              (push (cons stop (concatenate 'string kanji suffix)) units)
              (setf index stop))
            (progn (push (cons (1+ index) (string (char text index))) units)
                   (incf index)))))
    (nreverse units)))

(defun prefix-end (units candidate)
  "The index in the original text where CANDIDATE ends, when CANDIDATE is a
unit-aligned prefix of the expanded text; NIL otherwise."
  (when (plusp (length candidate))
    (let ((accumulated ""))
      (loop for (end . piece) in units do
        (setf accumulated (concatenate 'string accumulated piece))
        (cond ((< (length accumulated) (length candidate))
               (unless (and (<= (length accumulated) (length candidate))
                            (string= accumulated candidate
                                     :end2 (length accumulated)))
                 (return nil)))
              ((string= accumulated candidate) (return end))
              (t (return nil)))))))

(defun prefix-p (prefix text)
  "T when PREFIX starts TEXT."
  (and (<= (length prefix) (length text))
       (string= prefix text :end2 (length prefix))))

(defun town-match (row text units)
  "The longest prefix of TEXT that ROW's town name spells => (values LENGTH
KIND). KIND is `name' for the file's own spelling and `loose' for an
Arabic chome written without 丁目, which cannot pick a chome on its own."
  (let ((name (row-name row))
        (oaza (part-text row "oaza_cho"))
        (koaza (part-text row "koaza"))
        (best 0)
        (kind ""))
    (labels ((take (candidate candidate-kind)
               (let ((length (length candidate)))
                 (when (and (> length best) (plusp length)
                            (prefix-p candidate text)
                            (boundary-ok-p text length))
                   (setf best length kind candidate-kind)))))
      (when (plusp (length name))
        (take name "name")
        (let ((number (chome-number row)))
          (when (and (plusp (length oaza)) (zerop (length koaza)) number)
            (let* ((digits (princ-to-string number))
                   (bare (concatenate 'string oaza digits)))
              (take (concatenate 'string oaza digits "丁目") "name")
              (take (concatenate 'string oaza digits "-") "loose")
              (when (prefix-p bare text)
                (let ((next (and (< (length bare) (length text)) (char text (length bare)))))
                  (when (or (null next)
                            (digit-char-p* next)
                            (member next '(#\番 #\- #\号) :test #'char=)
                            (find next +boundary-kanji+ :test #'char=))
                    (take bare "loose")))))))
        (let ((end (prefix-end units name)))
          (when (and end (> end best) (boundary-ok-p text end))
            (setf best end kind "name"))))
    (values best kind))))

;;; --- the match ----------------------------------------------------------------

(defun pref-hit (text prefs)
  "The longest prefecture of PREFS that starts TEXT => (PREF . REST), or NIL."
  (let (best best-length)
    (dolist (pref prefs)
      (let* ((name (getf pref :name))
             (length (length name)))
        (when (and (prefix-p name text) (> length (or best-length 0)))
          (setf best pref best-length length))))
    (when best (cons best (subseq text best-length)))))

(defun expand-prefecture (text prefs)
  "TEXT with a prefecture written without 都, 府 or 県 spelled out in full,
so 東京千代田区 reads as 東京都千代田区; NIL when no name starts it."
  (loop for pref in prefs
        for name = (getf pref :name)
        when (and (> (length name) 1)
                  (find (char name (1- (length name))) "都府県" :test #'char=)
                  (prefix-p (subseq name 0 (1- (length name))) text))
          return (concatenate 'string name (subseq text (1- (length name))))))

(defun matching-cities (cities text)
  "The municipalities of CITIES whose name is the longest prefix of TEXT,
as (CITY . MATCHED-NAME) pairs."
  (let (best best-length)
    (dolist (city cities)
      (dolist (name (getf city :names))
        (when (and (plusp (length name)) (prefix-p name text))
          (let ((length (length name)))
            (cond ((> length (or best-length 0)) (setf best-length length best (list (cons city name))))
                  ((= length best-length) (push (cons city name) best)))))))
    (nreverse best)))

(defun ambiguous-cities (hits &optional (message +msg-muni+))
  "The refusal whose municipalities are the distinct official names HITS met."
  (list :status "ambiguous_city"
        :message message
        :municipalities (remove-duplicates (mapcar (lambda (hit) (getf (car hit) :official)) hits)
                                            :test #'string=)))

(defun level-2-result (rest pref city)
  "The answer when the municipality matched and no single town did: the
coarse, safe one, with the municipality's point."
  (list :level 2
        :pref (getf pref :name)
        :city (getf city :official)
        :other rest
        :point (or (point-object (getf city :point) 2)
                   (point-object (getf pref :point) 1))))

(defun ambiguous-town (entries message)
  "The answer when more than one town matched: the towns and their ids, so
the choice can be put to the person who asked."
  (list :status "ambiguous_town"
        :message message
        :towns (mapcar (lambda (entry) (town-display (getf entry :row))) entries)
        :machiaza-id (mapcar (lambda (entry)
                               (let ((id (json-field (getf entry :row) "machiaza_id")))
                                 (cond ((stringp id) id)
                                       ((integerp id) (princ-to-string id))
                                       (t ""))))
                             entries)))

(defun loose-ambiguity (best rows names)
  "The rows an Arabic form written without 丁目 cannot choose between, or
NIL: 宮ケ丘3-1 lists the row with no chome and the chome row, and does not
pick 三丁目."
  (let ((heads (remove-duplicates
                (loop for entry in best
                      when (string= (getf entry :kind) "loose")
                        collect (row-head (getf entry :row)))
                :test #'string=)))
    (when heads
      (let ((chosen (remove-if-not (lambda (entry)
                                     (member (row-head (getf entry :row)) heads
                                             :test #'string=))
                                   best))
            (bare (loop for row in rows
                        for name in names
                        when (and (member name heads :test #'string=)
                                  (zerop (length (part-text row "chome"))))
                          collect (list :row row :name name :length (length name) :kind "bare"))))
        (when (>= (+ (length chosen) (length bare)) 2)
          (append chosen bare))))))

(defun finish-town (entry rest pref city sorted-names)
  "The answer for the one town ENTRY matched: level 3, unless the same file
holds a longer name that starts with it and the address matched none of
those, in which case the coarser level 2 is the safe answer."
  (let ((name (getf entry :name)))
    (if (some (lambda (other)
                (and (> (length other) (length name)) (prefix-p name other)))
              sorted-names)
        (level-2-result rest pref city)
        (let* ((row (getf entry :row))
               (other (subseq rest (min (getf entry :length) (length rest)))))
          (when (and (plusp (length other)) (char= #\- (char other 0)))
            (setf other (subseq other 1)))
          (list :level 3
                :pref (getf pref :name)
                :city (getf city :official)
                :town (town-display row)
                :other other
                :point (or (point-object (point-of (json-field row "point")) 3)
                           (point-object (getf city :point) 2)
                           (point-object (getf pref :point) 1)))))))

(defun split-town (rest base timeout pref city)
  "REST, after PREF and CITY matched, against that municipality's town file."
  (let* ((json (fetch-json (town-url base (getf pref :name) (getf city :official)) timeout))
         (data (json-field json "data")))
    (unless (array-value-p data)
      (fail "~a" +msg-upstream+))
    (let* ((rows (json-array data))
           (names (mapcar #'row-name rows))
           (sorted (sort (remove-duplicates (remove "" names :test #'string=) :test #'string=)
                         #'string<))
           (units (expanded-units rest))
           (entries (loop for row in rows
                          for name in names
                          collect (multiple-value-bind (length kind) (town-match row rest units)
                                    (list :row row :name name :length length :kind kind))))
           (best-length (reduce #'max entries :key (lambda (entry) (getf entry :length))
                                            :initial-value 0)))
      (if (zerop best-length)
          (level-2-result rest pref city)
          (let* ((best (remove-if-not (lambda (entry) (= (getf entry :length) best-length))
                                       entries))
                 (loose (loose-ambiguity best rows names)))
            (cond (loose (ambiguous-town loose +msg-town+))
                  ((cdr best) (ambiguous-town best +msg-town+))
                  (t (finish-town (first best) rest pref city sorted))))))))

(defun split-in-prefecture (text pref rest base timeout)
  "TEXT whose prefecture PREF matched, with REST after its name."
  (let ((hits (matching-cities (getf pref :cities) rest)))
    (cond ((>= (length hits) 2) (ambiguous-cities hits))
          ((null hits) (list :level 1
                             :pref (getf pref :name)
                             :other rest
                             :point (point-object (getf pref :point) 1)))
          (t (let ((hit (first hits)))
               (split-town (subseq rest (length (cdr hit))) base timeout
                           pref (car hit)))))))

(defun split-anywhere (text prefs base timeout)
  "TEXT with no prefecture in it: the municipality is looked for in every
prefecture of the list, and only a name that is unique in Japan is used."
  (let (best best-length)
    (dolist (pref prefs)
      (dolist (city (getf pref :cities))
        (dolist (name (getf city :names))
          (when (and (plusp (length name)) (prefix-p name text))
            (let ((length (length name)))
              (cond ((> length (or best-length 0))
                     (setf best-length length best (list (list pref city name))))
                    ((= length best-length)
                     (push (list pref city name) best))))))))
    (setf best (nreverse best))
    (let ((prefectures (remove-duplicates (mapcar (lambda (hit) (getf (first hit) :name)) best)
                                          :test #'string=))
          (cities (remove-duplicates (mapcar #'second best) :test #'eq)))
      (cond ((>= (length prefectures) 2)
             (list :status "ambiguous_city" :message +msg-city+ :prefectures prefectures))
            ((>= (length cities) 2)
             (ambiguous-cities (mapcar (lambda (hit) (cons (second hit) (third hit))) best)))
            ((null best)
             (if (ascii-letters-p text)
                 (list :status "bad_args" :message +msg-script+)
                 (list :level 0 :other text)))
            (t (destructuring-bind (pref city name) (first best)
                 (split-town (subseq text (length name)) base timeout pref city)))))))

;;; --- one address ---------------------------------------------------------------

(defun split-address (text base timeout)
  "Split TEXT against the address files under BASE => a result plist. An
address that is empty after normalization is level 0 and sends nothing, and
one longer than the limit is refused before any request."
  (if (> (length text) +max-chars+)
      (list :status "bad_args" :message +msg-length+)
      (let ((text (normalize-text text)))
        (cond ((zerop (length text)) (list :level 0 :other ""))
              (t (let* ((prefs (parse-prefs (fetch-json (list-url base) timeout)))
                        (hit (pref-hit text prefs)))
                   (unless hit
                     (let ((expanded (expand-prefecture text prefs)))
                       (when expanded
                         (setf text expanded hit (pref-hit text prefs)))))
                   (if hit
                       (split-in-prefecture text (car hit) (cdr hit) base timeout)
                       (split-anywhere text prefs base timeout))))))))

(defun result-object (result)
  "RESULT as the JSON object a call answers, the shape the original tool
had: level, pref, city, town, other, an optional point, and attribution."
  (let ((object (nlk:make-json-object)))
    (if (getf result :status)
        (progn
          (setf (gethash "status" object) (getf result :status)
                (gethash "message" object) (getf result :message))
          (loop for (key name) in '((:towns "towns")
                                    (:machiaza-id "machiaza_id")
                                    (:municipalities "municipalities")
                                    (:prefectures "prefectures"))
                for value = (getf result key)
                when value do (setf (gethash name object) (coerce value 'vector))))
        (progn
          (setf (gethash "level" object) (getf result :level)
                (gethash "other" object) (or (getf result :other) ""))
          (loop for (key name) in '((:pref "pref") (:city "city") (:town "town"))
                for value = (getf result key)
                when value do (setf (gethash name object) value))))
    (let ((point (getf result :point)))
      (when point
        (setf (gethash "point" object)
              (nlk:make-json-object "lat" (getf point :lat)
                                     "lng" (getf point :lng)
                                     "level" (getf point :level)))))
    (setf (gethash "attribution" object) +attribution+)
    object))
