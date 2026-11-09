;;;; client.lisp --- the AnkiConnect call, and the collection verbs over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; AnkiConnect is an Anki add-on that answers one JSON object per POST on
;;;; loopback: an action, a version and its params in, a result and an error
;;;; out. HTTP is the only seam that leaves this image, so it is one
;;;; function -- HTTP here -- that a test replaces whole.

(in-package #:nodecode-anki)

(defparameter *default-url* "http://127.0.0.1:8765"
  "Where AnkiConnect listens when the section names nowhere else.")

(defparameter +ankiconnect-addon+ "2055492159"
  "AnkiConnect's add-on code, the one thing the operator has to install.")

(defparameter *max-list* 50
  "The most notes or cards one tool call ever asks a collection for.")

(defparameter *ease-values* '(("again" . 1) ("hard" . 2) ("good" . 3) ("easy" . 4))
  "A self-grade as the number Anki's scheduler takes.")

(defparameter *type-labels* '((0 . "new") (1 . "learning") (2 . "review") (3 . "relearning"))
  "Anki's card type number, in words.")

;;; --- the one seam ----------------------------------------------------------------

(defun http (url &key content headers timeout)
  "POST CONTENT to URL: (values BODY STATUS).  The cell's only way out."
  (nlk:http :post url :headers headers :content content :timeout timeout))

(defun endpoint ()
  "The AnkiConnect endpoint this cell talks to."
  (setting :anki-url))

(defun call (action &optional params)
  "One AnkiConnect ACTION, PARAMS a JSON object or NIL.  => the decoded RESULT."
  (let* ((payload (nlk:json-object "action" action "version" 6
                                    "params" (or params (nlk:json-object))))
         (key (setting :api-key)))
    (when key (setf (gethash "key" payload) key))
    (multiple-value-bind (body status)
        (http (endpoint)
              :content (nlk:encode-json-object payload)
              :headers '(("content-type" . "application/json"))
              :timeout (setting :request-timeout))
      (unless (and (integerp status) (= status 200))
        (fail "AnkiConnect answered HTTP ~a for `~a`~@[: ~a~]"
              status action (and body (shorten body 300))))
      (let ((data (ignore-errors (nlk:decode-json body))))
        (unless (hash-table-p data)
          (fail "AnkiConnect answered no JSON object for `~a`; check that ~a is AnkiConnect and not another service"
                action (endpoint)))
        (let ((error (gethash "error" data)))
          (when (and error (not (eq error :null)))
            (fail "~a  ~a" (princ-to-string error) (hint-for action error))))
        (let ((result (gethash "result" data)))
          (unless (eq result :null) result))))))

(defun shorten (text limit)
  "TEXT cut to LIMIT characters, on one line."
  (let ((flat (map 'string (lambda (char)
                              (if (member char '(#\Newline #\Return #\Tab)) #\Space char))
                    text)))
    (if (> (length flat) limit) (subseq flat 0 limit) flat)))

(defun hint-for (action error)
  "What to do about ERROR, or the add-on advice when ACTION was not understood."
  (let ((said (string-downcase (princ-to-string error))))
    (cond ((some (lambda (phrase) (search phrase said))
                 '("unsupported action" "unknown action" "action not found" "not supported"))
           (format nil "Update the AnkiConnect add-on (code ~a) from Anki's Tools -> Add-ons dialog."
                   +ankiconnect-addon+))
          ((search "api key" said)
           "Set anki.api_key in the section to the key AnkiConnect expects.")
          (t (format nil "Check the AnkiConnect log and the endpoint ~a." (endpoint))))))

;;; --- reading what Anki said --------------------------------------------------------

(defun as-list (value)
  "VALUE as a list of its elements when it is a JSON array, else NIL."
  (cond ((stringp value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun field (object key)
  "OBJECT's member KEY, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun text (value)
  "VALUE as text, an absent or null member reading as the empty string."
  (cond ((null value) "") ((eq value :null) "") (t (princ-to-string value))))

(defun fields-of (object)
  "OBJECT's members as (NAME VALUE) pairs, in the order Anki gave them."
  (if (hash-table-p object)
      (loop for name being the hash-keys of object using (hash-value value)
            collect (list name (if (hash-table-p value) (text (field value "value")) (text value))))
      '()))

;;; --- HTML, as the text a model reads ------------------------------------------------

(defparameter *entities*
  (list (cons "amp" "&") (cons "lt" "<") (cons "gt" ">")
        (cons "quot" (string (code-char 34))) (cons "apos" (string (code-char 39)))
        (cons "nbsp" " ") (cons "#39" (string (code-char 39))))
  "The entities Anki's own fields carry, and what each stands for.")

(defun tag-name (tag)
  "TAG, the inside of an HTML tag, as its lowercase name and whether it closes."
  (let* ((trimmed (string-left-trim " " tag))
         (shut (and (plusp (length trimmed)) (char= (char trimmed 0) (code-char 47))))
         (start (if shut 1 0))
         (end (or (position-if (lambda (char)
                                 (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
                               trimmed :start start)
                  (length trimmed))))
    (values (string-downcase (subseq trimmed start end)) shut)))

(defun skip-block (raw from what)
  "Where the style or script block starting after the tag at FROM ends in RAW."
  (let* ((close (concatenate 'string "</" what))
         (at (search close (string-downcase raw) :start2 from)))
    (if at (1+ (or (position (code-char 62) raw :start at) (1- (length raw)))) (length raw))))

(defun strip-html (value)
  "An Anki field, which is HTML, as the plain text a model reads."
  (let* ((raw (text value))
         (length (length raw))
         (out (make-string-output-stream)))
    (loop with at = 0
          while (< at length)
          for char = (char raw at)
          do (cond
               ((char= char (code-char 60))
                (let ((shut (position (code-char 62) raw :start at)))
                  (if (null shut)
                      (progn (write-char char out) (incf at))
                      (multiple-value-bind (name closing) (tag-name (subseq raw (1+ at) shut))
                        (cond ((and (not closing) (member name '("style" "script") :test #'string=))
                               (setf at (skip-block raw shut name)))
                              ((and (not closing) (string= name "br"))
                               (write-char #\Newline out) (setf at (1+ shut)))
                              ((and closing (member name '("div" "p" "li" "tr" "h1" "h2" "h3"
                                                       "h4" "h5" "h6") :test #'string=))
                               (write-char #\Newline out) (setf at (1+ shut)))
                              (t (setf at (1+ shut))))))))
               ((char= char (code-char 38))
                (let ((shut (position (code-char 59) raw :start at :end (min length (+ at 12)))))
                  (if (null shut)
                      (progn (write-char char out) (incf at))
                      (multiple-value-bind (plain found) (entity-text (subseq raw (1+ at) shut))
                        (if found
                            (progn (write-string plain out) (setf at (1+ shut)))
                            (progn (write-char char out) (incf at)))))))
               (t (write-char char out) (incf at))))
    (squash (get-output-stream-string out))))

(defun entity-text (name)
  "NAME, the inside of an entity, as the text it stands for; (values TEXT FOUND)."
  (let ((named (cdr (assoc name *entities* :test #'string-equal))))
    (if named
        (values named t)
        (let* ((hash (and (plusp (length name)) (char= (char name 0) (code-char 35))))
               (hex (and hash (> (length name) 1) (char-equal (char name 1) #\x)))
               (code (and hash (ignore-errors
                                (parse-integer name :start (if hex 2 1)
                                               :radix (if hex 16 10))))))
          (if (and code (< 0 code 1114112))
              (values (string (code-char code)) t)
              (values "" nil))))))

(defun squash (text)
  "TEXT with runs of spaces made one, three newlines made two, ends trimmed."
  (let ((flat (map 'string (lambda (char)
                              (if (member char '(#\Tab #\Return)) #\Space char))
                    text))
        (out (make-string-output-stream)))
    (loop with last = nil
          for char across flat
          do (cond ((and (char= char #\Space) (eq last :space)))
                   ((char= char #\Newline)
                    (write-char #\Newline out)
                    (setf last (if (eq last :newlines) :blank :newlines)))
                   ((and (char= char #\Space) (member last '(:newlines :blank))))
                   (t (write-char char out)
                      (setf last (if (char= char #\Space) :space :other)))))
    (string-trim '(#\Space #\Tab #\Newline #\Return) (get-output-stream-string out))))

;;; --- the collection ---------------------------------------------------------------

(defun deck-names ()
  "Every deck in the collection, as names."
  (let ((result (as-list (call "deckNames"))))
    (unless result (fail "AnkiConnect returned no deck list."))
    (mapcar #'text result)))

(defun ensure-deck (name)
  "Create deck NAME when the collection does not have it."
  (unless (member name (deck-names) :test #'string=)
    (call "createDeck" (nlk:json-object "deck" name)))
  name)

(defun model-field-names (model)
  "The fields of note model MODEL, in order."
  (let ((result (as-list (call "modelFieldNames" (nlk:json-object "modelName" model)))))
    (unless result
      (fail "Anki note model `~a` was not found or has no fields; create it in Anki, or name one that exists."
            model))
    (mapcar #'text result)))

(defun ids-of (result what)
  "RESULT as a list of integers, or a refusal naming WHAT."
  (let ((items (as-list result)))
    (unless items (fail "AnkiConnect returned no ~a." what))
    (remove nil (mapcar (lambda (id) (ignore-errors (parse-integer (text id)))) items))))

(defun find-cards (query)
  "The ids of the cards QUERY matches, in Anki's own search syntax."
  (ids-of (call "findCards" (nlk:json-object "query" query)) "card list"))

(defun find-notes (query)
  "The ids of the notes QUERY matches."
  (ids-of (call "findNotes" (nlk:json-object "query" query)) "note list"))

(defun cards-info (ids)
  "The cards IDS, as Anki describes them."
  (if (null ids)
      '()
      (remove-if-not #'hash-table-p
                     (as-list (call "cardsInfo"
                                    (nlk:json-object "cards" (coerce ids 'vector)))))))

(defun notes-info (ids)
  "The notes IDS, as Anki describes them."
  (if (null ids)
      '()
      (remove-if-not #'hash-table-p
                     (as-list (call "notesInfo"
                                    (nlk:json-object "notes" (coerce ids 'vector)))))))

(defun add-notes (notes)
  "Add NOTES, each a JSON object; => one id or NIL per note."
  (if (null notes)
      '()
      (let ((result (as-list (call "addNotes" (nlk:json-object "notes" (coerce notes 'vector))))))
        (unless result (fail "AnkiConnect returned an unexpected addNotes result."))
        (mapcar (lambda (id) (and id (not (eq id :null)) id)) result))))

(defun change-deck (ids deck)
  "Move the cards IDS into DECK."
  (when ids
    (call "changeDeck" (nlk:json-object "cards" (coerce ids 'vector) "deck" deck))))

(defun ensure-notes-in-deck (note-ids deck)
  "Move the cards of NOTE-IDS into DECK; => how many were moved."
  ;; Some AnkiConnect builds ignore deckName when notes are added.  changeDeck
  ;; is stable across those versions, so the cards are verified afterwards and
  ;; the strays repaired instead of trusting the add answer.
  (let ((ids '()))
    (dolist (note (notes-info note-ids))
      (dolist (card (as-list (field note "cards")))
        (let ((id (ignore-errors (parse-integer (text card)))))
          (when id (push id ids)))))
    (let ((strays '()))
      (dolist (card (cards-info ids))
        (let ((id (field card "cardId")))
          (when (and (integerp id) (not (string= (text (field card "deckName")) deck)))
            (push id strays))))
      (when strays (change-deck strays deck))
      (length strays))))

(defun answer-cards (answers)
  "Grade ANSWERS, each a card id and an ease; => Anki's own verdict per answer."
  (let ((result (as-list (call "answerCards"
                               (nlk:json-object "answers" (coerce answers 'vector))))))
    (unless result
      (fail "AnkiConnect returned no answerCards result; update AnkiConnect if grading is unavailable."))
    result))

(defun num-reviewed-today ()
  "How many cards were reviewed today, when AnkiConnect can say."
  (let ((result (call "getNumCardsReviewedToday")))
    (when (hash-table-p result)
      (setf result (or (field result "today") (field result "count") 0)))
    (typecase result
      (integer result)
      (string (ignore-errors (parse-integer result)))
      (t nil))))

(defun status ()
  "The collection's shape: decks, what is due, what is new, what was reviewed."
  ;; Anki's `is:due` includes cards in the learning queue and, on some
  ;; versions, new cards, so the new count is subtracted to keep them disjoint.
  (let ((decks (deck-names))
        (due (length (find-cards "is:due -is:new")))
        (new (length (find-cards "is:new"))))
    (list :decks decks :due due :new new
          :reviewed (ignore-errors (num-reviewed-today)))))

(defun deck-query (deck)
  "DECK as an Anki search term, its quotes and backslashes escaped."
  (let ((out (make-string-output-stream))
        (quote-char (code-char 34))
        (slash (code-char 92)))
    (loop for char across deck
          do (cond ((char= char quote-char) (write-char slash out) (write-char char out))
                   ((char= char slash) (write-char slash out) (write-char char out))
                   (t (write-char char out))))
    (format nil "deck:~c~a~c" quote-char (get-output-stream-string out) quote-char)))

(defun due-cards (deck &key (include-new t) (limit 20))
  "The cards ready to review, in DECK or across the whole collection."
  (let ((named (and deck (plusp (length deck))))
        (ids '()))
    (push (if named (format nil "~a is:due" (deck-query deck)) "is:due") ids)
    (when include-new
      (push (if named (format nil "~a is:new" (deck-query deck)) "is:new") ids))
    (let ((card-ids '()))
      (dolist (query (nreverse ids))
        (dolist (id (find-cards query))
          (pushnew id card-ids)))
      (setf card-ids (nreverse card-ids))
      (cards-info (if (> (length card-ids) limit) (subseq card-ids 0 limit) card-ids)))))

(defun search-notes (query &key (limit 20))
  "The notes QUERY matches, at most LIMIT of them."
  (let ((ids (find-notes query)))
    (notes-info (if (> (length ids) limit) (subseq ids 0 limit) ids))))

(defun card-object (card)
  "CARD as the JSON object a model reads: front and back as plain text."
  (let* ((fields (fields-of (field card "fields")))
         (front (field card "question"))
         (back (field card "answer")))
    (unless front (setf front (second (first fields))))
    (unless back (setf back (second (second fields))))
    (nlk:json-object
     "card_id" (field card "cardId")
     "deck" (text (field card "deckName"))
     "model" (text (field card "modelName"))
     "front" (strip-html front)
     "back" (strip-html back)
     "type" (or (cdr (assoc (field card "type") *type-labels*)) (text (field card "type")))
     "due" (field card "due")
     "interval" (field card "interval")
     "reps" (field card "reps")
     "lapses" (field card "lapses"))))
