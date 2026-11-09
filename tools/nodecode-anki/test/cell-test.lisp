
;;;; cell-test.lisp --- lifecycle, the five tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Anki: the config
;;;; surface, the tools through the real dispatch seam against a stubbed
;;;; AnkiConnect, every member they send, the text a card field becomes, and
;;;; the error a broken or refusing collection answers.
(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------
(define-cell-lifecycle-tests "anki"
  (:config "anki_url" +url+)
  (:help :anki)
  (:command "anki")
  (:running
   (is (nle::find-tool "anki_decks") "anki_decks is registered")
   (is (nle::find-tool "anki_add_cards") "anki_add_cards is registered")
   (is (nle::find-tool "anki_search") "anki_search is registered")
   (is (nle::find-tool "anki_due") "anki_due is registered")
   (is (nle::find-tool "anki_answer") "anki_answer is registered")
   (is (equal +url+ (nodecode-anki::setting :anki-url)) "the endpoint the section carries")
   (is (equal "Default" (nodecode-anki::setting :default-deck)) "and the deck a bare call gets"))
  (:stopped
   (is (null (nle::find-tool "anki_decks")) "stop unregisters the tools")
   (is (null (nle::find-tool "anki_answer")) "every one of them"))
  (:refused
   ("anki_url" "")
   ("anki_url" "127.0.0.1:8765")
   ("request_timeout" 0)
   ("max_cards" 0)))
(deftest anki-cell-a-blank-endpoint-says-what-to-set ()
  ;; The one setting a call cannot work without: a section naming no endpoint
  ;; refuses at start, in words that name the key.
  (let ((condition (signals-error nlk:config-refusal (anki-start "anki_url" ""))))
    (is (search "no endpoint" (princ-to-string condition)) "the refusal says what is missing"))
  (let ((condition (signals-error nlk:config-refusal
                                  (anki-start "anki_url" "127.0.0.1:8765"))))
    (is (search "http://" (princ-to-string condition)) "and what a good one looks like"))
  (is (null (nle::find-tool "anki_decks")) "and nothing was installed"))

;;; --- the read tools ---------------------------------------------------------------
(defparameter +decks+ "[\"Default\", \"Spanish\"]")
(defparameter +cards+ "[1, 2, 3]")
(defparameter +due-card+
  "{\"cardId\": 9, \"deckName\": \"Spanish\", \"modelName\": \"Basic\", \"question\": \"<style>.card {}</style><b>aprovechar</b>\", \"answer\": \"to make the most of &amp; more\", \"type\": 2, \"due\": 5, \"interval\": 4, \"reps\": 3, \"lapses\": 0}"
  "One due card, its question wrapped as Anki renders it.")
(deftest anki-cell-the-decks-tool-reports-the-collection (with-anki ()
  (answer-as "deckNames" +decks+)
  (answer-as "findCards" +cards+)
  (answer-as "getNumCardsReviewedToday" "7")
  (let ((text (anki-call-tool "anki_decks" "{}")))
    (is (search "\"Spanish\"" text) "the deck names are there")
    (is (search "\"due\":3" text) "with how many are scheduled")
    (is (search "\"new\":3" text) "how many are new")
    (is (search "\"reviewed_today\":7" text) "and how many were reviewed today"))
  (is (equal +url+ (getf (call-of "deckNames") :url)) "one endpoint, the section's")
  (is (equal "is:due -is:new" (asked (car (last (calls-of "findCards"))) "params" "query")) "the due count leaves new cards out")))
(deftest anki-cell-due-cards-arrive-as-plain-text (with-anki ()
  (answer-as "findCards" "[9]")
  (answer-as "cardsInfo" (format nil "[~a]" +due-card+))
  (let ((text (anki-call-tool "anki_due" "{\"deck\":\"Spanish\"}")))
    (is (search "\"front\":\"aprovechar\"" text) "the question, its style block and tags gone")
    (is (null (search "<b>" text)) "no tag survives")
    (is (null (search ".card" text)) "and the card's CSS is not text")
    (is (search "\"back\":\"to make the most of & more\"" text) "the answer, its entity decoded")
    (is (search "\"type\":\"review\"" text) "Anki's card type in words"))
  (is (equal "deck:\"Spanish\" is:due" (asked (car (last (calls-of "findCards"))) "params" "query")) "a named deck is an Anki search term")))
(deftest anki-cell-search-answers-note-fields-as-text (with-anki ()
  (answer-as "findNotes" "[7]")
  (answer-as "notesInfo"
             "[{\"noteId\": 7, \"modelName\": \"Basic\", \"tags\": [\"verbs\"], \"cards\": [9], \"fields\": {\"Front\": {\"value\": \"<i>aprovechar</i>\", \"order\": 0}}}]")
  (let ((text (anki-call-tool "anki_search" "{\"query\":\"tag:verbs\",\"limit\":5}")))
    (is (search "\"aprovechar\"" text) "the field, tags dropped")
    (is (null (search "<i>" text)) "no markup survives")
    (is (search "\"verbs\"" text) "the note's tags")
    (is (search "\"count\":1" text) "one note found"))
  (is (equal "tag:verbs" (asked (call-of "findNotes") "params" "query")) "the query rode the request")))

;;; --- writing to the collection ------------------------------------------------------
(deftest anki-cell-add-cards-creates-notes-and-reports-duplicates (with-anki ()
  (answer-as "deckNames" +decks+)
  (answer-as "modelFieldNames" "[\"Front\", \"Back\"]")
  (answer-as "addNotes" "[101, null]")
  (answer-as "notesInfo" "[{\"noteId\": 101, \"cards\": [1001]}]")
  (answer-as "cardsInfo" "[{\"cardId\": 1001, \"deckName\": \"Spanish\"}]")
  (let ((text (anki-call-tool "anki_add_cards"
                              "{\"deck\":\"Spanish\",\"tags\":[\"verbs\"],\"cards\":[{\"front\":\"<b>aprovechar</b>\",\"back\":\"to make the most of\"},{\"front\":\"hola\",\"back\":\"hello\"}]}")))
    (is (search "\"added\":1" text) "one note went in")
    (is (search "\"duplicates\":1" text) "and the duplicate Anki refused is reported, not forced")
    (is (search "\"note_ids\":[101]" text) "with the id Anki gave it"))
  (let ((add (call-of "addNotes")))
    (is (null (calls-of "createDeck")) "an existing deck is not created again")
    (is (equal "Spanish" (asked add "params" "notes" 0 "deckName")) "the note rides the named deck")
    (is (equal "<b>aprovechar</b>" (asked add "params" "notes" 0 "fields" "Front")) "the front becomes the model's first field")
    (is (equal "to make the most of" (asked add "params" "notes" 0 "fields" "Back")) "the back its second")
    (is (equal "verbs" (asked add "params" "notes" 0 "tags" 0)) "the call's tag is applied")
    (is (null (at add "params" "notes" 0 "options" "allowDuplicate")) "duplicates are refused by Anki, not forced"))))
(deftest anki-cell-add-cards-creates-a-deck-that-is-not-there (with-anki ()
  (answer-as "deckNames" +decks+)
  (answer-as "modelFieldNames" "[\"Front\", \"Back\"]")
  (answer-as "addNotes" "[102]")
  (answer-as "notesInfo" "[]")
  (anki-call-tool "anki_add_cards"
                   "{\"deck\":\"Biology\",\"cards\":[{\"front\":\"mitosis\",\"back\":\"cell division\"}]}")
  (let ((create (call-of "createDeck")))
    (is (not (null create)) "a named deck the collection lacks is created")
    (is (equal "Biology" (asked create "params" "deck")) "with the name the call asked for"))))
(deftest anki-cell-a-tag-with-a-space-is-refused-before-anything-is-written (with-anki ()
  (let ((text (anki-call-tool "anki_add_cards"
                               "{\"cards\":[{\"front\":\"a\",\"back\":\"b\"}],\"tags\":[\"two words\"]}")))
    (is (search "whitespace" text) "the refusal says what is wrong with the tag"))
  (is (null (call-of "addNotes")) "and nothing reached the collection")))
(deftest anki-cell-answer-grades-the-card-and-reschedules-it (with-anki ()
  (answer-as "answerCards" "[true]")
  (answer-as "cardsInfo"
             "[{\"cardId\": 42, \"deckName\": \"Spanish\", \"interval\": 4, \"due\": 100, \"reps\": 3, \"lapses\": 0}]")
  (let ((text (anki-call-tool "anki_answer" "{\"card_id\":42,\"ease\":\"good\"}")))
    (is (search "\"ease_name\":\"good\"" text) "the grade is named back")
    (is (search "\"next_interval\":4" text) "with the schedule Anki gave it"))
  (is (equal "3" (asked (call-of "answerCards") "params" "answers" 0 "ease")) "good is the third ease Anki takes")))

;;; --- a collection that refuses, and the command -------------------------------------
(deftest anki-cell-a-collection-that-refuses-answers-an-error-member (with-anki ()
  (setf *body* "{\"result\": null, \"error\": \"collection is not available\"}")
  (let ((text (anki-call-tool "anki_decks" "{}")))
    (is (search "\"error\"" text) "the tool answers an error member, it does not throw")
    (is (search "collection is not available" text) "in the collection's own words")
    (is (search "AnkiConnect log" text) "and says where to look"))))
(deftest anki-cell-an-unknown-ease-is-refused-at-the-boundary (with-anki ()
  (let ((text (anki-call-tool "anki_answer" "{\"card_id\":42,\"ease\":\"perfect\"}")))
    (is (search "again" text) "the refusal names the grades it takes"))
  (is (null (call-of "answerCards")) "and nothing was graded")))
(deftest anki-cell-the-command-reports-and-explains-itself (with-anki ()
  (answer-as "deckNames" +decks+)
  (answer-as "findCards" "[1]")
  (answer-as "getNumCardsReviewedToday" "4")
  (let ((text (cell-entry "nodecode-anki" "anki" "status" "anki-cell-1")))
    (is (search "Decks: Default, Spanish" text) "/anki status names the decks")
    (is (search "Reviewed today: 4" text) "and what today has done"))
  (is (search "Usage: /anki" (cell-entry "nodecode-anki" "anki" "help" "anki-cell-1")) "a bare /anki explains itself")))
(deftest anki-cell-strip-html-keeps-the-card-s-words ()
  (is (equal "a & b" (nodecode-anki:strip-html "<div>a &amp; b</div>")) "a div is a break, not text")
  (is (equal "one" (nodecode-anki:strip-html "<script>var x = 1;</script>one")) "a script block is not the card")
  (is (equal "one's two" (nodecode-anki:strip-html "one&#39;s two")) "a numeric entity is decoded"))
