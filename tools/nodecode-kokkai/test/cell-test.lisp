;;;; cell-test.lisp --- the kokkai cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- lifecycle, settings and the tools' shape ------------------------------------

(define-cell-lifecycle-tests "kokkai"
  (:help :kokkai)
  (:command "kokkai")
  (:running
   (is (nle::find-tool "kokkai_search") "the search tool is registered")
   (is (nle::find-tool "kokkai_speech") "the speech tool is registered")
   (is (nle::find-tool "kokkai_watch") "and the watch tool")
   (is (= 9 (nodecode-kokkai::setting :hour)) "the daily check defaults to the ninth hour")
   (is (= 30 (nodecode-kokkai::setting :timeout-seconds)) "and one request may wait thirty seconds"))
  (:stopped
   (is (null (nle::find-tool "kokkai_search")) "stop takes the tools back out"))
  (:refused ("hour" 25) ("lookback_days" 0) ("interval_minutes" 0))
  (:idle nodecode-kokkai:kokkai-error
         (nodecode-kokkai:meetings "" "岸田文雄" "" "" "" 10)))

(deftest kokkai-cell-refuses-a-watch-call-without-an-action ()
  ;; The one member the declaration requires: without it the call never
  ;; reaches the body, which is the repair the model reads.
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-kokkai::kokkai-watch-arguments)
                              (nlk:make-json-object "any" "予算")))))

;;; --- the meeting list -------------------------------------------------------------

(deftest kokkai-cell-searches-the-meeting-list ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page (kk-meeting
                                                 :speeches (list (kk-speech)
                                                                 (kk-speech :id "b" :speaker "林芳正")))))
      (let ((text (kk-call "kokkai_search" "any" "予算")))
        (is (search "Japan Diet meetings: 1" text) "the count comes first")
        (is (search "予算委員会" text) "the meeting is named")
        (is (search "https://kokkai.ndl.go.jp/txt/121305261X00820240603/0" text) "with its own meetingURL")
        (is (search "岸田文雄, 林芳正" text) "and the speakers the page named")
        (is (search "出典：" text) "and the credit line the licence asks for")
        (is (= 1 (length calls)) "one request answers one search")
        (is (equal "予算" (kk-param (first calls) "any")) "the words travel")
        (is (equal "1" (kk-param (first calls) "startRecord")) "from the first record")))))

(deftest kokkai-cell-refuses-a-page-that-does-not-hold-every-match ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-short-page 40 (kk-meeting)))
      (let ((text (kk-call "kokkai_search" "any" "予算")))
        (is (search "ERROR" text) "the call says so")
        (is (search "40 meetings match" text) "with the total")
        (is (search "not the whole result" text) "and never as 0 meetings")))))

(deftest kokkai-cell-refuses-what-the-api-would-drop ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page))
      (is (signals-error nlk:contract-refusal
            (kk-call "kokkai_search" "any" "予算" "house" "下院")) "a house outside the four is refused before a request")
      (is (search "together" (kk-call "kokkai_search" "any" "予算" "from" "2024-06-03")) "from without until is refused")
      (is (search "after"
                  (kk-call "kokkai_search" "any" "予算" "from" "2024-06-04" "until" "2024-06-03")) "an inverted range is refused")
      (is (null calls) "and none of them was sent"))))

;;; --- the speech search -------------------------------------------------------------

(defun long-speech-text ()
  "A speech longer than one excerpt, in the characters Japanese uses."
  (with-output-to-string (out)
    (dotimes (index (+ nodecode-kokkai::+excerpt-chars+ 40))
      (write-char (if (zerop (mod index 10)) #\Space #\あ) out))))

(deftest kokkai-cell-reads-speech-excerpts ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-json
                                         "numberOfRecords" 120 "numberOfReturn" 1
                                         "speechRecord" (vector (kk-speech :text (long-speech-text)))))
      (let ((text (kk-call "kokkai_speech" "speaker" "岸田文雄")))
        (is (search "120 in total" text) "the total comes first")
        (is (search "truncated" text) "with the truncation mark")
        (is (search "…" text) "and the excerpt's own cut")
        (is (search "speechID=121305261X00820240603_001" text) "and the speech URL to read the rest")
        (is (search "出典：" text) "and the credit line")
        (is (equal "speaker" (car (first (remove-if-not (lambda (pair) (equal "岸田文雄" (cdr pair)))
                                                         (kk-http-params (first calls)))))) "the speaker travels as a name")))))

(deftest kokkai-cell-refuses-a-speech-search-with-no-words-or-name ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page))
      (is (search "speaker or any" (kk-call "kokkai_speech" "house" "参議院")) "the repair is named")
      (is (null calls) "and nothing was sent"))))

;;; --- the floor between requests ----------------------------------------------------

(deftest kokkai-cell-keeps-the-five-second-floor ()
  (with-kokkai ()
    (let ((calls '()))
      (with-stubbed-fdefinition (nodecode-kokkai::http (url params timeout)
                                   (declare (ignore url params timeout))
                                   (push :sent calls)
                                   (values (kk-page (kk-meeting)) 200))
        (setf nodecode-kokkai::*gap-seconds* 5.0
              nodecode-kokkai::*last-done* nil)
        (kk-call "kokkai_search" "any" "予算")
        (let ((text (kk-call "kokkai_search" "any" "予算")))
          (is (search "ERROR" text) "a second call right away is refused")
          (is (search "wait" text) "and says how long to wait")
          (is (= 1 (length calls)) "so only one request was sent"))))))

;;; --- the watches -------------------------------------------------------------------

(deftest kokkai-cell-saves-lists-and-removes-a-watch ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page (kk-meeting :speeches (list (kk-speech)))))
      (let ((saved (kk-call "kokkai_watch" "action" "add" "any" "予算")))
        (is (search "Watch" saved) "the watch is named")
        (is (search "1 speech IDs already" saved) "and what the window already held is marked seen")
        (is (equal "100" (kk-param (first calls) "maximumRecords")) "the watch reads one full page")))
    (let ((listed (kk-call "kokkai_watch" "action" "list")))
      (is (search "1 watch saved" listed) "the list shows it")
      (is (search "any \"予算\"" listed) "with its words"))
    (let* ((watch (first (nodecode-kokkai::watches)))
           (id (nodecode-kokkai::json-text watch "id" ""))
           (gone (kk-call "kokkai_watch" "action" "remove" "watch_id" id)))
      (is (search "removed" gone) "remove takes it away")
      (is (search "No watch is saved" (kk-call "kokkai_watch" "action" "list")) "and the list says so"))))

(deftest kokkai-cell-announces-only-what-is-new ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page (kk-meeting :speeches (list (kk-speech)))))
      (kk-call "kokkai_watch" "action" "add" "any" "予算")
      (is (search "nothing new" (kk-call "kokkai_watch" "action" "run")) "the add's own page is not news"))
    (with-scripted-kokkai (calls :body (kk-page (kk-meeting
                                                 :speeches (list (kk-speech)
                                                                 (kk-speech :id "new" :speaker "林芳正")))))
      (let ((run (kk-call "kokkai_watch" "action" "run")))
        (is (search "1 new speech" run) "a speech that came after it is news")
        (is (search "121305261X00820240603/0" run) "with the meeting to read")
        (is (search "1 new speech" (kk-call "kokkai_watch" "action" "run")) "and a manual run says it again, since only the daily check marks it seen")))))

(deftest kokkai-cell-add-refuses-without-words-or-a-name ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page))
      (is (search "needs any or speaker" (kk-call "kokkai_watch" "action" "add" "house" "参議院")) "a watch needs words or a name")
      (is (null calls) "and nothing was sent"))))

;;; --- the command --------------------------------------------------------------------

(deftest kokkai-cell-command-searches ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page (kk-meeting)))
      (let ((text (cell-entry "nodecode-kokkai" "kokkai" "憲法" nil)))
        (is (search "Japan Diet meetings: 1" text) "the command answers the same block")
        (is (equal "憲法" (kk-param (first calls) "any")) "with the words typed")))))

(deftest kokkai-cell-bare-command-asks-for-words ()
  (with-kokkai ()
    (with-scripted-kokkai (calls :body (kk-page))
      (multiple-value-bind (text dialog)
          (cell-entry "nodecode-kokkai" "kokkai" "" nil)
        (is (search "Japan Diet" text) "the answer says what /kokkai does")
        (is (search "Words" (nlk:encode-json-object dialog)) "and offers a row to type them in")
        (is (null calls) "asking costs no request")))))
