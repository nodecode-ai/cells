;;;; support.lisp --- the kokkai test runner and its scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; KOKKAI-CELL- prefix, and RUN-KOKKAI-TESTS runs exactly that slice.
;;;;
;;;; The one network call is NODECODE-KOKKAI::HTTP; WITH-SCRIPTED-KOKKAI
;;;; stubs that name, records every call and answers scripted JSON, so no
;;;; test ever opens a socket. The watches are kept in a file of the test's
;;;; own, never in the operator's home.

(in-package #:nodecode.test)

(define-test-slice "kokkai" "KOKKAI-CELL-" :start nodecode-kokkai:start-cell)

;;; --- config and lifecycle fixtures ----------------------------------------------

(defun scratch-state-file ()
  "A watch state file of this test's own, in a directory made for it."
  (merge-pathnames
   "state.json"
   (ensure-directories-exist
    (merge-pathnames (format nil "kokkai-test-~36r/" (random (expt 36 8)))
                     (uiop:temporary-directory)))))

(defmacro with-kokkai ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS, its watches in a scratch
file, and stopped on unwind."
  `(let ((nodecode-kokkai::*state-file* (scratch-state-file))
         (nodecode-kokkai::*state* nil))
     (with-cell-stop ((kokkai-start ,@config-pairs))
       ,@body)))

;;; --- the scripted seam -----------------------------------------------------------

(defun kk-json (&rest specs)
  "A JSON text body built from alternating key/value pairs."
  (nlk:encode-json-object (apply #'nlk:make-json-object specs)))

(defun kk-object (&rest specs)
  "One JSON object built from alternating key/value pairs."
  (apply #'nlk:make-json-object specs))

(defun kk-page (&rest records)
  "The meeting list's answer for RECORDS: one page that holds them all."
  (kk-json "numberOfRecords" (length records)
           "numberOfReturn" (length records)
           "meetingRecord" (coerce records 'vector)))

(defun kk-short-page (total &rest records)
  "A page that holds RECORDS of TOTAL matches."
  (kk-json "numberOfRecords" total
           "numberOfReturn" (length records)
           "meetingRecord" (coerce records 'vector)))

(defun kk-meeting (&key (house "衆議院") (name "予算委員会") (session "第213回国会")
                      (issue "第8号") (date "2024-06-03")
                      (url "https://kokkai.ndl.go.jp/txt/121305261X00820240603/0")
                      (speeches '()))
  "One meeting row, with its speech rows folded inside it."
  (kk-object "nameOfHouse" house "nameOfMeeting" name "session" session
              "issue" issue "date" date "meetingURL" url
              "speechRecord" (coerce speeches 'vector)))

(defun kk-speech (&key (id "121305261X00820240603_001") (speaker "岸田文雄")
                        (meeting "予算委員会") (date "2024-06-03")
                        (url "https://kokkai.ndl.go.jp/#/detail?speechID=121305261X00820240603_001")
                        (text "これより予算委員会を開会いたします。"))
  "One speech row."
  (kk-object "speechID" id "speaker" speaker "nameOfMeeting" meeting
              "date" date "speechURL" url "speech" text))

(defstruct (kk-http (:copier nil))
  url params timeout)

(defun kk-param (call name)
  "The value CALL asked for under NAME, or NIL."
  (cdr (assoc name (kk-http-params call) :test #'string=)))

(defmacro with-scripted-kokkai ((calls &key body) &body forms)
  "Run FORMS with the cell's one network call stubbed: every call recorded
into the list CALLS, oldest first, and BODY -- a JSON string, or a function
of the call answering one -- answered as HTTP 200. The API's five-second
floor is lifted so a test may make two calls."
  `(let ((,calls '())
         (nodecode-kokkai::*gap-seconds* 0.0)
         (nodecode-kokkai::*last-done* nil))
     (with-stubbed-fdefinition (nodecode-kokkai::http (url params timeout)
                                  (let ((call (make-kk-http :url url :params params
                                                             :timeout timeout)))
                                    (setf ,calls (append ,calls (list call)))
                                    (values (if (functionp ,body) (funcall ,body call) ,body)
                                            200)))
       ,@forms)))

;;; --- calling what the cell contributes -------------------------------------------

(defun kk-call (name &rest pairs)
  "The registered tool NAME, called the way the engine calls it: the
declaration materializes the record, the body answers the text."
  (let ((entry (nle::find-tool name)))
    (assert entry () "the ~a tool is not registered" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))
