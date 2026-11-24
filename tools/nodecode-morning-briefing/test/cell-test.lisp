;;;; cell-test.lisp --- lifecycle, the page, the record, the refusal without a place.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stub: the weather service's two reads and each
;;;; subreddit's Atom feed answer from fixtures, and nothing reaches the network.

(in-package #:nodecode.test)

(define-test-slice "morning-briefing" "MORNING-BRIEFING-CELL-"
  :start nodecode-morning-briefing:start-cell
  :defaults (("latitude" "44.05") ("longitude" "-121.31")))

(define-cell-lifecycle-tests "morning-briefing"
  (:config "latitude" "44.05" "longitude" "-121.31")
  (:help :briefing)
  (:command "briefing")
  (:refused ("hours" 0) ("latitude" 5))
  (:idle nodecode-morning-briefing:morning-briefing-error
         (nodecode-morning-briefing:build-briefing)))

;;; --- fixtures --------------------------------------------------------------------

(defun briefing-iso (universal)
  "UNIVERSAL as an Atom timestamp, UTC."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d+00:00"
            year month day hour minute second)))

(defun briefing-atom (title universal)
  "A one-entry Atom feed, as a subreddit's .rss answers."
  (format nil (concatenate 'string
                            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%"
                            "<feed xmlns=\"http://www.w3.org/2005/Atom\">~%<title>r/test</title>~%"
                            "<entry><title>~a</title><link href=\"https://example.org/story\"/>"
                            "<updated>~a</updated><id>urn:1</id></entry>~%</feed>")
          title (briefing-iso universal)))

(defmacro with-stubbed-briefing (&body body)
  "BODY with nlk:http answering the weather service and each feed from fixtures."
  `(let ((now (get-universal-time)))
     (with-stubbed-fdefinition (nlk:http (method url &rest keys)
       (declare (ignore method keys))
       (cond ((search "api.weather.gov/points/" url)
              (values "{\"properties\":{\"forecast\":\"https://api.weather.gov/gridpoints/TST/1,1/forecast\"}}"
                      200 nil nil))
             ((search "api.weather.gov/gridpoints/" url)
              (values (concatenate 'string
                                   "{\"properties\":{\"periods\":["
                                   "{\"name\":\"Today\",\"temperature\":72,\"temperatureUnit\":\"F\","
                                   "\"shortForecast\":\"Sunny\",\"windSpeed\":\"5 mph\",\"windDirection\":\"NW\","
                                   "\"detailedForecast\":\"Sunny, with a high near 72.\"},"
                                   "{\"name\":\"Tonight\",\"temperature\":48,\"temperatureUnit\":\"F\","
                                   "\"shortForecast\":\"Clear\"}]}}")
                      200 nil nil))
             ((search "/r/news/new/.rss" url)
              (values (briefing-atom "Fresh story" (- now 1800)) 200 nil nil))
             ((search "/r/stale/new/.rss" url)
              (values (briefing-atom "Old story" (- now (* 48 3600))) 200 nil nil))
             (t (error "unexpected request to ~a" url))))
       ,@body)))

;;; --- the page --------------------------------------------------------------------

(deftest morning-briefing-cell-writes-one-newsprint-page (with-stubbed-briefing)
  (with-temp-workspace (dir)
    (with-cell-stop ((morning-briefing-start "latitude" "44.05" "longitude" "-121.31"
                                              "place" "Test Place"
                                              "output_dir" (namestring dir)))
      (multiple-value-bind (text path) (nodecode-morning-briefing:build-briefing :topics '("news") :hours 36 :max-items 12)
        (is (search "Test Place" text) "the dateline carries the place")
        (is (search "72" text) "the temperature")
        (is (search "Sunny" text) "and the sky")
        (is (search "Fresh story" text) "a headline inside the window")
        (is (not (search "Old story" text)) "and nothing two days old")
        (is-present (file (probe-file path)) "the page is written"
          (let ((html (uiop:read-file-string file)))
            (is (search "<!DOCTYPE html>" html) "as one HTML document")
            (is (search "Fresh story" html) "holding the same headline")
            (is (search "The Morning Briefing" html) "under the masthead")))))))

(deftest morning-briefing-tool-answers-through-its-contract (with-stubbed-briefing)
  (with-temp-workspace (dir)
    (with-cell-stop ((morning-briefing-start "latitude" "44.05" "longitude" "-121.31"
                                              "output_dir" dir))
      (let* ((arguments (nodecode-morning-briefing::decode-briefing-arguments
                         (shasht:read-json "{\"topics\":[\"news\"],\"hours\":36}")))
             (text (nodecode-morning-briefing:brief arguments)))
        (is (search "Fresh story" text) "the topic the call names is the feed read")
        (is (search "Page: " text) "and it answers where the page is")))))

(deftest morning-briefing-command-answers-the-same-page (with-stubbed-briefing)
  (with-temp-workspace (dir)
    (with-cell-stop ((morning-briefing-start "latitude" "44.05" "longitude" "-121.31"
                                              "output_dir" (namestring dir)))
      (let ((text (cell-entry "nodecode-morning-briefing" "briefing" "news")))
        (is (search "Morning briefing" text))
        (is (search "Fresh story" text))))))

(deftest morning-briefing-refuses-without-a-place (with-stubbed-briefing)
  (with-cell-stop ((morning-briefing-start "subreddits" (vector "news")))
    (is (signals-error nodecode-morning-briefing:morning-briefing-error
          (nodecode-morning-briefing:build-briefing)) "no latitude and longitude is a refusal")
    (is (search "latitude"
                (handler-case (progn (nodecode-morning-briefing:build-briefing) "")
                  (nodecode-morning-briefing:morning-briefing-error (condition)
                    (princ-to-string condition)))) "that names the setting to set")))
