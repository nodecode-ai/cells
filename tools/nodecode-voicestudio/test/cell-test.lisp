;;;; cell-test.lisp --- the six tools against a stubbed VoiceStudio server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NLK:HTTP is the one seam: every test answers a route's own JSON or bytes,
;;;; and nothing dials a server. Each tool is called the way a model's call
;;;; reaches it -- through the real dispatch, contract and all -- so the shape
;;;; the provider is shown and the body that runs are one.
;;;;
;;;; The files a call reads (a clip, a script, a video) live in a folder of
;;;; this test's own outside the home, because the cell refuses everything
;;;; inside the organism's own folder as the plugin refused its own data.

(in-package #:nodecode.test)

(define-test-slice "voicestudio" "VOICESTUDIO-" :start nodecode-voicestudio:start-cell
  :defaults (("base_url" "http://vs.test:3900")))

;;; --- the lifecycle, from the one declaration ------------------------------------------

(define-cell-lifecycle-tests "voicestudio"
  (:config "base_url" "http://vs.test:3900"
           "directory" (namestring (nlk:home "voicestudio-lifecycle/")))
  (:help :voicestudio)
  (:command "vs-speak")
  (:running (is (equal "http://vs.test:3900" (nodecode-voicestudio::setting :base-url)) "the section's own address")
            (is-present (entry (nle::find-tool "vs_speak"))
              "vs_speak is registered under the plugin's own name"
              (is (equal "vs_speak" (first entry))))
            (is-present (entry (nle::find-tool "vs_longform")) "and so is vs_longform")
            (is-present (entry (nle::find-registered-command "voicestudio"))
              "the cell's own command carries the folder's name"
              (is (equal "nodecode-voicestudio" (nle::slash-command-owner entry)))))
  (:stopped (is (null (nle::find-tool "vs_speak")) "stop takes the tools back out")
            (is (null (nle::find-registered-command "vs-status")) "and the commands"))
  (:refused ("base_url" "127.0.0.1:3900")))

;;; --- fixtures --------------------------------------------------------------------

(defun vs-directory ()
  "A folder of this test's own under the scratch home, for the audio written."
  (merge-pathnames (format nil "voicestudio-~8,'0x/" (random (expt 16 8))) (nlk:home)))

(defun vs-input (name)
  "A file of this test's own outside the home: what a user would point at."
  (let ((file (merge-pathnames (format nil "voicestudio-input-~8,'0x/" (random (expt 16 8)))
                                (uiop:temporary-directory))))
    (ensure-directories-exist file)
    (merge-pathnames name file)))

(defun vs-text-file (name text)
  "NAME holding TEXT, in a folder of this test's own."
  (let ((file (vs-input name)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string text out))
    file))

(defmacro with-voicestudio ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, in a folder of its own, stopped and
emptied afterwards."
  `(with-cell-stop ((voicestudio-start "base_url" "http://vs.test:3900"
                                        "directory" (namestring (vs-directory))
                                        ,@pairs))
     (let ((directory (nodecode-voicestudio::setting :directory)))
       (nlk:with-cleanup ((uiop:delete-directory-tree directory :validate t
                                                      :if-does-not-exist :ignore))
         ,@body))))

(defmacro with-vs-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL: ROUTES are (FRAGMENT FORM), tried in
order, each FORM run with METHOD, URL and OPTIONS bound. ASKED collects
(METHOD URL CONTENT), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected VoiceStudio request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun vs-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun vs-called (asked fragment)
  "The calls of ASKED whose URL carries FRAGMENT."
  (remove-if-not (lambda (call) (search fragment (second call))) asked))

(defun vs-file (answer)
  "The path ANSWER's first line names, without the size after it."
  (let* ((line (subseq answer (min 6 (length answer))
                        (or (position #\Newline answer) (length answer))))
         (at (search " (" line)))
    (subseq line 0 (or at (length line)))))

(defun vs-json (text)
  "TEXT, a JSON body a test sent, decoded."
  (nlk:decode-json (if (stringp text) text (nlk:body-text text 65536))))

(defmacro with-vs-no-sleep (&body body)
  "BODY with the wait between two job reads gone: a test never sleeps."
  `(let ((nodecode-voicestudio::*sleep* (lambda (seconds) (declare (ignore seconds)))))
     ,@body))

;;; --- what the server answers ------------------------------------------------------

(defparameter +take+ "ID3-this-is-not-a-real-mp3"
  "One take's bytes, as the server streams them.")

(defparameter +setup+ "{\"models_ready\":true,\"disk_free_gb\":42.5,\"missing\":[]}")
(defparameter +profiles+
  (concatenate 'string
               "{\"profiles\":[{\"id\":\"narrator\",\"name\":\"Narrator\",\"kind\":\"design\"},"
               "{\"id\":\"warm-reader\",\"name\":\"Warm Reader\"}]}"))
(defparameter +gallery+ "[{\"id\":\"g-1\",\"name\":\"Gallery One\"}]")
(defparameter +personalities+ "{\"personalities\":[{\"id\":\"p-1\",\"name\":\"Calm\"}]}")
(defparameter +describe+
  "{\"vd_states\":{\"gender\":\"female\",\"age\":\"young\"},\"unmatched\":[\"slight\"]}")
(defparameter +sse+
  (format nil "event: started~%data: {\"type\":\"started\",\"job_id\":\"job-7\"}~%~%~
               event: done~%data: {\"type\":\"done\",\"job_id\":\"job-7\",\"output\":\"book.m4b\",\"chapters\":2}~%~%")
  "A finished audiobook stream: one started event, one done.")

;;; --- vs_speak ---------------------------------------------------------------------

(deftest voicestudio-speak-writes-the-take-it-made ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/generate" (values +take+ 200
                                              (list (cons "content-type" "audio/mpeg")
                                                    (cons "x-seed" "4242")))))
      (let ((answer (vs-answer "vs-1" "vs_speak" "{\"text\":\"Hello there\",\"voice\":\"narrator\"}")))
        (is (search "wrote " answer) (format nil "the answer names the file it wrote: ~a" answer))
        (let ((file (vs-file answer)))
          (is (probe-file file) "and the file is there")
          (is (equal +take+ (uiop:read-file-string file)) "holding the bytes the server sent"))
        (is (search "seed 4242" answer) "the server's seed rides the answer")
        (let ((content (third (first asked))))
          (is (search "/generate" (second (first asked))) "one POST to /generate")
          (is (equal "Hello there" (cdr (assoc "text" content :test #'string=))) "with the words")
          (is (equal "narrator" (cdr (assoc "profile_id" content :test #'string=))) "the voice as profile_id")
          (is (equal "1.0" (cdr (assoc "speed" content :test #'string=))) "and the pace"))))))

(deftest voicestudio-speak-refuses-when-no-voice-is-named ()
  (with-voicestudio ()
    (with-vs-wire (asked)
      (let ((answer (vs-answer "vs-2" "vs_speak" "{\"text\":\"Hello there\"}")))
        (is (search "no voice chosen" answer) "the refusal says what is missing")
        (is (search "/vs-voices" answer) "and which command lists the voices")
        (is (null asked) "and nothing was sent")))))

(deftest voicestudio-speak-refuses-too-long-a-text-before-speaking ()
  (with-voicestudio ()
    (with-vs-wire (asked)
      (let ((answer (vs-answer "vs-3" "vs_speak"
                                (format nil "{\"text\":\"~a\",\"voice\":\"narrator\"}"
                                        (make-string 6001 :initial-element #\a)))))
        (is (search "6001 characters" answer) "the answer counts the characters")
        (is (search "vs_longform" answer) "and names the tool for a whole script")
        (is (null asked) "nothing is sent")))))

;;; --- vs_list_voices ----------------------------------------------------------------

(deftest voicestudio-lists-the-three-kinds-of-voice ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/setup/status" (values +setup+ 200))
                          ("/gallery/voices" (values +gallery+ 200))
                          ("/personalities" (values +personalities+ 200))
                          ("/profiles" (values +profiles+ 200)))
      (let ((answer (vs-answer "vs-4" "vs_list_voices" "{}")))
        (is (search "4 voices at" answer) (format nil "every kind is counted: ~a" answer))
        (is (search "narrator" answer) "a saved voice with its id")
        (is (search "g-1" answer) "a gallery voice")
        (is (search "p-1" answer) "and a preset")))))

(deftest voicestudio-filters-the-voices-it-lists ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/setup/status" (values +setup+ 200))
                          ("/gallery/voices" (values +gallery+ 200))
                          ("/profiles" (values +profiles+ 200)))
      (let ((answer (vs-answer "vs-5" "vs_list_voices" "{\"search\":\"warm\",\"kind\":\"mine\"}")))
        (is (search "warm-reader" answer) (format nil "the word that matches: ~a" answer))
        (is (not (search "narrator" answer)) "and not the voice it does not")
        (is (null (vs-called asked "/gallery/voices")) "kind=mine asks nothing of the gallery")))))

;;; --- vs_design_voice ---------------------------------------------------------------

(deftest voicestudio-designs-a-voice-from-words ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/design/describe" (values +describe+ 200))
                          ("/profiles" (values "{\"id\":\"new-voice\"}" 200))
                          ("/generate" (values +take+ 200)))
      (let ((answer (vs-answer "vs-6" "vs_design_voice"
                                "{\"name\":\"Narrator\",\"describe\":\"warm young female\"}")))
        (is (search "made voice" answer) "the voice was made")
        (is (search "new-voice" answer) "with the id the server answered")
        (is (search "preview:" answer) "and a preview was rendered")
        (is (= 3 (length asked)) "describe, save, preview")
        (is (equal "warm young female"
                   (nlk:json-value (vs-json (third (first asked))) :string "description")) "the description rides /design/describe")
        (is (equal "design"
                   (cdr (assoc "kind" (third (second asked)) :test #'string=))) "the profile is saved as a design")))))

(deftest voicestudio-clones-a-voice-from-a-recording ()
  (with-voicestudio ()
    (let ((clip (vs-text-file "clip.wav" "RIFF")))
      (with-vs-wire (asked ("/profiles" (values "{\"id\":\"cloned\"}" 200)))
        (let ((answer (vs-answer "vs-7" "vs_design_voice"
                                  (format nil "{\"name\":\"Mine\",\"sample\":~s,\"preview\":false}"
                                          (namestring clip)))))
          (is (search "cloned" answer) (format nil "the cloned id is the answer's: ~a" answer))
          (is (= 1 (length asked)) "one POST: no preview was asked for")
          (let ((content (third (first asked))))
            (is (equal "clone" (cdr (assoc "kind" content :test #'string=))) "saved as a clone")
            (is (pathnamep (cdr (assoc "ref_audio" content :test #'string=))) "and the recording goes as a file part")))))))

;;; --- vs_status ---------------------------------------------------------------------

(deftest voicestudio-status-says-what-is-ready ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/setup/status" (values +setup+ 200))
                          ("/engines" (values "{\"active\":\"chatterbox\"}" 200))
                          ("/profiles" (values +profiles+ 200)))
      (let ((answer (vs-answer "vs-8" "vs_status" "{}")))
        (is (search "ready" answer) "the server is ready")
        (is (search "chatterbox" answer) "with the engine it loaded")
        (is (search "voices: 2" answer) "and how many voices it has")))))

(deftest voicestudio-status-says-when-the-server-is-away ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/setup/status" (error "connection refused")))
      (let ((answer (vs-answer "vs-9" "vs_status" "{}")))
        (is (search "could not reach VoiceStudio" answer) "the plugin's own E1 line")
        (is (search "http://vs.test:3900" answer) "naming the address")))))

;;; --- vs_longform -------------------------------------------------------------------

(deftest voicestudio-renders-the-audiobook-an-event-stream-finished ()
  (with-voicestudio ()
    (let ((script (vs-text-file "episode.md"
                                 (format nil "# One~%the first chapter~%# Two~%the second~%"))))
      (with-vs-wire (asked ("/audiobook/plan" (values "{}" 200))
                            ("/audiobook" (values +sse+ 200
                                                   (list (cons "content-type" "text/event-stream"))))
                            ("/audio/" (values +take+ 200)))
        (with-vs-no-sleep
          (let ((answer (vs-answer "vs-10" "vs_longform"
                                    (format nil "{\"file\":~s,\"voice\":\"narrator\"}"
                                            (namestring script)))))
            (is (search "wrote " answer) (format nil "the artifact was fetched: ~a" answer))
            (is (search "2 chapter" answer) "with the chapter count the stream stated")
            (is (= 1 (length (vs-called asked "/audio/"))) "the artifact came from /audio/")
            (is (probe-file (vs-file answer)) "and is on disk")))))))

(deftest voicestudio-longform-refuses-markup-a-voice-cannot-say ()
  (with-voicestudio ()
    (let ((script (vs-text-file "bad.md" (format nil "# One~%[sigh] then (pause) he left~%"))))
      (with-vs-wire (asked)
        (let ((answer (vs-answer "vs-11" "vs_longform"
                                  (format nil "{\"file\":~s,\"voice\":\"narrator\"}"
                                          (namestring script)))))
          (is (search "[sigh]" answer) (format nil "the markup is named: ~a" answer))
          (is (search "pause" answer) "and the supported set is offered")
          (is (null asked) "nothing was rendered"))))))

;;; --- vs_dub ------------------------------------------------------------------------

(deftest voicestudio-dubs-a-video-and-names-the-files ()
  (with-voicestudio ()
    (let ((video (vs-text-file "intro.mp4" "mp4")))
      (with-vs-wire (asked ("/batch/enqueue" (values "{\"job_id\":\"dub-1\"}" 202))
                            ("/batch/jobs/" (values "{\"status\":\"done\",\"paths\":[\"/dubbed/es.mp4\",\"/dubbed/fr.mp4\"]}" 200)))
        (with-vs-no-sleep
          (let ((answer (vs-answer "vs-12" "vs_dub"
                                    (format nil "{\"video\":~s,\"langs\":\"es,fr\",\"voice\":\"narrator\"}"
                                            (namestring video)))))
            (is (search "es, fr" answer) (format nil "both languages are named: ~a" answer))
            (is (search "/dubbed/es.mp4" answer) "with the file each one made")
            (let ((content (third (first asked))))
              (is (equal "es,fr" (cdr (assoc "langs" content :test #'string=))) "enqueued as one job")
              (is (pathnamep (cdr (assoc "video" content :test #'string=))) "with the video as a file part"))))))))

;;; --- the commands, and the folder ----------------------------------------------------

(deftest voicestudio-the-command-speaks-what-was-typed ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/generate" (values +take+ 200 (list (cons "content-type" "audio/mpeg")))))
      (let ((answer (nodecode-voicestudio::command-speak
                     (format nil "--text ~s --voice narrator" "Hello from the prompt") "s1")))
        (is (search "wrote " answer) "/vs-speak --text speaks")
        (is (equal "Hello from the prompt"
                   (cdr (assoc "text" (third (first asked)) :test #'string=))) "with the quoted words as one value")))))

(deftest voicestudio-the-cells-own-command-says-what-it-offers ()
  (with-voicestudio ()
    (with-vs-wire (asked)
      (let ((answer (nodecode-voicestudio::command-main "" "s1")))
        (is (search "/vs-speak" answer) (format nil "it names its mirrors: ~a" answer))
        (is (search "vs_speak" answer) "and the tool a model calls")
        (is (search "http://vs.test:3900" answer) "and the address the cell runs on")
        (is (null asked) "without asking the server anything")))))

(deftest voicestudio-the-cells-own-command-asks-the-server-when-told ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/setup/status" (values +setup+ 200))
                          ("/profiles" (values +profiles+ 200)))
      (let ((answer (nodecode-voicestudio::command-main "--status" "s1")))
        (is (search "ready" answer) (format nil "the readiness line: ~a" answer))
        (is (eq :get (first (first asked))) "and /setup/status was the first read")))))

(deftest voicestudio-refuses-an-output-path-that-leaves-the-folder ()
  (with-voicestudio ()
    (with-vs-wire (asked ("/generate" (values +take+ 200 (list (cons "content-type" "audio/mpeg")))))
      (let ((answer (vs-answer "vs-13" "vs_speak"
                                "{\"text\":\"Hi\",\"voice\":\"narrator\",\"out_path\":\"../elsewhere.mp3\"}")))
        (is (search "just a file name" answer) (format nil "the refusal says what an out_path may be: ~a" answer))
        (is (not (probe-file (merge-pathnames "elsewhere.mp3"
                                              (nodecode-voicestudio::setting :directory)))) "and nothing was written under the name it escaped with")))))
