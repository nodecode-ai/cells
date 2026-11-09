;;;; cell-test.lisp --- the cell's lifecycle, its tools, and what it writes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every ElevenLabs endpoint is a stubbed NLK:HTTP answering a fixture, so
;;;; nothing here reaches the network, and every track lands in a temp folder.

(in-package #:nodecode.test)

(define-test-slice "elevenlabs-music" "ELEVENLABS-MUSIC-CELL-"
  :start nodecode-elevenlabs-music:start-cell)

(define-cell-lifecycle-tests "elevenlabs-music"
  (:config "api_key" "test-key")
  (:help :elevenlabs-music)
  (:running (is (nle::find-tool "elevenlabs_music_compose") "the compose tool is registered")
            (is (nle::find-tool "elevenlabs_music_compose_plan") "the plan tool")
            (is (nle::find-tool "elevenlabs_music_job") "and the job tool")
            (is (equal "test-key" (nodecode-elevenlabs-music::setting :api-key)) "the section's key is what the tools run on"))
  (:stopped (is (null (nle::find-tool "elevenlabs_music_compose")) "stop unregisters the tools"))
  (:refused ("music_model" "music_v9") ("output_format" "wav") ("wait_seconds" -1))
  (:idle nodecode-elevenlabs-music::elevenlabs-music-error
         (nodecode-elevenlabs-music::compose (nlk:make-json-object "prompt" "x"))))

;;; --- fixtures ----------------------------------------------------------------

(defun music-headers (pairs) "PAIRS as a dexador response header table: lowercase names in a hash table."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name . value) in pairs
          do (setf (gethash (string-downcase name) table) value))
    table))

(defun music-answer (metadata) "A multipart/mixed answer carrying METADATA and a short audio part."
  (let ((crlf (format nil "~c~c" #\Return #\LineFeed)))
    (nodecode-elevenlabs-music::octets-of
     (format nil "--b1~aContent-Type: application/json~a~a~a~a--b1~aContent-Type: audio/mpeg~a~aABCD~a--b1--~a"
             crlf crlf crlf metadata crlf crlf crlf crlf crlf crlf))))

(defun music-json (text)
  (nlk:decode-json text))

(defun push-u16 (out value)
  (vector-push-extend (logand value 255) out)
  (vector-push-extend (logand (ash value -8) 255) out))

(defun push-u32 (out value)
  (loop for shift from 0 below 32 by 8
        do (vector-push-extend (logand (ash value (- shift)) 255) out)))

(defun push-text (out text)
  (loop for char across text do (vector-push-extend (char-code char) out)))

(defun music-zip (members) "MEMBERS, (NAME . TEXT) pairs, as one stored ZIP archive."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
        (entries '()))
    (dolist (member members)
      (let* ((name (car member))
             (data (nodecode-elevenlabs-music::octets-of (cdr member)))
             (offset (fill-pointer out)))
        (push-u32 out #x04034b50) (push-u16 out 20) (push-u16 out 0) (push-u16 out 0)
        (push-u16 out 0) (push-u16 out 0) (push-u32 out 0)
        (push-u32 out (length data)) (push-u32 out (length data))
        (push-u16 out (length name)) (push-u16 out 0)
        (push-text out name)
        (loop for byte across data do (vector-push-extend byte out))
        (push (list name (length data) offset) entries)))
    (let ((start (fill-pointer out)))
      (dolist (entry (reverse entries))
        (let ((name (first entry)))
          (push-u32 out #x02014b50) (push-u16 out 20) (push-u16 out 20) (push-u16 out 0)
          (push-u16 out 0) (push-u16 out 0) (push-u16 out 0) (push-u32 out 0)
          (push-u32 out (second entry)) (push-u32 out (second entry))
          (push-u16 out (length name)) (push-u16 out 0) (push-u16 out 0)
          (push-u16 out 0) (push-u16 out 0)
          (push-u32 out 0) (push-u32 out (third entry))
          (push-text out name)))
      (let ((size (- (fill-pointer out) start)))
        (push-u32 out #x06054b50) (push-u16 out 0) (push-u16 out 0)
        (push-u16 out (length members)) (push-u16 out (length members))
        (push-u32 out size) (push-u32 out start) (push-u16 out 0)))
    out))

;;; --- compose ----------------------------------------------------------------

(defparameter +music-metadata+
  (concatenate 'string
               "{\"song_metadata\":{\"title\":\"Lighthouse\"},"
               "\"composition_plan\":{\"chunks\":[{\"text\":\"[Verse]\\nglow\","
               "\"duration_ms\":30000,\"positive_styles\":[\"folk\"]}]}}"))

(deftest elevenlabs-music-cell-composes-a-song-through-the-tool-seam ()
  (let ((dir (uiop:native-namestring (temp-path "music"))))
    (with-cell-stop ((elevenlabs-music-start "api_key" "test-key" "output_dir" dir))
      (let ((asked '()))
        (with-stubbed-fdefinition
            (nlk:http (method url &rest args)
              (push (list method url (getf args :headers) (getf args :content)) asked)
              (values (music-answer +music-metadata+) 200
                      (music-headers (list (cons "Content-Type" "multipart/mixed; boundary=b1")
                                           (cons "Song-Id" "song-9")))))
          (let* ((text (nle::message-content
                        (execute-wire-call
                         "call-1" "elevenlabs_music_compose"
                         "{\"prompt\":\"a sad folk song about a lighthouse\",\"length_seconds\":40}"))))
            (is (search "\"track_id\"" text) text)
            (let ((result (music-json text)))
            (is (= 1 (length asked)) "one request")
            (destructuring-bind (method url headers content) (first asked)
              (is (eq :post method))
              (is (equal "https://api.elevenlabs.io/v1/music/detailed?output_format=mp3_44100_128" url))
              (is (equal "test-key" (cdr (assoc "xi-api-key" headers :test #'string-equal))) "the key rides in the xi-api-key header")
              (let ((body (music-json content)))
                (is (equal "a sad folk song about a lighthouse"
                           (nlk:json-value body :string "prompt")))
                (is (eql 40000 (nlk:json-value body :integer "music_length_ms")) "the length the user named, in milliseconds")
                (is (eq t (nlk:json-value body :boolean "store_for_inpainting")) "kept for editing by default")))
            (is (equal "Lighthouse" (nlk:json-value result :string "title")) "the title ElevenLabs gave the song")
            (is (equal "song-9" (nlk:json-value result :string "elevenlabs_song_id")))
            (let ((path (nlk:json-value result :string "file_path")))
              (is (search dir path) "the track landed in output_dir")
              (is (probe-file path) "and the file is there")
              (is (equal "ABCD" (nodecode-elevenlabs-music::text-of
                                   (nodecode-elevenlabs-music::read-octets path))) "holding exactly the audio part"))
            (is (equal "MEDIA:" (subseq (nlk:json-value result :string "media_tag") 0 6)) "the result says how to deliver it")
            (let ((sections (nlk:json-value result :array "sections")))
              (is (= 1 (length sections)))
              (is (equal "Verse" (nlk:json-value (aref sections 0) :string "label"))))
            (is (search "glow" (nlk:json-value result :string "lyrics"))))))))))

(deftest elevenlabs-music-cell-drafts-a-plan-for-free ()
  (let ((plan (concatenate 'string
                            "{\"chunks\":[{\"text\":\"[Intro]\\nsoft\","
                            "\"duration_ms\":10000,\"positive_styles\":[\"ambient\"]}]}")))
    (with-cell-stop ((elevenlabs-music-start "api_key" "k"))
      (let ((asked nil))
        (with-stubbed-fdefinition
            (nlk:http (method url &rest args)
              (setf asked (list method url (getf args :content)))
              (values (nodecode-elevenlabs-music::octets-of plan) 200 nil))
          (let* ((text (nle::message-content
                        (execute-wire-call "call-2" "elevenlabs_music_draft_plan"
                                            "{\"prompt\":\"ambient for a walk\"}")))
                 (result (music-json text)))
            (is (eq :post (first asked)))
            (is (equal "https://api.elevenlabs.io/v1/music/plan" (second asked)))
            (is (equal "ambient for a walk" (nlk:json-value (music-json (third asked)) :string "prompt")))
            (is (null (nlk:json-value result :boolean "billed")) "a drafted plan costs nothing")
            (is (= 1 (length (nlk:json-value result :array "sections")))))))))
)
;;; --- refusals, stems, and a job -------------------------------------------

(deftest elevenlabs-music-cell-refuses-with-no-key ()
  (with-cell-stop ((elevenlabs-music-start))
    (with-stubbed-fdefinitions
        ((nodecode-elevenlabs-music::environment-key () nil)
         (nlk:http (method url &rest args)
           (declare (ignorable method url args))
           (error "no request may go out without a key")))
      (let ((text (nle::message-content
                   (execute-wire-call "call-3" "elevenlabs_music_compose"
                                       "{\"prompt\":\"x\"}"))))
        (is (search "no ElevenLabs API key" text) text)
        (is (search "api_key" text) "the refusal names the setting")))))

(deftest elevenlabs-music-cell-splits-a-song-into-stems ()
  (let ((dir (uiop:native-namestring (temp-path "stems")))
        (track (temp-path "song" "mp3")))
    (write-temp-file track "ID3")
    (let ((zip (music-zip (list (cons "vocals.mp3" "VOCALS")
                                (cons "other.mp3" "OTHER")))))
      (with-cell-stop ((elevenlabs-music-start "api_key" "k" "output_dir" dir))
        (let ((asked nil))
          (with-stubbed-fdefinition
              (nlk:http (method url &rest args)
                (setf asked (list method url (getf args :headers) (getf args :content)))
                (values zip 200 (music-headers (list (cons "Content-Type" "application/zip")))))
            (let* ((text (nle::message-content
                          (execute-wire-call
                           "call-4" "elevenlabs_music_stems"
                           (format nil "{\"file_path\":~s,\"stems\":\"two\"}"
                                   (uiop:native-namestring track)))))
                   (result (music-json text))
                   (files (nlk:json-value result :array "stems")))
              (is (eq :post (first asked)))
              (is (search "/v1/music/stem-separation?output_format=mp3_44100_128" (second asked)))
              (is (search "multipart/form-data; boundary="
                          (cdr (assoc "content-type" (third asked) :test #'string-equal))))
              (is (search "two_stems_v1" (nodecode-elevenlabs-music::text-of (fourth asked))) "the variation rides as a form field")
              (is (= 2 (length files)) "both members were saved as their own tracks")
              (is (equal (list "OTHER" "VOCALS")
                         (sort (loop for file across files
                                     collect (nodecode-elevenlabs-music::text-of
                                              (nodecode-elevenlabs-music::read-octets
                                               (nlk:json-value file :string "file_path"))))
                               #'string<)) "each stem file holds its own member's bytes")
              (is (equal 2 (length (nlk:json-value result :array "media_tags")))))))))))

(deftest elevenlabs-music-cell-answers-a-job-that-is-not-done ()
  (let ((dir (uiop:native-namestring (temp-path "job"))))
    (with-cell-stop ((elevenlabs-music-start "api_key" "k" "wait_seconds" 0
                                             "output_dir" dir))
      (with-stubbed-fdefinition
          (nlk:http (method url &rest args)
            (declare (ignorable method url args))
            (sleep 0.1)
            (values (music-answer +music-metadata+) 200
                    (music-headers (list (cons "Content-Type" "multipart/mixed; boundary=b1")))))
        (let* ((pending (music-json
                         (nle::message-content
                          (execute-wire-call "call-5" "elevenlabs_music_compose"
                                              "{\"prompt\":\"a lullaby\"}"))))
               (id (nlk:json-value pending :string "job_id")))
          (is (equal "running" (nlk:json-value pending :string "status")) "a zero wait answers at once with the job id")
          (is-present (entry id) "the job id"
            (let ((done (music-json
                         (nle::message-content
                          (execute-wire-call
                           "call-6" "elevenlabs_music_job"
                           (format nil "{\"job_id\":~s,\"wait_seconds\":240}" entry))))))
              (is (equal "Lighthouse" (nlk:json-value done :string "title")) "the job tool answers what the tool that started it would have")
              (is (equal entry (nlk:json-value done :string "job_id"))))))))))
