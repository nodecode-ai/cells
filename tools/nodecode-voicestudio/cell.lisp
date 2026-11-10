;;;; cell.lisp --- VoiceStudio's sidecar API, as six tools and six commands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; VoiceStudio is a desktop app that runs a local server (the plugin's own
;;;; default is http://127.0.0.1:3900). The original plugin was a thin client
;;;; of that server over Python's standard library: speak a text, design or
;;;; clone a voice, render a chaptered audiobook, dub a video, ask whether it
;;;; is ready. This cell is the same client over NLK:HTTP, with the plugin's
;;;; routes, form fields, ceilings and refusals -- nothing here runs the app,
;;;; and the server is whatever address the section names.
;;;;
;;;; The audio the server answers is written under the section's folder, and
;;;; the answer names the file: a chat surface has no audio player, and a path
;;;; is what a user hands to one.

(in-package #:nodecode-voicestudio)

;;; --- what the plugin fixed ----------------------------------------------------------

(defparameter +speak-chars+ 6000
  "One short take's ceiling (tools_speak.py:39).")
(defparameter +describe-chars+ 2000
  "One voice description's ceiling (tools_design.py:24).")
(defparameter +speed-min+ 0.25d0
  "The slowest take the plugin accepted (tools_speak.py:40).")
(defparameter +speed-max+ 4.0d0
  "The fastest take the plugin accepted (tools_speak.py:40).")
(defparameter +preview-text+ "Hello. This is a short preview of the new voice."
  "The sample the plugin rendered after designing a voice (tools_design.py:25).")
(defparameter +sample-types+ '("wav" "m4a")
  "The recordings the plugin clones from (tools_design.py:26).")
(defparameter +video-types+ '("mp4" "mov" "mkv" "webm" "m4v")
  "The videos the plugin dubs (tools_dub.py:20).")
(defparameter +script-types+ '("md" "txt")
  "The scripts the plugin renders (tools_longform.py:41).")
(defparameter +done-words+ '("done" "completed" "finished" "success")
  "A job status the plugin read as finished (tools_dub.py:130).")
(defparameter +failed-words+ '("failed" "error" "cancelled")
  "A job status the plugin read as stopped (tools_dub.py:145).")
(defparameter +unreachable+
  "I could not reach VoiceStudio at ~a. Start the VoiceStudio app, or check the address in the section. Then try again; /vs-status says when it is back."
  "The plugin's own E1 line (client.py:262).")

(defparameter *sleep* #'sleep
  "How the cell waits between two job reads. A test binds this.")

;;; --- a refusal is text, never an unhandled error -------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text."
  `(handler-case (progn ,@body)
     (voicestudio-error (condition)
       (format nil "ERROR: ~a" (voicestudio-error-detail condition)))
     (error (condition)
       (format nil "ERROR: VoiceStudio: ~a" (nlk:one-line (princ-to-string condition))))))

(defmacro answering-tool (&body body)
  "BODY's text as a tool result; a refusal is recorded as the call's failure."
  `(handler-case (progn ,@body)
     (voicestudio-error (condition)
       (nle:failure (format nil "ERROR: ~a" (voicestudio-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: VoiceStudio: ~a"
                            (nlk:one-line (princ-to-string condition)))))))

;;; --- small reads ---------------------------------------------------------------------

(defun nonblank (value)
  "VALUE as a string with no blank around it, or NIL when it is blank."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun words-of (text)
  "TEXT's words; a single- or double-quoted run is one word."
  (let ((words '())
        (current (make-string-output-stream))
        (quote nil)
        (any nil))
    (flet ((flush ()
             (when any
               (push (get-output-stream-string current) words)
               (setf current (make-string-output-stream) any nil))))
      (loop for character across (or text "")
            do (cond (quote
                      (if (char= character quote)
                          (setf quote nil)
                          (write-char character current))
                      (setf any t))
                     ((or (char= character #\') (char= character #\"))
                      (setf quote character any t))
                     ((member character '(#\Space #\Tab #\Newline #\Return))
                      (flush))
                     (t (write-char character current) (setf any t))))
      (flush))
    (nreverse words)))

(defun parse-flags (text)
  "TEXT as (NAME . VALUE) pairs: --name value, the words after one joined by
spaces; a name with no value is T. The plugin's own --flag reading, minus
shlex."
  (let ((flags '())
        (key nil)
        (pending '()))
    (flet ((finish ()
             (when key
               (push (cons key (if pending (format nil "~{~a~^ ~}" (nreverse pending)) t)) flags)
               (setf key nil pending '()))))
      (dolist (word (words-of text))
        (if (uiop:string-prefix-p "--" word)
            (progn (finish) (setf key (subseq word 2)))
            (when key (push word pending))))
      (finish))
    (nreverse flags)))

(defun flag (flags name)
  "FLAGS' value for NAME, or NIL."
  (cdr (assoc name flags :test #'string=)))

(defun flag-number (flags name)
  "FLAGS' NAME as a real, or NIL when absent or not a number."
  (let ((text (nonblank (flag flags name))))
    (and text (multiple-value-bind (number end)
                  (let ((*read-eval* nil)) (ignore-errors (read-from-string text)))
                (and (realp number) (= end (length text)) (coerce number 'double-float))))))

(defun flag-integer (flags name)
  "FLAGS' NAME as an integer, or NIL."
  (let ((number (flag-number flags name)))
    (and number (integerp number) number)))

(defun number-text (number)
  "NUMBER as a form field carries one: 1, 1.5, 0.75."
  (let ((text (format nil "~,3f" (coerce number 'double-float))))
    (setf text (string-right-trim "0" text))
    (if (or (zerop (length text)) (char= (char text (1- (length text))) #\.))
        (concatenate 'string text "0")
        text)))

(defun stamp ()
  "The moment a file is made, in UTC."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun file-id ()
  "Eight hex digits: what two files made in one second differ by."
  (format nil "~8,'0x" (random (expt 16 8))))

(defun save-name (kind extension)
  "A fresh file name for KIND's artifact."
  (format nil "~a-~a-~a.~a" kind (stamp) (file-id) extension))

(defun directory-path ()
  "The section's folder, as a namestring with a trailing slash."
  (ensure-directories-exist (setting :directory))
  (namestring (setting :directory)))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in)))

;;; --- the server --------------------------------------------------------------------

(defun base-url ()
  "The address the server answers on, with no trailing slash."
  (string-right-trim "/" (or (nonblank (setting :base-url)) "http://127.0.0.1:3900")))

(defun header (headers name)
  "HEADERS' value for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun body-text (body)
  "BODY as text, whatever NLK:HTTP answered it as."
  (nlk:body-text body 65536))

(defun decode (body)
  "BODY's decoded JSON, or NIL."
  (ignore-errors (nlk:decode-json (body-text body))))

(defun unreachable (&optional condition)
  "The plugin's E1 refusal, naming the address."
  (fail "~a~@[ (~a)~]" (format nil +unreachable+ (base-url))
        (and condition (nlk:one-line (princ-to-string condition)))))

(defun exchange (method path &key content content-type binary (timeout (setting :timeout)))
  "One exchange with the server => (values BODY STATUS HEADERS). A dial that
fails is a refusal naming the address."
  (handler-case
      (multiple-value-bind (body status headers)
          (nlk:http method (concatenate 'string (base-url) path)
                    :headers (append (when content-type
                                       (list (cons "content-type" content-type)))
                                     (list (cons "accept" "application/json, audio/*")))
                    :content content
                    :binary binary
                    :timeout timeout
                    :connect-timeout (min (setting :connect) timeout))
        (values body status headers))
    (voicestudio-error (condition) (error condition))
    (error (condition) (unreachable condition))))

(defun get-json (path &key (timeout (setting :probe)))
  "PATH's decoded answer => (values PAYLOAD STATUS); PAYLOAD is NIL past a 2xx."
  (multiple-value-bind (body status headers)
      (exchange :get path :timeout timeout)
    (declare (ignore headers))
    (values (and (<= 200 status 299) (decode body)) status)))

(defun post-json (path object &key (timeout (setting :timeout)))
  "PATH with OBJECT as its JSON body => (values BODY STATUS HEADERS)."
  (exchange :post path :content (nlk:encode-json-object object)
            :content-type "application/json" :timeout timeout))

(defun post-form (path fields &key (timeout (setting :timeout)))
  "PATH with FIELDS as its multipart body => (values BODY STATUS HEADERS).
A value that is a pathname is sent as a file part, as the plugin sent one."
  (exchange :post path :content fields :timeout timeout))

(defun items-of (payload)
  "PAYLOAD's array: itself when it is one, else the first member this API
names one by (the plugin's own shapes, tools_speak.py:224)."
  (cond ((nlk:json-value payload :array) payload)
        ((hash-table-p payload)
         (loop for key in '("profiles" "voices" "archetypes" "personalities" "jobs" "items")
               for value = (nlk:json-value payload :array key)
               when value return value))
        (t #())))

(defun status-word (state)
  "STATE's status, lowercased, or an empty string."
  (string-downcase (or (nlk:json-value state :string "status") "")))

(defun job-id-of (payload)
  "PAYLOAD's job id under the names the plugin tried (tools_dub.py:103)."
  (or (nlk:json-value payload :string "job_id")
      (nlk:json-value payload :string "id")
      (nlk:json-value payload :string "task_id")))

;;; --- the files a call reads and writes ------------------------------------------------

(defun octets-of (body)
  "BODY as octets: NLK:HTTP answers a string when the peer sent text."
  (etypecase body
    ((simple-array (unsigned-byte 8) (*)) body)
    ((vector (unsigned-byte 8)) body)
    (string (sb-ext:string-to-octets body :external-format :utf-8))
    (null (make-array 0 :element-type '(unsigned-byte 8)))))

(defun write-bytes (body path)
  "BODY at PATH, its folder made first. => PATH."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence (octets-of body) out))
  path)

(defun confined-file (name kind extension)
  "NAME as a file inside the section's folder: a bare file name, or one the
cell makes. The plugin confined an explicit out_path to its own folder
(tools_speak.py:86); here anything that names a folder is refused."
  (let ((name (nonblank name)))
    (when name
      (when (or (find #\/ name) (find #\\ name) (member name '("." "..") :test #'string=)
                (uiop:string-prefix-p "~" name))
        (fail "I cannot save to ~s: output files stay inside ~a, so give just a file name"
              name (directory-path))))
    (merge-pathnames (or name (save-name kind extension)) (setting :directory))))

(defun nat (path)
  "PATH as the native path a containment check compares: no trailing separator."
  (string-right-trim '(#\\ #\/) path))

(defun absolute-path (text)
  "TEXT as a native absolute path, a leading ~ read against this home."
  (let ((pathname (nlk:expand-home text)))
    (nat
     (namestring (if (uiop:absolute-pathname-p pathname)
                      pathname
                      (merge-pathnames pathname (uiop:getcwd)))))))

(defun refused-input-p (native)
  "Whether NATIVE lies inside the organism's own folder or inside ~/.ssh: the
two places the plugin never read from (tools_speak.py:63)."
  (flet ((under (directory)
           (ignore-errors (nlk::path-within-p native (nat (namestring directory))))))
    (or (under (nlk:home))
        (under (merge-pathnames ".ssh/" (user-homedir-pathname))))))

(defun input-file (path &key types label)
  "PATH as a native file this cell may read, refused unless it is one of
TYPES. LABEL names the kinds in the refusal."
  (let ((path (or (nonblank path) (fail "no file was given"))))
    (let ((native (absolute-path path)))
      (when (refused-input-p native)
        (fail "I cannot use ~a: the organism's own folder and ~/.ssh are never read" native))
      (unless (probe-file native)
        (fail "I could not open ~a: check the path" native))
      (when types
        (let ((type (string-downcase (or (pathname-type (pathname native)) ""))))
          (unless (member type types :test #'string=)
            (fail "I could not use ~a: ~@[a ~a is one of ~]~{~a~^, ~}"
                  native label types))))
      native)))

;;; --- vs_speak ---------------------------------------------------------------------

(defun unknown-voice (voice)
  "The plugin's own unknown-voice line (client.py:268)."
  (format nil "I do not know a voice called '~a'. Run /vs-voices --search ~a to find it, or make one with /vs-design."
          voice voice))

(defun take-fields (text voice speed seed)
  "POST /generate's multipart fields, as the plugin sent them."
  (append (list (cons "text" text)
                (cons "profile_id" voice)
                (cons "speed" (number-text speed)))
          (when seed (list (cons "seed" (princ-to-string seed))))))

(defun take-status (body status voice)
  "Refuse a take the server did not make, in the plugin's own words."
  (cond
    ((= status 200) t)
    ((= status 404) (fail "~a" (unknown-voice voice)))
    ((member status '(400 422))
     (let ((detail (nlk:json-value (decode body) :string "detail")))
       (when (and detail (search "voice" (string-downcase detail)))
         (fail "~a" (unknown-voice voice)))
       (fail "VoiceStudio did not accept that take~@[: ~a~]" detail)))
    (t (fail "VoiceStudio answered HTTP ~a for that take; run /vs-status and try again" status))))

(defun take-report (path voice speed note seed remote)
  (with-output-to-string (out)
    (format out "wrote ~a~%"
            (if remote
                (format nil "~a (on the server)" path)
                (format nil "~a (~a)" path (nlk:size-text (file-size path)))))
    (format out "voice ~a at ~ax~%" voice (number-text speed))
    (when seed (format out "seed ~a: reuse it to repeat this exact take~%" seed))
    (when note (format out "note: ~a~%" note))))

(defun take-answer (body headers voice speed note out-path)
  "The answer for a 200: the server's own file when it names one, else the
bytes written under the section's folder."
  (if (search "json" (string-downcase (or (header headers "content-type") "")))
      (let* ((payload (decode body))
             (remote (nlk:json-value payload :string "audio_path")))
        (unless remote
          (fail "VoiceStudio answered without audio: run /vs-status and try again"))
        (values (take-report remote voice speed note (nlk:json-value payload :string "seed") t)
                remote))
      (let* ((file (confined-file out-path "vs-speak" "mp3"))
             (written (write-bytes body file)))
        (values (take-report written voice speed note (header headers "x-seed") nil)
                written))))

(defun speak (text &key voice speed seed out-path)
  "TEXT into an audio file with a VoiceStudio voice => (values REPORT PATH)."
  (let ((text (or (nonblank text) (fail "there are no words to say: give the text"))))
    (when (> (length text) +speak-chars+)
      (fail "that text is ~d characters and one take is at most ~d: split it at paragraph breaks, or use vs_longform for the whole thing"
            (length text) +speak-chars+))
    (let* ((voice (or (nonblank voice)
                      (nonblank (setting :default-voice))
                      (fail "no voice chosen and no default voice is set: run /vs-voices to pick one, then name it")))
           (speed (coerce (or speed 1.0d0) 'double-float))
           (note nil))
      (unless (<= +speed-min+ speed +speed-max+)
        (setf note (format nil "speed ~a is outside 0.25 to 4.0, so 1.0 was used" speed)
              speed 1.0d0))
      (multiple-value-bind (body status headers)
          (post-form "/generate" (take-fields text voice speed seed))
        (take-status body status voice)
        (take-answer body headers voice speed note out-path)))))

;;; --- vs_list_voices ---------------------------------------------------------------

(defun matches-p (item search)
  "Whether ITEM's own JSON carries every word of SEARCH."
  (or (null search)
      (let ((hay (string-downcase (nlk:encode-json-object item))))
        (every (lambda (word) (search (string-downcase word) hay))
               (words-of search)))))

(defun voice-rows (path search)
  "PATH's voices, filtered by SEARCH, as (ID NAME) rows; none when the route
answers nothing this API lists."
  (handler-case
      (let ((payload (nth-value 0 (get-json path :timeout (setting :probe)))))
        (loop for item across (items-of payload)
              when (and (hash-table-p item) (matches-p item search))
                collect (list (nlk:json-value item :string "id")
                              (nlk:json-value item :string "name"))))
    (voicestudio-error (condition) (error condition))
    (error () '())))

(defun voice-lines (label rows)
  "One line per voice: its id, then its name."
  (when rows
    (format nil "~a:~%~{~a~%~}" label
            (mapcar (lambda (row)
                      (format nil "  ~a  ~@[~a~]" (first row) (second row)))
                    rows))))

(defun voice-report (mine gallery presets search)
  (let ((total (+ (length mine) (length gallery) (length presets))))
    (if (zerop total)
        (format nil "no voices ~@[match ~s~] at ~a~%try a shorter search, or make one with vs_design_voice"
                search (base-url))
        (concatenate
         'string
         (format nil "~d voices at ~a: ~d yours, ~d gallery, ~d presets~%"
                 total (base-url) (length mine) (length gallery) (length presets))
         (or (voice-lines "yours" mine) "")
         (or (voice-lines "gallery" gallery) "")
         (or (voice-lines "presets" presets) "")
         "name any id as the voice of vs_speak, vs_longform or vs_dub"))))

(defun list-voices (&key search kind)
  "The voices the server offers, filtered, as text."
  (let ((search (nonblank search))
        (kind (string-downcase (or (nonblank kind) "all"))))
    ;; The probe first: a stopped server is answered before three dead reads.
    (get-json "/setup/status" :timeout (setting :probe))
    (voice-report (if (member kind '("all" "mine") :test #'string=)
                      (voice-rows "/profiles" search)
                      '())
                  (if (member kind '("all" "gallery") :test #'string=)
                      (or (voice-rows "/gallery/voices" search)
                          (voice-rows "/archetypes" search))
                      '())
                  (if (member kind '("all" "presets") :test #'string=)
                      (voice-rows "/personalities" search)
                      '())
                  search)))

;;; --- vs_design_voice --------------------------------------------------------------

(defun profile-answer (status body name)
  "The profile POST's answer, or a refusal in the plugin's own words."
  (let ((payload (decode body)))
    (cond
      ((= status 200) (if (hash-table-p payload) payload (make-hash-table :test #'equal)))
      ((member status '(400 409 422))
       (let ((detail (or (nlk:json-value payload :string "detail")
                         (nlk:json-value payload :string "error"))))
         (when (and detail
                    (or (search "already" (string-downcase detail))
                        (search "exists" (string-downcase detail))
                        (search "taken" (string-downcase detail))))
           (fail "you already have a voice called '~a': pick another name" name))
         (fail "VoiceStudio did not accept that voice~@[: ~a~]" detail)))
      (t (fail "VoiceStudio answered HTTP ~a for that voice; run /vs-status and try again" status)))))

(defun preview-take (id)
  "A short sample of the new voice, best effort: NIL when it did not render."
  (handler-case
      (multiple-value-bind (report file) (speak +preview-text+ :voice id)
        (declare (ignore report))
        file)
    (error () nil)))

(defun voice-made (name payload preview &optional unmatched)
  (let* ((id (or (nlk:json-value payload :string "id")
                 (nlk:json-value payload :string "profile_id")
                 name))
         (file (and preview (preview-take id))))
    (with-output-to-string (out)
      (format out "made voice ~s (id ~a)~%" name id)
      (when unmatched (format out "note: not used: ~{~a~^, ~}~%" unmatched))
      (when file
        (format out "preview: ~a (~a)~%" (namestring file) (nlk:size-text (file-size file))))
      (format out "use it with voice ~a~%" id))))

(defun clone-voice (name sample preview)
  (let ((file (input-file sample :types +sample-types+ :label "recording")))
    (multiple-value-bind (body status headers)
        (post-form "/profiles" (list (cons "kind" "clone")
                                      (cons "name" name)
                                      (cons "ref_audio" (pathname file))))
      (declare (ignore headers))
      (voice-made name (profile-answer status body name) preview))))

(defun design-voice (name describe preview)
  (when (> (length describe) +describe-chars+)
    (fail "that description is ~d characters and a design takes ~d: keep it to a few words, such as \"warm young female, slight British accent\""
          (length describe) +describe-chars+))
  (multiple-value-bind (body status headers)
      (post-json "/design/describe" (nlk:make-json-object "description" describe))
    (declare (ignore headers))
    (unless (= status 200)
      (fail "VoiceStudio did not accept that description (HTTP ~a): try plainer words, or start from a voice /vs-voices lists"
            status))
    (let* ((decoded (decode body))
           (states (or (nlk:json-value decoded :object "vd_states")
                       (nlk:json-value decoded :object "attrs")))
           (unmatched (coerce (nlk:json-array decoded "unmatched") 'list)))
      (unless states
        (fail "I understood ~a, and not enough of it to make a voice~@[: I did not catch ~{~a~^, ~}~]: try plainer words (young or older, warm or clear, a named accent), or start from a voice /vs-voices lists"
              (or (nlk:json-value decoded :string "understood")
                  (nlk:json-value decoded :string "matched")
                  "the description")
              unmatched))
      (multiple-value-bind (body status headers)
          (post-form "/profiles" (list (cons "kind" "design")
                                        (cons "name" name)
                                        (cons "vd_states" (nlk:encode-json-object states))))
        (declare (ignore headers))
        (voice-made name (profile-answer status body name) preview unmatched)))))

(defun design (name &key describe sample (preview t))
  "A new voice from words or from a recording, as text."
  (let ((name (or (nonblank name) (fail "give the new voice a name")))
        (describe (nonblank describe))
        (sample (nonblank sample)))
    (when (and describe sample)
      (fail "use describe or sample, not both: describe a voice in words, or clone one from a recording"))
    (cond (sample (clone-voice name sample preview))
          (describe (design-voice name describe preview))
          (t (fail "say what the voice should sound like (describe), or name a recording to clone (sample)")))))

;;; --- vs_longform ------------------------------------------------------------------

(defparameter +unsupported-tags+
  '("\\[\\s*(?:sigh|laugh|breath|cough|gasp|break|rate[^\\]]*)\\s*\\]"
    "<break\\b[^>]*/?>"
    "\\(\\s*pause\\s*\\)"
    "\\*[a-z]+sighs?\\*|\\*sighs?\\*")
  "Markup a voice would read aloud literally (tools_longform.py:25).")

(defun unsupported-markup (text)
  "TEXT's unsupported markup => (values TAGS LINES), in reading order."
  (let ((tags '()) (lines '()))
    (loop for line in (ppcre:split "\\n" text)
          for number from 1
          do (dolist (pattern +unsupported-tags+)
               (ppcre:do-matches-as-strings (match pattern line)
                 (pushnew match tags :test #'string=)
                 (pushnew number lines))))
    (values (nreverse tags) (sort lines #'<))))

(defun chapter-titles (text)
  "The script's # headings: one chapter each; ## stays body text
(tools_longform.py:61)."
  (loop for line in (ppcre:split "\\n" text)
        when (ppcre:scan "^# (?!#)" line)
          collect (string-trim '(#\Space #\Tab #\Return) (subseq line (min 2 (length line))))))

(defun check-markup (text)
  (multiple-value-bind (tags lines) (unsupported-markup text)
    (when tags
      (fail "the script uses ~{~a~^, ~} on lines ~{~a~^, ~}, which a voice cannot perform: it would read them aloud literally. Remove them, or replace a sigh or a laugh with [pause 600ms] and a rewritten line. Supported: [pause], [pause 500ms], [slow], [fast], [emphasis], [spell], [[word|say-this]], [voice:NAME] and # chapter headings"
            tags (subseq lines 0 (min 12 (length lines)))))))

(defun audiobook-body (text voice container title author seed)
  "POST /audiobook's body, as the plugin built it (tools_longform.py:200)."
  (let ((body (nlk:make-json-object "text" text "default_voice" voice
                                     "format" container "loudness" "podcast")))
    (let ((metadata (remove nil (list (let ((title (nonblank title)))
                                        (and title (cons "title" title)))
                                      (let ((author (nonblank author)))
                                        (and author (cons "author" author)))))))
      (when metadata
        (setf (gethash "metadata" body)
              (apply #'nlk:make-json-object (loop for (key . value) in metadata
                                                  append (list key value))))))
    (when seed (setf (gethash "seed" body) seed))
    body))

(defun render-report (path container chapters &optional titles)
  (format nil "wrote ~a (~a)~%~a, ~d chapter~:p~@[ -- ~{~a~^, ~}~]"
          path (nlk:size-text (file-size path)) container chapters
          (and titles (subseq titles 0 (min 6 (length titles))))))

(defun fetch-render (output job container chapters &optional titles)
  "GET /audio/<output> and keep the artifact under the section's folder."
  (unless (nonblank output)
    (fail "VoiceStudio finished (job ~a) but named no file; run /vs-status and try again" job))
  (let* ((output (string-left-trim "/" (nonblank output)))
         (extension (or (pathname-type (pathname output)) container)))
    (multiple-value-bind (body status headers)
        (exchange :get (format nil "/audio/~a" output) :binary t)
      (declare (ignore headers))
      (unless (= status 200)
        (fail "VoiceStudio finished rendering but I could not fetch ~a (HTTP ~a)" output status))
      (let ((written (write-bytes body (merge-pathnames (save-name "vs-audiobook" extension)
                                                         (setting :directory)))))
        (render-report written container chapters titles)))))

(defun find-job (payload job)
  "JOB's own record inside a listed answer, or NIL."
  (loop for item across (items-of payload)
        when (and (hash-table-p item)
                  (string= (or (nlk:json-value item :string "job_id")
                               (nlk:json-value item :string "id")
                               "")
                           job))
          return item))

(defun render-state (job)
  "The render's record: /jobs/<id> first, else the job lists
(tools_longform.py:322)."
  (or (nth-value 0 (get-json (format nil "/jobs/~a" job) :timeout (setting :probe)))
      (find-job (nth-value 0 (get-json "/audiobook/jobs" :timeout (setting :probe))) job)
      (find-job (nth-value 0 (get-json "/longform/jobs" :timeout (setting :probe))) job)
      (nlk:make-json-object)))

(defun wait-for-render (job container chapters &optional titles)
  (multiple-value-bind (state where)
      (wait-for-job (lambda () (render-state job)))
    (case where
      (:done
       (let ((artifact (or (nlk:json-value state :string "destination_path")
                           (nlk:json-value state :string "audio_path")
                           (nlk:json-value state :string "path")
                           (nlk:json-value state :string "file"))))
         (if artifact
             (render-report artifact container
                            (or (nlk:json-value state :integer "chapters") chapters) titles)
             (format nil "render finished (job ~a); the server named no file" job))))
      (:failed
       (fail "rendering stopped~@[ at chapter ~a~]; nothing is lost: run it again with resume ~a"
             (nlk:json-value state :integer "chapter") job))
      (t
       (fail "still rendering (job ~a~@[, ~a~]); run it again with resume ~a to continue, nothing is lost"
             job (nonblank (status-word state)) job)))))

(defun longform-refusal (body voice)
  (let ((detail (nlk:json-value (decode body) :string "detail")))
    (when (and detail (search "voice" (string-downcase detail)))
      (fail "I do not know the narrator voice '~a': run /vs-voices --search ~a to find it" voice voice))
    (fail "VoiceStudio did not accept that script~@[: ~a~]" detail)))

(defun longform (file &key voice container title author resume seed)
  "A chaptered render from a markdown script, as text."
  (let ((voice (or (nonblank voice)
                   (nonblank (setting :default-voice))
                   (fail "no narrator chosen and no default voice is set: run /vs-voices to pick one, then name it")))
        (container (string-downcase (or (nonblank container) "m4b"))))
    (unless (member container '("m4b" "mp3") :test #'string=)
      (setf container "m4b"))
    (if (nonblank resume)
        (progn
          (post-json (format nil "/audiobook/resume/~a" (nonblank resume))
                     (nlk:make-json-object))
          (wait-for-render (nonblank resume) container 1))
        (let* ((script (input-file file :types +script-types+ :label "script"))
               (text (uiop:read-file-string script)))
          (unless (nonblank text)
            (fail "that script is empty: add the episode text first"))
          (check-markup text)
          (let ((titles (chapter-titles text)))
            ;; The plan is the server's own chapter pass; the plugin asked for it
            ;; and read nothing back (tools_longform.py:196).
            (ignore-errors
             (post-json "/audiobook/plan"
                         (nlk:make-json-object "text" text "default_voice" voice)))
            (multiple-value-bind (body status headers)
                (post-json "/audiobook" (audiobook-body text voice container title author seed))
              (when (member status '(400 422))
                (longform-refusal body voice))
              (unless (= status 200)
                (fail "VoiceStudio answered HTTP ~a for that script; run /vs-status and try again" status))
              (if (sse-body-p body (header headers "content-type"))
                  (finish-sse body container (length titles) titles)
                  (let ((payload (decode body)))
                    (let ((job (job-id-of payload))
                          (artifact (or (nlk:json-value payload :string "destination_path")
                                        (nlk:json-value payload :string "audio_path")
                                        (nlk:json-value payload :string "path"))))
                      (cond ((and artifact (null job))
                             (render-report artifact container (length titles) titles))
                            (job (wait-for-render job container (length titles) titles))
                            (t (fail "VoiceStudio accepted the script but gave no job to track; run /vs-status and try again"))))))))))))

;;; --- the audiobook's event stream -------------------------------------------------

(defun sse-body-p (body content-type)
  "Whether BODY is a server-sent-event stream: the plugin's own test
(client.py:186)."
  (or (search "event-stream" (string-downcase (or content-type "")))
      (uiop:string-prefix-p "data:" (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                                      (body-text body)))))

(defun sse-frames (body)
  "BODY's server-sent events as decoded JSON objects, oldest first
(client.py:198)."
  (let ((frames '()))
    (dolist (block (ppcre:split "\\r?\\n\\r?\\n" (body-text body)))
      (let ((data (loop for raw in (ppcre:split "\\r?\\n" block)
                        for line = (string-trim '(#\Space #\Tab #\Return) raw)
                        when (uiop:string-prefix-p "data:" line)
                          collect (string-trim '(#\Space #\Tab) (subseq line 5)))))
        (when data
          (let ((frame (ignore-errors (nlk:decode-json (format nil "~{~a~^~%~}" data)))))
            (when (hash-table-p frame) (push frame frames))))))
    (nreverse frames)))

(defun sse-done (frames)
  "FRAMES' last done event, the one carrying the artifact, or NIL."
  (let ((done nil))
    (dolist (frame frames)
      (when (string= (string-downcase (or (nlk:json-value frame :string "type") "")) "done")
        (setf done frame)))
    (and done (nonblank (nlk:json-value done :string "output")) done)))

(defun sse-job (frames)
  (loop for frame in frames
        for job = (nlk:json-value frame :string "job_id")
        when job return job))

(defun finish-sse (body container chapters titles)
  (let* ((frames (sse-frames body))
         (done (sse-done frames))
         (job (or (and done (nlk:json-value done :string "job_id")) (sse-job frames))))
    (cond ((and done (nlk:json-value done :string "output"))
           (fetch-render (nlk:json-value done :string "output") job container chapters titles))
          (job (wait-for-render job container chapters titles))
          (t (fail "VoiceStudio accepted the script but gave no job to track; run /vs-status and try again")))))

;;; --- vs_dub -----------------------------------------------------------------------

(defun language-list (text)
  "TEXT's languages, comma or space separated, as the plugin read them
(tools_dub.py:78)."
  (let ((langs (remove-if (lambda (word) (zerop (length word)))
                           (uiop:split-string (substitute #\, #\; (or text ""))
                                              :separator '(#\, #\Space #\Tab #\Newline)))))
    (or langs (fail "which languages should the dub be in? such as es,fr"))))

(defun dub-files (state)
  (append (coerce (nlk:json-array state "paths") 'list)
          (coerce (nlk:json-array state "files") 'list)))

(defun dub-answer (state langs)
  "What finished dubbing left on disk, per language when one failed."
  (let* ((files (dub-files state))
         (failed (nlk:json-value state :object "failed")))
    (if (and failed (plusp (hash-table-count failed)))
        (format nil "dubbed ~{~a~^, ~}, but ~{~a~^, ~} failed~%files: ~{~a~^, ~}"
                (remove-if (lambda (lang) (gethash lang failed)) langs)
                (loop for lang being the hash-keys of failed collect lang)
                files)
        (format nil "dubbed ~{~a~^, ~}~%files: ~{~a~^, ~}" langs files))))

(defun wait-for-dub (job langs)
  (multiple-value-bind (state where)
      (wait-for-job (lambda ()
                      (nth-value 0 (get-json (format nil "/batch/jobs/~a" job)
                                              :timeout (setting :probe)))))
    (case where
      (:done (dub-answer state langs))
      (:failed (fail "the dub job failed (~a); run /vs-status and try again"
                     (or (nlk:json-value state :string "error") job)))
      (t (fail "the dub is still going (job ~a); /batch/jobs/~a keeps it" job job)))))

(defun dub (video langs voice)
  "A dub job started, and what it left on disk, as text."
  (let ((file (input-file video :types +video-types+ :label "video"))
        (langs (language-list langs))
        (voice (or (nonblank voice)
                   (nonblank (setting :default-voice))
                   (fail "no voice chosen and no default voice is set: run /vs-voices to pick one, then name it"))))
    (multiple-value-bind (body status headers)
        (post-form "/batch/enqueue" (list (cons "langs" (format nil "~{~a~^,~}" langs))
                                          (cons "voice_id" voice)
                                          (cons "video" (pathname file))))
      (declare (ignore headers))
      (when (= status 404)
        ;; The plugin's other door for servers exposing the JSON route
        ;; (tools_dub.py:94).
        (multiple-value-bind (other-body other-status other-headers)
            (post-json "/dub/translate"
                       (nlk:make-json-object "video" file "langs" langs "voice_id" voice))
          (declare (ignore other-headers))
          (setf body other-body status other-status)))
      (unless (member status '(200 201 202))
        (fail "VoiceStudio did not accept that dub job (HTTP ~a); run /vs-status and try again" status))
      (let* ((payload (decode body))
             (job (job-id-of payload)))
        (if job
            (wait-for-dub job langs)
            (let ((files (dub-files payload)))
              (if files
                  (format nil "dubbed ~{~a~^, ~}~%files: ~{~a~^, ~}" langs files)
                  (fail "VoiceStudio accepted the dub but named no file: run /vs-status and try again"))))))))

;;; --- vs_status --------------------------------------------------------------------

(defun optional-json (path)
  "PATH's decoded answer, or NIL: a status read is best effort, as the plugin
read it (tools_dub.py:163)."
  (handler-case (nth-value 0 (get-json path :timeout (setting :probe)))
    (error () nil)))

(defun status-report (&key (detail t))
  "Whether VoiceStudio is ready, as /vs-status answered (tools_dub.py:160)."
  (let ((setup (optional-json "/setup/status")))
    (unless (hash-table-p setup)
      (return-from status-report (format nil +unreachable+ (base-url))))
    (let* ((engines (and detail (optional-json "/engines")))
           (model (and detail (optional-json "/model/status")))
           (errors (and detail (optional-json "/system/errors/recent")))
           (voices (length (items-of (optional-json "/profiles"))))
           (missing (coerce (nlk:json-array setup "missing") 'list)))
      (multiple-value-bind (models-ready present)
          (nlk:json-value setup :boolean "models_ready")
        (let ((ready (if present models-ready (null missing))))
          (with-output-to-string (out)
            (format out "VoiceStudio at ~a is ~:[not ready~;ready~]~%" (base-url) ready)
            (when missing (format out "missing: ~{~a~^, ~}~%" missing))
            (when detail
              (format out "engine: ~a~%"
                      (or (nlk:json-value engines :string "active")
                          (nlk:json-value engines :string "engine")
                          "unknown")))
            (format out "voices: ~d~%" voices)
            (when detail
              (let ((free (nlk:json-value setup :number "disk_free_gb")))
                (when free (format out "disk free: ~a GB~%" free)))
              (let ((state (nlk:json-value model :string "state")))
                (when state (format out "model: ~a~%" state)))
              (when errors
                (format out "recent errors: ~a~%" (nlk:encode-json-object errors))))
            (format out "files go to ~a~%" (directory-path))
            (format out "default voice: ~a~%"
                    (or (nonblank (setting :default-voice)) "none"))))))))

(defun wait-for-job (read-job)
  "READ-JOB answers the job's own record. Read it again between waits, up to
the section's polls => (values STATE :DONE | :FAILED | :PENDING)."
  (let ((state nil))
    (dotimes (attempt (setting :polls))
      (declare (ignore attempt))
      (setf state (funcall read-job))
      (let ((status (status-word state)))
        (cond ((member status +done-words+ :test #'string=)
               (return-from wait-for-job (values state :done)))
              ((member status +failed-words+ :test #'string=)
               (return-from wait-for-job (values state :failed)))
              (t (funcall *sleep* (setting :poll-interval))))))
    (values state :pending)))

;;; --- what the model calls --------------------------------------------------------------

(nlk:define-contract speak-call
  (:doc "Read a text aloud in a VoiceStudio voice and keep the audio file. Use it for a sentence or a few paragraphs, up to 6000 characters; for chapters or an audiobook use vs_longform.")
  ("text" :text :required t
          :doc "the words to say, up to 6000 characters")
  ("voice" :text
           :doc "a voice id from vs_list_voices; omitted, the section's default voice")
  ("speed" :number :slot pace :default 1.0d0
           :doc "the pace: 1.0 normal, 0.25 to 4.0")
  ("seed" :integer
          :doc "a seed an earlier take answered, to repeat it exactly")
  ("out_path" :text :slot out-path
              :doc "a file name for the audio, inside the section's folder; omitted, the cell names it"))

(nlk:define-contract design-call
  (:doc "Make a reusable VoiceStudio voice: describe it in words, or clone it from a short wav or m4a recording. Use describe or sample, never both, and give the voice a name.")
  ("name" :text :required t
          :doc "what to call the new voice")
  ("describe" :text :slot words
              :doc "plain words, up to 2000 characters, such as warm young female, slight British accent")
  ("sample" :text
            :doc "a wav or m4a recording of a few seconds of clear speech to clone")
  ("preview" :boolean :default t
             :doc "render a short sample of the new voice (default true)"))

(nlk:define-contract voices-call
  (:doc "List the voices VoiceStudio offers: the saved ones, the gallery, and the presets, with the ids every other tool takes. Read-only and cheap: call it first when the user names no voice.")
  ("search" :text :slot query
            :doc "a word or two that must appear in a voice's name or description")
  ("kind" :text :options '("mine" "gallery" "presets")
          :doc "list only one kind; omitted, all three"))

(nlk:define-contract longform-call
  (:doc "Render a markdown script into chaptered audio: an audiobook or a podcast episode. A # heading starts a chapter, ## stays body text. The supported markup is [pause], [pause 500ms], [slow], [fast], [emphasis], [spell], [[word|say-this]] and [voice:NAME]; anything else is refused before the render, because a voice would read it aloud literally.")
  ("file" :text :required t
          :doc "the script: a .md or .txt file, its # headings splitting chapters")
  ("voice" :text
           :doc "the narrator voice id; omitted, the section's default voice")
  ("format" :text :slot container :options '("m4b" "mp3")
            :doc "the container: m4b keeps chapters, mp3 is widely played")
  ("title" :text
           :doc "a title written into the file's information")
  ("author" :text
            :doc "an author written into the file's information")
  ("resume" :text
            :doc "a job id an interrupted render answered: continues it")
  ("seed" :integer
          :doc "a seed, to repeat a render"))

(nlk:define-contract dub-call
  (:doc "Dub a video into other languages with a VoiceStudio voice. The answer names the file each language made; when one language fails, the others are kept.")
  ("video" :text :required t
           :doc "the video file: mp4, mov, mkv, webm or m4v")
  ("langs" :text :required t
           :doc "the target languages, comma separated, such as es,fr")
  ("voice" :text
           :doc "the voice for the dubbed speech; omitted, the section's default voice"))

(nlk:define-contract status-call
  (:doc "Say whether VoiceStudio is running and ready: which engine is loaded, how many voices there are, how much disk is free, and what is missing. Call it when anything else reports the server unreachable, or when the user asks whether VoiceStudio is ready.")
  ("detail" :boolean :default t
            :doc "the engine, model, disk and error lines (default true); false answers one line"))

(defun speak-tool (call)
  "The vs_speak tool."
  (answering-tool
    (speak (speak-call-text call)
           :voice (speak-call-voice call)
           :speed (speak-call-pace call)
           :seed (speak-call-seed call)
           :out-path (speak-call-out-path call))))

(defun design-tool (call)
  "The vs_design_voice tool."
  (answering-tool
    (design (design-call-name call)
            :describe (design-call-words call)
            :sample (design-call-sample call)
            :preview (design-call-preview call))))

(defun voices-tool (call)
  "The vs_list_voices tool."
  (answering-tool
    (list-voices :search (voices-call-query call)
                 :kind (voices-call-kind call))))

(defun longform-tool (call)
  "The vs_longform tool."
  (answering-tool
    (longform (longform-call-file call)
              :voice (longform-call-voice call)
              :container (longform-call-container call)
              :title (longform-call-title call)
              :author (longform-call-author call)
              :resume (longform-call-resume call)
              :seed (longform-call-seed call))))

(defun dub-tool (call)
  "The vs_dub tool."
  (answering-tool
    (dub (dub-call-video call) (dub-call-langs call) (dub-call-voice call))))

(defun status-tool (call)
  "The vs_status tool."
  (answering-tool (status-report :detail (status-call-detail call))))

(defun settings-text ()
  "What the cell runs on, as a command prints it."
  (format nil "VoiceStudio at ~a; default voice ~a; files in ~a~%tools: vs_speak, vs_design_voice, vs_list_voices, vs_longform, vs_dub, vs_status"
          (base-url) (or (nonblank (setting :default-voice)) "none") (directory-path)))

(defparameter *usage*
  "the six commands, beside the settings /voicestudio prints:
  /vs-speak --text \"the words\" [--voice id] [--speed 1.2] [--seed n] [--out-path take.mp3]
  /vs-design --name NAME (--describe \"warm young female\" | --sample voice.wav) [--no-preview]
  /vs-voices [--search word] [--kind mine|gallery|presets]
  /vs-longform --file episode.md [--voice id] [--format m4b|mp3] [--title t] [--author a] [--resume job]
  /vs-dub --video intro.mp4 --langs es,fr [--voice id]
  /vs-status [--detail false]
a call that names no voice uses the section's default voice; every answer names the file it wrote"
  "What /voicestudio answers beside the settings: the six mirrors of the plugin's commands.")

(defun command-main (arguments session-id)
  "The cell's own command: what it offers and where it writes, or --status
for the server's own readiness line."
  (declare (ignore session-id))
  (answering
    (let ((flags (parse-flags arguments)))
      (if (flag flags "status")
          (status-report)
          (format nil "~a~%~a" (settings-text) *usage*)))))

(defun command-speak (arguments session-id)
  (declare (ignore session-id))
  (answering
    (let ((flags (parse-flags arguments)))
      (if (null (flag flags "text"))
          (format nil "usage: /vs-speak --text \"the words\" [--voice id] [--speed 1.2] [--seed n] [--out-path take.mp3]~%~a"
                  (settings-text))
          (speak (flag flags "text")
                 :voice (flag flags "voice")
                 :speed (flag-number flags "speed")
                 :seed (flag-integer flags "seed")
                 :out-path (flag flags "out-path"))))))

(defun command-design (arguments session-id)
  (declare (ignore session-id))
  (answering
    (let ((flags (parse-flags arguments)))
      (if (null (flag flags "name"))
          "usage: /vs-design --name \"Narrator\" --describe \"warm young female, slight British accent\"  or  --sample voice.wav [--no-preview]"
          (design (flag flags "name")
                  :describe (flag flags "describe")
                  :sample (flag flags "sample")
                  :preview (not (flag flags "no-preview")))))))

(defun command-voices (arguments session-id)
  (declare (ignore session-id))
  (answering
    (let ((flags (parse-flags arguments)))
      (list-voices :search (flag flags "search") :kind (flag flags "kind")))))

(defun command-longform (arguments session-id)
  (declare (ignore session-id))
  (answering
    (let ((flags (parse-flags arguments)))
      (if (null (flag flags "file"))
          "usage: /vs-longform --file episode.md [--voice id] [--format m4b|mp3] [--title t] [--author a] [--resume job]"
          (longform (flag flags "file")
                    :voice (flag flags "voice")
                    :container (flag flags "format")
                    :title (flag flags "title")
                    :author (flag flags "author")
                    :resume (flag flags "resume")
                    :seed (flag-integer flags "seed"))))))

(defun command-dub (arguments session-id)
  (declare (ignore session-id))
  (answering
    (let ((flags (parse-flags arguments)))
      (if (null (flag flags "video"))
          "usage: /vs-dub --video intro.mp4 --langs es,fr [--voice id]"
          (dub (flag flags "video") (flag flags "langs") (flag flags "voice"))))))

(defun command-status (arguments session-id)
  (declare (ignore session-id arguments))
  (answering (status-report)))

;;; --- the manual --------------------------------------------------------------------

(defparameter *manual*
  "VoiceStudio is available through the nodecode-voicestudio cell: six tools, and the
commands /vs-speak, /vs-design, /vs-voices, /vs-longform, /vs-dub and /vs-status;
/voicestudio prints them with the settings, and --status asks the server itself.
  vs_list_voices  {search, kind}
      The voices the server offers -- yours, the gallery, the presets -- with the ids the
      other tools take. Cheap and read-only: call it first when the user names no voice.
  vs_speak  {text, voice, speed, seed, out_path}
      One text into an audio file, up to 6000 characters. The answer names the file, the
      voice and the seed that repeats the take; speed runs 0.25 to 4.0. Use out_path only
      for a bare file name inside the cell's own folder.
  vs_design_voice  {name, describe, sample, preview}
      A new voice: describe it in words (up to 2000 characters) or clone it from a wav or
      m4a recording, never both. The answer names its id and, unless preview is false, a
      rendered sample.
  vs_longform  {file, voice, format, title, author, resume}
      A chaptered render from a .md or .txt script; each # heading starts a chapter. The
      supported markup is [pause], [pause 500ms], [slow], [fast], [emphasis], [spell],
      [[word|say-this]] and [voice:NAME]; anything else is refused before the render (a
      voice would read it aloud literally). When a render is interrupted, run it again
      with the job id in resume: nothing is lost.
  vs_dub  {video, langs, voice}
      A video dubbed into comma-separated languages, such as es,fr. The answer names the
      file each language made; when one language fails the others are kept.
  vs_status  {}
      Whether VoiceStudio is running and ready, which engine is loaded, how many voices
      there are, and what is missing. Call it when anything reports the server
      unreachable, or when the user asks whether VoiceStudio is ready.
The files land in the cell's own folder, and the answer names each one: give the user
that path. The audio is made by the VoiceStudio app on this machine; a section only says
which address it answers on."
  "What (help :voicestudio) answers while the cell runs.")

;;; --- the section -------------------------------------------------------------------

(defun checked-settings (values table)
  "The declared values, with the address trimmed and held to a scheme."
  (declare (ignore table))
  (let ((url (string-right-trim "/" (or (getf values :base-url) ""))))
    (unless (or (uiop:string-prefix-p "http://" url)
                (uiop:string-prefix-p "https://" url))
      (nlk:config-error "base_url ~s has no http:// or https:// scheme: it is the address the VoiceStudio app answers on, such as http://127.0.0.1:3900" url))
    (setf (getf values :base-url) url))
  values)

(nle:define-cell voicestudio
  (:section ("voicestudio")
    (:guide "start the VoiceStudio app first; base_url is the address its server answers on (http://127.0.0.1:3900 unless the app says otherwise); default_voice is the voice a call names none, and /vs-voices lists the ids; directory is where the audio files are written; timeout bounds one generation, poll_interval and polls bound how long the cell waits for a render")
    ("base_url" :string :default "http://127.0.0.1:3900"
                :doc "the address the VoiceStudio app answers on")
    ("default_voice" :string
                     :doc "the voice a call names none; /vs-voices lists the ids")
    ("directory" :directory :default "voice-studio/"
                 :doc "where the audio files are written, under the home")
    ("timeout" :integer :default 600 :min 1
               :doc "seconds one generation may take")
    ("connect" :integer :default 3 :min 1
               :doc "seconds a call waits for the server to answer at all")
    ("probe" :integer :default 3 :min 1
             :doc "seconds one status or job read waits")
    ("poll_interval" :integer :default 10 :min 0
                     :doc "seconds between two job reads")
    ("polls" :integer :default 60 :min 1
             :doc "how many times a job is read before the cell answers that it is still going"))
  (:settings (lambda (values table) (checked-settings values table)))
  (:tool "vs_speak" (nlk:contract 'speak-call) #'speak-tool)
  (:tool "vs_design_voice" (nlk:contract 'design-call) #'design-tool)
  (:tool "vs_list_voices" (nlk:contract 'voices-call) #'voices-tool)
  (:tool "vs_longform" (nlk:contract 'longform-call) #'longform-tool)
  (:tool "vs_dub" (nlk:contract 'dub-call) #'dub-tool)
  (:tool "vs_status" (nlk:contract 'status-call) #'status-tool)
  (:help :voicestudio
         "voicestudio: VoiceStudio speech on this machine - speak, design a voice, list voices, render a chaptered audiobook, dub a video, and check the server"
         *manual*)
  (:command "voicestudio" 'command-main
            :description "VoiceStudio: what this cell offers, and whether the server is ready"
            :argument-hint "[--status]")
  (:command "vs-speak" 'command-speak
            :description "Say a text in a VoiceStudio voice, and name the audio file"
            :argument-hint "--text \"...\" [--voice id] [--speed 1.2]")
  (:command "vs-design" 'command-design
            :description "Design a voice from words, or clone one from a recording"
            :argument-hint "--name NAME (--describe \"...\" | --sample file.wav)")
  (:command "vs-voices" 'command-voices
            :description "List the voices VoiceStudio offers"
            :argument-hint "[--search word] [--kind mine|gallery|presets]")
  (:command "vs-longform" 'command-longform
            :description "Render a chaptered script into an audiobook"
            :argument-hint "--file episode.md [--voice id] [--format m4b|mp3]")
  (:command "vs-dub" 'command-dub
            :description "Dub a video into other languages"
            :argument-hint "--video intro.mp4 --langs es,fr [--voice id]")
  (:command "vs-status" 'command-status
            :description "Is VoiceStudio running and ready?"
            :argument-hint "")
  (:start (lambda ()
            ;; The folder the answers name exists from the first start.
            (ensure-directories-exist (setting :directory)))))
