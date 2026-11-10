;;;; cell.lisp --- the phone's fifty-eight android_* verbs, over the relay's JSON routes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was a Hermes sidecar: a Python module whose every
;;;; tool was one HTTP call to a relay (localhost:8766 by default) that the
;;;; plugin itself started, and whose phone half is a Kotlin app. The tools are
;;;; the part a model uses, and every one of them was a thin wrapper over one
;;;; route, so this cell keeps the routes and the tools and speaks them
;;;; directly: the relay is the operator's to run, and url names it.
;;;;
;;;; What is under test and what is not: *HTTP* is the one seam, so a test
;;;; asserts the method, the URL, the headers and the JSON body of a call and
;;;; answers what it chose, with no relay, no phone and no network.

(in-package #:nodecode-android-bridge)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Android Bridge is available through the nodecode-android-bridge cell: fifty-eight android_*
tools, which a person can also call from eval by name. They drive ONE paired Android phone
through the Hermes Bridge companion app, reached at the `url' of the android-bridge section
(or the pairing code's relay in front of it), with the app's pairing code as the Bearer
token. Nothing here acts on this machine; every call lands on the phone.
  android_ping             does the relay answer, and is a phone connected.
  android_read_screen      the accessibility tree of the current screen: node ids, text,
                           classes, and with include_bounds the pixel rectangles. Read
                           this before tapping anything; it is cheaper than a screenshot.
  android_find_nodes       the nodes matching a text, a class or clickability, without the
                           whole tree; android_describe_node reads one node in full.
  android_tap              one node by node_id, or a point by x and y.
  android_tap_text         the first element whose text matches; android_type types into
                           the focused field; android_press_key is back, home, recents ...
  android_swipe, android_scroll, android_drag, android_pinch, android_long_press
                           the gestures; android_wait waits for an element to appear.
  android_screenshot       a base64 PNG; android_screen_record a short MP4.
  android_screen_hash, android_diff_screen
                           a cheap fingerprint of the screen, for polling.
  android_get_apps, android_current_app, android_open_app, android_send_intent,
  android_broadcast         what is installed, what is in front, and how to start it.
  android_clipboard_read, android_clipboard_write
                           the phone's clipboard.
  android_notifications, android_notify_reply
                           recent notifications, and a reply into one with a direct-reply
                           action.
  android_send_sms, android_call, android_search_contacts
                           messages, calls and the numbers for them.
  android_location, android_health, android_network_status
                           where it is, Health Connect totals for the last days, and the
                           network it is on.
  android_files_list, android_files_search, android_files_count, android_file_get,
  android_file_put, android_file_delete, android_files_permission
                           shared storage: browse, search, count, fetch a file to this
                           machine (MEDIA path in the answer), push one, delete, and the
                           all-files permission.
  android_photo, android_torch, android_media, android_volume, android_alarm,
  android_timer, android_speak, android_speak_stop
                           the camera, the flashlight, playback, sound, the clock and
                           text-to-speech.
  android_mic_record, android_mic_stop, android_mic_status, android_mic_fetch
                           a 16 kHz WAV recording, and its copy on this machine.
  android_events, android_event_stream, android_read_widgets, android_screen_hash
                           what changed on screen, and the home-screen widgets.
  android_macro              a list of {tool, args} run in order, stopping on the first
                           failure.
  android_apk_install        the companion app's own update, fetched from the relay the
                           phone already trusts; the person confirms on the device.
  android_setup              whether the relay answers, and what to do next.
Every tool answers with what the relay answered, as JSON, or with `error: ...' when the
call could not be made. SMS, calls, file deletion and the intents are risk-gated on the
phone side only: say what you are about to do before you do it."
  "What (help :android-bridge) answers while the cell runs.")

;;; --- the section, and the one refusal it makes at start ------------------------

(defun nonblank (text)
  (let ((text (and (stringp text)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, with the relay's URL checked: no URL is no bridge."
  (declare (ignore table))
  (let ((url (nonblank (getf values :url))))
    (unless url
      (nlk:config-error "url is empty: set the android-bridge section's url to the relay the phone dials (e.g. http://127.0.0.1:8766), or to the phone's own address on your LAN"))
    (let ((plist (copy-list values)))
      (setf (getf plist :url) url)
      plist)))

(defun base-url ()
  "The configured relay's base URL, without a trailing slash."
  (string-right-trim "/" (setting :url)))

;;; --- the one dial ---------------------------------------------------------------

(defparameter *http* #'nlk:http
  "How a call reaches the relay: (funcall *HTTP* METHOD URL :HEADERS H :CONTENT C
:TIMEOUT S :BINARY B) => (values BODY STATUS HEADERS TRUNCATED-P), as nlk:http
answers. A test binds it and needs no relay, no phone and no network.")

(defun auth-headers ()
  "The Bearer the phone's relay checks, when a pairing code is configured."
  (let ((token (nonblank (setting :token))))
    (when token
      (list (cons "Authorization" (format nil "Bearer ~a" token))))))

(defun call-relay (method path &key content binary)
  "One exchange with the relay, at PATH under the configured URL.
=> (values BODY STATUS); BODY is words a person can act on when the dial failed."
  (let ((url (concatenate 'string (base-url) path)))
    (handler-case
        (multiple-value-bind (body status)
            (funcall *http* method url
                     :headers (if content
                                  (cons (cons "Content-Type" "application/json")
                                        (auth-headers))
                                  (auth-headers))
                     :content content
                     :timeout (setting :timeout-seconds)
                     :binary binary)
          (values body status))
      (error (condition)
        (values (format nil "could not reach ~a: ~a" url
                        (nlk:one-line (princ-to-string condition)))
                nil)))))

(defun clip (text limit)
  (cond ((not (stringp text)) "")
        ((<= (length text) limit) text)
        (t (format nil "~a… (~d characters, clipped)" (subseq text 0 limit) (length text)))))

(defun answer (body status)
  "BODY as a tool answer: an error status's body, said as an error."
  (cond ((not (stringp body)) "")
        ((null status) body)
        ((>= status 400) (format nil "error: HTTP ~d: ~a" status (clip body 600)))
        (t (clip body (setting :max-output-chars)))))

;;; --- the wire ------------------------------------------------------------------

(defun percent-encode (text)
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          do (if (or (<= 48 octet 57) (<= 65 octet 90) (<= 97 octet 122)
                     (member octet '(45 46 95 126)))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun query-value (value)
  (if (eq value t) "true" (princ-to-string value)))

(defun query-string (path pairs)
  "PATH with PAIRS appended as a query, each value percent-encoded."
  (if (null pairs)
      path
      (format nil "~a?~{~a~^&~}" path
              (loop for (name . value) in pairs
                    collect (format nil "~a=~a" name
                                    (percent-encode (query-value value)))))))

(defun payload-json (pairs)
  "PAIRS as the JSON object a POST carries; no pairs is {}."
  (if (null pairs)
      "{}"
      (nlk:encode-json-object
       (apply #'nlk:make-json-object
              (loop for (name . value) in pairs append (list name value))))))

;;; --- the arguments, as the model sends them -------------------------------------

(defun keywordize (name)
  "A wire key as the keyword the arguments alist is read by: clear_first is
:CLEAR-FIRST."
  (intern (substitute #\- #\_ (string-upcase name)) :keyword))

(defun arguments-alist (arguments)
  "The decoded ARGUMENTS table as an alist of (keyword . value)."
  (if (hash-table-p arguments)
      (loop for key being the hash-keys of arguments using (hash-value value)
            when (stringp key) collect (cons (keywordize key) value))
      '()))

(defun arg (args key &optional default)
  "KEY's value in ARGS, or DEFAULT when the model sent none."
  (let ((cell (assoc key args)))
    (if cell (cdr cell) default)))

(defun wire (args &rest specs)
  "The (wire-key . value) pairs ARGS carries, from SPECS of wire-key, arg-key."
  (loop for (name key) on specs by #'cddr
        for value = (arg args key)
        when value collect (cons name value)))

(defun sender (method route &rest specs)
  "One verb's send function: METHOD and ROUTE, with SPECS as the body or query."
  (lambda (args)
    (values method route (apply #'wire args specs))))

;;; --- schemas out of one table ---------------------------------------------------

(defun verb-schema (name)
  "The plist provider schema for the verb called NAME, from its own parameters."
  (let ((verb (find-verb name))
        (properties (make-hash-table :test #'equal))
        (required '()))
    (dolist (parameter (getf verb :params))
      (destructuring-bind (argument type required-p doc) parameter
        ;; :ANY writes no type at all: a member that may be a string or a list
        ;; of them is advertised by its description, as the original did.
        (setf (gethash argument properties)
              (if (eq type :any)
                  (nlk:make-json-object "description" doc)
                  (nlk:make-json-object "type" (string-downcase (symbol-name type))
                                         "description" doc)))
        (when required-p (push argument required))))
    (list :description (getf verb :description)
          :parameters (nlk:make-json-object
                       "type" "object"
                       "properties" properties
                       "required" (coerce (nreverse required) 'vector)))))

(defun verb-handler (name)
  "The tool function the model calls for the verb called NAME."
  (lambda (arguments) (handle-tool name arguments)))

;;; --- the verbs, one route each --------------------------------------------------

(defun verb (name method route description params send)
  "One verb of the table: its tool NAME, its METHOD and ROUTE on the relay,
what it is for, its PARAMS and how it builds its SEND."
  (list :name name :method method :route route :description description
        :params params :send send))

(defun find-verb (name)
  (find name *verbs* :key (lambda (verb) (getf verb :name)) :test #'string=))

(defparameter *verbs*
  (list
   (verb "android_ping" :get "/ping"
         "Check that the phone's relay answers, and whether a phone is connected. Call this first; every other tool needs it."
         nil nil)
   (verb "android_read_screen" :get "/screen"
         "The accessibility tree of the current screen: node ids, text, classes and clickability. Read this before tapping; it is cheaper than a screenshot. System UI is excluded unless include_system_ui is set."
         '(("include_bounds" :boolean nil "Include the pixel rectangle of each node.")
           ("include_system_ui" :boolean nil "Include the status bar and the navigation bar."))
         (sender :get "/screen" "bounds" :include-bounds "system_ui" :include-system-ui))
   (verb "android_tap" :post "/tap"
         "Tap one UI element by node_id (preferred, from android_read_screen) or a point by x and y."
         '(("node_id" :string nil "Accessibility node id from android_read_screen.")
           ("x" :integer nil "X in pixels.")
           ("y" :integer nil "Y in pixels."))
         (lambda (args)
           (let ((node (arg args :node-id)))
             (cond (node (values :post "/tap" (list (cons "nodeId" node))))
                   ((and (integerp (arg args :x)) (integerp (arg args :y)))
                    (values :post "/tap" (list (cons "x" (arg args :x)) (cons "y" (arg args :y)))))
                   (t (values :error "error: give either a node_id, or both x and y"))))))
   (verb "android_tap_text" :post "/tap_text"
         "Tap the first visible element whose text matches: exact when exact is set, a case-insensitive contains otherwise."
         '(("text" :string t "The text to find and tap.")
           ("exact" :boolean nil "Match the text exactly instead of containing it."))
         (sender :post "/tap_text" "text" :text "exact" :exact))
   (verb "android_type" :post "/type"
         "Type text into the focused field. Tap the field first with android_tap or android_tap_text."
         '(("text" :string t "The text to type.")
           ("clear_first" :boolean nil "Clear the field before typing."))
         (sender :post "/type" "text" :text "clearFirst" :clear-first))
   (verb "android_swipe" :post "/swipe"
         "Swipe up, down, left or right: short, medium or long."
         '(("direction" :string t "up, down, left or right.")
           ("distance" :string nil "short, medium (the default) or long."))
         (sender :post "/swipe" "direction" :direction "distance" :distance))
   (verb "android_open_app" :post "/open_app"
         "Launch an app by its package name; android_get_apps lists them."
         '(("package" :string t "The app's package name, e.g. com.ubercab."))
         (sender :post "/open_app" "package" :package))
   (verb "android_press_key" :post "/press_key"
         "Press one key: back, home, recents, power, notifications, quick_settings, lock_screen, take_screenshot or wake."
         '(("key" :string t "The key to press."))
         (sender :post "/press_key" "key" :key))
   (verb "android_screenshot" :get "/screenshot"
         "A base64 PNG of the current screen. Use it to show a screen or to read what the tree cannot; for finding elements call android_read_screen or android_find_nodes."
         nil nil)
   (verb "android_scroll" :post "/scroll"
         "Scroll the screen, or one scrollable element by its node_id."
         '(("direction" :string t "up, down, left or right.")
           ("node_id" :string nil "The scrollable container's node id; the screen when absent."))
         (sender :post "/scroll" "direction" :direction "nodeId" :node-id))
   (verb "android_wait" :post "/wait"
         "Wait for an element to appear after an action that loads or navigates."
         '(("text" :string nil "Wait for an element with this text.")
           ("class_name" :string nil "Wait for an element of this class.")
           ("timeout_ms" :integer nil "The longest wait, in milliseconds (5000 by default)."))
         (sender :post "/wait" "timeoutMs" :timeout-ms "text" :text "className" :class-name))
   (verb "android_get_apps" :get "/apps"
         "List the installed apps, with their package names and labels." nil nil)
   (verb "android_current_app" :get "/current_app"
         "The package and activity of the app in front." nil nil)
   (verb "android_clipboard_read" :get "/clipboard"
         "Read the phone's clipboard as text." nil nil)
   (verb "android_clipboard_write" :post "/clipboard"
         "Put text on the phone's clipboard, for pasting into a field or another app."
         '(("text" :string t "The text to copy."))
         (sender :post "/clipboard" "text" :text))
   (verb "android_notifications" :get "/notifications"
         "Recent notifications, with their package, title, text and time. Needs the notification listener permission; dismissed ones only with include_removed."
         '(("limit" :integer nil "How many to return (50 by default).")
           ("since" :integer nil "Only notifications after this Unix time in milliseconds.")
           ("include_removed" :boolean nil "Include notifications the person already cleared."))
         (sender :get "/notifications" "limit" :limit "since" :since
                 "include_removed" :include-removed))
   (verb "android_long_press" :post "/long_press"
         "Long press a node by node_id or a point by x and y, for a context menu or a text selection."
         '(("node_id" :string nil "Accessibility node id.")
           ("x" :integer nil "X in pixels.")
           ("y" :integer nil "Y in pixels.")
           ("duration" :integer nil "How long the press lasts, in milliseconds (500 by default)."))
         (lambda (args)
           (let ((node (arg args :node-id))
                 (duration (arg args :duration))
                 (x (arg args :x))
                 (y (arg args :y)))
             (cond ((or node (and (integerp x) (integerp y)))
                    (values :post "/long_press"
                            (append (if node
                                        (list (cons "nodeId" node))
                                        (list (cons "x" x) (cons "y" y)))
                                    (if duration (list (cons "duration" duration)) nil))))
                   (t (values :error "error: give either a node_id, or both x and y"))))))
   (verb "android_drag" :post "/drag"
         "Drag from one point to another: rearrange apps, pull the shade down, move a map pin."
         '(("start_x" :integer t "Start X in pixels.")
           ("start_y" :integer t "Start Y in pixels.")
           ("end_x" :integer t "End X in pixels.")
           ("end_y" :integer t "End Y in pixels.")
           ("duration" :integer nil "How long the drag lasts, in milliseconds (500 by default)."))
         (sender :post "/drag" "startX" :start-x "startY" :start-y
                 "endX" :end-x "endY" :end-y "duration" :duration))
   (verb "android_describe_node" :post "/describe_node"
         "Everything one UI node carries: bounds, checked state, hint, the view id, child count."
         '(("node_id" :string t "Accessibility node id from android_read_screen."))
         (sender :post "/describe_node" "nodeId" :node-id))
   (verb "android_screen_hash" :get "/screen_hash"
         "A light fingerprint of the screen, for polling how it changed." nil nil)
   (verb "android_macro" :post "/macro"
         "Run a list of android tool calls in order, stopping at the first failure: each step is an object with tool and args."
         '(("steps" :array t "The ordered steps: [{\"tool\": \"android_tap_text\", \"args\": {...}}, ...].")
           ("name" :string nil "A name for this macro, for the answer's first line."))
         nil)
   (verb "android_location" :get "/location"
         "The phone's GPS fix: latitude, longitude, accuracy and provider." nil nil)
   (verb "android_send_sms" :post "/send_sms"
         "Send an SMS without opening the messaging app. Needs the SMS permission; say who you are texting first."
         '(("to" :string t "The phone number to text.")
           ("body" :string t "The message."))
         (sender :post "/send_sms" "to" :to "body" :body))
   (verb "android_call" :post "/call"
         "Start a phone call; the phone's call UI opens on the device."
         '(("number" :string t "The number to call."))
         (sender :post "/call" "number" :number))
   (verb "android_speak" :post "/speak"
         "Speak text aloud through the phone's speaker; flush interrupts what it is saying."
         '(("text" :string t "What to say.")
           ("flush" :boolean nil "Interrupt the current speech."))
         (lambda (args)
           (values :post "/speak"
                   (list (cons "text" (arg args :text))
                         (cons "queue" (if (arg args :flush) 0 1))))))
   (verb "android_speak_stop" :post "/stop_speaking"
         "Stop the phone's text-to-speech playback." nil nil)
   (verb "android_events" :get "/events"
         "Recent accessibility events (clicks, text changes, window changes), when event capture is on."
         '(("limit" :integer nil "How many to return (50 by default).")
           ("since" :integer nil "Only events after this Unix time in milliseconds."))
         (sender :get "/events" "limit" :limit "since" :since))
   (verb "android_event_stream" :post "/events/stream"
         "Turn accessibility event capture on or off; android_events reads what it caught."
         '(("enabled" :boolean nil "Capture events (true) or stop capturing (false)."))
         (lambda (args)
           ;; absent means on, as the original's default did; an explicit false
           ;; is a key that is there holding NIL, which shasht writes as false.
           (values :post "/events/stream"
                   (list (cons "enabled" (if (assoc :enabled args)
                                              (cdr (assoc :enabled args))
                                              t))))))
   (verb "android_screen_record" :post "/screen_record"
         "Record a short video of the screen (5 seconds by default) as an MP4."
         '(("duration_ms" :integer nil "How long to record, in milliseconds."))
         (sender :post "/screen_record" "durationMs" :duration-ms))
   (verb "android_mic_record" :post "/mic_start"
         "Start a 16 kHz mono WAV recording; 0 records until android_mic_stop, at most 1800 seconds."
         '(("duration" :integer nil "Seconds to record; 0 until stopped."))
         (sender :post "/mic_start" "duration" :duration))
   (verb "android_mic_stop" :post "/mic_stop"
         "Stop the recording and finalize its WAV file." nil nil)
   (verb "android_mic_status" :get "/mic_status"
         "What the microphone recorder is doing, and the last finished WAV." nil nil)
   (verb "android_mic_fetch" :get "/mic_file"
         "Copy the latest (or a named) recording to this machine as a MEDIA file."
         '(("remote_path" :string nil "The WAV's file name, as android_mic_status returned it; no paths."))
         nil)
   (verb "android_files_list" :get "/files"
         "List one directory of the phone's shared storage. Allowed roots: Download, Documents, Pictures, Music, Movies, DCIM, root."
         '(("path" :string nil "A relative path such as Download or Download/sub; root for the top level.")
           ("hidden" :boolean nil "Include hidden files."))
         (sender :get "/files" "path" :path "hidden" :hidden))
   (verb "android_files_search" :get "/files_search"
         "Search file names across the shared storage, case-insensitively."
         '(("query" :string t "What the file name contains.")
           ("limit" :integer nil "How many results at most (200 by default)."))
         (sender :get "/files_search" "query" :query "limit" :limit))
   (verb "android_photo" :post "/photo"
         "Take a photo with the back camera, saved to Pictures/Agent/ on the phone; android_file_get fetches it."
         '(("filename" :string nil "A file name for the photo."))
         (sender :post "/photo" "filename" :filename))
   (verb "android_torch" :post "/torch"
         "The flashlight on or off."
         '(("on" :boolean nil "On (the default) or off."))
         (lambda (args)
           (values :post "/torch"
                   (list (cons "on" (if (assoc :on args) (cdr (assoc :on args)) t))))))
   (verb "android_network_status" :get "/network"
         "The network the phone is on: wifi, its SSID and address, Bluetooth, whether the internet answers, metered." nil nil)
   (verb "android_volume" :post "/volume"
         "Read a volume stream (media, ring, alarm, notification), or set it when set_level is given."
         '(("stream" :string nil "media (the default), ring, alarm or notification.")
           ("set_level" :integer nil "0 up to the maximum the read answers; omit to only read."))
         (sender :post "/volume" "stream" :stream "set" :set-level))
   (verb "android_alarm" :post "/alarm"
         "Set an alarm in the phone's clock app."
         '(("hour" :integer t "The hour, 0-23.")
           ("minute" :integer nil "The minute, 0-59.")
           ("label" :string nil "A label for the alarm."))
         (sender :post "/alarm" "hour" :hour "minute" :minute "label" :label))
   (verb "android_timer" :post "/timer"
         "Set a countdown timer in the phone's clock app."
         '(("seconds" :integer t "How long the timer runs.")
           ("label" :string nil "A label for the timer."))
         (sender :post "/timer" "seconds" :seconds "label" :label))
   (verb "android_notify_reply" :post "/notify_reply"
         "Reply inside a notification through its direct-reply action (WhatsApp, Telegram, SMS): by key, or to the newest replyable notification of a package."
         '(("text" :string t "The reply to send.")
           ("key" :string nil "The exact notification key from android_notifications.")
           ("package_name" :string nil "An app package such as com.whatsapp."))
         (sender :post "/notify_reply" "text" :text "key" :key "package_name" :package-name))
   (verb "android_file_put" :post "/files_push"
         "Push a file to the phone's shared storage: text, or base64 content (max 48 MB)."
         '(("remote_path" :string t "A relative destination such as Download/notes.txt.")
           ("local_b64_or_text" :string t "The content: plain text, or base64.")
           ("overwrite" :boolean nil "Replace a file that is there."))
         nil)
   (verb "android_file_delete" :post "/files_delete"
         "Delete one file or a list of them on the phone (max 200); folders are refused. Destructive: name the files to the person first."
         '(("remote_path" :any t "One relative path, or a list of relative paths."))
         nil)
   (verb "android_files_permission" :get "/files_permission"
         "Whether the companion app has Android's all-files access; open_settings opens the grant screen on the phone."
         '(("open_settings" :boolean nil "Open the permission screen on the phone."))
         nil)
   (verb "android_files_count" :get "/files_count"
         "Count files with one extension across the shared storage."
         '(("ext" :string nil "The extension without its dot, e.g. pdf."))
         (sender :get "/files_count" "ext" :ext))
   (verb "android_file_get" :get "/file"
         "Copy one file from the phone to this machine, as a MEDIA file in the answer. Read-only, up to 512 MB."
         '(("remote_path" :string t "A relative path from android_files_list or android_files_search."))
         nil)
   (verb "android_apk_install" :post "/apk_install"
         "The companion app's own update: the phone checks the relay it already trusts, verifies the SHA-256 and starts the installer, which the person confirms on the device."
         nil nil)
   (verb "android_health" :post "/health_data"
         "Health Connect totals for the last days (read-only): steps, distance, calories, heart rate, sleep, blood oxygen, weight."
         '(("days" :integer nil "The window in days, 1 to 30 (1 by default)."))
         (sender :post "/health_data" "days" :days))
   (verb "android_read_widgets" :get "/widgets"
         "Read the home screen's widgets (weather, calendar, tasks) without opening their apps." nil nil)
   (verb "android_find_nodes" :post "/find_nodes"
         "The UI nodes matching a text, a class or clickability, without the whole tree."
         '(("text" :string nil "Text the node contains, case-insensitively.")
           ("class_name" :string nil "An Android class name, e.g. android.widget.Button.")
           ("clickable" :boolean nil "Only clickable nodes (true) or only ones that are not (false).")
           ("limit" :integer nil "How many at most (20 by default)."))
         (sender :post "/find_nodes" "text" :text "className" :class-name
                 "clickable" :clickable "limit" :limit))
   (verb "android_diff_screen" :post "/diff_screen"
         "Whether the screen changed since a hash android_screen_hash answered, and the new hash."
         '(("previous_hash" :string t "The hash from android_screen_hash."))
         (sender :post "/diff_screen" "previousHash" :previous-hash))
   (verb "android_pinch" :post "/pinch"
         "Pinch at a point: scale above 1 zooms in, below 1 zooms out."
         '(("x" :integer t "Centre X in pixels.")
           ("y" :integer t "Centre Y in pixels.")
           ("scale" :number nil "The zoom factor (1.5 by default).")
           ("duration" :integer nil "How the gesture lasts, in milliseconds (300 by default)."))
         (sender :post "/pinch" "x" :x "y" :y "scale" :scale "duration" :duration))
   (verb "android_media" :post "/media"
         "Control playback system-wide: play, pause, toggle, next or previous."
         '(("action" :string t "play, pause, toggle, next or previous."))
         (sender :post "/media" "action" :action))
   (verb "android_search_contacts" :get "/contacts"
         "Search the phone's contacts by name, for a number to call or text."
         '(("query" :string t "The name to search for.")
           ("limit" :integer nil "How many at most (20 by default)."))
         (sender :get "/contacts" "query" :query "limit" :limit))
   (verb "android_send_intent" :post "/intent"
         "Start an activity with an Android intent: a deep link, a settings screen, an app's own API."
         '(("action" :string t "The intent action, e.g. android.settings.WIFI_SETTINGS.")
           ("data_uri" :string nil "A data URI, e.g. tel:+1234567890.")
           ("extras" :object nil "String extras, as key-value pairs.")
           ("package" :string nil "The target package."))
         (sender :post "/intent" "action" :action "dataUri" :data-uri
                 "extras" :extras "packageOverride" :package))
   (verb "android_broadcast" :post "/broadcast"
         "Send a broadcast intent, which a system or an app's receiver answers."
         '(("action" :string t "The broadcast action, e.g. android.intent.action.AIRPLANE_MODE.")
           ("extras" :object nil "String extras, as key-value pairs."))
         (sender :post "/broadcast" "action" :action "extras" :extras))
   (verb "android_setup" :get "/ping"
         "Whether the relay answers and a phone is connected, and what to do next to connect one. Takes no arguments."
         nil nil))
  "Every verb this cell registers: the original plugin's fifty-eight tools.")

;;; --- files that come to this machine --------------------------------------------

(defvar *download-counter* 0
  "Names one downloaded file apart from the next in the temp directory.")

(defun temp-path (suffix)
  "A path under the temp directory that nothing has yet, ending in SUFFIX."
  (loop for name = (format nil "android-bridge-~d-~d~a"
                            (get-universal-time) (incf *download-counter*) suffix)
        for path = (merge-pathnames name (uiop:temporary-directory))
        unless (probe-file path) return path))

(defun download (path query suffix)
  "GET PATH with QUERY, and save the octets as a temp file ending in SUFFIX:
a MEDIA line naming it, or the error words."
  (multiple-value-bind (body status) (call-relay :get (query-string path query) :binary t)
    (cond ((null status) body)
          ((>= status 400)
           (format nil "error: HTTP ~d: ~a" status
                   (clip (if (stringp body) body "") 600)))
          ((not (vectorp body)) "error: the relay sent no file")
          (t (let ((file (temp-path suffix)))
               (with-open-file (out file :direction :output
                                          :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
                 (write-sequence body out))
               (format nil "File fetched (~d bytes)~%MEDIA:~a" (length body)
                       (uiop:native-namestring file)))))))

(defun relative-path-p (path)
  "PATH is a relative phone path: no leading slash, no '..' segment."
  (and (plusp (length path))
       (not (char= #\/ (char path 0)))
       (not (member ".." (uiop:split-string path :separator "/") :test #'string=))))

(defun file-get (args)
  "android_file_get: the phone's file, copied here, as a MEDIA answer."
  (let ((path (nonblank (arg args :remote-path))))
    (cond ((null path) "error: remote_path required (a relative path from files list or search)")
          ((not (relative-path-p path)) "error: path must be relative, with no '..' segment")
          (t (let ((suffix (let ((type (pathname-type (pathname path))))
                              (if (and type (<= (length type) 10)) (concatenate 'string "." type) ".bin"))))
               (download "/file" (list (cons "path" path)) suffix))))))

(defun mic-fetch (args)
  "android_mic_fetch: the last (or a named) WAV, copied here."
  (let ((name (nonblank (arg args :remote-path))))
    (cond ((and name (or (not (string= name (file-namestring (pathname name))))
                         (not (uiop:string-suffix-p (string-downcase name) ".wav"))))
           "error: remote_path must be a WAV file name, not a path")
          (t (download "/mic_file" (if name (list (cons "name" name)) nil) ".wav")))))

(defparameter *base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-encode (octets)
  "OCTETS as base64 text."
  (with-output-to-string (out)
    (loop for start from 0 below (length octets) by 3
          for a = (aref octets start)
          for b = (and (< (1+ start) (length octets)) (aref octets (1+ start)))
          for c = (and (< (+ start 2) (length octets)) (aref octets (+ start 2)))
          for number = (+ (ash a 16) (ash (or b 0) 8) (or c 0))
          do (write-char (char *base64-alphabet* (ldb (byte 6 18) number)) out)
             (write-char (char *base64-alphabet* (ldb (byte 6 12) number)) out)
             (write-char (if b (char *base64-alphabet* (ldb (byte 6 6) number)) #\=) out)
             (write-char (if c (char *base64-alphabet* (ldb (byte 6 0) number)) #\=) out))))

(defun base64-text-p (text)
  "TEXT looks like base64 already: the alphabet, padded to a multiple of four."
  (and (plusp (length text))
       (zerop (mod (length text) 4))
       (let ((body (string-right-trim "=" text)))
         (and (plusp (length body))
              (every (lambda (char) (find char *base64-alphabet*)) body)
              (<= (- (length text) (length body)) 2)))))

(defun file-put (args)
  "android_file_put: text or base64, re-encoded as the base64 the relay takes."
  (let ((path (nonblank (arg args :remote-path)))
        (content (arg args :local-b64-or-text)))
    (cond ((null path) "error: remote_path required")
          ((not (stringp content)) "error: content required, as text or base64")
          (t (let* ((encoded (if (base64-text-p content)
                                 content
                                 (base64-encode (sb-ext:string-to-octets content :external-format :utf-8))))
                    (payload (list (cons "path" path) (cons "data" encoded)
                                   (cons "overwrite" (if (arg args :overwrite) t nil)))))
               (if (> (length encoded) (* 64 1024 1024))
                   "error: payload too large (48 MB after decoding is the limit)"
                   (multiple-value-bind (body status) (call-relay :post "/files_push" :content (payload-json payload))
                     (answer body status))))))))

(defun file-delete (args)
  "android_file_delete: one relative path, or a list of them."
  (let ((value (arg args :remote-path)))
    (cond ((and (stringp value) (plusp (length value)))
           (multiple-value-bind (body status) (call-relay :post "/files_delete" :content (payload-json (list (cons "path" value))))
             (answer body status)))
          ((and (vectorp value) (plusp (length value)))
           (multiple-value-bind (body status) (call-relay :post "/files_delete" :content (payload-json (list (cons "paths" value))))
             (answer body status)))
          (t "error: give a relative path, or a list of them"))))

(defun files-permission (args)
  "android_files_permission: a read, or the POST that opens the grant screen."
  (multiple-value-bind (body status)
      (if (arg args :open-settings)
          (call-relay :post "/files_permission" :content "{}")
          (call-relay :get "/files_permission"))
    (answer body status)))

;;; --- a macro, a setup report ----------------------------------------------------

(defun run-macro (args)
  "android_macro: each step's tool called in turn, stopping at the first error."
  (let ((steps (arg args :steps))
        (name (or (arg args :name) "unnamed"))
        (results '())
        (stopped nil))
    (cond ((or (not (vectorp steps)) (zerop (length steps)))
           "error: steps is empty: give a list of {\"tool\": ..., \"args\": {...}}")
          (t (loop for step across steps
                   for index from 1
                   for tool = (and (hash-table-p step) (gethash "tool" step))
                   for tool-args = (and (hash-table-p step) (gethash "args" step))
                   until stopped
                   do (let ((text (if (stringp tool)
                                      (perform tool (arguments-alist (or tool-args (make-hash-table :test #'equal))))
                                      "error: a step needs a tool name")))
                        (push (format nil "~d. ~a: ~a" index (or tool "?") (clip text 500))
                              results)
                        (when (uiop:string-prefix-p "error:" text) (setf stopped t))))
             (format nil "macro ~a~%~{~a~^~%~}" name (nreverse results))))))

(defun setup-report ()
  "android_setup: whether the relay answers, and what to do about the phone."
  (multiple-value-bind (body status) (call-relay :get "/ping")
    (cond ((null status) body)
          ((>= status 400)
           (format nil "error: the relay at ~a answered HTTP ~d: ~a" (base-url) status (clip body 600)))
          (t (format nil "relay ~a answers: ~a~%~a" (base-url) (clip body 1200)
                     "In the Hermes Bridge app on the phone, enter the pairing code saved as the android-bridge section's token, and tap Connect.")))))

;;; --- one call, dispatched by name -----------------------------------------------

(defun call-and-answer (method path payload)
  (if (eq method :get)
      (multiple-value-bind (body status) (call-relay :get (query-string path payload))
        (answer body status))
      (multiple-value-bind (body status) (call-relay :post path :content (payload-json payload))
        (answer body status))))

(defun perform (name args)
  "NAME called with ARGS, an alist of the model's arguments; the answer's text."
  (handler-case
      (let ((verb (find-verb name)))
        (cond ((null verb) (format nil "error: there is no tool called ~a here" name))
              ((string= name "android_file_get") (file-get args))
              ((string= name "android_mic_fetch") (mic-fetch args))
              ((string= name "android_macro") (run-macro args))
              ((string= name "android_setup") (setup-report))
              ((string= name "android_file_put") (file-put args))
              ((string= name "android_file_delete") (file-delete args))
              ((string= name "android_files_permission") (files-permission args))
              (t (multiple-value-bind (method path payload)
                     (let ((send (getf verb :send)))
                       (if send
                           (funcall send args)
                           (values (getf verb :method) (getf verb :route) nil)))
                   (if (eq method :error)
                       path
                       (call-and-answer method path payload))))))
    (error (condition)
      (format nil "error: ~a" (nlk:one-line (princ-to-string condition))))))

(defun handle-tool (name arguments)
  "NAME called with the model's decoded ARGUMENTS, as the text the model reads."
  (perform name (arguments-alist arguments)))

;;; --- the declaration ------------------------------------------------------------

(nlk:define-peripheral android-bridge :not-running t)

(nle:define-cell android-bridge
  (:section ("android-bridge")
    (:guide "url is the relay the companion app dials, or the phone's own address on your LAN; token is the pairing code the app shows; nothing else is required; next step: call android_ping")
    ("url" :string :required t
     :doc "the relay's base URL, e.g. http://127.0.0.1:8766, or the phone's own address on your LAN")
    ("token" :secret
     :doc "the pairing code the Hermes Bridge app shows; sent as a Bearer token and to no other host")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one call to the phone's ceiling in seconds")
    ("max_output_chars" :integer :default 20000 :min 200
     :doc "how much of one answer a tool result carries"))
  (:settings #'read-settings)
  (:help :android-bridge
         "android-bridge: android_ping, android_read_screen, android_tap and fifty-five more tools drive a paired Android phone over its relay"
         *manual*)
  (:tool "android_alarm" (verb-schema "android_alarm") (verb-handler "android_alarm"))
  (:tool "android_apk_install" (verb-schema "android_apk_install") (verb-handler "android_apk_install"))
  (:tool "android_broadcast" (verb-schema "android_broadcast") (verb-handler "android_broadcast"))
  (:tool "android_call" (verb-schema "android_call") (verb-handler "android_call"))
  (:tool "android_clipboard_read" (verb-schema "android_clipboard_read") (verb-handler "android_clipboard_read"))
  (:tool "android_clipboard_write" (verb-schema "android_clipboard_write") (verb-handler "android_clipboard_write"))
  (:tool "android_current_app" (verb-schema "android_current_app") (verb-handler "android_current_app"))
  (:tool "android_describe_node" (verb-schema "android_describe_node") (verb-handler "android_describe_node"))
  (:tool "android_diff_screen" (verb-schema "android_diff_screen") (verb-handler "android_diff_screen"))
  (:tool "android_drag" (verb-schema "android_drag") (verb-handler "android_drag"))
  (:tool "android_event_stream" (verb-schema "android_event_stream") (verb-handler "android_event_stream"))
  (:tool "android_events" (verb-schema "android_events") (verb-handler "android_events"))
  (:tool "android_file_delete" (verb-schema "android_file_delete") (verb-handler "android_file_delete"))
  (:tool "android_file_get" (verb-schema "android_file_get") (verb-handler "android_file_get"))
  (:tool "android_file_put" (verb-schema "android_file_put") (verb-handler "android_file_put"))
  (:tool "android_files_count" (verb-schema "android_files_count") (verb-handler "android_files_count"))
  (:tool "android_files_list" (verb-schema "android_files_list") (verb-handler "android_files_list"))
  (:tool "android_files_permission" (verb-schema "android_files_permission") (verb-handler "android_files_permission"))
  (:tool "android_files_search" (verb-schema "android_files_search") (verb-handler "android_files_search"))
  (:tool "android_find_nodes" (verb-schema "android_find_nodes") (verb-handler "android_find_nodes"))
  (:tool "android_get_apps" (verb-schema "android_get_apps") (verb-handler "android_get_apps"))
  (:tool "android_health" (verb-schema "android_health") (verb-handler "android_health"))
  (:tool "android_location" (verb-schema "android_location") (verb-handler "android_location"))
  (:tool "android_long_press" (verb-schema "android_long_press") (verb-handler "android_long_press"))
  (:tool "android_macro" (verb-schema "android_macro") (verb-handler "android_macro"))
  (:tool "android_media" (verb-schema "android_media") (verb-handler "android_media"))
  (:tool "android_mic_fetch" (verb-schema "android_mic_fetch") (verb-handler "android_mic_fetch"))
  (:tool "android_mic_record" (verb-schema "android_mic_record") (verb-handler "android_mic_record"))
  (:tool "android_mic_status" (verb-schema "android_mic_status") (verb-handler "android_mic_status"))
  (:tool "android_mic_stop" (verb-schema "android_mic_stop") (verb-handler "android_mic_stop"))
  (:tool "android_network_status" (verb-schema "android_network_status") (verb-handler "android_network_status"))
  (:tool "android_notifications" (verb-schema "android_notifications") (verb-handler "android_notifications"))
  (:tool "android_notify_reply" (verb-schema "android_notify_reply") (verb-handler "android_notify_reply"))
  (:tool "android_open_app" (verb-schema "android_open_app") (verb-handler "android_open_app"))
  (:tool "android_photo" (verb-schema "android_photo") (verb-handler "android_photo"))
  (:tool "android_pinch" (verb-schema "android_pinch") (verb-handler "android_pinch"))
  (:tool "android_ping" (verb-schema "android_ping") (verb-handler "android_ping"))
  (:tool "android_press_key" (verb-schema "android_press_key") (verb-handler "android_press_key"))
  (:tool "android_read_screen" (verb-schema "android_read_screen") (verb-handler "android_read_screen"))
  (:tool "android_read_widgets" (verb-schema "android_read_widgets") (verb-handler "android_read_widgets"))
  (:tool "android_screen_hash" (verb-schema "android_screen_hash") (verb-handler "android_screen_hash"))
  (:tool "android_screen_record" (verb-schema "android_screen_record") (verb-handler "android_screen_record"))
  (:tool "android_screenshot" (verb-schema "android_screenshot") (verb-handler "android_screenshot"))
  (:tool "android_scroll" (verb-schema "android_scroll") (verb-handler "android_scroll"))
  (:tool "android_search_contacts" (verb-schema "android_search_contacts") (verb-handler "android_search_contacts"))
  (:tool "android_send_intent" (verb-schema "android_send_intent") (verb-handler "android_send_intent"))
  (:tool "android_send_sms" (verb-schema "android_send_sms") (verb-handler "android_send_sms"))
  (:tool "android_setup" (verb-schema "android_setup") (verb-handler "android_setup"))
  (:tool "android_speak" (verb-schema "android_speak") (verb-handler "android_speak"))
  (:tool "android_speak_stop" (verb-schema "android_speak_stop") (verb-handler "android_speak_stop"))
  (:tool "android_swipe" (verb-schema "android_swipe") (verb-handler "android_swipe"))
  (:tool "android_tap" (verb-schema "android_tap") (verb-handler "android_tap"))
  (:tool "android_tap_text" (verb-schema "android_tap_text") (verb-handler "android_tap_text"))
  (:tool "android_timer" (verb-schema "android_timer") (verb-handler "android_timer"))
  (:tool "android_torch" (verb-schema "android_torch") (verb-handler "android_torch"))
  (:tool "android_type" (verb-schema "android_type") (verb-handler "android_type"))
  (:tool "android_volume" (verb-schema "android_volume") (verb-handler "android_volume"))
  (:tool "android_wait" (verb-schema "android_wait") (verb-handler "android_wait")))
