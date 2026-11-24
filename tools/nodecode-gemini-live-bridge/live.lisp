;;;; live.lisp --- the Gemini Live API: the model catalog, the dial, one turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's backend (dashboard/plugin_api.py) mints a single-use
;;;; ephemeral token, opens one BidiGenerateContentConstrained socket, sends
;;;; setup and reports serverContent frames to its renderer. This file is that
;;;; backend without the renderer: the same ListModels filter and fixed
;;;; catalog order, the same auth_tokens dial, and a turn read until
;;;; turnComplete.
;;;;
;;;; Two seams, so tests never touch the network: ASK (HTTP, NLK:HTTP) and
;;;; *DIAL* (a websocket, WEBSOCKET-DRIVER).

(in-package #:nodecode-gemini-live-bridge)

(defparameter +list-models-url+ "https://generativelanguage.googleapis.com/v1beta/models"
  "ListModels, as dashboard/plugin_api.py:50 has it.")
(defparameter +auth-tokens-url+ "https://generativelanguage.googleapis.com/v1beta/auth_tokens"
  "The ephemeral-token endpoint, as dashboard/plugin_api.py:51 has it.")
(defparameter +live-ws-url+
  "wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1alpha.GenerativeService.BidiGenerateContentConstrained"
  "The constrained Live socket, as dashboard/plugin_api.py:52 has it.")
(defparameter +default-model+ "models/gemini-2.5-flash-native-audio-latest"
  "The model the original dials when the key lists none and nothing is named.")

(defvar *ask* #'nlk:http "The cell's one HTTP call; a test binds this name.")
(defvar *dial* 'dial-up "The cell's one websocket dial; a test binds this name.")

(defun ask (method url &key headers content (timeout 30))
  "One HTTP request through *ASK*, answering (VALUES BODY STATUS)."
  (funcall *ask* method url
           :headers (append headers '(("accept-encoding" . "identity")))
           :content content :timeout timeout))

(defun field (object name)
  "NAME's member of OBJECT, a hash table a JSON decoder answered, or NIL."
  (and (hash-table-p object) (gethash name object)))

(defun as-list (value)
  "VALUE, a JSON array the decoder answered as a vector, as a list."
  (cond ((listp value) value)
        ((vectorp value) (coerce value 'list))
        (t nil)))

(defun field-text (object name)
  "NAME's member of OBJECT as a string, or NIL."
  (let ((value (field object name)))
    (and value (princ-to-string value))))

(defun api-key ()
  "The Google AI Studio key, from the section's environment variable."
  (let* ((name (setting :api-key-env))
         (value (and name (uiop:getenv name))))
    (if (and value (plusp (length value)))
        value
        (fail "no API key: set ~a to a Google AI Studio key with Live API access"
              (or name "the section's api_key_env")))))

;;; --- the catalog ------------------------------------------------------------------
;;; The original's filter and order: a model the key lists that is Live,
;;; speaks bidiGenerateContent and is not a transcribe/translate model, then
;;; 3.8-live, extended-thinking, native-audio-latest, the newest
;;; native-audio-preview, and the rest by name (dashboard/plugin_api.py:79-104).

(defun live-model-p (model)
  "Does MODEL's entry name a Live model this cell may dial?"
  (let* ((name (string-downcase (or (field-text model "name") "")))
         (methods (field model "supportedGenerationMethods")))
    (and (or (search "live" name) (search "native-audio" name))
         (not (search "transcribe" name))
         (not (search "translate" name))
         (plusp (length (as-list methods)))
         (member "bidiGenerateContent" (as-list methods) :test #'equal))))

(defun model-rank (name)
  "The original's catalog group for NAME."
  (cond ((equal name "models/gemini-3.8-live") 0)
        ((search "extended-thinking" name) 1)
        ((search "native-audio-latest" name) 2)
        ((search "native-audio-preview" name) 3)
        (t 4)))

(defun rank-less-p (a b)
  "Does model A come before model B in the original's fixed order?"
  (let ((ra (model-rank a)) (rb (model-rank b)))
    (cond ((/= ra rb) (< ra rb))
          (t (string< a b)))))

(defun rank-models (names)
  "NAMES in the original's fixed order."
  (sort (copy-list names) #'rank-less-p))

(defun live-models ()
  "Every Live-capable model on the key, in the original's fixed order."
  (multiple-value-bind (body status)
      (ask :get (format nil "~a?key=~a&pageSize=200" +list-models-url+ (api-key)))
    (unless (eql status 200)
      (fail "ListModels answered HTTP ~a" status))
    (let* ((data (ignore-errors (nlk:decode-json body)))
           (models (field data "models"))
           (names (loop for model in (as-list models)
                        when (live-model-p model) collect (field-text model "name"))))
      (if names (rank-models names) (list +default-model+)))))

(defun chosen-model (&optional named)
  "NAMED, else the section's model, else the catalog's first candidate."
  (let ((name (or (and named (plusp (length named)) named)
                  (let ((set (setting :model)))
                    (and set (plusp (length set)) set)))))
    (cond (name (if (uiop:string-prefix-p "models/" name) name (format nil "models/~a" name)))
          (t (first (live-models))))))

;;; --- the socket --------------------------------------------------------------------

(defstruct (frames (:constructor make-frames ()))
  (lock (bt2:make-lock))
  (wait (bt2:make-condition-variable :name "gemini-live-bridge-frames"))
  (items '()))

(defun frames-push (frames item)
  "Append ITEM to FRAMES and wake the reader."
  (bt2:with-lock-held ((frames-lock frames))
    (setf (frames-items frames) (nconc (frames-items frames) (list item)))
    (bt2:condition-notify (frames-wait frames)))
  item)

(defun frames-pop (frames timeout)
  "The oldest item FRAMES holds, waiting up to TIMEOUT seconds; :TIMEOUT when none came."
  (bt2:with-lock-held ((frames-lock frames))
    (let ((deadline (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second))))
      (loop for items = (frames-items frames)
            when items
              do (setf (frames-items frames) (rest items))
                 (return (first items))
            do (let ((left (/ (- deadline (get-internal-real-time))
                              internal-time-units-per-second)))
                 (when (<= left 0) (return :timeout))
                 (bt2:condition-wait (frames-wait frames) (frames-lock frames)
                                      :timeout left))))))

(defstruct (link (:constructor make-link (&key send-fn recv-fn close-fn)))
  "One live socket as three functions: text in, text (or :CLOSED) out, close."
  send-fn recv-fn close-fn)

(defun link-send (link text) (funcall (link-send-fn link) text))
(defun link-recv (link &key (timeout 60)) (funcall (link-recv-fn link) timeout))
(defun link-close (link) (ignore-errors (funcall (link-close-fn link))))

(defun frame-text (message)
  "MESSAGE as a string, whether the driver answered text or octets."
  (if (stringp message)
      message
      (sb-ext:octets-to-string message :external-format :utf-8)))

(defun dial-up (url)
  "A live socket to URL: frames onto a queue, sends straight to the driver."
  (let* ((frames (make-frames))
         (socket (wsd:make-client url)))
    (wsd:on :message socket (lambda (message) (frames-push frames (frame-text message))))
    (wsd:on :close socket (lambda (&key code reason)
                             (declare (ignore code reason))
                             (frames-push frames :closed)))
    (wsd:start-connection socket)
    (make-link :send-fn (lambda (text) (wsd:send socket text))
               :recv-fn (lambda (timeout) (frames-pop frames timeout))
               :close-fn (lambda () (wsd:close-connection socket)))))

(defun iso-utc (universal &optional (minutes 0))
  "UNIVERSAL time plus MINUTES, as the API's expireTime wants it."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ universal (* 60 minutes)) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun mint-token ()
  "A single-use ephemeral token, as dashboard/plugin_api.py:63-70 mints it."
  (multiple-value-bind (body status)
      (ask :post +auth-tokens-url+
           :headers (list (cons "content-type" "application/json")
                          (cons "x-goog-api-key" (api-key)))
           :content (nlk:encode-json-object
                     (nlk:json-object
                      "uses" 1
                      "expireTime" (iso-utc (get-universal-time) 30)
                      "newSessionExpireTime" (iso-utc (get-universal-time) 3))))
    (unless (eql status 200) (fail "auth_tokens answered HTTP ~a" status))
    (or (field-text (ignore-errors (nlk:decode-json body)) "name")
        (fail "auth_tokens answered no token"))))
