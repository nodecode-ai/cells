;;;; cell.lisp --- the five tools, the /meme command and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model makes a meme with the five tools the original plugin's MCP
;;;; server publishes, forwarded to it by name; the operator does it with
;;;; /meme. The section of the config is `meme-studio':
;;;;   "meme-studio": {"enabled": true, "endpoint": "https://memes.mrhyde.dev/mcp",
;;;;                   "language": "en", "output_dir": "~/.nodecode/memes",
;;;;                   "timeout_seconds": 90, "max_image_bytes": 8388608}
;;;; No key: Meme Studio asks for none.

(in-package #:nodecode-meme-studio)

;;; --- the primer ----------------------------------------------------------------

(defparameter +primer+
  "Meme Studio is reachable through the nodecode-meme-studio cell: five tools, forwarded to
Meme Studio's own MCP server, and one slash command, /meme.
  suggest_templates(situation)   The first step: the templates that fit a joke or a situation,
      in any language, five by default, each with what it is used for (used_for), whether it is
      animated, and its slots: what goes in each (hint) and about how many characters fit
      (fits_chars).
  search_templates(query)        Templates by name, keyword or what a meme is used for.
  get_template(template)         One template in detail, its slots included; image=true also
      answers the picture with the slots numbered.
  popular_templates(kind)        trending this week, most_used overall, or new.
  create_meme(template, captions)  The meme: one caption per slot, each under the slot's
      fits_chars, in the person's language. Answers the finished picture (a GIF for an
      animated template) as a file this cell saves under output_dir, an edit_url that opens it
      in the studio, the server's warnings, and deliver: picture (give the file), edit_url
      (give the link exactly as it came) or fix_first (fix what it says and call again).
  /meme my code works locally but not in production    the templates that fit, here.
Doctrine: pick the template whose used_for and slot hints match the joke; a caption does what
its slot's hint says, in a few words. Read deliver before answering: with edit_url, paste the
link on a line of its own, never retyped. A failed call answers ERROR with what the server
said, which is the repair."
  "What (help :meme-studio) answers while the cell runs.")

;;; --- the settings --------------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the endpoint checked and output_dir made a
directory: read once, here."
  (declare (ignore table))
  (let* ((endpoint (or (getf values :endpoint) ""))
         (plist (copy-list values)))
    (unless (and (plusp (length endpoint)) (eql 0 (search "https://" endpoint))
                 (null (find-if (lambda (character)
                                  (member character '(#\Space #\Tab #\Newline #\Return)))
                                endpoint)))
      (nlk:config-error "endpoint is ~s; it must be an https address" endpoint))
    (setf (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (getf values :output-dir) "~/.nodecode/memes"))))
    plist))

;;; --- the arguments the model is shown ------------------------------------------

(defun schema-member (type &rest members)
  "One member of a tool's arguments: TYPE and its schema MEMBERS, as
(\"minimum\" 1 \"description\" \"...\") pairs."
  (let ((object (nlk:make-json-object "type" type)))
    (loop for (key value) on members by #'cddr
          do (setf (gethash key object) value))
    object))

(defun schema-object (properties &optional required)
  "A JSON-Schema object: PROPERTIES an alist of (name . member), REQUIRED the
names every call carries."
  (let ((table (nlk:make-json-object)))
    (dolist (property properties)
      (setf (gethash (car property) table) (cdr property)))
    (let ((object (nlk:make-json-object "type" "object" "properties" table)))
      (when required
        (setf (gethash "required" object) (coerce required 'vector)))
      object)))

(defparameter +caption+
  (schema-object
   (list (cons "text" (schema-member "string" "minLength" 1 "maxLength" 120
                                      "description" "the caption, one idea in a few words, under the slot's fits_chars; the server writes it in capitals and a \\n in it forces a line break"))
         (cons "slot" (nlk:make-json-object
                        "description" "slot number or name from get_template or suggest_templates"
                        "anyOf" (vector (schema-member "integer" "minimum" 1 "maximum" 12)
                                         (schema-member "string" "maxLength" 40))))
         (cons "position" (schema-member "string" "enum" (vector "top" "bottom" "middle")
                                          "description" "classic placement on a single photo; ignored with slot, x and y"))
         (cons "x" (schema-member "number" "minimum" 0 "maximum" 1
                                   "description" "left edge of the caption box, as a fraction of the width"))
         (cons "y" (schema-member "number" "minimum" 0 "maximum" 1
                                   "description" "top edge of the caption box, as a fraction of the height"))
         (cons "width" (schema-member "number" "minimum" 0.08 "maximum" 1
                                       "description" "box width, as a fraction of the width"))
         (cons "height" (schema-member "number" "minimum" 0.03 "maximum" 1
                                        "description" "box height, as a fraction of the height"))
         (cons "max_size" (schema-member "number" "minimum" 0.4 "maximum" 2
                                          "description" "largest text size, 1 the usual meme size"))
         (cons "color" (schema-member "string" "description" "auto, white, yellow, black or #rrggbb"))
         (cons "style" (schema-member "string" "enum" (vector "auto" "outline" "plain" "box")
                                       "description" "outline, plain or text on a solid box; auto by default"))
         (cons "align" (schema-member "string" "enum" (vector "center" "left" "right")))
         (cons "angle" (schema-member "number" "minimum" -45 "maximum" 45 "description" "degrees, clockwise"))
         (cons "font" (schema-member "string" "enum" (vector "impact" "arial"))))
   '("text")))

(defparameter +emojis+
  (schema-member "array" "maxItems" 8
                  "items" (schema-member "object" "description" "an emoji, its center_x, center_y (fractions), an optional width and angle")
                  "description" "emojis to place, each by its centre; Meme Studio's own set"))

(defparameter +stickers+
  (schema-member "array" "maxItems" 4
                  "items" (schema-member "object" "description" "a sticker (pixel_glasses or laser_eye), its center_x, center_y, width, angle, flip")
                  "description" "stickers worn over a face"))

(defparameter +shapes+
  (schema-member "array" "maxItems" 6
                  "items" (schema-member "object" "description" "a shape (bubble, rect, rounded, ellipse, arrow, star, heart, diamond, triangle), its x, y, width, height (fractions), color, opacity, angle, and an optional text")
                  "description" "shapes, some carrying text"))
(defparameter +suggest-tool+
  (list :description "The first step for a meme: give the joke or the situation, in any language, and get the templates that fit it best (five by default), each with what it is used for, whether it is animated, and its slots: what goes in each one and about how many characters fit. Pick the one whose used_for and hints match the joke, write one caption per slot that follows its hint, then call create_meme with those slot numbers."
        :parameters
        (schema-object
         (list (cons "situation" (schema-member "string" "minLength" 1 "maxLength" 300
                                                  "description" "the joke or situation the meme is about, in any language"))
               (cons "limit" (schema-member "integer" "minimum" 1 "maximum" 8
                                             "description" "how many templates to suggest (5 by default)"))
               (cons "animated" (schema-member "boolean"
                                                "description" "true: only animated templates; false: only still ones")))
         '("situation"))))

(defparameter +search-tool+
  (list :description "Search Meme Studio's catalog of templates by name, keyword or what a meme is used for (the intent), in any language. Best matches first, each with what it is used for, whether it is animated, and its slots."
        :parameters
        (schema-object
         (list (cons "query" (schema-member "string" "minLength" 1 "maxLength" 200
                                             "description" "the joke, the situation or the template name"))
               (cons "limit" (schema-member "integer" "minimum" 1 "maximum" 20))
               (cons "animated" (schema-member "boolean"
                                                "description" "true: only animated templates; false: only still ones")))
         '("query"))))

(defparameter +get-tool+
  (list :description "One Meme Studio template in detail: names, what it is used for, tags, size, whether it is animated and its slots (the boxes captions go in, with what goes in each and about how many characters fit). With image true it also answers the picture with the slots outlined and numbered. Give image (base64) or image_url instead of template to see the slots found in the person's own picture."
        :parameters
        (schema-object
         (list (cons "template" (schema-member "string"
                                                 "description" "template id from suggest_templates or search_templates"))
               (cons "image" (nlk:make-json-object
                               "description" "true: also answer the picture with the numbered slots; or the person's own picture in base64"
                               "anyOf" (vector (schema-member "boolean")
                                                (schema-member "string" "maxLength" 8388608))))
               (cons "image_url" (schema-member "string" "maxLength" 2048
                                                  "description" "the person's own picture instead of a template: an https address the server downloads"))
               (cons "show_slots" (schema-member "boolean"
                                                   "description" "the same as image: true"))))))

(defparameter +popular-tool+
  (list :description "The templates people choose most in Meme Studio (trending this week, or most used overall) or, with kind new, the latest added to the catalog."
        :parameters
        (schema-object
         (list (cons "kind" (schema-member "string" "enum" (vector "trending" "most_used" "new")
                                             "description" "which list: trending, most_used or new"))
               (cons "limit" (schema-member "integer" "minimum" 1 "maximum" 30))
               (cons "animated" (schema-member "boolean"
                                                "description" "true: only animated templates; false: only still ones"))))))

(defparameter +create-tool+
  (list :description "Make the meme: one template id from the catalog, or the person's own picture as image (base64 or a data: URL) or image_url, and one caption per slot. The server fits and colours every caption; a returned caption text is sent back as it came. Answers the finished picture (a GIF for an animated template) as a file this cell saves, an edit_url that opens the meme in the studio, what each caption got, warnings, and deliver: picture (give the file), edit_url (give the link exactly as returned) or fix_first (fix what the warnings say and call again)."
        :parameters
        (schema-object
         (list (cons "template" (schema-member "string" "description" "template id; or give image or image_url instead"))
               (cons "image" (schema-member "string" "maxLength" 8388608
                                             "description" "the person's own picture in base64 (a data: URL works too), up to 5 MB; JPEG, PNG, WebP or GIF"))
               (cons "image_url" (schema-member "string" "maxLength" 2048
                                                  "description" "the person's own picture: an https address the server downloads"))
               (cons "language" (schema-member "string" "enum" (vector "es" "en")
                                                "description" "the person's language: es opens the Spanish studio, en the English one"))
               (cons "captions" (schema-member "array" "minItems" 1 "maxItems" 12
                                                "items" +caption+
                                                "description" "one caption per slot, each with its text and the slot it goes in"))
               (cons "author" (schema-member "string" "maxLength" 25
                                              "description" "the person's handle, with or without @, only if they gave it"))
               (cons "emojis" +emojis+)
               (cons "stickers" +stickers+)
               (cons "shapes" +shapes+)
               (cons "filter" (schema-member "string"
                                              "enum" (vector "grayscale" "sepia" "invert" "contrast" "brightness"
                                                              "saturation" "pixelate" "blur" "deep_fried" "vignette")))
               (cons "filter_strength" (schema-member "number" "minimum" 0 "maximum" 1))
               (cons "picture" (schema-member "string" "enum" (vector "preview" "final")
                                               "description" "preview to check (800 px), final the finished meme (1200 px, or a GIF for an animated template)"))
               (cons "preview" (schema-member "boolean" "description" "false: answer no picture")))
         '("captions"))))

;;; --- the tools -----------------------------------------------------------------

(defun ask (tool arguments)
  "TOOL's answer as text: ERROR: with what went wrong, never a signalled
condition, so one failed call is a repair the model reads, not a failed turn."
  (handler-case (answer-text (call tool arguments))
    (error (condition) (format nil "ERROR: ~a" condition))))

(defun suggest-templates (arguments)
  "The `suggest_templates' tool: the templates that fit the situation."
  (ask "suggest_templates" arguments))

(defun search-templates (arguments)
  "The `search_templates' tool: templates by name, keyword or intent."
  (ask "search_templates" arguments))

(defun get-template (arguments)
  "The `get_template' tool: one template, its slots, and its picture."
  (ask "get_template" arguments))

(defun popular-templates (arguments)
  "The `popular_templates' tool: trending, most used or new."
  (ask "popular_templates" arguments))

(defun create-meme (arguments)
  "The `create_meme' tool: the meme itself. The person's language is the
section's unless the call names one."
  (let ((arguments (or arguments (nlk:make-json-object))))
    (unless (nlk:json-value arguments :string "language")
      (setf (gethash "language" arguments) (setting :language)))
    (ask "create_meme" arguments)))

;;; --- the command ---------------------------------------------------------------

(defun run-meme (args session-id)
  "The /meme command: the templates that fit the joke, in the conversation;
bare, a row to type the joke in."
  (declare (ignore session-id))
  (let ((situation (nlk:trimmed (or args ""))))
    (if (zerop (length situation))
        (values "the meme templates that fit a joke"
                (nle:list-dialog
                 :title "Meme Studio"
                 :summary "the templates that fit the joke, and what goes in each slot"
                 :rows (list (nle:ask-row "Find a template" "Joke" "/meme"
                                           :placeholder "my code works locally but not in production"
                                           :verb "suggest"))))
        (ask "suggest_templates" (nlk:make-json-object "situation" situation "limit" 5)))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell meme-studio
  (:section ("meme-studio")
    (:guide "no key is needed; endpoint is Meme Studio's MCP server, language is the studio's language, and output_dir is where the memes are written")
    ("endpoint" :string :default "https://memes.mrhyde.dev/mcp"
                :doc "the Meme Studio MCP endpoint every call goes to")
    ("language" :choice :options '("en" "es") :default "en"
                :doc "the studio's language, used when a meme names none")
    ("output_dir" :path :default "~/.nodecode/memes"
                  :doc "where the pictures a meme makes are written")
    ("timeout_seconds" :integer :default 90 :min 1
                        :doc "how long one call may wait for Meme Studio")
    ("max_image_bytes" :integer :default 8388608 :min 1024
                        :doc "the largest picture one answer may carry"))
  (:settings 'read-settings)
  (:tool "suggest_templates" +suggest-tool+ 'suggest-templates)
  (:tool "search_templates" +search-tool+ 'search-templates)
  (:tool "get_template" +get-tool+ 'get-template)
  (:tool "popular_templates" +popular-tool+ 'popular-templates)
  (:tool "create_meme" +create-tool+ 'create-meme)
  (:command "meme" 'run-meme
            :description "Find the meme template that fits a joke"
            :argument-hint "JOKE")
  (:help :meme-studio "meme-studio: a meme template, its slots and the finished picture, from Meme Studio" +primer+))
