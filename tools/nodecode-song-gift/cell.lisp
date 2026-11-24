;;;; cell.lisp --- the primer, the section, /song-gift and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was one hosted MCP server plus one skill. The server is the
;;;; four tools tools.lisp calls over MCP; the skill is +PRIMER+, which the
;;;; model reads with (help :song-gift) and whose one line rides every
;;;; request. Nothing hooks the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "song-gift": {"endpoint": "https://customsonggift.org/api/mcp",
;;;;                 "timeout_seconds": 45, "max_chars": 1200}
;;;; Nothing is required: the default endpoint is the public catalog. A vetoed
;;;; section ("enabled": false) installs nothing.

(in-package #:nodecode-song-gift)

(defparameter +primer+
  "Custom Song Gift is reachable through the nodecode-song-gift cell: four read-only tools over
the gift studio's public catalog, which lists the occasions it writes songs for and the styles it
records in, and gives advice on turning real memories into lyrics. Nothing is bought and nothing
is charged from here, and no account or key is needed.
  list_occasions           the occasions (birthday, anniversary, wedding, Mother's Day, holidays
                            and the rest), each with the memories that make a song for it work.
  list_styles              the musical styles -- warm acoustic, piano ballad, modern pop,
                            heartfelt country, R&B, lofi, lullaby and the others -- with the mood
                            each carries; name an occasion to ask what suits it.
  guide_story_and_lyrics   how to prepare a song: who it is for, which sensory details to
                            gather, what the chorus has to say, how the verses should run.
  get_pricing_options      the current prices and what a finished song comes with -- the keepsake
                            page, the lyrics, the audio download.
How to work: ask list_occasions and list_styles before proposing anything, so the occasion and
the style are the catalog's and not invented. Interview the operator for real memories -- names,
places, small habits, phrases they actually say -- and pass what they gave you to
guide_story_and_lyrics rather than inventing details. Then help them shape those memories into
verses and a chorus. Quote get_pricing_options when they ask what it costs; say that the song
itself is made on the studio's site. The tools list and the advice they give; they do not write
the song or deliver audio. An argument a tool does not name goes in its `extra' object. The
operator can ask for the catalog from the keyboard with /song-gift occasions, /song-gift styles,
/song-gift pricing or /song-gift guide."
  "What (help :song-gift) answers while the cell runs: the plugin's skill and manifest, in one topic.")

(defparameter +usage+
  "usage: /song-gift occasions          the gift occasions and their storytelling tips
       /song-gift styles [occasion]   the musical styles, or the ones one occasion suits
       /song-gift guide [occasion]    how to prepare memories and lyrics
       /song-gift pricing            prices and what a finished song comes with
       /song-gift, alone             what the cell is connected to")

(defun read-settings (values table)
  "The declared VALUES, with the endpoint checked here, once."
  (let ((written (and table (gethash "endpoint" table))))
    ;; A member written and left empty is not the same as one left out: the
    ;; declaration would default it, and the operator meant to unset it.
    (when (and table (nth-value 1 (gethash "endpoint" table))
               (not (eq written :null))
               (stringp written)
               (null (nonempty (nlk:trimmed written))))
      (nlk:config-error
       "no endpoint: \"endpoint\" is empty; leave the member out for ~a, or name an MCP url of your own"
       +endpoint+))
    (let ((url (or (nonempty (nlk:trimmed (or (getf values :endpoint) ""))) +endpoint+)))
      (unless (http-scheme-p url)
        (nlk:config-error
         "\"endpoint\" must be an http:// or https:// address; the catalog's own is ~a" +endpoint+))
      (list :endpoint (string-right-trim "/" url)
            :timeout-seconds (max 5 (min 300 (or (getf values :timeout-seconds) 45)))
            :max-chars (max 200 (min 4000 (or (getf values :max-chars) 1200)))))))

(defun status-text ()
  "What this cell is connected to."
  (format nil "song-gift: ~a; four tools over the public catalog~%~
list_occasions, list_styles, guide_story_and_lyrics, get_pricing_options~%~%
~a"
          (endpoint) +usage+))

(defun run-command (args session-id)
  "One /song-gift invocation: alone, what the cell is connected to; with a
word, that part of the catalog."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond
      ((zerop (length text)) (status-text))
      ((equal head "occasions") (occasions-tool (decode-occasions-call (nlk:json-object))))
      ((equal head "styles")
       (styles-tool (decode-styles-call (bolt "occasion" (nonempty rest)))))
      ((equal head "guide")
       (guide-tool (decode-guide-call (bolt "occasion" (nonempty rest)))))
      ((equal head "pricing") (pricing-tool (decode-pricing-call (nlk:json-object))))
      (t (format nil "~a~%~%unknown: ~a" +usage+ text)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell song-gift
  (:section ("song-gift")
    (:guide "nothing is required: the endpoint is the gift studio's public MCP catalog, no key and no account; max_chars bounds one answer's size; next step: /song-gift shows the connection")
    ("endpoint" :string :default "https://customsonggift.org/api/mcp"
     :doc "the MCP endpoint of the catalog; keep the default unless you run a server of your own")
    ("timeout_seconds" :integer :default 45 :min 5
     :doc "one tool call's ceiling, handshake and answer together (at most 300)")
    ("max_chars" :integer :default 1200 :min 200
     :doc "how much of one answer's text or one field is laid out (at most 4000)"))
  (:settings #'read-settings)
  (:help :song-gift
         "song-gift: gift occasions, musical styles and lyric storytelling advice for a personalized song - (help :song-gift)"
         +primer+)
  (:command "song-gift" 'run-command
            :description "Custom Song Gift: occasions, styles and lyric advice"
            :argument-hint "[occasions|styles [occasion]|guide [occasion]|pricing]"
            :session nil)
  (:tool "list_occasions" (nlk:contract 'occasions-call) #'occasions-tool)
  (:tool "list_styles" (nlk:contract 'styles-call) #'styles-tool)
  (:tool "guide_story_and_lyrics" (nlk:contract 'guide-call) #'guide-tool)
  (:tool "get_pricing_options" (nlk:contract 'pricing-call) #'pricing-tool))
