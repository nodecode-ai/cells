;;;; cell.lisp --- the primer, the section, the tools, /lovedone, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "lovedone": {"api_key": "...", "api_key_env": "LOVEDONE_API_KEY",
;;;;                 "url": "https://api.lovedone.app/mcp/v1",
;;;;                 "timeout_seconds": 30}
;;;; The key is the one required member, pasted or named by a variable -- as
;;;; the original read LOVEDONE_API_KEY from the environment -- and a section
;;;; naming no key refuses the start, the way the original offered no tool
;;;; without one.

(in-package #:nodecode-lovedone)

;;; --- the manual ---------------------------------------------------------------
;;; (help :lovedone) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "The LovedOne Family Inbox is reachable through the nodecode-lovedone cell: one
tool a verb, and the same calls from eval as (lovedone:call \"<tool>\" arguments).
It is the family's shared inbox for an aging parent -- mail from doctors, pharmacies,
insurers and attorneys, the loved ones it belongs to, and the family around them.
  lovedone_ping                 the key works, and what it authenticated as
  lovedone_profile              the signed-in person and their family workspace
  lovedone_list_loved_ones      the loved ones: id, name, Family Inbox address
  lovedone_list_family_members  who is on the workspace, their role and invite state
  lovedone_list_inbox_messages  one loved one's recent messages (subject, sender, preview)
  lovedone_get_inbox_message    one message as plain text
  lovedone_list_care_events     recent care events, one loved one or the workspace
  lovedone_invite_family_member WRITES: invite a family member by email
  lovedone_create_provider_code WRITES: mint a short-lived provider link code
  lovedone_reply_to_inbox_message WRITES: send a plaintext reply to a message
  lovedone_call                 any other tool the server offers, by its own name
Reads answer with what the server returned: cite it, and do not invent a message or a
date it did not carry. Write tools change a real family's shared workspace -- call one
only when the user clearly asked for that write, and say what you sent afterwards. This
is senior tech support and family coordination, not healthcare: do not diagnose, and do
not call anyone a patient. Answers can carry health information, so quote only what the
question needs. A refused key or an expired session is a LOVEDONE-ERROR naming the
reason; the key itself is in the lovedone section of the config, never in the chat."
  "What (help :lovedone) answers while the cell runs.")

;;; --- reading the section --------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, with the key read here, once: the inline secret, else
the variable api_key_env names, else a refusal."
  (declare (ignore table))
  (let* ((key (nonblank (getf values :api-key)))
         (env (nonblank (getf values :api-key-env)))
         (plist (copy-list values)))
    (setf (getf plist :api-key)
          (or key
              (and env (nonblank (nle::credential-env env)))
              (nlk:config-error
               "no key: paste a LovedOne Family MCP key into api_key, or name the ~
variable holding it in api_key_env~@[ (~a is unset or empty)~]"
               env)))
    plist))

;;; --- the schemas ---------------------------------------------------------------
;;; The original served its tool schemas from the remote server; these are the
;;; ten its README documents, written here so the model sees them before the
;;; first call. LOVEDONE_CALL reaches any tool a later server adds.

(defun prop (type description)
  "One property of a tool's arguments object."
  (nlk:json-object "type" type "description" description))

(defun tool-schema (description properties &optional required)
  "A tool's advertised schema: DESCRIPTION, the object of PROPERTIES, and the
member names REQUIRED of a call."
  (list :description description
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" properties
                     :when required "required" (coerce required 'vector))))

(defparameter +ping+
  (tool-schema "Check that the LovedOne key works: who it authenticated as and on
which workspace. No health information."
               (nlk:json-object)))

(defparameter +profile+
  (tool-schema "The signed-in person's profile and the family workspace it is linked to."
               (nlk:json-object)))

(defparameter +loved-ones+
  (tool-schema "The loved ones on this family workspace: id, name and Family Inbox address."
               (nlk:json-object)))

(defparameter +members+
  (tool-schema "The people on this family workspace: their role, relationship and
whether their invite is accepted."
               (nlk:json-object)))

(defparameter +inbox+
  (tool-schema "Recent messages in one loved one's Family Inbox: subject, sender,
category and a preview. May carry health information."
               (nlk:json-object
                "loved_one_id" (prop "string" "the loved one's id, from lovedone_list_loved_ones")
                "limit" (prop "integer" "how many messages, newest first"))
               '("loved_one_id")))

(defparameter +message+
  (tool-schema "One Family Inbox message as plain text, by its id. May carry health information."
               (nlk:json-object
                "message_id" (prop "string" "the message's id, from lovedone_list_inbox_messages"))
               '("message_id")))

(defparameter +events+
  (tool-schema "Recent care events, for one loved one or across the workspace."
               (nlk:json-object
                "loved_one_id" (prop "string" "the loved one's id; empty asks across the workspace")
                "limit" (prop "integer" "how many events, newest first"))))

(defparameter +invite+
  (tool-schema "WRITES. Invite a family member to the workspace by email. Ask the user
first: this sends a real invitation."
               (nlk:json-object
                "email" (prop "string" "the family member's email address")
                "role" (prop "string" "coordinator or viewer")
                "relationship" (prop "string" "how they are related, such as daughter"))
               '("email")))

(defparameter +provider-code+
  (tool-schema "WRITES. Mint a short-lived provider link code for one loved one, so a
doctor's office, pharmacy or insurer can send to that inbox."
               (nlk:json-object
                "loved_one_id" (prop "string" "the loved one's id"))
               '("loved_one_id")))

(defparameter +reply+
  (tool-schema "WRITES. Send a plaintext reply to a Family Inbox message. Ask the user
first, and show them the text you are about to send."
               (nlk:json-object
                "message_id" (prop "string" "the message's id being replied to")
                "body" (prop "string" "the plain text to send"))
               '("message_id" "body")))

(defparameter +any+
  (tool-schema "Call any LovedOne MCP tool by the server's own name, for one this cell
does not list. Use the tool list in (help :lovedone) first."
               (nlk:json-object
                "tool" (prop "string" "the tool's name on the server, such as list_loved_ones")
                "arguments" (nlk:json-object "type" "object"
                                              "description" "the arguments object that tool takes"))
               '("tool")))

;;; --- the tools ------------------------------------------------------------------

(defun ping-tool (arguments)
  (call "ping" arguments))

(defun profile-tool (arguments)
  (call "get_user_profile" arguments))

(defun loved-ones-tool (arguments)
  (call "list_loved_ones" arguments))

(defun members-tool (arguments)
  (call "list_family_members" arguments))

(defun inbox-tool (arguments)
  (call "list_inbox_messages" arguments))

(defun message-tool (arguments)
  (call "get_inbox_message" arguments))

(defun events-tool (arguments)
  (call "list_care_events" arguments))

(defun invite-tool (arguments)
  (call "invite_family_member" arguments))

(defun provider-code-tool (arguments)
  (call "create_provider_code" arguments))

(defun reply-tool (arguments)
  (call "reply_to_inbox_message" arguments))

(defun any-tool (arguments)
  "The escape hatch: one tool by the server's own name."
  (let ((tool (nlk:json-value arguments :string "tool"))
        (inner (nlk:json-value arguments :object "arguments")))
    (unless (nonblank tool)
      (fail "lovedone_call needs the name of a tool to call"))
    (call tool inner)))

;;; --- the command ----------------------------------------------------------------

(defun run-slash (args session-id)
  "One /lovedone invocation: the family it is connected to, or -- with a loved
one's id -- that loved one's recent inbox."
  (declare (ignore session-id))
  (let ((which (nlk:trimmed (or args ""))))
    (if (plusp (length which))
        (format nil "Inbox for ~a:~2%~a" which
                (call "list_inbox_messages" (nlk:json-object "loved_one_id" which)))
        (format nil "~a~2%~a"
                (call "get_user_profile")
                (call "list_loved_ones")))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell lovedone
  (:section ("lovedone")
    (:guide "make an MCP key at inbox.lovedone.app (Settings -> MCP keys), then paste it into api_key or name the variable holding it in api_key_env; url and timeout_seconds rarely change")
    ("api_key" :secret
               :doc "the LovedOne Family MCP key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "LOVEDONE_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("url" :string :default "https://api.lovedone.app/mcp/v1"
           :doc "the Family Inbox MCP endpoint (Streamable HTTP, JSON-RPC 2.0)")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "seconds one request may take"))
  (:settings #'read-settings)
  (:tool "lovedone_ping" +ping+ #'ping-tool)
  (:tool "lovedone_profile" +profile+ #'profile-tool)
  (:tool "lovedone_list_loved_ones" +loved-ones+ #'loved-ones-tool)
  (:tool "lovedone_list_family_members" +members+ #'members-tool)
  (:tool "lovedone_list_inbox_messages" +inbox+ #'inbox-tool)
  (:tool "lovedone_get_inbox_message" +message+ #'message-tool)
  (:tool "lovedone_list_care_events" +events+ #'events-tool)
  (:tool "lovedone_invite_family_member" +invite+ #'invite-tool)
  (:tool "lovedone_create_provider_code" +provider-code+ #'provider-code-tool)
  (:tool "lovedone_reply_to_inbox_message" +reply+ #'reply-tool)
  (:tool "lovedone_call" +any+ #'any-tool)
  (:command "lovedone" 'run-slash
            :catches 'lovedone-error
            :description "Your family's LovedOne inbox"
            :argument-hint "LOVED-ONE-ID")
  (:help :lovedone
   "lovedone: a family's shared LovedOne inbox - lovedone_list_inbox_messages and the rest, or (lovedone:call \"list_loved_ones\")"
   *primer*)
  (:start (lambda ()
            (setf *session* nil
                  *handshaked* nil)
            (nle:on-stop (lambda ()
                           (setf *session* nil
                                 *handshaked* nil))))))
