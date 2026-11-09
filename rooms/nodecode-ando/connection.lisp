;;;; connection.lisp --- the invitation, redeemed once, and its private file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An invitation link is the whole of the setup: the page it names says
;;;; which agent it is for (a new one, or the existing agent being
;;;; reconnected), and one POST to the same host answers the connection --
;;;; the api key, the workspace, the agent membership, the MCP URL. The key
;;;; goes straight into a mode-600 file, never into the config, never into a
;;;; transcript. Only the two hosts below are accepted, so a link for another
;;;; origin cannot hand this cell someone else's destination.

(in-package #:nodecode-ando)

(defparameter +environments+
  '(("agents.ando.so" . (:mcp "https://mcp.ando.so/mcp"
                          :api "https://api.ando.so"))
    ("staging-agents.ando.so" . (:mcp "https://ando-mcp-staging.onrender.com/mcp"
                               :api "https://ando-backend-staging.onrender.com")))
  "The hosts an invitation may name, and the endpoints each stands for.")

(defparameter +harness+ "nodecode"
  "What the redemption says is asking; Ando labels the agent with it.")

(defparameter +code-alphabet+ "abcdefghjkmnpqrstuvwxyz23456789"
  "An invitation code's alphabet: no l, o, 0 or 1 to misread.")

(defun invitation-code-p (code)
  "Whether CODE is an invitation code Ando issues."
  (or (and (= 32 (length code)) (every (lambda (c) (digit-char-p c 16)) code))
      (and (= 10 (length code))
           (every (lambda (c) (find c +code-alphabet+)) code))))

(defun split-https (text)
  "TEXT as (values HOST PATH) when it is an https URL, else NIL."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (when (uiop:string-prefix-p "https://" text)
      (let* ((rest (subseq text 8))
             (slash (position #\/ rest)))
        (values (if slash (subseq rest 0 slash) rest)
                (if slash (subseq rest slash) "/"))))))

(defun invitation-parts (invite)
  "INVITE, an Ando invitation link, as (values HOST CODE); refuses any other link."
  (multiple-value-bind (host path) (split-https invite)
    (unless (and host (assoc host +environments+ :test #'string=))
      (fail "~s is not an Ando invitation link: use the one from the workspace's Invite members sheet"
            invite))
    (when (or (find #\? path) (find #\# path))
      (fail "An Ando invitation link carries no query or fragment: ~s" invite))
    (unless (uiop:string-prefix-p "/invite/" path)
      (fail "An Ando invitation link's path is /invite/<code>: ~s" invite))
    (let ((code (subseq path 8)))
      (unless (invitation-code-p code)
        (fail "~s does not carry an Ando invitation code" invite))
      (values host code))))

(defun page-agent (page)
  "The agent id an invitation PAGE says it keeps, or NIL for a new agent."
  (let ((at (and (stringp page) (search "(agent " page))))
    (when at
      (subseq page (+ at 7) (min (length page) (+ at 7 36))))))

(defun random-receiver ()
  "A receiver id for this installation: the execution claim Ando asks for."
  (let ((hex (format nil "~(~32,'0x~)" (random (expt 16 32)))))
    (format nil "~a-~a-~a-~a-~a"
            (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
            (subseq hex 16 20) (subseq hex 20 32))))

(defun connection-field (object key)
  "OBJECT's KEY as a non-empty string, or NIL."
  (let ((value (nlk:json-value object :string key)))
    (and value (plusp (length (string-trim " " value))) value)))

(defun connection-from-answer (object host path)
  "OBJECT, an invitation's answer, as an ANDO-CONNECTION; refuses another shape."
  (dolist (key '("api_key" "workspace_id" "agent_membership_id"
                 "connected_by_membership_id" "mcp_url"))
    (unless (connection-field object key)
      (fail "The Ando invitation answered without ~a" key)))
  (unless (= (or (nlk:json-value object :integer "contract_version") 0) 1)
    (fail "This cell speaks Ando's invitation contract version 1; the invitation answered another"))
  (let ((endpoints (cdr (assoc host +environments+ :test #'string=))))
    (unless (equal (connection-field object "mcp_url") (getf endpoints :mcp))
      (fail "The Ando invitation named a destination this cell does not know"))
    (make-connection :api-key (connection-field object "api_key")
                     :workspace-id (connection-field object "workspace_id")
                     :membership-id (connection-field object "agent_membership_id")
                     :installer-id (connection-field object "connected_by_membership_id")
                     :mcp-url (connection-field object "mcp_url")
                     :origin (getf endpoints :api)
                     :display-name (or (connection-field object "display_name") "")
                     :receiver-id (or (connection-field object "receiver_id")
                                      (random-receiver))
                     :path (and path (pathname path)))))

(defun redeem (invite &optional name)
  "Redeem INVITE, naming a new agent NAME; => the ANDO-CONNECTION it answered.
The code is one-use: a failed POST is never retried here, because a second
attempt could pair nothing at all."
  (multiple-value-bind (host code) (invitation-parts invite)
    (let ((url (format nil "https://~a/invite/~a" host code)))
      (multiple-value-bind (page status) (dial "GET" url)
        (unless (= status 200)
          (fail "The Ando invitation is spent or expired (HTTP ~a): make a fresh one in Settings > Members"
                status))
        (let* ((existing (page-agent page))
               (body (if existing
                         (nlk:make-json-object "passcode" code "harness" +harness+)
                         (progn
                           (unless (and name (plusp (length (string-trim " " name))))
                             (fail "Name this agent first: set agent_name in the ando section"))
                           (nlk:make-json-object "passcode" code "harness" +harness+
                                                  "name" (string-trim " " name))))))
          (multiple-value-bind (answer status)
              (dial :post (format nil "https://~a/connect" host)
                    :headers '(("content-type" . "application/json"))
                    :content (nlk:encode-json-object body))
            (unless (= status 200)
              (fail "The Ando invitation was not redeemed (HTTP ~a); reconnect the existing agent from Settings > Members"
                    status))
            (let ((connection (connection-from-answer
                               (nlk:decode-json (if (stringp answer)
                                                     answer
                                                     (princ-to-string answer)))
                               host nil)))
              (when (and existing
                         (not (equal existing (ando-connection-membership-id connection))))
                (fail "The invitation is for another agent than the one its page names"))
              connection)))))))

(defun default-credential-path ()
  "Where a redeemed connection goes when the section names no file."
  (nlk:home "ando/connection.json"))

;;; --- the private file ----------------------------------------------------------

(defun credential-private-p (path)
  "Whether PATH is a regular file no one else can read or write."
  (let ((stat (sb-posix:stat (namestring path))))
    (and (zerop (logand #o077 (sb-posix:stat-mode stat)))
         (= #o100000 (logand #o170000 (sb-posix:stat-mode stat))))))

(defun credential-json (connection)
  "CONNECTION as the JSON object its file holds; the key is in it, nowhere else."
  (nlk:make-json-object
   "contract_version" 1
   "api_key" (ando-connection-api-key connection)
   "workspace_id" (ando-connection-workspace-id connection)
   "agent_membership_id" (ando-connection-membership-id connection)
   "connected_by_membership_id" (ando-connection-installer-id connection)
   "mcp_url" (ando-connection-mcp-url connection)
   "origin" (ando-connection-origin connection)
   "display_name" (ando-connection-display-name connection)
   "receiver_id" (ando-connection-receiver-id connection)))

(defun connection-for-stored (object path)
  "OBJECT, a credential file's content, as an ANDO-CONNECTION."
  (dolist (key '("api_key" "workspace_id" "agent_membership_id"
                 "connected_by_membership_id" "mcp_url" "origin"))
    (unless (connection-field object key)
      (fail "The Ando credential at ~a is incomplete: no ~a" path key)))
  (unless (= (or (nlk:json-value object :integer "contract_version") 0) 1)
    (fail "The Ando credential at ~a was written for another contract version" path))
  (unless (member (connection-field object "mcp_url")
                  (mapcar (lambda (entry) (getf (cdr entry) :mcp)) +environments+)
                  :test #'string=)
    (fail "The Ando credential at ~a names a destination this cell does not know" path))
  (make-connection :api-key (connection-field object "api_key")
                   :workspace-id (connection-field object "workspace_id")
                   :membership-id (connection-field object "agent_membership_id")
                   :installer-id (connection-field object "connected_by_membership_id")
                   :mcp-url (connection-field object "mcp_url")
                   :origin (connection-field object "origin")
                   :display-name (or (connection-field object "display_name") "")
                   :receiver-id (or (connection-field object "receiver_id")
                                    (random-receiver))
                   :path (pathname path)))

(defun read-credential (path)
  "PATH as an ANDO-CONNECTION; refuses anything but a private regular file."
  (let ((path (pathname path)))
    (unless (probe-file path)
      (fail "No Ando credential at ~a: give the ando section its invitation link, or the file it wrote"
            path))
    (unless (credential-private-p path)
      (fail "Ando credentials must be a private regular file (mode 600): ~a" path))
    (let ((object (handler-case (nlk:decode-json (uiop:read-file-string path))
                    (error () (fail "The Ando credential at ~a is not JSON" path)))))
      (connection-for-stored object path))))

(defun write-credential (connection path)
  "Write CONNECTION to PATH, mode 600, in one atomic rename; => CONNECTION."
  (let* ((path (pathname path))
         (directory (uiop:pathname-directory-pathname path))
         (temp (merge-pathnames (format nil "~a.~d.tmp" (file-namestring path)
                                        (random 1000000))
                                directory)))
    (ensure-directories-exist path)
    (with-open-file (stream temp :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
      (write-string (nlk:encode-json-object (credential-json connection)) stream))
    (sb-posix:chmod (namestring temp) #o600)
    (rename-file temp path)
    (setf (ando-connection-path connection) path)
    connection))

(defun connect-from-settings ()
  "The connection the section names: its credential file, else its
invitation, redeemed here once and written where the file would be."
  (let* ((named (setting :credential-file))
         (path (if named (pathname named) (default-credential-path)))
         (invite (setting :invite)))
    (cond
      ((probe-file path) (read-credential path))
      (invite (write-credential (redeem invite (setting :agent-name)) path))
      (t (fail "The ando section names neither a credential file nor an invitation link")))))
