;;;; cell.lisp --- the cell: Antigravity's agy CLI as a provider lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every other provider:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries antigravity-agy's row:
;;;;                           the chat lane's package, the relay's loopback
;;;;                           address as the base, and the slugs agy serves,
;;;;                           so /models lists them and a turn resolves its
;;;;                           lane and address like any catalog provider's
;;;;   :CREDENTIAL             a placeholder, since the login is the CLI's own
;;;;                           and the relay needs no key
;;;;
;;;; The relay is started with the cell and stopped with it. A vetoed
;;;; section ("enabled": false) installs nothing, the relay included.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "antigravity-agy": {"command": "agy",
;;;;                       "models": ["gemini-3.8-flash-low"],
;;;;                       "timeout_seconds": 300}

(in-package #:nodecode-antigravity-agy)

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the antigravity-agy row in it.")

(defun catalog-model (id)
  "ID as the catalog keeps a model: its family's window, text only, tools."
  (nle::make-catalog-model id (context-window id) nil #("text") #("text")
                            nil nil nil t nil))

(defun catalog-row ()
  "Antigravity's agy CLI as a models.dev provider: the chat lane's package,
the relay's base, and the slugs the section offers."
  (let ((models (make-hash-table :test 'equal)))
    (dolist (id (model-ids))
      (setf (gethash id models) (catalog-model id)))
    (nlk:json-object "name" "Antigravity (agy CLI)"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (relay-base)
                     "env" #()
                     "models" models)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the antigravity-agy row,
made once per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another section builds a new one."
  (setf *catalog* (cons nil nil)))

(defun credential (op next)
  "The :CREDENTIAL answer for antigravity-agy: a placeholder, because the
login is agy's own and nothing leaves this machine but agy's own traffic."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential "public" :public)
      (funcall next op)))

(defun start ()
  "Serve the relay, publish the row, and say when there is no CLI to drive."
  (forget-catalog)
  (start-relay)
  (nle:on-stop (lambda () (stop-relay) (forget-catalog)))
  (unless (agy-available-p)
    (nle:notice (format nil "antigravity-agy: no ~a on PATH, so antigravity-agy/ models cannot run; install the Antigravity CLI, run it once to sign in, then (restart-cells \"nodecode-antigravity-agy\")"
                        (first (command-argv)))
                :level :warning :key "nodecode-antigravity-agy")
    (nle:on-stop (lambda () (nle:notice nil :key "nodecode-antigravity-agy")))))

(nle:define-cell antigravity-agy
  (:section ("antigravity-agy")
    (:guide "command is the Antigravity CLI (https://antigravity.google/docs/cli/install), signed in by running it once; models are the slugs it serves, as `agy models` lists them")
    ("command" :string :default "agy"
     :doc "the agy CLI: a name on PATH, a path, or a command line whose first word is the program")
    ("models" :list :default (vector "gemini-3.8-flash-low" "gemini-3.8-flash-medium" "gemini-3.8-flash-high" "gemini-3.1-pro-low" "gemini-3.1-pro-high" "claude-sonnet-4-6" "claude-opus-4-6-thinking" "gpt-oss-120b-medium")
     :doc "the slugs /models offers under antigravity-agy/, exactly as the agy picker spells them")
    ("timeout_seconds" :integer :default 300 :min 1
     :doc "how long one round may take, agy's boot and its answer together"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
