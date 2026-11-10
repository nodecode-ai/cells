;;;; parse3.lisp --- one entry point, and the canonical agents.md a shared
;;;; ASCII block becomes.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-myswarm)

(defun document-title (text)
  "TEXT's H1, or a plain default."
  (or (loop for line in (lines-of (or text ""))
            for groups = (nth-value 1 (cl-ppcre:scan-to-strings "^#\\s+(.+)$" (trimmed line)))
            when groups do (return (trimmed (aref groups 0))))
      "MySwarm Agent Configuration"))

(defun merge-agent (old new)
  "OLD enriched by NEW: the same name is one agent, the richer fields win."
  (if (null old) new
      (progn
        (when (and (string= (agent-persona old) "Specialized swarm agent")
                   (plusp (length (agent-persona new))))
          (setf (agent-persona old) (agent-persona new)))
        (when (and (null (agent-delegates old)) (agent-delegates new))
          (setf (agent-delegates old) (agent-delegates new)))
        (when (and (null (agent-tools old)) (agent-tools new))
          (setf (agent-tools old) (agent-tools new)))
        (when (string= (agent-role new) "orchestrator")
          (setf (agent-role old) "orchestrator"))
        old)))

(defun merge-agents (agents)
  "AGENTS with one entry per name, in first-seen order."
  (let ((order '()) (table (make-hash-table :test #'equal)))
    (dolist (agent agents)
      (let ((key (string-downcase (agent-name agent))))
        (unless (gethash key table) (push key order))
        (setf (gethash key table) (merge-agent (gethash key table) agent))))
    (loop for key in (nreverse order) collect (gethash key table))))

(defun parse-agents (text)
  "TEXT as an agents.md: (VALUES TITLE AGENTS METHOD), METHOD one of
:ASCII-BLOCK, :SECTIONS, :TABLES, :BULLETS or :MONOLITHIC."
  (let ((text (or text "")))
    (multiple-value-bind (ascii-title ascii-agents) (parse-ascii-block text)
      (cond
        (ascii-agents (values ascii-title ascii-agents :ascii-block))
        (t (let* ((sections (parse-sections text))
                  (tables (parse-tables text))
                  (agents (merge-agents (append tables sections)))
                  (title (document-title text)))
             (cond
               (agents (values title agents (if sections :sections :tables)))
               (t (let ((bullets (parse-bullets text)))
                    (cond
                      (bullets (values title bullets :bullets))
                      ((plusp (length (trimmed text)))
                       (let* ((body (remove-if (lambda (line)
                                                 (or (zerop (length (trimmed line)))
                                                     (cl-ppcre:scan "^#" (trimmed line))
                                                     (cl-ppcre:scan "^```" (trimmed line))
                                                     (cl-ppcre:scan "^<!--" (trimmed line))))
                                               (lines-of text)))
                              (first-line (or (first body) "Primary workspace agent"))
                              (second-line (or (second body) first-line)))
                         (values title
                                 (list (make-agent :name "PrimaryAgent"
                                                   :persona (abbreviate first-line)
                                                   :goal (abbreviate second-line)
                                                   :role "orchestrator"))
                                 :monolithic)))
                      (t (values title '() :empty))))))))))))

(defun swarm-markdown (title agents)
  "AGENTS as the canonical agents.md a shared ASCII block imports back as."
  (with-output-to-string (out)
    (format out "<!-- Exported via MySwarm cell | Agents: ~d -->~%# ~a~%~%" (length agents) title)
    (dolist (agent agents)
      (format out "## ~a~%" (agent-name agent))
      (format out "- **Role**: ~a~%"
              (if (string-equal (agent-role agent) "orchestrator") "Orchestrator" "Specialist"))
      (format out "- **Persona**: ~a~%" (agent-persona agent))
      (format out "- **Goal**: ~a~%" (agent-goal agent))
      (when (agent-delegates agent)
        (format out "- **Delegates to**: ~a~%" (join (agent-delegates agent) ", ")))
      (when (agent-tools agent)
        (format out "- **Tools**: ~a~%" (join (agent-tools agent) ", ")))
      (terpri out))))
