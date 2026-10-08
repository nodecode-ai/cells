;;;; cell.lisp --- the cell: the gitlab-duo-agent lane among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One lane of its own and two hooks, each declining for every provider but
;;;; gitlab-duo-agent, and one command:
;;;;
;;;;   the gitlab-duo-agent lane  registered at start, taken out at stop:
;;;;                          CALL-DUO-WORKFLOW-STREAMING (workflow.lisp) runs
;;;;                          each round as a Duo workflow over GitLab's
;;;;                          WebSocket; the catalog row's package names it
;;;;   MODELS-CATALOG-TABLE   the catalog carries GitLab Duo Agent's row: the
;;;;                          lane's package, the instance as its base, the
;;;;                          models the account's namespace offers (else
;;;;                          omp's fallback row)
;;;;   :CREDENTIAL            the signed-in GitLab token, refreshed before it
;;;;                          expires, else GITLAB_TOKEN
;;;;
;;;;   /gitlab-duo-agent login | code TEXT | models | logout | status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "gitlab-duo-agent": {"gitlab_url": "https://gitlab.com",
;;;;                        "namespace_id": "", "project": "",
;;;;                        "workflow_definition": "ambient"}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-gitlab-duo-agent)

(defun register-lane ()
  "The gitlab-duo-agent lane: the Duo Workflow Service under this provider's name."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'call-duo-workflow-streaming
                            ;; the family only names the env ladder's default
                            ;; key, which the credential hook never falls to
                            :family :openai-completions
                            ;; a cut attempt's reasoning comes back as text: the
                            ;; goal transcript carries no thinking
                            :reasoning-carry :text
                            :default-endpoint +gitlab-url+
                            :path ""
                            :npm +npm+)))

(defun unregister-lane ()
  "Take the gitlab-duo-agent lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun discover-at-start (&optional (auth-path nle::*auth-file-path*))
  "When a sign-in is kept at AUTH-PATH, find the models the account offers,
on a thread of its own: a start never waits on GitLab."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file auth-path)))))
    (when (nlk:json-value entry :text "access_token")
      (bt2:make-thread (lambda ()
                         (let ((token (ignore-errors (fresh-token entry auth-path))))
                           (when token (discover token))))
                       :name "nodecode-gitlab-duo-agent start"))))

(defun start ()
  "Register the lane; on the way down take it out, end every waiting
workflow and any sign-in, and forget what discovery found."
  (forget-catalog)
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (nle:on-stop #'end-sessions)
  (nle:on-stop #'cancel-flow)
  (nle:on-stop (lambda () (setf *discovered* nil) (forget-catalog)))
  (discover-at-start))

(nle:define-cell gitlab-duo-agent
  (:section ("gitlab-duo-agent")
    (:guide "sign in with /gitlab-duo-agent login, or set GITLAB_TOKEN to a personal access token with the api scope; namespace_id or project scopes the workflow, else the session's GitLab remote or the first group with Duo does")
    ("gitlab_url" :string :default +gitlab-url+
     :doc "the GitLab instance: sign-in, the REST setup and the workflow socket")
    ("namespace_id" :string :default ""
     :doc "the root group the workflow runs in (an id, or gid://gitlab/Group/N); empty to find one")
    ("project" :string :default ""
     :doc "the project the workflow runs in: an id, or a group/project path; empty to find one")
    ("workflow_definition" :string :default +workflow-definition+
     :doc "the Duo flow a round runs: ambient, the inline flow omp runs"))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:command "gitlab-duo-agent" #'run-command
            :description "GitLab Duo Agent: sign in with GitLab, list the models it offers"
            :argument-hint "login | code <address or code> | models | logout | status")
  (:start #'start))
