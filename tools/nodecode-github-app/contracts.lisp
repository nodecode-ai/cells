;;;; contracts.lisp --- the seven tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.
;;;;
;;;; The names, the arguments and the sentences are the original plugin's
;;;; (github_app/tools.py, TOOL_SPECS): a model that learned these seven verbs
;;;; from the plugin calls them here the same way.

(in-package #:nodecode-github-app)

(nlk:define-contract github-identity
  (:doc "Verify the GitHub identity this cell acts as: the auth method (a GitHub App installation, a personal access token, or the gh CLI), the actor every result is attributed to, and the repositories it can reach. Call it before a write action.")
  ("repositories" :boolean :default t
   :doc "also list the repositories this identity can reach; false asks for the identity alone, one request cheaper"))

(nlk:define-contract issue-create
  (:doc "Create an issue in a repository as the resolved GitHub identity. The operator's allow_write_actions setting must be on, and the answer carries the bot/human attribution. Write the title as a sentence and the body as Markdown.")
  ("repo" :text :required t
   :doc "the repository, explicitly as owner/name, for example nodecode-ai/cells")
  ("title" :text :required t
   :doc "the issue title")
  ("body" :string :default ""
   :doc "the issue body, in Markdown")
  ("labels" :array :of :string :slot label-names
   :doc "label names to attach, as they exist in the repository")
  ("assignees" :array :of :string
   :doc "GitHub logins to assign the issue to"))

(nlk:define-contract issue-comment
  (:doc "Comment on an issue or a pull request as the resolved GitHub identity. The operator's allow_write_actions setting must be on, and the answer carries the attribution. Write the body as Markdown and say what you did.")
  ("repo" :text :required t
   :doc "the repository, explicitly as owner/name")
  ("number" :integer :required t :slot thread-number
   :doc "the issue or pull request number")
  ("body" :text :required t
   :doc "the comment, in Markdown"))

(nlk:define-contract issue-list
  (:doc "List the issues and pull requests of a repository. Read-only. Each row carries its number, title, state, author, labels and whether it is a pull request, so a number can be passed to github_get_issue, github_comment_issue or github_review_pr.")
  ("repo" :text :required t
   :doc "the repository, explicitly as owner/name")
  ("state" :string :default "open" :options '("open" "closed" "all")
   :doc "which issues to list")
  ("labels" :string :slot label-names :default ""
   :doc "comma-separated label names; empty for any")
  ("assignee" :string :default ""
   :doc "an assignee login; empty for any")
  ("creator" :string :default ""
   :doc "a creator login; empty for any")
  ("sort" :string :slot sort-field :default "created" :options '("created" "updated" "comments")
   :doc "the field to sort by")
  ("direction" :string :default "desc" :options '("asc" "desc")
   :doc "the sort direction")
  ("per_page" :integer :default 30
   :doc "how many rows to ask for, 1 to 100"))

(nlk:define-contract issue-read
  (:doc "Fetch one issue or pull request, and optionally its comments. Read-only. Read this before answering a thread: the number, title, state, author, body and labels are GitHub's own.")
  ("repo" :text :required t
   :doc "the repository, explicitly as owner/name")
  ("number" :integer :required t :slot thread-number
   :doc "the issue or pull request number")
  ("include_comments" :boolean :default nil
   :doc "fetch the comment thread as well, oldest first"))

(nlk:define-contract review-comment
  (:doc "One inline comment of a pull request review.")
  ("path" :text :required t
   :doc "the file the comment is on, as the diff names it")
  ("body" :text :required t
   :doc "the comment, in Markdown")
  ("position" :integer :slot diff-position
   :doc "the position in the diff, counted as GitHub counts it; use this or line with side, not both")
  ("line" :integer
   :doc "the line in the file the comment is on; side says which version")
  ("side" :string :options '("LEFT" "RIGHT")
   :doc "which version of the file line names: LEFT the old one, RIGHT the new one"))

(nlk:define-contract pr-review
  (:doc "Submit an APPROVE, REQUEST_CHANGES or COMMENT review on a pull request as the resolved identity. The operator's allow_write_actions setting must be on. commit_id must be the full 40-character head SHA the review applies to: GitHub refuses a review whose head has moved, and so does this cell.")
  ("repo" :text :required t
   :doc "the repository, explicitly as owner/name")
  ("number" :integer :required t :slot thread-number
   :doc "the pull request number")
  ("event" :text :required t :options '("APPROVE" "REQUEST_CHANGES" "COMMENT")
   :doc "the review verdict")
  ("commit_id" :text :required t
   :doc "the full 40-character SHA of the pull request head this review applies to")
  ("body" :string :default ""
   :doc "the review body, in Markdown")
  ("comments" :array :of review-comment
   :doc "inline comments, one per file position"))

(nlk:define-contract pr-merge
  (:doc "Merge a pull request as the resolved identity. The operator's allow_write_actions setting must be on, and expected_head_sha is required: the merge is rejected when the head has moved since the SHA was read. The default method is squash.")
  ("repo" :text :required t
   :doc "the repository, explicitly as owner/name")
  ("number" :integer :required t :slot thread-number
   :doc "the pull request number")
  ("expected_head_sha" :text :required t
   :doc "the full 40-character SHA of the pull request head that is being merged")
  ("method" :string :slot merge-method :default "squash" :options '("squash" "merge" "rebase")
   :doc "how to merge: squash, a merge commit, or a rebase")
  ("commit_title" :string :default ""
   :doc "the merge commit's title; empty leaves it to GitHub"))
