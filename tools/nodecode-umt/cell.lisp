;;;; cell.lisp --- the eleven tools, /umt, the manual, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The same three read-only servers the Universal MCP Toolkit ships with no
;;;; configuration -- Hacker News, arXiv, the npm registry -- as one cell: each
;;;; of their tools is here under its source's prefix (hn_, arxiv_, npm_), so
;;;; three namespaces never collide in one model's tool list, and the same
;;;; lookups are Lisp verbs in the UMT package for eval.
;;;;
;;;; Every verb answers text, keyless; nothing is cached and nothing is
;;;; written. A call that fails answers the model "ERROR: UMT-ERROR: ..."
;;;; naming the host and what it said, so the next attempt is informed.

(in-package #:nodecode-umt)

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "Hacker News, arXiv and the npm registry are readable through the nodecode-umt cell: eleven
read-only tools, and the same lookups as Lisp functions in the umt: package, called through eval.
All three APIs are public and need no key.
Hacker News: (umt:top-stories :limit 10), (umt:new-stories :limit 10), (umt:best-stories :limit 10),
  (umt:search-stories \"common lisp\" :limit 10), (umt:item-thread 12345 :depth 2 :max-children 20)
  Each answers one numbered story a block -- title, points, comments, author, date, url, item id --
  and a thread answers its comment tree, indented under each parent.
arXiv: (umt:search-papers \"diffusion models\" :max 5 :sort \"relevance\"), (umt:paper \"2405.12345\"),
  (umt:recent-papers :category \"cs.AI\" :max 5)
  Each answers the paper's id, title, authors, categories, published date, abs and pdf urls, and
  its abstract; sort is relevance, submittedDate or lastUpdatedDate.
npm: (umt:search-packages \"mcp server\" :limit 10 :from 0), (umt:package-metadata \"express\"),
  (umt:package-versions \"express\" :limit 20)
  Each answers the registry's own metadata -- latest version and date, licence, dist-tags,
  homepage, repository, maintainers, versions.
The model has the same eleven as tools: hn_top_stories, hn_new_stories, hn_best_stories,
hn_search_stories, hn_item_thread, arxiv_search_papers, arxiv_paper, arxiv_recent_papers,
npm_search_packages, npm_package_metadata, npm_package_versions.
/umt hn QUERY, /umt arxiv QUERY and /umt npm QUERY ask by hand; /umt alone says where each
base points. A refusal is ERROR: UMT-ERROR and names what failed."
  "What (help :umt) answers while the cell runs.")

;;; --- the boundary contracts ----------------------------------------------------

(nlk:define-contract hn-top-stories
  (:doc "The Hacker News front page in its current rank order, one numbered block a story: title, points, comments, author, date, url and item id. For a topic rather than the front page, use hn_search_stories; for a known item's discussion, hn_item_thread.")
  ("limit" :integer :default 10 :doc "how many stories, 1 to 30"))

(nlk:define-contract hn-new-stories
  (:doc "The newest Hacker News submissions, newest first, one numbered block a story. Use it when recency matters more than score; hn_top_stories and hn_best_stories rank by popularity.")
  ("limit" :integer :default 10 :doc "how many stories, 1 to 30"))

(nlk:define-contract hn-best-stories
  (:doc "Hacker News's best ranking: high-scoring stories beyond the current front page, one numbered block a story. Use hn_search_stories to look a topic up by keyword.")
  ("limit" :integer :default 10 :doc "how many stories, 1 to 30"))

(nlk:define-contract hn-search-stories
  (:doc "Search Hacker News stories by keyword through the public Algolia index, ranked by its relevance, one numbered block a story. No match answers an empty list, not an error; use the story-list tools for the rankings and hn_item_thread for a known item's comments.")
  ("query" :text :required t :doc "what to search for, as words or a phrase")
  ("limit" :integer :default 10 :doc "how many stories, 1 to 30"))

(nlk:define-contract hn-item-thread
  (:doc "One Hacker News item and its nested comment tree, each reply indented under its parent. Use it once a list or a search has given you an item id; a missing item is a refusal naming the id.")
  ("itemId" :integer :required t :slot item-id :doc "the Hacker News item id, as a number")
  ("depth" :integer :default 2 :doc "how many reply levels to expand, 1 to 6")
  ("max_children" :integer :default 20 :doc "at most this many replies a level, 1 to 50"))

(nlk:define-contract arxiv-search-papers
  (:doc "Search arXiv for papers by keyword across every field. Each answer carries the paper's id, title, authors, categories, date, abs and pdf urls and its abstract; sort is relevance, submittedDate or lastUpdatedDate.")
  ("query" :text :required t :doc "what to search for, as words or a phrase")
  ("max" :integer :default 5 :slot max-results :doc "how many papers, 1 to 25")
  ("sort" :text :default "relevance" :slot sort-by :doc "relevance, submittedDate or lastUpdatedDate"))

(nlk:define-contract arxiv-paper
  (:doc "One arXiv paper by its identifier, with its whole abstract, authors, categories, dates, and its abs and pdf urls.")
  ("paperId" :text :required t :slot paper-id :doc "the arXiv identifier, e.g. 2405.12345 or 2405.12345v2"))

(nlk:define-contract arxiv-recent-papers
  (:doc "The newest papers arXiv announced in one category, newest first, one block a paper. Use it to follow a field; use arxiv_search_papers to look something up by keyword.")
  ("category" :text :doc "the arXiv category, e.g. cs.AI; the cell's default when left out")
  ("max" :integer :default 5 :slot max-results :doc "how many papers, 1 to 25"))

(nlk:define-contract npm-search-packages
  (:doc "Search the npm registry for packages by text, answering each result's name, latest version, publish date, description and links. Paging is FROM plus LIMIT; use npm_package_metadata for one package in full.")
  ("query" :text :required t :doc "what to search for, as words or a phrase")
  ("limit" :integer :default 10 :doc "how many results, 1 to 50")
  ("from" :integer :default 0 :doc "how many results to skip, 0 to 500"))

(nlk:define-contract npm-package-metadata
  (:doc "One npm package's registry metadata: its latest version and when it was published, its licence, how many versions exist, its dist-tags, homepage, repository and maintainers.")
  ("packageName" :text :required t :slot name :doc "the package name, scoped names as @scope/name"))

(nlk:define-contract npm-package-versions
  (:doc "An npm package's versions, newest first, each with its publish date and whether it is deprecated, after the package's dist-tags. Use npm_package_metadata for the package as a whole.")
  ("packageName" :text :required t :slot name :doc "the package name, scoped names as @scope/name")
  ("limit" :integer :default 20 :doc "how many versions, 1 to 50"))

;;; --- their tools -----------------------------------------------------------------

(defun guarded (body)
  "BODY's string; a refusal becomes the tool answer the model repairs from,
naming this cell so the transcript shows which call failed."
  (handler-case (funcall body)
    (umt-error (condition) (nle:failure (format nil "ERROR: UMT-ERROR: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: UMT-ERROR: ~a" condition)))))

(defun top-stories-tool (call)
  (guarded (lambda () (top-stories :limit (hn-top-stories-limit call)))))

(defun new-stories-tool (call)
  (guarded (lambda () (new-stories :limit (hn-new-stories-limit call)))))

(defun best-stories-tool (call)
  (guarded (lambda () (best-stories :limit (hn-best-stories-limit call)))))

(defun search-stories-tool (call)
  (guarded (lambda () (search-stories (hn-search-stories-query call)
                                      :limit (hn-search-stories-limit call)))))

(defun item-thread-tool (call)
  (guarded (lambda () (item-thread (hn-item-thread-item-id call)
                                   :depth (hn-item-thread-depth call)
                                   :max-children (hn-item-thread-max-children call)))))

(defun search-papers-tool (call)
  (guarded (lambda () (search-papers (arxiv-search-papers-query call)
                                     :max (arxiv-search-papers-max-results call)
                                     :sort (arxiv-search-papers-sort-by call)))))

(defun paper-tool (call)
  (guarded (lambda () (paper (arxiv-paper-paper-id call)))))

(defun recent-papers-tool (call)
  (guarded (lambda () (recent-papers :category (arxiv-recent-papers-category call)
                                     :max (arxiv-recent-papers-max-results call)))))

(defun search-packages-tool (call)
  (guarded (lambda () (search-packages (npm-search-packages-query call)
                                       :limit (npm-search-packages-limit call)
                                       :from (npm-search-packages-from call)))))

(defun package-metadata-tool (call)
  (guarded (lambda () (package-metadata (npm-package-metadata-name call)))))

(defun package-versions-tool (call)
  (guarded (lambda () (package-versions (npm-package-versions-name call)
                                        :limit (npm-package-versions-limit call)))))

;;; --- /umt ------------------------------------------------------------------------

(defun status-line ()
  "One line: where the three bases point, the default category, the ceiling."
  (format nil "umt: Hacker News at ~a (search ~a), arXiv at ~a, npm at ~a; default category ~a; ~ds a call"
          (hn-api-base) (hn-search-base) (arxiv-api-base) (npm-registry-base)
          (setting :default-category) (setting :timeout)))

(defun panel ()
  "What bare /umt answers besides its line: one ask-row a source."
  (nle:list-dialog
   :title "UMT" :summary (status-line)
   :rows (list (nle:ask-row "Search Hacker News" "Query" "/umt hn"
                             :placeholder "what to look up" :verb "search")
               (nle:ask-row "Search arXiv" "Query" "/umt arxiv"
                             :placeholder "what to look up" :verb "search")
               (nle:ask-row "Search npm" "Query" "/umt npm"
                             :placeholder "what to look up" :verb "search"))))

(defun split-verb (args)
  "ARGS as (values HEAD REST): the first word, and the trimmed remainder."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun asked (body)
  "BODY's answer, or its refusal as text -- what a command shows."
  (handler-case (funcall body)
    (umt-error (condition) (format nil "ERROR: UMT-ERROR: ~a" condition))
    (error (condition) (format nil "ERROR: UMT-ERROR: ~a" condition))))

(defun run-slash (args session-id)
  "One /umt invocation => the answer's text, and typed alone the dialog."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (split-verb args)
    (cond ((zerop (length head)) (values (status-line) (panel)))
          ((string-equal head "status") (values (status-line) (panel)))
          ((string-equal head "hn")
           (values (if (zerop (length rest))
                       "umt: search Hacker News for what? /umt hn QUERY"
                       (asked (lambda () (search-stories rest))))
                   (panel)))
          ((string-equal head "arxiv")
           (values (if (zerop (length rest))
                       "umt: search arXiv for what? /umt arxiv QUERY"
                       (asked (lambda () (search-papers rest))))
                   (panel)))
          ((string-equal head "npm")
           (values (if (zerop (length rest))
                       "umt: search npm for what? /umt npm QUERY"
                       (asked (lambda () (search-packages rest))))
                   (panel)))
          (t (values (format nil "umt: ~a is not one of its choices (hn, arxiv, npm, status), or type /umt QUERY as ~
umt: hn QUERY" head)
                     (panel))))))

;;; --- the one declaration ---------------------------------------------------------

(nle:define-cell umt
  (:section ("umt")
    (:guide "no key: three public, read-only APIs, so the defaults work as they are; a base points at a mirror, timeout bounds one call")
    ("hn_api_base" :string :default "https://hacker-news.firebaseio.com/v0"
                   :doc "the Hacker News Firebase API the rankings and items come from")
    ("hn_search_base" :string :default "https://hn.algolia.com/api/v1"
                       :doc "the Algolia index keyword search reads")
    ("arxiv_api_base" :string :default "https://export.arxiv.org/api/query"
                       :doc "the arXiv Atom query endpoint")
    ("npm_registry_base" :string :default "https://registry.npmjs.org"
                          :doc "the npm registry, for search and package metadata")
    ("default_category" :string :default "cs.AI"
                         :doc "the arXiv category a recent-papers call reads when it names none")
    ("timeout" :integer :default 20 :min 1
               :doc "seconds one call to an API may take"))
  (:tool "hn_top_stories" (nlk:contract 'hn-top-stories) 'top-stories-tool)
  (:tool "hn_new_stories" (nlk:contract 'hn-new-stories) 'new-stories-tool)
  (:tool "hn_best_stories" (nlk:contract 'hn-best-stories) 'best-stories-tool)
  (:tool "hn_search_stories" (nlk:contract 'hn-search-stories) 'search-stories-tool)
  (:tool "hn_item_thread" (nlk:contract 'hn-item-thread) 'item-thread-tool)
  (:tool "arxiv_search_papers" (nlk:contract 'arxiv-search-papers) 'search-papers-tool)
  (:tool "arxiv_paper" (nlk:contract 'arxiv-paper) 'paper-tool)
  (:tool "arxiv_recent_papers" (nlk:contract 'arxiv-recent-papers) 'recent-papers-tool)
  (:tool "npm_search_packages" (nlk:contract 'npm-search-packages) 'search-packages-tool)
  (:tool "npm_package_metadata" (nlk:contract 'npm-package-metadata) 'package-metadata-tool)
  (:tool "npm_package_versions" (nlk:contract 'npm-package-versions) 'package-versions-tool)
  (:command "umt" 'run-slash
            :description "Hacker News, arXiv and npm lookups"
            :argument-hint "[hn|arxiv|npm] QUERY")
  (:help :umt "umt: Hacker News, arXiv and npm lookups - umt:top-stories, umt:search-papers, umt:package-metadata" +manual+))
