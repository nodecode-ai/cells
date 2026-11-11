;;;; cell.lisp --- the declaration, four tools, /forum-index, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A directory, not a client: every verb answers from the list in DATA.LISP
;;;; without a request, a key or an account. A link is a string a person opens
;;;; themselves -- the onion ones through Tor -- which is the whole of the
;;;; "direct access" this cell gives.

(in-package #:nodecode-forum-index)

;;; --- the manual --------------------------------------------------------------------

(defparameter +primer+
  "A directory of the live hacking, fraud and security forums: who each one is for, where it is,
and how careful the poster says to be. One snapshot of a Dread /d/hacking list (10 Nov 2026),
with the poster's own categories and tiers.
  forum_index_list    every forum, or those of one category or tier. The row gives the name,
      its categories and its first link. Tiers: established (around a long time; not vouched),
      rising (established, but concerns have been raised), other (listed, no tier).
  forum_index_get     one forum in full: its links -- clearnet and onion -- its categories, its
      note, and what its tier means.
  forum_index_search  free text over names, links, categories and notes: a word like 'carder',
      a domain, or 'escrow'.
  forum_index_summary how many, by category and tier, and where the list came from.
Explain the caveat with the answer, every time: these are places where people buy and sell stolen
data, malware, accounts and carding, and none of them is vouched for. A forum on the list can be
seized, dead, or run by the people it claims to protect against. Give the link so a person can
look, and say plainly that visiting is their call -- an onion link needs Tor, and a clearnet one
is a browser away. Do not fetch, crawl or 'verify' them unless asked: the list is the deliverable."
  "What (help :forum-index) answers while the cell runs.")

;;; --- the boundaries the model crosses ------------------------------------------------

(nlk:define-contract forum-index-list
  (:doc "List the hacking, fraud and security forums in the directory: name, categories and its first link, one line each. Narrow by category (fraud, leaks, hacking, cracking, malware, seo, development, accounts) or by tier (established, rising, other). Use it when the user asks which forums there are, or which ones trade in one thing. Give the links as they come, and say the list is a snapshot and none of them is vouched for.")
  ("category" :text
              :doc "one of fraud, leaks, hacking, cracking, malware, seo, development, accounts")
  ("tier" :text
          :doc "established, rising, or other"))

(nlk:define-contract forum-index-get
  (:doc "One forum in full: every link it has (clearnet and onion), its categories, the poster's note on it, and what its tier means. The name may be approximate -- 'carder' finds Legit Carder. Use it when the user names a forum or asks what one is.")
  ("name" :text :required t
          :doc "the forum's name, or enough of it to name one"))

(nlk:define-contract forum-index-search
  (:doc "Search the directory by any word: a name, a domain, a category, or a word from a note ('compromised', 'escrow', 'market'). Answers the forums that mention it, one line each. Use it when the user describes what they want without naming a forum.")
  ("text" :text :required t
          :doc "a word or phrase to look for"))

;;; --- what the model calls ----------------------------------------------------------

(defun list-text (&key category tier)
  "The rows of one category or tier, with the source under them."
  (let ((rows (forums-matching :category category :tier tier)))
    (format nil "~a~%~a"
            (forum-list-text rows
                              :heading (cond ((and category tier) (format nil "~a forums, ~a" category tier))
                                             (category (format nil "~a forums" category))
                                             (tier (format nil "~a forums" tier))
                                             (t "every forum in the list")))
            (source-line))))

(defun find-text (name)
  "One forum, whole, or the whole list when NAME names none."
  (let ((forum (forum-by-name name)))
    (if forum
        (format nil "~a~%~a~%" (forum-block forum) (source-line))
        (format nil "no forum in the list is named ~s. ~a"
                name (forum-list-text (forums) :heading "what the list has")))))

(defun search-text (text)
  "The forums that mention TEXT, with the source under them."
  (format nil "~a~%~a"
          (forum-list-text (forums-matching :text text)
                           :heading (format nil "forums mentioning ~s" text))
          (source-line)))

(defun list-tool (arguments)
  "The forum_index_list tool: the rows of one tier or category."
  (list-text :category (forum-index-list-category arguments)
             :tier (forum-index-list-tier arguments)))

(defun get-tool (arguments)
  "The forum_index_get tool: one forum, whole."
  (find-text (forum-index-get-name arguments)))

(defun search-tool (arguments)
  "The forum_index_search tool: the forums that mention a word."
  (search-text (forum-index-search-text arguments)))

(defparameter +summary-schema+
  (list :description
        (concatenate 'string
                     "How many forums the directory holds, split by category and by tier, and where the "
                     "list came from. Use it when the user asks how many there are or what the list covers.")
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "forum_index_summary takes no members: the directory counts itself.")

(defun summary-tool (arguments)
  "The forum_index_summary tool; its arguments are empty by declaration."
  (declare (ignore arguments))
  (format nil "~a~%~a" (summary-text) (source-line)))

;;; --- /forum-index -------------------------------------------------------------------

(defparameter +usage+
  "usage: /forum-index [words|category|tier]
  /forum-index             the whole list at a glance: how many, by what, and from where
  /forum-index categories  the legend: what each category and tier means
  /forum-index fraud       every forum of one category (fraud, leaks, hacking, cracking, malware,
                           seo, development, accounts) or one tier (established, rising, other)
  /forum-index carder      search names, links, categories and notes")

(defun run-slash (args session-id)
  "One /forum-index invocation: the summary, the legend, a filtered list, or a search."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text)) (format nil "~a~%~a" (summary-text) +usage+))
          ((string-equal text "categories") (format nil "~a~%~a" (category-text) (source-line)))
          ((member text (mapcar #'car +legend+) :test #'string-equal)
           (list-text :category text))
          ((member text '("established" "rising" "other" "dangerous") :test #'string-equal)
           (list-text :tier text))
          (t (search-text text)))))

;;; --- the entry ------------------------------------------------------------------------

(nle:define-cell forum-index
  (:section ("forum-index")
    (:guide "nothing is required: the directory ships with the cell. tor_socks is the address a
 person's Tor answers on, only so the cell can say how to open an onion link.")
    ("tor_socks" :string :default "127.0.0.1:9050"
                 :doc "where a local Tor answers, as the cell says it when an onion link is given"))
  (:help :forum-index
         "A directory of hacking, fraud and security forums: who is for what, where, and how careful to be"
         +primer+)
  (:tool "forum_index_list" (nlk:contract 'forum-index-list) 'list-tool)
  (:tool "forum_index_get" (nlk:contract 'forum-index-get) 'get-tool)
  (:tool "forum_index_search" (nlk:contract 'forum-index-search) 'search-tool)
  (:tool "forum_index_summary" +summary-schema+ 'summary-tool)
  (:command "forum-index" 'run-slash
            :description "The forums of the hacking, fraud and security underground"
            :argument-hint "category|tier|words")
  (:start (lambda () nil)))
