;;;; tools.lisp --- the calls, the five tool bodies, the words.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original gives the model a hosted MCP server's tools -- law.search,
;;;; law.get_node, law.list_coverage, account.status and the formation.*
;;;; family -- and two prose skills that teach when to reach for each. This
;;;; file is the same calls as Lisp verbs plus the same tools registered
;;;; against boundary contracts, so a model call and an eval form land in one
;;;; place.
;;;;
;;;; Corpus does the searching and the citation formatting: this cell sends
;;;; the query and hands back the text the server answered with, unchanged.
;;;; Nothing here is trimmed, summarised or re-sorted.

(in-package #:nodecode-corpus)

;;; --- the boundary: what a model call carries ---------------------------------------

(nlk:define-contract law-search-request
  (:doc "Search current US law -- statutes, regulations, ordinances -- through Corpus,
and get official text with a verbatim citation. Do this BEFORE answering any 'what
does the law say' question, instead of answering from memory. Pass jurisdiction when
the question is state- or city-scoped.")
  ("query" :text :required t
   :doc "the question, or the words to search for, e.g. \"food truck from a residential driveway\"")
  ("jurisdiction" :string
   :doc "the state, county or city the question is scoped to, e.g. \"Austin, TX\"; the section's jurisdiction answers when a call names none"))

(nlk:define-contract law-node-request
  (:doc "Read one provision's full official text through Corpus, by the citation a search
returned. Do this before you treat a result as the rule: a search snippet is not the
full text.")
  ("citation" :text :required t
   :doc "the citation exactly as a law_search result gave it")
  ("jurisdiction" :string
   :doc "the jurisdiction the citation sits under, when the citation alone is ambiguous"))

(nlk:define-contract corpus-call-request
  (:doc "Call any Corpus MCP tool by its own dotted name, with the arguments object the
server's schema wants -- the escape hatch for the rest of the catalog:
formation.requirements, formation.lookup_naics, formation.compare, formation.handoff,
formation.checkout, formation.payment_status, account.status, and any law.* call whose
members differ from the named tools'. (help :corpus) says what each takes.")
  ("tool" :text :required t
   :doc "the corpus tool's dotted name, e.g. formation.requirements")
  ("arguments" :object
   :doc "its arguments as one JSON object, e.g. {\"entityType\": \"llc\", \"state\": \"MS\"}; empty when the tool takes none"))

;;; --- the verbs --------------------------------------------------------------------

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object))))

(defun jurisdiction-of (asked)
  "ASKED when it says something, else the section's own default."
  (let ((asked (nlk:trimmed (or asked ""))))
    (if (plusp (length asked)) asked (nlk:trimmed (or (setting :jurisdiction) "")))))

(defun search (query &key jurisdiction)
  "Search current US law through Corpus. QUERY is the question or the words;
JURISDICTION scopes it and defaults to the section's. => the server's own text:
official results with the citations to quote verbatim."
  (let ((query (nlk:trimmed (or query "")))
        (jurisdiction (jurisdiction-of jurisdiction)))
    (unless (plusp (length query))
      (fail "search needs a query: give the question, or the words to search the law for"))
    (rpc-call "law.search"
              (nlk:json-object "query" query
                                :when (plusp (length jurisdiction)) "jurisdiction" jurisdiction))))

(defun provision (citation &key jurisdiction)
  "Read one provision's full official text through Corpus, by CITATION as a
search returned it. => the server's own text."
  (let ((citation (nlk:trimmed (or citation "")))
        (jurisdiction (jurisdiction-of jurisdiction)))
    (unless (plusp (length citation))
      (fail "provision needs the citation a search returned, verbatim"))
    (rpc-call "law.get_node"
              (nlk:json-object "citation" citation
                                :when (plusp (length jurisdiction)) "jurisdiction" jurisdiction))))

(defun coverage ()
  "Which jurisdictions and sources Corpus indexes: ask it rather than guess
whether one is covered. => the server's own text."
  (rpc-call "law.list_coverage" (nlk:make-json-object)))

(defun account ()
  "The credit balance and the limits on this key, free to ask: quote a limit or
a price only from what this answers. => the server's own text."
  (rpc-call "account.status" (nlk:make-json-object)))

(defun call-tool (tool &optional arguments)
  "Call the Corpus MCP tool TOOL -- a dotted name -- with ARGUMENTS, a JSON
object. => the server's own text."
  (let ((tool (nlk:trimmed (or tool ""))))
    (unless (plusp (length tool))
      (fail "call-tool needs the tool's name, e.g. formation.requirements"))
    (rpc-call tool (or arguments (nlk:make-json-object)))))

;;; --- the five tools the model calls ------------------------------------------------

(defun law-search-tool (request)
  "The law_search tool: the query a call carries, answered by the server."
  (search (law-search-request-query request)
          :jurisdiction (law-search-request-jurisdiction request)))

(defun law-node-tool (request)
  "The law_get_node tool: one provision's full text, by its citation."
  (provision (law-node-request-citation request)
             :jurisdiction (law-node-request-jurisdiction request)))

(defun law-coverage-tool (arguments)
  (declare (ignore arguments))
  (coverage))

(defun account-tool (arguments)
  (declare (ignore arguments))
  (account))

(defun corpus-call-tool (request)
  "The corpus_call tool: any Corpus tool, by name, with its own object."
  (call-tool (corpus-call-request-tool request)
              (corpus-call-request-arguments request)))

;;; --- the operator's own door -------------------------------------------------------

(defun run-slash (args session-id)
  "/corpus QUERY: the operator's own search, answered as the server answers the
model."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (zerop (length query))
        "corpus: type a question about current US law and it searches Corpus for the
         official text with citations. The cell also gives the model law_search,
         law_get_node, law_coverage, corpus_account and corpus_call (the formation
         intake)."
        (search query))))

;;; --- the words the model reads -----------------------------------------------------

(defparameter *primer*
  "Corpus is available through the nodecode-corpus cell: live US-law search with
citations, and a US company-formation intake, as five model tools and the same calls
in the CORPUS package from eval. No key is needed; a free key raises the research
limit, and formation is free.
  law_search {query, jurisdiction}
      Call it BEFORE answering what the law says, never from memory. Answers official
      text with a citation. Pass jurisdiction when the question is state- or
      city-scoped; the section's jurisdiction answers when a call names none.
  law_get_node {citation, jurisdiction}
      One provision's full official text, by the citation a search returned. Read it
      before you treat a result as the rule.
  law_coverage {}
      Which jurisdictions Corpus indexes. Ask instead of guessing, and say so when one
      is not covered.
  corpus_account {}
      Credits and limits, free to ask. Quote a limit or a price only from what it
      answers, never from memory.
  corpus_call {tool, arguments}
      Any other Corpus tool by its dotted name and its own JSON object: the formation
      family (formation.requirements, formation.lookup_naics, formation.compare,
      formation.handoff, formation.checkout, formation.payment_status) and any law.*
      call whose members differ from the named tools'.
Doctrine, from the two skills the original ships. In a law answer: search first,
quote the returned citation verbatim with its jurisdiction, read the provision before
relying on it, and say when a jurisdiction is not covered rather than filling the gap
from memory; on a quota or network failure say so instead of falling back to training
data. In a formation intake: ask for what formation.requirements lists a few answers
at a time, offer only what that response says is available, and quote a price only
from the live response. Never collect a Social Security Number (the tax-id add-on is
typed into the vendor's own page, never into chat), never promise to file, pay or
submit anything, and never call a payment-adjacent tool without the exact amount
confirmed in the turn you are answering. Treat referral codes as the vendor's
business: do not persist, pass back or recommend one. Corpus is not a law firm.
ERROR: CORPUS-ERROR names the problem."
  "What (help :corpus) answers while the cell runs.")
