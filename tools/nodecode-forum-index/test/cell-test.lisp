;;;; cell-test.lisp --- the directory's data, its four verbs, and /forum-index.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network: the point of the cell is that the whole
;;;; directory answers with no request. What is proved is the data's own shape
;;;; (a name, a link and a known category on every entry; no two entries named
;;;; the same) and what a person is handed: a filtered listing, one forum whole
;;;; with its onion link, a search that crosses names and categories, and the
;;;; summary that says where the list came from.

(in-package #:nodecode.test)

(define-test-slice "forum-index" "FORUM-INDEX-CELL-" :start nodecode-forum-index:start-cell)

(define-cell-lifecycle-tests "forum-index"
  (:help :forum-index)
  (:command "forum-index")
  (:running (is (nle::find-tool "forum_index_list") "the list tool is registered")
            (is (nle::find-tool "forum_index_get") "the get tool is registered")
            (is (nle::find-tool "forum_index_search") "the search tool is registered")
            (is (nle::find-tool "forum_index_summary") "the summary tool is registered"))
  (:stopped (is (null (nle::find-tool "forum_index_list")) "stop unregisters the list tool")
            (is (null (nle::find-tool "forum_index_summary")) "stop unregisters the summary tool")))

;;; --- the data ---------------------------------------------------------------------

(deftest forum-index-cell-every-entry-carries-what-a-listing-needs ()
  (let ((forums (nodecode-forum-index:forums)))
    (is (= 47 (length forums)) "the list holds the 47 forums the poster published")
    (loop for forum in forums
          do (is (plusp (length (nodecode-forum-index:forum-name forum))) "every entry has a name")
             (is (plusp (length (nodecode-forum-index:forum-links forum))) "and at least one link, clearnet or onion")
             (is (member (nodecode-forum-index:forum-tier forum)
                         '(:established :rising :other))
                 "and one of the list's tiers"))))

(deftest forum-index-cell-no-two-entries-share-a-name ()
  (let ((names (mapcar #'nodecode-forum-index:forum-name (nodecode-forum-index:forums))))
    (is (= (length names) (length (remove-duplicates names :test #'string-equal)))
        "a name a person types names one forum")))

(deftest forum-index-cell-every-category-is-one-the-legend-explains ()
  (let ((legend (mapcar #'car nodecode-forum-index:+legend+)))
    (loop for forum in (nodecode-forum-index:forums)
          do (loop for category in (nodecode-forum-index:forum-categories forum)
                   do (is (member category legend :test #'string-equal)
                          "the legend explains every category the list uses")))
    (is (member "accounts" legend :test #'string-equal) "including the accounts market")))

(deftest forum-index-cell-onion-links-are-onion-links ()
  (loop for forum in (nodecode-forum-index:forums)
        for onion = (nodecode-forum-index:forum-onion forum)
        when onion
          do (is (cl:search ".onion" onion) "an onion entry carries an .onion address")))

(deftest forum-index-cell-the-tiers-count-what-the-poster-marked ()
  (is (= 4 (length (nodecode-forum-index:forums-matching :tier "established"))) "four established forums")
  (is (= 2 (length (nodecode-forum-index:forums-matching :tier "rising"))) "two with concerns raised")
  (is (= 41 (length (nodecode-forum-index:forums-matching :tier "other"))) "and the rest listed without a tier"))

;;; --- what a person gets -------------------------------------------------------------

(deftest forum-index-cell-a-category-narrows-to-the-forums-that-trade-in-it ()
  (let* ((rows (nodecode-forum-index:forums-matching :category "fraud"))
         (names (mapcar #'nodecode-forum-index:forum-name rows)))
    (is (member "Crd Crew" names :test #'string=) "a carding forum is a fraud forum")
    (is (member "Legit Carder" names :test #'string=) "as is Legit Carder")
    (is (not (member "Crackia" names :test #'string=)) "a leaks and hacking forum is not")
    (is (< (length rows) 47) "and the filter leaves fewer than the whole list")))

(deftest forum-index-cell-a-listing-says-its-count-and-where-it-came-from ()
  (with-cell-stop ((forum-index-start))
    (let ((block (nodecode-forum-index::list-tool
                           (nodecode-forum-index:decode-forum-index-list
                            (nlk:make-json-object "category" "fraud")))))
      (is (cl:search "fraud forums" block) "the heading names the filter")
      (is (cl:search "https://crdcrew.cc" block) "a row carries its link")
      (is (cl:search "Source: Dread /d/hacking" block) "and the list's source is said once"))))

(deftest forum-index-cell-one-forum-comes-whole-with-its-onion ()
  (with-cell-stop ((forum-index-start))
    (let ((block (nodecode-forum-index::get-tool
                           (nodecode-forum-index:decode-forum-index-get
                            (nlk:make-json-object "name" "Exploit")))))
      (is (cl:search "Exploit (fraud, leaks, malware)" block) "the name and the categories")
      (is (cl:search "https://exploit.in" block) "its clearnet link")
      (is (cl:search "exploitivzcm5dawzhe6c32bbylyggbjvh5dyvsvb5lkuz5ptmunkmqd.onion" block) "and its onion address")
      (is (cl:search "a registration fee to join" block) "with the poster's note"))))

(deftest forum-index-cell-a-partial-name-still-finds-one ()
  (with-cell-stop ((forum-index-start))
    (let ((block (nodecode-forum-index::get-tool
                           (nodecode-forum-index:decode-forum-index-get
                            (nlk:make-json-object "name" "lolz")))))
      (is (cl:search "Lolz Forum" block) "a lowercase fragment names the forum")
      (is (cl:search "https://lolz.live" block) "and hands back its link")
      (is (cl:search "accounts market" block) "and what it is known for"))))

(deftest forum-index-cell-a-name-that-is-not-there-says-so-and-shows-the-list ()
  (with-cell-stop ((forum-index-start))
    (let ((block (nodecode-forum-index::get-tool
                           (nodecode-forum-index:decode-forum-index-get
                            (nlk:make-json-object "name" "nonsuch")))))
      (is (cl:search "no forum in the list is named" block) "the miss is said plainly")
      (is (cl:search "what the list has" block) "and the whole list is shown, so the next ask is easy"))))

(deftest forum-index-cell-a-search-crosses-names-categories-and-notes ()
  (let ((by-name (nodecode-forum-index:forums-matching :text "carder"))
        (by-note (nodecode-forum-index:forums-matching :text "compromised")))
    (is (member "Legit Carder" (mapcar #'nodecode-forum-index:forum-name by-name) :test #'string=)
        "a word in a name finds it")
    (is (member "Darkforums" (mapcar #'nodecode-forum-index:forum-name by-note) :test #'string=)
        "a word in a note finds it too")
    (is (null (nodecode-forum-index:forums-matching :text "zzzznothing"))
        "and a word in nothing finds nothing")))

(deftest forum-index-cell-the-summary-says-how-many-and-from-where ()
  (with-cell-stop ((forum-index-start))
    (let ((block (nodecode-forum-index::summary-tool nil)))
      (is (cl:search "47 forums" block) "the count")
      (is (cl:search "4 of them established and 2 with concerns raised" block) "the tiers")
      (is (cl:search "by category:" block) "and the categories, so the legend is one ask away")
      (is (cl:search "Source: Dread" block) "and the source"))))

;;; --- /forum-index -------------------------------------------------------------------

(deftest forum-index-cell-the-command-answers-all-four-ways ()
  (with-cell-stop ((forum-index-start))
    (let ((bare (nodecode-forum-index::run-slash nil nil))
          (legend (nodecode-forum-index::run-slash "categories" nil))
          (one (nodecode-forum-index::run-slash "fraud" nil))
          (words (nodecode-forum-index::run-slash "carder" nil)))
      (is (cl:search "47 forums" bare) "no arguments: the summary")
      (is (cl:search "usage: /forum-index" bare) "and how to ask for more")
      (is (cl:search "high-quality discussion and services about malware" legend) "categories: the legend")
      (is (cl:search "fraud forums" one) "a category: the listing")
      (is (cl:search "forums mentioning \"carder\"" words) "anything else: a search"))))

(deftest forum-index-cell-the-snapshot-says-when-it-was-read ()
  (let ((line (nodecode-forum-index:source-line)))
    (is (cl:search "Read 11 Nov 2026" line) "the reading is dated, not just the post")
    (is (cl:search "no forum on it is vouched for" line) "and the vouch is still disclaimed")))

(deftest forum-index-cell-the-dangerous-section-is-a-named-gap-not-a-silent-none ()
  (with-cell-stop ((forum-index-start))
    (let ((block (nodecode-forum-index::list-text :tier "dangerous")))
      (is (cl:search "no forum in the list matches that" block) "the poster's tier exists and has no rows here")
      (is (cl:search "below the fold" block) "and the reason is said, rather than read as none are dangerous")
      (is (cl:search "Dangerous" block) "naming the section it is missing"))))
