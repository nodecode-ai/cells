;;;; cell-test.lisp --- the table's data, its five verbs, and /attack-frameworks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network. What is proved is the data's own shape
;;;; (a repository and a licence on every entry, no two entries named alike),
;;;; that the table is big enough to match the post's "more than 60,000 stars",
;;;; and what a person is handed: a listing, one framework whole with its hunt
;;;; strings, the hunt table on its own, and a search that crosses descriptions.

(in-package #:nodecode.test)

(define-test-slice "attack-frameworks" "ATTACK-FRAMEWORKS-CELL-" :start nodecode-attack-frameworks:start-cell)

(define-cell-lifecycle-tests "attack-frameworks"
  (:help :attack-frameworks)
  (:command "attack-frameworks")
  (:running (is (nle::find-tool "attack_frameworks_list") "the list tool is registered")
            (is (nle::find-tool "attack_frameworks_hunt") "the hunt tool is registered")
            (is (nle::find-tool "attack_frameworks_get") "the get tool is registered")
            (is (nle::find-tool "attack_frameworks_search") "the search tool is registered")
            (is (nle::find-tool "attack_frameworks_summary") "the summary tool is registered"))
  (:stopped (is (null (nle::find-tool "attack_frameworks_list")) "stop unregisters the list tool")
            (is (null (nle::find-tool "attack_frameworks_summary")) "stop unregisters the summary tool")))

;;; --- the data ---------------------------------------------------------------------

(deftest attack-frameworks-cell-every-entry-carries-what-a-row-needs ()
  (let ((rows (nodecode-attack-frameworks:frameworks)))
    (is (= 5 (length rows)) "the table holds the post's five frameworks")
    (loop for f in rows
          do (is (plusp (length (nodecode-attack-frameworks:framework-name f))) "every entry has a name")
             (is (cl:search "https://github.com/" (nodecode-attack-frameworks:framework-url f)) "and a repository a person can open")
             (is (member (nodecode-attack-frameworks:framework-licence f)
                         '("MIT" "Apache-2.0" "AGPL-3.0") :test #'string=) "and a licence")
             (is (plusp (nodecode-attack-frameworks:framework-stars f)) "and a star count"))))

(deftest attack-frameworks-cell-no-two-entries-share-a-name ()
  (let ((names (mapcar #'nodecode-attack-frameworks:framework-name
                       (nodecode-attack-frameworks:frameworks))))
    (is (= (length names) (length (remove-duplicates names :test #'string-equal))) "a name a person types names one framework")))

(deftest attack-frameworks-cell-the-table-is-as-starred-as-the-post-says ()
  (is (> (nodecode-attack-frameworks:star-total) 60000) "the post's 'more than 60,000 GitHub stars' is what the table's own numbers give"))

(deftest attack-frameworks-cell-every-entry-says-what-it-leaves-behind ()
  (loop for f in (nodecode-attack-frameworks:frameworks)
        do (is (plusp (length (nodecode-attack-frameworks:framework-hunt f))) "every framework comes with at least one hunt default")
           (is (plusp (length (nodecode-attack-frameworks:framework-evidence f))) "and the field sighting that makes it worth hunting")))

(deftest attack-frameworks-cell-the-hunt-kinds-are-the-ones-a-log-has ()
  (let ((kinds (remove-duplicates
                (loop for f in (nodecode-attack-frameworks:frameworks)
                      append (mapcar #'car (nodecode-attack-frameworks:framework-hunt f)))
                :test #'string=)))
    (is (member "user-agent" kinds :test #'string=) "a User-Agent is huntable")
    (is (member "network" kinds :test #'string=) "so is a connection")
    (is (member "persistence" kinds :test #'string=) "and a persistence name")))

;;; --- what a person gets -------------------------------------------------------------

(deftest attack-frameworks-cell-a-listing-names-all-five-with-their-links ()
  (with-cell-stop ((attack-frameworks-start))
    (let ((block (nodecode-attack-frameworks::list-tool
                  (nodecode-attack-frameworks:decode-attack-frameworks-list
                   (nlk:make-json-object)))))
      (loop for name in '("ARTEX" "Cairn" "CyberStrikeAI" "HexStrike" "PentAGI")
            do (is (cl:search name block) "every framework is listed"))
      (is (cl:search "https://github.com/vxcontrol/pentagi" block) "with its repository")
      (is (cl:search "Source: DFIR Radar" block) "and the source is said once"))))

(deftest attack-frameworks-cell-one-framework-comes-whole-with-its-hunt-strings ()
  (with-cell-stop ((attack-frameworks-start))
    (let ((block (nodecode-attack-frameworks::get-tool
                  (nodecode-attack-frameworks:decode-attack-frameworks-get
                   (nlk:make-json-object "name" "PentAGI")))))
      (is (cl:search "PentAGI -- vxcontrol/pentagi" block) "the name and the repository")
      (is (cl:search "ASK_USER" block) "how it fails to ask before acting")
      (is (cl:search "28000 to 29999" block) "the port range it leaves open")
      (is (cl:search "update.pentagi[.]com" block) "and its check-in host")
      (is (cl:search "incident-response engagements" block) "with the field sighting"))))

(deftest attack-frameworks-cell-a-fragment-of-a-name-still-finds-one ()
  (with-cell-stop ((attack-frameworks-start))
    (let ((block (nodecode-attack-frameworks::get-tool
                  (nodecode-attack-frameworks:decode-attack-frameworks-get
                   (nlk:make-json-object "name" "pent")))))
      (is (cl:search "PentAGI" block) "a lowercase fragment names the framework")
      (is (cl:search "34,149 stars" block) "and the row carries its numbers"))))

(deftest attack-frameworks-cell-a-name-that-is-not-there-says-so-and-shows-the-table ()
  (with-cell-stop ((attack-frameworks-start))
    (let ((block (nodecode-attack-frameworks::get-tool
                  (nodecode-attack-frameworks:decode-attack-frameworks-get
                   (nlk:make-json-object "name" "nonsuch")))))
      (is (cl:search "no framework in the table is named" block) "the miss is said plainly")
      (is (cl:search "PentAGI" block) "and the five are shown, so the next ask is easy"))))

(deftest attack-frameworks-cell-the-hunt-verb-answers-the-strings-alone ()
  (with-cell-stop ((attack-frameworks-start))
    (let ((all (nodecode-attack-frameworks::hunt-tool
                (nodecode-attack-frameworks:decode-attack-frameworks-hunt
                 (nlk:make-json-object))))
          (one (nodecode-attack-frameworks::hunt-tool
                (nodecode-attack-frameworks:decode-attack-frameworks-hunt
                 (nlk:make-json-object "framework" "HexStrike")))))
      (is (cl:search "HexStrike-HTTP-Framework/1.0" all) "every framework's defaults are there")
      (is (cl:search "artex-enrich/1.0" all) "including ARTEX's User-Agent")
      (is (not (cl:search "artex-enrich/1.0" one)) "narrowed, another framework's strings are not")
      (is (cl:search "<hexstrikeXSSTest123>" one) "and the asked-for one's are"))))

(deftest attack-frameworks-cell-a-search-crosses-description-and-hunt-strings ()
  (is (member "HexStrike"
              (mapcar #'nodecode-attack-frameworks:framework-name
                      (nodecode-attack-frameworks:frameworks-matching :text "8888"))
              :test #'string=) "a port in a hunt string finds its framework")
  (is (member "ARTEX"
              (mapcar #'nodecode-attack-frameworks:framework-name
                      (nodecode-attack-frameworks:frameworks-matching :text "Storm-0940"))
              :test #'string=) "as does the actor a sighting is attributed to")
  (is (null (nodecode-attack-frameworks:frameworks-matching :text "zzzznothing")) "and nothing finds nothing"))

(deftest attack-frameworks-cell-a-licence-narrows-to-the-ones-you-may-not-reuse ()
  (let ((agpl (mapcar #'nodecode-attack-frameworks:framework-name
                      (nodecode-attack-frameworks:frameworks-matching :licence "agpl")))
        (mit (mapcar #'nodecode-attack-frameworks:framework-name
                     (nodecode-attack-frameworks:frameworks-matching :licence "mit"))))
    (is (= 2 (length agpl)) "two of the five are AGPL-3.0")
    (is (member "ARTEX" agpl :test #'string=) "ARTEX among them")
    (is (member "PentAGI" mit :test #'string=) "and the MIT ones are separate")))

(deftest attack-frameworks-cell-the-summary-says-how-many-and-what-it-means ()
  (with-cell-stop ((attack-frameworks-start))
    (let ((block (nodecode-attack-frameworks::summary-tool nil)))
      (is (cl:search "5 frameworks" block) "the count")
      (is (cl:search "None of the five enforces authorization" block) "the post's finding")
      (is (cl:search "60 to 90 minutes" block) "and how long a revoked token outlives the revoke")
      (is (cl:search "Source: DFIR Radar" block) "with the source"))))

;;; --- /attack-frameworks -------------------------------------------------------------

(deftest attack-frameworks-cell-the-command-answers-all-five-ways ()
  (with-cell-stop ((attack-frameworks-start))
    (let ((bare (nodecode-attack-frameworks::run-slash nil nil))
          (hunt (nodecode-attack-frameworks::run-slash "hunt" nil))
          (one (nodecode-attack-frameworks::run-slash "Cairn" nil))
          (licence (nodecode-attack-frameworks::run-slash "agpl" nil))
          (words (nodecode-attack-frameworks::run-slash "8888" nil)))
      (is (cl:search "5 frameworks" bare) "no arguments: the summary")
      (is (cl:search "usage: /attack-frameworks" bare) "and how to ask for more")
      (is (cl:search "HexStrike-BrowserAgent/1.0" hunt) "hunt: the strings, one line each")
      (is (cl:search "refusing worker" one) "a name: one framework whole")
      (is (cl:search "agpl-licensed frameworks" licence) "a licence: the listing")
      (is (cl:search "frameworks mentioning \"8888\"" words) "anything else: a search"))))

(deftest attack-frameworks-cell-the-source-says-when-it-was-read ()
  (let ((line (nodecode-attack-frameworks:source-line)))
    (is (cl:search "read 14 Nov 2026" line) "the reading is dated, not just the post")
    (is (cl:search "a lead, not as a signature" line) "and a hunt string is not sold as a signature")))
