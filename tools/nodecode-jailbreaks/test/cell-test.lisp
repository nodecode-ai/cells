;;;; cell-test.lisp --- the catalogue, the five verbs, and /jailbreaks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The catalogue is proved offline, because it is offline: every entry with a
;;;; target, a kind, a size and a digest; the duplicated files found by digest;
;;;; the kinds adding up to the catalogue. The reading path is proved against a
;;;; five-line fixture, so the slice needs no network at all.

(in-package #:nodecode.test)

(define-test-slice "jailbreaks" "JAILBREAKS-CELL-" :start nodecode-jailbreaks:start-cell)

(define-cell-lifecycle-tests "jailbreaks"
  (:help :jailbreaks)
  (:command "jailbreaks")
  (:running (is (nle::find-tool "jailbreaks_list") "the catalogue tool is registered")
            (is (nle::find-tool "jailbreaks_get") "the get tool is registered")
            (is (nle::find-tool "jailbreaks_search") "the search tool is registered")
            (is (nle::find-tool "jailbreaks_grep") "the grep tool is registered")
            (is (nle::find-tool "jailbreaks_quote") "the quote tool is registered"))
  (:stopped (is (null (nle::find-tool "jailbreaks_list")) "stop unregisters the catalogue tool")
            (is (null (nle::find-tool "jailbreaks_quote")) "stop unregisters the quote tool")))

;;; --- the catalogue -----------------------------------------------------------------

(deftest jailbreaks-cell-the-catalogue-holds-prompts-with-their-facts ()
  (let ((entries (nodecode-jailbreaks:entries)))
    (is (= 51 (length entries)) "51 prompt files in the collection")
    (loop for entry in entries
          do (is (plusp (length (nodecode-jailbreaks:entry-key entry))) "every entry has a key")
             (is (plusp (length (nodecode-jailbreaks:entry-vendor entry))) "and a folder")
             (is (plusp (length (nodecode-jailbreaks:entry-target entry))) "and a target")
             (is (plusp (length (nodecode-jailbreaks:entry-says entry))) "and a sentence saying what it does")
             (is (plusp (length (nodecode-jailbreaks:entry-path entry))) "and a path")
             (is (plusp (nodecode-jailbreaks:entry-bytes entry)) "and a size")
             (is (plusp (nodecode-jailbreaks:entry-lines entry)) "and a line count")
             (is (= 64 (length (nodecode-jailbreaks:entry-sha entry))) "and a sha256"))))

(deftest jailbreaks-cell-no-two-entries-share-a-key ()
  (let ((keys (mapcar #'nodecode-jailbreaks:entry-key (nodecode-jailbreaks:entries))))
    (is (= (length keys) (length (remove-duplicates keys :test #'string-equal))) "a key a person types names one entry")))

(deftest jailbreaks-cell-every-kind-is-one-the-data-declares ()
  (let ((declared (nodecode-jailbreaks:kinds)))
    (loop for entry in (nodecode-jailbreaks:entries)
          do (is (member (nodecode-jailbreaks:entry-kind entry) declared) "every entry is of a kind the cell can describe"))
    (is (every (lambda (kind) (plusp (length (nodecode-jailbreaks:kind-sentence kind)))) declared) "and every kind has its sentence")))

(deftest jailbreaks-cell-the-kinds-add-up-to-the-catalogue ()
  (is (= (length (nodecode-jailbreaks:entries))
         (loop for (nil . n) in (nodecode-jailbreaks:kind-counts) sum n)) "the counts by kind are the whole catalogue, with nothing dropped"))

(deftest jailbreaks-cell-the-collection-is-filed-in-nineteen-folders ()
  (let ((vendors (nodecode-jailbreaks:vendors)))
    (is (= 19 (length vendors)) "nineteen folders")
    (is (member "Anthropic" vendors :test #'string=) "Anthropic among them")
    (is (member "0x8D jailbreaks" vendors :test #'string=) "and the authors' own folder")))

(deftest jailbreaks-cell-a-recovered-system-prompt-is-not-a-persona ()
  (let ((system (nodecode-jailbreaks:entries-matching :kind "system-prompt")))
    (is (= 8 (length system)) "eight files are a model's own prompt")
    (is (find "qwen-3.7" system :test #'string= :key #'nodecode-jailbreaks:entry-key) "Qwen 3.7 among them")
    (is (not (find "jack" system :test #'string= :key #'nodecode-jailbreaks:entry-key)) "and the Jack register is not one of them")))

;;; --- the files that are the same file ----------------------------------------------

(deftest jailbreaks-cell-the-digest-finds-files-a-reader-would-not ()
  (let ((groups (nodecode-jailbreaks:groups)))
    (is (= 2 (length groups)) "two sets of identical files in the collection")
    (let ((jack (find "jack" groups :test #'string= :key #'nodecode-jailbreaks:group-name)))
      (is jack "the Jack register is one")
      (is (= 4 (length (nodecode-jailbreaks:group-paths jack))) "four files, one prompt")
      (is (string= "0x8D jailbreaks/chatgpt-free-tier.md" (first (nodecode-jailbreaks:group-paths jack))) "filed for ChatGPT first"))))

(deftest jailbreaks-cell-the-group-and-the-digest-agree ()
  (let* ((jack (find "jack" (nodecode-jailbreaks:groups) :test #'string= :key #'nodecode-jailbreaks:group-name))
         (entries (mapcar (lambda (path) (find path (nodecode-jailbreaks:entries)
                                               :test #'string= :key #'nodecode-jailbreaks:entry-path))
                          (nodecode-jailbreaks:group-paths jack))))
    (is (every #'identity entries) "every path in the group is an entry")
    (is (= 1 (length (remove-duplicates (mapcar #'nodecode-jailbreaks:entry-sha entries) :test #'string=))) "and every one of them carries the same digest")))

(deftest jailbreaks-cell-a-file-outside-a-group-says-so ()
  (is (string= "jack" (nodecode-jailbreaks:group-of "0x8D jailbreaks/grok-fast.md")) "a member names its group")
  (is (null (nodecode-jailbreaks:group-of "Anthropic/opus v1.md")) "and a file of its own names none"))

;;; --- naming one ----------------------------------------------------------------

(deftest jailbreaks-cell-a-key-finds-its-entry-or-nothing ()
  (let ((entry (nodecode-jailbreaks:entry-by-key "jack")))
    (is entry "an exact key finds its entry")
    (is (string= "0x8D jailbreaks/chatgpt-free-tier.md" (nodecode-jailbreaks:entry-path entry)) "and it is the Jack register"))
  (is (nodecode-jailbreaks:entry-by-key "zcode") "a fragment finds one too")
  (is (null (nodecode-jailbreaks:entry-by-key "no-such-prompt")) "a word the collection lacks is nil"))

(deftest jailbreaks-cell-a-block-says-what-the-entry-is ()
  (let* ((entry (nodecode-jailbreaks:entry-by-key "jack"))
         (block (nodecode-jailbreaks:entry-block entry)))
    (is (cl:search "kind:    persona" block) "the block names the kind")
    (is (cl:search "byte-identical" block) "and says the file is one of four")
    (is (cl:search "302 lines" block) "with the line count the digest was taken from")))

;;; --- what a person gets -------------------------------------------------------------

(deftest jailbreaks-cell-a-listing-names-every-prompt-it-shows ()
  (let ((text (nodecode-jailbreaks:entries-text (nodecode-jailbreaks:entries))))
    (loop for entry in (nodecode-jailbreaks:entries)
          do (is (cl:search (nodecode-jailbreaks:entry-key entry) text) "the listing carries every key it can be quoted by"))))

(deftest jailbreaks-cell-a-folder-narrows-to-its-own-files ()
  (let ((names (mapcar #'nodecode-jailbreaks:entry-key
                       (nodecode-jailbreaks:entries-matching :vendor "Anthropic"))))
    (is (= 4 (length names)) "Anthropic's four prompts")
    (is (member "opus-v1" names :test #'string=) "Opus among them")
    (is (not (member "jack" names :test #'string=)) "and the 0x8D folder's are not")))

(deftest jailbreaks-cell-a-search-crosses-what-an-entry-says ()
  (let ((names (mapcar #'nodecode-jailbreaks:entry-key
                       (nodecode-jailbreaks:entries-matching :text "unrestricted"))))
    (is names "a word only the description carries still finds its entry")
    (is (member "master" names :test #'string=) "the UNRESTRICTED SANDBOX file among them")))

(deftest jailbreaks-cell-the-summary-counts-what-the-catalogue-holds ()
  (let ((text (nodecode-jailbreaks:summary-text)))
    (is (cl:search "51 prompt files" text) "the summary gives the count")
    (is (cl:search "19 folders" text) "and the folders")
    (is (cl:search "persona" text) "and the kinds, by name")))

(deftest jailbreaks-cell-every-answer-says-where-it-came-from ()
  (let ((line (nodecode-jailbreaks:source-line)))
    (is (cl:search "togg53192-cmd/jailbreaks" line) "the provenance names the collection")
    (is (cl:search "12 November 2026" line) "and the day it was read")
    (is (cl:search "No licence file" line) "and that it carries no licence")))

(deftest jailbreaks-cell-a-space-in-a-path-is-escaped-for-the-url ()
  (let ((entry (nodecode-jailbreaks:entry-by-key "bread")))
    (is (cl:search "%20" (nodecode-jailbreaks::raw-url entry)) "a path with a space is fetched, not refused")
    (is (not (cl:search " " (nodecode-jailbreaks::raw-url entry))) "and the URL carries no bare space")))

;;; --- a prompt's own lines, on a fixture so the slice needs no network --------------

(deftest jailbreaks-cell-a-line-range-is-parsed-or-refused ()
  (is (equal (cons 12 12) (nodecode-jailbreaks::line-range "12")) "a line")
  (is (equal (cons 1 40) (nodecode-jailbreaks::line-range "1-40")) "a range")
  (is (null (nodecode-jailbreaks::line-range "persona")) "and a word is not a range"))

(deftest jailbreaks-cell-a-range-reads-the-numbered-lines-of-a-file ()
  (let ((fixture (merge-pathnames "jailbreaks-fixture.md" (uiop:temporary-directory))))
    (with-open-file (out fixture :direction :output :if-exists :supersede)
      (format out "### REGISTER~%two~%three~%four~%five~%"))
    (unwind-protect
         (let ((text (nodecode-jailbreaks::read-lines 2 3 :path fixture)))
           (is (cl:search "two" text) "the range starts where it was asked")
           (is (cl:search "three" text) "and ends there")
           (is (not (cl:search "four" text)) "and carries no line past it")
           (is (cl:search "2  two" text) "with the line number beside each line"))
      (ignore-errors (delete-file fixture)))))

(deftest jailbreaks-cell-a-quoted-word-comes-back-numbered-and-named ()
  (let ((fixture (merge-pathnames "jailbreaks-fixture-2.md" (uiop:temporary-directory))))
    (with-open-file (out fixture :direction :output :if-exists :supersede)
      (format out "### REGISTER~%You are Jack, the operating register.~%three~%"))
    (unwind-protect
         (let ((text (nodecode-jailbreaks:quote-text :text "register" :path fixture
                                                     :entry "jack")))
           (is (cl:search "You are Jack" text) "the line carrying the word comes back")
           (is (cl:search "2  You" text) "numbered by where it stands")
           (is (cl:search "jack" text) "and the answer says which prompt it was"))
      (ignore-errors (delete-file fixture)))))

(deftest jailbreaks-cell-an-opening-quotes-without-reading-everything ()
  (let ((fixture (merge-pathnames "jailbreaks-fixture-3.md" (uiop:temporary-directory))))
    (with-open-file (out fixture :direction :output :if-exists :supersede)
      (format out "### REGISTER~%~%You are Jack.~%~%Third line.~%"))
    (unwind-protect
         (let* ((entry (nodecode-jailbreaks:entry-by-key "jack"))
                (text (nodecode-jailbreaks::opening-text entry :lines 2 :path fixture)))
           (is (cl:search "### REGISTER" text) "the opening starts at the first line that carries something")
           (is (cl:search "You are Jack" text) "and the blank line is skipped, not counted")
           (is (cl:search "jailbreaks_quote reads the rest" text) "and it says where the rest is"))
      (ignore-errors (delete-file fixture)))))

;;; --- /jailbreaks ------------------------------------------------------------------

(deftest jailbreaks-cell-the-command-answers-its-shapes ()
  (let ((bare (nodecode-jailbreaks::run-slash "" nil))
        (kinds (nodecode-jailbreaks::run-slash "kinds" nil))
        (persona (nodecode-jailbreaks::run-slash "persona" nil))
        (folder (nodecode-jailbreaks::run-slash "Anthropic" nil))
        (one (nodecode-jailbreaks::run-slash "jack" nil)))
    (is (cl:search "persona" bare) "bare, the collection by kind")
    (is (cl:search "a named voice or a posture" kinds) "kinds gives the legend")
    (is (cl:search "The Jack register" persona) "a kind narrows to its entries")
    (is (cl:search "opus-v1" folder) "a folder narrows to its own")
    (is (cl:search "kind:    persona" one) "a key gives one entry, whole, even with no network to read it")))
