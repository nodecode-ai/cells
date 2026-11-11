;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-jailbreaks
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:entries #:entry-key #:entry-vendor #:entry-target #:entry-kind #:entry-says
           #:entry-path #:entry-bytes #:entry-lines #:entry-sha #:entry-group
           #:entry-by-key #:entries-matching #:entry-row #:entry-block #:entries-text
           #:kinds #:kind-key #:kind-name #:kind-sentence #:kind-counts
           #:groups #:group-name #:group-paths #:group-says #:groups-text #:group-of
           #:vendors #:targets #:facts #:fact-question #:fact-answer #:facts-matching
           #:summary-text #:source-line #:source-block
           #:opening-text #:quote-text #:grep-text #:read-lines #:fetch-file
           #:+entries+ #:+kinds+ #:+groups+ #:+facts+ #:+archive+ #:+captured+))
