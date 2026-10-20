;;;; markdown-test.lisp --- Markdown as WhatsApp spells it, pure.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest channel-whatsapp-markdown-is-respelled ()
  (flet ((wa (text) (ncw:whatsapp-text text)))
    (is (equal "*Verdict:* it _works_ and *really* ~fails~" (wa "**Verdict:** it *works* and __really__ ~~fails~~"))
        "bold, italics and strikethrough take WhatsApp's marks")
    (is (equal "*Plan*" (wa "## Plan")) "a heading is a bold line")
    (is (equal "see docs (https://x.com/a) or https://y.org" (wa "see [docs](https://x.com/a) or <https://y.org>"))
        "a link is its words with the address beside them")
    (is (equal "https://z.io" (wa "[https://z.io](https://z.io)")) "a bare link is its address once")
    (is (equal "- one" (wa "* one")) "a star list item is a dash one")
    (is (equal "keep `**this**` as is" (wa "keep `**this**` as is")) "inline code is untouched")
    (is (equal (format nil "```~%(* 2 **3**)~%```") (wa (format nil "```lisp~%(* 2 **3**)~%```")))
        "a fence keeps its code and drops its language")
    (is (equal "snake_case_name and 2*3*4" (wa "snake_case_name and 2*3*4")) "words are not marks")
    (is (equal "a literal * star" (wa "a literal \\* star")) "an escape is its character")
    (is (equal (format nil "```~%col        b~%---------  -~%long cell  2~%```")
               (wa (format nil "| col | b |~%|---|---|~%| long cell | 2 |")))
        "a table is a monospace block, its columns padded")))
