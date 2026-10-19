;;;; markdown-test.lisp --- Markdown as Matrix's HTML.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest channel-matrix-markdown-draws-what-a-model-writes ()
  (is (equal "<p>a <strong>bold</strong> and <em>soft</em> and <del>gone</del> word</p>"
             (ncm:matrix-html "a **bold** and *soft* and ~~gone~~ word")))
  (is (equal "<p>line one<br>line two</p><p>next</p>"
             (ncm:matrix-html (format nil "line one~%line two~%~%next")))
      "a line break stays a line break; a blank line parts paragraphs")
  (is (equal "<h2>Title</h2><ul><li>one</li><li><code>two</code></li></ul>"
             (ncm:matrix-html (format nil "## Title~%- one~%- `two`"))))
  (is (equal "<ol start=\"3\"><li>three</li><li>four</li></ol>"
             (ncm:matrix-html (format nil "3. three~%4. four"))))
  (is (equal "<pre><code class=\"language-lisp\">(a &lt;b&gt; *c*)</code></pre>"
             (ncm:matrix-html (format nil "```lisp~%(a <b> *c*)~%```")))
      "code passes as written, escaped, no emphasis inside")
  (is (equal "<table><thead><tr><th>a</th><th>b</th></tr></thead><tbody><tr><td>1</td><td><strong>2</strong></td></tr></tbody></table>"
             (ncm:matrix-html (format nil "| a | b |~%|---|---|~%| 1 | **2** |"))))
  (is (equal "<blockquote><p>quoted</p></blockquote>" (ncm:matrix-html "> quoted")))
  (is (equal "<p><a href=\"https://x.org/a_b?c=1&amp;d=2\">the link</a></p>"
             (ncm:matrix-html "[the link](https://x.org/a_b?c=1&d=2)"))
      "a link is drawn; an underscore inside its address is no emphasis")
  (is (equal "<p>&lt;script&gt;x&lt;/script&gt; [no](javascript:alert(1)) snake_case_word</p>"
             (ncm:matrix-html "<script>x</script> [no](javascript:alert(1)) snake_case_word"))
      "nothing a model writes opens a tag, and only web and mail links are drawn")
  (is (equal "<hr>" (ncm:matrix-html "---"))))
