;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-tabella)

(nlk:define-contract read-document
  (:doc "Read a document's text. Office formats (DOCX, ODT, EPUB, PPTX) and marked-up files (HTML, LaTeX, RST, Markdown) go through pandoc and come back as Markdown; a PDF goes through pdftotext; a plain text file is read directly. Reading an image needs OCR, which this cell does not carry.")
  ("path" :text :required t
   :doc "the file to read, absolute or ~/..." )
  ("max_chars" :integer
   :doc "cap on the characters answered; when omitted, the cell's max_chars setting"))

(nlk:define-contract document-metadata
  (:doc "What the filesystem knows about one document: its path, its format (the extension), its size in bytes and when it last changed. Cheap, and no program runs.")
  ("path" :text :required t
   :doc "the file to look at, absolute or ~/..."))

(nlk:define-contract convert-document
  (:doc "Convert a document to another format with pandoc: markdown to DOCX, PDF, HTML, ODT or EPUB, or any pair of built-in pandoc formats. A PDF as input is read with pdftotext first, because pandoc writes PDFs but cannot read them. A PDF as output needs a LaTeX engine on the machine.")
  ("input_path" :text :required t
   :doc "the document to convert, absolute or ~/...")
  ("to_format" :text :required t
   :doc "the target format as a built-in pandoc writer name: pdf, docx, html, odt, epub, markdown, rst, latex")
  ("from_format" :string :default ""
   :doc "optional source format hint; when empty it is read from the file's extension, as pandoc does")
  ("output_path" :string :default ""
   :doc "where to write the result; when empty it goes to the cell's output_dir, under category when one is given")
  ("extra_args" :array :of :string
   :doc "extra pandoc flags: only --toc, --number-sections and -Vkey=value (a --filter or --lua-filter would run code)")
  ("category" :string :default ""
   :doc "a subfolder of output_dir to write into, such as reports"))

(nlk:define-contract create-document
  (:doc "Create a document from content and write it with pandoc: markdown, DOCX, PDF, HTML, ODT, EPUB, RST or LaTeX. Spreadsheets and slide decks are not written here. The content is a Markdown (or HTML) string, or the block list the original tool took: headings, paragraphs, bullets, quotes, code and tables.")
  ("format" :text :slot document-format :required t
   :doc "the format to write: markdown, docx, pdf, html, odt, epub, rst or latex")
  ("content" :any :required t
   :doc "the document as a Markdown or HTML string, or a list of blocks: each an object with type (heading, paragraph, bullets, quote, code, table, page_break) and its own members")
  ("title" :string :default ""
   :doc "a title for the document, written as its first heading")
  ("output_path" :string :default ""
   :doc "where to write the result; when empty a name is made from the title or the time, under output_dir")
  ("category" :string :default ""
   :doc "a subfolder of output_dir to write into, such as reports"))
