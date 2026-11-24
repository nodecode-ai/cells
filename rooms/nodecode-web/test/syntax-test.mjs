// syntax-test.mjs -- page/syntax.js on its own, in node: each language's
// marks, the state a line hands down, the text kept letter for letter, and a
// painter fed edit after edit drawing what a fresh one draws. Run by
// syntax-test.lisp as `node syntax-test.mjs <page/syntax.js>`; its last line
// is "SYNTAX <n> passed, <m> failed".
import { pathToFileURL } from "node:url";

const { languageOf, paintLine, paintFor, LANGUAGE_NAMES } = await import(pathToFileURL(process.argv[2]).href);
let passed = 0;
let failed = 0;
const check = (ok, what) => {
  if (ok) passed++;
  else { failed++; console.log(`FAIL ${what}`); }
};

// Every mark of a painted text as "class:text", the text unescaped.
const unescape = (html) => html.replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&amp;/g, "&");
const marks = (html) => [...html.matchAll(/<span class="sx-(\w)">([^<]*)<\/span>/g)].map((m) => `${m[1]}:${unescape(m[2])}`);
const paint = (lang, text) => { const p = paintFor(lang); p.set(text); return p.html(0, p.count); };
const plain = (html) => unescape(html.replace(/<[^>]*>/g, ""));

// --- which language a name says ------------------------------------------------
for (const [name, want, text] of [
  ["src/main.js", "js"], ["App.tsx", "js"], ["x.mjs", "js"], ["a.py", "python"], ["main.go", "go"], ["lib.rs", "rust"], ["x.c", "c"],
  ["x.hpp", "c"], ["A.java", "java"], ["b.kt", "java"], ["c.cs", "csharp"], ["d.swift", "swift"], ["e.php", "php"], ["f.rb", "ruby"],
  ["g.lua", "lua"], ["h.zig", "zig"], ["run.sh", "shell"], ["q.sql", "sql"], ["m.proto", "proto"], ["main.tf", "hcl"],
  ["cell.lisp", "lisp"], ["x.asd", "lisp"], ["init.el", "lisp"], ["core.clj", "lisp"], ["index.html", "markup"], ["icon.svg", "markup"],
  ["app.css", "css"], ["a.scss", "css"], ["README.md", "markdown"], ["ci.yml", "yaml"], ["Cargo.toml", "ini"], [".env", "ini"],
  [".env.local", "ini"], ["fix.patch", "diff"], ["Makefile", "make"], ["Dockerfile", "docker"], ["Dockerfile.dev", "docker"],
  ["package.json", "json"], ["tsconfig.json", "json"], [".bashrc", "shell"], ["notes.txt", ""], ["LICENSE", ""], ["photo.png", ""],
  ["tool", "shell", "#!/usr/bin/env bash\necho"], ["serve", "python", "#!/usr/bin/python3\n"], ["cli", "js", "#!/usr/bin/env node\n"],
  ["repl", "lisp", "#!/usr/bin/env -S sbcl --script\n"], ["data", "", "plain words\n"],
]) check(languageOf(name, text) === want, `languageOf(${name}) = ${languageOf(name, text)}, wanted ${want}`);
for (const lang of Object.values({ ...Object.fromEntries(Object.keys(LANGUAGE_NAMES).map((k) => [k, k])) })) check(paintFor(lang), `${lang} has a painter`);
check(paintFor("") === null && paintFor("cobol") === null, "no painter for no language");

// --- each language's marks -------------------------------------------------------
const has = (lang, text, want) => {
  const got = marks(paint(lang, text));
  for (const mark of want) check(got.includes(mark), `${lang}: ${JSON.stringify(text.slice(0, 40))} lacks ${mark}; got ${JSON.stringify(got)}`);
  return got;
};
const lacks = (lang, text, unwanted) => {
  const got = marks(paint(lang, text));
  for (const mark of unwanted) check(!got.includes(mark), `${lang}: ${JSON.stringify(text.slice(0, 40))} should not mark ${mark}`);
};
has("js", 'export async function main(a) {\n  return a + 42; // done\n}', ["k:export", "k:async", "k:function", "f:main", "k:return", "n:42", "c:// done"]);
has("js", 'class Foo extends Bar { static x = "s"; y = null; }', ["k:class", "t:Foo", "k:extends", "t:Bar", "s:\"s\"", "n:null"]);
has("js", "const re = /[\"'`]/g; const t = `a ${b}`;", ["s:/[\"'`]/g", "s:`a ${b}`"]);
has("js", "const half = total / 2 / count;", ["n:2"]);
lacks("js", "const half = total / 2 / count;", ["s:/ 2 /"]);
has("js", "@Component({}) class A {}", ["m:@Component"]);
has("js", "let x: string | number = 0x1F + 1_000n;", ["t:string", "t:number", "n:0x1F", "n:1_000n"]);
lacks("js", 'const s = "if return // not a comment";', ["k:if", "k:return", "c:// not a comment\";"]);
has("json", '{ "name": "x", "n": -1.5e3, "ok": true, "none": null }', ["p:\"name\"", "s:\"x\"", "n:1.5e3", "n:true", "n:null"]);
has("python", '@dataclass\ndef f(x: int) -> str:\n    return f"{x}" + r"\\d" # c', ["m:@dataclass", "k:def", "f:f", "t:int", "t:str", "k:return", "s:f\"{x}\"", "s:r\"\\d\"", "c:# c"]);
has("python", "class A(Base):\n    x = None", ["k:class", "t:A", "t:Base", "n:None"]);
has("go", "package main\nfunc (r *T) Name() error { return nil }\ntype Pt struct{}", ["k:package", "k:func", "f:Name", "t:error", "n:nil", "k:type", "t:Pt", "k:struct"]);
has("go", "s := `raw ${x}` + \"q\"", ["s:`raw ${x}`", "s:\"q\""]);
has("rust", "fn f<'a>(x: &'a str) -> Option<i32> { println!(\"{}\", 'c'); None }", ["k:fn", "f:f", "m:'a", "t:str", "t:Option", "t:i32", "m:println", "s:'c'", "n:None"]);
has("rust", "#[derive(Debug)]\nstruct P;", ["m:#[derive(Debug)]", "k:struct", "t:P"]);
has("c", '#include <stdio.h>\nint main(void) { printf("%d", 1); return 0; }', ["m:#include", "s:<stdio.h>", "t:int", "f:main", "t:void", "f:printf", "s:\"%d\"", "k:return"]);
has("java", "@Override public static void main(String[] a) {}", ["m:@Override", "k:public", "k:static", "t:void", "f:main", "t:String"]);
has("csharp", 'public class A { var s = $"x"; }', ["k:public", "k:class", "t:A", "k:var", "s:$\"x\""]);
has("swift", "func go(x: Int) -> Bool { return true }", ["k:func", "f:go", "t:Int", "t:Bool", "n:true"]);
has("php", "<?php function f($x) { return $x; } // c", ["k:function", "f:f", "v:$x", "c:// c"]);
has("ruby", "def go(x)\n  @y = :sym # c\nend", ["k:def", "f:go", "v:@y", "n::sym", "c:# c", "k:end"]);
has("lua", "local function f() return nil end -- c", ["k:local", "k:function", "f:f", "n:nil", "c:-- c"]);
has("zig", "pub fn main() !void { @import(\"std\"); }", ["k:pub", "k:fn", "f:main", "t:void", "m:@import", "s:\"std\""]);
has("shell", 'if [ -f "$HOME/x" ]; then echo $1 ${NAME:-y} # c\nfi', ["k:if", "s:\"$HOME/x\"", "k:then", "f:echo", "v:$1", "v:${NAME:-y}", "c:# c", "k:fi"]);
lacks("shell", "echo $# a#b", ["c:# a#b", "c:#b"]);
has("sql", "SELECT name FROM users WHERE id = 1 -- c", ["k:SELECT", "k:FROM", "k:WHERE", "n:1", "c:-- c"]);
has("proto", 'syntax = "proto3";\nmessage A { string s = 1; }', ["k:syntax", "s:\"proto3\"", "k:message", "t:A", "t:string", "n:1"]);
has("hcl", 'resource "aws_s3" "b" {\n  bucket = "x" # c\n}', ["k:resource", "s:\"aws_s3\"", "p:bucket", "c:# c"]);
has("lisp", '(defun foo (x &optional y)\n  "Doc."\n  (let ((a 1)) (bar #\'baz :key *v* t #\\Space)))', ["k:defun", "f:foo", "k:&optional", "s:\"Doc.\"", "k:let", "n:1", "f:#'baz", "n::key", "v:*v*", "n:t", "s:#\\Space"]);
has("lisp", "(defvar *x* 1) (defclass point () ()) ; c", ["k:defvar", "v:*x*", "k:defclass", "t:point", "c:; c"]);
lacks("lisp", "(let ((a 1)) (call a))", ["f:a", "f:call"]);
has("markup", '<div class="x" data-y=\'z\'>a &amp; b<!-- c --></div>', ["k:div", "p:class", "s:\"x\"", "p:data-y", "s:'z'", "n:&amp;", "c: c -->"]);
has("markup", '<script>const a = "<b>";</script><style>p { color: red; }</style>', ["k:script", "k:const", "s:\"<b>\"", "k:style", "k:p", "p:color"]);
has("css", ".a:hover, #b > p { color: #fff; margin: 0 2px !important; }", ["t:.a", "m::hover", "t:#b", "k:p", "p:color", "n:#fff", "n:2px", "k:!important"]);
has("css", "@media (max-width: 30em) { a { --gap: var(--x); } }", ["k:@media", "p:max-width", "n:30em", "k:a", "p:--gap", "f:var", "v:--x"]);
has("css", ".a { &:hover { color: red; } }", ["t:.a", "m:&", "m::hover", "p:color"]);
has("markdown", "# Title\n- item `code` [link](https://x.y)\n> quote\n```js\nconst a = 1;\n```\ntext", ["h:# Title", "k:- ", "s:`code`", "p:https://x.y", "m:> ", "m:```js", "k:const", "n:1", "m:```"]);
has("yaml", 'name: ci\non: [push]\njobs:\n  - run: "x" # c\n    if: true\nblock: |\n  key: not a key\nnext: 1', ["p:name", "p:on", "k:- ", "p:run", "s:\"x\"", "c:# c", "p:if", "n:true", "p:block", "s:  key: not a key", "p:next", "n:1"]);
has("ini", '# c\n[server]\nport = 8080\nname = "x"\nok = true', ["c:# c", "t:[server]", "p:port", "n:8080", "p:name", "s:\"x\"", "n:true"]);
has("diff", "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n same", ["m:--- a/x", "m:+++ b/x", "p:@@ -1 +1 @@", "d:-old", "a:+new"]);
has("make", "CC := gcc\nall: main.o # c\n\t$(CC) -o $@ $< && echo \"done\"", ["p:CC", "f:all", "c:# c", "v:$(CC)", "v:$@", "v:$<", "f:echo", "s:\"done\""]);
has("docker", "FROM node:20 AS build\n# c\nRUN npm ci && \\\n    echo \"$HOME\"", ["k:FROM", "c:# c", "k:RUN", "s:\"$HOME\""]);

// --- what one line hands the next ----------------------------------------------------
has("js", "a /* one\nif two\n*/ return", ["c:/* one", "c:if two", "c:*/", "k:return"]);
has("js", "`one\nif ${two}\nthree` + 1", ["s:`one", "s:if ${two}", "s:three`", "n:1"]);
has("python", 'x = """one\nif\n""" + 1', ["s:\"\"\"one", "s:if", "s:\"\"\"", "n:1"]);
has("rust", "/* a /* b */ still */ fn", ["c:/* a /* b */ still */", "k:fn"]);
has("lisp", "#| a\n #| b |# (defun\n|# (let", ["c:#| a", "c: #| b |# (defun", "c:|#", "k:let"]);
has("markup", "<a\n  href='x'\n  title=\"y\">t</a>", ["k:a", "p:href", "s:'x'", "p:title", "s:\"y\""]);
has("markup", "<script>\nif (a) {}\n</script>", ["k:if"]);
has("css", "a {\n  color: red;\n}\nb { }", ["k:a", "p:color", "k:b"]);
has("markdown", "```python\ndef f(): pass\n```\n# after", ["k:def", "f:f", "h:# after"]);

// --- the text, kept ----------------------------------------------------------------------
const samples = {
  js: "const a = '<b>' && 1 < 2; // <&>\n\tlet 字 = `😀${x}`;\n",
  markup: "<p a=\"<&>\">x &lt; y</p>\n<!-- <> -->\n",
  lisp: "(format t \"~a <&>\" 'x) ; <tag>\n",
  markdown: "# <h1>\n```html\n<b>&</b>\n```\n",
  css: "a::before { content: \"<&>\"; }\n",
  shell: "cat <<EOF > x && echo $((1<2))\n",
};
for (const [lang, text] of Object.entries(samples)) {
  const html = paint(lang, text);
  check(plain(html) === text.split("\n").join("\n"), `${lang}: the painted text reads back as the text`);
  check(!/<(?!\/?span\b)/.test(html) && !/&(?!amp;|lt;|gt;)/.test(html), `${lang}: every < and & of the text is escaped`);
}
const long = "x".repeat(5000) + " if (";
check(paintLine("js", long)[0] === long, "a line past 4000 characters is drawn plain");

// --- a painter fed edits draws what a fresh one draws -----------------------------------
let seed = 7;
const rand = (n) => { seed = (seed * 1103515245 + 12345) % 2147483648; return seed % n; };
const pieces = ["/*", "*/", "`", "\"", "'", "\n", "if ", "x", "//", "#|", "|#", "(", ")", "{", "}", "<a ", ">", "<script>", "</script>", "```js", "\n```", "\"\"\""];
for (const lang of ["js", "python", "lisp", "markup", "markdown", "css", "rust", "yaml"]) {
  const painter = paintFor(lang);
  let text = samples[lang] || "a\nb\nc\n";
  let same = true;
  for (let step = 0; step < 300; step++) {
    const at = rand(text.length + 1);
    text = rand(3) === 0 ? text.slice(0, at) + text.slice(at + 1 + rand(4)) : text.slice(0, at) + pieces[rand(pieces.length)] + text.slice(at);
    painter.set(text);
    const lines = text.split("\n").length;
    const from = rand(lines);
    const to = from + 1 + rand(12);
    const fresh = paintFor(lang);
    fresh.set(text);
    if (painter.html(from, to) !== fresh.html(from, to)) { same = false; break; }
  }
  check(same, `${lang}: a painter fed 300 edits draws each window as a fresh painter does`);
}
const p = paintFor("js");
check(p.set("a") === true && p.set("a") === false && p.version === 1, "set says whether anything changed, and only then moves version");

console.log(`SYNTAX ${passed} passed, ${failed} failed`);
