// syntax.js -- a file's text in colour, for the Files view's reader and its
// editor (files.js). A file's language comes from its name (or a #! line), and
// each family of languages has one small scanner that reads a line at a time:
// a line starts in the state the line above left -- inside a block comment, a
// string that runs on, a fenced block of another language -- and hands its own
// state down. So a keystroke paints again only from the line it changed, and
// only as far down as the screen shows (paintFor's html(from, to)).
//
// What it draws is the text itself, escaped, with runs wrapped in
// <span class="sx-?">: k keyword, t type, f function, s string, n number or
// constant, c comment, p property (a key, an attribute), m meta (a decorator,
// a directive, a macro), v variable, h heading, a added, d deleted. Colour
// only, never a weight or a slant (app.css): the editor lays this over its
// textarea, letter on letter, so nothing may change a letter's width.
//
// It is a reader's colouring, not a parser: a line that fools it is drawn in
// the wrong colour, never wrong text.

const esc = (text) => text.replace(/[&<>]/g, (c) => (c === "&" ? "&amp;" : c === "<" ? "&lt;" : "&gt;"));
const words = (text) => new Set(text.split(/\s+/).filter(Boolean));

// A line longer than this is drawn as plain text: minified code has nothing
// to read in colour, and a megabyte of spans would slow every keystroke.
const LONG = 4000;

// One line's drawing: runs of plain text and coloured runs, as HTML.
class Line {
  constructor(text) { this.text = text; this.html = ""; this.at = 0; }
  // Everything before TO not yet drawn goes in plain.
  plain(to) { if (to > this.at) this.html += esc(this.text.slice(this.at, to)); this.at = Math.max(this.at, to); }
  // FROM..TO in colour CLS, after whatever plain text leads to it.
  mark(cls, from, to) {
    this.plain(from);
    if (to > from) this.html += `<span class="sx-${cls}">${esc(this.text.slice(from, to))}</span>`;
    this.at = to;
  }
  done() { this.plain(this.text.length); return this.html; }
}

// --- the C family: braces, words, strings, comments -----------------------------
// One scanner for every language that reads as words between punctuation, told
// apart by a spec: { line: ["//"], block: [["/*", "*/", nests]], strings:
// [{ open, close, escape, multi }], keywords, literals, types, builtins, defs
// (a keyword whose next word it names: def NAME), caps (Name is a type, NAME a
// constant), word (what a word is), fold (keywords in any case), extra (the
// language's own marks: a regex, a $variable, a lifetime). The state a line
// hands on is "" or "b<i>:<depth>" (in block comment i) or "s<i>" (in string i).

const NUMBER = /(?:0[xX][\da-fA-F_]+|0[bB][01_]+|0[oO][0-7_]+|\d[\d_]*(?:\.(?!\.)[\d_]*)?(?:[eE][+-]?\d+)?|\.\d[\d_]*(?:[eE][+-]?\d+)?)[a-zA-Z\d_]*/y;
const WORD = /[A-Za-z_$][\w$]*/y;
const SPACE = /\s/;

function startsAny(text, at, list) {
  for (let k = 0; k < list.length; k++) if (text.startsWith(list[k], at)) return k;
  return -1;
}

// Where a string that opened before FROM closes (past its close), or -1.
function stringEnd(text, from, s) {
  for (let j = from; j < text.length; j++) {
    if (s.escape && text[j] === "\\") { j++; continue; }
    if (text.startsWith(s.close, j)) return j + s.close.length;
  }
  return -1;
}

// Where a block comment that opened before FROM closes, at DEPTH (nested
// comments count their opens), as [end, depth]: end -1 when the line ends first.
function blockEnd(text, from, b, depth) {
  for (let j = from; j < text.length; j++) {
    if (b[2] && text.startsWith(b[0], j)) { depth++; j += b[0].length - 1; continue; }
    if (text.startsWith(b[1], j)) {
      depth--;
      j += b[1].length - 1;
      if (depth === 0) return [j + 1, 0];
    }
  }
  return [-1, depth];
}

function nextChar(text, at) {
  while (at < text.length && (text[at] === " " || text[at] === "\t")) at++;
  return text[at];
}

function scanCode(spec, text, state, out) {
  let i = 0;
  const n = text.length;
  if (state) {
    const kind = state[0];
    const index = parseInt(state.slice(1), 10);
    if (kind === "b") {
      const b = spec.block[index];
      const [end, depth] = blockEnd(text, 0, b, parseInt(state.split(":")[1], 10) || 1);
      if (end < 0) { out.mark("c", 0, n); return `b${index}:${depth}`; }
      out.mark("c", 0, end);
      i = end;
    } else if (kind === "s") {
      const s = spec.strings[index];
      const end = stringEnd(text, 0, s);
      if (end < 0) { out.mark(s.cls || "s", 0, n); return s.multi || text.endsWith("\\") ? state : ""; }
      out.mark(s.cls || "s", 0, end);
      i = end;
    }
  }
  let named = ""; // the colour of the next word, after a keyword that names it
  while (i < n) {
    const ch = text[i];
    if (ch === " " || ch === "\t") { i++; continue; }
    if (spec.line) {
      const k = startsAny(text, i, spec.line);
      if (k >= 0 && (!spec.lineAfterSpace || i === 0 || SPACE.test(text[i - 1]))) { out.mark("c", i, n); return ""; }
    }
    if (spec.block) {
      const k = startsAny(text, i, spec.block.map((b) => b[0]));
      if (k >= 0) {
        const b = spec.block[k];
        const [end, depth] = blockEnd(text, i + b[0].length, b, 1);
        if (end < 0) { out.mark("c", i, n); return `b${k}:${depth}`; }
        out.mark("c", i, end);
        i = end;
        continue;
      }
    }
    if (spec.extra) {
      const hit = spec.extra(text, i, out);
      if (hit) {
        if (hit.state !== undefined) return hit.state;
        i = hit.end;
        continue;
      }
    }
    if (spec.strings) {
      const k = startsAny(text, i, spec.strings.map((s) => s.open));
      if (k >= 0) {
        const s = spec.strings[k];
        const end = stringEnd(text, i + s.open.length, s);
        if (end < 0) {
          out.mark(s.cls || "s", i, n);
          return s.multi || (s.escape && text.endsWith("\\")) ? `s${k}` : "";
        }
        out.mark(s.cls || "s", i, end);
        i = end;
        named = "";
        continue;
      }
    }
    const code = text.charCodeAt(i);
    if ((code >= 48 && code <= 57) || (ch === "." && text.charCodeAt(i + 1) >= 48 && text.charCodeAt(i + 1) <= 57)) {
      NUMBER.lastIndex = i;
      const m = NUMBER.exec(text);
      if (m) { out.mark("n", i, i + m[0].length); i += m[0].length; continue; }
    }
    const word = spec.word || WORD;
    word.lastIndex = i;
    const m = word.exec(text);
    if (m && m[0]) {
      const w = m[0];
      const end = i + w.length;
      // A string's prefix (r"", b'', f""): the string, prefix and all.
      if (spec.prefix && spec.prefix.test(w) && spec.strings) {
        const k = startsAny(text, end, spec.strings.map((s) => s.open));
        if (k >= 0) {
          const s = spec.strings[k];
          const close = stringEnd(text, end + s.open.length, s);
          if (close < 0) { out.mark(s.cls || "s", i, n); return s.multi ? `s${k}` : ""; }
          out.mark(s.cls || "s", i, close);
          i = close;
          continue;
        }
      }
      const key = spec.fold ? w.toLowerCase() : w;
      let cls = "";
      if (spec.keywords?.has(key)) cls = "k";
      else if (spec.literals?.has(key)) cls = "n";
      else if (spec.types?.has(key)) cls = "t";
      else if (named) cls = named;
      else if (spec.macro && text[end] === "!" && text[end + 1] !== "=") cls = "m";
      else if (spec.caps && /^[A-Z][A-Z\d_]+$/.test(w)) cls = "n";
      else if (spec.call !== false && nextChar(text, end) === "(") cls = "f";
      else if (spec.caps && /^[A-Z]/.test(w) && /[a-z]/.test(w)) cls = "t";
      else if (spec.builtins?.has(key)) cls = "f";
      named = cls === "k" ? spec.defs?.[key] || "" : "";
      if (cls) out.mark(cls, i, end);
      i = end;
      continue;
    }
    i++;
  }
  return "";
}

// A JavaScript regex where a value may start (after ( , = : [ ! & | ? { } ; or
// return and its like), so a quote or backtick in one never opens a string.
const REGEX = /\/(?![*/])(?:\\.|\[(?:\\.|[^\]\\])*\]|[^/\\[\n])+\/[dgimsuyv]*/y;
const VALUE_WORDS = words("return typeof instanceof in of new delete void throw case do else yield await");
function jsExtra(text, i, out) {
  if (text[i] === "/") {
    let j = i - 1;
    while (j >= 0 && (text[j] === " " || text[j] === "\t")) j--;
    const before = j < 0 ? "" : text[j];
    let value = j < 0 || "(,=:[!&|?{};+-*%<>~^".includes(before);
    if (!value && /[\w$]/.test(before)) {
      const start = text.slice(0, j + 1).search(/[\w$]+$/);
      value = VALUE_WORDS.has(text.slice(start, j + 1));
    }
    if (value) {
      REGEX.lastIndex = i;
      const m = REGEX.exec(text);
      if (m) { out.mark("s", i, i + m[0].length); return { end: i + m[0].length }; }
    }
  }
  if (text[i] === "@") {
    const m = /@[A-Za-z_$][\w$.]*/y;
    m.lastIndex = i;
    const hit = m.exec(text);
    if (hit && (i === 0 || !/[\w$]/.test(text[i - 1]))) { out.mark("m", i, i + hit[0].length); return { end: i + hit[0].length }; }
  }
  return null;
}

const decorator = (text, i, out) => {
  if (text[i] !== "@" || (i > 0 && /\w/.test(text[i - 1]))) return null;
  const m = /@[A-Za-z_][\w.]*/y;
  m.lastIndex = i;
  const hit = m.exec(text);
  if (!hit) return null;
  out.mark("m", i, i + hit[0].length);
  return { end: i + hit[0].length };
};

// C's preprocessor: the directive a meta mark, an #include's file a string.
function cExtra(text, i, out) {
  if (text[i] !== "#" || text.slice(0, i).trim()) return null;
  const m = /#\s*[A-Za-z_]+/y;
  m.lastIndex = i;
  const hit = m.exec(text);
  if (!hit) return null;
  const end = i + hit[0].length;
  out.mark("m", i, end);
  if (/include|import/.test(hit[0])) {
    const file = /\s*(<[^>]*>|"[^"]*")/y;
    file.lastIndex = end;
    const f = file.exec(text);
    if (f) { out.mark("s", end + f[0].indexOf(f[1]), end + f[0].length); return { end: end + f[0].length }; }
  }
  return { end };
}

// Rust: 'a is a lifetime unless it closes as a char ('a'), #[attr] is meta.
function rustExtra(text, i, out) {
  if (text[i] === "'") {
    const char = /'(?:\\(?:x[\da-fA-F]{2}|u\{[\da-fA-F]+\}|.)|[^\\'])'/y;
    char.lastIndex = i;
    let m = char.exec(text);
    if (m) { out.mark("s", i, i + m[0].length); return { end: i + m[0].length }; }
    const life = /'[A-Za-z_]\w*/y;
    life.lastIndex = i;
    m = life.exec(text);
    if (m) { out.mark("m", i, i + m[0].length); return { end: i + m[0].length }; }
  }
  if (text[i] === "#" && (text[i + 1] === "[" || (text[i + 1] === "!" && text[i + 2] === "["))) {
    const close = text.indexOf("]", i);
    const end = close < 0 ? text.length : close + 1;
    out.mark("m", i, end);
    return { end };
  }
  return null;
}

// A shell's $NAME, ${...}, $1, $@ as a variable.
const SHELL_VAR = /\$(?:\{[^}]*\}|[A-Za-z_]\w*|[0-9@#?$!*-])/y;
function shellExtra(text, i, out) {
  if (text[i] !== "$") return null;
  SHELL_VAR.lastIndex = i;
  const m = SHELL_VAR.exec(text);
  if (!m) return null;
  out.mark("v", i, i + m[0].length);
  return { end: i + m[0].length };
}

const dollarVar = (text, i, out) => {
  if (text[i] !== "$") return null;
  const m = /\$[A-Za-z_]\w*/y;
  m.lastIndex = i;
  const hit = m.exec(text);
  if (!hit) return null;
  out.mark("v", i, i + hit[0].length);
  return { end: i + hit[0].length };
};

const DQ = { open: '"', close: '"', escape: true };
const SQ = { open: "'", close: "'", escape: true };
const BT = { open: "`", close: "`", escape: true, multi: true };
const C_COMMENTS = { line: ["//"], block: [["/*", "*/"]] };

const JS_KEYWORDS = "async await break case catch class const continue debugger default delete do else export extends finally for from function get if import in instanceof let new of return set static super switch this throw try typeof var void while with yield as satisfies";
const TS_KEYWORDS = "abstract declare enum implements interface keyof namespace private protected public readonly type infer is asserts override accessor module";
const JS_LITERALS = "true false null undefined NaN Infinity";
const TS_TYPES = "string number boolean any unknown never object symbol bigint void";
const JS_DEFS = { function: "f", class: "t", interface: "t", type: "t", enum: "t", extends: "t", implements: "t" };

const SPECS = {
  js: { ...C_COMMENTS, strings: [DQ, SQ, BT], keywords: words(`${JS_KEYWORDS} ${TS_KEYWORDS}`), literals: words(JS_LITERALS), types: words(TS_TYPES), defs: JS_DEFS, caps: true, extra: jsExtra },
  json: { ...C_COMMENTS, strings: [DQ], literals: words("true false null"), call: false, extra: jsonKey },
  python: {
    line: ["#"], strings: [{ open: '"""', close: '"""', escape: true, multi: true }, { open: "'''", close: "'''", escape: true, multi: true }, DQ, SQ],
    prefix: /^(?:[rRbBfFuU]|[rR][bBfF]|[bBfF][rR])$/,
    keywords: words("and as assert async await break class continue def del elif else except finally for from global if import in is lambda nonlocal not or pass raise return try while with yield match case"),
    literals: words("True False None NotImplemented Ellipsis __name__ __file__"),
    types: words("int float str bytes bool list dict set tuple object type complex frozenset bytearray"),
    builtins: words("print len range open super isinstance self cls"), defs: { def: "f", class: "t" }, caps: true, extra: decorator,
  },
  go: { ...C_COMMENTS, strings: [DQ, { open: "'", close: "'", escape: true }, { open: "`", close: "`", multi: true }],
    keywords: words("break case chan const continue default defer else fallthrough for func go goto if import interface map package range return select struct switch type var"),
    literals: words("true false nil iota"), types: words("bool byte complex64 complex128 error float32 float64 int int8 int16 int32 int64 rune string uint uint8 uint16 uint32 uint64 uintptr any comparable"),
    builtins: words("append cap close copy delete len make new panic print println recover min max clear"), defs: { type: "t" } },
  rust: { line: ["//"], block: [["/*", "*/", true]], strings: [{ ...DQ, multi: true }], prefix: /^b$/,
    keywords: words("as async await break const continue crate dyn else enum extern fn for if impl in let loop match mod move mut pub ref return self Self static struct super trait type union unsafe use where while yield"),
    literals: words("true false None Some Ok Err"), types: words("bool char str u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize f32 f64 String Vec Option Result Box"),
    defs: { fn: "f", struct: "t", enum: "t", trait: "t", type: "t", impl: "t" }, caps: true, macro: true, extra: rustExtra },
  c: { ...C_COMMENTS, strings: [DQ, SQ], prefix: /^(?:L|u8?|U|R)$/,
    keywords: words("auto break case const continue default do else enum extern for goto if inline register restrict return sizeof static struct switch typedef union volatile while class namespace template typename public private protected virtual override final new delete this throw try catch using operator friend explicit constexpr noexcept nullptr_t static_cast dynamic_cast reinterpret_cast const_cast decltype concept requires co_await co_return co_yield export import module @interface @implementation @end @property"),
    literals: words("true false NULL nullptr YES NO nil"), types: words("void char short int long float double signed unsigned bool size_t ssize_t int8_t int16_t int32_t int64_t uint8_t uint16_t uint32_t uint64_t wchar_t auto"),
    defs: { struct: "t", class: "t", enum: "t", union: "t", namespace: "t" }, caps: true, extra: cExtra },
  java: { ...C_COMMENTS, strings: [{ open: '"""', close: '"""', escape: true, multi: true }, DQ, SQ],
    keywords: words("abstract assert break case catch class const continue default do else enum extends final finally for goto if implements import instanceof interface native new package private protected public return static strictfp super switch synchronized this throw throws transient try volatile while var record sealed permits yield fun val object companion data when is in out override open internal lateinit by init typealias suspend inline reified"),
    literals: words("true false null"), types: words("boolean byte char short int long float double void String Int Long Boolean Unit Any Nothing"),
    defs: { class: "t", interface: "t", enum: "t", record: "t", fun: "f", object: "t" }, caps: true, extra: decorator },
  csharp: { ...C_COMMENTS, strings: [DQ, SQ], prefix: /^[@$]+$/,
    keywords: words("abstract as base break case catch checked class const continue default delegate do else enum event explicit extern finally fixed for foreach goto if implicit in interface internal is lock namespace new operator out override params private protected public readonly record ref return sealed sizeof stackalloc static struct switch this throw try typeof unchecked unsafe using virtual volatile while async await var get set init value yield where when"),
    literals: words("true false null"), types: words("bool byte char decimal double float int long object sbyte short string uint ulong ushort void dynamic"),
    defs: { class: "t", interface: "t", enum: "t", struct: "t", record: "t", namespace: "t" }, caps: true, extra: cExtra },
  swift: { ...C_COMMENTS, strings: [{ open: '"""', close: '"""', escape: true, multi: true }, DQ],
    keywords: words("associatedtype class deinit enum extension fileprivate func import init inout internal let open operator private protocol public rethrows static struct subscript typealias var break case continue default defer do else fallthrough for guard if in repeat return switch where while as catch is super self Self throw throws try async await some any actor nonisolated mutating override final lazy weak"),
    literals: words("true false nil"), types: words("Int Double Float String Bool Character Void Array Dictionary Set Optional"),
    defs: { func: "f", class: "t", struct: "t", enum: "t", protocol: "t", extension: "t", actor: "t" }, caps: true, extra: decorator },
  php: { line: ["//", "#"], block: [["/*", "*/"]], strings: [DQ, SQ],
    keywords: words("abstract and array as break callable case catch class clone const continue declare default do echo else elseif empty enddeclare endfor endforeach endif endswitch endwhile extends final finally fn for foreach function global goto if implements include include_once instanceof insteadof interface isset list match namespace new or print private protected public readonly require require_once return static switch throw trait try unset use var while xor yield"),
    literals: words("true false null TRUE FALSE NULL"), fold: false, defs: { function: "f", class: "t", interface: "t", trait: "t" }, caps: true, extra: dollarVar },
  ruby: { line: ["#"], strings: [DQ, SQ],
    keywords: words("alias and begin break case class def defined? do else elsif end ensure for if in module next not or redo rescue retry return self super then undef unless until when while yield require require_relative attr_accessor attr_reader attr_writer private protected public lambda proc"),
    literals: words("true false nil"), defs: { def: "f", class: "t", module: "t" }, caps: true, word: /[A-Za-z_@$][\w]*[?!]?/y, extra: rubyExtra },
  lua: { line: ["--"], block: [["--[[", "]]"]], strings: [DQ, SQ, { open: "[[", close: "]]", multi: true }],
    keywords: words("and break do else elseif end for function goto if in local not or repeat return then until while"), literals: words("true false nil"),
    builtins: words("print pairs ipairs require type tostring tonumber setmetatable getmetatable error assert pcall"), defs: { function: "f" } },
  zig: { line: ["//"], strings: [DQ, SQ],
    keywords: words("addrspace align allowzero and anyframe anytype asm async await break callconv catch comptime const continue defer else enum errdefer error export extern fn for if inline linksection noalias noinline nosuspend opaque or orelse packed pub resume return struct suspend switch test threadlocal try union unreachable usingnamespace var volatile while"),
    literals: words("true false null undefined"), types: words("i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 isize usize f16 f32 f64 f128 bool void noreturn type anyerror comptime_int comptime_float"), defs: { fn: "f" }, caps: true,
    extra: (text, i, out) => { if (text[i] !== "@") return null; const m = /@[A-Za-z_]\w*/y; m.lastIndex = i; const hit = m.exec(text); if (!hit) return null; out.mark("m", i, i + hit[0].length); return { end: i + hit[0].length }; } },
  shell: { line: ["#"], lineAfterSpace: true, strings: [{ open: "'", close: "'", multi: true }, { open: '"', close: '"', escape: true, multi: true }],
    keywords: words("if then else elif fi for while until do done case esac in function select return local export readonly declare typeset break continue time"),
    builtins: words("echo printf cd pwd read test exit source eval exec set unset shift trap alias wait kill true false"),
    word: /[A-Za-z_][\w-]*/y, call: true, extra: shellExtra },
  sql: { line: ["--"], block: [["/*", "*/"]], strings: [{ open: "'", close: "'", escape: true }, { open: '"', close: '"', cls: "p" }], fold: true,
    keywords: words("select from where and or not insert into values update set delete create table view index drop alter add column primary key foreign references join left right inner outer full cross on as group by order having limit offset distinct union all case when then else end is null in exists between like ilike begin commit rollback transaction returning with recursive if default unique check constraint cascade asc desc database schema grant revoke trigger function procedure language replace temporary temp"),
    literals: words("true false null"), types: words("int integer bigint smallint serial bigserial text varchar char boolean bool date time timestamp timestamptz interval numeric decimal real float double precision json jsonb uuid bytea blob"), call: true },
  proto: { ...C_COMMENTS, strings: [DQ, SQ],
    keywords: words("syntax package import option message enum service rpc returns repeated optional required oneof map reserved extend stream public weak"),
    literals: words("true false"), types: words("double float int32 int64 uint32 uint64 sint32 sint64 fixed32 fixed64 sfixed32 sfixed64 bool string bytes"), defs: { message: "t", enum: "t", service: "t", rpc: "f" }, caps: true },
  hcl: { line: ["#", "//"], block: [["/*", "*/"]], strings: [DQ], literals: words("true false null"),
    keywords: words("resource data variable output locals module provider terraform for in if each count"), extra: hclKey },
};

// JSON's keys: a string the next mark after is ":" reads as a property.
function jsonKey(text, i, out) {
  if (text[i] !== '"') return null;
  const end = stringEnd(text, i + 1, DQ);
  if (end < 0 || nextChar(text, end) !== ":") return null;
  out.mark("p", i, end);
  return { end };
}

// HCL's attribute names: a word at a line's start before "=".
function hclKey(text, i, out) {
  if (text.slice(0, i).trim()) return null;
  const m = /[A-Za-z_][\w-]*(?=\s*=(?!=))/y;
  m.lastIndex = i;
  const hit = m.exec(text);
  if (!hit) return null;
  out.mark("p", i, i + hit[0].length);
  return { end: i + hit[0].length };
}

// Ruby's :symbols, @instance and $global variables.
function rubyExtra(text, i, out) {
  const m = text[i] === ":" && text[i + 1] !== ":" && (i === 0 || !/[\w:]/.test(text[i - 1])) ? /:[A-Za-z_]\w*[?!]?/y
    : text[i] === "@" || text[i] === "$" ? /[@$]{1,2}[A-Za-z_]\w*/y : null;
  if (!m) return null;
  m.lastIndex = i;
  const hit = m.exec(text);
  if (!hit) return null;
  out.mark(text[i] === ":" ? "n" : "v", i, i + hit[0].length);
  return { end: i + hit[0].length };
}

const codeScanner = (spec) => (text, state, out) => scanCode(spec, text, state, out);

// --- Lisp: parens, symbols, ; and #| |# ----------------------------------------
// The word after an open paren is the form's head: a special form, a macro
// of the language or a definer is a keyword, and a definer's next word its
// name. Any other head stays plain, as the Lisp editors draw it: a call and
// a binding, (let ((x 1)) ...), look alike. :keyword symbols are constants,
// #'fn a function, *special* a variable.

const LISP_FORMS = words(`defun defmacro defvar defparameter defconstant defclass defmethod defgeneric defstruct defpackage deftype define-condition define-hook
  defsystem deftest define defn defn- def defonce defrecord defprotocol defmulti ns let let* flet labels macrolet lambda progn prog1 prog2 if when unless cond case ecase typecase etypecase
  and or not loop do do* dolist dotimes block return return-from tagbody go catch throw unwind-protect handler-case handler-bind restart-case ignore-errors
  setf setq incf decf push pop multiple-value-bind destructuring-bind with-slots with-accessors with-open-file declare in-package the function quote eval-when
  load-time-value locally symbol-macrolet the-only fn letfn if-let when-let begin set! define-syntax let-values receive`);
const LISP_DEFINERS = /^(?:def|define)/i;
const LISP_WORD = /[^\s()'"`,;|[\]{}]+/y;

function scanLisp(text, state, out) {
  let i = 0;
  const n = text.length;
  if (state) {
    if (state[0] === "b") {
      const [end, depth] = blockEnd(text, 0, ["#|", "|#", true], parseInt(state.slice(1), 10) || 1);
      if (end < 0) { out.mark("c", 0, n); return `b${depth}`; }
      out.mark("c", 0, end);
      i = end;
    } else if (state === "s") {
      const end = stringEnd(text, 0, DQ);
      if (end < 0) { out.mark("s", 0, n); return "s"; }
      out.mark("s", 0, end);
      i = end;
    }
  }
  let head = false;
  let name = ""; // the colour of the word a definer names
  while (i < n) {
    const ch = text[i];
    if (ch === " " || ch === "\t") { i++; continue; }
    if (ch === ";") { out.mark("c", i, n); return ""; }
    if (ch === "#" && text[i + 1] === "|") {
      const [end, depth] = blockEnd(text, i + 2, ["#|", "|#", true], 1);
      if (end < 0) { out.mark("c", i, n); return `b${depth}`; }
      out.mark("c", i, end);
      i = end;
      continue;
    }
    if (ch === '"') {
      const end = stringEnd(text, i + 1, DQ);
      if (end < 0) { out.mark("s", i, n); return "s"; }
      out.mark("s", i, end);
      i = end;
      head = name = false;
      continue;
    }
    if (ch === "#" && text[i + 1] === "\\") {
      const m = /#\\(?:[A-Za-z][\w-]+|.)/y;
      m.lastIndex = i;
      const hit = m.exec(text);
      if (hit) { out.mark("s", i, i + hit[0].length); i += hit[0].length; head = false; continue; }
    }
    if (ch === "#" && text[i + 1] === "'") {
      LISP_WORD.lastIndex = i + 2;
      const hit = LISP_WORD.exec(text);
      const end = hit && hit.index === i + 2 ? i + 2 + hit[0].length : i + 2;
      out.mark("f", i, end);
      i = end;
      head = false;
      continue;
    }
    // A form's head; not the open of a binding list, (let ((x 1)) ...).
    if (ch === "(" || ch === "[") { head = true; name = ""; i++; continue; }
    if ("()'`,[]{}".includes(ch)) { head = false; i++; continue; }
    if (ch === "|") {
      const end = text.indexOf("|", i + 1);
      const stop = end < 0 ? n : end + 1;
      i = stop;
      head = false;
      continue;
    }
    LISP_WORD.lastIndex = i;
    const m = LISP_WORD.exec(text);
    if (!m) { i++; continue; }
    const w = m[0];
    const end = i + w.length;
    const lower = w.toLowerCase();
    let cls = "";
    if (/^[+-]?(?:\d+\.?\d*(?:[eEdD][+-]?\d+)?|\d*\.\d+(?:[eEdD][+-]?\d+)?|\d+\/\d+|#[xXbBoO][\da-fA-F]+)$/.test(w)) cls = "n";
    else if (w[0] === ":" || w[0] === "&") cls = w[0] === ":" ? "n" : "k";
    else if (lower === "t" || lower === "nil" || lower === "true" || lower === "false" || lower === "#t" || lower === "#f") cls = "n";
    else if (name) cls = name;
    else if (head && (LISP_FORMS.has(lower) || LISP_DEFINERS.test(w))) cls = "k";
    else if (/^\*.+\*$/.test(w) || /^\+.+\+$/.test(w)) cls = "v";
    name = head && cls === "k" && LISP_DEFINERS.test(w)
      ? /var|parameter|constant|^defonce$|^def$/i.test(w) ? "v" : /class|struct|type|condition|record|protocol|package|system/i.test(w) ? "t" : "f"
      : "";
    head = false;
    if (cls) out.mark(cls, i, end);
    i = end;
  }
  return "";
}

// --- markup: HTML, XML, SVG ------------------------------------------------------
// Tags and their attributes, comments, entities; a <script> or <style>'s body
// in its own language. State: "" in text, "c" in a comment, "t" inside a tag,
// "t'" or 't"' in a tag's quoted value, "x" in a CDATA, and "js\u0001<s>" /
// "css\u0001<s>" inside a script or a style, <s> that language's own state.

function scanMarkup(text, state, out) {
  let i = 0;
  const n = text.length;
  let st = state;
  while (i <= n) {
    if (st.startsWith("js\u0001") || st.startsWith("css\u0001")) {
      const [lang, inner] = st.split("\u0001");
      const close = text.toLowerCase().indexOf(lang === "js" ? "</script" : "</style", i);
      const to = close < 0 ? n : close;
      const part = new Line(text.slice(i, to));
      const next = (lang === "js" ? SCANNERS.js : SCANNERS.css)(part.text, inner, part);
      out.plain(i);
      out.html += part.done();
      out.at = to;
      i = to;
      if (close < 0) return `${lang}\u0001${next}`;
      st = "";
      continue;
    }
    if (i >= n) break;
    if (st === "c" || st === "x") {
      const close = text.indexOf(st === "c" ? "-->" : "]]>", i);
      if (close < 0) { out.mark(st === "c" ? "c" : "s", i, n); return st; }
      out.mark(st === "c" ? "c" : "s", i, close + 3);
      i = close + 3;
      st = "";
      continue;
    }
    if (st[0] === "t") {
      // Inside a tag ("t<body>|<quote>": the language its body is in, when it
      // is a script or a style, and the quote of a value that ran on): the
      // value's rest, attributes, the closing >.
      const bar = st.indexOf("|");
      const body = st.slice(1, bar);
      const q = st.slice(bar + 1);
      if (q) {
        const close = text.indexOf(q, i);
        if (close < 0) { out.mark("s", i, n); return st; }
        out.mark("s", i, close + 1);
        i = close + 1;
        st = `t${body}|`;
        continue;
      }
      const ch = text[i];
      if (ch === ">" || (ch === "/" && text[i + 1] === ">")) {
        const end = ch === ">" ? i + 1 : i + 2;
        out.mark("m", i, end);
        i = end;
        st = body && ch === ">" ? `${body}\u0001` : "";
        continue;
      }
      if (ch === " " || ch === "\t") { i++; continue; }
      if (ch === '"' || ch === "'") {
        const close = text.indexOf(ch, i + 1);
        if (close < 0) { out.mark("s", i, n); return `t${body}|${ch}`; }
        out.mark("s", i, close + 1);
        i = close + 1;
        continue;
      }
      const attr = /[^\s"'>/=]+/y;
      attr.lastIndex = i;
      const m = attr.exec(text);
      if (m) { out.mark("p", i, i + m[0].length); i += m[0].length; continue; }
      i++;
      continue;
    }
    const lt = text.indexOf("<", i);
    const amp = text.indexOf("&", i);
    const next = [lt, amp].filter((x) => x >= 0).sort((a, b) => a - b)[0];
    if (next === undefined) break;
    i = next;
    if (text[i] === "&") {
      const m = /&(?:#\d+|#x[\da-fA-F]+|[A-Za-z]\w*);/y;
      m.lastIndex = i;
      const hit = m.exec(text);
      if (hit) { out.mark("n", i, i + hit[0].length); i += hit[0].length; } else i++;
      continue;
    }
    if (text.startsWith("<!--", i)) { st = "c"; out.mark("c", i, i + 4); i += 4; continue; }
    if (text.startsWith("<![CDATA[", i)) { st = "x"; out.mark("m", i, i + 9); i += 9; continue; }
    const tag = /<\/?[A-Za-z][\w:.-]*|<[!?][A-Za-z]*/y;
    tag.lastIndex = i;
    const m = tag.exec(text);
    if (!m) { i++; continue; }
    const open = m[0][1] === "/" ? 2 : 1;
    out.mark("m", i, i + open);
    out.mark("k", i + open, i + m[0].length);
    const nameOfTag = m[0].slice(open).toLowerCase();
    const body = m[0][1] !== "/" && (nameOfTag === "script" ? "js" : nameOfTag === "style" ? "css" : "");
    i += m[0].length;
    st = `t${body || ""}|`;
  }
  return st;
}

// --- CSS, SCSS, Less ---------------------------------------------------------------
// Outside braces a selector: tags as keywords, .classes and #ids as types,
// :pseudo as meta. Inside, "name:" a property and its value: numbers with
// their unit, #colours, strings, functions. State: "<depth>" or "c<depth>".

function scanCss(text, state, out) {
  let i = 0;
  const n = text.length;
  let depth = parseInt(state.replace("c", ""), 10) || 0;
  if (state.startsWith("c")) {
    const close = text.indexOf("*/");
    if (close < 0) { out.mark("c", 0, n); return state; }
    out.mark("c", 0, close + 2);
    i = close + 2;
  }
  let value = false; // after a property's colon, until ; or }
  let paren = 0; // inside an at-rule's ( ), where "name: value" reads as a declaration
  while (i < n) {
    const ch = text[i];
    if (ch === " " || ch === "\t") { i++; continue; }
    if (ch === "(" && (paren || (depth === 0 && !value))) { paren++; i++; continue; }
    if (ch === ")" && paren) { paren--; if (!paren) value = false; i++; continue; }
    if (text.startsWith("/*", i)) {
      const close = text.indexOf("*/", i + 2);
      if (close < 0) { out.mark("c", i, n); return `c${depth}`; }
      out.mark("c", i, close + 2);
      i = close + 2;
      continue;
    }
    if (text.startsWith("//", i) && (i === 0 || /\s/.test(text[i - 1]))) { out.mark("c", i, n); return String(depth); }
    if (ch === '"' || ch === "'") {
      const end = stringEnd(text, i + 1, { close: ch, escape: true });
      const to = end < 0 ? n : end;
      out.mark("s", i, to);
      i = to;
      continue;
    }
    if (ch === "{") { depth++; value = false; i++; continue; }
    if (ch === "}") { depth = Math.max(0, depth - 1); value = false; i++; continue; }
    if (ch === ";") { value = false; i++; continue; }
    if (ch === "@") {
      const m = /@[\w-]+/y;
      m.lastIndex = i;
      const hit = m.exec(text);
      if (hit) { out.mark("k", i, i + hit[0].length); i += hit[0].length; continue; }
    }
    if (ch === "$" && /[\w-]/.test(text[i + 1] || "")) {
      const m = /\$[\w-]+/y;
      m.lastIndex = i;
      const hit = m.exec(text);
      out.mark("v", i, i + hit[0].length);
      i += hit[0].length;
      continue;
    }
    if (value || ((depth > 0 || paren > 0) && looksValue(text, i, paren > 0))) {
      if (!value) {
        // A property name at the front of a declaration.
        const m = /--?[\w-]+|[A-Za-z][\w-]*/y;
        m.lastIndex = i;
        const hit = m.exec(text);
        if (hit && /^\s*:/.test(text.slice(i + hit[0].length))) {
          out.mark("p", i, i + hit[0].length);
          i += hit[0].length;
          const colon = text.indexOf(":", i);
          i = colon + 1;
          value = true;
          continue;
        }
      }
      if (ch === "#") {
        const m = /#[\da-fA-F]{3,8}\b/y;
        m.lastIndex = i;
        const hit = m.exec(text);
        if (hit) { out.mark("n", i, i + hit[0].length); i += hit[0].length; continue; }
      }
      const num = /[+-]?(?:\d+\.?\d*|\.\d+)(?:%|[A-Za-z]+)?/y;
      num.lastIndex = i;
      let hit = num.exec(text);
      if (hit && (i === 0 || !/[\w-]/.test(text[i - 1]))) { out.mark("n", i, i + hit[0].length); i += hit[0].length; continue; }
      const word = /--?[\w-]+|[A-Za-z_][\w-]*/y;
      word.lastIndex = i;
      hit = word.exec(text);
      if (hit) {
        const end = i + hit[0].length;
        const cls = text[end] === "(" ? "f" : hit[0] === "!important" ? "k" : hit[0].startsWith("--") ? "v" : "";
        if (cls) out.mark(cls, i, end);
        i = end;
        continue;
      }
      if (ch === "!") {
        const m = /!\s*important/y;
        m.lastIndex = i;
        const imp = m.exec(text);
        if (imp) { out.mark("k", i, i + imp[0].length); i += imp[0].length; continue; }
      }
      i++;
      continue;
    }
    // A selector.
    const sel = /[.#][\w-]+|::?[\w-]+|[A-Za-z][\w-]*|&|\*/y;
    sel.lastIndex = i;
    const hit = sel.exec(text);
    if (hit) {
      const s = hit[0];
      const cls = s[0] === "." || s[0] === "#" ? "t" : s[0] === ":" ? "m" : s === "&" || s === "*" ? "m" : "k";
      out.mark(cls, i, i + s.length);
      i += s.length;
      continue;
    }
    i++;
  }
  return String(depth);
}

// Inside braces, does the text at I start a declaration ("name: value")
// rather than a nested selector ("a:hover {", "&.on {")?
function looksValue(text, i, loose) {
  const rest = text.slice(i);
  const m = /^(--?[\w-]+|[A-Za-z][\w-]*)\s*:(?!:)/.exec(rest);
  if (!m) return false;
  if (loose) return true;
  const brace = rest.indexOf("{");
  const semi = rest.indexOf(";");
  return brace < 0 || (semi >= 0 && semi < brace) || m[1].startsWith("--");
}

// --- Markdown ------------------------------------------------------------------------
// Headings, list marks, quotes, `code`, links, and a fenced block drawn in its
// own language. State: "" or "f\u0001<fence>\u0001<lang>\u0001<inner state>".

function scanMarkdown(text, state, out) {
  const n = text.length;
  if (state.startsWith("f\u0001")) {
    const [, fence, lang, inner] = state.split("\u0001");
    if (text.trim().startsWith(fence) && !text.trim().slice(fence.length).trim()) { out.mark("m", 0, n); return ""; }
    const scan = SCANNERS[lang];
    if (!scan) return state;
    const next = scan(text, inner, out);
    return `f\u0001${fence}\u0001${lang}\u0001${next}`;
  }
  const fence = /^(\s*)(`{3,}|~{3,})\s*([\w+#.-]*)/.exec(text);
  if (fence) {
    out.mark("m", fence[1].length, n);
    const tag = fence[3].toLowerCase();
    return `f\u0001${fence[2]}\u0001${FENCES[tag] || languageOf(`x.${tag}`) || languageOf(tag)}\u0001`;
  }
  const heading = /^\s{0,3}#{1,6}(?:\s|$)/.exec(text);
  if (heading) { out.mark("h", 0, n); return ""; }
  if (/^\s{0,3}(?:[-*_]\s*){3,}$/.test(text)) { out.mark("m", 0, n); return ""; }
  let i = 0;
  const lead = /^(\s*)(?:(>\s?)+|[-*+]\s|\d+[.)]\s)/.exec(text);
  if (lead) { out.mark(lead[2] ? "m" : "k", lead[1].length, lead[0].length); i = lead[0].length; }
  const inline = /`[^`]+`|\[[^\]]*\]\([^)]*\)|<https?:\/\/[^>]+>|https?:\/\/[^\s)>]+/g;
  inline.lastIndex = i;
  let m;
  while ((m = inline.exec(text))) {
    if (m[0][0] === "`") out.mark("s", m.index, m.index + m[0].length);
    else if (m[0][0] === "[") {
      const close = m[0].indexOf("](");
      out.mark("p", m.index + close + 2, m.index + m[0].length - 1);
    } else out.mark("p", m.index, m.index + m[0].length);
  }
  return "";
}

// --- YAML ----------------------------------------------------------------------------

function scanYaml(text, state, out) {
  const n = text.length;
  if (/^(?:---|\.\.\.)\s*$/.test(text)) { out.mark("m", 0, n); return ""; }
  // A block scalar (key: |) holds text until a line indents no deeper.
  if (state) {
    const indent = parseInt(state, 10);
    const own = /^\s*/.exec(text)[0].length;
    if (!text.trim() || own > indent) { out.mark("s", 0, n); return state; }
  }
  let i = 0;
  const lead = /^(\s*)(-\s+)*/.exec(text);
  if (lead[2]) out.mark("k", lead[0].search(/-/), lead[0].length);
  i = lead[0].length;
  const key = /(?:"[^"]*"|'[^']*'|[^\s#:"'][^#:]*?)(?=\s*:(?:\s|$))/y;
  key.lastIndex = i;
  const k = key.exec(text);
  let next = "";
  if (k) {
    out.mark("p", i, i + k[0].length);
    i += k[0].length;
    i = text.indexOf(":", i) + 1;
    if (/^\s*[|>][-+]?\d*\s*(?:#.*)?$/.test(text.slice(i))) next = String(/^\s*/.exec(text)[0].length);
  }
  while (i < n) {
    const ch = text[i];
    if (ch === " " || ch === "\t") { i++; continue; }
    if (ch === "#" && (i === 0 || /\s/.test(text[i - 1]))) { out.mark("c", i, n); return next; }
    if (ch === '"' || ch === "'") {
      const end = stringEnd(text, i + 1, { close: ch, escape: ch === '"' });
      const to = end < 0 ? n : end;
      out.mark("s", i, to);
      i = to;
      continue;
    }
    if (ch === "&" || ch === "*" || ch === "!") {
      const m = /[&*!]{1,2}[^\s,\]}]+/y;
      m.lastIndex = i;
      const hit = m.exec(text);
      if (hit) { out.mark("m", i, i + hit[0].length); i += hit[0].length; continue; }
    }
    const scalar = /[^\s,\]}#][^,\]}#]*?(?=\s*(?:[,\]}]|\s#|$))/y;
    scalar.lastIndex = i;
    const m = scalar.exec(text);
    if (m) {
      const v = m[0];
      if (/^(?:true|false|yes|no|on|off|null|~|[+-]?(?:\d[\d_]*\.?\d*(?:e[+-]?\d+)?|\.\d+|0x[\da-f]+|\.inf|\.nan))$/i.test(v)) out.mark("n", i, i + v.length);
      i += v.length;
      continue;
    }
    i++;
  }
  return next;
}

// --- INI, TOML, .env, .gitconfig, .properties --------------------------------------

function scanIni(text, state, out) {
  const n = text.length;
  if (state) {
    const close = text.indexOf(state);
    if (close < 0) { out.mark("s", 0, n); return state; }
    out.mark("s", 0, close + 3);
    return "";
  }
  const lead = /^\s*/.exec(text)[0].length;
  if (text[lead] === "#" || text[lead] === ";") { out.mark("c", lead, n); return ""; }
  if (text[lead] === "[") { out.mark("t", lead, text.lastIndexOf("]") + 1 || n); return ""; }
  const key = /\s*(?:export\s+)?([\w."'@-]+(?:\s+[\w."'-]+)*)\s*(?==|:(?!\/))/y;
  key.lastIndex = 0;
  const k = key.exec(text);
  let i = 0;
  if (k) { const at = k[0].indexOf(k[1]); out.mark("p", at, at + k[1].length); i = k[0].length + 1; }
  while (i < n) {
    const ch = text[i];
    if (ch === "#" && /\s/.test(text[i - 1] || " ")) { out.mark("c", i, n); return ""; }
    if (ch === '"' || ch === "'") {
      const triple = text.startsWith(ch.repeat(3), i) ? ch.repeat(3) : "";
      if (triple) {
        const close = text.indexOf(triple, i + 3);
        if (close < 0) { out.mark("s", i, n); return triple; }
        out.mark("s", i, close + 3);
        i = close + 3;
        continue;
      }
      const end = stringEnd(text, i + 1, { close: ch, escape: ch === '"' });
      const to = end < 0 ? n : end;
      out.mark("s", i, to);
      i = to;
      continue;
    }
    const m = /(?:true|false|[+-]?\d[\d_:.TZ+-]*(?:e[+-]?\d+)?)(?![\w.-])/y;
    m.lastIndex = i;
    const hit = (i === 0 || /[\s=,[{:]/.test(text[i - 1])) && m.exec(text);
    if (hit) { out.mark("n", i, i + hit[0].length); i += hit[0].length; continue; }
    i++;
  }
  return "";
}

// --- a diff, a patch ------------------------------------------------------------------

function scanDiff(text, state, out) {
  const n = text.length;
  if (/^(?:diff |index |\+\+\+ |--- |new file|deleted file|similarity|rename |old mode|new mode)/.test(text)) out.mark("m", 0, n);
  else if (text.startsWith("@@")) out.mark("p", 0, n);
  else if (text[0] === "+") out.mark("a", 0, n);
  else if (text[0] === "-") out.mark("d", 0, n);
  return "";
}

// --- Makefile, Dockerfile: their own lines, shell inside ---------------------------

// A recipe is shell, and make's own $(NAME), $@, $< in it are variables.
const MAKE_VAR = /\$(?:\([^)]*\)|\{[^}]*\}|[@<^+?*%])/y;
const makeRecipe = codeScanner({
  ...SPECS.shell,
  extra: (text, i, out) => {
    if (text[i] !== "$") return null;
    MAKE_VAR.lastIndex = i;
    const m = MAKE_VAR.exec(text);
    if (!m) return shellExtra(text, i, out);
    out.mark("v", i, i + m[0].length);
    return { end: i + m[0].length };
  },
});

function scanMake(text, state, out) {
  if (text[0] === "\t") return makeRecipe(text, state, out);
  const n = text.length;
  const lead = /^\s*/.exec(text)[0].length;
  if (text[lead] === "#") { out.mark("c", lead, n); return ""; }
  const directive = /^\s*(-?include|ifeq|ifneq|ifdef|ifndef|else|endif|define|endef|export|override|\.PHONY)\b/.exec(text);
  let i = 0;
  if (directive) { const at = directive[0].indexOf(directive[1]); out.mark("k", at, at + directive[1].length); i = directive[0].length; }
  const assign = /^([\w.-]+)\s*(?::=|::=|\?=|\+=|!=|=)/.exec(text.slice(i));
  const target = /^([^:#=\t][^:#=]*?)\s*::?(?!=)/.exec(text.slice(i));
  if (assign) { out.mark("p", i, i + assign[1].length); i += assign[0].length; }
  else if (target && !directive) { out.mark("f", i, i + target[1].length); i += target[0].length; }
  const v = /\$(?:\([^)]*\)|\{[^}]*\}|[@<^+?*%])/g;
  v.lastIndex = i;
  let m;
  while ((m = v.exec(text))) {
    const comment = text.indexOf(" #", out.at);
    if (comment >= 0 && comment < m.index) break;
    out.mark("v", m.index, m.index + m[0].length);
  }
  const comment = text.indexOf(" #", out.at);
  if (comment >= 0) out.mark("c", comment + 1, n);
  return "";
}

const DOCKER = words("from run cmd label maintainer expose env add copy entrypoint volume user workdir arg onbuild stopsignal healthcheck shell as");
function scanDocker(text, state, out) {
  const lead = /^\s*/.exec(text)[0].length;
  if (!state) {
    if (text[lead] === "#") { out.mark("c", lead, text.length); return ""; }
    const m = /^\s*([A-Za-z]+)\b/.exec(text);
    if (m && DOCKER.has(m[1].toLowerCase())) {
      out.mark("k", lead, lead + m[1].length);
      const rest = new Line(text.slice(m[0].length));
      const next = SCANNERS.shell(rest.text, "", rest);
      out.plain(m[0].length);
      out.html += rest.done();
      out.at = text.length;
      return text.trimEnd().endsWith("\\") ? `c${next}` : next;
    }
  }
  const inner = state.startsWith("c") ? state.slice(1) : state;
  const next = SCANNERS.shell(text, inner, out);
  return text.trimEnd().endsWith("\\") ? `c${next}` : next;
}

// --- the languages ---------------------------------------------------------------------

const SCANNERS = {
  lisp: scanLisp, markup: scanMarkup, css: scanCss, markdown: scanMarkdown, yaml: scanYaml, ini: scanIni,
  diff: scanDiff, make: scanMake, docker: scanDocker,
};
for (const [lang, spec] of Object.entries(SPECS)) SCANNERS[lang] = codeScanner(spec);

// A language's name as the reader's head shows it.
export const LANGUAGE_NAMES = {
  js: "JavaScript", json: "JSON", python: "Python", go: "Go", rust: "Rust", c: "C", java: "Java", csharp: "C#", swift: "Swift", php: "PHP",
  ruby: "Ruby", lua: "Lua", zig: "Zig", shell: "Shell", sql: "SQL", proto: "Protobuf", hcl: "HCL", lisp: "Lisp", markup: "HTML", css: "CSS",
  markdown: "Markdown", yaml: "YAML", ini: "INI", diff: "Diff", make: "Makefile", docker: "Dockerfile",
};

const EXTENSIONS = {
  js: "js mjs cjs jsx ts tsx mts cts es6", json: "json jsonc json5 jsonl ndjson webmanifest babelrc eslintrc prettierrc code-workspace",
  python: "py pyi pyw pyx gyp", go: "go", rust: "rs", c: "c h cc cpp cxx c++ hpp hh hxx h++ ino m mm cu cuh",
  java: "java kt kts scala sc groovy gradle dart", csharp: "cs csx", swift: "swift", php: "php phtml", ruby: "rb rake gemspec ru",
  lua: "lua", zig: "zig zon", shell: "sh bash zsh ksh fish bats command", sql: "sql psql ddl", proto: "proto", hcl: "tf tfvars hcl nomad",
  lisp: "lisp lsp cl asd el scm ss rkt clj cljs cljc edn fnl lfe sld", markup: "html htm xhtml xml svg vue svelte xsl xslt plist csproj fsproj props targets xaml rss atom",
  css: "css scss sass less pcss", markdown: "md markdown mdx mkd", yaml: "yaml yml", ini: "toml ini cfg conf properties env editorconfig gitconfig gitmodules npmrc desktop service",
  diff: "diff patch rej", make: "mk mak make", docker: "dockerfile containerfile",
};
const BY_EXTENSION = {};
for (const [lang, list] of Object.entries(EXTENSIONS)) for (const ext of list.split(" ")) BY_EXTENSION[ext] = lang;
// Names that say their language whole.
const BY_NAME = {
  makefile: "make", gnumakefile: "make", dockerfile: "docker", containerfile: "docker", ".bashrc": "shell", ".bash_profile": "shell",
  ".zshrc": "shell", ".zprofile": "shell", ".profile": "shell", ".envrc": "shell", pkgbuild: "shell", ".env": "ini", ".gitconfig": "ini",
  ".editorconfig": "ini", ".npmrc": "ini", ".gitignore": "ini", ".dockerignore": "ini", "cmakelists.txt": "", gemfile: "ruby", rakefile: "ruby",
  "cargo.lock": "ini", "poetry.lock": "ini", "package-lock.json": "json", jenkinsfile: "java", vagrantfile: "ruby", ".sbclrc": "lisp", ".emacs": "lisp",
};
// A fenced block's tag (```python) where it is not an extension.
const FENCES = {
  javascript: "js", typescript: "js", node: "js", python: "python", python3: "python", golang: "go", rust: "rust", shell: "shell", console: "shell",
  shellsession: "shell", sh: "shell", yaml: "yaml", html: "markup", xml: "markup", svg: "markup", lisp: "lisp", "common-lisp": "lisp", commonlisp: "lisp",
  elisp: "lisp", "emacs-lisp": "lisp", clojure: "lisp", scheme: "lisp", racket: "lisp", json: "json", toml: "ini", ini: "ini", dotenv: "ini", sql: "sql",
  diff: "diff", patch: "diff", dockerfile: "docker", docker: "docker", makefile: "make", make: "make", ruby: "ruby", csharp: "csharp", "c#": "csharp",
  cpp: "c", "c++": "c", objc: "c", objectivec: "c", kotlin: "java", scala: "java", groovy: "java", dart: "java", swift: "swift", php: "php", lua: "lua",
  zig: "zig", css: "css", scss: "css", less: "css", markdown: "markdown", hcl: "hcl", terraform: "hcl", proto: "proto", protobuf: "proto",
};
const SHEBANG = [[/\b(?:ba|z|k|da|fi)?sh\b/, "shell"], [/\bpython/, "python"], [/\b(?:node|deno|bun)\b/, "js"], [/\bruby\b/, "ruby"], [/\bphp\b/, "php"],
  [/\b(?:sbcl|ccl|ecl|clisp|nodecode|guile|racket|bb|clojure)\b/, "lisp"], [/\blua/, "lua"]];

// NAME's language (a key of LANGUAGE_NAMES), or "" when it has none to colour:
// by its whole name, then its extension, then the #! line TEXT opens with.
export function languageOf(name, text = "") {
  const lower = String(name || "").toLowerCase();
  const base = lower.split("/").pop();
  if (base in BY_NAME) return BY_NAME[base];
  if (base.startsWith("dockerfile.") || base.endsWith(".dockerfile")) return "docker";
  if (base.startsWith(".env.")) return "ini";
  const dot = base.lastIndexOf(".");
  if (dot >= 0 && BY_EXTENSION[base.slice(dot + 1)]) return BY_EXTENSION[base.slice(dot + 1)];
  if (text.startsWith("#!")) {
    const line = text.slice(0, text.indexOf("\n") >>> 0);
    for (const [pattern, lang] of SHEBANG) if (pattern.test(line)) return lang;
  }
  return "";
}

// One line of LANG in colour, starting in STATE: [html, the state it hands on].
export function paintLine(lang, text, state = "") {
  if (text.length > LONG) return [esc(text), state];
  const out = new Line(text);
  const next = SCANNERS[lang](text, state, out);
  return [out.done(), next || ""];
}

// A text kept in colour as it changes: set(text) takes it whole (cheap when
// little changed), html(from, to) draws lines FROM..TO-1 joined by newlines,
// painting only what it has not painted for that line in that state before.
// version moves on whenever set changed anything.
export function paintFor(lang) {
  if (!SCANNERS[lang]) return null;
  const memo = new Map();
  let lines = [];
  let rows = []; // per line: [html, state after], good below `ready`
  let ready = 0;
  return {
    lang,
    version: 0,
    get count() { return lines.length; },
    set(text) {
      const next = text.split("\n");
      const same = Math.min(next.length, lines.length);
      let d = 0;
      while (d < same && next[d] === lines[d]) d++;
      if (d === next.length && d === lines.length) return false;
      lines = next;
      ready = Math.min(ready, d);
      rows.length = ready;
      this.version++;
      // A memo far bigger than the text holds lines long gone.
      if (memo.size > lines.length * 3 + 2000) memo.clear();
      return true;
    },
    html(from, to) {
      to = Math.min(to, lines.length);
      for (let i = ready; i < to; i++) {
        const state = i ? rows[i - 1][1] : "";
        const line = lines[i];
        if (line.length > LONG) { rows[i] = [esc(line), state]; continue; }
        const key = `${state}\u0000${line}`;
        let row = memo.get(key);
        if (!row) { row = paintLine(lang, line, state); memo.set(key, row); }
        rows[i] = row;
      }
      ready = Math.max(ready, to);
      let html = "";
      for (let i = Math.max(0, from); i < to; i++) html += (i > from ? "\n" : "") + rows[i][0];
      return html;
    },
  };
}
