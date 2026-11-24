# nodecode-chmod-ref

Chmod Ref: a file mode, a chmod change or a umask explained in octal, `ls -l` and plain words. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

## Get it

Tell the agent "add the chmod-ref cell", or run `nodecode add nc://chmod-ref`.

## Use it

Ask about a mode and the model can call `chmod_explain` with the value it read, or ask for `(nodecode-chmod-ref:explain "755")` through eval. Typing `/chmod 755` in the shell answers the same thing, and `/chmod` alone lists the examples.

```
755                     an octal mode (0755, 0o755 and 4-digit modes like 4755 too)
-rw-r--r--              ls -l text, with or without the file type letter (and + . @ after it)
100644                  an st_mode with the file type, as stat and git write it
644 u+x,go-w            a symbolic change applied to a mode, step by step
u+x,go-w                a symbolic change on its own, in words
100644 100755           what changes between two modes
umask 027               what new files and directories get
```

The answer for `100644 100755` and then `u+x,go-w`, as it comes back:

```
Mode 100644 -> 100755
  before    rw-r--r--  (100644)
  after     rwxr-xr-x  (100755)
  adds      execute for owner, group and others
Notes
  - this is the change git shows as old mode / new mode when a file's execute bit flips

Mode 100755 (rwxr-xr-x), a regular file
  octal     100755
  st_mode   100755 (file type and permission bits, as stat and git write them)
  ls -l     -rwxr-xr-x
  owner     read, write, execute
  group     read, execute
  others    read, execute
  special   none

chmod u+x,go-w
  u+x       add execute for the owner
  go-w      remove write from the group and others
  on 644    rwxr--r--  (744)
  on 755    rwxr-xr-x  (755)
Notes
  - give a starting mode to see one result: 644 u+x,go-w
```

Symbolic changes are applied the way GNU chmod applies them, with a umask of 0: `u`, `g`, `o` and `a`, the `+`, `-` and `=` operators (several in one clause, like `u+r-w`), `r`, `w`, `x`, `X`, `s` and `t`, and copies like `u=g`. On a directory, setuid and setgid are kept by `=` unless `s` is named, as GNU chmod does. A value that is not a mode answers why (`"788" is not octal: 8 is not an octal digit (0-7)`), and a wrong value in a tool call is recorded as a failed call.

What it says beyond the rows, when it applies:

- **Special bits**: what setuid, setgid and sticky do on a program and on a directory, that they are ignored where they are, and an `S` or `T` that means the bit is set without the matching execute.
- **Who can do what**: a mode no one but root can use, one everyone can change, one private to the owner (what ssh wants for keys), others able to write in a directory without the sticky bit, and a class with rights the class before it lacks.
- **Files and directories**: execute without read, a file read-only for its owner, and on a directory write without execute or read without execute.
- **Types and git**: a symbolic link's own mode, git's 160000 submodule entry, a file mode git would not keep, and the `+`, `.` and `@` after `ls -l` bits.

Nothing on disk is read and nothing is changed: a value is a string, the answer is a string.

## What is in it

| File | What it is |
| --- | --- |
| `modes.lisp` | The engine: the readers, the change applied, the umask, the notes and the wording. Common Lisp only. |
| `cell.lisp` | The tool `chmod_explain`, `/chmod`, and the manual at `(help :chmod-ref)`. |
| `test/` | The wiring against a real start and stop, and the answers for each kind of value. |

The one setting is optional: `max_target_chars` (default 200) bounds how long a value is read.

## Licence

MIT. The original is Copyright (c) 2026 olric3015: see `LICENSE` and `NOTICE`.
