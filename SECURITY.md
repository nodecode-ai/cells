# Security

## Reporting a vulnerability

Report it privately, never in a public issue: open
[a private advisory](https://github.com/nodecode-ai/cells/security/advisories/new)
(the repository's Security tab, then "Report a vulnerability"). Only the
maintainers see it, and the fix is discussed there until it ships.

A report that names the commit you read, the file and line, and what an
attacker gains from it is the fastest to act on.

A vulnerability in Nodecode itself, not in a cell, goes to
[nodecode's advisories](https://github.com/nodecode-ai/nodecode/security/advisories/new).

## What is in scope

Everything in this repository: the tools, rooms and providers Nodecode installs
as cells, the browser extension under `tools/nodecode-chrome/`, and the
cyber-security cell's feed.

Some limits are by design and documented in
[Safety and trust](https://nodecode.ai/docs/security): Nodecode runs
with your user's rights and is not a sandbox. A report showing that a cell
crosses one of those documented lines is still welcome.

## How a fix reaches you

A fix lands on `main`. An installed cell is pinned to a commit, so it
reaches an install with the Nodecode release that moves that pin.
