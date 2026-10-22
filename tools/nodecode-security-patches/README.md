# nodecode-security-patches

The last two weeks' breaches, each with the fix its vendor shipped, and a check that
says whether *this* machine or site still needs it.

```
nodecode add nc://nodecode-security-patches
```

Then ask nodecode to "check my site for the latest breaches", or:

```lisp
(security:report :dir "/var/www/site" :host "example.com")  ; what to patch first
(security:show "php2shell")                                  ; the fix, the stopgap, the signs
```

`/security`, `/security check /var/www/site example.com`, `/security show ID` do the same
from any shell or chat room.

## What it checks

Each advisory in [`advisories.json`](advisories.json) carries its own checks:

| kind | reads |
| --- | --- |
| `version` | a program's version (`php`, `node`, GitLab's VERSION file, Plex's package), judged per release line |
| `npm` | a package in the site folder's `package-lock.json` / `node_modules` |
| `composer` | a package in the site folder's `composer.lock` |
| `wp-plugin` | a WordPress plugin's `Version:` header |
| `command` | a command's output (e.g. the site certificate's issuer) |
| `files` | known signs of compromise on disk |

Advisories with no machine check (a platform breach: rotate your keys) are listed as
`READ`. Distro builds that backport a fix under the old version number are recognised
from the package changelog where the advisory says how.

## Staying current

An installed cell is pinned to a commit and never moves, so the advisories are not
baked in: the cell reads `advisories.json` from this repository's `main` branch at
start and every `refresh_hours` (6 by default). A fix to the feed reaches every
installed copy when it is committed. The copy shipped in the folder answers when the
network does not, and the report's first line always says which copy answered and
how old it is.

```jsonc
"security-patches": { "feed_url": "https://...", "refresh_hours": 6 }  // both optional
```

## Patching

The cell finds and explains; the person decides. Upgrades need sudo and restart
services, so nodecode states each command and asks before running it. A sign of
compromise under an advisory means stop and say so first: patching does not remove a
backdoor.

MIT.
