# nodecode-forum-index

A directory of the live hacking, fraud and security forums: who trades in what, where it lives, and how careful the list says to be. The list is a Dread /d/hacking post of 10 November 2026, relayed on X by [@solminingpunk](https://x.com/ImVibhek/status/2120370962025599002); the post it answers, with the four tables it screens off, is [here](https://x.com/solminingpunk/status/2120285444180959571).

## Get it

Tell the agent "add the forum index cell", or run `nodecode add nc://forum-index`.

## Use it

Ask in your own words: *which forums trade in carding*, *what is Exploit*, *where do people sell accounts*, *which of these have been compromised*. The model reads the same list you do.

- `forum_index_list` - every forum, or those of one category (`fraud`, `leaks`, `hacking`, `cracking`, `malware`, `seo`, `development`, `accounts`) or one tier (`established`, `rising`, `other`).
- `forum_index_get` - one forum whole: clearnet and onion links, categories, the poster's note. A fragment of the name is enough.
- `forum_index_search` - any word, over names, links, categories and notes.
- `forum_index_summary` - how many, by category and tier, and where the list came from.

In chat: `/forum-index` for the summary, `/forum-index categories` for the legend, `/forum-index fraud` for one category or tier, `/forum-index carder` to search.

From Lisp, with the cell on:

```lisp
(nodecode-forum-index:forums-matching :category "fraud")
(nodecode-forum-index:forum-by-name "lolz")
(nodecode-forum-index:forum-block (nodecode-forum-index:forum-by-name "Exploit"))
```

## What it does not do

It does not fetch, crawl or verify anything: the directory answers from the list in `data.lisp`, with no request, key or account. Open a link yourself, in a browser or through Tor - that is what the links are for.

## Read it with care

It is a snapshot of 10 November 2026, read on 11 November. One part of the poster's list is not in it: the rows under their `Dangerous` heading sat below the fold of the four screenshots this was built from, so `/forum-index dangerous` says so rather than implying the list has no dangerous forums. The post itself completes it.

These are places where people buy and sell stolen data, malware, accounts and carding, and none of them is vouched for - the poster says so in the post, and this cell repeats it with every answer. A forum on the list can be seized, dead, or run by the people it claims to protect you from. The tiers are the poster's: *established* means around a long time and active, *rising* means concerns have been raised (Darkforums was compromised in 2023), and the rest are listed without one.

## Set it up

Nothing is required: the directory ships with the cell.

- `forum-index.tor_socks` - where a local Tor answers (default `127.0.0.1:9050`), only so the cell can say how to open an onion link.

## Credit

The list is the Dread poster's, relayed on X by [Vibhek Soni (@solminingpunk)](https://x.com/solminingpunk); the code is MIT, ours. See `NOTICE`.
