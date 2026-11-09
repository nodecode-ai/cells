# nodecode-source-tray

Source Tray: the URLs the agent actually opened through a web or browser call, in one list. A Nodecode rewrite of [aydnOktay/hermes-source-tray](https://github.com/aydnOktay/hermes-source-tray) by [aydnOktay](https://github.com/aydnOktay).

## Get it

Tell the agent "add the source-tray cell", or run:

```sh
nodecode add nc://source-tray
```

Then restart, or run `(restart-cells "nodecode-source-tray")` from eval. Nothing is
required in the config: the cell runs on its defaults the moment its folder is
there.

## Use it

Search the web, fetch a page or drive a browser, then ask what you read or for
your sources. The tray records every URL those calls carried -- in their own
arguments and in their answers -- and answers as a numbered list of links, each
under the title the result gave it.

The model has two tools: `source_tray_list` (the list as JSON, plus a markdown
bullet list unless the call asks for items alone) and `source_tray_clear`. The
operator has `/sources` for the same list, `/sources list`, and
`/sources clear`; typed alone it also opens a panel whose one row clears the
tray.

A URL appears once: opening it again moves its title, call and time up to date
in place, and the list keeps the newest `max_items` (40 unless set). The calls
it reads default to `web_search`, `web_extract`, `web_fetch`,
`browser_navigate` and `browser_open`; name your own in the section's
`tools`. Every list is the session's own, so two sessions never share one and
a restart keeps it. Nothing here searches the web, fetches a page, or leaves
the machine.
