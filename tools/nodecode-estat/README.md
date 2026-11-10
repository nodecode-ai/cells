# nodecode-estat

e-Stat Japan Statistics: Japan's official government statistics, searched and read by name. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-estat](https://github.com/TakeshiTGAL/hermes-plugin-estat) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

Three read-only tools over **e-Stat** (政府統計の総合窓口), the portal of the Statistics Bureau of Japan: find a table, look at what it holds, then read its figures with the unit, the survey date and a citation you can paste. Ask in plain words, in English or Japanese; the cell turns the names it has (東京都, 2020年, 令和2年, latest, 女) into the codes e-Stat wants.

## Get it

Tell the agent "add the estat cell", or run `nodecode add nc://estat`.

## Set it up

e-Stat asks every caller for a free application ID:

1. Register at e-Stat (https://www.e-stat.go.jp/mypage/user/preregister).
2. Open My Page (マイページ) -> API機能（アプリケーションID発行）, enter any name and any URL, and press 発行.
3. Put the ID in the cell's settings, `estat.app_id`, or leave that empty and export it as `ESTAT_APP_ID` before Nodecode starts.

A section naming no ID refuses to start and says so. Nothing else is needed: no dependency, no extra program.

## Use it

| Tool | What it answers |
| --- | --- |
| `estat_search_tables` | Tables whose title matches Japanese words: id, title, survey, publisher, year, area level. |
| `estat_table_info` | One table's dimensions (地域, 時点, 表章項目, 男女, 産業) and the names a call may use. |
| `estat_get_data` | The numbers for the areas, periods and categories named, with the citation and e-Stat's credit. |

`/estat 人口 世帯数` runs the same search in the conversation; `/estat 0003445078` (a 10-digit id) opens that table. `(help :estat)` is the manual.

```
You:    What was Tokyo's population in the 2020 census?
Agent:  estat_search_tables("国勢調査 人口") -> 0003410379
then    estat_get_data("0003410379", area="東京都", time="2020年")
        東京都 | 2020年 | 総数 = 14047594 人
        出典：政府統計の総合窓口(e-Stat)「令和2年国勢調査 ...」(総務省)
```

A name that matches nothing, or several things (府中市 is in two prefectures), answers with the closest names and their codes, so the next call repairs itself. A selection wider than `max_rows` says how many values it left out instead of truncating silently, and a preliminary table (速報) is flagged.

## What it reaches

`https://api.e-stat.go.jp` and nothing else: `getStatsList`, `getMetaInfo`, `getStatsData`. The application ID rides in the query string because e-Stat requires it there, and the cell removes it from every message it shows. Table metadata is cached in memory for 30 minutes (16 tables at most); nothing is written to disk, no subprocess runs, and no other setting is read.

MIT, the original's licence (see LICENSE and NOTICE).
