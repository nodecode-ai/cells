# nodecode-culture

Global Business Culture: country culture briefs and home-vs-host gap analysis for cross-border business dealings. A Nodecode rewrite of [pswong661-eng/hermes-global-business-culture](https://github.com/pswong661-eng/hermes-global-business-culture) by [pswong661-eng](https://github.com/pswong661-eng).

It is bundled research data and arithmetic, not prompt text: 57 economies scored
on Meyer's eight Culture Map scales and Hofstede's six dimensions, grouped into 11
GLOBE/Lewis regional playbooks, with field notes on each country and on where it
diverges from its region. Ask it before a negotiation abroad, a first meeting, a
management decision or an email to a counterpart, and it answers the same text
every time, offline.

## Get it

Tell the agent "add the culture cell", or run `nodecode add nc://culture`.

## Set it up

Nothing is required. The `culture` section of the config tunes it:

```jsonc
{
  "culture": {
    "home_country": "Germany",   // the country you work from
    "max_gaps": 5,               // how many ranked gaps a gap analysis answers
    "max_highlights": 5,         // how many highlights, etiquette points and taboos a brief carries
    "include_hofstede": true    // Hofstede's six numbers in a brief
  }
}
```

`home_country` is the one worth setting: with it, every gap analysis knows your
side of the comparison and a call only names the counterpart's country.

## Use it

Three tools, the same names the original gives its harness:

| Tool | What it answers |
| --- | --- |
| `culture_brief {country, task?}` | How trust is built there, how decisions are really made, contracts, communication, etiquette, taboos, the country's own field notes, and a task hint when a task is named. |
| `culture_gap_analysis {host_country, home_country?, task?}` | The eight Meyer scale gaps between your country and theirs, ranked by the gap that matters most and weighted for the task, each with the misread it risks and the move that answers it; the Hofstede dimensions that differ most; one line of synthesis. |
| `culture_countries {query?}` | What is covered, optionally narrowed by a word from a name or a region. |

And `/culture Japan negotiation` for the operator, or from eval:

```lisp
(culture:brief "Japan" :task "negotiation")
(culture:gap-analysis "Japan" :home "Germany" :task "negotiation")
(culture:countries "asia")
```

Names resolve the way the original resolves them: exact (`Japan`), alias (`USA`,
`UK`, `UAE`, `Korea`, `Holland`, `Czech Republic`, `Burma`, `Lao PDR`, `PRC`,
`HK`, `KSA`, `RSA`, `Turkiye`), containment (`south korean market`) or a close
spelling (`Japon`); what it cannot place comes back as suggestions, never a guess.

`(help :culture)` carries the doctrine the original's skill teaches: name the
framework when a brief drives a decision, calibrate to the person in front of you,
and never treat a country average as a person -- ASEAN is not one culture, nor
Latin America, nor the Gulf.

## Credit

The data, the scales, the task weights, the misread risks, the adaptation moves
and the regional playbooks are pswong661-eng's, from the Hermes plugin at commit
`49218a35f670ceae6f79dc07d40536b19da23cc6`, MIT. See [NOTICE](NOTICE) for what
moved and what did not; [LICENSE](LICENSE) is the original's, unchanged.
