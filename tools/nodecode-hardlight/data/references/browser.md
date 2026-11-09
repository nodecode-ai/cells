# Browser evidence: inspect the delivered surface

## Choose the right environment

Use the project's documented dev/build workflow when exercising its application. Never install browsers, alter security settings, submit transactions or change production data without appropriate scope. Use local fixtures or an authorized test account for destructive and permission-sensitive paths.

Hardlight's optional local runner is a bounded evidence helper, not a full browser agent. Its pure API is `hardlight.browser.audit_page(target, output_dir, executable=None)`. It requires an explicit local file/file URI or literal-loopback HTTP target, an explicit output directory, optional Playwright and an already installed Chrome/Chromium. It does not install a browser automatically. External requests are blocked: remote fonts, analytics and CDN images may therefore fail even if a connected production site would load them. Use offline assets or report those limitations; do not disable the guard to make a screenshot look complete.

The runner captures initial states at wide 1440×1000, narrow 390×844 and narrow reduced motion. It can report bounded DOM/console observations and screenshot handles. It does not complete tasks, inspect all frames, certify accessibility, evaluate animation rhythm or establish aesthetic quality. Review its coverage, blocked requests and errors before interpreting findings.

## Make a proof matrix before capture

| Surface | Initial evidence | Engaged evidence | Additional path |
| --- | --- | --- | --- |
| Reading | Wide/narrow opening and contents | A real section/link target | Direct anchor, Back, long title, zoom |
| Workspace | Correct initial scope and work list | Filtered list, selected object or editor | Empty/error, save/retry, list/detail return |
| Event/commerce | Opening artifact plus practical action | Program/variant selection | Date/quantity availability and honest booking boundary |
| Navigation | Current place before opening | Wide disclosure and narrow sheet | Tab, Escape, focus return, direct child entry |

Capture the real initial state before a selected, scrolled or zoomed state. Use stable framing and names that identify viewport and state. A beautiful detail screenshot cannot stand in for an opening viewport the visitor never sees. A preloader at 100%, a faded incomplete image or a consent overlay is evidence of that captured state—not proof of the intended completed design.

## Exercise behavior beyond the runner

Use the available browser interaction tools or the project's browser tests to:
1. Follow the principal route with pointer and then keyboard; check literal destinations and current-location feedback.
2. Open/close disclosures and dialogs, reach every interactive item and verify Escape/focus return. Ensure hidden controls are not tabbable.
3. Complete a real reversible task, then exercise validation and transport failure with safe fixtures. Read back the result.
4. Paste a detail URL, refresh, go Back/Forward and confirm preserved filters and scroll where expected.
5. Inspect narrow reflow, long labels, zoom, touch equivalents and fixed bars. Check a representative intermediate width when topology changes.
6. Enable reduced motion and repeat the expressive interaction; check a recording when timing matters.
7. Read console/page errors, missing image/font states and network failures. A rendered page can still have hydration or interaction failures.

If a tool fails, preserve existing evidence, retry through an appropriate available browser path and mark the unresolved check. Do not replace missing execution with a plausible screenshot narrative.

## Interpret findings honestly

No horizontal overflow at two sampled widths is a useful bounded observation; it is not usability certification. Computed text checks may not understand transparency, image backgrounds, pseudo-elements, shadow DOM, canvas or complex compositing. Unknown contrast stays unknown until checked appropriately. Static inspection, graph reachability, contrast ratios, screenshot review, keyboard testing and audience research each answer different questions.

For performance, separate laboratory diagnostics from field experience. Core Web Vitals field assessment uses the 75th percentile of real visits; one local run or screenshot cannot establish a site's field status. Capture timing, hardware, network and build context when comparing performance, and do not invent a benchmark from a reference gallery.

**Handoff:** paths/handles to owned screenshots and reports, routes/states actually exercised, errors and coverage gaps, plus concise visual judgments tied to the brief. Keep third-party reference screenshots private unless licensed for publication.
