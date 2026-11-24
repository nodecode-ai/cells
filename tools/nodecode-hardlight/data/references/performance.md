# Performance: spend bytes and work on the subject

## Make the budget concrete

Choose the task, device/network conditions and implementation build before setting a budget. List the assets needed for the initial view, their bytes/dimensions, font families/weights, optional effects and when they load. Distinguish a proposed budget from an observed result. A meaningful product specimen often communicates more than a large decorative animation; choose the asset that earns its cost.

Core Web Vitals currently cover loading (LCP), responsiveness (INP) and stability (CLS). Good thresholds are LCP at most 2.5 seconds, INP at most 200 milliseconds and CLS at most 0.1, assessed at the 75th percentile of real visits, with mobile and desktop segmentation. These are field outcomes. Local captures, synthetic interactions and a Lighthouse score do not establish that a site meets them for its audience.

## Images and fonts

Give images intrinsic dimensions or an aspect ratio to reserve space. Use appropriately sized local or authorized assets, responsive sources where needed, and useful alternative text. Do not lazy-load the likely LCP image; defer below-the-fold imagery. Preload only the small number of assets known to be critical. Measure priority decisions rather than adding fetch priority everywhere.

Use a licensed WOFF2 font when it improves the design; a system-font fallback is a complete design choice. Subset only with required scripts and glyphs retained. Variable fonts can reduce separate files, but are not always smaller. Define real fallback families, appropriate `font-display` behavior and, when measured, fallback metrics such as `size-adjust` and ascent/descent overrides. Avoid copying metric values from a different font. Inspect both the loaded font and a failed/slow-font fallback before claiming stable text.

## Interaction and effects

Ship the primary task before optional canvas, video or motion. Avoid long synchronous work in event handlers. Update the visible state promptly; defer nonessential work and bound large lists where actual scale warrants it. Do not add virtualization to four records, and do not turn a long list into a keyboard or find-in-page regression merely to reduce nodes.

Transforms and opacity can avoid layout work but still use GPU memory and painting resources. Measure heavy effects on the intended device. Stop nonessential work when offscreen or hidden where practical, respect reduced motion and provide a still/ordinary DOM fallback. `content-visibility` may help long pages, but requires careful intrinsic-size estimates and checks for focus, search and anchor behavior; it is not a blanket rule for every section.

## Record evidence that can be reproduced

Record build, browser, viewport, hardware, network/throttling, cache state and actions with each lab observation. Compare the same task and environment before and after. For a layout shift, identify the shifting element and trigger; for a delayed action, identify the long task or rendering work. Preserve negative observations and unavailable measurements. Budget tests can catch a large asset regression but cannot predict all field experience.

For an offline demonstration, record asset bytes, no-network behavior, real interaction responsiveness and visual stability observations. Do not invent field measurements for fictional content or a local file. The result is a useful specimen and bounded evidence, not proof that generated apps will always be faster.

Sources checked 2026-10-22: [Web Vitals](https://web.dev/articles/vitals), [font best practices](https://web.dev/articles/font-best-practices), [optimize CLS](https://web.dev/articles/optimize-cls). Revisit guidance when browser targets or delivery conditions change.
