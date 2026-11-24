# Motion: give change a reason

## Name the job of each moving element

Classify motion as **feedback** (an action registered), **continuity** (where a panel or object went), **explanation** (how a real mechanism works), or **expression** (an authored identity moment). Remove motion with no clear job. Choose one governing behavior from the subject when expression is appropriate: a type specimen changes width, a diagram assembles, a material folds. A catalog of fashionable effects is not a motion identity.

Give each transition a trigger, affected elements, initial/final state, interruption policy and reduced-motion equivalent. Set an appropriate budget for the actual task and device. As starting hypotheses, a routine control response might occupy roughly 100–200ms, a panel transition 150–300ms, and a deliberate explanatory sequence longer under user control. These are craft choices to test, not accessibility standards. Repeated daily operations often need less visible travel than a one-time campaign entrance.

## Preserve control and information

- Render essential content in the initial document; do not leave it opacity-zero while waiting for an observer or script.
- Do not gate navigation, prices, dates, errors or the primary action behind an intro or loading animation. A ready interface can coexist with optional effects loading later.
- Use transforms/opacity where appropriate, but still profile actual layout, paint and memory. A compositor-friendly effect can be emotionally exhausting or GPU-expensive.
- Keep focus and pointer targets stable. Do not animate a focused button away, move a submit target under the pointer or interpolate a chart so that a value seems more certain than it is.
- Let the user interrupt a long transition; define the final state correctly under repeated activation, resize, route change and data refresh.
- Avoid continuous autoplay behind work. Provide the necessary pause/stop/hide behavior for moving content and do not create hazardous flashing. Sound starts with consent.

## Reduced motion is another complete composition

Read `prefers-reduced-motion` before starting an effect and respond to preference changes when the implementation supports them. Replace camera travel, parallax, elastic movement and large morphs with a strong still or direct state change. Keep useful feedback through labels, fill, border or an instantaneous expanded state. Do not erase a state transition's information or blanket-disable application logic by setting every animation duration to near-zero.

A three-dimensional explanation needs named static views or a two-dimensional equivalent. A kinetic title needs an intentionally designed still. A video needs a useful poster and user controls. Reduced motion must remain visually authored rather than an empty hero left where the animation used to be.

## Scroll and spatial effects

Normal document scrolling should remain normal unless a clearly bounded, accessible interaction genuinely requires another model. Never require a custom cursor, hover, precise drag or momentum scrolling to reach content. A horizontal gallery needs visible previous/next and index routes; a draggable model needs named part/view controls. Sticky sections must not trap reading, hide focus or create long blank runs on small screens.

## Verification

For a view change, the View Transition API can add continuity while the ordinary DOM update remains the baseline. Detect the API and check reduced motion at activation time. Keep focus, history and selection in the real update; a skipped animation is not a failed action. Do not tie persistence or visibility to `transition.ready`. The platform reference includes a minimal guarded pattern. Same-document support does not establish cross-document or newer transition-subfeature support.

Intrinsic-size interpolation and scroll-driven motion have differing support and behavior constraints. Use the feature ledger for a precise detection/fallback decision, then profile and exercise the chosen sequence. A browser recognizing a property does not establish that motion is comfortable, interruptible or useful. [MDN view transitions](https://developer.mozilla.org/en-US/docs/Web/API/Document/startViewTransition), checked 2026-10-22.

Record an ordinary initial state and a reduced-motion initial state at the same viewport. Exercise the triggering interaction in both. Test repeated clicks, interruption, long content, focus movement, slow loading and a narrow device profile. Inspect a recording or real sequence for rhythm; a still screenshot cannot prove timing, lack of flashing or interruptibility. Check console failures and frame behavior with browser performance tools when heavy effects are involved.

**Handoff:** a short motion ledger naming purpose, trigger, approximate duration, fallback and observed proof. If motion cannot be exercised, say it remains unverified rather than calling a static final frame an animation review.
