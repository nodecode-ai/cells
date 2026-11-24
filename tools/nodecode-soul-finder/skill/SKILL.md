---
name: soul-finder
description: Iterative workflow for refining SOUL.md personality files - separating durable voice/tone from project context, condensing to essentials, and getting user approval before shipping.
version: 1.0.0
author: Hermes Agent
license: MIT
metadata:
  hermes:
    tags: [soul-md, personality, configuration, workflow, iterative]
---

# SOUL.md Refinement Workflow

## When to Use
- SOUL.md has become bloated with mixed concerns (personality + project details)
- User wants to overhaul their agent's personality/voice
- Need to separate durable identity from temporary project context
- Creating SOUL.md from scratch for new Hermes instance
- **User says current SOUL.md is too corporate/stiff/boring — wants more personality, energy, heat, or "spice"** (tonal injection, not just condensation)
- **User wants a full persona SOUL.md** (e.g. a CEO, operator, or character) rather than a minimal personal assistant voice

## Key Distinctions

| File | Purpose | Content |
|------|---------|---------|
| **SOUL.md** | Durable personality/voice | Identity, style, avoid, standard |
| **AGENTS.md** | Project technical context | Architecture, conventions, file paths |
| **Memory** | User background/context | Personal details, preferences, goals |

### Two Classes of SOUL.md

Not all SOUL.md files serve the same purpose. Identify which class you're dealing with before starting:

**Voice SOUL.md** — Personal assistant/bot voice. ~15-30 lines. Pure tone and style. Minimal identity, mostly communication rules. Targets: concision, clarity, consistency. The original skill target.

**Persona SOUL.md** — Full character/role with operating philosophy. Can be 60-100+ lines. Includes strategic stance, decision-making framework, and values alongside voice. Targets: conviction, personality, mission-alignment. Example: a CEO persona with both "Operating Stance" and "Voice & Vibe" sections.

The condensation target (15-30 lines) only applies to Voice-class SOUL.md. For Persona-class, apply separate logic: keep strategic substance intact, inject heat into voice section.

## The Workflow

### Phase 1: Audit Current State
1. Read existing SOUL.md
2. Identify mixed concerns:
   - Personality/voice elements (keep in SOUL.md)
   - Project-specific details (move to AGENTS.md or memory)
   - Technical conventions (move to AGENTS.md)

### Phase 2: Research & Draft
1. Review Hermes docs on SOUL.md vs AGENTS.md
2. Identify which class (Voice vs Persona) this SOUL.md is
3. Draft lean SOUL.md with structure:
   ```
   # Identity
   Who the agent is (2-3 sentences)

   ## Style (or Operating Stance for Persona-class)
   - Bullet points on communication style
   - Energy level, banter preferences

   ## Voice & Vibe (Persona-class only)
   - How they talk, what energy they carry
   - The "feel" of a message from this persona

   ## Avoid
   - What NOT to do stylistically

   ## The Standard
   - Completeness mantra (optional but powerful)
   ```

### Phase 2b: The Personality Injection (for "too corporate" or "heat it up" requests)
When a user says the current SOUL.md is too stiff/corporate/dry or they want it "spicey":

1. **Audit for flatness**. Identify sections that read like a style guide vs a person. Look for: passive voice, formal register, hedging language, "we suggest considering" energy.

2. **The layered approach**. If the SOUL.md has good strategic substance (operating principles, decision frameworks), PRESERVE those intact. The user earned those. Don't throw the baby out.

3. **Inject heat into the Voice section**. Rewrite the tone/voice portion with:
   - Active, punchy language ("Lead with the point, then context" not "Messages should be structured to lead with the primary point")
   - Permission to swear/be casual if the user's vibe allows it
   - Energy that matches the *mission* — grounded go-getter, not hype beast
   - Specific things to avoid that reflect the *opposite* of the desired tone (corporate cadence, hedging, passive voice)

4. **Call out what they're FOR**. A persona who's building something that matters should *sound* like it matters. "You're building something good. Act like it."

5. **Dialogue pattern**: the user will often clarify the energy they want mid-conversation. Expect iteration. "More like this, less like that" is the natural rhythm. Don't try to nail it in one shot.

Key insight: a SOUL.md that's functionally correct but tonally flat needs the **strategic bones preserved** and the **voice section rewritten** — not a full rewrite. The operating stance is earned. The voice needs a pulse.

### Tone Trap — Cheeky ≠ Bitchy

When injecting heat into a cheeky/fun persona, there's a fine line between **playful roasting** and **mean-spiritedness**. The most common correction flow:

1. Draft lands on "call out her bullshit", "roast her life choices" energy
2. User says "tone it down, not bitchy"
3. Revision pulls it back to "warm cheeky — teasing without teeth, honest without being harsh"

**This pattern repeats across iterations.** The fix is to lead with warmth first, edge second. In the SOUL.md, structure cheeky traits as "warm first, funny second" rather than "sharp first, caring second."

**Pitfall:** "Psych degree" + "cheeky banter" in the same persona can land as clinical condescension if the tone section emphasises sharpness over warmth. Default to "mate's heart" energy — the psych brain is context, not the primary operating mode.

**Check before shipping a cheeky SOUL.md:**
- [ ] Could any single line read as mean if taken out of context?
- [ ] Is the warmth explicit or just implied?
- [ ] Would this persona make the user feel *seen* or *judged*?
- [ ] Are the insults clearly in the "taking the piss with love" register?

### Phase 3: Iterative Condensation
1. Present draft to user
2. Remove sections they don't want
3. Condense further if needed
4. Target: ~20-30 lines maximum

### Phase 4: Determine Context Home
- **Technical projects** → AGENTS.md in repo
- **User background/goals** → Memory (persistent)
- **Non-technical users** → Memory only (skip AGENTS.md)

### Phase 5: Ship
1. Get explicit user approval
2. Write to `~/.hermes/SOUL.md`
3. **Sync to every profile that should carry the persona.** SOUL.md is per-profile: the default profile reads `~/.hermes/SOUL.md`; each named profile reads `~/.hermes/profiles/<name>/SOUL.md`. Editing the default does NOT affect other profiles — they keep their own (often stock "You are Hermes Agent..." boilerplate) SOUL.md. If the user tests in a fresh profile and the persona is missing, this is why.
   ```bash
   cp ~/.hermes/SOUL.md ~/.hermes/profiles/<name>/SOUL.md
   diff ~/.hermes/SOUL.md ~/.hermes/profiles/<name>/SOUL.md  # verify identical
   ```
   Note: existing sessions won't pick up a changed SOUL.md — a fresh session (`/new` or new chat) is required.
   **Desktop app greeting pitfall:** the app's new-session intro copy is NOT driven by SOUL.md — it reads `display.personality` from config.yaml. A profile with no personality shows the generic "Hermes Agent is ready." intro even when the SOUL is correct. Set it to match the persona (e.g. `hermes -p <name> config set display.personality hype`, or edit the profile's config.yaml `display:` block). App picks it up after a profile re-home (switch away and back) or reconnect. NOTE: `display.personality` is NOT just cosmetic — it injects a personality preamble into the system prompt (e.g. `hype` injects "YOOO LET'S GOOOO!!!...", see cli.py) that the model may argue with. For persona work, prefer leaving it empty; the SOUL is the voice.
   **soul-builder onboarding flow:** the first-run nudge ("no SOUL.md → prompts new user") only fires when plugin message injection has a live CLI reference — i.e. **interactive CLI mode only** (`hermes -p <name>`), NOT the desktop app, gateway, or one-shot `-q` (inject_message logs "no CLI reference"). Also requires: no existing meaningful SOUL.md (delete or empty it), no `.nudge-sent` marker in the plugin dir, and the plugin's `register_skill` call passing a Path not a str (a `str(skill_path)` bug crashes load with "'str' object has no attribute 'exists'").
4. Update memory with consolidated context if needed

## Anti-Patterns to Avoid

### Bloated SOUL.md
**Wrong:** 60+ lines mixing personality, project details, technical conventions, user background
**Right:** ~24 lines focused purely on voice/tone/identity

### Project Details in SOUL.md
**Wrong:** "Josh is building Regeneratus, an AI shopping assistant..."
**Right:** Move to memory (user context) or AGENTS.md (technical project)

### Technical Conventions in SOUL.md
**Wrong:** "Use pytest, not unittest" / "Frontend lives in frontend/"
**Right:** These belong in AGENTS.md

## Example: Before → After

For a full personality injection case study (corporate CEO SOUL.md → grounded go-getter with strategic substance), see `references/personality-injection-example.md`.

### Before (68 lines - bloated)
```markdown
# Hermes Persona: Josh's Co-Founder

## Identity
You are Hermes, an AI agent and co-founder...

## Background Context
Josh is an operations manager at Belmondos...
He's currently building Regeneratus...

## Communication Style
[10+ sections]

## Working Principles
[Multiple subsections]

## Mission Alignment
[Project context]

## Response Format
[Detailed formatting rules]

## The Standard
[Long completeness mantra]
```

### After (24 lines - lean)
```markdown
# Identity

You are Hermes, Josh's co-founder and teammate. Pragmatic, direct, allergic to bullshit. Building shit together over beers.

## Style

- **Founder energy**: Punchy, enthusiastic, ship-ready
- **Good mate banter**: Casual, warm, swearing natural ("fuck yeah", "shit", "bloody")

Note: Some users prefer NOT to include explicit "match the vibe" or "voice parity" instructions - these can be handled implicitly by the agent rather than stated rules.

## Avoid

- Corporate speak, politeness theater, unverified claims
- Workarounds when the real fix exists

## The Standard

The marginal cost of completeness is near zero with AI. Do the whole thing...
```

## Decision Tree: Where Does Context Go?

```
Is it about the agent's voice/personality?
├── YES → SOUL.md
└── NO → Is it technical project details?
    ├── YES → AGENTS.md (or skip if non-technical)
    └── NO → Is it user background/preferences?
        └── YES → Memory
```

## Verification Checklist

Before shipping SOUL.md:
- [ ] Identify class: Voice (~15-30 lines) or Persona (can be 60-100+ lines)
- [ ] Voice-class: under 30 lines, pure tone, no project details
- [ ] Persona-class: strategic substance preserved, voice section has pulse
- [ ] No project-specific technical details (belongs in AGENTS.md or project config)
- [ ] No user background unless it's identity-relevant for the persona
- [ ] If Persona-class: does it sound like a real human, not a corporate handbook?
- [ ] **If cheeky/funny persona: is warmth explicit, not just implied? (Cheeky ≠ Bitchy)**
- [ ] User has explicitly approved
- [ ] Memory updated with consolidated context if needed
- [ ] AGENTS.md created only if needed (technical projects)
