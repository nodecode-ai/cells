# nodecode-hotwire

Analog circuit bench: proves a SPICE netlist against measurable specs with its own simulator. A Nodecode rewrite of [RohanGlitched/hotwire](https://github.com/RohanGlitched/hotwire) by [RohanGlitched](https://github.com/RohanGlitched).

## Get it

Tell the agent "add the hotwire cell", or run `nodecode add nc://hotwire`.

## Set it up

Nothing is required: the cell simulates from the first message and needs no key - no host is dialled, no file is written, and a netlist is never a file read. The `hotwire` section of the config refines two bounds: `ac_points` (60; samples per decade of the frequency sweep, from 1 Hz to 200 kHz) and `max_elements` (400; the most parts one netlist may carry). `config.example.jsonc` is the section written out.

## Use it

The loop is the original's: a circuit is written as SPICE text, a spec says what it must do, and a tool result says whether it does.

- `hotwire_check` takes a netlist (or a `library` id, or a `path` to a `.cir` file), runs the electrical rule check, solves the DC operating point, measures every `*@ spec` line plus any `specs` passed with the call, and answers a verdict: `PASS`, `PASS_WITH_WARNINGS` or `FAIL`, one line per spec with what was measured and the margin, the idle node voltages, and each transistor's bias region.
- `hotwire_simulate` runs one raw analysis: `op` for the node voltages, element currents and supply draw, or `ac` for the gain and phase from the input source to the output node and the -3 dB corner.
- `hotwire_library` lists three reference circuits taken from the original's library - `rc_lowpass`, `sallen_key_lowpass`, `passive_tone_stack` - and returns one netlist whole.
- `hotwire_explain` nudges every resistor, capacitor, inductor and pot (`step`, 10% by default) and reports which spec each one moves and by how much, so a question about a part is answered with numbers.
- `hotwire_bom` is the parts list: equal parts grouped, resistors snapped to `series` (E24 unless asked) and capacitors to E6/E12, a package named for every semiconductor, and the jacks, footswitch, LED and enclosure a pedal also needs.
- `hotwire_tune` picks standard values for the parts a call names (`vary`): a coordinate search over the neighbouring E-series values, scored by total violation and then by the smallest margin, answering the measurements before and after the search.
- `hotwire_yield` builds the circuit `builds` times (60 by default) with every resistor, capacitor and inductor drawn uniformly inside `tolerance` (5% by default) and counts the builds that still meet every spec, naming the spec that breaks first.
- `hotwire_export` writes the circuit out: `format` `spice` is a self-contained ngspice/LTspice deck, its models included and its op-amps an ideal 100k-gain source; `json` is the netlist and its verdict as one object for another program.
- `/hotwire` is the operator's door to the same: bare for the help and the library, `/hotwire library` for the list, `/hotwire check <id|file.cir>` for one circuit checked, `/hotwire explain <id>` and `/hotwire bom <id>` for the two an operator asks in chat. `(help :hotwire)` is the manual the model reads.

A netlist is ordinary SPICE with comment annotations that other simulators ignore:

```
*@ title RC low-pass filter (1 kHz)
*@ in VIN
*@ out out
*@ spec fc in 960..1160 Hz
*@ spec gain_db(10000) in -21..-18.5 dB
VIN in 0 DC 0 AC 1
R1 in out 10k
C1 out 0 15n
.end
```

Elements: `R C L V I D Q`, an ideal op-amp `X1 in+ in- vcc vee out TL072`, a voltage-controlled source `E1 a b c d 10`. Built-in parts need no `.model` line: `1N4148 1N914 1N34A 1N5817 1N4001 LED_RED LED_GREEN 2N3904 2N2222 BC549C 2N5088 MPSA18 2N3906 BC559C TL071 TL072 NE5532 LM358 RC4558 OPAMP_IDEAL`. Values may be written `4.7k`, `100n`, `2meg`, `1e-9`, `4k7`; `M` is milli. Ground is node 0 (or gnd).

Specs are the original's grammar: `metric(args) <op> value` with `< <= > >= ==` or `in lo..hi`. The metrics this cell measures: `gain_db(f)`, `phase(f)`, `fc`, `f3db_low`, `f3db_high`, `peak_db`, `peak_f`, `v(node)`, `i(element)`, `p(element)`, `supply_current`, `zin(f)`, `region(Q1)`.

What comes back:

```
RC low-pass filter (1 kHz): PASS
3/3 specs pass.
Nodes: in 0 V, out 0 V
PASS  fc in 960..1160 Hz  ->  fc wants in 960 Hz..1.16k Hz; measured 1.058k Hz (margin 98.43 Hz)
PASS  gain_db(10000) in -21..-18.5 dB  ->  gain_db wants in -21 dB..-18.5 dB; measured -19.53 dB (margin 1.034 dB)
```

What it does not do: there is no transient run, so `thd`, `headroom`, `thd_gv`, `cleanup_vol`, `gain_tran` and `vout_peak` - the audio and distortion half of the original - are not measured, and `hotwire_audition` (play the circuit, write loudness-matched WAV files), `hotwire_match` (find the knob settings that reproduce a recorded take) and `hotwire_build` (stripboard placement, track cuts, build order) have no counterpart here; `hotwire_export` writes a SPICE deck or JSON, not a KiCad netlist or a schematic SVG. Engineering stays inside the DC and small-signal half the original's filters, dividers, bias networks and gain stages live in. The op-amp is ideal and linear: no saturation, no slew, no supply current. Yield spreads resistors, capacitors and inductors, not transistor parameters, and it is a fixed-seed draw rather than the original's clipped normal. A netlist's `.subckt` blocks and `.param` expressions are not read, and the desktop pane and the pre_llm_call hint hook are surfaces of the original harness with no counterpart in a cell.

Security: no network, no subprocess, no temporary file, no credential. The only file this cell ever reads is the `.cir` path a call names, and only its bytes.

## What is in it

| File | What it is |
| --- | --- |
| `netlist.lisp` | The SPICE subset: values, parts, `*@` annotations, the parser, the electrical rule check. |
| `sim.lisp` | Modified nodal analysis: the DC operating point by Newton's method, the small-signal AC sweep. |
| `analysis.lisp` | The spec grammar, every metric, the verdict and the report. |
| `parts.lisp` | Preferred-value tables, snapping, and the parts list. |
| `tooling.lisp` | Explain, tune, yield, and the SPICE/JSON export, all on the same solver. |
| `library.lisp` | The three reference circuits, as text. |
| `cell.lisp` | The declaration: the `hotwire` section, the eight tools, `/hotwire`, `(help :hotwire)`. |
| `test/` | The offline tests: the lifecycle, a divider, a biased transistor stage, a reference filter, wiring errors, the bounds, and the five new tools. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 Rohan Borade: see `LICENSE` and `NOTICE`.
