# nodecode-gameforge

GameForge: sound effects, music, pixel sprites, tilesets and engine scaffolds a game project can use. A Nodecode rewrite of [DECRUX9812/hermes-plugin-gameforge](https://github.com/DECRUX9812/hermes-plugin-gameforge) by [DECRUX9812](https://github.com/DECRUX9812).

Six tools write real files into a game project folder, with no key, no network and nothing installed: a WAV sound effect from the sfxr synthesizer, a WAV music loop, a two-frame pixel-art PNG sheet, a sixteen-cell autotiling tileset, and a playable web page or Godot project to put them in. Every asset is recorded in the project's `gameforge.json` ledger with the seed and the recipe that made it, so the same arguments rebuild the same bytes.

## Get it

Tell the agent "add the gameforge cell", or run:

```sh
nodecode add nc://gameforge
```

## Set it up

Nothing is required. The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `directory` | `gameforge/` | where game projects are kept, under the home |
| `default_project` | `game` | the project a call that names none writes to |
| `sprite_scale` | `4` | screen pixels a drawn pixel becomes in a saved sprite |

A call may pass `project` as an absolute path to write anywhere; a relative one goes under `directory`, and `..` is refused.

## Use it

Ask for what the game needs: "a coin and a jump sound for my platformer", "an eight-bar tense loop in D", "a pixel-art hero sprite", "a grass-and-water tileset", "a playable page I can open". The model calls:

| Tool | Arguments | Writes |
|---|---|---|
| `forge_sfx` | `preset` (coin, pickup, laser, shoot, explosion, powerup, hit, hurt, jump, blip, select), `name`, `seed`, `variations` 1-8, `style` (retro, modern, crunchy, distant), `project` | `assets/sfx/*.wav` |
| `forge_music` | `mood` (heroic, happy, adventure, sad, mysterious, tense, calm), `bars` 4-64 (a multiple of 4), `bpm`, `key`, `mode`, `style` (chiptune, synthwave, ambient, orchestral), `name`, `seed`, `project` | `assets/music/*.wav` |
| `forge_sprite` | `kind` (character, monster, item), `name`, `size` 8-64, `scale`, `hue`, `palette` (pico8, sweetie16, endesga32, gameboy, nes), `seed`, `project` | `assets/sprites/*.png` and a frames `.json` |
| `forge_tileset` | `lower`, `upper` (grass, dirt, sand, water, deep_water, stone, snow, lava), `scale`, `seed`, `project` | `assets/tiles/*.png` |
| `forge_engine` | `engine` (web, godot), `action` (scaffold, import, run, status), `title`, `seed`, `project` | `web/index.html` or `godot/project.godot` with a scene and a script; `import` copies the project's assets in beside a manifest and `run` opens the engine; `status` reports the engines and programs installed |
| `forge_export` | `target` (web, single, capacitor), `title`, `source`, `app_id`, `project` | `dist/web` and `dist/<title>-web.zip` (a PWA bundle: manifest, two icons, a service worker); `dist/<title>.html` (one file, every asset inlined); `dist/mobile` (a Capacitor project) |

`/gameforge [project]` prints the folder, the default project, what this machine has installed and, when the project has one, its ledger.

A sprite is a sheet: frame 1 beside frame 2, each `size * scale` wide, so an engine reads the two frames by slicing it. A tileset is a Wang corner set: sixteen cells in a four-by-four image, bits 0-3 of the cell index the corners (top-left, top-right, bottom-left, bottom-right) that are the upper terrain.

## What the rewrite leaves out

AI text-to-3D (fal, Tripo, Meshy), ElevenLabs voices and prompt-sound, the headless-Chrome playtester, and the Unity, Blender and Unreal builders. They need a key, a heavyweight toolchain, or a browser the cell cannot assume, so they stay in the original; `forge_engine`'s `status` still reports which of those programs this machine has. The exporter ships the web build (bundle, zip, single file, a Capacitor project); the butler push to itch.io and the Godot and Unity platform builds still need the original. See NOTICE for the long version.
