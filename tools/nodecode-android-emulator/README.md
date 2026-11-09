# nodecode-android-emulator

Android Emulator: eighteen tools that drive a headless Android emulator from the chat — a screenshot, a tap, an app, a simulated GPS fix or battery, the logs. A Nodecode rewrite of [PIR8-Software/hermes-android-emulator](https://github.com/PIR8-Software/hermes-android-emulator) by [PIR8-Software](https://github.com/PIR8-Software).

The tools are the original plugin's, with the same names and arguments:

| Tool | What it does |
| --- | --- |
| `emu_status` | Whether the emulator is connected, and its version, model, screen, boot state and free space. |
| `emu_screenshot` | A PNG of the screen, saved on this machine, with its path. |
| `emu_tap`, `emu_swipe` | A tap at (x, y); a swipe from one point to another. |
| `emu_type`, `emu_key` | Text into the focused field; one key event (HOME, BACK, ENTER …). |
| `emu_launch`, `emu_deeplink` | An app, one activity, a URL, or a deep link such as `myapp://path`. |
| `emu_install`, `emu_uninstall`, `emu_packages` | An APK from this machine; an app by package name; what is installed. |
| `emu_push`, `emu_pull` | A file into the emulator; a file out of it. |
| `emu_logcat` | Recent logs, filtered by tag and priority (`*:E` for errors). |
| `emu_gps`, `emu_battery`, `emu_network` | A simulated location, battery level or network condition (offline, slow, fast). |
| `emu_shell` | Any device shell command; it runs on Android, never on this machine. |

Ask "what is on the emulator's screen?", "install this build and open it", "type this into the login field", "show me the crash logs" — the model picks the tool.

## Get it

Tell the agent "add the android-emulator cell", or run `nodecode add nc://android-emulator`.

## Set it up

The cell drives the emulator you already started, through the `adb` that comes with the Android SDK's platform-tools. Nothing else is needed.

```sh
# once, if you have no emulator yet: the SDK, one system image, one AVD
sdkmanager "platform-tools" "emulator" "platforms;android-34" "system-images;android-34;google_apis;x86_64"
avdmanager create avd -n pixel7pro -k "system-images;android-34;google_apis;x86_64" -d pixel_7_pro
emulator -avd pixel7pro -no-window &
```

Then, in `/settings` (or in the `android-emulator` section of `config.jsonc`):

```jsonc
"android-emulator": {
  "serial": "emulator-5554",
  "adb": ["adb"]
}
```

| Setting | What it does |
| --- | --- |
| `adb` | The argv prefix that runs adb: `["adb"]` for platform-tools on PATH, or `["~/Android/Sdk/platform-tools/adb"]`. |
| `serial` | The one emulator every call is pinned to (`emulator-5554` by default); `adb devices` lists it. |
| `allow_physical` | Act on a serial that is not an emulator's — a real phone — on purpose. |
| `output_root` | One more directory screenshots and pulls may be written under. |
| `timeout_seconds` | One adb run's ceiling (60 by default); its process tree is killed past it. |
| `max_output_chars` | How much of what adb wrote one tool result carries. |

Nothing is required beyond `adb` on PATH and an emulator that is already running: this cell does not start, stop or create emulators.

## Use it

You do not call the tools; the model does, when a task is about the emulator. A person can call the same Lisp functions from eval, in the `android-emulator` package:

```lisp
(android-emulator:handle-tap
  (android-emulator:decode-tap-at (nlk:json-object "x" 540 "y" 1200)))
```

`(help :android-emulator)` is the model's manual for the eighteen verbs, kept while the cell runs.

### What the guards are

The original plugin's hardening pass is kept, because it is what keeps a phone safe from a plugin that says "emulator":

- **Every device call is pinned** with `adb -s SERIAL`. A `serial` that is not an emulator's refuses at start unless `allow_physical` says otherwise, so a phone plugged in beside the emulator is never touched.
- **Names are checked before anything runs**: a package name is letters, digits, dot and underscore; an activity adds `$`; a URL needs a scheme and no shell metacharacters; a remote path starts at `/`; a logcat filter is `tag:priority` with a priority of `V D I W E F S`.
- **Text for the device is escaped**: spaces become `%s` and the whole is one quoted device-shell word, so `; rm -rf /sdcard` is typed, not run.
- **A write lands under the home directory, the cwd, the temp dir or `output_root`**, never replaces an existing file without `overwrite`, and needs an existing parent directory.

Only one verb is deliberately raw: `emu_shell` runs the command through the device's own `sh -c`, on Android.

### What the rewrite leaves out

The original plugin is also a Hermes surface: a dashboard API that starts and stops the emulator, creates, lists, wipes and deletes AVDs and installs system images; a desktop sidebar with a live screen and keyboard shortcuts; `emu start`/`stop`/`status` on the command line; screen recording, touch record-and-replay, an instrumented-test runner and push notifications. Those are the Hermes gateway's and desktop's surfaces; this cell is the eighteen tools. One smaller difference: the original resolved symlinks to confine a write, and this one refuses a `..` segment instead — the same confinement, said differently.

## License

MIT. The tools, their arguments and their guards are PIR8-Software's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
