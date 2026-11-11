# nodecode-bubblewrap

Bubblewrap: run a command in its own bwrap sandbox — read-only host, secrets hidden, limits. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

Linux only. Needs [bubblewrap](https://github.com/containers/bubblewrap) 0.9 or later and `prlimit` from util-linux (installed on every mainstream distribution), and unprivileged user namespaces — or a setuid `bwrap`.

## Get it

Tell the agent "add the bubblewrap cell", or run `nodecode add nc://bubblewrap`.

## Set it up

Install the sandbox, then tell the cell where a command may write:

```sh
sudo apt install bubblewrap        # Debian, Ubuntu
sudo dnf install bubblewrap        # Fedora
sudo pacman -S bubblewrap          # Arch
```

Set `root` to the one directory a command may write — a project or a scratch directory, never your home directory — in `/settings` or with `(config-set '("bubblewrap" "root") $HOME/work/scratch)`. `/bubblewrap` then reports the box: whether `bwrap` and `prlimit` were found, whether the user-namespace probe passes, the profile in force, what a command may write and what is hidden from it.

If the probe fails, unprivileged user namespaces are restricted for your user; your distribution's notes on bubblewrap say how to allow them (on recent Ubuntu it is the AppArmor `kernel.apparmor_restrict_unprivileged_userns` setting). A missing `bwrap` still answers every call, with the package named, and one standing notice says so; commands are never run outside a box by this cell.

## Use it

Two tools, and the model calls them the way it calls any other:

- `sandbox` — one shell command, in its own box: `{"command": "make test"}`, optionally a `profile`, a `cwd` inside the root, and a `timeout_seconds`. It answers the command's output and its exit status.
- `sandbox_status` — the report above, in words.

`/bubblewrap` in a shell shows the report, and `/bubblewrap uname -a` runs one command inside the box. In eval the same run is `(bubblewrap:sandbox "make test")`, or `(bubblewrap:status)` for the report.

What a command sees:

- **The host root, read-only**, at the host's own paths, and `root` writable (only readable under `restricted`).
- **The home directory's dot entries hidden** unless they are on the allowlist (the shell startup files, `.gitconfig`, `.cache`, `.cargo`, `.rustup`, `.nvm`, `.pyenv`, and the common children of `.config`); `home_allow` adds more and `hide` hides more. Credential stores (`.ssh`, `.aws`, `.gnupg`, `.kube`, `.docker`, `.netrc`, `.config/gh`, `.config/gcloud`, the browser profiles, the keyrings) are hidden whatever the allowlist says. An allowed entry is read-only, so a command cannot edit `~/.gitconfig` or `~/.bashrc`.
- **A fresh `/dev`, `/proc` and `/tmp` per command**, with `/run/user` emptied, so ssh-agent, gpg-agent, keyring and D-Bus sockets are unreachable and the docker socket is gone. `SSH_AUTH_SOCK`, `GPG_AGENT_INFO` and `DBUS_SESSION_BUS_ADDRESS` are removed from the environment.
- **Limits**: `memory_mb` of address space and `cpu_seconds` of CPU (in front of bwrap) and `max_procs` processes (inside it). `0` disables one. They are deliberately tight: compilers and package managers want `memory_mb` 2048 and `cpu_seconds` 120 or more.
- **`--die-with-parent` and `--new-session`**: the box dies with Nodecode and cannot inject input into the controlling terminal.

Profiles:

| Profile | Working directory | Network | Use it for |
|---------|-------------------|---------|------------|
| `restricted` | read-only | none (loopback only) | inspection and read-only analysis |
| `workspace` | writable | none (loopback only) | builds and edits that must not reach the network |
| `network` | writable | host network | everything else (the default) |

`binds` and `bind_rw` mount more host paths at their own paths, read-only and read-write.

## What it does not isolate

- The rest of the host filesystem is readable: keep secrets in the hidden paths.
- No seccomp filter and no cgroup limits: this is a filesystem and process boundary, and the limits are per-process rlimits.
- Network is all or nothing under `network`: no egress filtering.
- Each command's box ends with it, so a background process does not survive.
- Only the tools and the verb listed above are sandboxed. A `(sh "...")` in eval runs on the host as it always did.

## Config

```jsonc
"bubblewrap": { "root": "~/scratch", "profile": "workspace", "memory_mb": 2048, "cpu_seconds": 60 }
```

Every member is in `config.example.jsonc`. `root` is required and refused when it is not an absolute path or does not exist; a member of the wrong shape refuses at start and the folder's line says `REFUSED` until it is fixed.

## Credits

Rewritten from NousResearch's `bubblewrap` plugin, MIT, at commit `e7a218f`. See `NOTICE` for what was carried over and what was left out, and `LICENSE` for the original licence.
