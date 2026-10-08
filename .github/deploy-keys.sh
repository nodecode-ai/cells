#!/usr/bin/env bash
# deploy-keys.sh --- let the check clone the index's private repositories.
#
# A private entry's repository carries a read-only deploy key whose private
# half is this repository's secret DEPLOY_KEY_<NAME>: NAME is the
# repository's name upper-cased, - as _ (nodecode-cline is
# DEPLOY_KEY_NODECODE_CLINE). SECRETS is the workflow's secrets as JSON.
# Each key gets an ssh host alias, and a git rewrite sends the entry's https
# URL through it, so check.lisp clones a private entry exactly as it clones a
# public one. check.sh gives the trial a scratch HOME, so the rewrites live in
# a file GIT_CONFIG_GLOBAL names and ssh is handed its config and GitHub's
# published host key; both variables go to $GITHUB_ENV for the steps after
# this one, which never see SECRETS.
set -euo pipefail

dir="$RUNNER_TEMP/deploy-keys"
mkdir -p "$dir"
chmod 700 "$dir"

python3 - "$dir" <<'PY'
import json, os, pathlib, sys

dir = pathlib.Path(sys.argv[1])
ssh, git = [], []
for name, key in sorted(json.loads(os.environ["SECRETS"]).items()):
    if not name.startswith("DEPLOY_KEY_"):
        continue
    repo = name[len("DEPLOY_KEY_"):].lower().replace("_", "-")
    path = dir / repo
    path.write_text(key.strip() + "\n")
    path.chmod(0o600)
    ssh.append(f"Host github-{repo}\n  HostName github.com\n  User git\n"
               f"  IdentityFile {path}\n  IdentitiesOnly yes\n")
    git.append(f'[url "git@github-{repo}:nodecode-ai/{repo}"]\n'
               f"  insteadOf = https://github.com/nodecode-ai/{repo}\n")
    print(f"deploy key: nodecode-ai/{repo}")
(dir / "ssh_config").write_text("\n".join(ssh))
(dir / "gitconfig").write_text("\n".join(git))
PY

# GitHub's ed25519 host key, as https://api.github.com/meta publishes it.
echo "github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl" \
  > "$dir/known_hosts"

{
  echo "GIT_CONFIG_GLOBAL=$dir/gitconfig"
  echo "GIT_SSH_COMMAND=ssh -F $dir/ssh_config -o UserKnownHostsFile=$dir/known_hosts -o StrictHostKeyChecking=yes"
} >> "$GITHUB_ENV"
