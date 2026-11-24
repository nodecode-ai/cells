# Diagnostic commands

Copy-paste blocks for the four things you almost always need on a Windows or
WSL report: which layer you are in, what processes exist and who their
parent is, what a command line actually reached, and whether a file carries a
BOM. All of these are read-only; the two destructive commands are marked and
never run without asking.

## Which layer

```sh
# WSL or native?
uname -a                                  # kernel: *-microsoft-standard-WSL2 => WSL
echo "$WSL_DISTRO_NAME"                   # non-empty only under WSL
command -v wslpath >/dev/null && echo wsl-present

# Python layer
python -c "import sys; print(sys.platform, sys.version)"
```

```powershell
# Windows side
$PSVersionTable.PSVersion
[System.Environment]::OSVersion.Version   # 10.0.2xxxx on Windows 11
```

Nodecode itself is one SBCL image: `sbcl.exe` where it runs natively on
Windows, a Linux `sbcl` under WSL. On Windows there is no background gateway,
so the `nodecode serve` terminal is the process everything else descends from.

## Orphan sweep

Every candidate is checked against the set of live PIDs, so a process whose
parent has exited shows up as `parentAlive=False`. Adjust `$fam` to the family
you are investigating.

```powershell
$all = Get-CimInstance Win32_Process | Select-Object ProcessId, ParentProcessId, Name, CommandLine
$ids = @{}; foreach ($p in $all) { $ids[[int]$p.ProcessId] = $true }
$fam = 'sbcl.exe','uv.exe','python.exe','pythonw.exe','node.exe'
$cand = $all | Where-Object { $fam -contains $_.Name }
"candidates: $($cand.Count)"
foreach ($p in $cand) {
  $parentAlive = $ids.ContainsKey([int]$p.ParentProcessId)
  $cmd = ($p.CommandLine -replace '\s+', ' ')
  if ($cmd.Length -gt 110) { $cmd = $cmd.Substring(0, 110) }
  "pid=$($p.ProcessId) ppid=$($p.ParentProcessId) parentAlive=$parentAlive $($p.Name) :: $cmd"
}
```

Narrow to one Nodecode home before concluding anything -- a machine running
other Python and Node tooling has a dozen legitimate `python.exe` processes:

```powershell
$home_ = "$env:USERPROFILE\.nodecode"
$all | Where-Object { $_.CommandLine -like "*$home_*" } |
  Select-Object ProcessId, ParentProcessId, Name | Format-Table -AutoSize
```

Reading the tree upward is often faster than reading it downward: take the PID
from the error, then walk `ParentProcessId` one step at a time.

```powershell
function Get-ProcessAncestry([int]$pid) {
  while ($pid -and $pid -ne 0) {
    $p = Get-CimInstance Win32_Process -Filter "ProcessId=$pid" -ErrorAction SilentlyContinue
    if (-not $p) { "  $pid (gone)"; break }
    "  $pid $($p.Name)"
    $pid = [int]$p.ParentProcessId
  }
}
Get-ProcessAncestry 12345
```

**Destructive, ask first.** Terminating a tree that includes a live gateway can
drop sessions:

```powershell
# Stop-Process -Id <pid> -Force          # one process
# taskkill /PID <pid> /T /F              # tree, follows live PPIDs only
```

`taskkill /T` only follows parents that are still alive. A reparented
grandchild needs to be matched by command line instead.

## Interop: what actually reached the shell

When a Windows-side action behaves like a shell line, capture the exact
argument vector rather than the rendered command:

```powershell
# Watch for a short window while reproducing the action
Get-CimInstance Win32_Process -Filter "Name='cmd.exe'" |
  Select-Object ProcessId, ParentProcessId, CommandLine | Format-List
```

The hazard is a value passed as an argv element and re-parsed by `cmd.exe`
(`&`, `|`, `<`, `>`, `^`, `(`, `)`, `,`, `;`, `=`). `%NAME%` expands before
caret escaping applies, so it cannot be defused on a command line. If the
reproduction needs a scratch URL, use one that carries a metacharacter and
observe whether a second process appears.

```sh
# From WSL: which Windows binary is on the path, and how does it see the path
command -v cmd.exe rundll32.exe explorer.exe
wslpath -w /home/me/file.txt      # -> \\wsl$\<distro>\home\me\file.txt
wslpath -u 'C:\Users\me\file.txt' # -> /mnt/c/Users/me/file.txt
```

## Config encoding

```powershell
# First bytes: EF BB BF is a UTF-8 BOM, FF FE is UTF-16
Format-Hex -Path "$env:USERPROFILE\.nodecode\config.jsonc" -Count 3
```

```sh
# Same check from WSL against any config the agent reads
head -c 3 ~/.nodecode/config.jsonc | od -An -tx1
```

A BOM or a UTF-16 file shows up as a reader that stops at its first byte --
`config.jsonc is not valid JSONC (byte N)`. Re-save as UTF-8 without BOM
instead of editing the model list.

## Console encoding

```powershell
chcp                       # active code page; 65001 is UTF-8, 936 is GBK
[Console]::OutputEncoding
```

```sh
locale                     # inside WSL
echo "$PYTHONUTF8$PYTHONIOENCODING"
```

Non-ASCII text that survives a file read but corrupts through a pipe is almost
always a code-page mismatch, not a Nodecode bug -- confirm by re-running the
same command with `PYTHONUTF8=1` and `chcp 65001`.

## Job object state

Read back what a process's job actually enforces. `IsProcessInJob(h, NULL, ...)`
is useless here (it returns true for nearly every process on Windows 8 and
later); pass the real job handle and query the limit flags.

```powershell
# Is this PID in any job that kills on close?
$p = Get-CimInstance Win32_Process -Filter "ProcessId=$pid"
$p | Select-Object ProcessId, ParentProcessId, Name, ExecutablePath
```

Deeper inspection needs the job handle, which only the owning process has --
capture it from inside that process (a small helper that calls
`QueryInformationJobObject`) rather than from a separate shell.
