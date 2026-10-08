#!/bin/sh
# Builds the nodecode-apple bridge helper: omp's bridge.swift and this cell's
# main.swift, compiled into one executable. Derived from omp's
# crates/pi-natives/src/applefm/build-bridge.sh (MIT, see NOTICE): the same
# toolchain detection, an executable instead of a dylib.
#
#   build.sh <out>
#       Finds a toolchain able to build bridge.swift (Swift 6.4+ with the
#       macOS 27+ SDK): $OMP_APPLEFM_SWIFTC and the xcode-select'ed toolchain
#       with the selected SDK, then the Command Line Tools with their own SDK
#       (a set $SDKROOT is the SDK for every candidate), and compiles
#       bridge.swift and main.swift into <out> for arm64-apple-macos27.0.
#       Exits 3, saying why, when there is no such toolchain.
#
# Swift module caches live in $OMP_APPLEFM_MODULE_CACHE (default: under
# $TMPDIR) so repeated builds skip re-importing the SDK.
set -eu

here=$(cd "$(dirname "$0")" && pwd)

swift_version() {
	"$1" -version 2>/dev/null | sed -n 's/.*Swift version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1 \2/p' | head -n 1
}

sdk_major() {
	/usr/bin/plutil -extract Version raw "$1/SDKSettings.plist" 2>/dev/null | cut -d. -f1
}

usable() {
	[ -n "$1" ] && [ -x "$1" ] && [ -n "$2" ] && [ -d "$2/System/Library/Frameworks/FoundationModels.framework" ] || return 1
	set -- "$1" "$2" $(swift_version "$1")
	[ $# -eq 4 ] || return 1
	[ "$3" -gt 6 ] || { [ "$3" -eq 6 ] && [ "$4" -ge 4 ]; } || return 1
	major=$(sdk_major "$2")
	[ -n "$major" ] && [ "$major" -ge 27 ]
}

detect() {
	selected_sdk=${SDKROOT:-$(/usr/bin/xcrun --sdk macosx --show-sdk-path 2>/dev/null || true)}
	clt=/Library/Developer/CommandLineTools
	for candidate in \
		"${OMP_APPLEFM_SWIFTC:-}|$selected_sdk" \
		"$(/usr/bin/xcrun --find swiftc 2>/dev/null || true)|$selected_sdk" \
		"$clt/usr/bin/swiftc|${SDKROOT:-$clt/SDKs/MacOSX.sdk}"; do
		swiftc=${candidate%%|*}
		sdk=${candidate#*|}
		if usable "$swiftc" "$sdk"; then
			sdk=$(cd "$sdk" && pwd -P)
			printf '%s\t%s\n' "$swiftc" "$sdk"
			return 0
		fi
	done
}

out=${1:?usage: build.sh <out>}
found=$(detect || true)
if [ -z "$found" ]; then
	echo "no Swift 6.4+ toolchain with the macOS 27 SDK (Xcode 27, or its Command Line Tools)" >&2
	exit 3
fi
swiftc=$(printf '%s' "$found" | cut -f1)
sdk=$(printf '%s' "$found" | cut -f2)
cache=${OMP_APPLEFM_MODULE_CACHE:-${TMPDIR:-/tmp}/omp-applefm-module-cache}
mkdir -p "$(dirname "$out")"
"$swiftc" -sdk "$sdk" -target arm64-apple-macos27.0 -swift-version 6 -O \
	-module-name NodecodeAppleBridge -module-cache-path "$cache" \
	"$here/bridge.swift" "$here/main.swift" -o "$out.tmp.$$"
mv -f "$out.tmp.$$" "$out"
