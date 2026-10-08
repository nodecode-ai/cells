// main.swift -- the nodecode-apple helper: omp's Foundation Models bridge as a process.
//
// SPDX-License-Identifier: MIT
//
// bridge.swift (omp's, verbatim) exposes a C ABI that omp's Rust addon
// dlopens in-process. A Lisp image cannot take the bridge's events the same
// way -- they arrive on Swift's own threads -- so this file wraps the very
// same ABI in an executable the cell runs as a child, one request per process:
//
//   nodecode-apple-bridge availability   prints the availability event, one line
//   nodecode-apple-bridge generate       reads one request (JSON) from stdin to EOF,
//                                        prints every event as one JSON line, and
//                                        exits after the terminal done or error
//
// The request and the events are bridge.swift's own schema. Cancelling a
// generation is ending the process.

import Foundation

func writeLine(_ text: String) {
	FileHandle.standardOutput.write(Data((text + "\n").utf8))
}

func run() -> Never {
	let verb = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : ""
	if verb == "availability" {
		if let pointer = ompAppleFmAvailability() {
			writeLine(String(cString: pointer))
			ompAppleFmFree(pointer)
		}
		exit(0)
	}
	if verb == "generate" {
		let input = FileHandle.standardInput.readDataToEndOfFile()
		// The bridge decodes the request before this call returns, so the
		// C string need not outlive it.
		String(decoding: input, as: UTF8.self).withCString { request in
			ompAppleFmGenerate(1, request, nil) { _, event, final in
				writeLine(String(cString: event))
				if final { exit(0) }
			}
		}
		dispatchMain()
	}
	FileHandle.standardError.write(Data("usage: nodecode-apple-bridge availability | generate\n".utf8))
	exit(2)
}

run()
