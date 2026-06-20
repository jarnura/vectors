#!/usr/bin/env node
// validate-standards.mjs
//
// Coding-standards validator for the `vectors` repo.
//
// Enforces a hard per-file line-count cap (default 800) across tracked source
// files. It is intentionally dependency-free: it uses Node built-ins only
// (node:fs, node:path, node:child_process) so it can run in any CI/babysitter
// process without an install step.
//
// Output contract (stdout): EXACTLY one JSON object, e.g.
//   {"offenders":[{"file":"test/Main.purs","lines":2945}],"count":1}
// offenders are sorted by DESCENDING line count; paths are repo-relative with
// forward slashes. Exits 1 when count > 0, else 0.
//
// Config: env MAX_FILE_LINES overrides the cap (default 800).

import { readFileSync, existsSync, readdirSync, statSync } from "node:fs";
import { join, relative, sep, posix } from "node:path";
import { execFileSync } from "node:child_process";

// Repo root = parent of this script's directory (scripts/..).
const SCRIPT_DIR = new URL(".", import.meta.url).pathname;
const ROOT = join(SCRIPT_DIR, "..");

// The line cap. Default 800; overridable via env for ratcheting experiments.
const MAX_FILE_LINES = Number.parseInt(process.env.MAX_FILE_LINES ?? "800", 10);

// Directories that never count, even if somehow tracked.
const IGNORED_DIRS = new Set([
  "dist",
  "output",
  ".spago",
  "node_modules",
  ".a5c",
]);

// Which tracked files to scan. A file matches if it satisfies any predicate.
const MATCHERS = [
  (p) => p.startsWith("src/") && p.endsWith(".purs"),
  (p) => p.startsWith("test/") && p.endsWith(".purs"),
  (p) => p.startsWith("src/") && p.endsWith(".js"),
  (p) => p.startsWith("e2e/") && p.endsWith(".js"),
];

// Normalise an OS path to a repo-relative POSIX path (forward slashes).
function toRepoRelative(absPath) {
  return relative(ROOT, absPath).split(sep).join(posix.sep);
}

// True if the given repo-relative path lives under an ignored directory.
function isIgnored(relPath) {
  return relPath.split(posix.sep).some((seg) => IGNORED_DIRS.has(seg));
}

// Enumerate tracked files via `git ls-files`. Returns null if git is
// unavailable / this is not a git checkout, so the caller can fall back.
function listTrackedFiles() {
  try {
    const out = execFileSync("git", ["ls-files"], {
      cwd: ROOT,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
    });
    return out.split("\n").map((l) => l.trim()).filter(Boolean);
  } catch {
    return null;
  }
}

// Fallback: recursively walk the filesystem from ROOT, skipping ignored dirs.
function walkFiles(dir, acc) {
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    if (entry.isDirectory()) {
      if (IGNORED_DIRS.has(entry.name) || entry.name === ".git") continue;
      walkFiles(join(dir, entry.name), acc);
    } else if (entry.isFile()) {
      acc.push(toRepoRelative(join(dir, entry.name)));
    }
  }
  return acc;
}

// Count \n-delimited lines in a file. A single trailing newline terminates
// the last line rather than starting a new (empty) one, so it is not counted.
function countLines(absPath) {
  const text = readFileSync(absPath, "utf8");
  if (text.length === 0) return 0;
  const body = text.endsWith("\n") ? text.slice(0, -1) : text;
  return body.split("\n").length;
}

function main() {
  // Gather candidate repo-relative paths.
  let candidates = listTrackedFiles();
  if (candidates === null) {
    candidates = walkFiles(ROOT, []).filter((p) => !isIgnored(p));
  }

  // Filter to the source globs we care about, excluding ignored dirs.
  const targets = candidates.filter(
    (p) => !isIgnored(p) && MATCHERS.some((m) => m(p)),
  );

  const offenders = [];
  for (const relPath of targets) {
    const absPath = join(ROOT, relPath);
    if (!existsSync(absPath) || !statSync(absPath).isFile()) continue;
    const lines = countLines(absPath);
    if (lines > MAX_FILE_LINES) {
      offenders.push({ file: relPath, lines });
    }
  }

  // Sort by descending line count (stable tiebreak on path for determinism).
  offenders.sort((a, b) => b.lines - a.lines || a.file.localeCompare(b.file));

  process.stdout.write(
    JSON.stringify({ offenders, count: offenders.length }) + "\n",
  );
  process.exit(offenders.length > 0 ? 1 : 0);
}

main();
