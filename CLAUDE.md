# AGENT.md

This repository contains an Emacs Lisp package.
These instructions apply to all coding agents contributing to this project.

## General Principles
- Prefer **clarity over cleverness**.
- Follow **idiomatic, modern Emacs Lisp** practices.
- Keep changes **minimal and focused**.
- Do not introduce new dependencies unless clearly justified.

## Coding Conventions
- Use **lexical binding** (`-*- lexical-binding: t -*-`) in all Elisp files.
- Follow standard Emacs Lisp naming conventions:
  - Package prefix for all public symbols (functions, variables, faces).
  - Internal helpers should use `--` in their names.
- Use `defcustom` (with appropriate `:type` and `:group`) for user-facing configuration.
- Avoid global state unless necessary; prefer buffer-local or explicit state passing.
- Document public functions and variables with proper docstrings.
- Prefer built-in libraries over third-party ones when possible.

## Project Structure (De Facto)
- Source files live at the project root directory.
- Tests live in a `tests/` directory.
- Do not assume any other fixed structure.

## Testing
- Tests are written using **ERT**.
- Any behavior change **must** be accompanied by tests.
- Before submitting changes:
  - Run the full test suite.
  - Ensure all tests pass with no warnings or errors.
- If tests fail, fix them—do not disable or remove them without justification.

## Compatibility
- Maintain compatibility with supported Emacs versions as documented by the project.
- Avoid relying on bleeding-edge APIs unless explicitly allowed.
- Guard version-specific code when necessary.

## Style & Tooling
- Format code according to standard Emacs Lisp style (as in Emacs core).
- Avoid excessive macros or meta-programming unless they clearly improve maintainability.
- Byte-compile cleanly (no warnings, unless unavoidable and documented).

## Commit Expectations
- Make commits logically atomic.
- Do not mix refactors, formatting, and behavior changes in a single commit unless trivial.
- Update documentation and comments when behavior changes.

When in doubt, prefer the simplest, most boring solution that works.
