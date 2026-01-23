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

## Source Code Organization

Follow these conventions for organizing code across multiple files:

### Main Entry Point
- `doing.el` — The main package file. Contains:
  - Package headers and metadata (version, dependencies, description)
  - `require` statements for all submodules
  - Top-level customization group (`defgroup doing`)
  - The final `(provide 'doing)` statement
  - Autoload cookies for user-facing commands

### Feature Files
Each major user-facing command or feature gets its own file:
- `doing-now.el` — Code for the `doing-now` command and related functionality
- `doing-later.el` — Code for the `doing-later` command
- `doing-done.el` — Code for the `doing-done` command
- Pattern: `doing-<feature>.el` for each distinct feature

Each feature file should:
- Begin with lexical-binding declaration and standard headers
- `(require 'doing-lib)` for shared utilities (if needed)
- Define all functions, variables, and faces related to that feature
- End with `(provide 'doing-<feature>)`

### Shared Code
- `doing-lib.el` — Internal utilities and shared helpers. Contains:
  - Common parsing, formatting, or data manipulation functions
  - Shared constants and internal variables
  - Functions used by multiple feature files
  - All symbols should use `doing--` (double hyphen) prefix

### When to Split Files
- **Do split** when a feature is logically independent and could be loaded separately
- **Do split** when a file exceeds ~500 lines and has clear subsections
- **Don't split** prematurely—start with one file and extract when patterns emerge
- **Don't split** tightly coupled code that always loads together

### File Dependencies
```
doing.el (main entry point)
├── doing-lib.el (shared utilities, loaded first)
├── doing-now.el (requires doing-lib)
├── doing-later.el (requires doing-lib)
└── doing-done.el (requires doing-lib)
```

### Autoloads
- Add `;;;###autoload` cookies only to user-facing commands
- Internal functions should NOT have autoload cookies
- The main `doing.el` handles requiring submodules when the package loads

## Testing
- Tests are written using **ERT**.
- Tests live in the `tests/` directory.
- Any behavior change **must** be accompanied by tests.
- Before submitting changes:
  - Run the full test suite.
  - Ensure all tests pass with no warnings or errors.
- If tests fail, fix them—do not disable or remove them without justification.

### Running Tests
```bash
# Run all tests (CI-friendly, no GUI required)
make test

# Run tests with verbose output
make test-verbose

# Run a single test by name
make test-one TEST=doing-test-function-exists

# Byte-compile source files (check for warnings)
make compile
```

Tests run in batch mode (`emacs --batch -Q`) which requires no display and ignores user configuration for reproducibility.

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
