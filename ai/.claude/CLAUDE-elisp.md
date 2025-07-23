# CLAUDE-elisp.md

This file provides guidance to you, Claude Code, when working with any Elisp
(Emacs Lisp) repositories owned by the user. This file will be imported by the
project-specific memory files.

## Elisp Guidelines

### Operation principles

- Make edits that take the edited file from one syntactically correct state to
  another. Avoid breaking paren balance.
- Use `.dir-locals.el` to set up `elisp-lint-indent-specs` for `elisp-autofmt`
  as needed.

### File-level guidelines

- Use the common Elisp package file structure template.
- Use lexical binding (`-*- lexical-binding: t -*-`).
- Put any `(require)` statements at the top of the file, at the start of the
  code section.

### Dependencies

- The use of `cl-lib` for modern Common Lisp constructs (`cl-defun`, `cl-letf`,
  etc.) is allowed as needed.

### Symbol-level guidelines

- Prefer named functions to lambdas.
- All symbols must be prefixed with the package name.
- All internal symbols must finish the package name prefix with a double hyphen.
- All symbols must have documentation strings.

### Testing

- Test file names must end with `-test.el`
- Tests must use ERT (Emacs Lisp Regression Testing) framework
