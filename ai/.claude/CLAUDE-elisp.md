# CLAUDE-elisp.md

This file provides guidance to you, Claude Code, when working with any Elisp
(Emacs Lisp) source code owned by the user. This file will be imported by the
global user memory file.

## Elisp Guidelines

### Operation principles

- Use `.dir-locals.el` to set up `elisp-lint-indent-specs` for `elisp-autofmt`
  as needed.

### File-level guidelines

- Use the common Elisp package file structure template.
- Use lexical binding (`-*- lexical-binding: t -*-`).
- Put any `(require)` statements at the top of the file, at the start of the
  code section.
- All declarations must be above their first uses.

### Dependencies

- The use of `cl-lib` for modern Common Lisp constructs (`cl-defun`, `cl-letf`,
  etc.) is allowed as needed.

### Symbol-level guidelines

- Prefer named functions to lambdas.
- All symbols must be prefixed with the package name.
- All internal symbols must finish the package name prefix with a double hyphen.
- All symbols must have documentation strings.

### Testing

- Test filenames must end with `-test.el`
- Tests must use ERT (Emacs Lisp Regression Testing) framework
