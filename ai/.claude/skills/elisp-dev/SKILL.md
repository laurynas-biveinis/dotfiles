---
description: >-
  Emacs Lisp (elisp) development guidelines. Apply whenever writing, editing,
  reviewing, or discussing Emacs Lisp / elisp code or .el files.
user-invocable: false
---

# Elisp Guidelines

## Tooling

Use the `elisp-dev` MCP server when working with elisp.

## Supported versions and compatibility

The oldest supported Emacs version is 27.1. The code must be compatible with
this and all the newer versions.

## Operation principles

- Use `.dir-locals.el` to set up `elisp-lint-indent-specs` for `elisp-autofmt`
  as needed.

## File-level guidelines

- Use the common Elisp package file structure template.
- Use lexical binding (`-*- lexical-binding: t -*-`).
- Put any `(require)` statements at the top of the file, at the start of the
  code section.
- All declarations must be above their first uses.

## Dependencies

- The use of `cl-lib` for modern Common Lisp constructs (`cl-defun`, `cl-letf`,
  etc.) is allowed as needed.

## Symbol-level guidelines

- Prefer named functions to lambdas always except when lambda must capture
  variables from the surrounding lexical scope.
- All symbols must be prefixed with the package name.
- All internal symbols must finish the package name prefix with a double hyphen.
- All symbols must have documentation strings.

## Function-level guidelines

- Never use `let*` for one binding, always use plain `let` then. Pay attention
  when you remove bindings to leave only one untouched.
- Always use combined binding and conditional forms (`if-let*`, `when-let*`,
  `and-let*`) to replace `let` for one binding with a conditional (e.g. `if`)
  on the same variable immediately inside it. For the combined bindings, the
  previous rule of star form not used for a single binding does not apply.
  Note: `while-let` is only available from Emacs 29.1 onwards and should not
  be used when supporting Emacs 27.2.

  ```elisp
  ;; Instead of:
  (let ((value (get-value)))
    (if value
        (process value)))

  ;; Use:
  (if-let* ((value (get-value)))
    (process value))
  ```

## Literals

- Prefer `'(...)` to `(list ...)` for literal lists; use `(list ...)` only when
  elements must be evaluated, or when the resulting list will be destructively
  modified (`setcar`/`setcdr`/`nconc`/`sort`) — quoted literals are
  shared constants and must not be mutated. The same applies to `[...]` vector
  literals.

## Testing

- Test filenames must end with `-test.el`
- Tests must use ERT (Emacs Lisp Regression Testing) framework
