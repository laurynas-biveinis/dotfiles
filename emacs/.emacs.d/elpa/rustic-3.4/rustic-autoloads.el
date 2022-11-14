;;; rustic-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rustic" "rustic.el" (0 0 0 0))
;;; Generated autoloads from rustic.el

(autoload 'rustic-mode "rustic" "\
Major mode for Rust code.

\\{rustic-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

(register-definition-prefixes "rustic" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-babel" "rustic-babel.el" (0 0 0 0))
;;; Generated autoloads from rustic-babel.el

(register-definition-prefixes "rustic-babel" '("cargo-toml-dependencies" "crate-dependencies" "org-babel-execute:rust" "rustic-"))

;;;***

;;;### (autoloads nil "rustic-cargo" "rustic-cargo.el" (0 0 0 0))
;;; Generated autoloads from rustic-cargo.el

(autoload 'rustic-cargo-test-run "rustic-cargo" "\
Start compilation process for 'cargo test' with optional TEST-ARGS.

\(fn &optional TEST-ARGS)" t nil)

(autoload 'rustic-cargo-test "rustic-cargo" "\
Run 'cargo test'.

If ARG is not nil, use value as argument and store it in `rustic-test-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-test-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-test-rerun "rustic-cargo" "\
Run 'cargo test' with `rustic-test-arguments'." t nil)

(autoload 'rustic-cargo-current-test "rustic-cargo" "\
Run 'cargo test' for the test near point." t nil)

(autoload 'rustic-cargo-test-dwim "rustic-cargo" "\
Run test or mod at point. Otherwise run `rustic-cargo-test'." t nil)

(autoload 'rustic-cargo-outdated "rustic-cargo" "\
Use 'cargo outdated' to list outdated packages in `tabulated-list-mode'.
Execute process in PATH.

\(fn &optional PATH)" t nil)

(autoload 'rustic-cargo-reload-outdated "rustic-cargo" "\
Update list of outdated packages." t nil)

(autoload 'rustic-cargo-mark-upgrade "rustic-cargo" "\
Mark an upgradable package." t nil)

(autoload 'rustic-cargo-mark-latest-upgrade "rustic-cargo" "\
Mark an upgradable package to the latest available version." t nil)

(autoload 'rustic-cargo-mark-all-upgrades-latest "rustic-cargo" "\
Mark all packages in the Package Menu to latest version." t nil)

(autoload 'rustic-cargo-mark-all-upgrades "rustic-cargo" "\
Mark all upgradable packages in the Package Menu." t nil)

(autoload 'rustic-cargo-menu-mark-unmark "rustic-cargo" "\
Clear any marks on a package." t nil)

(autoload 'rustic-cargo-upgrade-execute "rustic-cargo" "\
Perform marked menu actions." t nil)

(autoload 'rustic-cargo-new "rustic-cargo" "\
Run 'cargo new' to start a new package in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library.

\(fn PROJECT-PATH &optional BIN)" t nil)

(autoload 'rustic-cargo-init "rustic-cargo" "\
Run 'cargo init' to initialize a directory in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library.

\(fn PROJECT-PATH &optional BIN)" t nil)

(autoload 'rustic-cargo-run-command "rustic-cargo" "\
Start compilation process for 'cargo run' with optional RUN-ARGS.

\(fn &optional RUN-ARGS)" t nil)

(autoload 'rustic-cargo-run "rustic-cargo" "\
Run 'cargo run'.

If ARG is not nil, use value as argument and store it in `rustic-run-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-run-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-run-rerun "rustic-cargo" "\
Run 'cargo run' with `rustic-run-arguments'." t nil)

(autoload 'rustic-run-shell-command "rustic-cargo" "\
Run an arbitrary shell command using ARG for the current project.
Example: use it to provide an environment variable to your
application like this `env MYVAR=1 cargo run' so that it can read
it at the runtime.  As a byproduct, you can run any shell command
in your project like `pwd'

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-build "rustic-cargo" "\
Run 'cargo build' for the current project, allow configuring
`rustic-cargo-build-arguments' when prefix argument (C-u) is enabled.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-clean "rustic-cargo" "\
Run 'cargo clean' for the current project.

If ARG is not nil, use value as argument and store it in `rustic-clean-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clean-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-check "rustic-cargo" "\
Run 'cargo check' for the current project, allow configuring
`rustic-cargo-check-arguments' when prefix argument (C-u) is enabled.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-bench "rustic-cargo" "\
Run 'cargo bench' for the current project." t nil)

(autoload 'rustic-cargo-build-doc "rustic-cargo" "\
Build the documentation for the current project." t nil)

(autoload 'rustic-cargo-doc "rustic-cargo" "\
Open the documentation for the current project in a browser.
The documentation is built if necessary." t nil)

(autoload 'rustic-cargo-add "rustic-cargo" "\
Add crate to Cargo.toml using 'cargo add'.
If running with prefix command `C-u', read whole command from minibuffer.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-rm "rustic-cargo" "\
Remove crate from Cargo.toml using 'cargo rm'.
If running with prefix command `C-u', read whole command from minibuffer.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-upgrade "rustic-cargo" "\
Upgrade dependencies as specified in the local manifest file using 'cargo upgrade'.
If running with prefix command `C-u', read whole command from minibuffer.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-update "rustic-cargo" "\
Update dependencies as recorded in the local lock file.
If running with prefix command `C-u', use ARG by reading whole
command from minibuffer.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-login "rustic-cargo" "\
Add crates.io API token using `cargo login'.

`TOKEN' the token for interacting with crates.io. Visit [1] for
        how to get one

\[1] https://doc.rust-lang.org/cargo/reference/publishing.html#before-your-first-publish

\(fn TOKEN)" t nil)

(autoload 'rustic-cargo-install-rerun "rustic-cargo" "\
Run 'cargo install' with `rustic-install-arguments'." t nil)

(autoload 'rustic-cargo-install "rustic-cargo" "\
Install rust binary using 'cargo install'.
If running with prefix command `C-u', read whole command from minibuffer.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "rustic-cargo" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-clippy" "rustic-clippy.el" (0 0 0 0))
;;; Generated autoloads from rustic-clippy.el

(autoload 'rustic-cargo-clippy-run "rustic-clippy" "\
Run `cargo clippy' with optional ARGS.

\(fn &rest ARGS)" t nil)

(autoload 'rustic-cargo-lints "rustic-clippy" "\
Run cargo-lints with optional ARGS." t nil)

(autoload 'rustic-cargo-clippy "rustic-clippy" "\
Run 'cargo clippy'.

If ARG is not nil, use value as argument and store it in `rustic-clippy-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clippy-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-clippy-rerun "rustic-clippy" "\
Run 'cargo clippy' with `rustic-clippy-arguments'." t nil)

(register-definition-prefixes "rustic-clippy" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-comint" "rustic-comint.el" (0 0 0 0))
;;; Generated autoloads from rustic-comint.el

(autoload 'rustic-cargo-comint-run "rustic-comint" "\
Run 'cargo run' but for interactive programs.

If ARG is not nil, use value as argument and store it in `rustic-run-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-run-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-comint-run-rerun "rustic-comint" "\
Run 'cargo run' with `rustic-run-comint-arguments'." t nil)

(autoload 'rustic-cargo-plain-run "rustic-comint" "\
Run 'cargo run' for the current project.
If running with prefix command `C-u', read whole command from minibuffer.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "rustic-comint" '("poly-rustic-cargo-comint-switch-buffer-hook" "rustic-"))

;;;***

;;;### (autoloads nil "rustic-compile" "rustic-compile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rustic-compile.el

(autoload 'rustic-compile "rustic-compile" "\
Compile rust project.

If `compilation-read-command' is non-nil or if called with prefix
argument ARG then read the command in the minibuffer.  Otherwise
use `rustic-compile-command'.

In either store the used command in `compilation-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-recompile "rustic-compile" "\
Re-compile the program using `compilation-arguments'." t nil)

(register-definition-prefixes "rustic-compile" '("rust"))

;;;***

;;;### (autoloads nil "rustic-doc" "rustic-doc.el" (0 0 0 0))
;;; Generated autoloads from rustic-doc.el

(autoload 'rustic-doc-dumb-search "rustic-doc" "\
Search all projects and std for SEARCH-TERM.
Use this when `rustic-doc-search' does not find what you're looking for.
Add `universal-argument' to only search level 1 headers.
See `rustic-doc-search' for more information.

\(fn SEARCH-TERM)" t nil)

(autoload 'rustic-doc-search "rustic-doc" "\
Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc)
to limit the number of results.
To limit search results to only level 1 headers, add `universal-argument'
Level 1 headers are things like struct or enum names.
if ROOT is non-nil the search is performed from the root dir.
This function tries to be smart and limits the search results
as much as possible. If it ends up being so smart that
it doesn't manage to find what you're looking for, try `rustic-doc-dumb-search'.

\(fn SEARCH-TERM &optional ROOT)" t nil)

(autoload 'rustic-doc-convert-current-package "rustic-doc" "\
Convert the documentation for a project and its dependencies." t nil)

(autoload 'rustic-doc-setup "rustic-doc" "\
Setup or update rustic-doc filter and convert script. Convert std.
If NO-DL is non-nil, will not try to re-download
the pandoc filter and bash script.
NO-DL is primarily used for development of the filters.
If NOCONFIRM is non-nil, install all dependencies without prompting user.

\(fn &optional NO-DL NOCONFIRM)" t nil)

(autoload 'rustic-doc-mode "rustic-doc" "\
Convert rust html docs to .org, and browse the converted docs.

This is a minor mode.  If called interactively, toggle the
`Rustic-Doc mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rustic-doc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "rustic-doc" '("rustic-doc-"))

;;;***

;;;### (autoloads nil "rustic-expand" "rustic-expand.el" (0 0 0 0))
;;; Generated autoloads from rustic-expand.el

(autoload 'rustic-cargo-expand "rustic-expand" "\
Run 'cargo expand'.

If ARG is not nil, use value as argument and store it in
`rustic-expand-arguments'.  When calling this function from
`rustic-popup-mode', always use the value of
`rustic-expand-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-expand-rerun "rustic-expand" "\
Run 'cargo expand' with `rustic-expand-arguments'." t nil)

(register-definition-prefixes "rustic-expand" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-flycheck" "rustic-flycheck.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rustic-flycheck.el

(autoload 'rustic-flycheck-setup "rustic-flycheck" "\
Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout." t nil)

(register-definition-prefixes "rustic-flycheck" '("rustic-flycheck-"))

;;;***

;;;### (autoloads nil "rustic-interaction" "rustic-interaction.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rustic-interaction.el

(autoload 'rustic-open-dependency-file "rustic-interaction" "\
Open the 'Cargo.toml' file at the project root if the current buffer is
visiting a project." t nil)

(register-definition-prefixes "rustic-interaction" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-lsp" "rustic-lsp.el" (0 0 0 0))
;;; Generated autoloads from rustic-lsp.el

(autoload 'rustic-analyzer-macro-expand "rustic-lsp" "\
Default method for displaying macro expansion results.

\(fn RESULT)" t nil)

(register-definition-prefixes "rustic-lsp" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-playpen" "rustic-playpen.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rustic-playpen.el

(autoload 'rustic-playpen "rustic-playpen" "\
Create a shareable URL for the contents of the current region,
src-block or buffer on the Rust playpen.

\(fn BEGIN END)" t nil)

(register-definition-prefixes "rustic-playpen" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-popup" "rustic-popup.el" (0 0 0 0))
;;; Generated autoloads from rustic-popup.el

(autoload 'rustic-popup "rustic-popup" "\
Setup popup.
If directory is not in a rust project call `read-directory-name'.

\(fn &optional ARGS)" t nil)

(autoload 'rustic-popup-invoke-popup-action "rustic-popup" "\
Execute commands which are listed in `rustic-popup-commands'.

\(fn EVENT)" t nil)

(autoload 'rustic-popup-default-action "rustic-popup" "\
Change backtrace and `compilation-arguments' when executed on
corresponding line." t nil)

(autoload 'rustic-popup-cargo-command-help "rustic-popup" "\
Display help buffer for cargo command at point." t nil)

(autoload 'rustic-popup-kill-help-buffer "rustic-popup" "\
Kill popup help buffer and switch to popup buffer." t nil)

(register-definition-prefixes "rustic-popup" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-racer" "rustic-racer.el" (0 0 0 0))
;;; Generated autoloads from rustic-racer.el

(autoload 'rustic-racer-describe "rustic-racer" "\
Show a *Racer Help* buffer for the function or type at point." t nil)

(register-definition-prefixes "rustic-racer" '("racer-src-button" "rustic-racer-"))

;;;***

;;;### (autoloads nil "rustic-rustfix" "rustic-rustfix.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rustic-rustfix.el

(autoload 'rustic-rustfix "rustic-rustfix" "\
Run 'cargo fix'." t nil)

(register-definition-prefixes "rustic-rustfix" '("rustic-rustfix-"))

;;;***

;;;### (autoloads nil "rustic-rustfmt" "rustic-rustfmt.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rustic-rustfmt.el

(autoload 'rustic-cargo-fmt "rustic-rustfmt" "\
Use rustfmt via cargo." t nil)

(autoload 'rustic-format-region "rustic-rustfmt" "\
Format the current active region using rustfmt.

This operation requires a nightly version of rustfmt.

\(fn BEGIN END)" t nil)

(autoload 'rustic-format-buffer "rustic-rustfmt" "\
Format the current buffer using rustfmt." t nil)

(autoload 'rustic-format-file "rustic-rustfmt" "\
Unlike `rustic-format-buffer' format file directly and revert the buffer.

\(fn &optional FILE)" t nil)

(register-definition-prefixes "rustic-rustfmt" '("rustic-"))

;;;***

;;;### (autoloads nil "rustic-spellcheck" "rustic-spellcheck.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rustic-spellcheck.el

(autoload 'rustic-cargo-spellcheck "rustic-spellcheck" "\
Run 'cargo spellcheck'.

If ARG is not nil, use value as argument and store it in
`rustic-spellcheck-arguments'.  When calling this function from
`rustic-popup-mode', always use the value of
`rustic-spellcheck-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-cargo-spellcheck-rerun "rustic-spellcheck" "\
Run 'cargo spellcheck' with `rustic-spellcheck-arguments'." t nil)

(register-definition-prefixes "rustic-spellcheck" '("rustic-"))

;;;***

;;;### (autoloads nil nil ("rustic-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rustic-autoloads.el ends here
