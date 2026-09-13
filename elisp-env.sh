#!/bin/bash
# Shared environment for first-party Emacs Lisp checks.
#
# This file and its two callers are Bash so that shellcheck -x and shfmt -d
# cover them — locally through check.sh's SHELL_FILES, in CI by no longer being
# FILTER_REGEX_EXCLUDE'd — coverage their earlier move to Zsh had cost. The
# price: these scripts inherit the caller's environment. Under the old
# `#!/bin/zsh' they did not: Zsh reads ~/.zshenv even non-interactively, which
# is what put Homebrew's bin on PATH. Bash reads no startup file, so a caller
# that lacks `emacs' on PATH (launchd, cron, a hook) now fails at emacs_batch.
# A UTF-8 LANG is likewise the caller's to supply.
repo_root="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
readonly repo_root
readonly my_dir="$repo_root/emacs/.emacs.d/my"
readonly org_autotask_dir="$repo_root/emacs/.emacs.d/elpa/org-autotask"
if [[ ! -f "$org_autotask_dir/org-autotask.el" ]]; then
	echo "${BASH_SOURCE[0]##*/}: org-autotask submodule not checked out; run: git submodule update --init emacs/.emacs.d/elpa/org-autotask" >&2
	exit 1
fi

# No `shopt -s nullglob': shell options belong to the sourcing shell, and the
# Zsh original needed NULL_GLOB only because NOMATCH is on by default there.
# Both outcomes an unmatched pattern can have — nullglob's empty array, the
# default's literal pattern — are handled below, so the guard needs neither
# option. `failglob' aborts the assignment itself, upstream of any guard.
test_files=("$my_dir"/*-test.el)
readonly test_files
if [[ ${#test_files[@]} -eq 0 ]] || [[ ! -e "${test_files[0]}" ]]; then
	echo "${BASH_SOURCE[0]##*/}: no first-party Emacs Lisp tests found" >&2
	exit 1
fi

# Prefer newer submodule source and keep local mu4e packages off `load-path'.
emacs_batch() {
	emacs --batch -Q --eval "(setq load-prefer-newer t)" \
		-L "$my_dir" -L "$org_autotask_dir" "$@"
}
