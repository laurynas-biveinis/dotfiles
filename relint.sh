#!/bin/bash
# relint.sh - Run relint (Emacs Lisp regexp linter) on first-party .el files,
# loading relint from the vendored elpa/ so no network install is needed.
#
# With no arguments, lints every tracked first-party Emacs Lisp file, excluding
# the vendored elpa/ and the generated custom.el, plus ~/secrets.el and
# ~/secrets-local.el when they exist (untracked, machine-local config). Any file
# arguments given are treated as repo-relative and linted instead. Shared by
# check.sh and the Emacs Lisp CI workflow.

set -eu -o pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$repo_root" || exit 1

files=("$@")
if [ ${#files[@]} -eq 0 ]; then
	while IFS= read -r file; do
		files+=("$file")
	done < <(git ls-files '*.el' \
		':(exclude)emacs/.emacs.d/elpa/**' \
		':(exclude)emacs/.emacs.d/custom.el')
	for secret in "$HOME/secrets.el" "$HOME/secrets-local.el"; do
		if [ -f "$secret" ]; then
			files+=("$secret")
		fi
	done
fi

if [ ${#files[@]} -eq 0 ]; then
	echo "relint.sh: no Emacs Lisp files to lint" >&2
	exit 1
fi

emacs -Q --batch \
	--eval "(setq package-user-dir \"$repo_root/emacs/.emacs.d/elpa\")" \
	--eval "(require 'package)" \
	--eval "(package-initialize)" \
	--eval "(require 'relint)" \
	-f relint-batch "${files[@]}"
