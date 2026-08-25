#!/bin/zsh
# Shared environment for first-party Emacs Lisp checks.

repo_root="${${(%):-%x}:A:h}"
readonly repo_root
readonly my_dir="$repo_root/emacs/.emacs.d/my"
readonly org_autotask_dir="$repo_root/emacs/.emacs.d/elpa/org-autotask"
if [[ ! -f "$org_autotask_dir/org-autotask.el" ]]; then
	echo "${0##*/}: org-autotask submodule not checked out; run: git submodule update --init emacs/.emacs.d/elpa/org-autotask" >&2
	exit 1
fi

setopt NULL_GLOB
test_files=("$my_dir"/*-test.el)
readonly test_files
if ((${#test_files[@]} == 0)); then
	echo "${0##*/}: no first-party Emacs Lisp tests found" >&2
	exit 1
fi

# Prefer newer submodule source and keep local mu4e packages off `load-path'.
emacs_batch() {
	emacs --batch -Q --eval "(setq load-prefer-newer t)" \
		-L "$my_dir" -L "$org_autotask_dir" "$@"
}
