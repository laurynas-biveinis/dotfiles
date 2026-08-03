#!/bin/bash
# Run first-party Emacs Lisp tests.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly repo_root
readonly test_dir="$repo_root/emacs/.emacs.d/my"

shopt -s nullglob
test_files=("$test_dir"/*-test.el)
if ((${#test_files[@]} == 0)); then
	echo "test-elisp.sh: no first-party Emacs Lisp tests found" >&2
	exit 1
fi

load_args=()
for test_file in "${test_files[@]}"; do
	load_args+=(-l "$test_file")
done

exec emacs --batch -Q -L "$test_dir" "${load_args[@]}" \
	-f ert-run-tests-batch-and-exit
