#!/bin/zsh
# Run first-party Emacs Lisp tests.

set -euo pipefail

source "${0:A:h}/elisp-env.sh"

load_args=()
for test_file in "${test_files[@]}"; do
	load_args+=(-l "$test_file")
done

emacs_batch "${load_args[@]}" -f ert-run-tests-batch-and-exit
