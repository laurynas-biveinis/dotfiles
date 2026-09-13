#!/bin/bash
# Run first-party Emacs Lisp tests.

set -euo pipefail

# shellcheck source=elisp-env.sh source-path=SCRIPTDIR
source "$(dirname -- "${BASH_SOURCE[0]}")/elisp-env.sh"

load_args=()
for test_file in "${test_files[@]}"; do
	load_args+=(-l "$test_file")
done

emacs_batch "${load_args[@]}" -f ert-run-tests-batch-and-exit
