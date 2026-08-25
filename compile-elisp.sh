#!/bin/zsh
# Byte-compile tested first-party Emacs Lisp and validate its declarations.

set -euo pipefail

source "${0:A:h}/elisp-env.sh"

# Compile each subject before its test so subject diagnostics appear first.
files=()
for test_file in "${test_files[@]}"; do
	files+=("${test_file%-test.el}.el" "$test_file")
done

scratch_dir="$(mktemp -d)"
readonly scratch_dir
trap 'rm -rf "$scratch_dir"' EXIT
# Keep diagnostic bytecode out of the source tree.
export ELISP_SCRATCH_DIR="$scratch_dir"

emacs_batch \
	--eval '(setq byte-compile-dest-file-function
                (lambda (file)
                  (expand-file-name
                   (concat (file-name-nondirectory file) "c")
                   (or (getenv "ELISP_SCRATCH_DIR")
                       (error "ELISP_SCRATCH_DIR is not set")))))' \
	--eval "(setq byte-compile-warnings '(callargs unresolved))" \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(provide 'mu4e-message)" \
	--eval "(provide 'mu4e-autotask)" \
	-f batch-byte-compile "${files[@]}"

# Validate `declare-function' arglists against available dependency sources.
emacs_batch \
	-L "$repo_root/emacs/.emacs.d/elpa/mu4e-autotask-1.0.0" \
	-L "$repo_root/emacs/.emacs.d/elpa/request-20250219.2213" \
	--eval '(progn
                  (require (quote check-declare))
                  (let ((files argv))
                    (setq argv nil)
                    (when (apply (function check-declare-files) files)
                      (kill-emacs 1))))' \
	"${files[@]}"
