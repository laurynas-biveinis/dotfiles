#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.

set -eu -o pipefail

readonly BASH_FILES=(check.sh compile-elisp.sh elisp-env.sh relint.sh setup-ubuntu.sh setup-ubuntu-mbp-late-2013.sh setup-ubuntu-mysql-work.sh test-elisp.sh)
# The Zsh root scripts. shellcheck refuses Zsh (SC1071), so this list carries
# that dialect and only zsh -n reads these.
readonly ZSH_FILES=(setup-macos-ai.sh setup-macos-cpp.sh setup-macos-emacs.sh setup-macos-gh.sh setup-macos-git.sh setup-macos-mysql-work.sh setup-macos-python.sh setup-macos-rust.sh setup-macos.sh)
readonly PYTHON_FILES=(ai/.claude/hooks/*.py scripts/usr/bin/xml2qif scripts/usr/bin/*.py dotfiles/tests/*.py)
readonly JSON_FILES=(ai/.claude/settings.json biome.json)

ERRORS=0
BASH_SYNTAX_FAILED=0

# Shell

# One file per invocation: `<shell> -n f1 f2` parses f1 and turns the rest into
# positional parameters — rc 0, no diagnostic — in bash, sh and zsh alike. The
# stages below do take file lists, so collapsing this loop reads as cleanup;
# that is how the old multi-file call spent ten months parsing check.sh alone.
# Gate on output as well as status: Bash 3.2 diagnoses a nested `(' in an array
# assignment yet exits 0. Two things this stage still does not prove: `bash -n`
# uses the Bash grammar whatever the shebang says, so POSIX conformance of the
# #!/bin/sh scripts is shellcheck's SC3xxx, not this stage's; and it is silent
# on an unterminated here-document. shellcheck -x and shfmt -d reject both,
# which is why gating on output does not make -n an oracle.
syntax_check() {
	local interpreter="$1"
	shift
	local file output failed=0
	for file in "$@"; do
		output="$("$interpreter" -n "$file" 2>&1)" || failed=1
		if [ -n "$output" ]; then
			printf '%s\n' "$output" >&2
			failed=1
		fi
	done
	return "$failed"
}

echo -n "Checking Bash syntax... ${BASH_FILES[*]} "
if syntax_check bash "${BASH_FILES[@]}"; then
	echo "OK!"
else
	echo "Bash syntax check failed!"
	ERRORS=$((ERRORS + 1))
	BASH_SYNTAX_FAILED=1
fi

if [ $BASH_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running shellcheck... ${BASH_FILES[*]} "
	if shellcheck "${BASH_FILES[@]}"; then
		echo "OK!"
	else
		echo "shellcheck check failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running shfmt to format Bash scripts... ${BASH_FILES[*]} "
	if shfmt -w "${BASH_FILES[@]}"; then
		echo "OK!"
	else
		echo "shfmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping shellcheck, and shfmt due to previous errors"
fi

echo -n "Checking Zsh syntax... ${ZSH_FILES[*]} "
if syntax_check zsh "${ZSH_FILES[@]}"; then
	echo "OK!"
else
	echo "Zsh syntax check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Markdown files... "
MD_FILES=()
while IFS= read -r file; do
	MD_FILES+=("$file")
done < <(
	find . -maxdepth 1 -type f -name "*.md"
	find ai -type f -name "*.md"
)
echo "${MD_FILES[*]} "

echo -n "Checking Markdown formatting with prettier... "
if [ ${#MD_FILES[@]} -gt 0 ]; then
	if prettier --log-level warn --check "${MD_FILES[@]}"; then
		echo "OK!"
	else
		echo "prettier check failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "No Markdown files found, skipping"
fi

echo -n "Checking Markdown with markdownlint-cli... "
if [ ${#MD_FILES[@]} -gt 0 ]; then
	if markdownlint "${MD_FILES[@]}"; then
		echo "OK!"
	else
		echo "markdownlint check failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "No Markdown files found, skipping"
fi

echo -n "Checking terminology... "
if textlint --rule terminology ai CLAUDE.md; then
	echo "OK!"
else
	echo "textlint check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking GitHub workflows... $(echo .github/workflows/*.yml) "
if actionlint .github/workflows/*.yml; then
	echo "OK!"
else
	echo "actionlint check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking GitHub workflows security with zizmor... "
if zizmor --offline .github/workflows/*.yml; then
	echo "OK!"
else
	echo "zizmor check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking YAML formatting... $(echo .github/workflows/*.yml) gh/.config/gh/hosts.yml"
if prettier --log-level warn --check .github/workflows/*.yml gh/.config/gh/hosts.yml; then
	echo "OK!"
else
	echo "prettier check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking JSON formatting... ${JSON_FILES[*]} "
if prettier --log-level warn --check "${JSON_FILES[@]}"; then
	echo "OK!"
else
	echo "prettier check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running biome format checker... "
if npx @biomejs/biome format .; then
	echo "OK!"
else
	echo "biome format check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running biome linter... "
if npx @biomejs/biome lint .; then
	echo "OK!"
else
	echo "biome lint check failed!"
	ERRORS=$((ERRORS + 1))
fi

# Python
echo -n "Checking Python formatting with black... ${PYTHON_FILES[*]} "
if black --check "${PYTHON_FILES[@]}" 2>/dev/null; then
	echo "OK!"
else
	echo "black check failed! Run 'black ${PYTHON_FILES[*]}' to fix"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Python import sorting with isort... ${PYTHON_FILES[*]} "
if isort --check-only --diff "${PYTHON_FILES[@]}" 2>/dev/null; then
	echo "OK!"
else
	echo "isort check failed! Run 'isort ${PYTHON_FILES[*]}' to fix"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running pylint... ${PYTHON_FILES[*]} "
if pylint "${PYTHON_FILES[@]}"; then
	echo "OK!"
else
	echo "pylint check failed!"
	ERRORS=$((ERRORS + 1))
fi

# Super-linter runs mypy from this same config file, so pointing at it is what
# makes the two sides agree — mypy never discovers a config under
# .github/linters on its own. The cache goes outside the tree: neither
# .gitignore nor the jscpd ignore list covers .mypy_cache, and the jscpd stage
# scans '.'.
echo -n "Running mypy... ${PYTHON_FILES[*]} "
if mypy --config-file .github/linters/.mypy.ini \
	--cache-dir "${TMPDIR:-/tmp}/dotfiles-check-mypy-cache" \
	"${PYTHON_FILES[@]}"; then
	echo "OK!"
else
	echo "mypy check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running Python unit tests... "
if python3 -B -m unittest discover -s dotfiles/tests; then
	echo "OK!"
else
	echo "Python unit tests failed!"
	ERRORS=$((ERRORS + 1))
fi

# Emacs Lisp
echo -n "Byte-compiling tested Emacs Lisp files... "
if ./compile-elisp.sh; then
	echo "OK!"
else
	echo "byte-compile check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running relint on Emacs Lisp files... "
if ./relint.sh; then
	echo "OK!"
else
	echo "relint check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo "Running Emacs Lisp tests..."
if ./test-elisp.sh; then
	echo "Emacs Lisp tests OK!"
else
	echo "Emacs Lisp tests failed!"
	ERRORS=$((ERRORS + 1))
fi

# Copy/paste detection (matches Super-Linter's JSCPD)
echo -n "Running jscpd copy/paste detection... "
if jscpd_out=$(npx --yes jscpd --config .github/linters/.jscpd.json . 2>&1); then
	echo "OK!"
else
	echo "jscpd check failed!"
	echo "$jscpd_out"
	ERRORS=$((ERRORS + 1))
fi

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "All checks passed successfully!"
else
	echo "$ERRORS check(s) failed!"
	exit 1
fi
