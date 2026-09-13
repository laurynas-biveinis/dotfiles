#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.

set -eu -o pipefail

# The rule these lists follow: derive the file set where an extension and a
# location decide membership, name it where a shebang decides it. A shebang
# grep would derive those too, so naming is a choice and not a necessity: a
# derived ZSH_FILES could not reuse the exclusions super-linter applies, since
# mysql-work.sh is excluded there and required here, so it would cost a second
# partial mirror of FILTER_REGEX_EXCLUDE; SHELL_FILES is a multi-term set
# difference rather than one grep. Derived: the Markdown, workflow, biome, jscpd and
# Emacs Lisp stages. Location bounds most of the derived
# sets as much as extension does — Markdown to the root and ai/, the
# byte-compile and test set to emacs/.emacs.d/my, the workflow stages to
# .github/workflows/*.yml — so a file of the right kind elsewhere still needs
# an edit. JSON_FILES predates the rule and does not follow it: it is a frozen
# literal, and it already omits .github/linters/.jscpd.json. Each way costs
# something: a named list does not grow, so a new script that is not added
# here is unchecked locally and CI is the backstop, except for zsh -n, which
# CI does not run; a derived set grows by itself, but it can pull a file in
# silently, and an index-derived one sees a new file only once it is staged.
#
# The same green-local/red-CI asymmetry exists one level up, at the stage set
# and the linter configs, where no file list can close it: super-linter also
# runs codespell and ruff, which have no stage here, and runs pylint against
# its own bundled .python-lint rather than this repo's defaults.

# Everything bash -n and shellcheck cover: every non-Zsh shell file
# super-linter shellchecks — the root scripts, the module files, and the
# extension-less #!/bin/sh programs under */usr/bin/, which CI has linted all
# along. shellcheck refuses Zsh (SC1071), so ZSH_FILES carries that dialect. A
# shebang decides membership here, so this list is named. SHFMT_FILES takes it
# whole, so the entries beyond the root are format-checked locally too.
readonly SHELL_FILES=(
	check.sh
	compile-elisp.sh
	elisp-env.sh
	relint.sh
	setup-centos9.sh
	setup-cpu-benchmark.sh
	setup-macos-mail.sh
	setup-ubuntu-mbp-late-2013.sh
	setup-ubuntu-mysql-work.sh
	setup-ubuntu.sh
	test-elisp.sh
	aerospike/setup-centos6.sh
	aerospike/setup-macos.sh
	aerospike/setup-ubuntu.sh
	bash/.noninteractive_init.bash
	ccache/.bash.d/noninteractive_init/ccache.bash
	cpp/.zsh.d/env/cmake.sh
	emacs/.bash.d/rc/emacs.sh
	git/.bash.d/noninteractive_init/git.sh
	gnupg/.zsh.d/env/gnupg.sh
	python/.zsh.d/rc/python.sh
	ripgrep/.zsh.d/env/ripgrep.sh
	rust/.zsh.d/env/rust.sh
	wakatime/.bash.d/rc/wakatime.bash
	ccache/usr/bin/ccachestats
	git/usr/bin/gittake
	nightly/usr/bin/reloadnightly
	rust/usr/bin/rustupdate
	scripts/usr/bin/npmupdate
)

# The Zsh root scripts.
readonly ZSH_ROOT_FILES=(
	setup-macos-ai.sh
	setup-macos-cpp.sh
	setup-macos-emacs.sh
	setup-macos-gh.sh
	setup-macos-git.sh
	setup-macos-mysql-work.sh
	setup-macos-python.sh
	setup-macos-rust.sh
	setup-macos.sh
)

# Everything zsh -n covers: the root scripts plus every other #!/bin/zsh file
# in the tree. shellcheck refuses Zsh (SC1071); shfmt -d parses the root
# scripts here, and the #!/bin/zsh files super-linter sees in CI, but only as
# far as its Zsh support reaches, so this is the only check that uses Zsh's own
# grammar. The entries beyond the root scripts are not in SHFMT_FILES;
# mysql-work.sh is FILTER_REGEX_EXCLUDE'd in CI besides, leaving this stage as
# its only check anywhere.
readonly ZSH_FILES=(
	"${ZSH_ROOT_FILES[@]}"
	aerospike/usr/bin/as_branch
	emacs/usr/bin/em-commit-metadata-update
	emacs/usr/bin/em-commit-pkg-update
	emacs/usr/bin/em-commit-update
	emacs/usr/bin/em-regen-info-dir
	git/usr/bin/gitrmworktree
	mail/usr/bin/reviewsyncmail
	mail/usr/bin/syncmail
	mysql-work/.zsh.d/rc/mysql-work.sh
	mysql-work/usr/bin/fetchworksrc
	mysql-work/usr/bin/gca
	mysql-work/usr/bin/patch2testlist
	mysql-work/usr/bin/reupmerge
	nightly/usr/bin/nightly
	nightly/usr/bin/reviewnightly
	python/usr/bin/pythonupdate
	scripts/usr/bin/dotfilesupdate
	scripts/usr/bin/macupdate
	wakatime/.zsh.d/rc/wakatime.zsh
	zsh/.zsh.d/paths
	zsh/.zshenv
)

# shfmt reads the shebang and handles Zsh too. The Zsh half stops at the root
# because ZSH_FILES cannot be taken whole: mysql-work.sh is
# FILTER_REGEX_EXCLUDE'd exactly because shfmt would reformat it, and the root
# boundary drops it without a second mirror of the regex.
readonly SHFMT_FILES=(
	"${SHELL_FILES[@]}"
	"${ZSH_ROOT_FILES[@]}"
)

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
# on an unterminated here-document. shellcheck and shfmt reject both,
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

echo -n "Checking shell syntax with bash -n... ${#SHELL_FILES[@]} files "
if syntax_check bash "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "Shell syntax check failed!"
	ERRORS=$((ERRORS + 1))
	BASH_SYNTAX_FAILED=1
fi

echo -n "Checking Zsh syntax with zsh -n... ${#ZSH_FILES[@]} files "
if syntax_check zsh "${ZSH_FILES[@]}"; then
	echo "OK!"
else
	echo "Zsh syntax check failed!"
	ERRORS=$((ERRORS + 1))
fi

if [ $BASH_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running shellcheck... ${#SHELL_FILES[@]} files "
	if shellcheck "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shellcheck check failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running shfmt to format shell scripts... ${#SHFMT_FILES[@]} files "
	if shfmt -w "${SHFMT_FILES[@]}"; then
		echo "OK!"
	else
		echo "shfmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping shellcheck, and shfmt due to previous errors"
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
