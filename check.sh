#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.

set -eu -o pipefail

# The rule these lists follow: derive the file set where an extension and a
# location decide membership, name it where a shebang decides it. A shebang
# grep would derive those too, so naming is a choice and not a necessity: a
# derived ZSH_FILES could not reuse SUPER_LINTER_EXCLUDES, since mysql-work.sh
# is excluded there and required here, so it would cost a second partial
# mirror of FILTER_REGEX_EXCLUDE; SHELL_FILES is a multi-term set difference
# rather than one grep. Derived: the Markdown, workflow, biome, jscpd and
# Emacs Lisp stages, and the .py half of PYTHON_FILES. MODE_PATHSPECS is both,
# extension globs plus the named arrays. Location bounds most of the derived
# sets as much as extension does — Markdown to the root and ai/, the
# byte-compile and test set to emacs/.emacs.d/my, the workflow stages to
# .github/workflows/*.yml — so a file of the right kind elsewhere still needs
# an edit. JSON_FILES predates the rule and does not follow it: it is a frozen
# literal, and it already omits .github/linters/.jscpd.json. Each way costs
# something: a named list does not grow, so a new script that is not added
# here is unchecked locally and CI is the backstop, except for zsh -n, which
# CI does not run; a derived set grows by itself, but it can pull a file in
# silently, and an index-derived one sees a new file only once it is staged.
# SUPER_LINTER_EXCLUDES is the one place FILTER_REGEX_EXCLUDE is mirrored; see
# its comment below.
#
# The same green-local/red-CI asymmetry exists one level up, at the stage set
# and the linter configs, where no file list can close it: super-linter also
# runs codespell and ruff, which have no stage here, and runs pylint against
# its own bundled .python-lint rather than this repo's defaults.

# Everything bash -n and shellcheck -x cover: every non-Zsh shell file
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

# The four FILTER_REGEX_EXCLUDE alternatives, as pathspecs: elpa/
# (third-party), .venv/ (virtual env), and the two files shfmt would reformat.
# Super-linter drops these from its file list before any validator runs, so it
# demands nothing of them. Both derivations below mirror it from here, so the
# regex has one counterpart in this file rather than two partial ones. The
# leading * is what makes each entry the same predicate as its alternative:
# each regex alternative sits inside .*(...).* and so matches at any depth,
# while a pathspec carrying no such wildcard is anchored at the repository
# root. A leading * crosses / and matches empty, so it covers both.
readonly SUPER_LINTER_EXCLUDES=(
	':(exclude)*emacs/.emacs.d/elpa/**'
	':(exclude)*.venv/**'
	':(exclude)*mysql-work/.zsh.d/rc/mysql-work.sh'
	':(exclude)*zsh/.p10k.zsh'
)

# The files super-linter's bash-exec requires mode 755 on, including ones that
# are never run, as pathspecs rather than a list. It runs on actions/checkout's
# tree, so the mode that matters is the one Git records, not the working-tree
# bit — which is why the stage reads the index. The globs cover what an
# extension decides; SHELL_FILES and ZSH_FILES supply the rest, because
# super-linter also detects a shell file by its shebang and every entry of
# those two arrays carries one or the other. Reusing them adds no list to
# maintain: a new extension-less program has to join one of them anyway for
# syntax and lint coverage, so this stage follows the edit that was already
# owed instead of silently not covering it. A hand-kept list could only catch
# a mode regression in a file someone had remembered to add, never the new
# file the stage exists for; four such files reached master at 100644 and were
# flipped in later linter-fix commits. Symlinks carry no mode
# of their own and are skipped by mode below.
readonly MODE_PATHSPECS=(
	'*.sh'
	'*.bash'
	'*.zsh'
	"${SHELL_FILES[@]}"
	"${ZSH_FILES[@]}"
	"${SUPER_LINTER_EXCLUDES[@]}"
)

ERRORS=0

# Derived by extension, then extended by name. Super-linter's rule is ".py
# extension or Python shebang, minus FILTER_REGEX_EXCLUDE"; the extension half
# is mechanically reproducible, so a new .py file enrols itself here instead of
# being remembered. The tools read the working tree, so the listing does too:
# --others --exclude-standard admits a new file before it is staged, and the
# existence test drops a tracked file deleted but not yet staged, as the globs
# this replaced did. It needs -z: without it git quotes unusual names, and the
# test would drop those silently; tr turns the NULs back into the newlines the
# loop reads, leaving only a path with a literal newline in it unhandled. The
# status is captured because a failed listing must fail the stage rather than
# silently shrink the set. The elpa and .venv excludes it takes from
# SUPER_LINTER_EXCLUDES are not optional: a bare glob pulls in the vendored
# elpa copies, and .venv is hidden only by the .gitignore the virtualenv tool
# wrote inside it, which not every tool writes. The other two match no .py
# file, which costs nothing and keeps one mirror of the regex. The
# extension-less programs are named, for the reason the header gives. Neither
# half was current before: four of the five programs were unnamed, and
# scripts/usr/lib/python/common.py matched none of the replaced globs — all of
# them linted by CI throughout.
PYTHON_FILES=()
python_listing=$(git ls-files -z --cached --others --exclude-standard -- '*.py' "${SUPER_LINTER_EXCLUDES[@]}" | tr '\000' '\n') || {
	echo "listing .py files failed (git names it above): the Python stages below cover only the named programs"
	ERRORS=$((ERRORS + 1))
}
while IFS= read -r file; do
	if [ -f "$file" ]; then
		PYTHON_FILES+=("$file")
	fi
done <<<"$python_listing"
PYTHON_FILES+=(
	scripts/usr/bin/finbee2qif
	scripts/usr/bin/goindex2qif
	scripts/usr/bin/ib2qif
	scripts/usr/bin/invl2qif
	scripts/usr/bin/xml2qif
)
readonly PYTHON_FILES
readonly JSON_FILES=(ai/.claude/settings.json biome.json)

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

echo -n "Checking shell syntax with bash -n... ${#SHELL_FILES[@]} files "
if syntax_check bash "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "Shell syntax check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Zsh syntax with zsh -n... ${#ZSH_FILES[@]} files "
if syntax_check zsh "${ZSH_FILES[@]}"; then
	echo "OK!"
else
	echo "Zsh syntax check failed!"
	ERRORS=$((ERRORS + 1))
fi

# --error-unmatch: without it a pathspec that matches nothing still exits 0,
# and the stage reports OK on a list that silently stopped covering it. A
# positive entry fails when it matches nothing in the index — a named path once
# it leaves the index or before it ever enters one, a glob once its match set
# empties. Excludes never trip
# the flag, and a glob whose matches are all excluded still counts as matched,
# so that case shrinks silently instead. git still prints the rows it did
# match, so those are checked either way. A new file has no recorded mode, so
# the globs miss it until it is staged, while a named entry for it trips
# --error-unmatch instead — which is why the message offers staging.
# No -z: the path reaches only the failure message, so git quoting an unusual
# name is cosmetic here, unlike the PYTHON_FILES listing where a quoted name
# fails the -f test and drops the file. -z would also collapse every row into
# one, since bash strips NULs in command substitution, costing the capture.
echo -n "Checking shell script modes... "
MODES_STATUS=0
shell_modes="$(git ls-files -s --error-unmatch -- "${MODE_PATHSPECS[@]}")" || MODES_STATUS=$?
NON_EXEC=()
while read -r mode _ _ path; do
	# An empty $shell_modes still feeds the here-string one empty line, and
	# 120000 is a symlink, which carries no mode of its own.
	if [ -n "$mode" ] && [ "$mode" != 100755 ] && [ "$mode" != 120000 ]; then
		NON_EXEC+=("$path")
	fi
done <<<"$shell_modes"
if [ "$MODES_STATUS" -eq 0 ] && [ ${#NON_EXEC[@]} -eq 0 ]; then
	echo "OK!"
else
	if [ "$MODES_STATUS" -ne 0 ]; then
		echo "a MODE_PATHSPECS entry matched nothing in the index (git names it above): stage it if the file is new, or fix or drop the entry if it was renamed or deleted"
	fi
	if [ ${#NON_EXEC[@]} -gt 0 ]; then
		echo "recorded mode is not 100755 (super-linter bash-exec requires it): ${NON_EXEC[*]}"
		echo "  chmod +x them and stage the mode change; a bare chmod is not enough"
	fi
	ERRORS=$((ERRORS + 1))
fi

# Not gated on the syntax check: shellcheck brings its own parser and reports
# its own parse errors at error severity, so its result never depended on the
# bash -n one, and skipping the stage would discard every other file's
# diagnostics to suppress a three-line cascade.
echo -n "Running shellcheck... ${#SHELL_FILES[@]} files "
if shellcheck -x "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "shellcheck check failed"
	ERRORS=$((ERRORS + 1))
fi

# Super-linter runs shfmt in check mode, so rewriting in place here would report
# OK on exactly the content CI rejects.
echo -n "Checking shell script formatting... ${#SHFMT_FILES[@]} files "
if shfmt -d "${SHFMT_FILES[@]}"; then
	echo "OK!"
else
	echo "shfmt check failed! Run 'shfmt -w ${SHFMT_FILES[*]}' to fix"
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
if black --check -q "${PYTHON_FILES[@]}"; then
	echo "OK!"
else
	BLACK_STATUS=$?
	if [ "$BLACK_STATUS" -eq 1 ] || [ "$BLACK_STATUS" -eq 123 ]; then
		# Quiet mode also hides the filenames of formatting violations.
		black --check "${PYTHON_FILES[@]}" || BLACK_STATUS=$?
	fi
	echo "black check failed! Run 'black ${PYTHON_FILES[*]}' to fix"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Python import sorting with isort... ${PYTHON_FILES[*]} "
if isort --check-only --diff "${PYTHON_FILES[@]}"; then
	echo "OK!"
else
	echo "isort check failed! Run 'isort ${PYTHON_FILES[*]}' to fix"
	ERRORS=$((ERRORS + 1))
fi

# pylint normally encodes message categories in its exit status. Exit 1 also
# covers configured score/fail-on failures and early aborts, so status alone
# cannot establish whether analysis completed.
echo -n "Running pylint... ${PYTHON_FILES[*]} "
PYLINT_STATUS=0
pylint "${PYTHON_FILES[@]}" || PYLINT_STATUS=$?
if [ "$PYLINT_STATUS" -eq 0 ]; then
	echo "OK!"
else
	echo "pylint check failed!"
	if [ "$PYLINT_STATUS" -gt 31 ]; then
		echo "  exit $PYLINT_STATUS is outside pylint's message bitmask, so pylint did not complete: 32 usage error, 126/127 not runnable, 128+N signal"
	elif [ $((PYLINT_STATUS & 1)) -ne 0 ]; then
		echo "  bit 0 set: pylint may have reported a fatal error, failed a configured score/fail-on check, or aborted; the report may be incomplete — inspect its output above"
		echo "  if those diagnostics suggest a dependency conflict, run 'pip check'"
	fi
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
