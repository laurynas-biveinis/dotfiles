#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.

set -eu -o pipefail

readonly SHELL_FILES=(check.sh)
readonly PYTHON_FILES=(ai/.claude/hooks/*.py)

ERRORS=0
SHELL_SYNTAX_FAILED=0

# Shell

echo -n "Checking shell syntax... ${SHELL_FILES[*]} "
if bash -n "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "shell syntax check failed!"
	ERRORS=$((ERRORS + 1))
	SHELL_SYNTAX_FAILED=1
fi

if [ $SHELL_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running shellcheck... ${SHELL_FILES[*]} "
	if shellcheck "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shellcheck check failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running shfmt to format all shell scripts... ${SHELL_FILES[*]} "
	if shfmt -w "${SHELL_FILES[@]}"; then
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
done < <(find . -maxdepth 1 -name "*.md" && find ai -name "*.md" 2>/dev/null || true)
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

echo -n "Checking Markdown with mdl... "
if [ ${#MD_FILES[@]} -gt 0 ]; then
	if mdl --no-verbose "${MD_FILES[@]}"; then
		echo "OK!"
	else
		echo "mdl check failed"
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

echo -n "Checking JSON formatting... ai/.claude/settings.json "
if prettier --log-level warn --check ai/.claude/settings.json; then
	echo "OK!"
else
	echo "prettier check failed!"
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

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "All checks passed successfully!"
else
	echo "$ERRORS check(s) failed!"
	exit 1
fi
