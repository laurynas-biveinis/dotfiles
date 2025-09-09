#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.

set -eu -o pipefail

readonly SHELL_FILES=(check.sh)

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

echo -n "Checking Markdown files... $(echo ./*.md) "
if mdl --no-verbose ./*.md; then
	echo "OK!"
else
	echo "mdl check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking terminology... "
if textlint --rule terminology ai; then
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

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "All checks passed successfully!"
else
	echo "$ERRORS check(s) failed!"
	exit 1
fi
