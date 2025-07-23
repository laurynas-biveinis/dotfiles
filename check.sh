#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.

set -eu -o pipefail

ERRORS=0

echo -n "Checking Markdown files... $(echo ./*.md) "
if mdl --no-verbose ./*.md; then
    echo "OK!"
else
    echo "mdl check failed"
    ERRORS=$((ERRORS + 1))
fi

echo -n "Checking GitHub workflows... $(echo .github/workflows/*.yml) "
if actionlint .github/workflows/*.yml; then
    echo "OK!"
else
    echo "actionlint check failed!"
    ERRORS=$((ERRORS + 1))
fi

# Final result
if [ $ERRORS -eq 0 ]; then
    echo "All checks passed successfully!"
else
    echo "$ERRORS check(s) failed!"
    exit 1
fi
