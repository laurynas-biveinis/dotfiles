#!/bin/zsh

set -euo pipefail

# Claude
npm install -g @anthropic-ai/claude-code

claude plugin marketplace add https://github.com/wakatime/claude-code-wakatime.git
claude plugin i claude-code-wakatime@wakatime

# Codex
curl -fsSL https://chatgpt.com/codex/install.sh | sh
