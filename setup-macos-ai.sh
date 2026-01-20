#!/bin/zsh

set -euo pipefail

npm install -g @anthropic-ai/claude-code

claude plugin marketplace add https://github.com/wakatime/claude-code-wakatime.git
claude plugin i claude-code-wakatime@wakatime
