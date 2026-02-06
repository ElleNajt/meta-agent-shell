#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Setting up meta-agent-shell..."

# Create meta-agent directory
mkdir -p ~/.claude-meta
echo "Created ~/.claude-meta/"

# Link meta-agent CLAUDE.md
if [ -L ~/.claude-meta/CLAUDE.md ]; then
    rm ~/.claude-meta/CLAUDE.md
fi
ln -s "$SCRIPT_DIR/meta-claude.md" ~/.claude-meta/CLAUDE.md
echo "Linked meta-agent CLAUDE.md"

# Create logs directory
mkdir -p ~/.meta-agent-shell/logs
echo "Created ~/.meta-agent-shell/logs/"

echo ""
echo "Done! Now:"
echo ""
echo "1. Add to your shell config (.bashrc/.zshrc):"
echo "   export PATH=\"$SCRIPT_DIR/bin:\$PATH\""
echo ""
echo "2. Add to ~/.claude/CLAUDE.md:"
echo "   @$SCRIPT_DIR/agent-overview.md"
echo ""
echo "3. Add to your Emacs config:"
echo "   (use-package meta-agent-shell :after agent-shell)"
