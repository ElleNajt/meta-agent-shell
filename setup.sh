#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Setting up meta-agent-shell..."

# Create meta-agent directory
mkdir -p ~/.claude-meta
echo "Created ~/.claude-meta/"

# Remove old CLAUDE.md symlink (instructions now injected via session-meta)
if [ -L ~/.claude-meta/CLAUDE.md ]; then
    rm ~/.claude-meta/CLAUDE.md
    echo "Removed old CLAUDE.md symlink (no longer needed)"
fi

# Create logs directory
mkdir -p ~/.meta-agent-shell/logs
echo "Created ~/.meta-agent-shell/logs/"

# Create config file if it doesn't exist
if [ ! -f ~/.meta-agent-shell/config.org ]; then
    cat >~/.meta-agent-shell/config.org <<'CONF'
# Meta-agent config - add @file references to include in the system prompt
# Example:
# @/absolute/path/to/priorities.org
# @relative/path/from/here.org
CONF
    echo "Created ~/.meta-agent-shell/config.org"
fi

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
echo ""
echo "4. Edit ~/.meta-agent-shell/config.org to add @file references"
echo "   for context the meta-agent should have (e.g. priorities)."
echo "   Example: @/home/you/notes/priorities.org"
