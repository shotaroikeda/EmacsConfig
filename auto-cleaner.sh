#!/usr/bin/env bash
find . -name "*" -depth 1 -not -path "./.git" \
     -exec rm -rf {} +; # Remove all files but the .git to enable resetting
git reset --hard	# Reset all to reestablish directory structure
