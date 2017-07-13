#!/usr/bin/env bash
find . -path './.git' -prune -o -name '*' \
	-exec rm -rf {} +; # Remove all files but the .git to enable resetting
git reset --hard	# Reset all to reestablish directory structure
