# git-delorean

A terminal UI that lets you walk a single file’s entire Git history with syntax highlighting, smooth scrolling, and one-key PR lookup. Inspired by Emacs git-timemachine.

## Features

- Browse every revision of one file, noise-free views (no inline diffs).
- Jump to newer/older commits instantly; header shows author, subject, date, and short hash.
- Open the associated pull request in your browser via `gh pr view` when present.

## Usage

```bash
git-delorean path/to/file.ext
```

The path must be inside a Git repository. The viewer renders the file for each commit and lets you navigate chronologically.
