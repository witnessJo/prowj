# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Prowj is an Emacs project under development designed to provide users with easy control over project commands. The project aims to integrate commands with Projectile or Project (built-in Emacs project functionality) and offers frame and buffer options to execute project commands.

## Current Status

This is a very early-stage project that currently contains:
- Basic project structure with .projectile configuration
- README.org with project description
- No source code files yet

## Development Environment

This is an Emacs Lisp project that will likely follow standard Emacs package conventions:
- Source files will typically be `.el` files
- Package definition will likely be in a main `.el` file with package headers
- Integration with Projectile and Emacs built-in project.el

## Expected Development Commands

Since this is an early-stage Emacs project, standard Emacs development commands will apply once source code is added:
- Byte-compile: `emacs -batch -f batch-byte-compile *.el`
- Package linting: Use `package-lint` when available
- Testing: Likely to use `buttercup` or `ert` testing frameworks

## Project Architecture

The project will integrate with:
- Projectile (external project management package)
- Project.el (built-in Emacs 28+ project functionality)
- Frame and buffer management for command execution

## Notes for Future Development

- This project is explicitly marked as "under development and not yet released"
- When adding source code, follow standard Emacs Lisp package conventions
- Consider adding proper package headers and dependencies when creating the main package file