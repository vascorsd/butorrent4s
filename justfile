# SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
# SPDX-License-Identifier: CC0-1.0

@_default:
    just --list

# Check if all files have copyright info. Uses REUSE tool.
lint-copyright:
    reuse lint

# Formats the scala code.
format:
    scala-cli format .

# Compiles / builds the code.
build:
    scala-cli compile . 

alias b := build

alias compile := build
alias c := build
