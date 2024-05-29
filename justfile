# SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
# SPDX-License-Identifier: CC0-1.0

alias compile := build
alias b := build
alias c := build
alias r := run
alias t := test

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

# Runs the program.
run *INPUT:
    scala-cli . -- {{INPUT}}

# Runs the tests of the project.
test:
    scala-cli test .

# Clean workspace
clean:
    scala-cli clean .




