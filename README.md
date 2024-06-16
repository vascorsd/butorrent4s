<!--
SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
SPDX-License-Identifier: CC0-1.0
-->

# License: AGPL-3.0-or-later

Respect your licenses folks!
In the future I may change this to a more permissible one if it really becomes useful.


## Description

The code currently is focused on handling *[BENCODING](https://wiki.theory.org/BitTorrentSpecification#Bencoding)*.

This is the underlying format for things related to BitTorrent protocol to work. 
Eventually I plan to develop on top of that and implement some more torrent related things.

The purpose for all of this is mostly as an *exercise* and to explore protocols and these lower level
details which I've always been interested in but never found the opportunity to explore.

Idea came from looking at the 'Build your own BitTorrent' exercise at CodeCrafters.io.

It's also an opportunity to try and focus on scala 3 only codebase and see how it goes.


### Status / TODO

- [x] Bencode Decoder / Parsing
  - [x] Decoder tests
- [x] Bencode Ast 
  - [x] Ast tests
- [ ] Bencode Encoder - wip

General status: It's possible to compile and run tests.  
It's also possible to launch the cli app to decode given values, and it should be working properly at this point.

In the future I want to explore the following topics:
- [ ] Experiment with alternative parsing using `cats-parse` library
- [ ] Implement proper streaming parsing using `fs2` 
- [ ] Eventually go higher and parse `.torrent` files
- [ ] Implement some codecs with `derive` for arbitrary types 


## Development

The project uses [scala-cli](https://scala-cli.virtuslab.org/), so that needs to be installed.
Also using [just](https://just.systems/) as command runner.

Cloning the repo and checking which commands are available:
```shell
> just
Available recipes:
    build          # Compiles / builds the code.
    b              # alias for `build`
    c              # alias for `build`
    compile        # alias for `build`
    clean          # Clean workspace
    console        # Open scala repl with the code of the project available
    format         # Formats the scala code.
    lint-copyright # Check if all files have copyright info. Uses REUSE tool.
    run *INPUT     # Runs the program.
    r *INPUT       # alias for `run`
    test           # Runs the tests of the project.
    t              # alias for `test`
```

Compiling:
```shell
> just build
```

Run:
```shell
> just run --help
scala-cli . -- --help

Usage:
    butorrent4s encode
    butorrent4s decode

Experiment with bitorrent protocol details

Options and flags:
    --version, -V
        Print version
    --help, -h
        Print help

Subcommands:
    encode
        Encode Bencoded data
    decode
        Decode Bencoded data
```

Tests:
```shell
just test
```

## Examples

```shell
> just r decode "d0:li0eee"
scala-cli . -- decode d0:li0eee

toString: bdict{ bstring"" -> blist[ bint:0 ] }
Decoded value: List((bstring"",blist[ bint:0 ]))
Remaining unparsed input: ByteVector(empty)
```

```shell
> echo -en "4:hey\xC0" | just r decode -
scala-cli . -- decode -

toString: bstring|4:0x686579c0|
Decoded value raw: ByteVector(4 bytes, 0x686579c0)
Decoded value utf8: None
Remaining unparsed input: ByteVector(empty)
```
