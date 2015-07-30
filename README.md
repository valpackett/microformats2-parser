# microformats2-parser [![Hackage](https://img.shields.io/hackage/v/microformats2-parser.svg?style=flat)](https://hackage.haskell.org/package/microformats2-parser) [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

[Microformats 2] parser for Haskell!

Originally created for [sweetroll] :-)

The types are located in a separate package called [microformats2-types].

[Microformats 2]: http://microformats.org/wiki/microformats2
[sweetroll]: https://codeberg.org/valpackett/sweetroll
[microformats2-types]: https://codeberg.org/valpackett/microformats2-types

## Development

Use [stack] to build.  
Use ghci to run tests quickly with `:test` (see the `.ghci` file).

```bash
$ stack build

$ stack test && rm tests.tix

$ stack ghci --ghc-options="-fno-hpc"
```

[stack]: https://github.com/commercialhaskell/stack

## Contributing

Please feel free to submit pull requests!
Bugfixes and simple non-breaking improvements will be accepted without any questions :-)

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/2/0/).

[The list of contributors is available on GitHub](https://codeberg.org/valpackett/microformats2-parser/graphs/contributors).

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
