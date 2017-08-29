# kotct/dot [![Build Status](https://travis-ci.org/kotct/dot.svg?branch=master)](https://travis-ci.org/kotct/dot) [![Coverage Status](https://coveralls.io/repos/github/kotct/dot/badge.svg)](https://coveralls.io/github/kotct/dot)

Dot is a collective configuration system which contains configurations
for programs like Emacs and zsh, but also (potentially) Vim.

## Installation

You can use this command to install our configuration.  (Ruby must be
installed for this to work, so make sure you have done that.)

    $ \ruby -e "$(\curl -fsSL https://raw.githubusercontent.com/kotct/dot/master/scripts/install)"

This script will interactively ask you if it doesn't know what to do.
It will immediately start installing the configuration, though, so be
sure that is what you want to do before running it.

This script assumes a fairly regular system with the requisite
software installed, so if you are in an esoteric environment, you can
plan on manually installing this by symlinking into `~`.  If something
breaks down on a normal system, (like Ubuntu, Fedora) please open an
Issue and we will get back to you ASAP.

If your system is running an older version of a dependency software,
(i.e. Emacs < 25.1, Ruby < 2.4) please do your best to update to the
latest version directly available from your package manager.  If an
issue is not reproducible on our system, we cannot fix it.

It is worth noting that you should be able to simply simlink the
respective files and directories into your `$HOME` directory.

You can do this by cloning this repository, then running:

    $ bundle install
    $ rake install

This is effectively what the above script does, except it allows you
to clone to your own desired location.

## License

`dot` is released and distributed under the terms of the MIT license.
See [`LICENSE`](https://github.com/kotct/dot/blob/master/LICENSE) for
more information.
