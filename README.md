# udots
Next generation dotfiles.

This project contains all my dotfiles I use on a daily basis to make my life
easier. I use OS X first and Archlinux second, but it really should work
anywhere you can install the dependencies. If something doesn't work on your
platform submit a pull request. I'm always open for suggestions!

## what's in the box
* some wonderful shell scripts in `udbin`
* "it works for me" emacs and vim configs.
* zsh setup based on the wonderful [prezto](prezto)
* manages stuff using [rcm](rcm)

[prezto]: https://github.com/sorin-ionescu/prezto
[rcm]: https://github.com/thoughtbot/rcm

## requirements
* thoughtbot's [rcm](rcm)
* recent zsh
* recent emacs
* recent vim

## things you might want
* youtube-dl
* mpv
* livestreamer

## how do I get started
1. recursive checkout the sourcecode to `~/.udots` (I use submodules)
2. cd into `~/.udots`
3. run `bootstrap.sh`
4. change your shell to zsh if it's not zsh already.
5. start doing things

on first vim or emacs launch you'll need to install a ton of packages. I use
NeoBundle on vim and package.el on emacs

## updating
1. `git pull` in `~/.udots`
2. run `bootstrap.sh`
