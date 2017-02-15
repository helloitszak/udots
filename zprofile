#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Programming
#
export CODE=$HOME/code
export GOPATH=$CODE/go

#
# Paths
#

typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

fpath=(
  $HOME/.udots/zshfunc
  $fpath
)

# Set the list of directories that Zsh searches for programs.
# /usr/bin is in here for cygwin compatibility because fml
path=(
  $HOME/bin
  $HOME/.udots/udbin
  $HOME/.cargo/bin
  $GOPATH/bin
  $HOME/perl5/bin
  /usr/local/opt/go/libexec/bin
  /usr/local/{bin,sbin}
  /usr/bin
  $path
)


#
# Homebrew
# Default homebrew to /Applications over ~/Applications
#
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
export TWITCHER_PROGRAM="livestreamer"

#
# Perl
#
export PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
export PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}";
export PERL_MB_OPT="--install_base \"${HOME}/perl5\"";
export PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5";


#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

#
# Temporary Files
#

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi
