pushd ~/.udots
if [ ! "$(ls -A ~/.udots/zprezto 2>/dev/null)" ]; then
  echo "Submodules not init'd for zprezto"
  echo "Doing a recursive checkout"
  git submodule update --init --recursive
fi

if [ ! "$(command -v rcup 2>/dev/null)" ]; then
  echo "rcm is not installed. please install for your platform"
  echo "and make sure it is in your PATH"
  echo "https://github.com/thoughtbot/rcm"
  exit 1
fi

RCRC=~/.udots/rcrc rcup -d ~/.udots -v
popd
