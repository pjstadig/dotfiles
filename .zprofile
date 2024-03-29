if [ -e "${HOME}/.nix-profile/etc/profile.d/nix.sh" ]; then
  . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
fi

# setup clubhouse
export CLUBHOUSE_HOME=~/src
export PATH="${CLUBHOUSE_HOME}/backend/bin:${CLUBHOUSE_HOME}/backend/infrastructure/build-support:${PATH}"

export EDITOR="emacs -nw"

export GEM_HOME="${HOME}/.gem"
export BUNDLE_PATH="${HOME}/.gem"

# setup nvm
export NVM_DIR="${HOME}/.nvm"
if [ -s "${NVM_DIR}/nvm.sh" ]; then
  . "${NVM_DIR}/nvm.sh"
fi

# setup nvm bash completion
if [ -s "${NVM_DIR}/bash_completion" ]; then
  . "${NVM_DIR}/bash_completion"
fi

export JAVA_HOME=/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home
export GRAALVM_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.1/Contents/Home

export AWS_VAULT_BACKEND=keychain
export AWS_VAULT_KEYCHAIN_NAME=login

# setup yarn
export YARN_HOME="${HOME}/.yarn"
export PATH="${PATH}:${YARN_HOME}/bin"
export PATH="${PATH}:${HOME}/.config/yarn/global/node_modules/.bin"

# setup fc4
export FC4_HOME="${HOME}/fc4"
export PATH="${PATH}:${FC4_HOME}"

# include GraalVM tools at the *end* of your path to give you the 'gu' command
export PATH="${PATH}:${GRAALVM_HOME}/bin"

# set PATH to include user's private bin
export PATH="${HOME}/bin:${HOME}/.local/bin:${PATH}"

# set FPATH
if type brew &>/dev/null; then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

export FPATH="${HOME}/.local/zfunctions:${FPATH}"

# configure spaceship prompt
export SPACESHIP_EXIT_CODE_SHOW=true

# Enable 256 color capabilities for appropriate terminals

# Set this variable in your local shell config if you want remote
# xterms connecting to this system, to be sent 256 colors.
# This can be done in /etc/csh.cshrc, or in an earlier profile.d script.
#   SEND_256_COLORS_TO_REMOTE=1

# Terminals with any of the following set, support 256 colors (and are local)
local256="$COLORTERM$XTERM_VERSION$ROXTERM_ID$KONSOLE_DBUS_SESSION"

if [ -n "$local256" ] || [ -n "$SEND_256_COLORS_TO_REMOTE" ]; then

  case "$TERM" in
    'xterm') TERM=xterm-256color;;
    'screen') TERM=screen-256color;;
    'Eterm') TERM=Eterm-256color;;
  esac
  export TERM

  if [ -n "$TERMCAP" ] && [ "$TERM" = "screen-256color" ]; then
    TERMCAP=$(echo "$TERMCAP" | sed -e 's/Co#8/Co#256/g')
    export TERMCAP
  fi
fi

unset local256
export ASPELL_CONF="conf .aspell.$(hostname -s).conf; conf-dir ${HOME}"

# the below causes all kinds of issues with programs that, for example, find the incorrect
# version of glibc

# for programs that require some libraries (caesium for clojure, or appimage executables)
# I can set LD_LIBRARY_PATH for only those commands

# LD_LIBRARY_PATH="${HOME}/.nix-profile/lib${LD_LIBRARY_PATH+:${LD_LIBRARY_PATH}}"
# export LD_LIBRARY_PATH
