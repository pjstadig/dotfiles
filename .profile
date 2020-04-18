# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
    PATH="${HOME}/bin:${PATH}"
fi

# set EDITOR
export EDITOR="emacs -nw"

if [ -e "${HOME}/.nix-profile/etc/profile.d/nix.sh" ]; then
    # shellcheck source=.nix-profile/etc/profile.d/nix.sh
    . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
fi

export GEM_HOME="${HOME}/.gem"
export BUNDLE_PATH="${HOME}/.gem"
export _USE_CLUBHOUSE_VPN=1
export CLUBHOUSE_HOME=~/src
if [ -d "${CLUBHOUSE_HOME}/backend/bin" ] ; then
    PATH="${PATH}:${CLUBHOUSE_HOME}/backend/bin"
fi

## Load nvm
export NVM_DIR="${HOME}/.nvm"
# shellcheck source=/dev/null
[ -s "${NVM_DIR}/nvm.sh" ] && . "${NVM_DIR}/nvm.sh"  # This loads nvm
#[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm

## Load nvm bash completion
# shellcheck source=/dev/null
[ -s "${NVM_DIR}/bash_completion" ] && . "${NVM_DIR}/bash_completion"  # This loads nvm bash_completion
#[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

## Set JAVA_HOME to the Adopt OpenJDK
export JAVA_HOME=/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home

## Set GRAALVM_HOME
export GRAALVM_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.1/Contents/Home

## Include GraalVM tools at the *end* of your path to give you the 'gu' command
export PATH="${PATH}:${GRAALVM_HOME}/bin"

export AWS_VAULT_BACKEND=keychain
export AWS_VAULT_KEYCHAIN_NAME=login

# setup yarn path
export YARN_HOME="${HOME}/.yarn"
if [ -d "${YARN_HOME}/bin" ] ; then
    export PATH="${PATH}:${YARN_HOME}/bin"
fi

if [ -d "${HOME}/.config/yarn/global/node_modules/.bin" ] ; then
    export PATH="${PATH}:${HOME}/.config/yarn/global/node_modules/.bin"
fi

# Setup fc4 path
export FC4_HOME="${HOME}/fc4"
if [ -d "${FC4_HOME}" ] ; then
    export PATH="${PATH}:${FC4_HOME}"
fi

# if running bash
if [ -n "${BASH_VERSION:-}" ]; then
    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
        # shellcheck source=/dev/null
        . "${HOME}/.bashrc"
    fi
fi
