if [ -e "${HOME}/.nix-profile/etc/profile.d/nix.sh" ]; then
  . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
fi

# setup clubhouse
export CLUBHOUSE_HOME=~/src
export PATH="${CLUBHOUSE_HOME}/backend/bin:${CLUBHOUSE_HOME}/backend/infrastructure/build-support:${PATH}"
export _USE_CLUBHOUSE_VPN=1

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
export PATH="${HOME}/bin:${PATH}"
