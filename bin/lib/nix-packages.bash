#!/usr/bin/env bash
# -*- mode: shell-script; coding: utf-8-unix; fill-column: 80 -*-
# Copyright © 2015 Paul Stadig.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

require log

nix_env="${HOME}/.nix-profile/bin/nix-env"

function ensure-nix() {
  if [ -x "${nix_env}" ]; then
    debug "=== Skipping installing nix-env"
  else
    log "+++ Installing nix-env"
    curl https://nixos.org/nix/install | sh
    . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
  fi
}

function nix-package-installed() {
  ensure-nix
  "${nix_env}" -q 2>/dev/null | grep "${1}" &>/dev/null
}

function install-nix-packages() {
  ensure-nix
  "${nix_env}" -i "${@}"
}

function ensure-nix-packages() {
  ensure-nix
  uninstalled=()
  for package in "${@}"; do
    if nix-package-installed "${package}"; then
      debug "=== Skipping installing package '${package}'"
    else
      uninstalled["${#uninstalled[@]}"]="${package}"
    fi
  done

  if [ "${#uninstalled[@]}" -gt 0 ]; then
    log "+++ Installing packages $(for p in "${uninstalled[@]}"; do echo -n "'""${p}""'"" "; done)"
    install-nix-packages "${uninstalled[@]}"
  fi
}

provide nix-packages
