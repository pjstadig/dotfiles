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

function package-installed() {
  dpkg-query -W --showformat='${Status}\n' "${1}" 2>/dev/null | grep "install ok installed" &>/dev/null
}

function install-packages() {
  sudo apt-get -y install "${@}"
}

function ensure-packages() {
  uninstalled=()
  for package in "${@}"; do
    if package-installed "${package}"; then
      debug "=== Skipping installing package '${package}'"
    else
      uninstalled["${#uninstalled[@]}"]="${package}"
    fi
  done

  if [ "${#uninstalled[@]}" -gt 0 ]; then
    log "+++ Installing packages $(for p in "${uninstalled[@]}"; do echo -n "'""${p}""'"" "; done)"
    install-packages "${uninstalled[@]}"
  fi
}

provide packages
