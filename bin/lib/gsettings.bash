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

function get-setting() {
  gsettings get "${1}" "${2}"
}

function set-setting() {
  gsettings set "${1}" "${2}" "${3}"
}

function ensure-setting() {
  if [ "$(get-setting "${1}" "${2}")" = "${3}" ]; then
    log-verbose "=== Skipping setting '${1}/${2}' to '${3}'"
  else
    log "+++ Setting '${1}/${2}' to '${3}'"
    set-setting "${1}" "${2}" "${3}"
  fi
}

provide gsettings
