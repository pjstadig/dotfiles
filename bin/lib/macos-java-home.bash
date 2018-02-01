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

java_home="/usr/libexec/java_home"

function use-java9() {
  if [ -x "${java_home}" ]; then
    export JAVA_HOME=$(${java_home} -v '9*')
    hash -r
  else
    debug "=== Java must be installed on Mac OSX at ${java_home} to be activated"
  fi
}

function use-java8() {
  if [ -x "${java_home}" ]; then
    export JAVA_HOME=$(${java_home} -v '1.8*')
    hash -r
  else
    debug "=== Java must be installed on Mac OSX at ${java_home} to be activated"
  fi
}

provide macos-java-home
