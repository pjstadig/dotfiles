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

android_path="${HOME}/.local/opt/android-sdk-linux/tools"

if [ -d "${android_path}" ]; then
  export PATH="${PATH}:${android_path}"
else
  log "=== The Android SDK must be installed at #{android_path} to be activated"
fi

provide android-sdk-path
