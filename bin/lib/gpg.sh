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

ensure_gpg_agent () {
  gpg_agent="/usr/bin/gpg-agent"
  if [ -x ${gpg_agent} ]; then
    gpg-connect-agent /bye 2>&1 1>>"/tmp/dotfiles-${USER}.log"
    export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
  fi
}
