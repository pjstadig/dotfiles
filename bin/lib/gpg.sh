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
    if [ -f "$HOME/.gpg-agent-info" ]; then
      . "$HOME/.gpg-agent-info"
    fi
    if ! ${gpg_agent} >/dev/null 2>&1; then
      ${gpg_agent} --daemon --enable-ssh-support --write-env-file "$HOME/.gpg-agent-info"
      . "$HOME/.gpg-agent-info"
    fi
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_ID
  fi
}
