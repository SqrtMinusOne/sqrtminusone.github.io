+++
title = "Console"
author = ["Pavel"]
draft = false
+++

\#+TOC headlines 6


## `.profile` {#dot-profile}


### Environment {#environment}

```sh
# export EDITOR=/usr/bin/vim
# export BROWSER=/usr/bin/firefox
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export TZ="Asia/Yekaterinburg"
# export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
```


### My paths {#my-paths}

My script folders

```sh
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi
```


### Guix settings {#guix-settings}

Enable extra profiles

```sh
if [ -z "$IS_ANDROID" ]; then
    GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
    for i in $GUIX_EXTRA_PROFILES/*; do
	profile=$i/$(basename "$i")
	if [ -f "$profile"/etc/profile ]; then
	    GUIX_PROFILE="$profile"
	    . "$GUIX_PROFILE"/etc/profile
	fi
	export XDG_DATA_DIRS="$XDG_DATA_DIRS:$profile/share"
	unset profile
    done
fi
```

Set Jupyter config PATH. It defaults to readonly directory somewhere in Guix profile.

```sh
export JUPYTER_CONFIG_DIR=$HOME/.config/jupyter
```

Set a folder for my packages.

```sh
export GUIX_PACKAGE_PATH=~/guix-packages
```


### Other package managers {#other-package-managers}

Using other package managers with Guix requires some extra work.

Cask

```sh
if [ -d "$HOME/.cask" ]; then
    export PATH="/home/pavel/.cask/bin:$PATH"
fi
```

Make flatpak apps visible to launchers:

```sh
if [ -d "$HOME/.local/share/flatpak" ]; then
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"
fi
```

Enable Nix

```sh
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi
```

Use Guix fontconfig. Necessary for nix apps

```sh
if [ -d "$HOME/.guix-extra-profiles/desktop" ]; then
    export FONTCONFIG_PATH="$HOME/.guix-extra-profiles/desktop/desktop/etc/fonts"
fi
```

Make nix apps visible to launchers:

```sh
if [ -d "$HOME/.nix-profile" ]; then
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.nix-profile/share/applications"
fi
```


#### npm {#npm}

npm is especially cumbersome, for instance because by default it tries to install packages to `/gnu/store/`.

In principle, one can set a prefix like this:

```conf
prefix=/home/pavel/.npm-packages
```

But I also want to use node from conda occasionally, where prefix is already set correctly. So instead of tangling the above to the `~/.npmrc` directly, I set an environment variable in the profile:

```sh
export NPM_CONFIG_USERCONFIG=$HOME/._npmrc
```

The variable is unset in a script in [Guix.org]({{< relref "Guix" >}}).

Set PATH & MANPATH

```sh
NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$PATH:$NPM_PACKAGES/bin"
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"
```


### XResources {#xresources}

| Guix dependency |
|-----------------|
| xrdb            |

```sh
if [ -z "$IS_ANDROID" ]; then
    xrdb ~/.Xresources
fi
```


### <span class="org-todo done OFF">OFF</span> (OFF) Package manager paths {#off--package-manager-paths}

Turned off for now, because probably it won't be necessary in Guix.

LaTeX

```sh
if [ -d "/usr/local/texlive/2020" ]; then
    export MANPATH="/usr/local/texlive/2020/texmf-dist/doc/man:$MANPATH"
    export INFOPATH="/usr/local/texlive/2020/texmf-dist/doc/info:$INFOPATH"
    export PATH="/usr/local/texlive/2020/bin/x86_64-linux:$PATH"
fi
```

Cargo (Rust)

```sh
if [ -d "$HOME/.cargo" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
```

RVM (Ruby)

```sh
if [ -d "$HOME/.rvm" ] ; then
    export PATH="$PATH:$HOME/.rvm/bin"
fi
# if [ -d "$HOME/.gem" ]; then
#     export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"
# fi
```

Go

```sh
if [ -d "$HOME/go" ] ; then
    export PATH="$HOME/go/bin:$PATH"
fi
```

ghcup (Haskell)

```sh
[ -f "/home/pavel/.ghcup/env" ] && source "/home/pavel/.ghcup/env" # ghcup-env
```

Perl

```sh
if [ -d "$HOME/perl5" ] ; then
    PATH="/home/pavel/perl5/bin${PATH:+:${PATH}}"
    PERL5LIB="/home/pavel/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
    PERL_LOCAL_LIB_ROOT="/home/pavel/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
    PERL_MB_OPT="--install_base \"/home/pavel/perl5\""; export PERL_MB_OPT;
    PERL_MM_OPT="INSTALL_BASE=/home/pavel/perl5"; export PERL_MM_OPT;
fi
```


## Bash {#bash}


### `.bash_profile` {#dot-bash-profile}

```bash
[[ -f ~/.profile ]] && . ~/.profile

[[ -f ~/.bashrc ]] && . ~/.bashrc
```


### `.bashrc` {#dot-bashrc}

My `.bashrc`, which has pieces from the default ones in Guix & Manjaro, as well some mine settings.


#### Startup & environment {#startup-and-environment}

Export 'SHELL' to child processes.  Programs such as 'screen' honor it and otherwise use /bin/sh.

```bash
export SHELL
```

We are being invoked from a non-interactive shell.  If this is an SSH session (as in "ssh host command"), source /etc/profile so we get PATH and other essential variables.

```bash
if [[ $- != *i* ]]
then
    [[ -n "$SSH_CLIENT" && -f "/etc/bashrc" ]] && source /etc/profile
    return
fi
```

Source the system-wide file

```bash
if [[ -f "/etc/bashrc" ]]; then
    source /etc/bashrc
fi
```

| Guix dependency |
|-----------------|
| xhost           |

Allow other users to access X server. Necessary for stuff like aw-watcher-window.

```bash
xhost +local:root > /dev/null 2>&1
```

Set manpager to bat

```bash
export MANPAGER="sh -c 'sed -e s/.\\\\x08//g | bat -l man -p'"
```


#### Launch fish {#launch-fish}

Launch fish shell unless bash itself is launched from fish.

```bash
use_fish=true

if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" && ${use_fish} && $(command -v fish) ]]
then
    exec fish
fi
```

The rest of `.bashrc` is not executed if fish was launched.


#### Colors {#colors}

Setting for colors, packed in the default `.bashrc` in Manjaro

```bash
use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
    && type -P dircolors >/dev/null \
    && match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
    # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
    if type -P dircolors >/dev/null ; then
	if [[ -f ~/.dir_colors ]] ; then
	    eval $(dircolors -b ~/.dir_colors)
	elif [[ -f /etc/DIR_COLORS ]] ; then
	    eval $(dircolors -b /etc/DIR_COLORS)
	fi
    fi

    if [[ ${EUID} == 0 ]] ; then
	PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
    else
	PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
    fi

    alias ls='ls --color=auto'
    alias grep='grep --colour=auto'
    alias egrep='egrep --colour=auto'
    alias fgrep='fgrep --colour=auto'
else
    if [[ ${EUID} == 0 ]] ; then
	# show root@ when we don't have colors
	PS1='\u@\h \W \$ '
    else
	PS1='\u@\h \w \$ '
    fi
fi

unset use_color safe_term match_lhs sh
```


#### Settings {#settings}

Some general bash settings.

References:

-   [shopt list](https://www.gnu.org/software/bash/manual/html%5Fnode/The-Shopt-Builtin.html)

<!--listend-->

```bash
complete -cf sudo           # Sudo autocompletion

shopt -s checkwinsize       # Check windows size after each command
shopt -s expand_aliases     # Aliases
shopt -s autocd             # Cd to directory just by typing its name (without cd)
```

History control

```bash
shopt -s histappend
export HISTCONTROL=ignoredups:erasedups
HISTSIZE=
HISTFILESIZE=
```

Autocompletions

```bash
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion
if [ -d "/usr/share/fzf" ]; then
    source /usr/share/fzf/completion.bash
    source /usr/share/fzf/key-bindings.bash
fi
```


#### Aliases {#aliases}

```bash
alias v="vim"
alias gg="lazygit"
alias ls="exa --icons"
alias ll="exa -lah --icons"
alias q="exit"
alias c="clear"
alias ci="init_conda"
alias ca="conda activate"
alias cii="export INIT_CONDA=true && init_conda"
```

```bash
if [[ ! -z "$SIMPLE" ]]; then
    unalias ls
    alias ll="ls -lah"
fi
```


#### Anaconda {#anaconda}

> managed by 'conda init' !!!

Yeah, tell this to yourself

```bash
init_conda () {
    __conda_setup="$('/home/pavel/.guix-extra-profiles/dev/dev/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
	eval "$__conda_setup"
    else
	if [ -f "/home/pavel/.guix-extra-profiles/dev/dev/etc/profile.d/conda.sh" ]; then
	    . "/home/pavel/.guix-extra-profiles/dev/dev/etc/profile.d/conda.sh"
	else
	    # export PATH="/home/pavel/Programs/miniconda3/bin:$PATH"
	    echo "what"
	fi
    fi
    unset __conda_setup
}

if [[ ! -z "$INIT_CONDA" ]]; then
    init_conda
fi
```


#### Starship prompt {#starship-prompt}

```bash
if [[ -z "$SIMPLE" ]]; then
    eval "$(starship init bash)"
fi
```


## Fish {#fish}

| Guix dependency | Description                              |
|-----------------|------------------------------------------|
| fish            | An alternative non POSIX-compliant shell |

[Fish shell](https://fishshell.com/) is a non-POSIX-compliant shell, which offers some fancy UI & UX features.

Launch starship

```fish
starship init fish | source
```

Enable vi keybindings & aliases. The alias syntax is the same as in bash, so it's just a noweb reference to `.bashrc`.

```fish
fish_vi_key_bindings

<<shell-aliases>>
```

| Guix dependency |
|-----------------|
| dt-colorscripts |

Launch a random [DT's colorscript](https://gitlab.com/dwt1/shell-color-scripts) unless ran inside tmux or Emacs.

```fish
if ! test -n "$TMUX"; and ! test -n "$IS_EMACS";
    colorscript random
end
```

Suppress fish greeting

```fish
set fish_greeting
```


### Anaconda {#anaconda}

First, a function to initialize anaconda.

```fish
function init_conda
    eval /home/pavel/.guix-extra-profiles/dev/dev/bin/conda "shell.fish" "hook" $argv | source
end

if test -n "$INIT_CONDA";
    init_conda
end
```

Then, check if launched from Emacs with environment activated.

```fish
if test -n "$EMACS_CONDA_ENV";
    conda activate $EMACS_CONDA_ENV
end
```


### Colors {#colors}

Fish seems to have hardcoded colorcodes in some color settings. I set these to base16 colors so they would match Xresources.

```fish
set fish_color_command cyan
set fish_color_comment green
set fish_color_end white
set fish_color_error red
set fish_color_escape yellow
set fish_color_operator yellow
set fish_color_param magenta
set fish_color_quote brwhite
set fish_color_redirection yellow
```


### Keybindings {#keybindings}

```fish
bind -M insert \el forward-char
bind -M insert \eh backward-char
bind -M insert \ew forward-word
bind -M insert \eb backward-word
```


## Nushell {#nushell}

| Guix dependency |
|-----------------|
| nushell-bin     |

A structured shell. I don't use it as of now, but perhaps one day.

Set starship prompt

```toml
startup = [
    <<nu-aliases>>,
    "mkdir ~/.cache/starship",
    "starship init nu | save ~/.cache/starship/init.nu",
    "source ~/.cache/starship/init.nu",
]
prompt = "starship_prompt"
```

Skip welcome message

```toml
skip_welcome_message = true
```

Set table mode

```toml
table_mode = "rounded"
```

Aliases

```toml
"alias ll = ls -l",
"alias c = clear",
"alias q = exit"
```

Colors

```toml
[color_config]
primitive_filesize="ub"
primitive_boolean="yu"
primitive_duration="g"
primitive_path="y"
primitive_date="r"
primitive_int="c"
primitive_decimal="c"
```


## Starship prompt {#starship-prompt}

| Guix dependency | Description         |
|-----------------|---------------------|
| rust-starship   | my prompt of choice |

[Starship](https://starship.rs/) is a nice cross-shell prompt, written in Rust.

References:

-   [Startship config guide](https://starship.rs/config/)

<!--listend-->

```toml
[character]
success_symbol = "[➤ ](bold green)"
error_symbol = "[ ](bold red)"
vicmd_symbol = "[ᐊ ](bold green)"

[aws]
symbol = " "

# [battery]
# full_symbol = ""
# charging_symbol = ""
# discharging_symbol = ""

[conda]
symbol = " "

[cmd_duration]
min_time = 500
format = " [$duration]($style) "

[docker_context]
symbol = " "

[elixir]
symbol = " "

[elm]
symbol = " "

[git_branch]
symbol = " "
truncation_length = 20

[golang]
symbol = " "

# [haskell]
# symbol = " "

[hg_branch]
symbol = " "

[java]
symbol = " "

[julia]
symbol = " "

[memory_usage]
symbol = " "

[nim]
symbol = " "

[nix_shell]
symbol = " "

[nodejs]
symbol = " "

[package]
symbol = " "
disabled = true

[php]
symbol = " "

[python]
symbol = " "

[ruby]
symbol = " "

[rust]
symbol = " "
```


## Tmux {#tmux}

| Guix dependency |
|-----------------|
| tmux            |
| python-tmuxp    |

[tmux](https://github.com/tmux/tmux) is my terminal multiplexer of choice.

It provides pretty sane defaults, so the config is not too large. I rebind the prefix to `C-a` though.


### Term settings {#term-settings}

I have no idea how and why these two work.

```vim
set -g default-terminal "screen-256color"
set -ga terminal-overrides ",*256col*:Tc"
```

History limit.

```vim
set -g history-limit 20000
```


### Keybindings {#keybindings}

Enable vi keys and mouse.

```vim
set-window-option -g mode-keys vi
set-option -g xterm-keys on
set-option -g mouse on
set -sg escape-time 10
```

Change prefix from `C-b` to `C-a`.

```vim
unbind C-b
set -g prefix C-a
bind C-a send-prefix
```

Vi-like keybindings to manage panes & windows.

```vim
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

bind s split-window
bind v split-window -h

bind-key n new-window
bind-key t next-window
bind-key T previous-window
```

Reload the config.

```vim
bind r source-file ~/.tmux.conf
```


### Copy to clipboard {#copy-to-clipboard}

| Guix dependency |
|-----------------|
| xclip           |

Make tmux copying copy to clipboard as well

```vim
bind-key -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "xclip -selection clipboard -i"
bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "xclip -selection clipboard -i"
```


### UI {#ui}

I generated the following with [tmuxline.vim](https://github.com/edkolev/tmuxline.vim) plugin and palenight theme for [vim-airline](https://github.com/vim-airline/vim-airline)

```vim
# This tmux statusbar config was created by tmuxline.vim
# on Wed, 22 Jan 2020

set -g status-justify "centre"
set -g status "on"
set -g status-left-style "none"
set -g message-command-style "fg=#bfc7d5,bg=#474b59"
set -g status-right-style "none"
set -g pane-active-border-style "fg=#939ede"
set -g status-style "none,bg=#333747"
set -g message-style "fg=#bfc7d5,bg=#474b59"
set -g pane-border-style "fg=#474b59"
set -g status-right-length "100"
set -g status-left-length "100"
setw -g window-status-activity-style "none,fg=#939ede,bg=#333747"
setw -g window-status-separator ""
setw -g window-status-style "none,fg=#bfc7d5,bg=#333747"
set -g status-left "#[fg=#292D3E,bg=#939ede] #S #[fg=#939ede,bg=#474b59,nobold,nounderscore,noitalics]#[fg=#bfc7d5,bg=#474b59] #W #[fg=#474b59,bg=#333747,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=#333747,bg=#333747,nobold,nounderscore,noitalics]#[fg=#bfc7d5,bg=#333747] %-H:%M #[fg=#474b59,bg=#333747,nobold,nounderscore,noitalics]#[fg=#bfc7d5,bg=#474b59] %a, %b %d #[fg=#939ede,bg=#474b59,nobold,nounderscore,noitalics]#[fg=#292D3E,bg=#939ede] #H "
setw -g window-status-format "#[fg=#333747,bg=#333747,nobold,nounderscore,noitalics]#[default] #I #W #[align=left] #[fg=#333747,bg=#333747,nobold,nounderscore,noitalics]"
setw -g window-status-current-format "#[fg=#333747,bg=#474b59,nobold,nounderscore,noitalics]#[fg=#bfc7d5,bg=#474b59] #I #W #[fg=#474b59,bg=#333747,nobold,nounderscore,noitalics]"
```

Source the line config:

```vim
source ~/.tmux.line.conf
```


## Alacritty {#alacritty}

| Guix dependency |
|-----------------|
| alacritty       |

[Alacritty](https://github.com/alacritty/alacritty) is a GPU-accelerated terminal emulator. I haven't found it to be an inch faster than st, but configuration the in yml format is way more convinient than patches.

Once again, we have an application which doesn't support reading Xresources, so here goes noweb.

<a id="code-snippet--get-xrdb"></a>
```bash
xrdb -query all | grep "$color:" | cut -f 2
```

```emacs-lisp
(setq-local org-confirm-babel-evaluate nil)
```

References:

-   [default config](https://github.com/alacritty/alacritty/blob/master/alacritty.yml)

<!--listend-->

```yaml
decorations: none

font:
  normal:
    family: JetBrainsMono Nerd Font
    style: Regular

  size: 10

env:
  TERM: xterm-256color

colors:
  primary:
    background: '<<get-xrdb(color="color0")>>'
    foreground: '<<get-xrdb(color="color7")>>'
  normal:
    black: '<<get-xrdb(color="color0")>>'
    red: '<<get-xrdb(color="color1")>>'
    green: '<<get-xrdb(color="color2")>>'
    yellow: '<<get-xrdb(color="color3")>>'
    blue: '<<get-xrdb(color="color4")>>'
    magenta: '<<get-xrdb(color="color5")>>'
    cyan: '<<get-xrdb(color="color6")>>'
    white: '<<get-xrdb(color="color7")>>'
  bright:
    Black: '<<get-xrdb(color="color8")>>'
    Red: '<<get-xrdb(color="color9")>>'
    Green: '<<get-xrdb(color="color10")>>'
    Yellow: '<<get-xrdb(color="color11")>>'
    Blue: '<<get-xrdb(color="color12")>>'
    Magenta: '<<get-xrdb(color="color13")>>'
    Cyan: '<<get-xrdb(color="color14")>>'
    White: '<<get-xrdb(color="color15")>>'

background_opacity: 0.80

window:
  padding:
    x: 0
    y: 0
  dynamic_padding: true

key_bindings:
  - { key: Paste,                                       action: Paste          }
  - { key: Copy,                                        action: Copy           }
  - { key: L,         mods: Control,                    action: ClearLogNotice }
  - { key: L,         mods: Control, mode: ~Vi|~Search, chars: "\x0c"          }
  - { key: PageUp,    mods: Shift,   mode: ~Alt,        action: ScrollPageUp,  }
  - { key: PageDown,  mods: Shift,   mode: ~Alt,        action: ScrollPageDown }
  - { key: Home,      mods: Shift,   mode: ~Alt,        action: ScrollToTop,   }
  - { key: End,       mods: Shift,   mode: ~Alt,        action: ScrollToBottom }

  #  Turn off vi mode
  - { key: Space,  mods: Shift|Control, mode: ~Search,    action: ReceiveChar             }

  # (Windows, Linux, and BSD only)
  - { key: V,              mods: Control|Shift, mode: ~Vi,        action: Paste            }
  - { key: C,              mods: Control|Shift,                   action: Copy             }
  - { key: F,              mods: Control|Shift, mode: ~Search,    action: ReceiveChar    }
  - { key: B,              mods: Control|Shift, mode: ~Search,    action: ReceiveChar   }
  - { key: Insert,         mods: Shift,                           action: PasteSelection   }
  - { key: Key0,           mods: Control,                         action: ResetFontSize    }
  - { key: Equals,         mods: Control,                         action: IncreaseFontSize }
  - { key: Plus,           mods: Control,                         action: IncreaseFontSize }
  - { key: NumpadAdd,      mods: Control,                         action: IncreaseFontSize }
  - { key: Minus,          mods: Control,                         action: DecreaseFontSize }
  - { key: NumpadSubtract, mods: Control,                         action: DecreaseFontSize }
```


## Various console applications {#various-console-applications}

| Guix dependency | Description                                 |
|-----------------|---------------------------------------------|
| ncurses         | Provides stuff like `clear`                 |
| exa             | `ls` replacement, written in Rust           |
| bat             | `cat` clone with syntax highlighting        |
| htop            | Interactive process viewer                  |
| nethogs         | A tool to group processed by used bandwidth |
| osync           | rsync wrapper                               |
| neofetch        | Fetch system info                           |
| fzf             | fuzzy finder                                |
| p7zip           | archiver                                    |
| password-store  | CLI password manager                        |
| unzip           |                                             |
| jmtpfs          | A tool to mount MTP devices (e.g. Android)  |
| tokei           | Count lines of code                         |


## Misc scripts {#misc-scripts}


### `nt` - exec command with a finished notification {#nt-exec-command-with-a-finished-notification}

Usage:

```text
nt <command>
```

```sh
command="$@"
if [ ! -z "$command" ]; then
    start_time="$(date -u +%s)"
    $command
    end_time="$(date -u +%s)"
    elapsed="$(($end_time-$start_time))"
    notify-send "Terminal" "Command\n$command\nexecuted in $elapsed seconds"
else
    notify-send "Terminal" "Command execution complete"
fi
```


### `autocommmit` {#autocommmit}

A script to autocommit files in a repository. I use it to sync my org directory and password store. I guess it's not how git is intended to be used, but it works for me.

Usage:

```text
autocommit <repository> [-F]
```

Environment:

| Variable      | Description     | Default value |
|---------------|-----------------|---------------|
| `TIMEOUT_MIN` | Default timeout | 60            |

Here's more or less what the script is doing:

-   If there is a merge conflict, notify
-   If there are changed files in the last `TIMEOUT_MIN` minutes, commit
-   Fetch
-   If there are were changes in the last `TTMEOUT_MIN`, merge (usually the merge has to be fast-forward)
-   If fetch was successful & merge was successful or delayed because of changes in the last `TIMEOUT_MIN`, push
-   Send a notification about the events above
-   Send a separate notification if there is a merge conflict

<!--listend-->

```bash
TIMEOUT_MIN=${TIMEOUT_MIN:-60}

export DISPLAY=:0
cd "$1"

TIMESTAMP=$(date +%s)
LAST_COMMIT_TIMESTAMP=$(git log -1 --format="%at" | xargs -I{} date -d @{} +%s)
RECENTLY_CHANGED_NUM=$(find . -not -path '*/\.*' -mmin -$TIMEOUT_MIN | wc -l)
CHANGED_NUM=$(git status --porcelain | wc -l)
COMMITED="No"
PUSHED="No"
FETCHED="No"
MERGED="No"

if [[ $(git ls-files -u | wc -l) -gt 0 ]]; then
    notify-send -u critical "Autocommit $(pwd)" "Merge conflict!"
    exit
fi

if [[ ($RECENTLY_CHANGED_NUM -eq 0 || $2 = "-F") && $CHANGED_NUM -gt 0 ]]; then
    read -r -d '' MESSAGE << EOM
Autocommit $(date -Iminutes)

Hostname: $(hostname)
EOM
    git add -A
    git commit -m "$MESSAGE"
    COMMITED="Yes"
fi

NEED_TO_PUSH=$(git log origin/master..HEAD | wc -l)

git fetch && FETCHED="Yes" || FETCHED="No"
if [[ $RECENTLY_CHANGED_NUM -gt 0 && $2 != '-F' ]]; then
    MERGED="Waiting"
fi

if [[ ($RECENTLY_CHANGED_NUM -eq 0 || $2 = "-F") && $FETCHED = "Yes" ]]; then
    MERGE_OUT=$(git merge origin/master) && MERGED="Yes" || MERGED="No"
fi

if [[ $NEED_TO_PUSH -gt 0 && ($MERGED = "Yes" || $MERGED = "Waiting") ]]; then
    git push origin && PUSHED="Yes" || PUSHED="No"
fi

if [[ $PUSHED = "Yes" || $COMMITED = "Yes" || ($MERGED = "Yes" &&  $MERGE_OUT != "Already up to date.")]]; then
    read -r -d '' NOTIFICATION << EOM
Commited: $COMMITED
Fetched: $FETCHED
Merged: $MERGED
Pushed: $PUSHED
EOM
    notify-send "Autocommit $(pwd)" "$NOTIFICATION"
fi

if [[ $(git ls-files -u | wc -l) -gt 0 ]]; then
    notify-send -u critical "Autocommit $(pwd)" "Merge conflict!"
fi
```

`mcron` job:

```scheme
(job "0 * * * *" "autocommit ~/Documents/org-mode")
(job "0,15,30,45 * * * *" "autocommit ~/.password-store")
```


## Guix settings {#guix-settings}

<a id="code-snippet--packages"></a>
```emacs-lisp
(my/format-guix-dependencies)
```

```scheme
(specifications->manifest
 '(
   <<packages()>>))
```


## Android notes {#android-notes}

SSH instructions: <https://wiki.termux.com/wiki/Remote%5FAccess>

Don't forget to install the following termux packages:

| Termux package |
|----------------|
| vim            |
| tmux           |
| starship       |
| fish           |
| exa            |
| bat            |
| git            |

Also:

-   cleanup `$PREFIX/etc/motd` to remove hello message.
-   copy the required font at `$HOME/.termux/font.ttf` and run `termux-reload-settings`.


### Installation of [DT's colorscripts](https://gitlab.com/dwt1/shell-color-scripts): {#installation-of-dt-s-colorscripts}

```bash
git clone https://gitlab.com/dwt1/shell-color-scripts.git
cd shell-color-scripts
```

Apply a patch:

```diff
--- a/colorscript.sh
+++ b/colorscript.sh
@@ -2,7 +2,7 @@

 # Simple CLI for shell-color-scripts

-DIR_COLORSCRIPTS="/opt/shell-color-scripts/colorscripts"
+DIR_COLORSCRIPTS="$PREFIX/opt/shell-color-scripts/colorscripts"
 LS_CMD="$(command -v ls)"
 fmt_help="  %-20s\t%-54s\n"
 list_colorscripts="$($LS_CMD "${DIR_COLORSCRIPTS}" | cut -d ' ' -f 1 | nl)"
```

```bash
sudo mkdir -p $PREFIX/opt/shell-color-scripts/colorscripts || return 1
sudo cp -rf colorscripts/* $PREFIX/opt/shell-color-scripts/colorscripts
sudo cp colorscript.sh $PREFIX/bin/colorscript
```