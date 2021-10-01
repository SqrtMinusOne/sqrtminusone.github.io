+++
title = "Desktop"
author = ["Pavel"]
draft = false
+++

My general desktop environment configuration.

Parts prefixed with (OFF) are not used, but kept for historic purposes. For some reason GitHub's org renderer ignores TODO status, hence such a prefix. Round brackets instead of square ones to prevent GitHub's org renderer from screwing up.

<div class="ox-hugo-toc toc">
<div></div>

<div class="heading">Table of Contents</div>

- [Global customization](#global-customization)
    - [Colors](#colors)
    - [Xresources](#xresources)
        - [Colors in Xresources](#colors-in-xresources)
        - [Fonts](#fonts)
    - [Themes](#themes)
    - [Device-specific settings](#device-specific-settings)
- [i3wm](#i3wm)
    - [General settings](#general-settings)
    - [Managing windows](#managing-windows)
    - [Workspaces](#workspaces)
    - [Rules](#rules)
    - [Scratchpad](#scratchpad)
        - [Launch script](#launch-script)
        - [i3 config](#i3-config)
    - [Gaps & borders](#gaps-and-borders)
        - [Keybindings](#keybindings)
    - [Move & resize windows](#move-and-resize-windows)
    - [<span class="org-todo done OFF">OFF</span> (OFF) Intergration with dmenu](#off--intergration-with-dmenu)
    - [Integration with rofi](#integration-with-rofi)
    - [Launching apps & misc keybindings](#launching-apps-and-misc-keybindings)
        - [Apps](#apps)
        - [Media controls & brightness](#media-controls-and-brightness)
        - [Screenshots](#screenshots)
    - [Colors](#colors)
    - [<span class="org-todo done OFF">OFF</span> (OFF) i3blocks](#off--i3blocks)
    - [Keyboard Layout](#keyboard-layout)
    - [Autostart](#autostart)
- [Polybar](#polybar)
    - [Launching](#launching)
    - [General settings](#general-settings)
        - [Colors](#colors)
        - [Bar config](#bar-config)
    - [Modules](#modules)
        - [ipstack-vpn](#ipstack-vpn)
        - [weather](#weather)
        - [aw-afk](#aw-afk)
        - [sun](#sun)
        - [SEP](#sep)
        - [TSEP](#tsep)
        - [i3](#i3)
        - [xkeyboard](#xkeyboard)
        - [mpd](#mpd)
        - [pulseaudio](#pulseaudio)
        - [cpu](#cpu)
        - [ram-memory](#ram-memory)
        - [swap-memory](#swap-memory)
        - [network](#network)
        - [date](#date)
        - [battery](#battery)
- [Rofi](#rofi)
    - [Theme](#theme)
    - [Scripts](#scripts)
        - [Buku bookmarks](#buku-bookmarks)
        - [Man pages](#man-pages)
        - [pass](#pass)
- [Flameshot](#flameshot)
- [dunst](#dunst)
- [keynav](#keynav)
    - [Config](#config)
    - [Using with picom](#using-with-picom)
- [Picom](#picom)
    - [Shadows](#shadows)
    - [Fading](#fading)
    - [Opacity](#opacity)
    - [General settings](#general-settings)
- [Zathura](#zathura)
- [Various software](#various-software)
    - [Browsers](#browsers)
    - [Office](#office)
    - [LaTeX](#latex)
    - [Dev](#dev)
    - [Manifests](#manifests)
    - [Flatpak](#flatpak)
    - [Nix](#nix)
- [Services](#services)
    - [Music](#music)
    - [GNU Mcron](#gnu-mcron)
    - [ActivityWatch](#activitywatch)
    - [PulseEffects](#pulseeffects)
    - [xsettingsd](#xsettingsd)
    - [Discord rich presence](#discord-rich-presence)
    - [Polkit Authentication agent](#polkit-authentication-agent)
    - [Xmodmap](#xmodmap)
    - [VPN](#vpn)
    - [Davmail](#davmail)
    - [Shepherd config](#shepherd-config)
    - [Sync](#sync)
- [Guix settings](#guix-settings)

</div>
<!--endtoc-->


## Global customization {#global-customization}


### Colors {#colors}

Most of the colors are from the Palenight theme. Colorcodes are taken from [this repo](https://github.com/JonathanSpeek/palenight-iterm2):

<a id="table--colors"></a>

| color         | key     | value   |
|---------------|---------|---------|
| black         | color0  | #292d3e |
| red           | color1  | #f07178 |
| green         | color2  | #c3e88d |
| yellow        | color3  | #ffcb6b |
| blue          | color4  | #82aaff |
| magenta       | color5  | #c792ea |
| cyan          | color6  | #89ddff |
| white         | color7  | #d0d0d0 |
| light-black   | color8  | #434758 |
| light-red     | color9  | #ff8b92 |
| light-green   | color10 | #ddffa7 |
| light-yellow  | color11 | #ffe585 |
| light-blue    | color12 | #9cc4ff |
| light-magenta | color13 | #e1acff |
| light-cyan    | color14 | #a3f7ff |
| light-white   | color15 | #ffffff |

The table above is the only source of truth for colors in this config.

The first way to get colors of it is to use the following noweb:

<a id="code-snippet--get-color"></a>
```emacs-lisp
(let ((color (seq-some (lambda (e) (and (string= name (car e)) (nth 2 e))) table)))
  (if (> quote 0)
      (concat "\"" color "\"")
    color))
```

Also, run the following to disable configuration for noweb evaluations:

```emacs-lisp
(setq-local org-confirm-babel-evaluate nil)
```

Test:

```emacs-lisp
<<get-color(name="red", quote=1)>>
```


### Xresources {#xresources}


#### Colors in Xresources {#colors-in-xresources}

However, I'd rather use the `Xresources` file wherever possible. Here is the code to generate an Xresources file from this table:

<a id="code-snippet--get-xresources"></a>
```emacs-lisp
(apply
 #'concat
 (mapcar
  (lambda (elem)
    (concat "*" (nth 1 elem) ": " (nth 2 elem) "\n"))
  (seq-filter
   (lambda (elem) (nth 1 elem))
   table)))
```

```vim
<<get-xresources()>>

*background: <<get-color(name="black")>>
*foreground: <<get-color(name="white")>>
```


#### Fonts {#fonts}

Also, Xresources are used to set `Xft` settings. Unfortunately, the DPI setting has to be unique for each machine, which means I cannot commit `Xresources` to the repo.

<a id="code-snippet--get-dpi"></a>
```emacs-lisp
(let ((hostname (system-name)))
  (cond ((string-equal hostname "azure") 120)
	((string-equal hostname "eminence") 120)
	((string-equal hostname "indigo") 120)
	(t 96)))
```

```vim
Xft.dpi: <<get-dpi()>>
```


### Themes {#themes}

A few programs I use to customize the apperance are listed below.

| Guix dependency    | Description             |
|--------------------|-------------------------|
| matcha-theme       | My preferred GTK theme  |
| papirus-icon-theme | My preferred Icon theme |
| xsettingsd         | X11 settings daemon     |

[xsettingsd](https://github.com/derat/xsettingsd) is a lightweight daemon which configures X11 applications. It is launched with shepherd in the [Services](#services) section.

```vim
Net/ThemeName "Matcha-dark-azul"
Net/IconThemeName "Papirus-Dark"
Gtk/DecorationLayout "menu:minimize,maximize,close"
Gtk/FontName "Sans 10"
Gtk/MonospaceFontName "JetBrainsMono Nerd Mono 12"
Gtk/CursorThemeName "Adwaita"
Xft/Antialias 1
Xft/Hinting 0
Xft/HintStyle "hintnone"
```


### Device-specific settings {#device-specific-settings}

| Guix dependency | Description                                |
|-----------------|--------------------------------------------|
| xrandr          | X11 CLI to RandR                           |
| xgamma          | A tool to alter monitor's gamma correction |
| xinput          | Configure input devices                    |

Set screen layout & other params depending on hostname

```sh
hostname=$(hostname)
if [ "$hostname" = "indigo" ]; then
    xrandr --output DisplayPort-0 --off --output HDMI-A-0 --mode 1920x1080 --pos 0x0 --rotate normal --output DVI-D-0 --mode 1920x1080 --pos 1920x0 --rotate normal
elif [ "$hostname" = "eminence" ]; then
    xgamma -gamma 1.25
fi
```


## i3wm {#i3wm}

| Guix dependency | Disabled |
|-----------------|----------|
| i3-gaps         |          |
| i3lock          | true     |

`i3lock` is disabled because the global one has to be used.

[i3wm](https://i3wm.org/) is a manual tiling window manager, which is currently my window manager of choice. I've tried several alternatives, including [xmonad](https://xmonad.org/) & [EXWM](https://github.com/ch11ng/exwm), but i3 seems to fit my workflow best.

[i3-gaps](https://github.com/Airblader/i3) is an i3 fork with a few features like window gaps. I like to enable inner gaps when there is at least one container in a workspace.

References:

-   [i3wm docs](https://i3wm.org/docs/)
-   [i3-gaps wiki](https://github.com/Airblader/i3/wiki)


### General settings {#general-settings}

```vim
set $mod Mod4
font pango:monospace 10

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# Move cursor between monitors
mouse_warping output

# Apply XFCE Settings
# exec xfsettingsd
# exec xiccd

# Set screen layout
exec ~/bin/scripts/screen-layout

# Most needed keybindigs
# reload the configuration file
bindsym $mod+Shift+c reload

# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Shift+r restart

# exit i3 (logs you out of your X session)
bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"
```


### Managing windows {#managing-windows}

Some keybindings for managing windows.

Kill focused windows

```vim
bindsym $mod+Shift+q kill
```

Change focus

```vim
bindsym $mod+h focus left
bindsym $mod+j focus down
bindsym $mod+k focus up
bindsym $mod+l focus right

bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right
```

Move windows around

```vim
bindsym $mod+Shift+h move left
bindsym $mod+Shift+j move down
bindsym $mod+Shift+k move up
bindsym $mod+Shift+l move right

bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right
```

Split windows

```vim
bindsym $mod+s split h
bindsym $mod+v split v
```

Enter fullscreen mode

```vim
# enter fullscreen mode for the focused container
bindsym $mod+f fullscreen toggle
```

Changing layout

```vim
bindsym $mod+w layout stacking
bindsym $mod+t layout tabbed
bindsym $mod+e layout toggle split
```

Toggle tiling/floating, switch between tiled and floating windows

```vim
bindsym $mod+Shift+f floating toggle
bindsym $mod+z focus mode_toggle
```

Switching outputs

```vim
bindsym $mod+Tab move workspace to output right
bindsym $mod+q focus output right
```

Focus parent and child container

```vim
bindsym $mod+a focus parent
bindsym $mod+Shift+A focus child
```

Toggle sticky

```vim
bindsym $mod+i sticky toggle
```

Set windows as floating and sticky, move to the top right.

```vim
bindsym $mod+x floating enable; sticky enable; move position 1220 0; resize set width 700 px
```


### Workspaces {#workspaces}

```vim
set $w1 "1 🚀"
set $w2 "2 🌍"
set $w3 "3 💬"
set $w4 "4 🛠️️"
set $w7 "7 🛰️"
set $w8 "8 📝"
set $w9 "9 🎵"
set $w10 "10 📦"

bindsym $mod+1 workspace $w1
bindsym $mod+2 workspace $w2
bindsym $mod+3 workspace $w3
bindsym $mod+4 workspace $w4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace $w7
bindsym $mod+8 workspace $w8
bindsym $mod+9 workspace $w9
bindsym $mod+0 workspace $w10

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace $w1
bindsym $mod+Shift+2 move container to workspace $w2
bindsym $mod+Shift+3 move container to workspace $w3
bindsym $mod+Shift+4 move container to workspace $w4
bindsym $mod+Shift+5 move container to workspace 5
bindsym $mod+Shift+6 move container to workspace 6
bindsym $mod+Shift+7 move container to workspace $w7
bindsym $mod+Shift+8 move container to workspace $w8
bindsym $mod+Shift+9 move container to workspace $w9
bindsym $mod+Shift+0 move container to workspace $w10

# Cycle workspaces
bindsym $mod+comma workspace prev
bindsym $mod+period workspace next
```


### Rules {#rules}

Rules to automatically assign applications to workspaces and do other stuff, like enable floating.

Most apps can be distinguished by a WM class (you can get one with [xprop](https://www.x.org/releases/X11R7.5/doc/man/man1/xprop.1.html)), but in some cases it doesn't work, e.g. for terminal applications. In that case rules can be based on a window title, for instance.

However, watch out for the following: rule such as `for_window [title="ncmpcpp.*"] move to workspace $w9` will move **any** window with a title starting with `ncmpcpp` to workspace `$w9`. For instance, it moves your browser when you google "ncmpcpp".

```vim
assign [class="Emacs"] $w1
assign [class="qutebrowser"] $w2
assign [class="firefox"] $w2
assign [class="VK"] $w3
assign [class="Slack"] $w3
assign [class="discord"] $w3
assign [class="TelegramDesktop"] $w3
assign [class="Postman"] $w4
assign [class="Chromium-browse"] $w4
assign [class="chromium"] $w4
assign [class="google-chrome"] $w4
assign [title="Vue Developer Tools"] $w4
assign [class="Google Play Music Desktop Player"] $w9
assign [class="jetbrains-datagrip"] $w4
assign [class="zoom"] $w7
assign [class="skype"] $w7
assign [class="Mailspring"] $w8
assign [class="Thunderbird"] $w8
assign [class="Joplin"] $w8
assign [class="keepassxc"] $w10

for_window [title="VirtScreen"] floating enable

for_window [title="ncmpcpp.*"] move to workspace $w9
for_window [title="newsboat.*"] move to workspace $w9
for_window [title=".*run_wego"] move to workspace $w9
for_window [class="cinnamon-settings*"] floating enable
for_window [title="Picture-in-Picture"] sticky enable
for_window [window_role="GtkFileChooserDialog"] resize set width 1000 px height 800 px
for_window [window_role="GtkFileChooserDialog"] move position center
```


### Scratchpad {#scratchpad}

Scratch terminal, inspired by [this Luke Smith's video](https://www.youtube.com/watch?v=q-l7DnDbiiU).


#### Launch script {#launch-script}

First of all, we have to distinguish a scratchpad terminal from a normal one. To do that, one can create st with a required classname.

Then, it would be cool not to duplicate scratchpads, so the following script first looks for a window with a created classname. If it exists, the script just toggles the scratchpad visibility. Otherwise, a new instance of a window is created.

```bash
CLASSNAME="dropdown_tmux"
COMMAND="alacritty --class $CLASSNAME -e tmux new-session -s $CLASSNAME"
pid=$(xdotool search --classname "dropdown_tmux")
if [[ ! -z $pid  ]]; then
    i3-msg scratchpad show
else
    setsid -f ${COMMAND}
fi
```


#### i3 config {#i3-config}

```vim
# Scratchpad
for_window [instance="dropdown_*"] floating enable
for_window [instance="dropdown_*"] move scratchpad
for_window [instance="dropdown_*"] sticky enable
for_window [instance="dropdown_*"] scratchpad show
for_window [instance="dropdown_*"] move position center

bindsym $mod+u exec ~/bin/scripts/dropdown
```


### Gaps & borders {#gaps-and-borders}

The main reason to use i3-gaps

```vim
# Borders
# for_window [class=".*"] border pixel 0
default_border pixel 3
hide_edge_borders both

# Gaps
set $default_inner 10
set $default_outer 0

gaps inner $default_inner
gaps outer $default_outer

smart_gaps on
```


#### Keybindings {#keybindings}

```vim
mode "inner gaps" {
    bindsym plus gaps inner current plus 5
    bindsym minus gaps inner current minus 5
    bindsym Shift+plus gaps inner all plus 5
    bindsym Shift+minus gaps inner all minus 5
    bindsym 0 gaps inner current set 0
    bindsym Shift+0 gaps inner all set 0

    bindsym r gaps inner current set $default_inner
    bindsym Shift+r gaps inner all set $default_inner

    bindsym Return mode "default"
    bindsym Escape mode "default"
}

mode "outer gaps" {
    bindsym plus gaps outer current plus 5
    bindsym minus gaps outer current minus 5
    bindsym Shift+plus gaps outer all plus 5
    bindsym Shift+minus gaps outer all minus 5
    bindsym 0 gaps outer current set 0
    bindsym Shift+0 gaps outer all set 0

    bindsym r gaps outer current set $default_outer
    bindsym Shift+r gaps outer all set $default_outer

    bindsym Return mode "default"
    bindsym Escape mode "default"
}

bindsym $mod+g mode "inner gaps"
bindsym $mod+Shift+g mode "outer gaps"
```


### Move & resize windows {#move-and-resize-windows}

A more or less standard set of keybindings to move & resize floating windows.

Just be careful to always make a way to return from these new modes, otherwise you'd end up in a rather precarious situation.

```vim
# resize window (you can also use the mouse for that)
mode "resize" {
    # These bindings trigger as soon as you enter the resize mode

    bindsym h resize shrink width 10 px or 10 ppt
    bindsym j resize grow height 10 px or 10 ppt
    bindsym k resize shrink height 10 px or 10 ppt
    bindsym l resize grow width 10 px or 10 ppt

    bindsym Shift+h resize shrink width 100 px or 100 ppt
    bindsym Shift+j resize grow height 100 px or 100 ppt
    bindsym Shift+k resize shrink height 100 px or 100 ppt
    bindsym Shift+l resize grow width 100 px or 100 ppt

    # same bindings, but for the arrow keys
    bindsym Left resize shrink width 10 px or 10 ppt
    bindsym Down resize grow height 10 px or 10 ppt
    bindsym Up resize shrink height 10 px or 10 ppt
    bindsym Right resize grow width 10 px or 10 ppt

    bindsym Shift+Left resize shrink width 100 px or 100 ppt
    bindsym Shift+Down resize grow height 100 px or 100 ppt
    bindsym Shift+Up resize shrink height 100 px or 100 ppt
    bindsym Shift+Right resize grow width 100 px or 100 ppt

    # back to normal: Enter or Escape
    bindsym Return mode "default"
    bindsym Escape mode "default"
}

bindsym $mod+r mode "resize"

mode "move" {
    bindsym $mod+Tab focus right

    bindsym Left  move left
    bindsym Down  move down
    bindsym Up    move up
    bindsym Right move right

    bindsym h     move left
    bindsym j     move down
    bindsym k     move up
    bindsym l     move right

    # back to normal: Enter or Escape
    bindsym Return mode "default"
    bindsym Escape mode "default"
}

bindsym $mod+m mode "move" focus floating
```


### <span class="org-todo done OFF">OFF</span> (OFF) Intergration with dmenu {#off--intergration-with-dmenu}

[dmenu](https://tools.suckless.org/dmenu/) is a dynamic menu program for X. I've opted out of using it in favour of rofi, but here is a relevant bit of config.

Scripts are located in the `bin/scripts` folder.

```vim
# dmenu
bindsym $mod+d exec i3-dmenu-desktop --dmenu="dmenu -l 10"
bindsym $mod+apostrophe mode "dmenu"

mode "dmenu" {
    bindsym d exec i3-dmenu-desktop --dmenu="dmenu -l 10"; mode default
    bindsym p exec dmenu_run -l 10; mode default
    bindsym m exec dmenu-man; mode default
    bindsym b exec dmenu-buku; mode default
    bindsym f exec dmenu-explore; mode default
    bindsym t exec dmenu-tmuxp; mode default
    bindsym Escape mode "default"
}

bindsym $mod+b exec --no-startup-id dmenu-buku
```


### Integration with rofi {#integration-with-rofi}

Keybindings to launch [rofi](https://github.com/davatorium/rofi). For more detail, look the [Rofi](#rofi) section.

```vim
bindsym $mod+d exec "rofi -modi 'drun,run' -show drun"
bindsym $mod+b exec --no-startup-id rofi-buku-mine
bindsym $mod+minus exec rofi-pass; mode default

bindsym $mod+apostrophe mode "rofi"

mode "rofi" {
    bindsym d exec "rofi -modi 'drun,run' -show drun"
    bindsym m exec rofi-man; mode default
    bindsym b exec rofi-buku-mine; mode default
    bindsym k exec rofi-pass; mode default
    bindsym Escape mode "default"
}
```


### Launching apps & misc keybindings {#launching-apps-and-misc-keybindings}

I prefer to use a separate mode to launch most of my apps, with some exceptions.


#### Apps {#apps}

```vim
# Launch apps
# start a terminal at workspace 1
bindsym $mod+Return exec "i3-msg 'workspace 1 🚀; exec alacritty'"

bindsym $mod+p exec "copyq menu"
bindsym $mod+Shift+x exec "i3lock -f -i /home/pavel/Pictures/lock-wallpaper.png"

bindsym $mod+semicolon mode "apps"

mode "apps" {
    bindsym Escape mode "default"
    bindsym b exec firefox; mode default
    bindsym v exec vk; mode default
    bindsym s exec slack-wrapper; mode default;
    bindsym d exec "flatpak run com.discordapp.Discord"; mode default;
    bindsym m exec "alacritty -e ncmpcpp"; mode default
    bindsym c exec "copyq toggle"; mode default
    bindsym k exec "keepassxc"; mode default
    # bindsym e exec mailspring; mode default
    bindsym a exec emacs; mode default
    bindsym n exec "alacritty -e newsboat"; mode default
    bindsym w exec "alacritty /home/pavel/bin/scripts/run_wego"; mode default
    # bindsym a exec emacsclient -c; mode default
    # bindsym Shift+a exec emacs; mode default
}
```


#### Media controls & brightness {#media-controls-and-brightness}

```vim
# Pulse Audio controls
bindsym XF86AudioRaiseVolume exec --no-startup-id "ponymix increase 5 --max-volume 150"
bindsym XF86AudioLowerVolume exec --no-startup-id "ponymix decrease 5 --max-volume 150"
bindsym XF86AudioMute exec --no-startup-id "ponymix toggle"

exec --no-startup-id xmodmap -e 'keycode 135 = Super_R' && xset -r 135
bindsym $mod+F2 exec --no-startup-id "ponymix increase 5"
bindsym $mod+F3 exec --no-startup-id "ponymix decrease 5"

# Media player controls
bindsym XF86AudioPlay exec mpc toggle
bindsym XF86AudioPause exec mpc pause
bindsym XF86AudioNext exec mpc next
bindsym XF86AudioPrev exec mpc prev

# Screen brightness
bindsym XF86MonBrightnessUp exec light -A 5
bindsym XF86MonBrightnessDown exec light -U 5
```


#### Screenshots {#screenshots}

```vim
# Screenshots
bindsym --release Print exec "flameshot gui"
bindsym --release Shift+Print exec "xfce4-screenshooter"
```


### Colors {#colors}

Application of the XResources theme to the WM.

```vim
exec xrdb -merge $HOME/.Xresources

# Colors
set_from_resource $bg-color            background
set_from_resource $active-color        color4
set_from_resource $inactive-bg-color   color8
set_from_resource $text-color          foreground
set_from_resource $inactive-text-color color7
set_from_resource $urgent-bg-color     color1
set_from_resource $urgent-text-color   color0

# window colors
#                       border              background         text                 indicator       child border
client.focused          $active-color       $bg-color          $text-color          $bg-color       $active-color
client.unfocused        $bg-color           $inactive-bg-color $inactive-text-color $bg-color       $bg-color
client.focused_inactive $active-color       $inactive-bg-color $inactive-text-color $bg-color       $bg-color
client.urgent           $urgent-bg-color    $urgent-bg-color   $urgent-text-color   $bg-color       $urgent-bg-color
```


### <span class="org-todo done OFF">OFF</span> (OFF) i3blocks {#off--i3blocks}

I've opted out of i3bar & [i3blocks](https://github.com/vivien/i3blocks) for [polybar](https://github.com/polybar/polybar)

```vim
bar {
    status_command i3blocks -c ~/.config/i3/i3blocks.conf
    i3bar_command i3bar
    font pango:monospace 12
    output HDMI-A-0
    tray_output none
    colors {
	background $bg-color
	separator #757575
	#                  border             background         text
	focused_workspace  $bg-color          $bg-color          $text-color
	inactive_workspace $inactive-bg-color $inactive-bg-color $inactive-text-color
	urgent_workspace   $urgent-bg-color   $urgent-bg-color   $urgent-text-color
    }
}

bar {
    status_command i3blocks -c ~/.config/i3/i3blocks.conf
    i3bar_command i3bar
    font pango:monospace 10
    output DVI-D-0
    colors {
	background $bg-color
	separator #757575
	#                  border             background         text
	focused_workspace  $bg-color          $bg-color          $text-color
	inactive_workspace $inactive-bg-color $inactive-bg-color $inactive-text-color
	urgent_workspace   $urgent-bg-color   $urgent-bg-color   $urgent-text-color
    }
}
```


### Keyboard Layout {#keyboard-layout}

A script to set Russian-English keyboard layout:

```bash
setxkbmap -layout us,ru
setxkbmap -model pc105 -option 'grp:win_space_toggle' -option 'grp:alt_shift_toggle'
```

A script to toggle the layout

```bash
if setxkbmap -query | grep -q us,ru; then
    setxkbmap -layout us
    setxkbmap -option
else
    setxkbmap -layout us,ru
    setxkbmap -model pc105 -option 'grp:win_space_toggle' -option 'grp:alt_shift_toggle'
fi
```

And the relevant i3 settings:

```vim
# Layout
exec_always --no-startup-id set_layout
bindsym $mod+slash exec toggle_layout
```


### Autostart {#autostart}

```vim
# Polybar
exec_always --no-startup-id "bash /home/pavel/bin/polybar.sh"

# Wallpaper
exec_always "feh --bg-fill ~/Pictures/wallpaper.jpg"

# Picom
exec picom

# Keynav
exec keynav

# Applets
exec --no-startup-id nm-applet
# exec --no-startup-id /usr/bin/blueman-applet

exec shepherd
exec dunst
exec copyq
exec "xmodmap ~/.Xmodmap"
# exec "xrdb -merge ~/.Xresources"
# exec "bash ~/bin/autostart.sh"
```


## Polybar {#polybar}

| Guix dependency | Description |
|-----------------|-------------|
| polybar         | statusbar   |

[Polybar](https://github.com/polybar/polybar) is a nice-looking, WM-agnostic statusbar program.

I switched to polybar because I wanted to try out some WMs other than i3, but decided to stick with i3 for now.

Don't forget to install the Google Noto Color Emoji font. Guix package with all Noto fonts is way too large.

References:

-   [polybar docs](https://github.com/polybar/polybar/wiki)


### Launching {#launching}

The script below allows me to:

-   have different blocks on my two different-sized monitors and my laptop;
-   have different settings on my desktop PC and laptop;

<!--listend-->

```bash
hostname=$(hostname)
# Settings varying on the hostname
export WLAN_INTERFACE=$(nmcli -f DEVICE con show | grep -Ev "(.*docker.*|DEVICE|br.*|tun.*|veth.*|--)" | xargs)
if [ "$hostname" = "azure" ]; then
    TRAY_MONITOR="eDP-1"
    # export WLAN_INTERFACE="wlp3s0"
elif [ "$hostname" = "eminence" ]; then
    TRAY_MONITOR="eDP"
    # export WLAN_INTERFACE="wlo1"
else
    TRAY_MONITOR="HDMI-A-0"
    # export WLAN_INTERFACE="wlp35s0f3u2"
fi

# Setting varying on the monitor
declare -A FONT_SIZES=(
    ["eDP"]="13"
    ["eDP-1"]="13"
    ["DVI-D-0"]="13"
    ["HDMI-A-0"]="13"
)
declare -A EMOJI_SCALE=(
    ["eDP"]="9"
    ["eDP-1"]="9"
    ["DVI-D-0"]="10"
    ["HDMI-A-0"]="10"
)
declare -A BAR_HEIGHT=(
    ["eDP"]="29"
    ["eDP-1"]="29"
    ["DVI-D-0"]="29"
    ["HDMI-A-0"]="29"
)
declare -A BLOCKS=(
    ["eDP"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP battery SEP sun aw-afk date TSEP"
    ["eDP-1"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP battery SEP sun aw-afk date TSEP"
    ["DVI-D-0"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP weather SEP sun aw-afk date TSEP"
    ["HDMI-A-0"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP weather SEP sun aw-afk date TSEP"
)

# Geolocation for some modules
export LOC="SPB"

export IPSTACK_API_KEY=$(pass show My_Online/APIs/ipstack | head -n 1)

pkill polybar
for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    export MONITOR=$m
    if [ "$MONITOR" = "$TRAY_MONITOR" ]; then
	export TRAY="right"
    else
	export TRAY="none"
    fi
    SIZE=${FONT_SIZES[$MONITOR]}
    SCALE=${EMOJI_SCALE[$MONITOR]}
    if [[ -z "$SCALE" ]]; then
	continue
    fi
    export FONT0="pango:monospace:size=$SIZE;1"
    export FONT1="NotoEmoji:scale=$SCALE:antialias=false;1"
    export FONT2="fontawesome:pixelsize=$SIZE;1"
    export FONT3="JetBrains Mono Nerd Font:monospace:size=$SIZE;1"
    export HEIGHT=${BAR_HEIGHT[$MONITOR]}
    export RIGHT_BLOCKS=${BLOCKS[$MONITOR]}
    polybar mybar &
done
```


### General settings {#general-settings}


#### Colors {#colors}

```ini
[colors]
; Palenight colorscheme https://github.com/JonathanSpeek/palenight-iterm2
black = ${xrdb:color0}
red = ${xrdb:color1}
green = ${xrdb:color2}
yellow = ${xrdb:color3}
blue = ${xrdb:color4}
magenta = ${xrdb:color5}
cyan = ${xrdb:color6}
white = ${xrdb:color7}

black-lighter = ${xrdb:color8}
red-lighter = ${xrdb:color9}
green-lighter = ${xrdb:color10}
yellow-lighter = ${xrdb:color11}
blue-lighter = ${xrdb:color12}
magenta-lighter = ${xrdb:color13}
cyan-lighter = ${xrdb:color14}
white-lighter = ${xrdb:color15}

background = ${xrdb:background}
foreground = ${xrdb:foreground}
```


#### Bar config {#bar-config}

```ini
[bar/mybar]
monitor = ${env:MONITOR:}
width = 100%
height = ${env:HEIGHT:27}
offset-x = 0
offset-y = 0
radius = 0.0
fixed-center = false
bottom=true

background = ${colors.background}
foreground = ${colors.foreground}

line-size = 3
line-color = #f00

padding-left = 0
padding-right = 0

module-margin-left = 1
module-margin-right = 1

font-0 = ${env:FONT0:pango:monospace:size=10;1}
font-1 = ${env:FONT1:NotoEmoji:scale=10:antialias=false;0}
font-2 = ${env:FONT2:fontawesome:pixelsize=10;1}
font-3 = ${env:FONT3:JetBrains Mono Nerd Font:monospace:size=10;1}

modules-left = i3
; modules-center = test
modules-right = ${env:RIGHT_BLOCKS}

tray-position = ${env:TRAY:right}
tray-padding = 0
tray-maxsize = 16
;tray-background = #0063ff

wm-restack = i3
; override-redirect = true

scroll-up = i3wm-wsnext
scroll-down = i3wm-wsprev

; cursor-click = pointer
; cursor-scroll = ns-resize

[settings]
screenchange-reload = true
;compositing-background = xor
;compositing-background = screen
;compositing-foreground = source
;compositing-border = over
;pseudo-transparency = false

[global/wm]
margin-top = 0
margin-bottom = 0
```


### Modules {#modules}

Some of the custom modules below use Org mode noweb to evaluate colors, because it's faster than querying `xrdb` at runtime. I wish I could reference polybar values there, but [it looks like this is impossible](https://github.com/polybar/polybar/issues/615).

If you want to copy something, you can go to the [bin/polybar](bin/polybar/) folder.


#### ipstack-vpn {#ipstack-vpn}

| Guix dependency | Description             |
|-----------------|-------------------------|
| bind:utils      | Provides dig            |
| curl            |                         |
| jq              | util to work with JSONs |

A module to get a country of the current IP and openvpn status. Uses [ipstack](https://ipstack.com/) API.

```bash
ip=$(dig +short +timeout=1 myip.opendns.com @resolver1.opendns.com 2> /dev/null)
# API_KEY="$(pass show My_Online/APIs/ipstack | head -n 1)"
API_KEY=$IPSTACK_API_KEY
if [[ -z $ip || $ip == *"timed out"* ]]; then
    echo "%{u<<get-color(name="red")>>}%{+u} ?? %{u-}"
    exit
fi
ip_info=$(curl -s http://api.ipstack.com/${ip}?access_key=${API_KEY})
# emoji=$(echo $ip_info | jq -r '.location.country_flag_emoji')
code=$(echo $ip_info | jq -r '.country_code')
vpn=$(pgrep -a openvpn$ | head -n 1 | awk '{print $NF }' | cut -d '.' -f 1)

if [ -n "$vpn" ]; then
    echo "%{u<<get-color(name="blue")>>}%{+u}  $code %{u-}"
else
    echo "%{u<<get-color(name="red")>>}%{+u}  $code %{u-}"
fi
```

```ini
[module/ipstack-vpn]
type = custom/script
exec = /home/pavel/bin/polybar/ipstack-vpn.sh
interval = 1200
```


#### weather {#weather}

Gets current weather from [wttr.in](http://wttr.in/)

```bash
bar_format="${BAR_FORMAT:-"%t"}"
location="${LOCATION:-"Saint-Petersburg"}"
format_1=${FORMAT_1:-"qF"}
format_2=${FORMAT_1:-"format=v2n"}

bar_weather=$(curl -s wttr.in/${location}?format=${bar_format} || echo "??")
if [ -z "$bar_weather" ]; then
    exit 1
elif [[ "$bar_weather" == *"Unknown"* || "$bar_weather" == *"Sorry"* || "$bar_weather" == *"Bad Gateway"* ]]; then
    echo "??"
    exit 1
else
    echo ${bar_weather}
fi
```

```ini
[module/weather]
type = custom/script
exec = /home/pavel/bin/polybar/weather.sh
format-underline = ${colors.red}
interval = 1200
```


#### aw-afk {#aw-afk}

Prints out a current uptime and non-AFK time from [ActivityWatch](https://github.com/ActivityWatch) server

| Guix dependency |
|-----------------|
| dateutils       |

```bash
afk_event=$(curl -s -X GET "http://localhost:5600/api/0/buckets/aw-watcher-afk_$(hostname)/events?limit=1" -H "accept: application/json")
status=$(echo ${afk_event} | jq -r '.[0].data.status')
afk_time=$(echo "${afk_event}" | jq -r '.[0].duration' | xargs -I !  date -u -d @! +"%H:%M")

uptime=$(uptime | awk '{ print substr($3, 0, length($3) - 1) }' | xargs -I ! date -d ! +"%H:%M")
res="${afk_time} / ${uptime}"
if [[ $status == 'afk' ]]; then
    echo "%{u<<get-color(name="red")>>}%{+u} [AFK] $res %{u-}"
else
    echo "%{u<<get-color(name="blue")>>}%{+u} $res %{u-}"
fi
```

```ini
[module/aw-afk]
type = custom/script
exec = /home/pavel/bin/polybar/aw_afk.sh
interval = 60
```


#### sun {#sun}

| Guix dependency |
|-----------------|
| sunwait         |

Prints out the time of sunrise/sunset. Uses [sunwait](https://github.com/risacher/sunwait)

```bash
declare -A LAT_DATA=(
    ["TMN"]="57.15N"
    ["SPB"]="59.9375N"
)
declare -A LON_DATA=(
    ["TMN"]="65.533333E"
    ["SPB"]="30.308611E"
)
if [ -z "$LOC" ]; then
    echo "LOC?"
    exit -1
fi
LAT=${LAT_DATA[$LOC]}
LON=${LON_DATA[$LOC]}

time=$(sunwait poll daylight rise ${LAT} $LON)

if [[ ${time} == 'DAY' ]]; then
    sunset=$(sunwait list daylight set ${LAT} ${LON})
    echo "%{u<<get-color(name="yellow")>>}%{+u} $sunset %{u-}"
else
    sunrise=$(sunwait list daylight rise ${LAT} ${LON})
    echo "%{u<<get-color(name="red")>>}%{+u} $sunrise %{u-}"
fi
```

```ini
[module/sun]
type = custom/script
exec = /home/pavel/bin/polybar/sun.sh
interval = 60
```


#### SEP {#sep}

A simple separator

```ini
[module/SEP]
type = custom/text
content = "|"
content-foreground = ${colors.magenta}
content-padding = 0
content-margin = 0
interval = 100000
```


#### TSEP {#tsep}

A separator, which appears only if monitor is set to have a tray in the launch script

```bash
if [ ! -z "$TRAY" ] && [ "$TRAY" != "none" ]; then
    echo "| "
fi
```

```ini
[module/TSEP]
type = custom/script
exec = /home/pavel/bin/polybar/tray-sep.sh
format-foreground = ${colors.magenta}
interval = 100000
```


#### i3 {#i3}

Show i3wm workspaces

```ini
[module/i3]
type = internal/i3
format = <label-state> <label-mode>
index-sort = true
wrapping-scroll = false

; Only show workspaces on the same output as the bar
pin-workspaces = true

label-mode-padding = 1
label-mode-foreground = #000
label-mode-background = ${colors.blue}

; focused = Active workspace on focused monitor
label-focused = %
; label-focused-background = ${colors.background-alt}
label-focused-underline= ${colors.blue}
label-focused-padding = 1

; unfocused = Inactive workspace on any monitor
label-unfocused = %
label-unfocused-padding = 1

; visible = Active workspace on unfocused monitor
label-visible = %
; label-visible-background = ${self.label-focused-background}
label-visible-underline = ${self.label-focused-underline}
label-visible-padding = ${self.label-focused-padding}

; urgent = Workspace with urgency hint set
label-urgent = %
label-urgent-background = ${colors.red}
label-urgent-foreground = ${colors.black}
label-urgent-padding = 1
```


#### xkeyboard {#xkeyboard}

Current keyboard layout

```ini
[module/xkeyboard]
type = internal/xkeyboard
format = <label-layout>

format-underline = ${colors.magenta}
label-layout = %icon%
layout-icon-0 = ru;RU
layout-icon-1 = us;US
```


#### mpd {#mpd}

[Music Player Daemon](https://www.musicpd.org/) status

```ini
[module/mpd]
type = internal/mpd

format-playing = <toggle> <label-time> <label-song>
format-paused = <toggle> <label-time> <label-song>
format-stopped = 
label-song = [%album-artist%] %title%
label-time = %elapsed%/%total%

label-song-maxlen = 30
label-song-ellipsis = true

format-playing-underline = ${colors.yellow}
format-paused-underline = ${colors.yellow}
format-stopped-underline = ${colors.yellow}

label-separator = 0
separator-foreground = ${colors.red}

icon-pause = 
icon-play = 
icon-stop = 
icon-prev = 1
icon-next = 2
```


#### pulseaudio {#pulseaudio}

PulseAudio status

```ini
[module/pulseaudio]
type = internal/pulseaudio
use-ui-max = true

bar-volume-width = 7
bar-volume-foreground-0 = ${colors.white}
bar-volume-foreground-1 = ${colors.yellow}
bar-volume-foreground-2 = ${colors.yellow}
bar-volume-foreground-3 = ${colors.blue}
bar-volume-foreground-4 = ${colors.blue}
bar-volume-foreground-5 = ${colors.green}
bar-volume-foreground-6 = ${colors.green}
bar-volume-gradient = false
bar-volume-indicator = |
bar-volume-indicator-font = 2
bar-volume-fill = ─
bar-volume-fill-font = 2
bar-volume-empty = ─
bar-volume-empty-font = 2
bar-volume-empty-foreground = ${colors.white-lighter}

format-volume = ♪ <bar-volume> <label-volume>
label-volume = %percentage%%

format-mute = ♪ <label-muted>
label-muted = MUTE

format-volume-underline = ${colors.white}
format-muted-underline = ${colors.black-lighter}
```


#### cpu {#cpu}

CPU usage

```ini
[module/cpu]
type = internal/cpu
format =   <label>
label = %percentage%%
format-underline = ${colors.green-lighter}
```


#### ram-memory {#ram-memory}

RAM usage

```ini
[module/ram-memory]
type = internal/memory
interval = 10

ramp-used-0 = ▁
ramp-used-1 = ▂
ramp-used-2 = ▃
ramp-used-3 = ▄
ramp-used-4 = ▅
ramp-used-5 = ▆
ramp-used-6 = ▇
ramp-used-7 = █

format =  <label>
label=%gb_used:.1f%

format-underline = ${colors.blue}
```


#### swap-memory {#swap-memory}

Swap usage

```ini
[module/swap-memory]
type = internal/memory
interval = 10

label= %gb_swap_used:.1f%
format-underline = ${colors.yellow}
```


#### network {#network}

Upload/download speed

```ini
[module/network]
type = internal/network
interval = 1

interface = ${env:WLAN_INTERFACE}

; format-connected = [<ramp-signal>] <label-connected>

label-connected = ↓ %downspeed% ↑ %upspeed%
label-disconnected = X

format-connected-underline = ${colors.green}
format-disconnected-underline = ${colors.red}

ramp-signal-0 = 0
ramp-signal-1 = 1
ramp-signal-2 = 2
ramp-signal-3 = 3
ramp-signal-4 = 4
ramp-signal-5 = 5
```


#### date {#date}

Current date

```ini
[module/date]
type = internal/date
interval = 5

date =
date-alt = "%Y-%m-%d"

time = %H:%M
time-alt = %H:%M:%S

format-underline = ${colors.cyan}
label = "%date% %time%"
```


#### battery {#battery}

```ini
[module/battery]
type = internal/battery
battery = BAT0
adapter = ADP0

time-format = %H:%M
format-discharging = <ramp-capacity> <label-discharging>
format-discharging-underline = ${colors.cyan}
format-charging-underline = ${colors.yellow}
format-full-underline = ${colors.green}
label-discharging = %percentage%% %time%
label-charging =  %percentage%% %time%

ramp-capacity-0 = 
ramp-capacity-1 = 
ramp-capacity-2 = 
ramp-capacity-3 = 
ramp-capacity-4 = 
```


## Rofi {#rofi}

| Guix dependency |
|-----------------|
| rofi            |

[rofi](https://github.com/davatorium/rofi) is another dynamic menu generator. It can act as dmenu replacement but offers a superset of dmenu's features.


### Theme {#theme}

A theme, based on [dracula theme](https://github.com/dracula/rofi) for rofi, but with palenight colorscheme.

<a id="code-snippet--get-rofi-colors"></a>
```emacs-lisp
(apply
 #'concat
 (mapcar
  (lambda (elem)
    (concat (nth 0 elem) ": " (nth 2 elem) ";\n"))
  table))
```

```css
/* Generated from [[file:../../Desktop.org::*Theme][Theme:1]] */
 * {
    <<get-rofi-colors()>>

    foreground:                  @white;
    background:                  @black;
    background-color:            @black;
    separatorcolor:              @blue;
    border-color:                @blue;
    selected-normal-foreground:  @black;
    selected-normal-background:  @blue;
    selected-active-foreground:  @black;
    selected-active-background:  @blue;
    selected-urgent-foreground:  @foreground;
    selected-urgent-background:  @red;
    normal-foreground:           @foreground;
    normal-background:           @background;
    active-foreground:           @blue;
    active-background:           @background;
    urgent-foreground:           @red;
    urgent-background:           @background;
    alternate-normal-foreground: @foreground;
    alternate-normal-background: @light-black;
    alternate-active-foreground: @blue;
    alternate-active-background: @light-black;
    alternate-urgent-foreground: @red;
    alternate-urgent-background: @light-black;
    spacing:                     2;
}
window {
    background-color: @background;
    border:           1;
    padding:          5;
}
mainbox {
    border:           0;
    padding:          0;
}
message {
    border:           1px dash 0px 0px ;
    border-color:     @separatorcolor;
    padding:          1px ;
}
textbox {
    text-color:       @foreground;
}
listview {
    fixed-height:     0;
    border:           2px dash 0px 0px ;
    border-color:     @separatorcolor;
    spacing:          2px ;
    scrollbar:        true;
    padding:          2px 0px 0px ;
}
element {
    border:           0;
    padding:          1px ;
}
element normal.normal {
    background-color: @normal-background;
    text-color:       @normal-foreground;
}
element normal.urgent {
    background-color: @urgent-background;
    text-color:       @urgent-foreground;
}
element normal.active {
    background-color: @active-background;
    text-color:       @active-foreground;
}
element selected.normal {
    background-color: @selected-normal-background;
    text-color:       @selected-normal-foreground;
}
element selected.urgent {
    background-color: @selected-urgent-background;
    text-color:       @selected-urgent-foreground;
}
element selected.active {
    background-color: @selected-active-background;
    text-color:       @selected-active-foreground;
}
element alternate.normal {
    background-color: @alternate-normal-background;
    text-color:       @alternate-normal-foreground;
}
element alternate.urgent {
    background-color: @alternate-urgent-background;
    text-color:       @alternate-urgent-foreground;
}
element alternate.active {
    background-color: @alternate-active-background;
    text-color:       @alternate-active-foreground;
}
scrollbar {
    width:            4px ;
    border:           0;
    handle-color:     @normal-foreground;
    handle-width:     8px ;
    padding:          0;
}
sidebar {
    border:           2px dash 0px 0px ;
    border-color:     @separatorcolor;
}
button {
    spacing:          0;
    text-color:       @normal-foreground;
}
button selected {
    background-color: @selected-normal-background;
    text-color:       @selected-normal-foreground;
}
inputbar {
    spacing:          0px;
    text-color:       @normal-foreground;
    padding:          1px ;
    children:         [ prompt,textbox-prompt-colon,entry,case-indicator ];
}
case-indicator {
    spacing:          0;
    text-color:       @normal-foreground;
}
entry {
    spacing:          0;
    text-color:       @normal-foreground;
}
prompt {
    spacing:          0;
    text-color:       @normal-foreground;
}
textbox-prompt-colon {
    expand:           false;
    str:              ":";
    margin:           0px 0.3000em 0.0000em 0.0000em ;
    text-color:       inherit;
}
```


### Scripts {#scripts}


#### Buku bookmarks {#buku-bookmarks}

Inspired by the [knatsakis/rofi-buku](https://github.com/knatsakis/rofi-buku) script.

```bash
if [ $(hostname) = 'pdsk' ]; then
    BUKU="/home/pavel/.local/bin/buku"
else
    BUKU="/home/pavel/Programs/miniconda3/bin/buku"
fi

# COMMAND="$BUKU -o %"
# COMMAND="qutebrowser $(buku -f 10 -p %)"
COMMAND="firefox %"
if [[ $1 == '-e' ]]; then
    COMMAND="$BUKU -w %"
fi
$BUKU -f 4 -p | awk -F'\t' -v OFS='\t' '{
    split($4, tags, ",")
    joined = sep = ""
    for (i = 1; i in tags; i++) {
	joined = joined sep "[" tags[i] "]"
	sep = " "
    }
    url = substr($2, 1, 40)
    if (length($2) > 40) {
	url = url "..."
    }
    if ($1 != "waiting for input") {
	printf "%-5s %-60s %-45s %s\n", $1, $3, url, joined
    }
}' | sort -k 2 | rofi -dmenu -matching normal -sort -sorting-method fzf -width 80 -l 20 | cut -d ' ' -f 1 | {
    read index;
    if [[ -z "$index" ]]; then
	exit 0
    fi
    url=$($BUKU -f 10 -p $index)
    echo ${url#"waiting for input"} | cut -d ' ' -f 1 | xargs -I % $COMMAND
}
```


#### Man pages {#man-pages}

Inspired by [this Luke Smith's video](https://www.youtube.com/watch?v=8E8sUNHdzG8).

A script to open a man page with zathura. There is no particular reason why one should look through man pages in pdf viewer rather than in console, but why not.

```bash
SELECTED=$(man -k . | rofi -dmenu -l 20 | awk '{print $1}')
if [[ ! -z $SELECTED ]]; then
    man -Tpdf $SELECTED | zathura -
fi
```


#### pass {#pass}

| Guix dependency |
|-----------------|
| rofi-pass       |
| xset            |

A nice [pass frontend for Rofi](https://github.com/carnager/rofi-pass), which is even packaged for Guix.

```bash
USERNAME_field='username'
EDITOR=vim
default_autotype='username :tab pass'
clip=both
```


## Flameshot {#flameshot}

| Guix dependency |
|-----------------|
| flameshot       |

[flameshot](https://github.com/flameshot-org/flameshot) is my program of choice to make screenshots.

As it overwrites its own config all the time, I do not keep the file in VC.

```ini
[General]
disabledTrayIcon=false
drawColor=#ff0000
drawThickness=0
saveAfterCopyPath=/home/pavel/Pictures
savePath=/home/pavel/Pictures
savePathFixed=false
showStartupLaunchMessage=false
uiColor=<<get-color(name="blue")>>

[Shortcuts]
TYPE_ARROW=A
TYPE_CIRCLE=C
TYPE_CIRCLECOUNT=
TYPE_COMMIT_CURRENT_TOOL=Ctrl+Return
TYPE_COPY=Ctrl+C
TYPE_DRAWER=D
TYPE_EXIT=Ctrl+Q
TYPE_IMAGEUPLOADER=Return
TYPE_MARKER=M
TYPE_MOVESELECTION=Ctrl+M
TYPE_MOVE_DOWN=Down
TYPE_MOVE_LEFT=Left
TYPE_MOVE_RIGHT=Right
TYPE_MOVE_UP=Up
TYPE_OPEN_APP=Ctrl+O
TYPE_PENCIL=P
TYPE_PIN=
TYPE_PIXELATE=B
TYPE_RECTANGLE=R
TYPE_REDO=Ctrl+Shift+Z
TYPE_RESIZE_DOWN=Shift+Down
TYPE_RESIZE_LEFT=Shift+Left
TYPE_RESIZE_RIGHT=Shift+Right
TYPE_RESIZE_UP=Shift+Up
TYPE_SAVE=Ctrl+S
TYPE_SELECTION=S
TYPE_SELECTIONINDICATOR=
TYPE_SELECT_ALL=Ctrl+A
TYPE_TEXT=T
TYPE_TOGGLE_PANEL=Space
TYPE_UNDO=Ctrl+Z
```


## dunst {#dunst}

| Guix dependency |
|-----------------|
| dunst           |
| libnotify       |

| Type | Note                            |
|------|---------------------------------|
| TODO | Cleanup default config comments |

[dunst](https://github.com/dunst-project/dunst) is a lightweight notification daemon.

My customizations of the original config consist mostly of changing colors.

References:

-   [dunst documentation](https://dunst-project.org/documentation/)

<!--listend-->

```vim
[global]
    monitor = 0

    follow = mouse

    # The geometry of the window:
    #   [{width}]x{height}[+/-{x}+/-{y}]
    # The geometry of the message window.
    # The height is measured in number of notifications everything else
    # in pixels.  If the width is omitted but the height is given
    # ("-geometry x2"), the message window expands over the whole screen
    # (dmenu-like).  If width is 0, the window expands to the longest
    # message displayed.  A positive x is measured from the left, a
    # negative from the right side of the screen.  Y is measured from
    # the top and down respectively.
    # The width can be negative.  In this case the actual width is the
    # screen width minus the width defined in within the geometry option.
    geometry = "300x5-30+20"

    # Show how many messages are currently hidden (because of geometry).
    indicate_hidden = yes

    # Shrink window if its smaller than the width.  Will be ignored if
    # width is 0.
    shrink = no

    # The transparency of the window.  Range: [0; 100].
    # This option will only work if a compositing window manager is
    # present (e.g. xcompmgr, compiz, etc.).
    transparency = 15

    # The height of the entire notification.  If the height is smaller
    # than the font height and padding combined, it will be raised
    # to the font height and padding.
    notification_height = 0

    # Draw a line of "separator_height" pixel height between two
    # notifications.
    # Set to 0 to disable.
    separator_height = 2

    # Padding between text and separator.
    padding = 8

    # Horizontal padding.
    horizontal_padding = 8

    # Defines width in pixels of frame around the notification window.
    # Set to 0 to disable.
    frame_width = 1

    # Defines color of the frame around the notification window.
    frame_color = <<get-color(name="white", quote=1)>>

    # Define a color for the separator.
    # possible values are:
    #  * auto: dunst tries to find a color fitting to the background;
    #  * foreground: use the same color as the foreground;
    #  * frame: use the same color as the frame;
    #  * anything else will be interpreted as a X color.
    separator_color = frame

    # Sort messages by urgency.
    sort = yes

    # Don't remove messages, if the user is idle (no mouse or keyboard input)
    # for longer than idle_threshold seconds.
    # Set to 0 to disable.
    # A client can set the 'transient' hint to bypass this. See the rules
    # section for how to disable this if necessary
    idle_threshold = 120

    ### Text ###

    font = DejaVu Sans 9

    # The spacing between lines.  If the height is smaller than the
    # font height, it will get raised to the font height.
    line_height = 0

    # Possible values are:
    # full: Allow a small subset of html markup in notifications:
    #        <b>bold</b>
    #        <i>italic</i>
    #        <s>strikethrough</s>
    #        <u>underline</u>
    #
    #        For a complete reference see
    #        <http://developer.gnome.org/pango/stable/PangoMarkupFormat.html>.
    #
    # strip: This setting is provided for compatibility with some broken
    #        clients that send markup even though it's not enabled on the
    #        server. Dunst will try to strip the markup but the parsing is
    #        simplistic so using this option outside of matching rules for
    #        specific applications *IS GREATLY DISCOURAGED*.
    #
    # no:    Disable markup parsing, incoming notifications will be treated as
    #        plain text. Dunst will not advertise that it has the body-markup
    #        capability if this is set as a global setting.
    #
    # It's important to note that markup inside the format option will be parsed
    # regardless of what this is set to.
    markup = full

    # The format of the message.  Possible variables are:
    #   %a  appname
    #   %s  summary
    #   %b  body
    #   %i  iconname (including its path)
    #   %I  iconname (without its path)
    #   %p  progress value if set ([  0%] to [100%]) or nothing
    #   %n  progress value if set without any extra characters
    #   %%  Literal %
    # Markup is allowed
    format = "<b>%s</b>\n%b"

    # Alignment of message text.
    # Possible values are "left", "center" and "right".
    alignment = left

    # Show age of message if message is older than show_age_threshold
    # seconds.
    # Set to -1 to disable.
    show_age_threshold = 60

    # Split notifications into multiple lines if they don't fit into
    # geometry.
    word_wrap = yes

    # When word_wrap is set to no, specify where to make an ellipsis in long lines.
    # Possible values are "start", "middle" and "end".
    ellipsize = middle

    # Ignore newlines '\n' in notifications.
    ignore_newline = no

    # Stack together notifications with the same content
    stack_duplicates = true

    # Hide the count of stacked notifications with the same content
    hide_duplicate_count = false

    # Display indicators for URLs (U) and actions (A).
    show_indicators = yes

    ### Icons ###

    # Align icons left/right/off
    icon_position = left

    # Scale larger icons down to this size, set to 0 to disable
    max_icon_size = 32

    # Paths to default icons.
    icon_path = /usr/share/icons/Mint-Y/status/32/;/usr/share/icons/Mint-Y/devices/32

    ### History ###

    # Should a notification popped up from history be sticky or timeout
    # as if it would normally do.
    sticky_history = yes

    # Maximum amount of notifications kept in history
    history_length = 20

    ### Misc/Advanced ###

    # dmenu path.
    dmenu = /usr/bin/dmenu -p dunst:

    # Browser for opening urls in context menu.
    browser = /usr/bin/sensible-browser

    # Always run rule-defined scripts, even if the notification is suppressed
    always_run_script = true

    # Define the title of the windows spawned by dunst
    title = Dunst

    # Define the class of the windows spawned by dunst
    class = Dunst

    # Print a notification on startup.
    # This is mainly for error detection, since dbus (re-)starts dunst
    # automatically after a crash.
    startup_notification = false

    # Manage dunst's desire for talking
    # Can be one of the following values:
    #  crit: Critical features. Dunst aborts
    #  warn: Only non-fatal warnings
    #  mesg: Important Messages
    #  info: all unimportant stuff
    # debug: all less than unimportant stuff
    verbosity = mesg

    # Define the corner radius of the notification window
    # in pixel size. If the radius is 0, you have no rounded
    # corners.
    # The radius will be automatically lowered if it exceeds half of the
    # notification height to avoid clipping text and/or icons.
    corner_radius = 0

    ### Legacy

    # Use the Xinerama extension instead of RandR for multi-monitor support.
    # This setting is provided for compatibility with older nVidia drivers that
    # do not support RandR and using it on systems that support RandR is highly
    # discouraged.
    #
    # By enabling this setting dunst will not be able to detect when a monitor
    # is connected or disconnected which might break follow mode if the screen
    # layout changes.
    force_xinerama = false

    ### mouse

    # Defines action of mouse event
    # Possible values are:
    # * none: Don't do anything.
    # * do_action: If the notification has exactly one action, or one is marked as default,
    #              invoke it. If there are multiple and no default, open the context menu.
    # * close_current: Close current notification.
    # * close_all: Close all notifications.
    mouse_left_click = close_current
    mouse_middle_click = do_action
    mouse_right_click = close_all

# Experimental features that may or may not work correctly. Do not expect them
# to have a consistent behaviour across releases.
[experimental]
    # Calculate the dpi to use on a per-monitor basis.
    # If this setting is enabled the Xft.dpi value will be ignored and instead
    # dunst will attempt to calculate an appropriate dpi value for each monitor
    # using the resolution and physical size. This might be useful in setups
    # where there are multiple screens with very different dpi values.
    per_monitor_dpi = false

[shortcuts]

    # Shortcuts are specified as [modifier+][modifier+]...key
    # Available modifiers are "ctrl", "mod1" (the alt-key), "mod2",
    # "mod3" and "mod4" (windows-key).
    # Xev might be helpful to find names for keys.

    # Close notification.
    close = ctrl+space

    # Close all notifications.
    close_all = ctrl+shift+space

    # Redisplay last message(s).
    # On the US keyboard layout "grave" is normally above TAB and left
    # of "1". Make sure this key actually exists on your keyboard layout,
    # e.g. check output of 'xmodmap -pke'
    history = ctrl+grave

    # Context menu.
    context = ctrl+shift+period

[urgency_low]
    # IMPORTANT: colors have to be defined in quotation marks.
    # Otherwise the "#" and following would be interpreted as a comment.
    background = <<get-color(name="light-black", quote=1)>>
    frame_color = <<get-color(name="white", quote=1)>>
    foreground = <<get-color(name="light-white", quote=1)>>
    timeout = 10
    # Icon for notifications with low urgency, uncomment to enable
    #icon = /path/to/icon

[urgency_normal]
    background = <<get-color(name="black", quote=1)>>
    frame_color = <<get-color(name="white", quote=1)>>
    foreground = <<get-color(name="light-white", quote=1)>>
    timeout = 10
    # Icon for notifications with normal urgency, uncomment to enable
    #icon = /path/to/icon

[urgency_critical]
    background = <<get-color(name="red", quote=1)>>
    foreground = <<get-color(name="light-white", quote=1)>>
    frame_color = <<get-color(name="red", quote=1)>>
    timeout = 0
    # Icon for notifications with critical urgency, uncomment to enable
    #icon = /path/to/icon
```


## keynav {#keynav}

| Guix dependency |
|-----------------|
| keynav          |

| Type    | Note                           |
|---------|--------------------------------|
| SYMLINK | ./config/keynavrc -> .keynavrc |

How many times you have been working with keyboard-driven programs and had to use a mouse just to press some pesky little button in a modal window?

[keynav](https://github.com/jordansissel/keynav) is a program that allows you to control the mouse with the keyboard with the general idea of bisecting the screen to get to the required point. I'm still not sure if there is any point in using it, but it's rather funny. Unfortunately, the colors seem to be hardcoded.

One of the usecases I found so far is to use the program to scroll webpages when tridactyl's scroll captures the wrong scroll area.

References:

-   [keynav documentation](https://github.com/jordansissel/keynav/blob/master/keynav.pod)


### Config {#config}

```vim
# clear all previous keybindings
clear

# Start & stop
ctrl+semicolon start
Super_L+bracketright start
Super_R+bracketright start
Escape end
ctrl+bracketleft end

# Macros
q record ~/.keynav_macros
shift+at playback

# Bisecting
a history-back
Left cut-left
Right cut-right
Down cut-down
Up cut-up
h cut-left
j cut-down
k cut-up
l cut-right
t windowzoom                          # Zoom to the current window
c cursorzoom 300 300                  # Limit the bisection area by 300x300

# Move the bisecting area
shift+h move-left
shift+j move-down
shift+k move-up
shift+l move-right
shift+Left move-left
shift+Right move-right
shift+Up move-up
shift+Down move-down

# Actions
space warp,click 3,end                # Right click
Return warp,click 1,end               # Left click
Shift+Return warp,doubleclick 1,end   # Double left click
semicolon warp,end                    # Move the cursor and exit
w warp                                # Just move the cursor
e end                                 # exit
u warp,click 4                        # scroll up
d warp,click 5                        # scroll down
1 click 1
2 click 2
3 click 3
4 click 4
5 click 5
```


### Using with picom {#using-with-picom}

I've noticed that the program does not play nice with picom's fade effect. To fix that, add the following to you config:

```ini
fade-exclude = [
  "class_i = 'keynav'",
  "class_g = 'keynav'",
]
```


## Picom {#picom}

| Guix dependency |
|-----------------|
| picom           |

[picom](https://github.com/yshui/picom) is a compositor for X11. It allows effects such as transparency, blurring, etc.

Sample configuration is a good resource for getting an overview of the available settings. I have only a bunch of necessary settings in mine.

There are a bunch of forks for picom (e.g. [ibhagwan/picom](https://github.com/ibhagwan/picom) adds rounded corners) which seem to have some popularity, but I use the base one.

References:

-   [picom wiki](https://github.com/yshui/picom/wiki)
-   [Picom on ArchWiki](https://wiki.archlinux.org/index.php/Picom)
-   [Sample configuration](https://github.com/yshui/picom/blob/next/picom.sample.conf)


### Shadows {#shadows}

```ini
shadow = true;
shadow-radius = 2;
shadow-offset-x = -2;
shadow-offset-y = -2;

shadow-exclude = [
  "name = 'Notification'",
  "class_g = 'Conky'",
  "name ?= 'cpt_frame_window'",
  "class_g ?= 'Notify-osd'",
  "class_g = 'Cairo-clock'",
  "_GTK_FRAME_EXTENTS@:c"
];
```


### Fading {#fading}

```ini
fading = true

fade-in-step = 0.03;
fade-out-step = 0.03;
fade-delta = 10

fade-exclude = [
  "class_i = 'keynav'",
  "class_g = 'keynav'",
]
```


### Opacity {#opacity}

I don't use stuff like transparency for inactive windows.

The first 5 lines of `opacity-rule` make i3wm's hidden windows 100% transparent, so I see the background behind the semi-transparent windows in i3wm's stacked and tabbed layout. Here is [StackExchange question](https://unix.stackexchange.com/questions/281131/compton-i3-tabbed-stacked-transparency-background-image) about that.

I also noticed that for some reason it doesn't play well with Emacs's built-in transparency, so the last line sets up Emacs transparency at 90%.

```ini
inactive-opacity = 1;

frame-opacity = 1.0;
inactive-opacity-override = false;
focus-exclude = [ "class_g = 'Cairo-clock'" ];

opacity-rule = [
  "0:_NET_WM_STATE@[0]:32a = '_NET_WM_STATE_HIDDEN'",
  "0:_NET_WM_STATE@[1]:32a = '_NET_WM_STATE_HIDDEN'",
  "0:_NET_WM_STATE@[2]:32a = '_NET_WM_STATE_HIDDEN'",
  "0:_NET_WM_STATE@[3]:32a = '_NET_WM_STATE_HIDDEN'",
  "0:_NET_WM_STATE@[4]:32a = '_NET_WM_STATE_HIDDEN'",
  "90:class_g = 'Emacs'"
];
```


### General settings {#general-settings}

Default general settings. Editing some of these may be neeeded in case of performance issues.

```ini
backend = "xrender";
vsync = true
mark-wmwin-focused = true;
mark-ovredir-focused = true;
detect-rounded-corners = true;
detect-client-opacity = true;
refresh-rate = 0
detect-transient = true
detect-client-leader = true
use-damage = true
log-level = "warn";

wintypes:
{
  tooltip = { fade = true; shadow = true; opacity = 0.75; focus = true; full-shadow = false; };
  dock = { shadow = false; }
  dnd = { shadow = false; }
  popup_menu = { opacity = 1; }
  dropdown_menu = { opacity = 1; }
};
```


## Zathura {#zathura}

| Guix dependency     |
|---------------------|
| zathura             |
| zathura-ps          |
| zathura-pdf-poppler |
| zathura-djvu        |

[Zathura](https://pwmt.org/projects/zathura/) is a pdf viewer with vim-like keybindings. One of my favorite features is an ability to invert the document colors.

```vim
set abort-clear-search false
set show-scrollbars true
set show-h-scrollbar true
set show-v-scrollbar true
set selection-clipboard clipboard
set recolor-lightcolor <<get-color(name="black", quote=1)>>
set recolor true
map <C-r> set recolor false
map <C-R> set recolor true
```


## Various software {#various-software}

This section generates manifests for various desktop software that I'm using.


### Browsers {#browsers}

| Category | Guix dependency    |
|----------|--------------------|
| browsers | ungoogled-chromium |
| browsers | firefox            |


### Office {#office}

| Category | Guix dependency |
|----------|-----------------|
| office   | libreoffice     |
| office   | gimp            |
| office   | krita           |


### LaTeX {#latex}

| Category | Guix dependency               |
|----------|-------------------------------|
| latex    | texlive                       |
| latex    | texlab-bin                    |
| latex    | biber                         |
| latex    | python-pygments               |
| latex    | font-microsoft-web-core-fonts |


### Dev {#dev}

| Category | Guix dependency |
|----------|-----------------|
| dev      | conda           |
| dev      | docker-compose  |
| dev      | postgresql      |
| dev      | virt-manager    |
| dev      | git-filter-repo |
| dev      | node            |
| dev      | openjdk         |
| dev      | go              |
| dev      | gcc-toolchain   |
| dev      | lua             |
| dev      | libfaketime     |
| dev      | hugo-extended   |
| dev      | make            |


### Manifests {#manifests}

<a id="code-snippet--packages"></a>
```emacs-lisp
(my/format-guix-dependencies category)
```

Dev

```scheme
(specifications->manifest
 '(
   <<packages("dev")>>))
```

Browsers

```scheme
(specifications->manifest
 '(
   <<packages("browsers")>>))
```

Music

```scheme
(specifications->manifest
 '(
   <<packages("music")>>))
```

Office

```scheme
(specifications->manifest
 '(
   <<packages("office")>>))
```

LaTeX

```scheme
(specifications->manifest
 '(
   <<packages("latex")>>))
```


### Flatpak {#flatpak}

A lot of proprietary desktop applications can be installed most easily with flatpak & flathub.

| Guix dependency    |
|--------------------|
| flatpak            |
| xdg-desktop-portal |

After installation, add the following repositories:

```text
flatpak remote-add --user --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak remote-add --user --if-not-exists flathub-beta https://flathub.org/beta-repo/flathub-beta.flatpakrepo
```

Installation syntax is as follows:

```text
flatpak install --user <remote> <package>
```

Packages to install:

<a id="table--flatpak-deps"></a>

| Flatpak dependency           | Channel |
|------------------------------|---------|
| com.github.wwmm.pulseeffects | flathub |
| com.discordapp.Discord       | flathub |
| us.zoom.Zoom                 | flathub |
| com.slack.Slack              | flathub |

```emacs-lisp
(mapconcat
 (lambda (c) (concat "flatpak install -y --user " (nth 1 c) " " (nth 0 c)))
 table
 "\n")
```


### Nix {#nix}

| Type | Description        |
|------|--------------------|
| TODO | Make nix manifest? |

I probably should've used nix, as almost every program I packaged so far exists in the Nix repo.

But it's easy enough to use Nix on Guix.

```conf
https://nixos.org/channels/nixpkgs-unstable nixpkgs
```

Don't forget to run the following after the first installation:

```sh
nix-channel --update
```

Installing packages:

```nil
nix-env -i vk-messenger slack
```


## Services {#services}

[GNU Shepherd](https://www.gnu.org/software/shepherd/manual/html%5Fnode/index.html) is a service management system for GNU Guix.

I previously used supervisor, but shepherd also seems pretty capable.


### Music {#music}

| Category | Guix dependency |
|----------|-----------------|
| music    | mpd             |
| music    | ncmpcpp         |
| music    | picard          |
| music    | mpd-watcher     |
| music    | mpd-mpc         |
| music    | shntool         |
| music    | cuetools        |
| music    | flac            |

Music player daemon

```scheme
(define mpd
  (make <service>
    #:provides '(mpd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd" "--no-daemon"))
    #:stop (make-kill-destructor)))
```

MPD watcher

```scheme
(define mpd-watcher
  (make <service>
    #:provides '(mpd-watcher)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd_watcher"))
    #:stop (make-kill-destructor)
    #:requires '(mpd)))
```


### GNU Mcron {#gnu-mcron}

[GNU Mcron](https://www.gnu.org/software/mcron/) is a replacement for cron, written in Scheme.

```scheme
(define mcron
  (make <service>
    #:provides '(mcron)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop (make-kill-destructor)))
```


### ActivityWatch {#activitywatch}

[ActivityWatch](https://activitywatch.net/) is a FOSS time tracker. It tracks screen and application usage and has integrations with browsers, Emacs, etc.

| Guix dependency   |
|-------------------|
| activitywatch-bin |

aw-server

```scheme
(define aw-server
  (make <service>
    #:provides '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("aw-server"))
    #:stop (make-kill-destructor)))
```

`aw-watcher-afk` has some problems with statup, so there is a wrapper script

```sh
sleep 5
aw-watcher-afk
```

aw-watcher-afk

```scheme
(define aw-watcher-afk
  (make <service>
    #:provides '(aw-watcher-afk)
    #:requires '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/aw-watcher-afk-wrapper"))
    #:stop (make-kill-destructor)))
```

aw-watcher-window

```scheme
(define aw-watcher-window
  (make <service>
    #:provides '(aw-watcher-window)
    #:requires '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("aw-watcher-window"))
    #:stop (make-kill-destructor)))
```


### PulseEffects {#pulseeffects}

```scheme
(define pulseeffects
  (make <service>
    #:provides '(pulseeffects)
    #:respawn? #t
    #:start (make-forkexec-constructor '("flatpak" "run" "com.github.wwmm.pulseeffects" "--gapplication-service"))
    #:stop (make-kill-destructor)))
```


### xsettingsd {#xsettingsd}

```scheme
(define xsettingsd
  (make <service>
    #:provides '(xsettingsd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("xsettingsd"))
    #:stop (make-kill-destructor)))
```


### Discord rich presence {#discord-rich-presence}

References:

-   [Rich Precense (discord rpc)](https://github.com/flathub/com.discordapp.Discord/wiki/Rich-Precense-(discord-rpc))

<!--listend-->

```scheme
(define discord-rich-presence
  (make <service>
    #:provides '(discord-rich-presence)
    #:one-shot? #t
    #:start (make-system-constructor "ln -sf {app/com.discordapp.Discord,$XDG_RUNTIME_DIR}/discord-ipc-0")))
```


### Polkit Authentication agent {#polkit-authentication-agent}

Launch an authentication agent. Necessary for stuff like `pkexec`. I suspect I'm not doing that the intended way, but it seems to work.

```scheme
(define polkit-gnome
  (make <service>
    #:provides '(polkit-gnome)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/.guix-extra-profiles/desktop/desktop/libexec/polkit-gnome-authentication-agent-1"))
    #:stop (make-kill-destructor)))
```


### Xmodmap {#xmodmap}

```scheme
(define xmodmap
  (make <service>
    #:provides '(xmodmap)
    #:one-shot? #t
    #:start (make-system-constructor "xmodmap /home/pavel/.Xmodmap")))
```


### VPN {#vpn}

Run my [OpenVPN setup]({{< relref "Guix" >}}). Not lauching this automatially, as it requires an active connection.

```scheme
(define vpn
  (make <service>
    #:provides '(vpn)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/vpn-start"))
    #:stop (make-kill-destructor)))
```


### Davmail {#davmail}

```scheme
(define davmail
  (make <service>
    #:provides '(davmail)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/davmail"))
    #:stop (make-kill-destructor)))
```


### Shepherd config {#shepherd-config}

Register services

```scheme
(register-services
 mpd
 mpd-watcher
 mcron
 aw-server
 aw-watcher-afk
 aw-watcher-window
 pulseeffects
 xsettingsd
 discord-rich-presence
 polkit-gnome
 vpn
 davmail
 xmodmap)
```

Daemonize shepherd

```scheme
(action 'shepherd 'daemonize)
```

Run services

```scheme
(for-each start '(mpd mpd-watcher mcron aw-server aw-watcher-afk aw-watcher-window pulseeffects xsettingsd discord-rich-presence polkit-gnome davmail xmodmap))
```


### Sync {#sync}

| Guix dependency |
|-----------------|
| megacmd-1.4     |


## Guix settings {#guix-settings}

Other desktop programs I use are listed below.

| Guix dependency        | Description                               |
|------------------------|-------------------------------------------|
| xprop                  | Tool to display properties of X windows   |
| arandr                 | GUI to xrandr                             |
| light                  | Control screen brightness                 |
| ponymix                | Control PulseAudio CLI                    |
| pavucontrol            | Control PulseAudio GUI                    |
| network-manager-applet | Applet to manage network connections      |
| feh                    | Image viewer. Used to set background      |
| copyq                  | Clipboard manager                         |
| xmodmap                | Program to modify keybindings on X server |
| thunar                 | My preferred GUI file manager             |
| keepassxc              | My preferred password manager             |
| telegram-desktop       | telegram client                           |
| xdg-utils              | gives xdg-open and stuff                  |
| gnome-font-viewer      | view fonts                                |
| qbittorrent            | torrent client                            |
| fontconfig             |                                           |
| polkit-gnome           | Polkit authentication agent               |
| anydesk                | Remote desktop software                   |
| gnome-disk-utility     | Manage disks                              |
| gparted                | Manage partitions                         |

<a id="code-snippet--packages"></a>
```emacs-lisp
(my/format-guix-dependencies)
```

```scheme
(specifications->manifest
 '(
   <<packages()>>))
```