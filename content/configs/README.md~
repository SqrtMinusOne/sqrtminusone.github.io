+++
title = "My dotfiles"
author = ["Pavel"]
aliases = ["/config"]
draft = false
+++

{{< figure src="https://forthebadge.com/images/badges/works-on-my-machine.svg" >}}

A set of my GNU/Linux configuration files. [View at GitHub](https://github.com/SqrtMinusOne/dotfiles).

The majority of the software is configured with [literate configuration](https://leanpub.com/lit-config/read) strategy via Emacs' Org Mode. This way has its advantages and disadvantages, but overall it's pretty nice to keep the configs interweaved with comments in a handful of files.

The files themselves are managed and deployed via [yadm](https://yadm.io/), but I mostly use Org Mode rich noweb whenever I can instead of what yadm offers.

My current GNU/Linux distribution is [GNU Guix](https://guix.gnu.org/). In the context of this repo, Guix allows me to list all the used programs in manifests, which means I have the same set of programs across multiple machines. Look for tables with "Guix dependency" in the header.

Literate configuration files:

-   [Emacs.org]({{< relref "Emacs" >}})
-   [Desktop.org]({{< relref "Desktop" >}})
-   [Console.org]({{< relref "Console" >}})
-   [Guix.org]({{< relref "Guix" >}})
-   [Mail.org]({{< relref "Mail" >}})


## Programs used {#programs-used}

Some of the notable programs are listed in the table below.

| Group     | Program                                                 | Purpose                     | Status           | Documented?                                                                                 | Notes                                                     |
|-----------|---------------------------------------------------------|-----------------------------|------------------|---------------------------------------------------------------------------------------------|-----------------------------------------------------------|
| console   | bash                                                    | shell                       | launches fish :) | [Console.org]({{< relref "Console" >}})                                                     |                                                           |
| console   | [fish](https://fishshell.com/)                          | shell                       | **active**       | [Console.org]({{< relref "Console" >}})                                                     |                                                           |
| console   | [starship](https://github.com/starship/starship)        | prompt                      | **active**       | [Console.org]({{< relref "Console" >}})                                                     |                                                           |
| console   | [tmux](https://github.com/tmux/tmux)                    | terminal multiplexer        | **active**       | [Console.org]({{< relref "Console" >}})                                                     |                                                           |
| console   | [alacritty](https://github.com/alacritty/alacritty)     | terminal emulator           | **active**       | [Console.org]({{< relref "Console" >}})                                                     |                                                           |
| mail      | [notmuch](https://notmuchmail.org/)                     | mail indexer                | **active**       | [Mail.org,]({{< relref "Mail" >}}) [post](https://sqrtminusone.xyz/posts/2021-02-27-gmail/) |                                                           |
| mail      | [lieer](https://github.com/gauteh/lieer)                | gmail API client            | **active**       | [Mail.org]({{< relref "Mail" >}}), [post](https://sqrtminusone.xyz/posts/2021-02-27-gmail/) | credentials are encrypted                                 |
| mail      | [msmtp](https://marlam.de/msmtp/)                       | SMTP client                 | **active**       | [Mail.org]({{< relref "Mail" >}})                                                           |                                                           |
| editor    | [emacs](https://www.gnu.org/software/emacs/)            | everything                  | **active**       | [Emacs.org]({{< relref "Emacs" >}})                                                         | GitHub renders .org files without labels and `tangle: no` |
| editor    | [vim](https://www.vim.org/)                             | text edtior                 | **active**       | -                                                                                           | A minimal config to have a lightweight terminal $EDITOR   |
| editor    | [neovim](https://neovim.io/)                            | text edtior                 | archive          | -                                                                                           |                                                           |
| documents | [latexmk](https://mg.readthedocs.io/latexmk.html)       | LaTeX build tool            | **active**       | -                                                                                           |                                                           |
| documents | [zathura](https://pwmt.org/projects/zathura/)           | pdf viewer                  | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [dunst](https://github.com/dunst-project/dunst)         | notification manager        | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [i3wm](https://i3wm.org/)                               | tiling WM                   | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [keynav](https://github.com/jordansissel/keynav)        | control mouse with keyboard | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [polybar](https://github.com/polybar/polybar)           | status bar                  | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [rofi](https://github.com/davatorium/rofi)              | generic menu                | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [flameshot](https://github.com/flameshot-org/flameshot) | screenshot                  | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [picom](https://github.com/yshui/picom)                 | X11 compositor              | **active**       | [Desktop.org]({{< relref "Desktop" >}})                                                     |                                                           |
| desktop   | [i3blocks](https://github.com/vivien/i3blocks)          | status bar                  | archive          | -                                                                                           |                                                           |
| internet  | [tridactyl](https://github.com/tridactyl/tridactyl)     | vim bindings for Firefox    | **active**       | -                                                                                           | templated with yadm                                       |
| internet  | [newsboat](https://newsboat.org/)                       | terminal RSS reader         | archive          | -                                                                                           | urls are encrypted                                        |
| internet  | [qutebrowser](https://qutebrowser.org/)                 | browser with vim bindings   | archive          | -                                                                                           |                                                           |
| internet  | [buku](https://github.com/jarun/buku)                   | bookmarks manager           | archive          | -                                                                                           |                                                           |
| audio     | [mpd](https://www.musicpd.org/)                         | music player daemon         | **active**       | -                                                                                           |                                                           |
| audio     | [ncmpcpp](https://github.com/ncmpcpp/ncmpcpp)           | MPD frontend                | **active**       | -                                                                                           |                                                           |
| misc      | [yadm](https://yadm.io)                                 | dotfiles manager            | **active**       | -                                                                                           |                                                           |
| misc      | [sunwait](https://github.com/risacher/sunwait)          | sunrise calculator          | **active**       | -                                                                                           |                                                           |
| misc      | [vnstat](https://github.com/vergoh/vnstat)              | traffic stats               | **active**       | -                                                                                           |                                                           |


## Posts about my configuration {#posts-about-my-configuration}

-   [Replacing Jupyter Notebook with Org Mode](https://sqrtminusone.xyz/posts/2021-05-01-org-python/)
-   [Multiple Gmail accounts & labels with Emacs](https://sqrtminusone.xyz/posts/2021-02-27-gmail/)


## Some statistics {#some-statistics}

If you are viewing the file in Emacs, eval the following to show the pictures with reasonable width:

```elisp
(setq-local org-image-actual-width '(1024))
```


### History {#history}

{{< figure src="/ox-hugo/all.png" >}}

{{< figure src="/ox-hugo/emacs-vim.png" >}}

{{< figure src="/ox-hugo/literate-config.png" >}}


## Misc {#misc}


### Notes {#notes}

-   `M-u C-c C-v t` to tangle a particular block
-   `M-u M-u C-c C-v t` to tangle a particular file
-   `C-c C-v d` to demarcate a block

Uses yadm's `post_alt` hook to create symlinks


### Encrypted files {#encrypted-files}

```text
.config/newsboat/urls
.config/filezilla/sitemanager.xml
.config/filezilla/filezilla.xml
Mail/thexcloud/.credentials.gmailieer.json
Mail/progin6304/.credentials.gmailieer.json
.emacs.d/dired-bookmarks.el
.emacs.d/elfeed.org
.emacs.d/private.org
.emacs.d/prodigy-config.el
.emacs.d/private.el
```