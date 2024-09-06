+++
title = "My Emacs packages"
author = ["Pavel Korytov"]
draft = false
norss = true
+++

Following is the list of Emacs packages I authored.

See also [the same list with timestamps](/packages/), and the corresponding [RSS feed](/packages/index.xml). This page is somewhat inspired by <https://protesilaos.com/emacs/>, thanks Prot.


## eshell-atuin {#eshell-atuin}

Integrate [eshell](https://www.gnu.org/software/emacs/manual/html_mono/eshell.html) with [atuin](https://github.com/atuinsh/atuin).

`atuin` stores shell history in a database, which allows for having the same history across multiple shells, sessions, and optionally across different machines. See the project page for the complete list of features.

This package provides functionality to store and browse eshell history in `atuin`.

-   Package name: `eshell-atuin`
-   Homepage: <https://sqrtminusone.xyz/packages/eshell-atuin>
-   Git repo: <https://github.com/SqrtMinusOne/eshell-atuin>
-   Package at MELPA: <https://melpa.org/#/eshell-atuin>


## org-clock-agg {#org-clock-agg}

Aggregate [org-clock](https://orgmode.org/manual/Clocking-Work-Time.html) records and display the results in a tree form.

-   Package name: `org-clock-agg`
-   Homepage: <https://sqrtminusone.xyz/packages/org-clock-agg>
-   Git repo: <https://github.com/SqrtMinusOne/org-clock-agg>


## BIOME {#biome}

Emacs client for [Open Meteo](https://open-meteo.com/), an open-source weather API.

The package provides a bunch of [transients](https://github.com/magit/transient/) to query the service and displays the results with formatted tables.

-   Package name: `biome`
-   Homepage: <https://sqrtminusone.xyz/packages/biome>
-   Git repo: <https://github.com/SqrtMinusOne/biome>
-   Package at MELPA: <https://melpa.org/#/biome>
-   Backronym: Boutiful Interface to Open Meteo for Emacs


## micromamba.el {#micromamba-dot-el}

Emacs package for working with [micromamba](https://mamba.readthedocs.io/en/latest/user_guide/micromamba.html) environments.

Essentially, it provides a subset of the functionality of [conda.el](https://github.com/necaris/conda.el), adapted for `micromamba` because interfaces of the two aren't quite compatible.

-   Package name: `micromamba`
-   Homepage: <https://sqrtminusone.xyz/packages/micromamba>
-   Git repo: <https://github.com/SqrtMinusOne/micromamba.el>
-   Package at MELPA: <https://melpa.org/#/micromamba>


## reverso.el {#reverso-dot-el}

A client for a service called [Reverso](https://www.reverso.net/); supports translation, grammar check, context and synonyms search.

The package uses a reverse-engineered API, but it's been surprisingly stable.

-   Package name: `reverso`
-   Homepage: <https://sqrtminusone.xyz/packages/reverso>
-   Git repo: <https://github.com/SqrtMinusOne/reverso.el>
-   Package at MELPA: <https://melpa.org/#/reverso>


## Elfeed Sync {#elfeed-sync}

Sync [elfeed](https://github.com/skeeto/elfeed) and [elfeed-summary](https://github.com/SqrtMinusOne/elfeed-summary) with [Tiny Tiny RSS](https://tt-rss.org/).

This syncs the statuses of entries (read/unread) and, optionally, the feed tree of elfeed-summary. The latter is the key difference from [elfeed-protocol](https://github.com/fasheng/elfeed-protocol).

-   Package name: `elfeed-sync`
-   Homepage: <https://sqrtminusone.xyz/packages/elfeed-sync>
-   Git repo: <https://github.com/SqrtMinusOne/elfeed-sync>


## avy-dired {#avy-dired}

My experiment with reducing the number of keystrokes required for filesystem navigation to an absolute minimum, with [avy](https://github.com/abo-abo/avy) and [dired](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html).

This is a bit flaky and will probably remain so because somehow I found that way of navigation cognitively harder than evil motions with [dired-narrow](https://github.com/Fuco1/dired-hacks/blob/master/dired-narrow.el). But it works.

-   Status: _experimental_
-   Package name: `avy-dired`
-   Homepage: <https://sqrtminusone.xyz/packages/avy-dired>
-   Git repo: <https://github.com/SqrtMinusOne/avy-dired>


## Elfeed Summary {#elfeed-summary}

An alternative tree-based interface for [elfeed](https://github.com/skeeto/elfeed).

-   Package name: `elfeed-summary`
-   Homepage: <https://sqrtminusone.xyz/packages/elfeed-summary>
-   Git repo: <https://github.com/SqrtMinusOne/elfeed-summary>
-   Package at MELPA: <https://melpa.org/#/elfeed-summary>


## password-store-completion {#password-store-completion}

Auto-type fields from [pass](https://www.passwordstore.org/) entries. Essentially, this is a reimplementation of [rofi-pass](https://github.com/carnager/rofi-pass) with `completing-read` which I made after migrating to [EXWM](https://github.com/ch11ng/exwm).

It was called `password-store-ivy` before, but I've changed the name after I switched to vertico and added `completing-read` (+ embark integration) to the package.

-   Package name: `password-store-completion`
-   Homepage: <https://sqrtminusone.xyz/packages/password-store-completion>
-   Git repo: <https://github.com/SqrtMinusOne/password-store-completion>


## Org Journal Tags {#org-journal-tags}

The package extends [org-journal](https://github.com/bastibe/org-journal) by introducing "tags" that reference a journal section or subset thereof. These tags can be later queried. The package also provides a UI with statistics about tags and things like records on this day a year ago.

-   Package name: `org-journal-tags`
-   Homepage: <https://sqrtminusone.xyz/packages/org-journal-tags>
-   Git repo: <https://github.com/SqrtMinusOne/org-journal-tags>
-   Package at MELPA: <https://melpa.org/#/org-journal-tags>


## EXWM Modeline {#exwm-modeline}

Display EXWM workspaces in the modeline. Supports multiple monitors.

-   Package name: `exwm-modeline`
-   Homepage: <https://sqrtminusone.xyz/packages/exwm-modeline>
-   Git repo: <https://github.com/SqrtMinusOne/exwm-modeline>
-   Package at MELPA: <https://melpa.org/#/exwm-modeline>


## perspective-exwm {#perspective-exwm}

A bunch of hacks &amp; functions that make [perspective.el](https://github.com/nex3/perspective-el) play better with [EXWM](https://github.com/ch11ng/exwm).

The package advices away certain weird interactions between EXWM frames and perspectives, such as a chance to break the current perspective when killing a floating frame. Both EXWM and perspective poke into the same domain of unconventional management of buffers and frames, so such issues arise.

I'm uncertain how many of these issues are reproducible outside my config or workflow, because I saw people using perspective+EXWM before I had written this package. But these were blockers for my adoption of EXWM, at the very least.

-   Homepage: <https://sqrtminusone.xyz/packages/perspective-exwm>
-   Git repo: <https://github.com/SqrtMinusOne/perspective-exwm.el>
-   Package at MELPA: <https://melpa.org/#/perspective-exwm>


## pomm {#pomm}

Implementation of [Pomodoro](https://en.wikipedia.org/wiki/Pomodoro_Technique) and [Third Time](https://www.lesswrong.com/posts/RWu8eZqbwgB9zaerh/third-time-a-better-way-to-work) methods for Emacs.

-   Package name: `pomm`
-   Homepage: <https://sqrtminusone.xyz/packages/pomm>
-   Git repo: <https://github.com/SqrtMinusOne/pomm.el>
-   Package at MELPA: <https://melpa.org/#/pomm>
-   Backronym: Perfectly Optimized Management Methods


## Lyrics Fetcher {#lyrics-fetcher}

Retrieve and display song lyrics from [genius.com](https://genius.com/) and [music.163.com](https://music.163.com/) (thanks [Eli](https://github.com/Elilif) for the latter). The package is primarily designed for use with [EMMS](https://www.gnu.org/software/emms/), for instance, to display lyrics for the current song or for one at point in the EMMS browser. However, it can be used independently.

Also, this is my first Emacs package.

-   Package name: `lyrics-fetcher`
-   Homepage: <https://sqrtminusone.xyz/packages/lyrics-fetcher>
-   Git repo: <https://github.com/SqrtMinusOne/lyrics-fetcher.el>
-   Package at MELPA: <https://melpa.org/#/lyrics-fetcher>
