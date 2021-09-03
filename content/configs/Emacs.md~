+++
title = "Emacs config"
author = ["Pavel"]
draft = false
+++

> One day we won't hate one another, no young boy will march to war and I will clean up my Emacs config. But that day isn't today.

My [Emacs](https://www.gnu.org/software/emacs/) configuration.

As with other files in the repo, parts prefixed with (OFF) are not used but kept for historic purposes.

<div class="ox-hugo-toc toc">
<div></div>

<div class="heading">Table of Contents</div>

- [Primary setup](#primary-setup)
    - [Measure startup speed](#measure-startup-speed)
    - [straight.el](#straight-dot-el)
    - [use-package](#use-package)
    - [Performance](#performance)
        - [Garbage collection](#garbage-collection)
        - [Run garbage collection when Emacs is unfocused](#run-garbage-collection-when-emacs-is-unfocused)
        - [Misc](#misc)
        - [Native compilation](#native-compilation)
    - [Anaconda & environment](#anaconda-and-environment)
    - [Custom file location](#custom-file-location)
    - [Private config](#private-config)
    - [No littering](#no-littering)
- [Global editing configuration](#global-editing-configuration)
    - [General keybindings stuff](#general-keybindings-stuff)
        - [general.el](#general-dot-el)
        - [which-key](#which-key)
            - [dump keybindings](#dump-keybindings)
    - [Evil mode](#evil-mode)
        - [evil](#evil)
        - [Addons](#addons)
        - [evil-collection](#evil-collection)
    - [More keybindings](#more-keybindings)
        - [Escape key](#escape-key)
        - [Home & end](#home-and-end)
        - [My leader](#my-leader)
        - [Universal argument](#universal-argument)
        - [Profiler](#profiler)
        - [Buffer switching](#buffer-switching)
        - [Buffer management](#buffer-management)
        - [xref](#xref)
        - [Folding](#folding)
        - [Zoom](#zoom)
    - [Editing helpers](#editing-helpers)
        - [Visual fill column mode](#visual-fill-column-mode)
        - [smartparens](#smartparens)
        - [Aggressive Indent](#aggressive-indent)
        - [Delete trailing whitespace](#delete-trailing-whitespace)
        - [Expand region](#expand-region)
    - [Various settings](#various-settings)
        - [Tabs](#tabs)
        - [Scrolling config](#scrolling-config)
        - [Clipboard](#clipboard)
        - [Backups](#backups)
    - [Undo Tree](#undo-tree)
    - [Help](#help)
    - [Ivy, counsel, swiper](#ivy-counsel-swiper)
        - [ivy-rich](#ivy-rich)
        - [prescient](#prescient)
        - [Keybindings](#keybindings)
    - [<span class="org-todo done OFF">OFF</span> (OFF) Helm](#off--helm)
    - [Treemacs](#treemacs)
        - [Helper functions](#helper-functions)
        - [Custom icons](#custom-icons)
    - [Projectile](#projectile)
    - [Company](#company)
    - [Git & Magit](#git-and-magit)
    - [Editorconfig](#editorconfig)
    - [Snippets](#snippets)
    - [Time trackers](#time-trackers)
        - [WakaTime](#wakatime)
        - [ActivityWatch](#activitywatch)
- [UI](#ui)
    - [General UI & GUI Settings](#general-ui-and-gui-settings)
    - [Theme & global stuff](#theme-and-global-stuff)
        - [Custom theme](#custom-theme)
        - [Font](#font)
    - [Custom frame title](#custom-frame-title)
    - [perspective.el](#perspective-dot-el)
        - [Some functions](#some-functions)
    - [<span class="org-todo done OFF">OFF</span> (OFF) Tab bar](#off--tab-bar)
        - [Setup](#setup)
        - [My title](#my-title)
    - [Modeline](#modeline)
    - [Font stuff](#font-stuff)
        - [Emojis](#emojis)
        - [Ligatures](#ligatures)
        - [Icons](#icons)
        - [Highlight todo](#highlight-todo)
    - [Text highlight improvements](#text-highlight-improvements)
- [Dired](#dired)
    - [Basic config & keybindings](#basic-config-and-keybindings)
    - [Addons](#addons)
    - [Subdirectories](#subdirectories)
    - [TRAMP](#tramp)
    - [Bookmarks](#bookmarks)
- [Shells](#shells)
    - [vterm](#vterm)
        - [Configuration](#configuration)
        - [Subterminal](#subterminal)
        - [Dired integration](#dired-integration)
    - [Eshell](#eshell)
- [Org Mode](#org-mode)
    - [Installation & basic settings](#installation-and-basic-settings)
        - [Encryption](#encryption)
        - [org-contrib](#org-contrib)
    - [Integration with evil](#integration-with-evil)
    - [Literate programing](#literate-programing)
        - [Python & Jupyter](#python-and-jupyter)
        - [Hy](#hy)
        - [View HTML in browser](#view-html-in-browser)
        - [PlantUML](#plantuml)
        - [Setup](#setup)
        - [Managing Jupyter kernels](#managing-jupyter-kernels)
        - [Do not wrap the output in emacs-jupyter](#do-not-wrap-the-output-in-emacs-jupyter)
        - [Wrap source code output](#wrap-source-code-output)
    - [Productivity & Knowledge management](#productivity-and-knowledge-management)
        - [Capture templates & various settings](#capture-templates-and-various-settings)
        - [Custom agendas](#custom-agendas)
        - [Org Journal](#org-journal)
        - [Org Roam](#org-roam)
            - [org-roam-ui](#org-roam-ui)
            - [org-roam-protocol](#org-roam-protocol)
        - [org-ref](#org-ref)
        - [org-roam-bibtex](#org-roam-bibtex)
    - [UI](#ui)
        - [<span class="org-todo done OFF">OFF</span> (OFF) Instant equations preview](#off--instant-equations-preview)
        - [LaTeX fragments](#latex-fragments)
        - [Better headers](#better-headers)
        - [Org Agenda Icons](#org-agenda-icons)
    - [Export](#export)
        - [General settings](#general-settings)
        - [Hugo](#hugo)
        - [Jupyter Notebook](#jupyter-notebook)
        - [Html export](#html-export)
        - [LaTeX](#latex)
    - [Keybindings & stuff](#keybindings-and-stuff)
        - [Copy a link](#copy-a-link)
    - [Presentations](#presentations)
    - [TOC](#toc)
    - [System configuration](#system-configuration)
        - [Tables for Guix Dependencies](#tables-for-guix-dependencies)
        - [Noweb evaluations](#noweb-evaluations)
        - [yadm hook](#yadm-hook)
- [<span class="org-todo done OFF">OFF</span> (OFF) EAF](#off--eaf)
    - [Installation](#installation)
    - [Config](#config)
- [Programming](#programming)
    - [General setup](#general-setup)
        - [LSP](#lsp)
            - [Setup](#setup)
            - [Integrations](#integrations)
            - [Keybindings](#keybindings)
        - [Flycheck](#flycheck)
        - [Tree Sitter](#tree-sitter)
        - [<span class="org-todo done OFF">OFF</span> (OFF) DAP](#off--dap)
        - [<span class="org-todo done OFF">OFF</span> (OFF) TabNine](#off--tabnine)
        - [<span class="org-todo done OFF">OFF</span> (OFF) Code Compass](#off--code-compass)
            - [Dependencies](#dependencies)
            - [Plugin](#plugin)
        - [<span class="org-todo todo CHECK">CHECK</span> (OFF) Format-all](#off--format-all)
        - [General additional config](#general-additional-config)
    - [Web development](#web-development)
        - [Emmet](#emmet)
        - [Prettier](#prettier)
        - [TypeScript](#typescript)
        - [JavaScript](#javascript)
        - [Jest](#jest)
        - [web-mode](#web-mode)
        - [<span class="org-todo done OFF">OFF</span> (OFF) Vue.js](#off--vue-dot-js)
            - [mmm-mode fix](#mmm-mode-fix)
        - [<span class="org-todo done OFF">OFF</span> (OFF) Svelte](#off--svelte)
        - [SCSS](#scss)
        - [PHP](#php)
    - [LaTeX](#latex)
        - [AUCTeX](#auctex)
        - [BibTeX](#bibtex)
        - [Import \*.sty](#import-dot-sty)
        - [Snippets](#snippets)
            - [Greek letters](#greek-letters)
            - [English letters](#english-letters)
            - [Math symbols](#math-symbols)
            - [Section snippets](#section-snippets)
    - [Other markup languages](#other-markup-languages)
        - [Markdown](#markdown)
        - [PlantUML](#plantuml)
        - [LanguageTool](#languagetool)
    - [Lisp](#lisp)
        - [Meta Lisp](#meta-lisp)
        - [Emacs Lisp](#emacs-lisp)
            - [Package Lint](#package-lint)
            - [General](#general)
        - [Common lisp](#common-lisp)
        - [Clojure](#clojure)
        - [Hy](#hy)
        - [Scheme](#scheme)
        - [CLIPS](#clips)
    - [Python](#python)
        - [pipenv](#pipenv)
        - [yapf](#yapf)
        - [isort](#isort)
        - [sphinx-doc](#sphinx-doc)
        - [pytest](#pytest)
            - [Fix comint buffer width](#fix-comint-buffer-width)
        - [code-cells](#code-cells)
        - [tensorboard](#tensorboard)
    - [Java](#java)
    - [Go](#go)
    - [.NET](#dot-net)
        - [C#](#c)
        - [MSBuild](#msbuild)
    - [fish](#fish)
    - [sh](#sh)
    - [Haskell](#haskell)
    - [JSON](#json)
    - [YAML](#yaml)
    - [.env](#dot-env)
    - [CSV](#csv)
    - [<span class="org-todo done OFF">OFF</span> (OFF) PDF](#off--pdf)
    - [Docker](#docker)
- [Apps & Misc](#apps-and-misc)
    - [Managing dotfiles](#managing-dotfiles)
        - [Open Emacs config](#open-emacs-config)
        - [Open Magit for yadm](#open-magit-for-yadm)
        - [Open a dotfile](#open-a-dotfile)
    - [Internet & Multimedia](#internet-and-multimedia)
        - [Notmuch](#notmuch)
        - [Elfeed](#elfeed)
            - [Some additions](#some-additions)
            - [YouTube](#youtube)
        - [EMMS](#emms)
            - [MPD](#mpd)
            - [MPV](#mpv)
            - [Cache cleanup](#cache-cleanup)
            - [Fetching lyrics](#fetching-lyrics)
            - [Some keybindings](#some-keybindings)
            - [EMMS & mpd Fixes](#emms-and-mpd-fixes)
        - [EWW](#eww)
        - [ERC](#erc)
        - [Google Translate](#google-translate)
    - [Reading documentation](#reading-documentation)
        - [tldr](#tldr)
        - [man & info](#man-and-info)
        - [devdocs.io](#devdocs-dot-io)
    - [Utilities](#utilities)
        - [pass](#pass)
        - [Docker](#docker)
        - [Progidy](#progidy)
        - [screenshot.el](#screenshot-dot-el)
        - [proced](#proced)
        - [Guix](#guix)
    - [Productivity](#productivity)
        - [Pomidor](#pomidor)
        - [Calendar](#calendar)
    - [Fun](#fun)
        - [Discord integration](#discord-integration)
        - [Snow](#snow)
        - [Zone](#zone)
- [Guix settings](#guix-settings)

</div>
<!--endtoc-->


## Primary setup {#primary-setup}


### Measure startup speed {#measure-startup-speed}

A small function to print out the loading time and number of GCs during the loading. Can be useful as a point of data for optimizing Emacs startup time.

```emacs-lisp
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; (setq use-package-verbose t)
```


### straight.el {#straight-dot-el}

Straight.el is my Emacs package manager of choice. Its advantages & disadvantages over other options are listed pretty thoroughly in the README file in the repo.

The following is a straight.el bootstrap script.

References:

-   [straight.el repo](https://github.com/raxod502/straight.el)

<!--listend-->

```emacs-lisp
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
```


### use-package {#use-package}

A macro to simplify package specification & configuration. Integrates with straight.el.

Set `use-package-verbose` to `t` to print out individual package loading time.

References:

-   [use-package repo](https://github.com/jwiegley/use-package)

<!--listend-->

```emacs-lisp
(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))
 ;; (setq use-package-verbose t)
```


### Performance {#performance}


#### Garbage collection {#garbage-collection}

Just setting `gc-cons-treshold` to a larger value.

```emacs-lisp
(setq gc-cons-threshold 80000000)
(setq read-process-output-max (* 1024 1024))
```


#### Run garbage collection when Emacs is unfocused {#run-garbage-collection-when-emacs-is-unfocused}

Run GC when Emacs loses focus. ~~Time will tell if that's a good idea.~~

Some time has passed, and I still don't know if there is any quantifiable advantage to this, but it doesn't hurt.

```emacs-lisp
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (if (boundp 'after-focus-change-function)
		(add-function :after after-focus-change-function
			      (lambda ()
				(unless (frame-focus-state)
				  (garbage-collect))))
	      (add-hook 'after-focus-change-function 'garbage-collect))))
```


#### Misc {#misc}

The following variable is true when my machine is not powerful enough for some resource-heavy packages.

```emacs-lisp
(setq my/lowpower (string= (system-name) "azure"))
```

The following is true if Emacs is meant to be used with TRAMP over slow ssh.

```emacs-lisp
(setq my/slow-ssh (string= (getenv "IS_TRAMP") "true"))
```

And the following is true if Emacs is run from termux on Android.

```emacs-lisp
(setq my/is-termux (string-match-p (rx (* nonl) "com.termux" (* nonl)) (getenv "HOME")))
```


#### Native compilation {#native-compilation}

Set number of jobs to 1 on low-power machines

```emacs-lisp
(when my/lowpower
  (setq comp-async-jobs-number 1))
```


### Anaconda & environment {#anaconda-and-environment}

[Anaconda](https://www.anaconda.com/) is a free package and environment manager. I currently use it to manage multiple versions of Python and Node.js

The following code uses the conda package to activate the base environment on startup if Emacs is launched outside the environment.

Also, some strange things are happening if vterm is launched with conda activated from Emacs, so I advise `conda-env-activate` to set an auxililary environment variable.

References:

-   [Anaconda docs](https://docs.anaconda.com/)
-   [conda.el repo](https://github.com/necaris/conda.el)

<!--listend-->

```emacs-lisp
(use-package conda
  :straight t
  :if (executable-find "conda")
  :config
  (setq conda-anaconda-home (string-replace "/bin/conda" "" (executable-find "conda")))
  (setq conda-env-home-directory (expand-file-name "~/.conda/"))
  (setq conda-env-subdirectory "envs")
  (setenv "INIT_CONDA" "true")
  (advice-add 'conda-env-activate :after
	      (lambda (&rest _) (setenv "EMACS_CONDA_ENV" conda-env-current-name)))
  (unless (getenv "CONDA_DEFAULT_ENV")
    (conda-env-activate "general")))
```

Also, I sometimes need to know if a program is running inside Emacs (say, inside a terminal emulator). To do that, I set the following environment variable:

```emacs-lisp
(setenv "IS_EMACS" "true")
```


### Custom file location {#custom-file-location}

By default, custom writes stuff to `init.el`, which is somewhat annoying. The following makes a separate file `custom.el`

```emacs-lisp
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
```


### Private config {#private-config}

I have some variables which I don't commit to the repo, e.g. my current location. They are stored in `private.el`

```emacs-lisp
(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))
```


### No littering {#no-littering}

By default emacs and its packages create a lot files in `.emacs.d` and in other places. [no-littering](https://github.com/emacscollective/no-littering) is a collective effort to redirect all of this to two folders in `user-emacs-directory`.

```emacs-lisp
(use-package no-littering
  :straight t)
```


## Global editing configuration {#global-editing-configuration}


### General keybindings stuff {#general-keybindings-stuff}


#### general.el {#general-dot-el}

general.el provides a convenient interface to manage Emacs keybindings.

References:

-   [general.el repo](https://github.com/noctuid/general.el)

<!--listend-->

```emacs-lisp
(use-package general
  :straight t
  :config
  (general-evil-setup))
```


#### which-key {#which-key}

A package that displays the available keybindings in a popup.

Pretty useful, as Emacs seems to have more keybindings than I can remember at any given point.

References:

-   [which-key repo](https://github.com/justbur/emacs-which-key)

<!--listend-->

```emacs-lisp
(use-package which-key
  :config
  (setq which-key-idle-delay (if my/lowpower 1 0.3))
  (setq which-key-popup-type 'frame)
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (set-face-attribute 'which-key-local-map-description-face nil
		      :weight 'bold)
  :straight t)
```


##### dump keybindings {#dump-keybindings}

A function to dump keybindings starting with a prefix to a buffer in tree-like form.

```emacs-lisp
(defun my/dump-bindings-recursive (prefix &optional level)
  (dolist (key (which-key--get-bindings (kbd prefix)))
    (when level
      (insert (make-string level ? )))
    (insert (apply #'format "%s%s%s\n" key))
    (when (string-match-p
	   (rx bos "+" (* nonl))
	   (substring-no-properties (elt key 2)))
      (my/dump-bindings-recursive
       (concat prefix " " (substring-no-properties (car key)))
       (+ 2 (or level 0))))))

(defun my/dump-bindings (prefix)
  "Dump keybindings starting with PREFIX in tree-like form."
  (interactive "sPrefix: ")
  (with-current-buffer (get-buffer-create "bindings")
    (point-max)
    (erase-buffer)
    (save-excursion
      (my/dump-bindings-recursive prefix)))
  (switch-to-buffer-other-window "bindings"))
```


### Evil mode {#evil-mode}

A whole ecosystem of packages that emulates the main features of Vim. Probably the best vim emulator out there.

The only problem is that the package name makes it hard to google anything by just typing "evil".

References:

-   [evil repo](https://github.com/emacs-evil/evil)
-   [(YouTube) Evil Mode: Or, How I Learned to Stop Worrying and Love Emacs](https://www.youtube.com/watch?v=JWD1Fpdd4Pc)


#### evil {#evil}

Basic evil configuration.

```emacs-lisp
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-search-module 'evil-search)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  ;; (setq evil-respect-visual-line-mode t)
  (evil-set-undo-system 'undo-tree)
  ;; (add-to-list 'evil-emacs-state-modes 'dired-mode)
  )
```


#### Addons {#addons}

[evil-surround](https://github.com/emacs-evil/evil-surround) emulates one of my favorite vim plugins, surround.vim. Adds a lot of parentheses management options.

```emacs-lisp
(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))
```

[evil-commentary](https://github.com/linktohack/evil-commentary) emulates commentary.vim.

```emacs-lisp
(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))
```

[evil-quickscope](https://github.com/blorbx/evil-quickscope) emulates quickscope.vim. It highlights the important target characters for f, F, t, T keys.

```emacs-lisp
(use-package evil-quickscope
  :straight t
  :after evil
  :config
  :hook ((prog-mode . turn-on-evil-quickscope-mode)
	 (LaTeX-mode . turn-on-evil-quickscope-mode)
	 (org-mode . turn-on-evil-quickscope-mode)))
```

[evil-numbers](https://github.com/cofi/evil-numbers) allows incrementing and decrementing numbers at the point.

```emacs-lisp
(use-package evil-numbers
  :straight t
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (general-nmap
    "g+" 'evil-numbers/inc-at-pt
    "g-" 'evil-numbers/dec-at-pt))
```

[evil-lion](https://github.com/edkolev/evil-lion) provides alignment operators, somewhat similar to vim-easyalign.

```emacs-lisp
(use-package evil-lion
  :straight t
  :config
  (setq evil-lion-left-align-key (kbd "g a"))
  (setq evil-lion-right-align-key (kbd "g A"))
  (evil-lion-mode))
```

[evil-matchit](https://github.com/redguardtoo/evil-matchit) makes "%" to match things like tags.

```emacs-lisp
(use-package evil-matchit
  :straight t
  :config
  (global-evil-matchit-mode 1))
```


#### evil-collection {#evil-collection}

[evil-collection](https://github.com/emacs-evil/evil-collection) is a package that provides evil bindings for a lot of different packages. One can see the whole list in the [modes](https://github.com/emacs-evil/evil-collection/tree/master/modes) folder.

I don't enable the entire package, just the modes I need.

```emacs-lisp
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init
   '(eww
     devdocs
     proced
     emms
     pass
     calendar
     dired
     debug
     guix
     calc
     docker
     ibuffer
     geiser
     pdf
     info
     elfeed
     edebug
     bookmark
     company
     vterm
     flycheck
     profiler
     cider
     explain-pause-mode
     notmuch
     custom
     xref
     eshell
     helpful
     compile
     comint
     git-timemachine
     magit
     prodigy)))
```


### More keybindings {#more-keybindings}

The main keybindings setup is positioned after evil mode to take the latter into account.


#### Escape key {#escape-key}

Use the escape key instead of `C-g` whenever possible.

I must have copied it from somewhere, but as I googled to find out the source, I discovered quite a number of variations of the following code over time. I wonder if Richard Dawkins was inspired by something like this a few decades ago.

```emacs-lisp
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(general-define-key
 :keymaps '(normal visual global)
 [escape] 'keyboard-quit)

(general-define-key
 :keymaps '(minibuffer-local-map
	    minibuffer-local-ns-map
	    minibuffer-local-completion-map
	    minibuffer-local-must-match-map
	    minibuffer-local-isearch-map)
 [escape] 'minibuffer-keyboard-quit)
```


#### Home & end {#home-and-end}

```emacs-lisp
(general-def :states '(normal insert visual)
  "<home>" 'beginning-of-line
  "<end>" 'end-of-line)
```


#### My leader {#my-leader}

Using the `SPC` key as a leader key, like in Doom Emacs or Spacemacs.

```emacs-lisp
(general-create-definer my-leader-def
  :keymaps 'override
  :prefix "SPC"
  :states '(normal motion emacs))

(general-def :states '(normal motion emacs) "SPC" nil)

(my-leader-def "?" 'which-key-show-top-level)
(my-leader-def "E" 'eval-expression)
```

`general.el` has a nice integration with which-key, so I use this fact to show more descriptive annotations for certain groups of keybindings (the default one is `prefix`).

```emacs-lisp
(my-leader-def
  "a" '(:which-key "apps"))
```


#### Universal argument {#universal-argument}

Change the universal argument to `M-u`. I use `C-u` to scroll up, as I'm used to from vim.

```emacs-lisp
(general-def
  :keymaps 'universal-argument-map
  "M-u" 'universal-argument-more)
(general-def
  :keymaps 'override
  :states '(normal motion emacs insert visual)
  "M-u" 'universal-argument)
```


#### Profiler {#profiler}

The built-in profiler is a magnificent tool to troubleshoot performance issues.

```emacs-lisp
(my-leader-def
  :infix "P"
  "" '(:which-key "profiler")
  "s" 'profiler-start
  "e" 'profiler-stop
  "p" 'profiler-report)
```


#### Buffer switching {#buffer-switching}

Some keybindings I used in vim to switch buffers and can't let go of.

```emacs-lisp
(general-define-key
  :keymaps 'override
  "C-<right>" 'evil-window-right
  "C-<left>" 'evil-window-left
  "C-<up>" 'evil-window-up
  "C-<down>" 'evil-window-down
  "C-h" 'evil-window-left
  "C-l" 'evil-window-right
  "C-k" 'evil-window-up
  "C-j" 'evil-window-down
  "C-x h" 'previous-buffer
  "C-x l" 'next-buffer)

(general-define-key
 :keymaps 'evil-window-map
 "x" 'kill-buffer-and-window
 "d" 'kill-current-buffer)
```

And winner-mode to keep the history of window states.

```emacs-lisp
(winner-mode 1)

(general-define-key
 :keymaps 'evil-window-map
 "u" 'winner-undo
 "U" 'winner-redo)
```


#### Buffer management {#buffer-management}

```emacs-lisp
(my-leader-def
  :infix "b"
  "" '(:which-key "buffers")
  "s" '((lambda () (interactive) (switch-to-buffer (persp-scratch-buffer)))
	:which-key "*scratch*")
  "m" '((lambda () (interactive) (persp-switch-to-buffer "*Messages*"))
	:which-key "*Messages*")
  "l" 'next-buffer
  "h" 'previous-buffer
  "k" 'kill-buffer
  "b" 'persp-ivy-switch-buffer
  "u" 'ibuffer)
```


#### xref {#xref}

Some keybindings for xref and go to definition.

```emacs-lisp
(general-nmap
  "gD" 'xref-find-definitions-other-window
  "gr" 'xref-find-references
  "gd" 'evil-goto-definition)

(my-leader-def
  "fx" 'xref-find-apropos)
```


#### Folding {#folding}

There are multiple ways to fold text in Emacs.

The most versatile is the built-in `hs-minor-mode`, which seems to work out of the box for Lisps, C-like languages and Python. `outline-minor-mode` works for org-mode, LaTeX and the like. There is a 3rd-party solution [origami.el](https://github.com/elp-revive/origami.el), but I don't use it at the moment.

Evil does a pretty good job of uniting these two in the set of vim-like keybindings. I was using `SPC` in vim, but as now this isn't an option, I set `TAB` to toggle folding.

```emacs-lisp
(general-nmap :keymaps '(hs-minor-mode-map outline-minor-mode-map)
  "ze" 'hs-hide-level
  "TAB" 'evil-toggle-fold)
```


#### Zoom {#zoom}

```emacs-lisp
(defun my/zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
		      :height
		      (+ (face-attribute 'default :height) 10)))

(defun my/zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
		      :height
		      (- (face-attribute 'default :height) 10)))

;; change font size, interactively
(global-set-key (kbd "C-+") 'my/zoom-in)
(global-set-key (kbd "C-=") 'my/zoom-out)
```


### Editing helpers {#editing-helpers}


#### Visual fill column mode {#visual-fill-column-mode}

```emacs-lisp
(use-package visual-fill-column
  :straight t
  :config
  (add-hook 'visual-fill-column-mode-hook
	    (lambda () (setq visual-fill-column-center-text t))))
```


#### smartparens {#smartparens}

A minor mode to deal with pairs. Its functionality overlaps with evil-surround, but smartparens provides the most comfortable way to do stuff like automatically insert pairs.

References:

-   [smartparens repo](https://github.com/Fuco1/smartparens)

<!--listend-->

```emacs-lisp
(use-package smartparens
  :straight t)
```


#### Aggressive Indent {#aggressive-indent}

A package to keep the code intended.

Doesn't work too well with js ecosystem, because the LSP-based indentation is rather slow but nice for Lisps.

References:

-   [aggressive-indent-mode repo](https://github.com/Malabarba/aggressive-indent-mode)

<!--listend-->

```emacs-lisp
(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :straight t)
```


#### Delete trailing whitespace {#delete-trailing-whitespace}

Delete trailing whitespace on save, unless in particular modes where trailing whitespace is important, like Markdown.

```emacs-lisp
(setq my/trailing-whitespace-modes '(markdown-mode))

(require 'cl-extra)

(add-hook 'before-save-hook
	  (lambda ()
	    (unless (cl-some #'derived-mode-p my/trailing-whitespace-modes)
	      (delete-trailing-whitespace))))
```


#### Expand region {#expand-region}

```emacs-lisp
(use-package expand-region
  :straight t
  :commands (er/expand-region)
  :init
  (general-nmap "+" 'er/expand-region))
```


### Various settings {#various-settings}


#### Tabs {#tabs}

Some default settings to manage tabs.

```emacs-lisp
(setq tab-always-indent nil)

(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round nil)
```


#### Scrolling config {#scrolling-config}

```emacs-lisp
(setq scroll-conservatively scroll-margin)
(setq scroll-step 1)
(setq scroll-preserve-screen-position t)
(setq scroll-error-top-bottom t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-inhibit-click-time nil)
```


#### Clipboard {#clipboard}

```emacs-lisp
(setq select-enable-clipboard t)
(setq mouse-yank-at-point t)
```


#### Backups {#backups}

```emacs-lisp
(setq backup-inhibited t)
(setq auto-save-default nil)
```


### Undo Tree {#undo-tree}

Replaces Emacs build-in sequential undo system with a tree-based one. Probably one of the greatest features of Emacs as a text editor.

References:

-   [UndoTree on EmacsWiki](https://www.emacswiki.org/emacs/UndoTree)

<!--listend-->

```emacs-lisp
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)

  (my-leader-def "u" 'undo-tree-visualize)
  (fset 'undo-auto-amalgamate 'ignore)
  (setq undo-limit 6710886400)
  (setq undo-strong-limit 100663296)
  (setq undo-outer-limit 1006632960))
```


### Help {#help}

[helpful](https://github.com/Wilfred/helpful) package improves the `*help*` buffer.

```emacs-lisp
(use-package helpful
  :straight t
  :commands (helpful-callable
	     helpful-variable
	     helpful-key
	     helpful-macro
	     helpful-function
	     helpful-command))

```

As I use `C-h` to switch buffers, I moved the help to `SPC-h` with the code below. Of course, I didn't type it all by hand.

```emacs-lisp
(my-leader-def
  :infix "h"
  "" '(:which-key "help")
  "RET" 'view-order-manuals
  "." 'display-local-help
  "?" 'help-for-help
  "C" 'describe-coding-system
  "F" 'Info-goto-emacs-command-node
  "I" 'describe-input-method
  "K" 'Info-goto-emacs-key-command-node
  "L" 'describe-language-environment
  "P" 'describe-package
  "S" 'info-lookup-symbol
  "a" 'helm-apropos
  "b" 'describe-bindings
  "c" 'describe-key-briefly
  "d" 'apropos-documentation
  "e" 'view-echo-area-messages
  "f" 'helpful-function
  "g" 'describe-gnu-project
  "h" 'view-hello-file
  "i" 'info
  "k" 'helpful-key
  "l" 'view-lossage
  "m" 'describe-mode
  "n" 'view-emacs-news
  "o" 'describe-symbol
  "p" 'finder-by-keyword
  "q" 'help-quit
  "r" 'info-emacs-manual
  "s" 'describe-syntax
  "t" 'help-with-tutorial
  "v" 'helpful-variable
  "w" 'where-is
  "<f1>" 'help-for-help)
```


### Ivy, counsel, swiper {#ivy-counsel-swiper}

Minibuffer completion tools for Emacs.

References:

-   [repo](https://oremacs.com/swiper/)
-   [User Manual](https://oremacs.com/swiper/)

<!--listend-->

```emacs-lisp
(use-package ivy
  :straight t
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package counsel
  :straight t
  :after ivy
  :config
  (counsel-mode))

(use-package swiper
  :defer t
  :straight t)
```


#### ivy-rich {#ivy-rich}

[ivy-rich](https://github.com/Yevgnen/ivy-rich) provides a more informative interface for ivy.

```emacs-lisp
(use-package ivy-rich
  :straight t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
```


#### prescient {#prescient}

A package that enhances sorting & filtering of candidates. `ivy-prescient` adds integration with Ivy.

References:

-   [prescient.el repo](https://github.com/raxod502/prescient.el)

<!--listend-->

```emacs-lisp
(use-package ivy-prescient
  :straight t
  :after counsel
  :config
  (ivy-prescient-mode +1)
  (setq ivy-prescient-retain-classic-highlighting t)
  (prescient-persist-mode 1)
  (setq ivy-prescient-sort-commands
	'(:not swiper
	       swiper-isearch
	       ivy-switch-buffer
	       ;; ivy-resume
	       ;; ivy--restore-session
	       lsp-ivy-workspace-symbol
	       counsel-grep
	       ;; counsel-find-file
	       counsel-git-grep
	       counsel-rg
	       counsel-ag
	       counsel-ack
	       counsel-fzf
	       counsel-pt
	       counsel-imenu
	       counsel-yank-pop
	       counsel-recentf
	       counsel-buffer-or-recentf
	       proced-filter-interactive
	       proced-sort-interactive))
  ;; Do not use prescient in find-file
  (ivy--alist-set 'ivy-sort-functions-alist #'read-file-name-internal #'ivy-sort-file-function-default))
```


#### Keybindings {#keybindings}

```emacs-lisp
(my-leader-def
  :infix "f"
  "" '(:which-key "various completions")'
  ;; "b" 'counsel-switch-buffer
  "b" 'persp-ivy-switch-buffer
  "e" 'conda-env-activate
  "f" 'project-find-file
  "c" 'counsel-yank-pop
  "a" 'counsel-rg
  "A" 'counsel-ag)

(general-define-key
 :states '(insert normal)
 "C-y" 'counsel-yank-pop)

(my-leader-def "SPC" 'ivy-resume)
(my-leader-def "s" 'swiper-isearch
  "S" 'swiper-all)

(general-define-key
 :keymaps '(ivy-minibuffer-map swiper-map)
 "M-j" 'ivy-next-line
 "M-k" 'ivy-previous-line
 "<C-return>" 'ivy-call
 "M-RET" 'ivy-immediate-done
 [escape] 'minibuffer-keyboard-quit)
```


### <span class="org-todo done OFF">OFF</span> (OFF) Helm {#off--helm}

Config for the Helm incremental completion framework. I switched to Ivy some time ago, but keep the configuration just in case.

```emacs-lisp
(use-package helm
  :init
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-move-to-line-cycle-in-source t)
  :straight t
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-ag
  :straight t)

(use-package helm-rg
  :straight t)

(general-nmap
  :keymaps 'helm-ag-mode-map
  "RET" 'helm-ag-mode-jump
  "M-RET" 'helm-ag-mode-jump-other-window)

(general-nmap
  :keymaps 'helm-occur-mode-map
  "RET" 'helm-occur-mode-goto-line
  "M-RET" 'helm-occur-mode-goto-line-ow)

(general-define-key "M-x" 'helm-M-x)
(my-leader-def
  "fb" 'helm-buffers-list
  "fs" 'helm-lsp-workspace-symbol
  "fw" 'helm-lsp-global-workspace-symbol
  "fc" 'helm-show-kill-ring
  ;; "fa" 'helm-do-ag-project-root
  "fm" 'helm-bookmarks
  "ff" 'project-find-file
  "fe" 'conda-env-activate)

(my-leader-def "s" 'helm-occur)
(my-leader-def "SPC" 'helm-resume)

(general-define-key
  :keymaps 'helm-map
  "C-j" 'helm-next-line
  "C-k" 'helm-previous-line)

(general-define-key
  :keymaps '(helm-find-files-map helm-locate-map)
  "C-h" 'helm-find-files-up-one-level
  "C-l" 'helm-execute-persistent-action)

(general-imap
  "C-y" 'helm-show-kill-ring)
;; (general-nmap "C-p" 'project-find-file)
```


### Treemacs {#treemacs}

[Treemacs](https://github.com/Alexander-Miller/treemacs) calls itself a tree layout file explorer, but looks more like a project and workspace management system.

Integrates with evil, magit, projectile and perspective. The latter is particularly great - each perspective can have its own treemacs workspace!

```emacs-lisp
(use-package treemacs
  :straight t
  :commands (treemacs treemacs-switch-workspace treemacs-edit-workspace)
  :config
  (setq treemacs-follow-mode nil)
  (setq treemacs-follow-after-init nil)
  (setq treemacs-space-between-root-nodes nil)
  (treemacs-git-mode 'extended)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (general-define-key
   :keymaps 'treemacs-mode-map
   [mouse-1] #'treemacs-single-click-expand-action
   "M-l" #'treemacs-root-down
   "M-h" #'treemacs-root-up))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package treemacs-perspective
  :after (treemacs perspective)
  :straight t
  :config
  (treemacs-set-scope-type 'Perspectives))

(general-define-key
 :keymaps '(normal override global)
 "C-n" 'treemacs)

(general-define-key
 :keymaps '(treemacs-mode-map) [mouse-1] #'treemacs-single-click-expand-action)

(my-leader-def
  :infix "t"
  "" '(:which-key "treemacs")
  "w" 'treemacs-switch-workspace
  "e" 'treemacs-edit-workspaces)
```


#### Helper functions {#helper-functions}

Function to open dired and vterm at given nodes.

```emacs-lisp
(defun my/treemacs-open-dired ()
  "Open dired at given treemacs node"
  (interactive)
  (let (path (treemacs--prop-at-point :path))
    (dired path)))

(defun my/treemacs-open-vterm ()
  "Open vterm at given treemacs node"
  (interactive)
  (let ((default-directory (file-name-directory (treemacs--prop-at-point :path))))
    (vterm)))

(with-eval-after-load 'treemacs
  (general-define-key
   :keymaps 'treemacs-mode-map
   :states '(treemacs)
   "gd" 'my/treemacs-open-dired
   "gt" 'my/treemacs-open-vterm
   "`" 'my/treemacs-open-vterm))
```


#### Custom icons {#custom-icons}

```emacs-lisp
;; (treemacs-define-custom-icon (concat " " (all-the-icons-fileicon "typescript")) "spec.ts")
;; (setq treemacs-file-extension-regex (rx "." (or "spec.ts" (+ (not "."))) eos))
```


### Projectile {#projectile}

[Projectile](https://github.com/bbatsov/projectile) gives a bunch of useful functions for managing projects, like finding files within a project, fuzzy-find, replace, etc.

`defadvice` is meant to speed projectile up with TRAMP a bit.

```emacs-lisp
(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code" "~/Documents"))
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it)))

(use-package counsel-projectile
  :after (counsel projectile)
  :straight t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

(my-leader-def
  "p" '(:keymap projectile-command-map :which-key "projectile"))

(general-nmap "C-p" 'counsel-projectile-find-file)
```


### Company {#company}

A completion framework for Emacs.

References:

-   [company homepage](http://company-mode.github.io/)
-   [company-box homepage](https://github.com/sebastiencs/company-box)

<!--listend-->

```emacs-lisp
(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-idle-delay (if my/lowpower 0.5 0.125))
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t))

(general-imap "C-SPC" 'company-complete)
```

A company frontend with nice icons.

```emacs-lisp
(use-package company-box
  :straight t
  :if (not my/lowpower)
  :after (company)
  :hook (company-mode . company-box-mode))
```


### Git & Magit {#git-and-magit}

[Magic](https://magit.vc/) is a git interface for Emacs. The closest non-Emacs alternative (sans actual clones) I know is [lazygit](https://github.com/jesseduffield/lazygit), which I used before Emacs.

[git-gutter](https://github.com/emacsorphanage/git-gutter) is a package which shows git changes for each line (added/changed/deleted lines).

[git-timemachine](https://github.com/emacsmirror/git-timemachine) allows to visit previous versions of a file.

```emacs-lisp
(use-package magit
  :straight t
  :commands (magit-status magit-file-dispatch)
  :config
  (setq magit-blame-styles
	'((margin
	   (margin-format    . ("%a %A %s"))
	   (margin-width     . 42)
	   (margin-face      . magit-blame-margin)
	   (margin-body-face . (magit-blame-dimmed)))
	  (headings
	   (heading-format   . "%-20a %C %s\n"))
	  (highlight
	   (highlight-face   . magit-blame-highlight))
	  (lines
	   (show-lines       . t)
	   (show-message     . t)))))

(use-package git-gutter
  :straight t
  :if (not my/slow-ssh)
  :config
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :straight t
  :commands (git-timemachine))

(my-leader-def
  "m" 'magit
  "M" 'magit-file-dispatch)
```


### Editorconfig {#editorconfig}

Editorconfig support for Emacs.

References:

-   [Editorconfig reference](https://editorconfig.org/)

<!--listend-->

```emacs-lisp
(use-package editorconfig
  :straight t
  :config
  (unless my/slow-ssh (editorconfig-mode 1))
  (add-to-list 'editorconfig-indentation-alist
	       '(emmet-mode emmet-indentation)))
```


### Snippets {#snippets}

A snippet system for Emacs and a collection of pre-built snippets.

`yasnippet-snippets` has to be loaded before `yasnippet` for user snippets to override the pre-built ones.

References:

-   [yasnippet documentation](http://joaotavora.github.io/yasnippet/)

<!--listend-->

```emacs-lisp
(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs `(,(concat (expand-file-name user-emacs-directory) "snippets")))
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(general-imap "M-TAB" 'company-yasnippet)
```


### Time trackers {#time-trackers}

A bunch of time trackers I use.

References:

-   [WakaTime](https://wakatime.com)
-   [ActivityWatch](https://activitywatch.net/)


#### WakaTime {#wakatime}

Before I figure out how to package this for Guix:

-   Clone [the repo](https://github.com/wakatime/wakatime-cli)
-   Run `go build`
-   Copy the binary to the `~/bin` folder

<!--listend-->

```emacs-lisp
(use-package wakatime-mode
  :straight (:host github :repo "SqrtMinusOne/wakatime-mode")
  :if (not my/is-termux)
  :config
  (setq wakatime-ignore-exit-codes '(0 1 102))
  (advice-add 'wakatime-init :after (lambda () (setq wakatime-cli-path "/home/pavel/bin/wakatime-cli")))
  ;; (setq wakatime-cli-path (executable-find "wakatime"))
  (global-wakatime-mode))
```


#### ActivityWatch {#activitywatch}

```emacs-lisp
(use-package request
  :straight t)

(use-package activity-watch-mode
  :straight t
  :if (not my/is-termux)
  :config
  (global-activity-watch-mode))
```


## UI {#ui}


### General UI & GUI Settings {#general-ui-and-gui-settings}

Disable GUI elements

```emacs-lisp
(unless my/is-termux
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
```

Transparency. Not setting it now, as I'm using [picom]({{< relref "Desktop" >}}).

```emacs-lisp
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
```

Prettify symbols. Also not setting it, ligatures seem to be enough for me.

```emacs-lisp
;; (global-prettify-symbols-mode)
```

No start screen

```emacs-lisp
(setq inhibit-startup-screen t)
```

Visual bell

```emacs-lisp
(setq visible-bell 0)
```

y or n instead of yes or no

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
```

Hide mouse cursor while typing

```emacs-lisp
(setq make-pointer-invisible t)
```

Line numbers. There seems to be a catch with the relative number setting:

-   `visual` doesn't take folding into account but also doesn't take wrapped lines into account (makes multiple numbers for a single wrapped line)
-   `relative` makes a single number for a wrapped line, but counts folded lines.

`visual` option seems to be less of a problem in most cases.

```emacs-lisp
(global-display-line-numbers-mode 1)
(line-number-mode nil)
(setq display-line-numbers-type 'visual)
(column-number-mode)
```

Show pairs

```emacs-lisp
(show-paren-mode 1)
```

Word wrapping. These settings aren't too obvious compared to `:set wrap` from vim:

-   `word-wrap` means just "don't split one word between two lines". So, if there isn't enough place to put a word at the end of the line, it will be put on a new one. Run `M-x toggle-word-wrap` to toggle that.
-   `visual-line-mode` seems to be a superset of `word-wrap`. It also enables some editing commands to work on visual lines instead of logical ones, hence the naming.
-   `auto-fill-mode` does the same as `word-wrap`, except it actually **edits the buffer** to make lines break in the appropriate places.
-   `truncate-lines` truncate long lines instted of continuing them. Run `M-x toggle-truncate-lines` to toggle that. I find that `truncate-lines` behaves strangely when `visual-line-mode` is on, so I use one or another.

<!--listend-->

```emacs-lisp
(setq word-wrap 1)
(global-visual-line-mode 1)
```

Highlight current line

```emacs-lisp
(global-hl-line-mode 1)
```


### Theme & global stuff {#theme-and-global-stuff}

Dim inactive buffers.

```emacs-lisp
(use-package auto-dim-other-buffers
  :straight t
  :if (display-graphic-p)
  :config
  (auto-dim-other-buffers-mode t))
```

My colorscheme of choice.

```emacs-lisp
(use-package doom-themes
  :straight t
  :if (not my/is-termux)
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))
```


#### Custom theme {#custom-theme}

A custom theme, dependent on Doom. I set all my custom variables there.

A custom theme is necessary because if one calls `custom-set-faces` and `custom-set-variables` in code, whenever a variable is changed and saved in a customize buffer, data from all calls of these functions is saved as well.

Also, a hook allows me to change doom-theme more or less at will, although I do that only to switch to a light theme once in a blue moon.

```emacs-lisp
(unless my/is-termux
  (deftheme my-theme)

  (defun my/update-my-theme (&rest _)
    (custom-theme-set-faces
     'my-theme
     `(tab-bar-tab ((t (
			:background ,(doom-color 'bg)
			:foreground ,(doom-color 'yellow)
			:underline ,(doom-color 'yellow)))))
     `(org-block ((t (:background ,(color-darken-name (doom-color 'bg) 3)))))
     `(org-block-begin-line ((t (
				 :background ,(color-darken-name (doom-color 'bg) 3)
				 :foreground ,(doom-color 'grey)))))
     `(auto-dim-other-buffers-face ((t (:background ,(color-darken-name (doom-color 'bg) 3)))))
     `(aweshell-alert-buffer-face ((t (:foreground ,(doom-color 'red) :weight bold))))
     `(aweshell-alert-command-face ((t (:foreground ,(doom-color 'yellow) :weight bold))))
     `(epe-pipeline-delimiter-face ((t (:foreground ,(doom-color 'green)))))
     `(epe-pipeline-host-face ((t (:foreground ,(doom-color 'blue)))))
     `(epe-pipeline-time-face ((t (:foreground ,(doom-color 'yellow)))))
     `(epe-pipeline-user-face ((t (:foreground ,(doom-color 'red)))))
     `(elfeed-search-tag-face ((t (:foreground ,(doom-color 'yellow)))))
     `(notmuch-wash-cited-text ((t (:foreground ,(doom-color 'yellow))))))
    (custom-theme-set-variables
     'my-theme
     `(aweshell-invalid-command-color ,(doom-color 'red))
     `(aweshell-valid-command-color ,(doom-color 'green)))
    (enable-theme 'my-theme))

  (advice-add 'load-theme :after #'my/update-my-theme)
  (when (fboundp 'doom-color)
    (my/update-my-theme)))
```


#### Font {#font}

To install a font, download the font and unpack it into the `.local/share/fonts` directory. Create one if it doesn't exist.

As I use nerd fonts elsewhere, I use one in Emacs as well.

References:

-   [nerd fonts homepage](https://nerdfonts.com)

<!--listend-->

```emacs-lisp
(set-frame-font "JetBrainsMono Nerd Font 10" nil t)
```

To make the icons work (e.g. in the Doom Modeline), run `M-x all-the-icons-install-fonts`. The package definition is somewhere later in the config.


### Custom frame title {#custom-frame-title}

Title format, which looks something like `emacs:project@hostname`.

```emacs-lisp
(setq-default frame-title-format
	      '(""
		"emacs"
		(:eval
		 (let ((project-name (projectile-project-name)))
		   (if (not (string= "-" project-name))
		       (format ":%s@%s" project-name (system-name))
		     (format "@%s" (system-name)))))))
```


### perspective.el {#perspective-dot-el}

[perspective.el](https://github.com/nex3/perspective-el) is a package which provides gives Emacs capacities to group buffers into "perspectives", which are like workspaces in tiling WMs.

An advantage over `tab-bar.el` is that `perspective.el` has better capacities for managing buffers, e.g. gives an ibuffer-like interface inside a perspective.

However, I don't like that list of workspaces is displayed inside the modeline rather than in an actual bar on the top of the frame. I may look into that later.

```emacs-lisp
(use-package perspective
  :straight t
  :init
  ;; (setq persp-show-modestring 'header)
  (setq persp-sort 'created)
  :config
  (persp-mode)
  (my-leader-def "x" '(:keymap perspective-map :which-key "perspective"))
  (general-define-key
   :keymaps 'override
   :states '(normal emacs)
   "gt" 'persp-next
   "gT" 'persp-prev
   "gn" 'persp-switch
   "gN" 'persp-kill)
  (general-define-key
   :keymaps 'perspective-map
   "b" 'persp-ivy-switch-buffer
   "x" 'persp-ivy-switch-buffer
   "u" 'persp-ibuffer))
```


#### Some functions {#some-functions}

Move the current buffer to a perspective and switch to it.

```emacs-lisp
(defun my/persp-move-window-and-switch ()
  (interactive)
  (let* ((buffer (current-buffer)))
    (call-interactively #'persp-switch)
    (persp-set-buffer (buffer-name buffer))
    (switch-to-buffer buffer)))
```

Copy the current buffer to a perspective and switch to it.

```emacs-lisp
(defun my/persp-copy-window-and-switch ()
  (interactive)
  (let* ((buffer (current-buffer)))
    (call-interactively #'persp-switch)
    (persp-add-buffer (buffer-name buffer))
    (switch-to-buffer buffer)))
```

Add keybindings to the default map.

```emacs-lisp
(with-eval-after-load 'perspective
  (general-define-key
   :keymaps 'perspective-map
   "m" #'my/persp-move-window-and-switch
   "f" #'my/persp-copy-window-and-switch))
```


### <span class="org-todo done OFF">OFF</span> (OFF) Tab bar {#off--tab-bar}

~~I rely rather heavily on tab-bar in my workflow. I have a suspicion I'm not using it the intended way, but that works for me.~~

For now switched to perspective.el, so the following block is not tangled.


#### Setup {#setup}

```emacs-lisp
(general-define-key
 :keymaps 'override
 :states '(normal emacs)
 "gt" 'tab-bar-switch-to-next-tab
 "gT" 'tab-bar-switch-to-prev-tab
 "gn" 'tab-bar-new-tab)

(setq tab-bar-show 1)
(setq tab-bar-tab-hints t)
(setq tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)

;; Tabs
(general-nmap "gn" 'tab-new)
(general-nmap "gN" 'tab-close)
```


#### My title {#my-title}

Prepend tab name with the shortened projectile project title

```emacs-lisp
(setq my/project-title-separators "[-_ ]")

(setq my/project-names-override-alist
      '((".password-store" . "pass")))

(defun my/shorten-project-name-elem (elem crop)
  (if (string-match "^\\[.*\\]$" elem)
      (concat "["
	      (my/shorten-project-name-elem (substring elem 1 (- (length elem) 1)) crop)
	      "]")
    (let* ((prefix (car (s-match my/project-title-separators elem)))
	   (rest
	    (substring
	     (if prefix
		 (substring elem (length prefix))
	       elem)
	     0 (if crop 1 nil))))
      (concat prefix rest))))

(defun my/shorten-project-name (project-name)
  (or
   (cdr (assoc project-name my/project-names-override-alist))
   (let ((elems (s-slice-at my/project-title-separators project-name)))
     (concat
      (apply
       #'concat
       (cl-mapcar (lambda (elem) (my/shorten-project-name-elem elem t)) (butlast elems)))
      (my/shorten-project-name-elem (car (last elems)) nil)))))

(defun my/tab-bar-name-function ()
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
	(tab-bar-tab-name-current-with-count)
      (concat "[" (my/shorten-project-name project-name) "] "
	      (replace-regexp-in-string "<.*>" "" (tab-bar-tab-name-current-with-count))))))

(setq tab-bar-tab-name-function #'my/tab-bar-name-function)
```


### Modeline {#modeline}

A modeline from Doom Emacs.

References:

-   [Doom Modeline](https://github.com/seagle0128/doom-modeline)

<!--listend-->

```emacs-lisp
(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-env-enable-go nil)
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon nil))
```


### Font stuff {#font-stuff}


#### Emojis {#emojis}

| Note | Type                                                      |
|------|-----------------------------------------------------------|
| TODO | Figure out how to display emojis without prettify symbols |

```emacs-lisp
(use-package emojify
  :straight t
  :if (not (or my/lowpower my/is-termux))
  :hook (after-init . global-emojify-mode))
```


#### Ligatures {#ligatures}

Ligature setup for the JetBrainsMono font.

```emacs-lisp
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :if (not my/is-termux)
  :config
  (ligature-set-ligatures
   '(
     typescript-mode
     js2-mode
     vue-mode
     svelte-mode
     scss-mode
     php-mode
     python-mode
     js-mode
     markdown-mode
     clojure-mode
     go-mode
     sh-mode
     haskell-mode)
   '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<="
     ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!" "??"
     "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>"
     "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##"
     "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:"
     "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>"
     "<*" "*>" "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>"
     "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>"
     "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<"
     "<-|" "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>"
     "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<"
     ">]" "|>" "<|" "||>" "<||" "|||>" "<|||" "<|>" "..." ".."
     ".=" ".-" "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>"
     "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
  (global-ligature-mode t))
```


#### Icons {#icons}

```emacs-lisp
(use-package all-the-icons
  :straight t)
```


#### Highlight todo {#highlight-todo}

```emacs-lisp
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :straight t)
```


### Text highlight improvements {#text-highlight-improvements}

Highlight indent guides.

```emacs-lisp
(use-package highlight-indent-guides
  :straight t
  :if (not my/lowpower)
  :hook (
	 (prog-mode . highlight-indent-guides-mode)
	 (vue-mode . highlight-indent-guides-mode)
	 (LaTeX-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))
```

Rainbow parentheses.

```emacs-lisp
(use-package rainbow-delimiters
  :straight t
  :if (not my/lowpower)
  :hook ((prog-mode . rainbow-delimiters-mode))
  ;; :commands (rainbow-delimiters-mode)
  ;; :init
  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (unless (org-in-src-block-p)
  ;;               (rainbow-delimiters-mode))))
  )
```

Highlight colors

```emacs-lisp
(use-package rainbow-mode
  :commands (rainbow-mode)
  :straight t)
```


## Dired {#dired}

Dired is a built-in file manager. I use it as my primary file manager, hence the top level of config.


### Basic config & keybindings {#basic-config-and-keybindings}

My config mostly follows ranger's and vifm's keybindings which I'm used to.

```emacs-lisp
(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-alh --group-directories-first"))
  :commands (dired)
  :config
  (setq dired-dwim-target t)
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (setq truncate-lines t)
	      (visual-line-mode nil)))
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "h" 'dired-up-directory
   "l" 'dired-find-file
   "=" 'dired-narrow
   "-" 'dired-create-empty-file
   "~" 'vterm
   "<left>" 'dired-up-directory
   "<right>" 'dired-find-file
   "M-<return>" 'dired-open-xdg))

(defun my/dired-home ()
  "Open dired at $HOME"
  (interactive)
  (dired (expand-file-name "~")))

(my-leader-def
  "ad" #'dired
  "aD" #'my/dired-home)
```


### Addons {#addons}

I used to use [dired+](https://www.emacswiki.org/emacs/DiredPlus), which provides a lot of extensions for dired functionality, but it also creates some new problems, so I opt out of it. Fortunately, the one feature I want from this package - adding more colors to dired buffers - is available as a separate package

```emacs-lisp
(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode 1))
```

~~Reuse the current dired buffer instead of spamming new ones.~~ Looks like not neccesary with Emacs 28.1

```emacs-lisp
(use-package dired-single
  :after dired
  :disabled
  :straight t)
```

Display icons for files.

| Note        | Type                                                     |
|-------------|----------------------------------------------------------|
| **ACHTUNG** | This plugin is slow as hell with TRAMP or in `gnu/store` |

```emacs-lisp
(use-package all-the-icons-dired
  :straight t
  :if (not (or my/lowpower my/slow-ssh))
  :hook (dired-mode . (lambda ()
			(unless (string-match-p "/gnu/store" default-directory)
			  (all-the-icons-dired-mode))))
  :config
  (advice-add 'dired-add-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-remove-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-kill-subdir :around #'all-the-icons-dired--refresh-advice))
```

Provides stuff like `dired-open-xdg`

```emacs-lisp
(use-package dired-open
  :straight t
  :commands (dired-open-xdg))
```

vifm-like filter

```emacs-lisp
(use-package dired-narrow
  :straight t
  :commands (dired-narrow)
  :config
  (general-define-key
   :keymaps 'dired-narrow-map
   [escape] 'keyboard-quit))
```

Display git info, such as the last commit for file and stuff. It's pretty useful but also slows down Dired a bit, hence I don't turn it out by default.

```emacs-lisp
(use-package dired-git-info
  :straight t
  :after dired
  :if (not my/slow-ssh)
  :config
  (general-define-key
   :keymap 'dired-mode-map
   :states '(normal emacs)
   ")" 'dired-git-info-mode))
```


### Subdirectories {#subdirectories}

Subdirectories are one of the interesting features of Dired. It allows displaying multiple folders on the same window.

I add my own keybindings and some extra functionality.

```emacs-lisp
(defun my/dired-open-this-subdir ()
  (interactive)
  (dired (dired-current-directory)))

(defun my/dired-kill-all-subdirs ()
  (interactive)
  (let ((dir dired-directory))
    (kill-buffer (current-buffer))
    (dired dir)))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "s" nil
   "ss" 'dired-maybe-insert-subdir
   "sl" 'dired-maybe-insert-subdir
   "sq" 'dired-kill-subdir
   "sk" 'dired-prev-subdir
   "sj" 'dired-next-subdir
   "sS" 'my/dired-open-this-subdir
   "sQ" 'my/dired-kill-all-subdirs
   (kbd "TAB") 'dired-hide-subdir))
```


### TRAMP {#tramp}

TRAMP is a package that provides remote editing capacities. It is particularly useful for remote server management.

One of the reasons why TRAMP may be slow is that some plugins do too many requests to the filesystem. To debug these issues, set the following variable to 6:

```emacs-lisp
(setq tramp-verbose 1)
```

To check if a file is remote, you can use `file-remote-p`. E.g. `(file-remote-p default-directory)` for a current buffer. The problem with this approach is that it's rather awkward to add these checks in every hook, especially for global modes, so for now, I just set an environment variable for Emacs which disables these modes.

So far I have found the following problematic plugins:

| Plugin              | Note                                     | Solution                      |
|---------------------|------------------------------------------|-------------------------------|
| editorconfig        | looks for .editorconfig in the file tree | do not enable globally        |
| all-the-icons-dired | runs test on every file in the directory | disable                       |
| projectile          | looks for .git, .svn, etc                | advice `projectile-file-name` |
| lsp                 | does a whole lot of stuff                | disable                       |
| git-gutter          | runs git                                 | disable                       |
| vterm               | no proper TRAMP integration              | use eshell or shell           |

At any rate, it's usable, although not perfect.

Some other optimization settings:

```emacs-lisp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))
```

Also, here is a hack to make TRAMP find `ls` on Guix:

```emacs-lisp
(with-eval-after-load 'tramp
  (setq tramp-remote-path
	(append tramp-remote-path
		'(tramp-own-remote-path))))
```


### Bookmarks {#bookmarks}

A simple bookmark list for Dired, mainly to use with TRAMP. I may look into a proper bookmarking system later.

Bookmarks are listed in the [dired-bookmarks.el](.emacs.d/dired-bookmarks.el) file, which looks like this:

```text
(setq my/dired-bookmarks
      '(("sudo" . "/sudo::/")))
```

The file itself is encrypted with yadm.

```emacs-lisp
(defun my/dired-bookmark-open ()
  (interactive)
  (unless (boundp 'my/dired-bookmarks)
    (load (concat user-emacs-directory "dired-bookmarks")))
  (let ((bookmarks
	 (mapcar
	  (lambda (el) (cons (format "%-30s %s" (car el) (cdr el)) (cdr el)))
	  my/dired-bookmarks)))
    (dired
     (cdr
      (assoc
       (completing-read "Dired: " bookmarks nil nil "^")
       bookmarks)))))
```


## Shells {#shells}


### vterm {#vterm}

My terminal emulator of choice.

References:

-   [emacs-libvterm repo](https://github.com/akermu/emacs-libvterm)


#### Configuration {#configuration}

I use the package from the Guix repository to avoid building libvterm.

```emacs-lisp
(use-package vterm
  ;; :straight t
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-kill-buffer-on-exit t)

  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local global-display-line-numbers-mode nil)
	      (display-line-numbers-mode 0)))


  (advice-add 'evil-collection-vterm-insert
	      :before (lambda (&rest args)
			(ignore-errors
			  (apply #'vterm-reset-cursor-point args))))

  (general-define-key
   :keymaps 'vterm-mode-map
   "M-q" 'vterm-send-escape

   "C-h" 'evil-window-left
   "C-l" 'evil-window-right
   "C-k" 'evil-window-up
   "C-j" 'evil-window-down

   "C-<right>" 'evil-window-right
   "C-<left>" 'evil-window-left
   "C-<up>" 'evil-window-up
   "C-<down>" 'evil-window-down

   "M-<left>" 'vterm-send-left
   "M-<right>" 'vterm-send-right
   "M-<up>" 'vterm-send-up
   "M-<down>" 'vterm-send-down)

  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(normal insert)
   "<home>" 'vterm-beginning-of-line
   "<end>" 'vterm-end-of-line)

  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(insert)
   "C-r" 'vterm-send-C-r
   "C-k" 'vterm-send-C-k
   "C-j" 'vterm-send-C-j
   "M-l" 'vterm-send-right
   "M-h" 'vterm-send-left
   "M-k" 'vterm-send-up
   "M-j" 'vterm-send-down))
```


#### Subterminal {#subterminal}

Open a terminal in the lower third of the frame with the `` ` `` key.

```emacs-lisp
(add-to-list 'display-buffer-alist
	     `(,"vterm-subterminal.*"
	       (display-buffer-reuse-window
		display-buffer-in-side-window)
	       (side . bottom)
	       (reusable-frames . visible)
	       (window-height . 0.33)))

(defun my/toggle-vterm-subteminal ()
  "Toogle subteminal."
  (interactive)
  (let
      ((vterm-window
	(seq-find
	 (lambda (window)
	   (string-match
	    "vterm-subterminal.*"
	    (buffer-name (window-buffer window))))
	 (window-list))))
    (if vterm-window
	(if (eq (get-buffer-window (current-buffer)) vterm-window)
	    (kill-buffer (current-buffer))
	  (select-window vterm-window))
      (vterm-other-window "vterm-subterminal"))))
(unless my/slow-ssh
  (general-nmap "`" 'my/toggle-vterm-subteminal)
  (general-nmap "~" 'vterm))
```


#### Dired integration {#dired-integration}

A function to get pwd for vterm. Couldn't find a built-in function for some reason, but this seems to be working fine:

```emacs-lisp
(defun my/vterm-get-pwd ()
  (if vterm--process
      (file-truename (format "/proc/%d/cwd" (process-id vterm--process)))
    default-directory))
```

Now we can open dired for vterm pwd:

```emacs-lisp
(defun my/vterm-dired-other-window ()
  "Open dired in vterm pwd in other window"
  (interactive)
  (dired-other-window (my/vterm-get-pwd)))

(defun my/vterm-dired-replace ()
  "Replace vterm with dired"
  (interactive)
  (let ((pwd (my/vterm-get-pwd)))
    (kill-process vterm--process)
    (dired pwd)))
```

The second function is particularly handy because that way I can alternate between vterm and dired.

Keybindings:

```emacs-lisp
(with-eval-after-load 'vterm
  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(normal)
   "gd" #'my/vterm-dired-other-window
   "gD" #'my/vterm-dired-replace))
```


### Eshell {#eshell}

A shell written in Emacs lisp. I don't use it as of now, but keep the config just in case.

```emacs-lisp
(defun my/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 10000)
  (setq eshell-hist-ingnoredups t)
  (setq eshell-buffer-maximum-lines 10000)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (general-define-key
   :states '(normal)
   :keymaps 'eshell-mode-map
   (kbd "C-h") 'evil-window-left
   (kbd "C-l") 'evil-window-right
   (kbd "C-k") 'evil-window-up
   (kbd "C-j") 'evil-window-down))

(use-package eshell
  :ensure nil
  :after evil-collection
  :commands (eshell)
  :config
  (add-hook 'eshell-first-time-mode-hook 'my/configure-eshell 90)
  (when my/slow-ssh
    (add-hook 'eshell-mode-hook
	      (lambda ()
		(setq-local company-idle-delay 1000))))
  (setq eshell-banner-message ""))

(use-package aweshell
  :straight (:repo "manateelazycat/aweshell" :host github)
  :after eshell
  :config
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'epe-theme-pipeline))

(use-package eshell-info-banner
  :defer t
  :if (not my/slow-ssh)
  :straight (eshell-info-banner :type git
				:host github
				:repo "phundrak/eshell-info-banner.el")
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

(when my/slow-ssh
  (general-nmap "`" 'aweshell-dedicated-toggle)
  (general-nmap "~" 'eshell))
```


## Org Mode {#org-mode}

The best feature of Emacs. Just after every other best feature of Emacs, probably.

References:

-   [Org Mode homepage](https://orgmode.org/)
-   [Manual](https://orgmode.org/manual/)


### Installation & basic settings {#installation-and-basic-settings}

Use the built-in org mode.

```emacs-lisp
(use-package org
  :straight t
  :defer t
  :config
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively nil)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (visual-line-mode -1)
	      (toggle-truncate-lines 1)
	      (display-line-numbers-mode 0)))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (rainbow-delimiters-mode -1)))
  <<org-crypt-setup>>
  (unless my/is-termux
    <<org-lang-setup>>)
  <<org-ui-setup>>
  <<org-keys-setup>>
  <<org-productivity-setup>>)
```


#### Encryption {#encryption}

```emacs-lisp
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "C1EC867E478472439CC82410DE004F32AFA00205")
```


#### org-contrib {#org-contrib}

`org-contrib` is a package with various additions to Org. I use the following:

-   `ox-extra` - extensions for org export
-   `ol-notmuch` - integration with notmuch

<!--listend-->

```emacs-lisp
(use-package org-contrib
  :straight (org-contrib
	     :type git
	     :host nil
	     :repo "https://git.sr.ht/~bzg/org-contrib"
	     :build t)
  :after (org)
  :config
  (require 'ox-extra)
  (require 'ol-notmuch)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
```


### Integration with evil {#integration-with-evil}

```emacs-lisp
(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo))))
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
```


### Literate programing {#literate-programing}


#### Python & Jupyter {#python-and-jupyter}

Use jupyter kernels for Org Mode.

References:

-   [emacs-jupyter repo](https://github.com/nnicandro/emacs-jupyter)
-   [SCIMAX manual](https://github.com/jkitchin/scimax/blob/master/scimax.org)

<!--listend-->

```emacs-lisp
(use-package jupyter
  :straight t
  :if (not my/is-termux)
  :init
  (my-leader-def "ar" 'jupyter-run-repl))
```

Refresh kernelspecs.

Kernelspecs by default are hashed, so even switching Anaconda environments doesn't change the kernel (i.e. kernel from the first environment is run after the switch to the second one).

```emacs-lisp
(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))
```

Also, if some kernel wasn't present at the moment of the load of `emacs-jupyter`, it won't be added to the `org-src-lang-modes` list. E.g. I have Hy kernel installed in a separate Anaconda environment, so if Emacs hasn't been launched in this environment, I wouldn't be able to use `hy` in org-src blocks.

Fortunately, `emacs-jupyter` provides a function for that problem as well.

```emacs-lisp
(defun my/jupyter-refesh-langs ()
  "Refresh Jupyter languages"
  (interactive)
  (org-babel-jupyter-aliases-from-kernelspecs t))
```


#### Hy {#hy}

```emacs-lisp
(use-package ob-hy
  :straight t)
```


#### View HTML in browser {#view-html-in-browser}

Open HTML in the `begin_export` block with xdg-open.

```emacs-lisp
(setq my/org-view-html-tmp-dir "/tmp/org-html-preview/")

(use-package f
  :straight t)

(defun my/org-view-html ()
  (interactive)
  (let ((elem (org-element-at-point))
	(temp-file-path (concat my/org-view-html-tmp-dir (number-to-string (random (expt 2 32))) ".html")))
    (cond
     ((not (eq 'export-block (car elem)))
      (message "Not in an export block!"))
     ((not (string-equal (plist-get (car (cdr elem)) :type) "HTML"))
      (message "Export block is not HTML!"))
     (t (progn
	  (f-mkdir my/org-view-html-tmp-dir)
	  (f-write (plist-get (car (cdr elem)) :value) 'utf-8 temp-file-path)
	  (start-process "org-html-preview" nil "xdg-open" temp-file-path))))))
```


#### PlantUML {#plantuml}

```emacs-lisp
(setq org-plantuml-executable-path "/home/pavel/.guix-extra-profiles/emacs/emacs/bin/plantuml")
(setq org-plantuml-exec-mode 'plantuml)
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
```


#### Setup {#setup}

Enable languages

```emacs-lisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sql . t)
   ;; (typescript .t)
   (hy . t)
   (shell . t)
   (plantuml . t)
   (octave . t)
   (jupyter . t)))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
```

Use Jupyter block instead of built-in Python.

```emacs-lisp
(org-babel-jupyter-override-src-block "python")
(org-babel-jupyter-override-src-block "hy")
```

Turn of some minor modes in source blocks.

```emacs-lisp
(add-hook 'org-src-mode-hook
	  (lambda ()
	    ;; (hs-minor-mode -1)
	    ;; (electric-indent-local-mode -1)
	    ;; (rainbow-delimiters-mode -1)
	    (highlight-indent-guides-mode -1)))
```

Async code blocks evaluations. Jupyter blocks have a built-in async, so they are set as ignored.

```emacs-lisp
(use-package ob-async
  :straight t
  :after (org)
  :config
  (setq ob-async-no-async-languages-alist '("python" "hy" "jupyter-python" "jupyter-octave")))
```


#### Managing Jupyter kernels {#managing-jupyter-kernels}

Functions for managing local Jupyter kernels.

`my/insert-jupyter-kernel` inserts a path to an active Jupyter kernel to the buffer. Useful to quickly write a header like:

```text
#+PROPERTY: header-args:python :session <path-to-kernel>
```

`my/jupyter-connect-repl` opens a `emacs-jupyter` REPL, connected to an active kernel. `my/jupyter-qtconsole` runs a standalone Jupyter QtConsole.

Requirements: `ss`

```emacs-lisp
(setq my/jupyter-runtime-folder (expand-file-name "~/.local/share/jupyter/runtime"))

(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))

(defun my/list-jupyter-kernel-files ()
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes my/jupyter-runtime-folder t ".*kernel.*json$")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
	(files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: "
     (seq-filter
      (lambda (file)
	(member (cdr file) ports))
      files))))

(defun my/insert-jupyter-kernel ()
  "Insert a path to an active Jupyter kernel into the buffer"
  (interactive)
  (insert (my/select-jupyter-kernel)))

(defun my/jupyter-connect-repl ()
  "Open an emacs-jupyter REPL, connected to a Jupyter kernel"
  (interactive)
  (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

(defun my/jupyter-qtconsole ()
  "Open Jupyter QtConsole, connected to a Jupyter kernel"
  (interactive)
  (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
		 (file-name-nondirectory (my/select-jupyter-kernel))))
```

I've also noticed that there are JSON files left in the runtime folder whenever the kernel isn't stopped correctly. So here is a cleanup function.

```emacs-lisp
(defun my/jupyter-cleanup-kernels ()
  (interactive)
  (let* ((ports (my/get-open-ports))
	 (files (my/list-jupyter-kernel-files))
	 (to-delete (seq-filter
		     (lambda (file)
		       (not (member (cdr file) ports)))
		     files)))
    (when (and (length> to-delete 0)
	       (y-or-n-p (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
	(delete-file (car file))))))
```


#### Do not wrap the output in emacs-jupyter {#do-not-wrap-the-output-in-emacs-jupyter}

Emacs-jupyter has its own insertion mechanisms, which always prepends output statements with `:`. That is not desirable in cases where a kernel supports only plain output, e.g. calysto\_hy kernel.

So there we have a minor mode that overrides this behavior.

```emacs-lisp
(defun my/jupyter-org-scalar (value)
  (cond
   ((stringp value) value)
   (t (jupyter-org-scalar value))))

(define-minor-mode my/emacs-jupyter-raw-output
  "Make emacs-jupyter do raw output")

(defun my/jupyter-org-scalar-around (fun value)
  (if my/emacs-jupyter-raw-output
      (my/jupyter-org-scalar value)
    (funcall fun value)))

(advice-add 'jupyter-org-scalar :around #'my/jupyter-org-scalar-around)
```


#### Wrap source code output {#wrap-source-code-output}

A function to remove :RESULTS: drawer from the results. Once again, necessary because emacs-jupyter doesn't seem to respect :results raw.

```emacs-lisp
(defun my/org-strip-results (data)
  (replace-regexp-in-string ":\\(RESULTS\\|END\\):\n" "" data))
```

And an all-in-one function to:

-   prepend `#+NAME:` and `#+CAPTION:` to the source block output. Useful if the output is an image.
-   strip :RESULTS: drawer from the output, if necessary
-   wrap results in the `src` block

As for now, looks sufficient to format source code outputs to get a tolerable LaTeX.

```emacs-lisp
(defun my/org-caption-wrap (data &optional name caption attrs strip-drawer src-wrap)
  (let* ((data-s (if (and strip-drawer (not (string-empty-p strip-drawer)))
		     (my/org-strip-results data)
		   data))
	 (drawer-start (if (string-match-p "^:RESULTS:.*" data-s) 10 0)))
    (concat
     (substring data-s 0 drawer-start)
     (and name (not (string-empty-p name)) (concat "#+NAME:" name "\n"))
     (and caption (not (string-empty-p caption)) (concat "#+CAPTION:" caption "\n"))
     (and attrs (not (string-empty-p attrs)) (concat "#+ATTR_LATEX:" attrs "\n"))
     (if (and src-wrap (not (string-empty-p src-wrap)))
	 (concat "#+begin_src " src-wrap "\n"
		 (substring data-s drawer-start)
		 (when (not (string-match-p ".*\n" data-s)) "\n")
		 "#+end_src")
       (substring data-s drawer-start)))))
```

To use, add the following snippet to the org file:

```text
#+NAME: out_wrap
#+begin_src emacs-lisp :var data="" caption="" name="" attrs="" strip-drawer="" src-wrap="" :tangle no :exports none
(my/org-caption-wrap data name caption attrs strip-drawer src-wrap)
#+end_src
```

Example usage:

```text
:post out_wrap(name="fig:chart", caption="График", data=*this*)
```


### Productivity & Knowledge management {#productivity-and-knowledge-management}

My ongoing effort to get a productivity setup in Org.

Some inspiration:

-   [Nicolas P. Rougier. Get Things Done with Emacs](https://www.labri.fr/perso/nrougier/GTD/index.html)
-   [Jetro Kuan. Org-mode Workflow](https://blog.jethro.dev/posts/org%5Fmode%5Fworkflow%5Fpreview/)
-   [Alexey Shmalko: How I note](https://www.alexeyshmalko.com/how-i-note/)
-   [Rohit Goswami: An Orgmode Note Workflow](https://rgoswami.me/posts/org-note-workflow/)

Used files

```emacs-lisp
(setq org-directory (expand-file-name "~/Documents/org-mode"))
(setq org-agenda-files '("inbox.org" "projects.org" "work.org"))
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
```

Hotkeys

```emacs-lisp
(my-leader-def
  :infix "o"
  "" '(:which-key "org-mode")
  "c" 'org-capture
  "a" 'org-agenda)
```

Refile targets

```emacs-lisp
(setq org-refile-targets
      '(("projects.org" :maxlevel . 2)
	("work.org" :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
```


#### Capture templates & various settings {#capture-templates-and-various-settings}

Settings for Org capture mode. The goal here is to have a non-disruptive process to capture various ideas.

```emacs-lisp
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
	 ,(concat "* TODO %?\n"
		  "/Entered on/ %U"))
	("e" "email" entry (file "inbox.org")
	 ,(concat "* TODO %:from %:subject \n"
		  "/Entered on/ %U\n"
		  "/Received on/ %:date-timestamp-inactive\n"
		  "%a\n"))
	("f" "elfeed" entry (file "inbox.org")
	 ,(concat "* TODO %:elfeed-entry-title\n"
		  "/Entered on/ %U\n"
		  "%a\n"))))
```

Effort estimation

```emacs-lisp
(add-to-list 'org-global-properties
	     '("Effort_ALL" . "0 0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00"))
```

Log DONE time

```emacs-lisp
(setq org-log-done 'time)
```


#### Custom agendas {#custom-agendas}

```emacs-lisp
(defun my/org-scheduled-get-time ()
  (let ((scheduled (org-get-scheduled-time (point))))
    (if scheduled
	(format-time-string "%Y-%m-%d" scheduled)
      "")))

(setq org-agenda-custom-commands
      `(("p" "My outline"
	 ((agenda "")
	  (todo "NEXT"
		((org-agenda-prefix-format "  %i %-12:c [%e] ")
		 (org-agenda-overriding-header "Next tasks")))
	  (tags-todo "inbox"
		     ((org-agenda-overriding-header "Inbox")
		      (org-agenda-prefix-format " %i %-12:c")
		      (org-agenda-hide-tags-regexp ".")))
	  (tags-todo "+waitlist+SCHEDULED<=\"<+14d>\""
		     ((org-agenda-overriding-header "Waitlist")
		      (org-agenda-hide-tags-regexp "waitlist")
		      (org-agenda-prefix-format " %i %-12:c %-12(my/org-scheduled-get-time)")))))
	("tp" "Personal tasks"
	 ((tags-todo "personal"
		     ((org-agenda-prefix-format "  %i %-12:c [%e] ")))))))
```


#### Org Journal {#org-journal}

[org-journal](https://github.com/bastibe/org-journal) is a plugin for maintaining a journal in org mode. I want to have its entries separate from my knowledge base.

```emacs-lisp
(use-package org-journal
  :straight t
  :after org
  :config
  (setq org-journal-dir (concat org-directory "/journal"))
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-enable-encryption t))

(my-leader-def
  :infix "oj"
  "" '(:which-key "org-journal")
  "j" 'org-journal-new-entry
  "o" 'org-journal-open-current-journal-file
  "s" 'org-journal-search)
```

Also, I want to store some information in the journal as properties of the record. So below is a function which does just that.

As of now, it stores Emacs version, hostname, location and current EMMS track if there is one.

```emacs-lisp
(defun my/set-journal-header ()
  (org-set-property "Emacs" emacs-version)
  (org-set-property "Hostname" system-name)
  (when (boundp 'my/location)
    (org-set-property "Location" my/location))
  (when (fboundp 'emms-playlist-current-selected-track)
    (let ((track (emms-playlist-current-selected-track)))
      (when track
	(let ((album (cdr (assoc 'info-album track)))
	      (artist (or (cdr (assoc 'info-albumartist track))
			  (cdr (assoc 'info-album track))))
	      (title (cdr (assoc 'info-title track)))
	      (string ""))
	  (when artist
	    (setq string (concat string "[" artist "] ")))
	  (when album
	    (setq string (concat string album " - ")))
	  (when title
	    (setq string (concat string title)))
	  (when (> (length string) 0)
	    (org-set-property "EMMS_Track" string)))))))

(add-hook 'org-journal-after-entry-create-hook
	  #'my/set-journal-header)
```


#### Org Roam {#org-roam}

[org-roam](https://github.com/org-roam/org-roam) is a plain-text knowledge database.

| Guix dependency       |
|-----------------------|
| emacs-emacsql-sqlite3 |
| graphviz              |

References:

-   [Hitchhiker's Rough Guide to Org roam V2](https://github.com/org-roam/org-roam/wiki/Hitchhiker%27s-Rough-Guide-to-Org-roam-V2)

<!--listend-->

```emacs-lisp
(use-package emacsql-sqlite
  :defer t
  :straight (:type built-in))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
	     :files (:defaults "extensions/*.el"))
  :after org
  :init
  (setq org-roam-directory (concat org-directory "/roam"))
  (setq org-roam-file-extensions '("org"))
  (setq org-roam-v2-ack t)
  (setq orb-insert-interface 'ivy-bibtex)
  :config
  (org-roam-setup)
  (setq org-roam-capture-templates
	`(("d" "default" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t)))
  (require 'org-roam-protocol))

(my-leader-def
  :infix "or"
  "r" '(:which-key "org-roam")
  "i" 'org-roam-node-insert
  "r" 'org-roam-node-find
  "g" 'org-roam-graph
  "c" 'org-roam-capture
  "b" 'org-roam-buffer-toggle)

(with-eval-after-load 'org
  (my-leader-def
    :keymap 'org-mode-map
    :infix "or"
    "t" 'org-roam-tag-add
    "T" 'org-toam-tag-remove)
  (general-define-key
   :keymap 'org-mode-map
   "C-c i" 'org-id-get-create
   "C-c l o" 'org-roam-node-insert))
```


##### org-roam-ui {#org-roam-ui}

A browser frontend to visualize a Roam directory in a form of a graph.

```emacs-lisp
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;; :hook (org-roam . org-roam-ui-mode)
  :init
  (my-leader-def "oru" #'org-roam-ui-mode))
```


##### org-roam-protocol {#org-roam-protocol}

Open links such as `org-protocol://` from browser. Run `M-x server-start` for org-protocol to work.

```conf
[Desktop Entry]
Name=Org-Protocol
Exec=emacsclient %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/org-protocol
```

Don't forget to run the following after setup:

```bash
xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
```


#### org-ref {#org-ref}

| Type | Description                     |
|------|---------------------------------|
| TODO | Figure out how not to load Helm |

[org-ref](https://github.com/jkitchin/org-ref) is a package that provides support for various citations & references in Org mode.

Useful to use BibTeX citations in LaTeX export.

As of now, this package loads Helm on start. To avoid this, I have to exclude Helm from the `Package-requires` in the [org-ref.el](.emacs.d/straight/repos/org-ref/org-ref.el) file. I haven't found a way to do this without modifying the package source yet.

```emacs-lisp
(use-package org-ref
  :straight (:files (:defaults (:exclude "*helm*")))
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq bibtex-dialect 'biblatex)
  (setq org-ref-default-bibliography '("~/Documents/org-mode/bibliography.bib"))
  (setq reftex-default-bibliography org-ref-default-bibliography)
  (setq bibtex-completion-bibliography org-ref-default-bibliography)
  :after (org)
  :config
  (general-define-key
   :keymaps 'org-mode-map
   :infix "C-c l"
   "" '(:which-key "org-ref")
   "l" 'org-ref-ivy-insert-cite-link
   "r" 'org-ref-ivy-insert-ref-link
   "h" 'org-ref-cite-hydra/body)
  (general-define-key
   :keymaps 'bibtex-mode-map
   "M-RET" 'org-ref-bibtex-hydra/body)
  (add-to-list 'orhc-candidate-formats
	       '("online" . "  |${=key=}| ${title} ${url}")))
```


#### org-roam-bibtex {#org-roam-bibtex}

Integration with bibtex and org-ref.

There are some problems with org roam v2, so I disabled it as of now. I will probably use another way of managing bibliography notes anyway.

```emacs-lisp
(use-package org-roam-bibtex
  :straight (:host github :repo "org-roam/org-roam-bibtex")
  :after (org-roam org-ref)
  :disabled
  :config
  (org-roam-bibtex-mode))
```


### UI {#ui}


#### <span class="org-todo done OFF">OFF</span> (OFF) Instant equations preview {#off--instant-equations-preview}

Instant math previews for org mode.

References:

-   [org-latex-impatient repo](https://github.com/yangsheng6810/org-latex-impatient)

<!--listend-->

```emacs-lisp
(use-package org-latex-impatient
  :straight (:repo "yangsheng6810/org-latex-impatient"
		   :branch "master"
		   :host github)
  :hook (org-mode . org-latex-impatient-mode)
  :disabled
  :init
  (setq org-latex-impatient-tex2svg-bin
	"/home/pavel/Programs/miniconda3/lib/node_modules/mathjax-node-cli/bin/tex2svg")
  (setq org-latex-impatient-scale 1.75)
  (setq org-latex-impatient-delay 1)
  (setq org-latex-impatient-border-color "#ffffff"))
```


#### LaTeX fragments {#latex-fragments}

A function to enable LaTeX native highlighting. Not setting this as default, because it loads LaTeX stuff.

```emacs-lisp
(defun my/enable-org-latex ()
  (interactive)
  (customize-set-variable 'org-highlight-latex-and-related '(native))
  (add-hook 'org-mode-hook (lambda () (yas-activate-extra-mode 'LaTeX-mode)))
  (sp-local-pair 'org-mode "$" "$")
  (sp--remove-local-pair "'"))
```

Call the function before opening an org file or reopen a buffer after calling the function.

Scale latex fragments preview.

```emacs-lisp
(setq my/org-latex-scale 1.75)
(setq org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale))
```

Also, LaTeX fragments preview tends to break whenever the are custom `#+LATEX_HEADER` entries. To circumvent this, I add a custom header and modify the `org-preview-latex-process-alist` variable

```emacs-lisp
(setq my/latex-preview-header "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage{hyperref}
\\pagestyle{empty}")

(setq org-preview-latex-process-alist
      (mapcar
       (lambda (item)
	 (cons
	  (car item)
	  (plist-put (cdr item) :latex-header my/latex-preview-header)))
       org-preview-latex-process-alist))
```


#### Better headers {#better-headers}

```emacs-lisp
(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode))
```


#### Org Agenda Icons {#org-agenda-icons}

Categories are broad labels to group agenda items.

```emacs-lisp
(if (not my/lowpower)
    (setq org-agenda-category-icon-alist
	  `(("inbox" ,(list (all-the-icons-faicon "inbox")) nil nil :ascent center)
	    ("work" ,(list (all-the-icons-faicon "cog")) nil nil :ascent center)
	    ("education" ,(list (all-the-icons-material "build")) nil nil :ascent center)
	    ("personal" ,(list (all-the-icons-faicon "music")) nil nil :ascent center)
	    ("misc" ,(list (all-the-icons-material "archive")) nil nil :ascent center)
	    ;; ("lesson" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
	    ;; ("meeting" ,(list (all-the-icons-material "chat")) nil nil :ascent center)
	    ;; ("event" ,(list (all-the-icons-octicon "clock")) nil nil :ascent center)
	    ("." ,(list (all-the-icons-faicon "circle-o")) nil nil :ascent center))))
```


### Export {#export}


#### General settings {#general-settings}

```emacs-lisp
(setq org-export-backends '(md html latex beamer org))
```


#### Hugo {#hugo}

```emacs-lisp
(use-package ox-hugo
  :straight t
  :after ox)
```


#### Jupyter Notebook {#jupyter-notebook}

```emacs-lisp
(use-package ox-ipynb
  :straight (:host github :repo "jkitchin/ox-ipynb")
  :after ox)
```


#### Html export {#html-export}

```emacs-lisp
(use-package htmlize
  :straight t
  :after ox
  :config
  (setq org-html-htmlize-output-type 'css))
```


#### LaTeX {#latex}

Add a custom LaTeX template without default packages. Packages are indented to be imported with function from [Import \*.sty](#import-dot-sty).

```emacs-lisp
(defun my/setup-org-latex ()
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-compiler "xelatex") ;; Probably not necessary
  (setq org-latex-pdf-process '("latexmk -outdir=%o %f")) ;; Use latexmk
  (setq org-latex-listings 'minted) ;; Use minted to highlight source code
  (setq org-latex-minted-options    ;; Some minted options I like
	'(("breaklines" "true")
	  ("tabsize" "4")
	  ("autogobble")
	  ("linenos")
	  ("numbersep" "0.5cm")
	  ("xleftmargin" "1cm")
	  ("frame" "single")))
  ;; Use extarticle without the default packages
  (add-to-list 'org-latex-classes
	       '("org-plain-extarticle"
		 "\\documentclass{extarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; Use beamer without the default packages
  (add-to-list 'org-latex-classes
	       '("org-latex-beamer"
		 "\\documentclass{beamer}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
		 ("beamer" "\\documentclass[presentation]{beamer}"
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;; Make sure to eval the function when org-latex-classes list already exists
(with-eval-after-load 'ox-latex
  (my/setup-org-latex))
```


### Keybindings & stuff {#keybindings-and-stuff}

```emacs-lisp
(general-define-key
 :keymaps 'org-mode-map
 "C-c d" 'org-decrypt-entry
 "C-c e" 'org-encrypt-entry
 "M-p" 'org-latex-preview
 "M-o" 'org-redisplay-inline-images)

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal emacs)
 "L" 'org-shiftright
 "H" 'org-shiftleft
 "S-<next>" 'org-next-visible-heading
 "S-<prior>" 'org-previous-visible-heading
 "M-0" 'org-next-visible-heading
 "M-9" 'org-previous-visible-heading
 "M-]" 'org-babel-next-src-block
 "M-[" 'org-babel-previous-src-block)

(general-define-key
 :keymaps 'org-agenda-mode-map
 "M-]" 'org-agenda-later
 "M-[" 'org-agenda-earlier)

;; (general-imap :keymaps 'org-mode-map "RET" 'evil-org-return)
(general-nmap :keymaps 'org-mode-map "RET" 'org-ctrl-c-ctrl-c)

;; (my-leader-def "aa" 'org-agenda)
```


#### Copy a link {#copy-a-link}

```emacs-lisp
(defun my/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
	  (type (org-element-property :type link))
	  (url (org-element-property :path link))
	  (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(general-nmap :keymaps 'org-mode-map
    "C-x C-l" 'my/org-link-copy)
```


### Presentations {#presentations}

Doing presentations with [org-present](https://github.com/rlister/org-present).

```emacs-lisp
(use-package hide-mode-line
  :straight t
  :after (org-present))

(defun my/present-next-with-latex ()
  (interactive)
  (org-present-next)
  (org-latex-preview '(16)))

(defun my/present-prev-with-latex ()
  (interactive)
  (org-present-prev)
  (org-latex-preview '(16)))

(use-package org-present
  :straight (:host github :repo "rlister/org-present")
  :commands (org-present)
  :config
  (general-define-key
   :keymaps 'org-present-mode-keymap
   "<next>" 'my/present-next-with-latex
   "<prior>" 'my/present-prev-with-latex)
  (add-hook 'org-present-mode-hook
	    (lambda ()
	      (blink-cursor-mode 0)
	      (org-present-big)
	      ;; (org-display-inline-images)
	      (org-present-hide-cursor)
	      (org-present-read-only)
	      (display-line-numbers-mode 0)
	      (hide-mode-line-mode +1)
	      (setq-local org-format-latex-options
			  (plist-put org-format-latex-options
				     :scale (* org-present-text-scale my/org-latex-scale 0.5)))
	      (org-latex-preview '(16))
	      (tab-bar-mode 0)))
  (add-hook 'org-present-mode-quit-hook
	    (lambda ()
	      (blink-cursor-mode 1)
	      (org-present-small)
	      ;; (org-remove-inline-images)
	      (org-present-show-cursor)
	      (org-present-read-write)
	      (display-line-numbers-mode 1)
	      (hide-mode-line-mode 0)
	      (setq-local org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale))
	      (org-latex-preview '(64))
	      (tab-bar-mode 1))))
```


### TOC {#toc}

Make a TOC inside the org file.

References:

-   [alphapapa/org-make-toc](https://github.com/alphapapa/org-make-toc)

<!--listend-->

```emacs-lisp
(use-package org-make-toc
  :after (org)
  :commands
  (org-make-toc
   org-make-toc-insert
   org-make-toc-set
   org-make-toc-at-point)
  :straight t)
```


### System configuration {#system-configuration}

Functions used across my literate config files.


#### Tables for Guix Dependencies {#tables-for-guix-dependencies}

A function to extract Guix dependencies from the org file.

-   If column name matches `[G|g]uix.*dep`, its contents will be added to the result.
-   If `CATEGORY` is passed, a column with name `[C|c]ategory` will be used to filter results. That way one file can be used to produce multiple manifests.
-   If `CATEGORY` is not passed, entries with non-empty category will be filtered out
-   If there is a `[D|d]isabled` column, entries that have a non-empty value in this column will be filtered out.

<!--listend-->

```emacs-lisp
(defun my/extract-guix-dependencies (&optional category)
  (let ((dependencies '()))
    (org-table-map-tables
     (lambda ()
       (let* ((table
	       (seq-filter
		(lambda (q) (not (eq q 'hline)))
		(org-table-to-lisp)))
	      (dep-name-index
	       (cl-position
		nil
		(mapcar #'substring-no-properties (nth 0 table))
		:test (lambda (_ elem)
			(string-match-p "[G|g]uix.*dep" elem))))
	      (category-name-index
	       (cl-position
		nil
		(mapcar #'substring-no-properties (nth 0 table))
		:test (lambda (_ elem)
			(string-match-p ".*[C|c]ategory.*" elem))))
	      (disabled-name-index
	       (cl-position
		nil
		(mapcar #'substring-no-properties (nth 0 table))
		:test (lambda (_ elem)
			(string-match-p ".*[D|d]isabled.*" elem)))))
	 (when dep-name-index
	   (dolist (elem (cdr table))
	     (when
		 (and
		  ;; Category
		  (or
		   ;; Category not set and not present in the table
		   (and
		    (or (not category) (string-empty-p category))
		    (not category-name-index))
		   ;; Category is set and present in the table
		   (and
		    category-name-index
		    (not (string-empty-p category))
		    (string-match-p category (nth category-name-index elem))))
		  ;; Not disabled
		  (or
		   (not disabled-name-index)
		   (string-empty-p (nth disabled-name-index elem))))
	       (add-to-list
		'dependencies
		(substring-no-properties (nth dep-name-index elem)))))))))
    dependencies))
```

Now, join the dependencies list to make it compatible with Scheme:

```emacs-lisp
(defun my/format-guix-dependencies (&optional category)
  (mapconcat
   (lambda (e) (concat "\"" e "\""))
   (my/extract-guix-dependencies category)
   "\n"))
```


#### Noweb evaluations {#noweb-evaluations}

Turn off eval confirmations for configuration files.

```emacs-lisp
(setq my/org-config-files
      '("/home/pavel/Emacs.org"
	"/home/pavel/Desktop.org"
	"/home/pavel/Console.org"
	"/home/pavel/Guix.org"
	"/home/pavel/Mail.org"))

(add-hook 'org-mode-hook
	  (lambda ()
	    (when (member (buffer-file-name) my/org-config-files)
	      (setq-local org-confirm-babel-evaluate nil))))
```


#### yadm hook {#yadm-hook}

A script to run tangle from CLI.

```emacs-lisp
(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; Do not ask to confirm evaluations
(setq org-confirm-babel-evaluate nil)

<<guix-tables>>

;; A few dummy modes to avoid being prompted for comment systax
(define-derived-mode fish-mode prog-mode "Fish"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

(define-derived-mode yaml-mode text-mode "YAML"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *"))

(mapcar #'org-babel-tangle-file
	'("/home/pavel/Emacs.org"
	  "/home/pavel/Desktop.org"
	  "/home/pavel/Console.org"
	  "/home/pavel/Guix.org"
	  "/home/pavel/Mail.org"))
```

To launch from CLI, run:

```bash
emacs -Q --batch -l run-tangle.el
```

I have added this line to yadm's `post_alt` hook, so tangle is run after `yadm alt`


## <span class="org-todo done OFF">OFF</span> (OFF) EAF {#off--eaf}

[Emacs Application Framework](https://github.com/manateelazycat/emacs-application-framework) provides a way to integrate PyQt applications with Emacs.

I've made it work, but don't find any uses cases for me at the moment


### Installation {#installation}

Requirements: Node >= 14

```bash
pip install qtconsole markdown qrcode[pil] PyQt5 PyQtWebEngine
```


### Config {#config}

```emacs-lisp
(use-package eaf
  :straight (:host github :repo "manateelazycat/emacs-application-framework" :files ("*"))
  :init
  (use-package epc :defer t :straight t)
  (use-package ctable :defer t :straight t)
  (use-package deferred :defer t :straight t)
  :config
  (require 'eaf-evil)
  (setq eaf-evil-leader-key "SPC"))
```


## Programming {#programming}


### General setup {#general-setup}


#### LSP {#lsp}

LSP-mode provides an IDE-like experience for Emacs - real-time diagnostic, code actions, intelligent autocompletion, etc.

References:

-   [lsp-mode homepage](https://emacs-lsp.github.io/lsp-mode/)


##### Setup {#setup}

```emacs-lisp
(use-package lsp-mode
  :straight t
  :if (not (or my/slow-ssh my/is-termux))
  :hook (
	 (typescript-mode . lsp)
	 (vue-mode . lsp)
	 (go-mode . lsp)
	 (svelte-mode . lsp)
	 ;; (python-mode . lsp)
	 (json-mode . lsp)
	 (haskell-mode . lsp)
	 (haskell-literate-mode . lsp)
	 (java-mode . lsp)
	 ;; (csharp-mode . lsp)
	 )
  :commands lsp
  :config
  (setq lsp-idle-delay 1)
  (setq lsp-eslint-server-command '("node" "/home/pavel/.emacs.d/.cache/lsp/eslint/unzipped/extension/server/out/eslintServer.js" "--stdio"))
  (setq lsp-eslint-run "onSave")
  (setq lsp-signature-render-documentation nil)
  ;; (lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte")))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 2)
  (setq lsp-ui-sideline-show-hover nil))
```


##### Integrations {#integrations}

The only integration left now is treemacs.

Origami should've leveraged LSP folding, but it was too unstable at the moment I tried it.

```emacs-lisp
;; (use-package helm-lsp
;;   :straight t
;;   :commands helm-lsp-workspace-symbol)

;; (use-package origami
;;   :straight t
;;   :hook (prog-mode . origami-mode))

;; (use-package lsp-origami
;;   :straight t
;;   :config
;;   (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)
```


##### Keybindings {#keybindings}

```emacs-lisp
(my-leader-def
  :infix "l"
  "" '(:which-key "lsp")
  "d" 'lsp-ui-peek-find-definitions
  "r" 'lsp-rename
  "u" 'lsp-ui-peek-find-references
  "s" 'lsp-ui-find-workspace-symbol
  "l" 'lsp-execute-code-action
  "e" 'list-flycheck-errors)
```


#### Flycheck {#flycheck}

A syntax checking extension for Emacs. Integrates with LSP-mode, but can also use various standalone checkers.

References:

-   [Flycheck homepage](https://www.flycheck.org/en/latest/)

<!--listend-->

```emacs-lisp
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled))
  ;; (add-hook 'evil-insert-state-exit-hook
  ;;           (lambda ()
  ;;             (if flycheck-checker
  ;;                 (flycheck-buffer))
  ;;             ))
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.33))))
```


#### Tree Sitter {#tree-sitter}

An incremental code parsing system, constructing a syntax tree at runtime.

Right now it doesn't do much except provide a better syntax highlighting than regexes, but this integration is a rather recent development. There are already some major modes built on top of this thing.

Also, it seems to break if ran from mmm-mode, so there is a small workaround.

References:

-   [Tree-sitter library](https://tree-sitter.github.io/tree-sitter/)
-   [Emacs Tree-sitter](https://ubolonton.github.io/emacs-tree-sitter/)

<!--listend-->

```emacs-lisp
(defun my/tree-sitter-if-not-mmm ()
  (when (not (and (boundp 'mmm-temp-buffer-name)
		  (string-equal mmm-temp-buffer-name (buffer-name))))
    (tree-sitter-mode)
    (tree-sitter-hl-mode)))

(use-package tree-sitter
  :straight t
  :hook ((typescript-mode . my/tree-sitter-if-not-mmm)
	 (js-mode . my/tree-sitter-if-not-mmm)
	 (python-mode . tree-sitter-mode)
	 (python-mode . tree-sitter-hl-mode)
	 (csharp-mode . tree-sitter-mode)))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)
```


#### <span class="org-todo done OFF">OFF</span> (OFF) DAP {#off--dap}

An Emacs client for Debugger Adapter Protocol.

I don't use it now, because there are debuggers I like more for the technologies I'm currently using.

References:

-   [dap-mode homepage](https://emacs-lsp.github.io/dap-mode/)

<!--listend-->

```emacs-lisp
(use-package dap-mode
  :straight t
  :defer t
  :init
  (setq lsp-enable-dap-auto-configure nil)
  :config

  (setq dap-ui-variable-length 100)
  (require 'dap-node)
  (dap-node-setup)

  (require 'dap-chrome)
  (dap-chrome-setup)

  (require 'dap-python)

  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(my-leader-def
  :infix "d"
  "d" 'dap-debug
  "b" 'dap-breakpoint-toggle
  "c" 'dap-breakpoint-condition
  "wl" 'dap-ui-locals
  "wb" 'dap-ui-breakpoints
  "wr" 'dap-ui-repl
  "ws" 'dap-ui-sessions
  "we" 'dap-ui-expressions)

(my-leader-def
  :infix "d"
  :keymaps 'dap-mode-map
  "h" 'dap-hydra)

(defun my/dap-yank-value-at-point (node)
  (interactive (list (treemacs-node-at-point)))
  (kill-new (message (plist-get (button-get node :item) :value))))
```


#### <span class="org-todo done OFF">OFF</span> (OFF) TabNine {#off--tabnine}

A ML-based autocompletion system.

More often than not gives really good results, but is slow as hell & consumes a lot of RAM. Also, LSP-provided completions were more useful in my experience.

References:

-   [TabNine Homepage](https://www.tabnine.com/)

<!--listend-->

```emacs-lisp
(use-package company-tabnine
  :straight t
  :if (not my/lowpower)
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))
```


#### <span class="org-todo done OFF">OFF</span> (OFF) Code Compass {#off--code-compass}

A set of code analyzing tools.

References:

-   [code-compass repo](https://github.com/ag91/code-compass)


##### Dependencies {#dependencies}

```emacs-lisp
(use-package async
  :straight t)
(use-package dash
  :straight t)
(use-package f
  :straight t)
(use-package s
  :straight t)
(use-package simple-httpd
  :straight t)
```


##### Plugin {#plugin}

```emacs-lisp
(use-package code-compass
  :straight (
  :repo "ag91/code-compass"
  :files ("code-compass.el")
  :branch "main"
  ))
```


#### <span class="org-todo todo CHECK">CHECK</span> (OFF) Format-all {#off--format-all}

```emacs-lisp
(use-package format-all
  :straight t)
```


#### General additional config {#general-additional-config}

Make smartparens behave the way I like for C-like languages.

```emacs-lisp
(defun my/set-smartparens-indent (mode)
  (sp-local-pair mode "{" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "[" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "(" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET"))))
```

Override flycheck checker with eslint.

```emacs-lisp
(defun my/set-flycheck-eslint()
  "Override flycheck checker with eslint."
  (setq-local lsp-diagnostic-package :none)
  (setq-local flycheck-checker 'javascript-eslint))
```


### Web development {#web-development}

Configs for various web development technologies I'm using.


#### Emmet {#emmet}

[Emmet](https://emmet.io/) is a toolkit which greatly speeds up typing HTML & CSS.

| Type | Note                                              |
|------|---------------------------------------------------|
| TODO | Do not enable for every Svelte mode               |
| TODO | make expand div[disabled] as <div disabled></div> |

My bit of config here:

-   makes Emmet activate only in certain mmm-mode submodes.
-   makes `TAB` the only key I have to use

<!--listend-->

```emacs-lisp
(use-package emmet-mode
  :straight t
  :hook ((vue-html-mode . emmet-mode)
	 (svelte-mode . emmet-mode)
	 (web-mode . emmet-mode)
	 (html-mode . emmet-mode)
	 (css-mode . emmet-mode)
	 (scss-mode . emmet-mode))
  :config
  ;; (setq emmet-indent-after-insert nil)
  (setq my/emmet-mmm-submodes '(vue-html-mode css-mode))
  (defun my/emmet-or-tab (&optional arg)
    (interactive)
    (if (and
	 (boundp 'mmm-current-submode)
	 mmm-current-submode
	 (not (member mmm-current-submode my/emmet-mmm-submodes)))
	(indent-for-tab-command arg)
      (or (emmet-expand-line arg)
	  (emmet-go-to-edit-point 1)
	  (indent-for-tab-command arg))))
  (general-imap :keymaps 'emmet-mode-keymap
    "TAB" 'my/emmet-or-tab
    "<backtab>" 'emmet-prev-edit-point))
```


#### Prettier {#prettier}

```emacs-lisp
(use-package prettier
  :commands (prettier-prettify)
  :straight t
  :init
  (my-leader-def
    :keymaps '(js-mode-map web-mode-map typescript-mode-map vue-mode-map svelte-mode-map)
    "rr" #'prettier-prettify))
```


#### TypeScript {#typescript}

```emacs-lisp
(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-mode-hook #'smartparens-mode)
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'typescript-mode-hook #'hs-minor-mode)
  (my/set-smartparens-indent 'typescript-mode))
```


#### JavaScript {#javascript}

```emacs-lisp
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'js-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'js-mode)
```


#### Jest {#jest}

```emacs-lisp
(use-package jest-test-mode
  :straight t
  :hook ((typescript-mode . jest-test-mode)
	 (js-mode . jest-test-mode))
  :config
  (my-leader-def
    :keymaps 'jest-test-mode-map
    :infix "t"
    "t" 'jest-test-run-at-point
    "r" 'jest-test-run
    "a" 'jest-test-run-all-tests))
```

```emacs-lisp
(defun my/jest-test-run-at-point-copy ()
  "Run the top level describe block of the current buffer's point."
  (interactive)
  (let ((filename (jest-test-find-file))
	(example  (jest-test-example-at-point)))
    (if (and filename example)
	(jest-test-from-project-directory filename
	  (let ((jest-test-options (seq-concatenate 'list jest-test-options (list "-t" example))))
	    (kill-new (jest-test-command filename))))
      (message jest-test-not-found-message))))
```


#### web-mode {#web-mode}

[web-mode.el](https://web-mode.org/) is a major mode to edit various web templates.

Trying this one out instead of vue-mode and svelte-mode, because this one seems to have better support for tree-sitter and generally less problems.

```emacs-lisp
(use-package web-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'smartparens-mode)
  (add-hook 'web-mode-hook 'hs-minor-mode)
  (my/set-smartparens-indent 'web-mode)
  (add-hook 'web-mode-hook ))
```

Hooking this up with lsp.

```emacs-lisp
(setq my/web-mode-lsp-extensions
      `(,(rx ".svelte" eos)
	,(rx ".vue" eos)))

(defun my/web-mode-lsp ()
  (when (seq-some
	 (lambda (regex) (string-match-p regex (buffer-name)))
	 my/web-mode-lsp-extensions)
    (lsp-deferred)))

(add-hook 'web-mode-hook #'my/web-mode-lsp)
```

Vue settings

```emacs-lisp
(defun my/web-mode-vue-setup ()
  (when (string-match-p (rx ".vue" eos) (buffer-name))
    (setq-local web-mode-script-padding 0)))

(add-hook 'web-mode-hook 'my/web-mode-vue-setup)
```


#### <span class="org-todo done OFF">OFF</span> (OFF) Vue.js {#off--vue-dot-js}

```emacs-lisp
(use-package vue-mode
  :straight t
  :disabled
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'hs-minor-mode)
  (add-hook 'vue-mode-hook #'smartparens-mode)
  (my/set-smartparens-indent 'vue-mode)
  (add-hook 'vue-mode-hook (lambda () (set-face-background 'mmm-default-submode-face nil)))
  <<override-mmm-mode-func>>)

(with-eval-after-load 'editorconfig
  (add-to-list 'editorconfig-indentation-alist
	       '(vue-mode css-indent-offset
			  js-indent-level
			  sgml-basic-offset
			  ssass-tab-width
			  typescript-indent-level
			  emmet-indentation
			  vue-html-extra-indent)))
```


##### mmm-mode fix {#mmm-mode-fix}

References:

-   [mmm-mode issue](https://github.com/purcell/mmm-mode/issues/112)

<!--listend-->

```emacs-lisp
(defun mmm-syntax-propertize-function (start stop)
  (let ((saved-mode mmm-current-submode)
	(saved-ovl  mmm-current-overlay))
    (mmm-save-changed-local-variables
     mmm-current-submode mmm-current-overlay)
    (unwind-protect
	(mapc (lambda (elt)
		(let* ((mode (car elt))
		       (func (get mode 'mmm-syntax-propertize-function))
		       (beg (cadr elt)) (end (nth 2 elt))
		       (ovl (nth 3 elt))
		       syntax-ppss-cache
		       syntax-ppss-last)
		  (goto-char beg)
		  (mmm-set-current-pair mode ovl)
		  (mmm-set-local-variables mode mmm-current-overlay)
		  (save-restriction
		    (if mmm-current-overlay
			(narrow-to-region (overlay-start mmm-current-overlay)
					  (overlay-end mmm-current-overlay))
		      (narrow-to-region beg end))
		    (cond
		     (func
		      (funcall func beg end))
		     (font-lock-syntactic-keywords
		      (let ((syntax-propertize-function nil))
			(font-lock-fontify-syntactic-keywords-region beg end))))
		    (run-hook-with-args 'mmm-after-syntax-propertize-functions
					mmm-current-overlay mode beg end))))
	      (mmm-regions-in start stop))
      (mmm-set-current-pair saved-mode saved-ovl)
      (mmm-set-local-variables (or saved-mode mmm-primary-mode) saved-ovl))))
```


#### <span class="org-todo done OFF">OFF</span> (OFF) Svelte {#off--svelte}

Had some problems with this and tree-sitter. Web-mode seems to be doing better.

```emacs-lisp
(use-package svelte-mode
  :straight t
  :mode "\\.svelte\\'"
  :disabled
  :config
  (add-hook 'svelte-mode-hook 'my/set-flycheck-eslint)
  (add-hook 'svelte-mode-hook #'smartparens-mode)
  (my/set-smartparens-indent 'svelte-mode)
  ;; I have my own Emmet
  (setq lsp-svelte-plugin-css-completions-emmet nil)
  (setq lsp-svelte-plugin-html-completions-emmet nil))
```


#### SCSS {#scss}

```emacs-lisp
(add-hook 'scss-mode-hook #'smartparens-mode)
(add-hook 'scss-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'scss-mode)
```


#### PHP {#php}

```emacs-lisp
(use-package php-mode
  :straight t
  :mode "\\.php\\'")
```


### LaTeX {#latex}


#### AUCTeX {#auctex}

The best LaTeX editing environment I've found so far.

References:

-   [AUCTeX homepage](https://www.gnu.org/software/auctex/)

<!--listend-->

```emacs-lisp
(use-package tex
  :straight auctex
  :defer t
  :config
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (TeX-PDF-mode)
  ;; Use XeLaTeX & stuff
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-command-extra-options "-shell-escape")
  (setq-default TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode)
  (setq-default TeX-source-correlate-start-server t)
  (setq-default LaTeX-math-menu-unicode t)

  (setq-default font-latex-fontify-sectioning 1.3)

  ;; Scale preview for my DPI
  (setq-default preview-scale-function 1.4)
  (when (boundp 'tex--prettify-symbols-alist)
    (assoc-delete-all "--" tex--prettify-symbols-alist)
    (assoc-delete-all "---" tex--prettify-symbols-alist))

  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (TeX-fold-mode 1)
	      (outline-minor-mode)))

  (add-to-list 'TeX-view-program-selection
	       '(output-pdf "Zathura"))

  ;; Do not run lsp within templated TeX files
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (unless (string-match "\.hogan\.tex$" (buffer-name))
		(lsp))
	      (setq-local lsp-diagnostic-package :none)
	      (setq-local flycheck-checker 'tex-chktex)))

  (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook #'smartparens-mode)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)

  (my/set-smartparens-indent 'LaTeX-mode)
  (require 'smartparens-latex)

  (general-nmap
    :keymaps '(LaTeX-mode-map latex-mode-map)
    "RET" 'TeX-command-run-all
    "C-c t" 'orgtbl-mode)

  <<init-greek-latex-snippets>>
  <<init-english-latex-snippets>>
  <<init-math-latex-snippets>>
  <<init-section-latex-snippets>>)
```


#### BibTeX {#bibtex}

```emacs-lisp
(use-package ivy-bibtex
  :commands (ivy-bibtex)
  :straight t
  :init
  (my-leader-def "fB" 'ivy-bibtex))

(add-hook 'bibtex-mode 'smartparens-mode)
```


#### Import \*.sty {#import-dot-sty}

A function to import `.sty` files to the LaTeX document.

```emacs-lisp
(defun my/list-sty ()
  (reverse
   (sort
    (seq-filter
     (lambda (file) (if (string-match ".*\.sty$" file) 1 nil))
     (directory-files
      (seq-some
       (lambda (dir)
	 (if (and
	      (f-directory-p dir)
	      (seq-some
	       (lambda (file) (string-match ".*\.sty$" file))
	       (directory-files dir))
	      ) dir nil))
       (list "./styles" "../styles/" "." "..")) :full))
    (lambda (f1 f2)
      (let ((f1b (file-name-base f1))
	    (f1b (file-name-base f2)))
	(cond
	 ((string-match-p ".*BibTex" f1) t)
	 ((and (string-match-p ".*Locale" f1) (not (string-match-p ".*BibTex" f2))) t)
	 ((string-match-p ".*Preamble" f2) t)
	 (t (string-lessp f1 f2))))))))

(defun my/import-sty ()
  (interactive)
  (insert
   (apply #'concat
	  (cl-mapcar
	   (lambda (file) (concat "\\usepackage{" (file-name-sans-extension (file-relative-name file default-directory)) "}\n"))
	   (my/list-sty)))))

(defun my/import-sty-org ()
  (interactive)
  (insert
   (apply #'concat
	  (cl-mapcar
	   (lambda (file) (concat "#+LATEX_HEADER: \\usepackage{" (file-name-sans-extension (file-relative-name file default-directory)) "}\n"))
	   (my/list-sty)))))
```


#### Snippets {#snippets}

| Note | Type                                                            |
|------|-----------------------------------------------------------------|
| TODO | Move yasnippet snippets here? Maybe extract to a separate file? |


##### Greek letters {#greek-letters}

Autogenerate snippets for greek letters. I have a few blocks like this because it's faster & more flexible than usual yasnippet snippets.

Noweb points to the AUCTeX config block.

```emacs-lisp
(setq my/greek-alphabet
      '(("a" . "\\alpha")
	("b" . "\\beta" )
	("g" . "\\gamma")
	("d" . "\\delta")
	("e" . "\\epsilon")
	("z" . "\\zeta")
	("h" . "\\eta")
	("o" . "\\theta")
	("i" . "\\iota")
	("k" . "\\kappa")
	("l" . "\\lambda")
	("m" . "\\mu")
	("n" . "\\nu")
	("x" . "\\xi")
	("p" . "\\pi")
	("r" . "\\rho")
	("s" . "\\sigma")
	("t" . "\\tau")
	("u" . "\\upsilon")
	("f" . "\\phi")
	("c" . "\\chi")
	("v" . "\\psi")
	("g" . "\\omega")))

(setq my/latex-greek-prefix "'")

;; The same for capitalized letters
(dolist (elem my/greek-alphabet)
  (let ((key (car elem))
	(value (cdr elem)))
    (when (string-equal key (downcase key))
      (add-to-list 'my/greek-alphabet
		   (cons
		    (capitalize (car elem))
		    (concat
		     (substring value 0 1)
		     (capitalize (substring value 1 2))
		     (substring value 2)))))))

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (list (concat my/latex-greek-prefix (car elem)) (cdr elem) (concat "Greek letter " (car elem))))
  my/greek-alphabet))
```


##### English letters {#english-letters}

```emacs-lisp
(setq my/english-alphabet
      '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(dolist (elem my/english-alphabet)
  (when (string-equal elem (downcase elem))
    (add-to-list 'my/english-alphabet (upcase elem))))

(setq my/latex-mathbb-prefix "`")

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (list (concat my/latex-mathbb-prefix elem) (concat "\\mathbb{" elem "}") (concat "Mathbb letter " elem)))
  my/english-alphabet))
```


##### Math symbols {#math-symbols}

```emacs-lisp
(setq my/latex-math-symbols
      '(("x" . "\\times")
	("." . "\\cdot")
	("v" . "\\forall")
	("s" . "\\sum_{$1}^{$2}$0")
	("p" . "\\prod_{$1}^{$2}$0")
	("d" . "\\partial")
	("e" . "\\exists")
	("i" . "\\int_{$1}^{$2}$0")
	("c" . "\\cap")
	("u" . "\\cup")
	("0" . "\\emptyset")
	("^" . "\\widehat{$1}$0")
	("_" . "\\overline{$1}$0")
	("~" . "\\sim")
	("|" . "\\mid")
	("_|" . "\\perp")))

(setq my/latex-math-prefix ";")

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (let ((key (car elem))
	  (value (cdr elem)))
      (list (concat my/latex-math-prefix key) value (concat "Math symbol " value))))
  my/latex-math-symbols))
```


##### Section snippets {#section-snippets}

Section snippets. The code turned out to be more complicated than just writing the snippets by hand.

```emacs-lisp
(setq my/latex-section-snippets
      '(("ch" . "\\chapter{$1}")
	("sec" . "\\section{$1}")
	("ssec" . "\\subsection{$1}")
	("sssec" . "\\subsubsection{$1}")
	("par" . "\\paragraph{$1}}")))

(setq my/latex-section-snippets
      (mapcar
       (lambda (elem)
	 `(,(car elem)
	   ,(cdr elem)
	   ,(progn
	      (string-match "[a-z]+" (cdr elem))
	      (match-string 0 (cdr elem)))))
       my/latex-section-snippets))

(dolist (elem my/latex-section-snippets)
  (let* ((key (nth 0 elem))
	 (value (nth 1 elem))
	 (desc (nth 2 elem))
	 (star-index (string-match "\{\$1\}" value)))
    (add-to-list 'my/latex-section-snippets
		 `(,(concat key "*")
		   ,(concat
		     (substring value 0 star-index)
		     "*"
		     (substring value star-index))
		   ,(concat desc " with *")))
    (add-to-list 'my/latex-section-snippets
		 `(,(concat key "l")
		   ,(concat value "%\n\\label{sec:$2}")
		   ,(concat desc " with label")))))

(dolist (elem my/latex-section-snippets)
  (setf (nth 1 elem) (concat (nth 1 elem) "\n$0")))

(yas-define-snippets
 'latex-mode
 my/latex-section-snippets)
```


### Other markup languages {#other-markup-languages}


#### Markdown {#markdown}

```emacs-lisp
(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command
	(concat
	 "pandoc"
	 " --from=markdown --to=html"
	 " --standalone --mathjax --highlight-style=pygments"
	 " --css=pandoc.css"
	 " --quiet"
	 ))
  (setq markdown-live-preview-delete-export 'delete-on-export)
  (setq markdown-asymmetric-header t)
  (setq markdown-open-command "/home/pavel/bin/scripts/chromium-sep")
  (add-hook 'markdown-mode-hook #'smartparens-mode)
  (general-define-key
   :keymaps 'markdown-mode-map
   "M-<left>" 'markdown-promote
   "M-<right>" 'markdown-demote))

;; (use-package livedown
;;   :straight (:host github :repo "shime/emacs-livedown")
;;   :commands livedown-preview
;;   :config
;;   (setq livedown-browser "qutebrowser"))

```


#### PlantUML {#plantuml}

| Guix dependency |
|-----------------|
| plantuml        |

```emacs-lisp
(use-package plantuml-mode
  :straight t
  :mode "(\\.\\(plantuml?\\|uml\\|puml\\)\\'"
  :config
  (setq plantuml-executable-path "/home/pavel/.guix-extra-profiles/emacs/emacs/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-indent-level 2)
  (setq my/plantuml-indent-regexp-return "^\s*return\s+.+$")
  (add-to-list
   'plantuml-indent-regexp-end
   my/plantuml-indent-regexp-return)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-hook 'plantuml-mode-hook #'smartparens-mode))

(general-nmap
  :keymaps 'plantuml-mode-map
  "RET" 'plantuml-preview)
```


#### LanguageTool {#languagetool}

LanguageTool is a great offline spell checker. For some reason, the download link is nowhere to be found on the home page, so it is listed in the references as well.

References:

-   [LanguageTool homepage](https://languagetool.org/)
-   [LanguageTool http server](https://dev.languagetool.org/http-server)
-   [LanguageTool for Emacs repo](https://github.com/mhayashi1120/Emacs-langtool)

<!--listend-->

```emacs-lisp
(use-package langtool
  :straight t
  :commands (langtool-check)
  :config
  (setq langtool-language-tool-server-jar "/home/pavel/Programs/LanguageTool-5.1/languagetool-server.jar")
  (setq langtool-mother-tongue "ru")
  (setq langtool-default-language "en-US"))

(my-leader-def
  :infix "L"
  "" '(:which-key "languagetool")
  "c" 'langtool-check
  "s" 'langtool-server-stop
  "d" 'langtool-check-done
  "n" 'langtool-goto-next-error
  "p" 'langtool-goto-previous-error
  "l" 'langtool-correct-buffer)
```


### Lisp {#lisp}

These are your father's parentheses. Elegant weapons for a more... civilized age.


#### Meta Lisp {#meta-lisp}

Some packages for editing various Lisps.

```emacs-lisp
(use-package lispy
  :commands (lispy-mode)
  :straight t)

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :straight t)

(sp-with-modes sp-lisp-modes
  (sp-local-pair "'" nil :actions nil))
```


#### Emacs Lisp {#emacs-lisp}


##### Package Lint {#package-lint}

A package that checks for the metadata in Emacs Lisp packages.

```emacs-lisp
(use-package flycheck-package
  :straight t
  :after flycheck
  :config
  (flycheck-package-setup))
```


##### General {#general}

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
```


#### Common lisp {#common-lisp}

```emacs-lisp
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)
```


#### Clojure {#clojure}

```emacs-lisp
(use-package clojure-mode
  :straight t
  :mode "\\.clj[sc]?\\'"
  :config
  ;; (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package cider
  :mode "\\.clj[sc]?\\'"
  :straight t)
```


#### Hy {#hy}

Python requirements:

-   `hy`
-   `jedhy`

<!--listend-->

```emacs-lisp
(use-package hy-mode
  :straight t
  :mode "\\.hy\\'"
  :config
  (add-hook 'hy-mode-hook #'lispy-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))
```


#### Scheme {#scheme}

```emacs-lisp
(use-package geiser
  :straight t
  :if (not my/lowpower)
  :config
  (setq geiser-default-implementation 'guile))

(use-package geiser-guile
  :straight t
  :after geiser)

(add-hook 'scheme-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'lispy-mode)
```


#### CLIPS {#clips}

An honorary Lisp

```emacs-lisp
(use-package clips-mode
  :straight t
  :mode "\\.cl\\'"
  :config
  (add-hook 'clips-mode 'lispy-mode))
```


### Python {#python}

Use [Microsoft Language Server for Python](https://github.com/Microsoft/python-language-server).

For some reason it doesn't use pipenv python executable, so here is a small workaround.

```emacs-lisp
(setq my/pipenv-python-alist '())

(defun my/get-pipenv-python ()
  (let ((default-directory (projectile-project-root)))
    (if (file-exists-p "Pipfile")
	(let ((asc (assoc default-directory my/pipenv-python-alist)))
	  (if asc
	      (cdr asc)
	    (let ((python-executable
		   (string-trim (shell-command-to-string "PIPENV_IGNORE_VIRTUALENVS=1 pipenv run which python 2>/dev/null"))))
	      (if (string-match-p ".*not found.*" python-executable)
		  (message "Pipfile found, but not pipenv executable!")
		(message (format "Found pipenv python: %s" python-executable))
		(add-to-list 'my/pipenv-python-alist (cons default-directory python-executable))
		python-executable))))
      "python")))

(use-package lsp-pyright
  :straight t
  :defer t
  :if (not my/slow-ssh)
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (setq-local lsp-pyright-python-executable-cmd (my/get-pipenv-python))
			 (lsp))))

(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'hs-minor-mode)
```


#### pipenv {#pipenv}

[Pipenv](https://github.com/pypa/pipenv) is a package manager for Python.

Automatically creates & manages virtualenvs and stores data in `Pipfile` and `Pipfile.lock` (like npm's `package.json` and `package-lock.json`).

```emacs-lisp
(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode)
  :if (not my/slow-ssh)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
```


#### yapf {#yapf}

[yapf](https://github.com/google/yapf) is a formatter for Python files.

| Guix dependency |
|-----------------|
| python-yapf     |

References:

-   [yapf repo](https://github.com/google/yapf)
-   [yapfify.el repo](https://github.com/JorisE/yapfify)

<!--listend-->

```emacs-lisp
(use-package yapfify
  :straight (:repo "JorisE/yapfify" :host github)
  :commands (yapfify-region
	     yapfify-buffer
	     yapfify-region-or-buffer
	     yapf-mode))
```

Global config:

```ini
[style]
based_on_style = facebook
column_limit = 80
```


#### isort {#isort}

[isort](https://github.com/PyCQA/isort) is a Python package to sort Python imports.

| Guix dependency |
|-----------------|
| python-isort    |

References:

-   [isort docs](https://pycqa.github.io/isort/)
-   [py-isort.el repo](https://github.com/paetzke/py-isort.el)

<!--listend-->

```emacs-lisp
(use-package py-isort
  :straight t
  :commands (py-isort-buffer py-isort-region))
```

The following bindings calls yapf & isort on the buffer

```emacs-lisp
(my-leader-def
  :keymaps 'python-mode-map
  "rr" (lambda ()
	 (interactive)
	 (unless (and (fboundp #'org-src-edit-buffer-p) (org-src-edit-buffer-p))
	   (py-isort-buffer))
	 (yapfify-buffer)))
```


#### sphinx-doc {#sphinx-doc}

A package to generate sphinx-compatible docstrings.

```emacs-lisp
(use-package sphinx-doc
  :straight t
  :hook (python-mode . sphinx-doc-mode)
  :config
  (my-leader-def
    :keymaps 'sphinx-doc-mode-map
    "rd" 'sphinx-doc))
```


#### pytest {#pytest}

[pytest](https://docs.pytest.org/en/6.2.x/) is an unit testing framework for Python.

Once again a function to set pytest executable from pipenv.

References:

-   [pytest docs](https://docs.pytest.org/en/6.2.x/)
-   [emacs-python-pytest](https://github.com/wbolster/emacs-python-pytest)

<!--listend-->

```emacs-lisp
(defun my/set-pipenv-pytest ()
  (setq-local
   python-pytest-executable
   (concat (my/get-pipenv-python) " -m pytest")))

(use-package python-pytest
  :straight t
  :commands (python-pytest-dispatch)
  :init
  (my-leader-def
    :keymaps 'python-mode-map
    :infix "t"
    "t" 'python-pytest-dispatch)
  :config
  <<override-pytest-run>>
  (add-hook 'python-mode-hook #'my/set-pipenv-pytest)
  (when (derived-mode-p 'python-mode)
    (my/set-pipenv-pytest)))
```


##### Fix comint buffer width {#fix-comint-buffer-width}

For some reason default comint output width is way too large.

To fix that, I've modified the following function in the `python-pytest` package.

```emacs-lisp
(cl-defun python-pytest--run-as-comint (&key command)
  "Run a pytest comint session for COMMAND."
  (let* ((buffer (python-pytest--get-buffer))
	 (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (when (comint-check-proc buffer)
	(unless (or compilation-always-kill
		    (yes-or-no-p "Kill running pytest process?"))
	  (user-error "Aborting; pytest still running")))
      (when process
	(delete-process process))
      (let ((inhibit-read-only t))
	(erase-buffer))
      (unless (eq major-mode 'python-pytest-mode)
	(python-pytest-mode))
      (compilation-forget-errors)
      (display-buffer buffer)
      (setq command (format "export COLUMNS=%s; %s"
			    (- (window-width (get-buffer-window buffer)) 5)
			    command))
      (insert (format "cwd: %s\ncmd: %s\n\n" default-directory command))
      (setq python-pytest--current-command command)
      (when python-pytest-pdb-track
	(add-hook
	 'comint-output-filter-functions
	 'python-pdbtrack-comint-output-filter-function
	 nil t))
      (run-hooks 'python-pytest-setup-hook)
      (make-comint-in-buffer "pytest" buffer "bash" nil "-c" command)
      (run-hooks 'python-pytest-started-hook)
      (setq process (get-buffer-process buffer))
      (set-process-sentinel process #'python-pytest--process-sentinel))))
```


#### code-cells {#code-cells}

Support for text with magic comments.

```emacs-lisp
(use-package code-cells
  :straight t
  :commands (code-cells-mode))
```


#### tensorboard {#tensorboard}

A function to start up [TensorBoard](https://www.tensorflow.org/tensorboard).

```emacs-lisp
(setq my/tensorboard-buffer "TensorBoard-out")

(defun my/tensorboard ()
  (interactive)
  (start-process
   "tensorboard"
   my/tensorboard-buffer
   "tensorboard"
   "serve"
   "--logdir"
   (car (find-file-read-args "Directory: " t)))
  (display-buffer my/tensorboard-buffer))
```


### Java {#java}

```emacs-lisp
(use-package lsp-java
  :straight t
  :after (lsp)
  :config
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"))

(add-hook 'java-mode-hook #'smartparens-mode)
;; (add-hook 'java-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'java-mode)
```


### Go {#go}

```emacs-lisp
(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :config
  (my/set-smartparens-indent 'go-mode)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'hs-minor-mode))
```


### .NET {#dot-net}


#### C# {#c}

| Guix dependencies | Disabled |
|-------------------|----------|
| omnisharp         | t        |
| dotnet            | t        |

```emacs-lisp
(use-package csharp-mode
  :straight t
  :mode "\\.cs\\'"
  :config
  (setq lsp-csharp-server-path (executable-find "omnisharp-wrapper"))
  (add-hook 'csharp-mode-hook #'csharp-tree-sitter-mode)
  (add-hook 'csharp-tree-sitter-mode-hook #'smartparens-mode)
  (add-hook 'csharp-mode-hook #'hs-minor-mode)
  (my/set-smartparens-indent 'csharp-tree-sitter-mode))
```


#### MSBuild {#msbuild}

```emacs-lisp
(use-package csproj-mode
  :straight t
  :mode "\\.csproj\\'"
  :config
  (add-hook 'csproj-mode #'smartparens-mode))
```


### fish {#fish}

```emacs-lisp
(use-package fish-mode
  :straight t
  :mode "\\.fish\\'"
  :config
 (add-hook 'fish-mode-hook #'smartparens-mode))
```


### sh {#sh}

```emacs-lisp
(add-hook 'sh-mode-hook #'smartparens-mode)
```


### Haskell {#haskell}

```emacs-lisp
(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'")

(use-package lsp-haskell
  :straight t
  :after (lsp haskell-mode))
```


### JSON {#json}

```emacs-lisp
(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode #'smartparens-mode)
  (add-hook 'json-mode #'hs-minor-mode)
  (my/set-smartparens-indent 'json-mode))
```


### YAML {#yaml}

```emacs-lisp
(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook 'smartparens-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
```


### .env {#dot-env}

```emacs-lisp
(use-package dotenv-mode
  :straight t
  :mode "\\.env\\..*\\'")
```


### CSV {#csv}

```emacs-lisp
(use-package csv-mode
  :straight t
  :mode "\\.csv\\'")
```


### <span class="org-todo done OFF">OFF</span> (OFF) PDF {#off--pdf}

A decent package to view PDFs in Emacs, but I prefer Zathura.

References:

-   <https://github.com/vedang/pdf-tools/>

<!--listend-->

```emacs-lisp
(use-package pdf-tools
  :straight t
  :commands (pdf-tools-install))
```


### Docker {#docker}

```emacs-lisp
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :straight t
  :config
  (add-hook 'dockerfile-mode 'smartparens-mode))
```


## Apps & Misc {#apps-and-misc}


### Managing dotfiles {#managing-dotfiles}

A bunch of functions for managing dotfiles with yadm.


#### Open Emacs config {#open-emacs-config}

```emacs-lisp
(defun my/edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file "~/Emacs.org"))

;; (defun my/edit-exwm-configuration ()
;;   "Open the exwm config file."
;;   (interactive)
;;   (find-file "~/.emacs.d/exwm.org"))

(general-define-key "C-c c" 'my/edit-configuration)
;; (general-define-key "C-c C" 'my/edit-exwm-configuration)
(my-leader-def
  :infix "c"
  "" '(:which-key "configuration")
  "c" 'my/edit-configuration)
```


#### Open Magit for yadm {#open-magit-for-yadm}

Idea:

-   <https://www.reddit.com/r/emacs/comments/gjukb3/yadm%5Fmagit/>

<!--listend-->

```emacs-lisp
(with-eval-after-load 'tramp
  (add-to-list 'tramp-methods
	       `("yadm"
		 (tramp-login-program "yadm")
		 (tramp-login-args (("enter")))
		 (tramp-login-env (("SHELL") "/bin/sh"))
		 (tramp-remote-shell "/bin/sh")
		 (tramp-remote-shell-args ("-c")))))


(defun my/yadm-magit ()
  (interactive)
  (magit-status "/yadm::"))

(my-leader-def "cm" 'my/yadm-magit)
```


#### Open a dotfile {#open-a-dotfile}

Open a file managed by yadm.

```emacs-lisp
(defun my/open-yadm-file ()
  "Open a file managed by yadm"
  (interactive)
  (find-file
   (concat
    (file-name-as-directory (getenv "HOME"))
    (completing-read
     "yadm files: "
     (split-string
      (shell-command-to-string "yadm ls-files $HOME --full-name") "\n")))))

(general-define-key "C-c f" 'my/open-yadm-file)
(my-leader-def "cf" 'my/open-yadm-file)
```


### Internet & Multimedia {#internet-and-multimedia}


#### Notmuch {#notmuch}

My notmuch config now resides in [Mail.org]({{< relref "Mail" >}}).

```emacs-lisp
(unless my/is-termux
  (load-file (expand-file-name "mail.el" user-emacs-directory)))
```


#### Elfeed {#elfeed}

[elfeed](https://github.com/skeeto/elfeed) is an Emacs RSS client.

The advice there sets `shr-use-fonts` to nil while rendering HTML, so the `elfeed-show` buffer will use monospace font.

Using my own fork until the modifications are merged into master.

```emacs-lisp
(use-package elfeed
  :straight (:repo "SqrtMinusOne/elfeed" :host github)
  :commands (elfeed)
  :init
  (my-leader-def "ae" 'elfeed)
  :config
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-enclosure-default-dir (expand-file-name "~"))
  (advice-add #'elfeed-insert-html
	      :around
	      (lambda (fun &rest r)
		(let ((shr-use-fonts nil))
		  (apply fun r))))
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-search-mode-map
   "o" #'my/elfeed-search-filter-source
   "c" #'elfeed-search-clear-filter
   "gl" (lambda () (interactive) (elfeed-search-set-filter "+later")))
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "ge" #'my/elfeed-show-visit-eww))
```

[elfeed-org](https://github.com/remyhonig/elfeed-org) allows configuring Elfeed feeds with an Org file.

```emacs-lisp
(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org"))
  (elfeed-org))
```


##### Some additions {#some-additions}

Filter elfeed search buffer by the feed under the cursor.

```emacs-lisp
(defun my/elfeed-search-filter-source (entry)
  "Filter elfeed search buffer by the feed under cursor."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-search-set-filter
     (concat
      "@6-months-ago "
      "+unread "
      "="
      (replace-regexp-in-string
       (rx "?" (* not-newline) eos)
       ""
       (elfeed-feed-url (elfeed-entry-feed entry)))))))
```

Open a URL with eww.

```emacs-lisp
(defun my/elfeed-show-visit-eww ()
  "Visit the current entry in eww"
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww link))))
```


##### YouTube {#youtube}

Previously this block was opening MPV with `start-process`, but now I've managed to hook up MPV with EMMS. So there is the EMMS+elfeed "integration".

The following function converts URLs from Invidious and the like to YouTube.

```emacs-lisp
(defun my/get-youtube-url (link)
  (let ((watch-id (cadr
		   (assoc "watch?v"
			  (url-parse-query-string
			   (substring
			    (url-filename
			     (url-generic-parse-url link))
			    1))))))
    (concat "https://www.youtube.com/watch?v=" watch-id)))
```

Now, a function to add YouTube link with metadata from elfeed to EMMS.

```emacs-lisp
(with-eval-after-load 'emms
  (define-emms-source elfeed (entry)
    (let ((track (emms-track
		  'url (my/get-youtube-url (elfeed-entry-link entry)))))
      (emms-track-set track 'info-title (elfeed-entry-title entry))
      (setq my/test track)
      (emms-playlist-insert-track track))))

(defun my/elfeed-add-emms-youtube ()
  (interactive)
  (emms-add-elfeed elfeed-show-entry))

(with-eval-after-load 'elfeed
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "gm" #'my/elfeed-add-emms-youtube))
```


#### EMMS {#emms}

EMMS is the Emacs Multi-Media System. I use it to control MPD & MPV.

References:

-   [EMMS Manual](https://www.gnu.org/software/emms/manual/)
-   [Uncle Dave's video](https://www.youtube.com/watch?v=xTVN8UDScqk)

<!--listend-->

```emacs-lisp
(use-package emms
  :straight t
  :commands (emms-smart-browse
	     emms-browser
	     emms-add-url
	     emms-add-file
	     emms-add-find)
  :if (not my/is-termux)
  :init
  (my-leader-def
    :infix "as"
    "" '(:which-key "emms")
    "s" 'emms-smart-browse
    "b" 'emms-browser
    "p" 'emms-pause
    "q" 'emms-stop
    "h" 'emms-previous
    "l" 'emms-next
    "u" 'emms-player-mpd-connect)
  (setq emms-mode-line-icon-enabled-p nil)
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-player-mpv)
  (emms-all)
  ;; MPD setup
  <<emms-mpd-setup>>
  ;; MPV setup
  <<emms-mpv-setup>>
  ;; evil-lion and evil-commentary shadow some gX bindings
  (add-hook 'emms-browser-mode-hook
	    (lambda ()
	      (evil-lion-mode -1)
	      ;; (evil-commentary-mode -1)
	      ))
  ;; I have everything I need in polybar
  (emms-mode-line-mode -1)
  (emms-playing-time-display-mode -1)
  <<emms-fixes>>)
```


##### MPD {#mpd}

[mpd](https://www.musicpd.org/) is a server for playing music. It has a couple of first-class clients, including curses-based [ncmpcpp](https://github.com/ncmpcpp/ncmpcpp), but of course, I want to use Emacs.

```emacs-lisp
(setq emms-source-file-default-directory (expand-file-name "~/Music/"))
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq emms-player-mpd-music-directory "~/Music")
```

Connect on setup. For some reason, it stops the mpd playback whenever it connects, but it is not a big issue.

```emacs-lisp
(emms-player-mpd-connect)
```

Clear MPD playlist on clearing EMMS playlist. IDK if this is fine for MPD library playlist, I don't use them anyhow.

```emacs-lisp
(add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear)
```

Set a custom regex for MPD. EMMS sets up the default one from MPD's diagnostic output so that regex opens basically everything, including videos, https links, etc. That is fine if MPD is the only player in EMMS, but as I want to use MPV as well, I override the regex.

```emacs-lisp
(emms-player-set emms-player-mpd
		 'regex
		 (emms-player-simple-regexp
		  "m3u" "ogg" "flac" "mp3" "wav" "mod" "au" "aiff"))
```

After all this is done, run `M-x emms-cache-set-from-mpd-all` to set cache from MPD. If everything is correct, EMMS browser will be populated with MPD database.


##### MPV {#mpv}

| Guix dependency |
|-----------------|
| mpv             |
| youtube-dl      |

[mpv](https://mpv.io/) is a decent media player, which has found a place in this configuration because it integrates with youtube-dl.

```emacs-lisp
(add-to-list 'emms-player-list 'emms-player-mpv)
```

Also a custom regex. My demands for MPV include running `youtube-dl`, so there is a regex that matches youtube.com or some of the video formats.

```emacs-lisp
(emms-player-set emms-player-mpv
		 'regex
		 (rx (or (: "https://" (* nonl) "youtube.com" (* nonl))
			 (+ (? (or "https://" "http://"))
			    (* nonl)
			    (regexp (eval (emms-player-simple-regexp
					   "mp4" "mov" "wmv" "webm" "flv" "avi" "mkv")))))))
```

By default MPV plays the video in the best possible quality, which may be pretty high, even too high with limited bandwidth. So here is the logic to choose the quality.

```emacs-lisp
(setq my/youtube-dl-quality-list
      '("bestvideo[height<=720]+bestaudio/best[height<=720]"
	"bestvideo[height<=480]+bestaudio/best[height<=480]"
	"bestvideo[height<=1080]+bestaudio/best[height<=1080]"))

(setq my/default-emms-player-mpv-parameters
      '("--quiet" "--really-quiet" "--no-audio-display"))

(defun my/set-emms-mpd-youtube-quality (quality)
  (interactive "P")
  (unless quality
    (setq quality (completing-read "Quality: " my/youtube-dl-quality-list nil t)))
  (setq emms-player-mpv-parameters
	`(,@my/default-emms-player-mpv-parameters ,(format "--ytdl-format=%s" quality))))

(my/set-emms-mpd-youtube-quality (car my/youtube-dl-quality-list))
```

Now `emms-add-url` should work on YouTube URLs just fine. Just keep in mind that it will only add the URL to the playlist, not play it right away.


##### Cache cleanup {#cache-cleanup}

All added URLs reside in the EMMS cache after being played. I don't want them to stay there for a long time, so here is a handy function to clean it.

```emacs-lisp
(defun my/emms-cleanup-urls ()
  (interactive)
  (let ((keys-to-delete '()))
    (maphash (lambda (key value)
	       (when (eq (cdr (assoc 'type value)) 'url)
		 (add-to-list 'keys-to-delete key)))
	     emms-cache-db)
    (dolist (key keys-to-delete)
      (remhash key emms-cache-db)))
  (setq emms-cache-dirty t))

(my-leader-def "asc" #'my/emms-cleanup-urls)
```


##### Fetching lyrics {#fetching-lyrics}

My package for fetching EMMS lyrics and album covers.

```emacs-lisp
(use-package lyrics-fetcher
  :straight t
  :after (emms)
  :init
  (my-leader-def
    "ast" #'lyrics-fetcher-show-lyrics
    "asT" #'lyrics-fetcher-show-lyrics-query)
  :config
  (setq lyrics-fetcher-genius-access-token
	(password-store-get "My_Online/APIs/genius.com"))
  (general-define-key
   :states '(emacs normal)
   :keymaps 'emms-browser-mode-map
   "gl" 'lyrics-fetcher-emms-browser-show-at-point
   "gC" 'lyrics-fetcher-emms-browser-fetch-covers-at-point
   "go" 'lyrics-fetcher-emms-browser-open-large-cover-at-point))
```


##### Some keybindings {#some-keybindings}

```emacs-lisp
(with-eval-after-load 'emms-browser
  (general-define-key
   :states '(normal)
   :keymaps 'emms-browser-mode-map
   "q" 'quit-window))

(with-eval-after-load 'emms
  (general-define-key
   :states '(normal)
   :keymaps 'emms-playlist-mode-map
   "q" 'quit-window))
```


##### EMMS & mpd Fixes {#emms-and-mpd-fixes}

~~Some fixes until I submit a patch.~~ I've submitted a patch for with these fixes, so I'll remove this section eventually.

For some reason EMMS doesn't fetch `albumartist` from MPD. Overriding this function fixes that.

```emacs-lisp
(defun emms-info-mpd-process (track info)
  (dolist (data info)
    (let ((name (car data))
	  (value (cdr data)))
      (setq name (cond ((string= name "artist") 'info-artist)
		       ((string= name "albumartist") 'info-albumartist)
		       ((string= name "composer") 'info-composer)
		       ((string= name "performer") 'info-performer)
		       ((string= name "title") 'info-title)
		       ((string= name "album") 'info-album)
		       ((string= name "track") 'info-tracknumber)
		       ((string= name "disc") 'info-discnumber)
		       ((string= name "date") 'info-year)
		       ((string= name "genre") 'info-genre)
		       ((string= name "time")
			(setq value (string-to-number value))
			'info-playing-time)
		       (t nil)))
      (when name
	(emms-track-set track name value)))))
```

Also, `emms-player-mpd-get-alists` has an interesting bug. This function parses the response to `listallinfo`, which looks something like this:

```text
tag1: value1
tag2: value2
...
tag1: value1'
tag2: value2'
```

This structure has to be converted to list of alists, which looks like:

```text
(("tag1" . "value1"
  "tag2" . "value2")
  ("tag1" . "value1'"
  ("tag2" . "value2'")))
```

The original implementation creates a new alist whenever it encounters a tag it has already put in the current alist. Which doesn't work too well if some tags don't repeat, if the order is messed up, etc.

Fortunately, according to the [protocol specification](https://mpd.readthedocs.io/en/latest/protocol.html#command-lsinfo), each new record has to start with `file`, `directory` or `playlist`. I've overridden the function with that in mind and it fixed the import, at least in my case.

```emacs-lisp
(defun emms-player-mpd-get-alists (info)
  "Turn the given parsed INFO from MusicPD into an list of alists.

The list will be in reverse order."
  (when (and info
	     (null (car info))          ; no error has occurred
	     (cdr info))                ; data exists
    (let ((alists nil)
	  (alist nil)
	  cell)
      (dolist (line (cdr info))
	(when (setq cell (emms-player-mpd-parse-line line))
	  (if (member (car cell) '("file" "directory" "playlist"))
	      (setq alists (cons alist alists)
		    alist (list cell))
	    (setq alist (cons cell alist)))))
      (when alist
	(setq alists (cons alist alists)))
      alists)))
```


#### EWW {#eww}

Emacs built-in web browser. ~~I wonder if anyone actually uses it.~~

I use it occasionally to open links in elfeed.

```emacs-lisp
(defun my/toggle-shr-use-fonts ()
  "Toggle the shr-use-fonts variable in buffer"
  (interactive)
  (setq-local shr-use-fonts (not shr-use-fonts)))

(my-leader-def "aw" 'eww)

(general-define-key
 :keymaps 'eww-mode-map
 "+" 'text-scale-increase
 "-" 'text-scale-decrease)
```


#### ERC {#erc}

ERC is a built-it Emacs IRC client.

```emacs-lisp
(use-package erc
  :commands (erc erc-tls)
  :straight (:type built-in)
  :init
  (my-leader-def "ai" #'erc-tls)
  :config
  ;; Logging
  (setq erc-log-channels-directory "~/.erc/logs")
  (setq erc-save-buffer-on-part t)
  ;; Config of my ZNC instance.
  (setq erc-server "sqrtminusone.xyz")
  (setq erc-port 1984)
  (setq erc-nick "sqrtminusone")
  (setq erc-user-full-name "Pavel Korytov")
  (setq erc-password (password-store-get "Selfhosted/ZNC"))
  (setq erc-kill-buffer-on-part t)
  (setq erc-track-shorten-start 8))
```

Exclude everything but actual messages from notifications.

```emacs-lisp
(setq erc-track-exclude-types '("NICK" "JOIN" "LEAVE" "QUIT" "PART"
				"301"   ; away notice
				"305"   ; return from awayness
				"306"   ; set awayness
				"324"   ; modes
				"329"   ; channel creation date
				"332"   ; topic notice
				"333"   ; who set the topic
				"353"   ; Names notice
				))
```

A plugin to highlight IRC nicknames:

```emacs-lisp
(use-package erc-hl-nicks
  :hook (erc-mode . erc-hl-nicks-mode)
  :after (erc)
  :straight t)
```

ZNC support. Seems to provide a few nice features for ZNC.

```emacs-lisp
(use-package znc
  :straight t
  :after (erc))
```


#### Google Translate {#google-translate}

Emacs interface to Google Translate.

Can't make it load lazily for some strange reason.

References:

-   [google-translate repo](https://github.com/atykhonov/google-translate)
-   [issue with ttk error fix](https://github.com/atykhonov/google-translate/issues/137#issuecomment-728278849)

<!--listend-->

```emacs-lisp
(use-package google-translate
  :straight t
  :functions (my-google-translate-at-point google-translate--search-tkk)
  :custom
  (google-translate-backend-method 'curl)
  :config
  (require 'facemenu)
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  (defun my-google-translate-at-point()
    "reverse translate if prefix"
    (interactive)
    (if current-prefix-arg
	(google-translate-at-point)
      (google-translate-at-point-reverse)))
  (setq google-translate-translation-directions-alist
	'(("en" . "ru")
	  ("ru" . "en"))))

(my-leader-def
  :infix "at"
  "" '(:which-key "google translate")
  "p" 'google-translate-at-point
  "P" 'google-translate-at-point-reverse
  "q" 'google-translate-query-translate
  "Q" 'google-translate-query-translate-reverse
  "t" 'google-translate-smooth-translate)
```


### Reading documentation {#reading-documentation}


#### tldr {#tldr}

[tldr](https://tldr.sh/) is a collaborative project providing cheatsheets for various console commands. For some reason, the built-in download in the package is broken, so I use my own function.

```emacs-lisp
(use-package tldr
  :straight t
  :commands (tldr)
  :config
  (setq tldr-source-zip-url "https://github.com/tldr-pages/tldr/archive/refs/heads/main.zip")

  (defun tldr-update-docs ()
    (interactive)
    (shell-command-to-string (format "curl -L %s --output %s" tldr-source-zip-url tldr-saved-zip-path))
    (when (file-exists-p "/tmp/tldr")
      (delete-directory "/tmp/tldr" t))
    (shell-command-to-string (format "unzip -d /tmp/tldr/ %s" tldr-saved-zip-path) nil nil)
    (when (file-exists-p tldr-directory-path)
      (delete-directory tldr-directory-path 'recursive 'no-trash))
    (shell-command-to-string (format "mv %s %s" "/tmp/tldr/tldr-main" tldr-directory-path))))

(my-leader-def "hT" 'tldr)
```


#### man & info {#man-and-info}

Of course, Emacs can also display man and info pages.

```emacs-lisp
(setq Man-width-max 180)
(my-leader-def "hM" 'man)

(general-define-key
 :states '(normal)
 :keymaps 'Info-mode-map
 (kbd "RET") 'Info-follow-nearest-node)

(defun my/man-fix-width (&rest _)
  (setq-local Man-width (- (window-width) 4)))

(advice-add #'Man-update-manpage :before #'my/man-fix-width)
```


#### devdocs.io {#devdocs-dot-io}

Finally, there is also an Emacs plugin for [devdocs.io](https://devdocs.io).

```emacs-lisp
(use-package devdocs
  :straight t
  :commands (devdocs-install devdocs-lookup)
  :init
  (my-leader-def
    "he" #'devdocs-lookup
    "hE" #'devdocs-install))
```


### Utilities {#utilities}


#### pass {#pass}

I use [pass](https://www.passwordstore.org/) as my password manager. Expectedly, there is Emacs frontend for it.

```emacs-lisp
(use-package pass
  :straight t
  :commands (pass)
  :init
  (my-leader-def "ak" #'pass)
  :config
  (setq pass-show-keybindings nil))
```


#### Docker {#docker}

A package to manage docker containers from Emacs.

The file `progidy-config.el` sets variable `my/docker-directories`, which allows to

```emacs-lisp
(use-package docker
  :straight t
  :commands (docker)
  :init
  (my-leader-def "ao" 'docker))
```

By default, docker commands are run in `default-directory`. Even worse, transient doesn't allow to set `default-directory` temporarily, via `let`. But often I don't want to change `default-directory` of a buffer (e.g. via Dired) to run a command from there.

So I decided to implement the following advice:

```emacs-lisp
(setq my/selected-docker-directory nil)

(defun my/docker-override-dir (fun &rest args)
  (let ((default-directory (or my/selected-docker-directory default-directory)))
    (setq my/selected-docker-directory nil)
    (apply fun args)))
```

It overrides `default-directory` for the first launch of a function. Now, add the advice to the required functions from `docker.el`:

```emacs-lisp
(with-eval-after-load 'docker
  (advice-add #'docker-compose-run-docker-compose-async :around #'my/docker-override-dir)
  (advice-add #'docker-compose-run-docker-compose :around #'my/docker-override-dir)
  (advice-add #'docker-run-docker-async :around #'my/docker-override-dir)
  (advice-add #'docker-run-docker :around #'my/docker-override-dir))
```

And here is a function which prompts the user for the directory. File `progidy-config.el` sets an alist of possible directories, look the section about [progidy](#progidy).

```emacs-lisp
(defun my/docker-from-dir ()
  (interactive)
  (when (not (boundp 'my/docker-directories))
    (load (concat user-emacs-directory "prodigy-config")))
  (let* ((directories
	  (mapcar
	   (lambda (el) (cons (format "%-30s %s" (car el) (cdr el)) (cdr el)))
	   my/docker-directories))
	 (selected-directory
	  (cdr (assoc (completing-read "Docker: " directories nil nil "^")
		      directories))))
    (setq my/selected-docker-directory selected-directory)
    (docker)))

(my-leader-def "aO" 'my/docker-from-dir)
```


#### Progidy {#progidy}

[prodigy.el](https://github.com/rejeep/prodigy.el) is a package to run various services. I've previously used tmuxp + tmux, but want to try this as well.

The actual service definitions are in the `~/.emacs.d/prodigy.org`, which tangles to `prodigy-config.el`. Both files are encrypted in yadm, as they contain personal data.

```emacs-lisp
(use-package prodigy
  :straight t
  :commands (prodigy)
  :init
  (my-leader-def "ap" 'prodigy)
  :config
  (when (not (boundp 'my/docker-directories))
    (load (concat user-emacs-directory "prodigy-config")))
  (general-define-key
   :states '(normal)
   :keymaps 'prodigy-view-mode-map
   "C-h" 'evil-window-left
   "C-l" 'evil-window-right
   "C-k" 'evil-window-up
   "C-j" 'evil-window-down))
```

A few functions to work with apps on ports.

```emacs-lisp
(defun my/get-apps-on-ports ()
  (mapcar
   (lambda (line)
     (let* ((split (split-string line (rx (| (+ " ") (+ "\t")))))
	    (process (elt split 6)))
       `((netid . ,(elt split 0))
	 (state . ,(elt split 1))
	 (recv-q . ,(elt split 2))
	 (send-q . ,(elt split 3))
	 ,@(let ((data (elt split 4)))
	     (save-match-data
	       (string-match (rx (group-n 1 (* nonl)) ":" (group-n 2 (or (+ num) "*"))) data)
	       `((local-address . ,(match-string 1 data))
		 (local-port . ,(match-string 2 data)))))
	 ,@(unless (string-empty-p process)
	     `((pid . ,(save-match-data
			 (string-match (rx "pid=" (+ num)) process)
			 (string-to-number (substring (match-string 0 process) 4)))))))))
   (seq-filter
    (lambda (s) (not (string-empty-p s)))
    (split-string
     (shell-command-to-string "ss -tulpnH | grep LISTEN") "\n"))))

(defun my/kill-app-on-port (port &optional signal)
  (let ((apps (my/get-apps-on-ports)))
    (dolist (app apps)
      (when (string-equal (cdr (assoc 'local-port app)) port)
	(signal-process (cdr (assoc 'pid app)) (or signal 15))
	(message "Sent %d to %d" (or signal 15) (cdr (assoc 'pid app)))))))
```


#### screenshot.el {#screenshot-dot-el}

Tecosaur's plugin to make beautiful code screenshots.

| Guix dependency |
|-----------------|
| imagemagick     |

```emacs-lisp
(use-package screenshot
  :straight (:repo "tecosaur/screenshot" :host github :files ("screenshot.el"))
  :commands (screenshot)
  :init
  (my-leader-def "S" 'screenshot))
```


#### proced {#proced}

proced is an Emacs built-it process viewer, like top.

```emacs-lisp
(my-leader-def "ah" 'proced)
(setq proced-auto-update-interval 1)
(add-hook 'proced-mode-hook (lambda ()
			      (visual-line-mode -1)
			      (setq-local truncate-lines t)
			      (proced-toggle-auto-update 1)))
```


#### Guix {#guix}

An Emacs package to help managing GNU Guix.

```emacs-lisp
(use-package guix
  :straight t
  :commands (guix)
  :init
  (my-leader-def "ag" 'guix))
```


### Productivity {#productivity}


#### Pomidor {#pomidor}

A simple pomodoro technique timer.

```emacs-lisp
(use-package pomidor
  :straight t
  :commands (pomidor)
  :init
  (my-leader-def "aP" #'pomidor)
  :config
  (setq pomidor-sound-tick nil)
  (setq pomidor-sound-tack nil)
  (general-define-key
   :states '(normal)
   :keymaps 'pomidor-mode-map
   (kbd "q") #'quit-window
   (kbd "Q") #'pomidor-quit
   (kbd "R") #'pomidor-reset
   (kbd "h") #'pomidor-hold
   (kbd "H") #'pomidor-unhold
   (kbd "RET") #'pomidor-stop
   (kbd "M-RET") #'pomidor-break))
```


#### Calendar {#calendar}

Emacs' built-in calendar. Can even calculate sunrise and sunset times.

```emacs-lisp
(setq calendar-date-style 'iso) ;; YYYY/mm/dd
(setq calendar-week-start-day 1)
(setq calendar-time-display-form '(24-hours ":" minutes))

(setq calendar-latitude 59.9375)
(setq calendar-longitude 30.308611)
```


### Fun {#fun}


#### Discord integration {#discord-integration}

Integration with Discord. Shows which file is being edited in Emacs.

In order for this to work in Guix, a service is necessary - [Discord rich presence]({{< relref "Desktop" >}}).

```emacs-lisp
(use-package elcord
  :straight t
  :if (and (or
	    (string= (system-name) "indigo")
	    (string= (system-name) "eminence"))
	   (not my/slow-ssh))
  :config
  (elcord-mode)
  (add-to-list 'elcord-boring-buffers-regexp-list
	       (rx bos (+ num) "-" (+ num) "-" (+ num) ".org" eos))
  (add-to-list 'elcord-boring-buffers-regexp-list
	       (rx bos (= 14 num) "-" (* not-newline) ".org" eos))
  )
```


#### Snow {#snow}

```emacs-lisp
(use-package snow
  :straight (:repo "alphapapa/snow.el" :host github)
  :commands (snow))
```


#### Zone {#zone}

```emacs-lisp
(use-package zone
  :ensure nil
  :config
  (setq original-zone-programs (copy-sequence zone-programs)))

(defun my/zone-with-select ()
  (interactive)
  (ivy-read "Zone programs"
	    (cl-pairlis
	     (cl-mapcar 'symbol-name original-zone-programs)
	     original-zone-programs)
	    :action (lambda (elem)
		      (setq zone-programs (vector (cdr elem)))
		      (zone))))
```


## Guix settings {#guix-settings}

| Guix dependency     | Description                   |
|---------------------|-------------------------------|
| emacs-vterm         | A vterm package               |
| ripgrep             | A recursive search tool       |
| the-silver-searcher | Another recursive search tool |

<a id="code-snippet--packages"></a>
```emacs-lisp
(my/format-guix-dependencies)
```

```scheme
(specifications->manifest
 '("emacs-native-comp"
   <<packages()>>))
```