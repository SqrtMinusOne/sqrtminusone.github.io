+++
title = "Replacing Jupyter Notebook with Org Mode"
author = ["Pavel"]
date = 2021-04-08
tags = ["emacs", "org"]
draft = true
+++

## Why? {#why}


## Basic setup {#basic-setup}

There are multiple ways of doing literate programming with Python in Emacs, [ein](https://github.com/millejoh/emacs-ipython-notebook) being one of the notable alternatives.

However, I go with the [emacs-jupyter](https://github.com/nnicandro/emacs-jupyter) package. Installing it is pretty straightforward, e.g. `use-package` with `straight.el`:

```emacs-lisp
(use-package jupyter
  :straight t)
```

Then, we have to enable languages for `org-babel`. The following isn't the best practice for startup performance time, but the least problematic in my experience.

```emacs-lisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   ;; Python & Jupyter
   (python . t)
   (jupyter . t)))
```

That adds Org source blocks with names like `jupyter-LANG`, e.g. `jupyter-python`. To use just `LANG` src blocks, call the following function after `org-babel-do-load-languages`:

```emacs-lisp
(org-babel-jupyter-override-src-block "python")
```

That overrides built-in `python` block with `jupyter-python`.

If you use [ob-async](https://github.com/astahlman/ob-async), you have to set `jupyter-LANG` blocks as ignored by this package, because emacs-jupyter has async execution of its own.

```emacs-lisp
(setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
```


## Environments {#environments}

So, we've set up a basic emacs-jupyter configuration.

The catch here is that Jupyter should be available on Emacs startup (at the time of evaluation of the `emacs-jupyter` package, to be precise). That means, if you are launching Emacs with something like an application launcher, global Python & Jupyter will be used.

```python
import sys
sys.executable
```

```text
/usr/bin/python3
```

Which is probably not what we want. To resolve that, we have to make the right Python available at the required time.


### Anaconda {#anaconda}

If you were using Jupyter Lab or Notebook before, there is a good change you used it via [Anaconda](https://anaconda.org/). If not, in a nutshell, it is a package & environment manager, which specializes on Python & R, but also supports a whole lot of stuff like Node.js. In my opinion, it is the easiest way to manage multiple Python installations if you don't use some advanced package manager like Guix.

As one may expect, there is an Emacs package called [conda.el](https://github.com/necaris/conda.el) to help working with conda environments in Emacs. We have to put it somewhere before `emacs-jupyter` package and call `conda-env-activate`:

```emacs-lisp
(use-package conda
  :straight t
  :config
  (setq conda-anaconda-home (expand-file-name "~/Programs/miniconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/Programs/miniconda3/"))
  (setq conda-env-subdirectory "envs"))

(unless (getenv "CONDA_DEFAULT_ENV")
  (conda-env-activate "base"))
```

If you have Anaconda installed on a custom path, as I do, you'd have to add these 3 `setq` in the `:config` section. Also, there is no point in activating environment if Emacs is somehow already lauched in an environment.

That'll give us Jupyter from a base conda environment.


### virtualenv {#virtualenv}

TODO


### Switching an environment {#switching-an-environment}

However, as you may have noticed, `emacs-jupyter` will always use the Python kernel found on startup. So if you switch to a new environment, the code will still be ran in the old one, which is not too convinient.

Fortunately, to fix that we have only to refresh the jupyter kernelspecs:

```emacs-lisp
(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))
```

Calling `M-x my/jupyter-refresh-kernelspecs` after a switch will give you a new kernel. Just keep in mind that the kernelspec seems to be attached to a session, so you'd also have to change the session name to get a new kernel.

```python
import sys
sys.executable
```

```text
/home/pavel/Programs/miniconda3/bin/python
```

```emacs-lisp
(conda-env-activate "ann")
```

```python
import sys
sys.executable
```

```text
/home/pavel/Programs/miniconda3/bin/python
```

```emacs-lisp
(my/jupyter-refresh-kernelspecs)
```

```python
import sys
sys.executable
```

```text
/home/pavel/Programs/miniconda3/envs/ann/bin/python
```


## Programming {#programming}

To test if everything is working correctly, run `M-x jupyter-run-repl`, which should give you a REPL with a chosen kernel. If so, we can finally start using Python in org mode.

```text
#+begin_src python :session hello :async yes
print('Hello, world!')
#+end_src

#+RESULTS:
: Hello, world!
#+end_src
```

To avoid repeating similar arguments for the src block, we can set the `header-args` property at the start of the file:

```text
#+PROPERTY: header-args:python :session hello
#+PROPERTY: header-args:python+ :async yes
```

When a kernel is initialized, an associated REPL buffer is also created with a name like `*jupyter-repl[python 3.9.2]-hello*`. That may also come in handy, although you may prefer running a standalone REPL, doing which will be discussed further.

Also, one advantage of emacs-jupyter is that kernel requests for input are queried through the minibuffer. So, you can run a code like this:

```text
#+begin_src python
name = input('Name: ')
print(f'Hello, {name}!')
#+end_src

#+RESULTS:
: Hello, Pavel!
```

without any additional setup.


## Code output {#code-output}


### Images {#images}

Image output should work out of box. Run `M-x org-toggle-inline-images` (`C-c C-x C-v`) after the execution to see the image inline.

```text
#+begin_src python
import matplotlib.pyplot as plt
fig, ax = plt.subplots()
ax.plot([1, 2, 3, 4], [1, 4, 2, 3])
pass
#+end_src

#+RESULTS:
[[file:./.ob-jupyter/86b3c5e1bbaee95d62610e1fb9c7e755bf165190.png]]
```

However, there is some room for improvement. First, you can add the following hook if you don't want press this awkward keybinding every time:

```emacs-lisp
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
```

Second, we may override the image save path like this:

```text
#+begin_src python :file img/hello.png
import matplotlib.pyplot as plt
fig, ax = plt.subplots()
ax.plot([1, 2, 3, 4], [1, 4, 2, 3])
pass
#+end_src

#+RESULTS:
[[file:img/hello.png]]
```

That can save you a `savefig` call if the image has to be used somewhere further.

Finally, by default the image has tranparent background and ridiculously small size. That can be fixed with some matplotlib settings:

```python
import matplotlib as mpl

mpl.rcParams['figure.dpi'] = 200
mpl.rcParams['figure.facecolor'] = '1'
```

At the same time, we can set image width to prevent images from becoming too large. I prefer to do it inside a `emacs-lisp` code block in the same org file:

```emacs-lisp
(setq-local org-image-actual-width '(1024))
```


### Basic tables {#basic-tables}

If you are evaluating something like pandas DataFrame, it will be outputted in the HTML format, wrapped in the `begin_export` block. To view the data in text format, you can set `:display plain`:

```text
#+begin_src python :display plain
import pandas as pd
pd.DataFrame({"a": [1, 2], "b": [3, 4]})
#+end_src

#+RESULTS:
:    a  b
: 0  1  3
: 1  2  4
```

Another solution is to use something like the [tabulate](https://pypi.org/project/tabulate/) package:

```text
#+begin_src python
import pandas as pd
import tabulate
df = pd.DataFrame({"a": [1, 2], "b": [3, 4]})
print(tabulate.tabulate(df, headers=df.columns, tablefmt="orgtbl"))
#+end_src

#+RESULTS:
: |    |   a |   b |
: |----+-----+-----|
: |  0 |   1 |   3 |
: |  1 |   2 |   4 |
```


### HTML & other rich output {#html-and-other-rich-output}

Yet another solution is to use emacs-jupyter's option `:pandoc t`, which invokes pandoc to convert HTML, LaTeX and Markdown to Org. Predictably, this is slower than the options above.

```text
#+begin_src python :pandoc t
import pandas as pd
df = pd.DataFrame({"a": [1, 2], "b": [3, 4]})
df
#+end_src

#+RESULTS:
:RESULTS:
|   | a | b |
|---+---+---|
| 0 | 1 | 3 |
| 1 | 2 | 4 |
:END:
```

Finally, every once in a while I have to view an actual, unconverted HTML in a browser, e.g. when using [folium](https://python-visualization.github.io/folium/) or [displaCy](https://spacy.io/usage/visualizers).

To do that, I've written a small function, which performs `xdg-open` on the HTML export block under the cursor:

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

`f.el` is used by a lot of packages, including the above mentioned `conda.el`, so you probably already have it installed.

Put a cursor on the `begin_export html` block and run `M-x my/org-view-html`.

There also [seems to be widgets support](https://github.com/nnicandro/emacs-jupyter#building-the-widget-support-experimental) in emacs-jupyter, but I wasn't able to make it work.


### DataFrames {#dataframes}

Last but not least option I want to mention here is specifically about pandas' DataFrames. There aren't many good options to view the full dataframe inside Emacs. The way I can think of is to save the dataframe in csv and view it with `csv-mode`.

However, there are standalone packages to view dataframes. My favorite one is [dtale](https://github.com/man-group/dtale), which is a Flask + React app designed just for that purpose. It has a rather extensive list of features, including charting, basic statistical instruments, filters, etc. [Here](http://alphatechadmin.pythonanywhere.com/dtale/main/1) is an online demo.

And example usage:

```python
import dtale
d = dtale.show(df)
d.open_browser() # Or get an URL from d._url
```

Another notable alternative is [PandasGUI](https://github.com/adamerose/pandasgui), which, as one can guess, is a GUI (PyQt5) application, although it uses QtWebEngine inside.

The obvious downside is, of course, that these applications are huge ones with lots of dependencies, and they have to be installed in the same environment as your project.


## Remote kernels {#remote-kernels}

There are yet some problems in the current configuration.

-   Input/output handling is far from perfect. For instance, (at least in my configuration) Emacs tends to get slow for log-like outputs, e.g. Keras with `verbose=2`. It may even hang if an output is a one long line.
-   `ipdb` behaves rather awkwardly if called from an `src` block, although it at least will let you type `quit`.
-   Whenever you close Emacs, kernels are stopped, so you'd have to execute the code again on the next start.


### Using a "remote" kernel {#using-a-remote-kernel}

For the reasons above I prefer to use a standalone kernel. To do that, execute the following command in the path and environment you need:

```bash
jupyter kernel --kernel=python
```

After the kernel is launched, put the path to the connection file into the `:session` header and press `C-c C-c` to refresh the setup:

```text
#+PROPERTY: header-args:python :session /home/pavel/.local/share/jupyter/runtime/kernel-e770599c-2c98-429b-b9ec-4d1ddf5fc16c.json
```

To open a REPL, run `M-x jupyter-connect-repl` and select the given JSON. Or launch a standalone REPL like this:

```bash
jupyter qtconsole --existing kernel-e770599c-2c98-429b-b9ec-4d1ddf5fc16c.json
```


### Some automation {#some-automation}

Now, I wouldn't use Emacs if it was impossible to automate at least some the listed steps. So here are some functions I've written.

First, we need to get open ports on the system:

```emacs-lisp
(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))
```

Then, list the available kernel JSONs:

```emacs-lisp
(setq my/jupyter-runtime-folder (expand-file-name "~/.local/share/jupyter/runtime"))

(defun my/list-jupyter-kernel-files ()
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes my/jupyter-runtime-folder t ".*kernel.*json$")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))
```

And query the user for an running kernel:

```emacs-lisp
(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
        (files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: "
     (seq-filter
      (lambda (file)
        (member (cdr file) ports))
      files))))
```

After which we can use the `my/select-jupyter-kernel` function however we want:

```emacs-lisp
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

The first function, which simply inserts the path to the kernel, is meant to be used on the `:session` header. I can go even further and locate the header automatically, but that's an idea for the next time.

The second one opens a REPL provided by emacs-jupyter. The `t` argument is necessary to pop up the REPL immediately.

The last one launches Jupyter QtConsole. `setsid` is required to run a console in a new session, so it won't close together with Emacs.


### Cleaning up {#cleaning-up}

I've also noticed that there are JSON files left in the runtime folder whenever kernel isn't stopped correctly. So here is a cleanup function.

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
        (delete-file (car file)))))
```


## Export {#export}

A lot of articles were written on the subject of Org Mode export, so I will just cover my particular setup.


### HTML {#html}

Export to html is pretty straightforward and should work out of box with `M-x org-html-export-to-html`. However, we can improve the output a bit.

First, we can add a custom CSS to the file:

```text
#+HTML_HEAD: <link rel="stylesheet" type="text/css" href="https://gongzhitaao.org/orgcss/org.css"/>
```


### LaTeX {#latex}


### ipynb {#ipynb}
