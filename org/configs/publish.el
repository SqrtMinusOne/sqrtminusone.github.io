(require 'package)

(setq package-user-dir (expand-file-name "./.packages"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq package-user-dir (expand-file-name "./.packages"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package ox-hugo
  :ensure t)

(use-package org-make-toc
  :ensure t)

(setq org-make-toc-link-type-fn #'org-make-toc--link-entry-org)

(setq org-hugo-section "configs")
(setq org-hugo-base-dir "../../")

(setq my/config-files '("README.org"
                        "Emacs.org"
                        "Desktop.org"
                        "Console.org"
                        "Mail.org"
                        "Guix.org"))

(dolist (file my/config-files)
  (copy-file (expand-file-name (format "~/%s" file)) file 'overwrite))

(copy-directory (expand-file-name "~/dot-stats/img") "dot-stats/img" t t)

(dolist (file my/config-files)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents file)
    (unless (string-equal file "README.org")
      (setq org-make-toc-link-type-fn #'org-make-toc--link-entry-org)
      (org-make-toc))
    (setq-local buffer-file-name file)
    (message "Publish %s" file)
    (org-hugo-export-to-md)))
