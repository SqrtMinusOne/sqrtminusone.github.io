(require 'package)
(require 'vc)
(require 'files)

(message (concat (vc-find-root default-directory ".git") "org/configs"))
(cd (concat (vc-find-root default-directory ".git") "org/configs"))

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
(setq org-hugo-anchor-functions '(org-hugo-get-page-or-bundle-name
                                  org-hugo-get-custom-id
                                  org-hugo-get-md5))

(setq org-hugo-section "configs")
(setq org-hugo-base-dir (vc-find-root default-directory ".git"))

;; (setq org-hugo-export-with-toc 6)

(setq my/config-files '("README.org"
                        "Emacs.org"
                        "Desktop.org"
                        "Console.org"
                        "Mail.org"
                        "Guix.org"))


(dolist (file my/config-files)
  (copy-file (expand-file-name
              (format "%s/repos/dotfiles/%s"
                      (vc-find-root default-directory ".git")
                      file))
             file 'overwrite))

;; (copy-directory (expand-file-name "~/dot-stats/img") "dot-stats/img" t t)

(dolist (file my/config-files)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents file)
    (unless (string-equal file "README.org")
      (org-make-toc))
    (replace-string "conf-space" "vim" t (point-min) (point-max))
    (replace-string "conf-unix" "ini" t (point-min) (point-max))
    (replace-string "conf-windows" "ini" t (point-min) (point-max))
    (replace-string "conf-xdefaults" "vim" t (point-min) (point-max))
    (replace-string "conf-toml" "toml" t (point-min) (point-max))
    (replace-string ":noweb yes" ":noweb no-export" nil (point-min) (point-max))
    (setq-local buffer-file-name file)
    (message "Publish %s" file)
    (org-hugo-export-to-md)))
