;; -*- lexical-binding: t; -*-
(require 'package)
(require 'vc)
(require 'files)

(setq byte-compile-warnings nil)

;; Initialize the package system
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)

;; Org Hugo
(straight-use-package 'org)

(defun my/org-link-search-headlines-case-fold-around
    (fn words &optional ignore-pipes)
  "Retry FN case-insensitively when searching for WORDS in headlines."
  (or (funcall fn words ignore-pipes)
      (let ((wanted (mapcar #'upcase words))
            (normalizers (if ignore-pipes
                             '(statistics-cookies pipe-chars)
                           '(statistics-cookies))))
        (goto-char (point-min))
        (catch :found
          (while (re-search-forward org-outline-regexp-bol nil t)
            (when (equal
                   wanted
                   (mapcar
                    #'upcase
                    (split-string
                     (org-link-normalize-string
                      (org-get-heading t t t t)
                      normalizers))))
              (throw :found t)))
          nil))))

;; Org commit 76dc6e358 accidentally made the final headline comparison
;; case-sensitive.  Keep the old behavior until that regression is fixed.
(with-eval-after-load 'ol
  (when (fboundp 'org-link--search-headlines)
    (unless (advice-member-p
             #'my/org-link-search-headlines-case-fold-around
             'org-link--search-headlines)
      (advice-add 'org-link--search-headlines :around
                  #'my/org-link-search-headlines-case-fold-around))))

(defvar-local my/org-hugo-heading-slugs nil)

(defun my/increase-slug (string)
  (if (string-match (rx "-" (1+ num) eos) string)
      (replace-match
       (concat
        "-"
        (thread-first
          (match-string 0 string)
          (substring 1)
          (string-to-number)
          (+ 1)
          (number-to-string)))
       t t string)
    (concat string "-" "1")))

(defun my/org-hugo-get-heading-slug (element info)
  (let* ((title (org-export-data-with-backend
                 (org-element-property :title element) 'md info))
         (slug (org-string-nw-p (org-hugo-slug title :allow-double-hyphens))))
    (unless my/org-hugo-heading-slugs
      (setq my/org-hugo-heading-slugs (make-hash-table :test 'equal)))
    (unless (or (null slug) (string-empty-p slug))
      (while (gethash slug my/org-hugo-heading-slugs)
        (setq slug (my/increase-slug slug)))
      (puthash slug t my/org-hugo-heading-slugs))
    slug))

(use-package ox-hugo
  :straight t
  :config
  (setq org-hugo-anchor-functions '(org-hugo-get-page-or-bundle-name
                                    org-hugo-get-custom-id
                                    my/org-hugo-get-heading-slug
                                    org-hugo-get-md5))
  (setq org-hugo-base-dir (vc-find-root default-directory ".git")))

;; Org Make TOC

(use-package org-make-toc
  :straight t
  :config
  (setq org-make-toc-link-type-fn #'org-make-toc--link-entry-org))


;; No clue what's wrong here
(defun my/org-blackfriday-table-cell-alignment-around (fn &rest args)
  (condition-case error
      (apply fn args)
    (error
     (apply #'org-export-table-cell-alignment args))))

;; (advice-add #'org-blackfriday-table-cell-alignment
;;             :around #'my/org-blackfriday-table-cell-alignment-around)

;; Export dotfiles

(setq my/config-files
      '("README.org"
        "Emacs.org"
        "Desktop.org"
        "Console.org"
        "Mail.org"
        "Arch.org"
        "Guix.org"))

(defun my/export-dotfiles ()
  (cd (concat (vc-find-root default-directory ".git") "org/configs"))
  (dolist (file my/config-files)
    (copy-file (expand-file-name
                (format "%s/repos/dotfiles/%s"
                        (vc-find-root default-directory ".git")
                        file))
               file 'overwrite))
  (copy-directory
   (expand-file-name
    (format "%s/repos/dotfiles/dot-imgs/"
            (vc-find-root default-directory ".git")))
   "dot-imgs" t t)
  (dolist (file my/config-files)
    (with-temp-buffer
      (let ((org-mode-hook nil) (text-mode-hook nil))
        (org-mode))
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
      (let ((org-hugo-section "configs")
            (org-mode-hook nil)
            (text-mode-hook nil))
        (org-hugo-export-to-md)))))

;; Export packages
(setq my/emacs-packages
      '("lyrics-fetcher"
        "pomm"
        "elfeed-summary"
        "exwm-modeline"
        "perspective-exwm"
        "org-journal-tags"
        "elfeed-sync"
        "password-store-completion"
        "avy-dired"
        "reverso"
        "micromamba"
        "biome"
        "org-clock-agg"
        "eshell-atuin"))

(defun my/export-packages ()
  (cd (concat (vc-find-root default-directory ".git") "org/packages"))
  (let ((root (vc-find-root default-directory ".git")))
    (dolist (package my/emacs-packages)
      (copy-file
       (expand-file-name
        (format "%s/repos/%s/README.org" root package))
       (format "./%s.org" package) 'overwrite)
      (when (file-directory-p (format "%s/repos/%s/img" root package))
        (copy-directory
         (expand-file-name
          (format "%s/repos/%s/img" root package))
         (format "./static/%s-img" package) t t))
      (with-current-buffer (generate-new-buffer "tmp")
        (insert-file-contents (format "./%s.org" package))
        (goto-char (point-min))
        (insert
         "#+HUGO_CUSTOM_FRONT_MATTER: :repo "
         (let ((default-directory (format "%s/repos/%s/" root package)))
           (string-trim
            (shell-command-to-string
             "git remote get-url origin | sed 's/.*SqrtMinusOne\\/\\(.*\\)\\.git/\\1/'")))
         "\n")
        (when-let
            (published-at
             (with-temp-buffer
               (insert-file-contents (format "%s/repos/%s/%s.el" root package package))
               (goto-char (point-min))
               (when (re-search-forward (rx bol ";; Published-At:" (0+ space) (group (1+ nonl))) nil t)
                 (substring-no-properties
                  (match-string 1)))))
          (insert
           "#+DATE: " published-at "\n"))
        (replace-string
         "./img/" (format "./static/%s-img/" package) nil (point-min) (point-max))
        (setq-local buffer-file-name (format "./%s.org" package))
        (message "Publish %s" package)
        (let ((org-hugo-section "packages")
              (org-mode-hook nil)
              (text-mode-hook nil))
          (org-hugo-export-to-md))))))

(my/export-dotfiles)
(my/export-packages)
