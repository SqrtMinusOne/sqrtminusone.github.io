(defmacro my/load-history--iter-load-history (&rest body)
  "Iterate through `load-history'.

The following are bound in BODY in the process:
- file-item is one item in `load-history' providing a feature, given
  that it's not \"-autoloads\";
- provide-symbol is the feature name, provided by the item;
- requires is the list of feature, required by the item."
  `(dolist (file-item load-history)
     (let (provide-symbol requires)
       (dolist (symbol-item (cdr file-item))
         (pcase (car-safe symbol-item)
           ('require (push (cdr symbol-item) requires))
           ('provide (setq provide-symbol (cdr symbol-item)))))
       (when (and provide-symbol
                  (not (string-match-p
                        (rx "-autoloads" eos)
                        (symbol-name provide-symbol))))
         ,@body))))

(defun my/load-history--get-feature-required-by ()
  "Get the hashmap of which features were required by which.

The key is the feature name; the value is the list of features in
which it was required."
  (let ((feature-required-by (make-hash-table)))
    (my/load-history--iter-load-history
     (dolist (require-symbol requires)
       (puthash require-symbol
                (cons provide-symbol
                      (gethash require-symbol feature-required-by))
                feature-required-by)))
    feature-required-by))

(defun my/load-history--get-feature-tree (feature-name feature-hash &optional found-features)
  "Get the tree of features with FEATURE-NAME as the root.

FEATURE-HASH is the hashmap with features as keys and lists of
features as values.

FOUND-FEATURES is the recursive paratemer to avoid infinite loop.

The output is a cons cell, with the car being the feature name
and the cdr being a list cons cell of the same kind."
  (unless found-features
    (setq found-features (make-hash-table)))
  (puthash feature-name t found-features)
  (prog1
      (cons feature-name
            (mapcar
             (lambda (dependent-feature-name)
               (if (gethash dependent-feature-name found-features)
                   (cons dependent-feature-name 'loop)
                 (my/load-history--get-feature-tree
                  dependent-feature-name feature-hash found-features)))
             (gethash feature-name feature-hash)))
    (remhash feature-name found-features)))

(setq use-package-compute-statistics t)

(defun my/load-history--narrow-tree-by-use-package (tree)
  "Leave only features managed by `use-package' in TREE."
  (when (= (hash-table-count use-package-statistics) 0)
    (user-error "use-package-statistics is empty"))
  (if (eq (cdr tree) 'loop)
      (cons (car tree) nil)
    (let (res)
      (dolist (child (cdr tree))
        (let ((found-p (gethash (car child) use-package-statistics))
              (child-narrowed (my/load-history--narrow-tree-by-use-package child)))
          (if found-p
              (push child-narrowed res)
            (dolist (grandchild (cdr child-narrowed))
              (push grandchild res)))))
      (cons (car tree)
            (seq-uniq
             (nreverse res)
             (lambda (a b)
               (eq (car a) (car b))))))))

(defun my/load-history--render-feature-tree-recur (tree &optional level)
  "Render the feature tree recursively.

TREE is the output of `my/load-history--get-feature-tree'.  LEVEL is
the recursion level."
  (unless level (setq level 1))
  (insert (make-string level ?*) " " (symbol-name (car tree)))
  (if (eq (cdr tree) 'loop)
      (insert ": loop\n")
    (insert "\n")
    (dolist (feature (cdr tree))
      (my/load-history--render-feature-tree-recur feature (1+ level)))))

(defvar my/load-history-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map outline-mode-map)
    (define-key map (kbd "q") (lambda () (interactive) (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        (kbd "TAB") #'outline-toggle-children
        "q" (lambda () (interactive) (quit-window t))))
    map))

(define-derived-mode my/load-history-tree-mode outline-mode "Load Tree"
  "Display load tree."
  (setq-local buffer-read-only t))

(defun my/completing-read-features-or-packages ()
  "Read a feature name or a `use-package'-package from the minibuffer.

The choice depends on the value of the prefix argument."
  (intern
   (if (equal current-prefix-arg '(4))
       (completing-read "Package: " (cl-loop for p being the hash-keys of
                                             use-package-statistics
                                             collect p))
     (completing-read "Feature: " features))))

(defun my/load-history-feature-dependents (feature-name &optional narrow-use-package)
  "Display the tree of features that depend on FEATURE-NAME.

If NARROW-USE-PACKAGE is non-nil, only show the features that are
managed by `use-package'."
  (interactive (list (my/completing-read-features-or-packages)
                     (equal current-prefix-arg '(4))))
  (let* ((feature-required-by (my/load-history--get-feature-required-by))
         (tree (my/load-history--get-feature-tree feature-name feature-required-by))
         (buffer (generate-new-buffer (format "*feature-dependents-%s*" feature-name))))
    (when narrow-use-package
      (setq tree (my/load-history--narrow-tree-by-use-package tree)))
    (with-current-buffer buffer
      (my/load-history--render-feature-tree-recur tree)
      (my/load-history-tree-mode)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun my/load-history--get-feature-requires ()
  "Get the hashmap of which features require which.

The key is the feature name; the value is the list of features it
requires."
  (let ((feature-requires (make-hash-table)))
    (my/load-history--iter-load-history
     (dolist (require-symbol requires)
       (puthash provide-symbol
                (cons require-symbol
                      (gethash provide-symbol feature-requires))
                feature-requires)))
    feature-requires))

(defun my/load-history-feature-dependencies (feature-name &optional narrow-use-package)
  "Display the tree of features that FEATURE-NAME depends on.

If NARROW-USE-PACKAGE is non-nil, only show the features that are
managed by `use-package'."
  (interactive (list (my/completing-read-features-or-packages)
                     (equal current-prefix-arg '(4))))
  (let* ((feature-requires (my/load-history--get-feature-requires))
         (tree (my/load-history--get-feature-tree feature-name feature-requires))
         (buffer (generate-new-buffer (format "*feature-dependencies-%s*" feature-name))))
    (when narrow-use-package
      (setq tree (my/load-history--narrow-tree-by-use-package tree)))
    (with-current-buffer buffer
      (my/load-history--render-feature-tree-recur tree)
      (my/load-history-tree-mode)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(require 'dired)

(eval-when-compile
  (require 'dired))

(defun my-function-with-dired ()
  "Do something important with dired."
  (interactive)
  (require 'dired))
