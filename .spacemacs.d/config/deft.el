;; deft-options
(use-package deft
  :config
  (progn
    (evil-set-initial-state 'deft-mode 'emacs))
  :init
  (progn
    (setq deft-default-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-directory "~/Dropbox/notes")
    (setq deft-use-filter-string-for-filename  t)))
