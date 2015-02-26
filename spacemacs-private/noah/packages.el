;;; packages.el --- noah Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar noah-packages
  '(
    ;; package noahs go here
    deft magit htmlize ;irony
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar noah-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function noah/init-<package-noah>
;;
;; (defun noah/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun noah/init-deft ()
  (use-package deft
    :init
     ; Deft settings
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-directory "~/Dropbox/notes")
    (setq deft-use-filename-as-title t)

    :config
    (progn
                                        ; Deft keybinding
      (evil-leader/set-key "od" 'deft) )))
(defun noah/init-magit ()
  (use-package magit
    :config
    (evil-leader/set-key "og" 'magit-status)))

(defun justincase ()
  (use-package irony
    :config
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)

      ;; replace the `completion-at-point' and `complete-symbol' bindings in
      ;; irony-mode's buffers by irony-mode's function
      (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))))
