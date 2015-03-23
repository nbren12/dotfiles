;;; init.el --- My init.el
;;; Commentary:
;;;
;;; Code:
(server-start)


(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)


; List of plugins
(setq my-plugins '(use-package evil-org evil-surround evil-leader
		     evil-nerd-commenter org cdlatex reftex
		     company yasnippet deft company-anaconda
		     projectile  flycheck idomenu 
		     magit cython-mode monokai-theme leuven-theme
		     ido-vertical-mode))

; Install list of plugins 
(dolist (plugin my-plugins)
      (unless (package-installed-p plugin) 
	(package-install plugin)))

(require 'use-package)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (progn

    (global-evil-leader-mode)
    (evil-mode '1)
    (global-evil-surround-mode 1)
    (add-hook 'prog-mode-hook 'hs-minor-mode)
    (add-hook 'prog-mode-hook 'linum-mode)
    (add-hook 'view-mode-hook 'evil-emacs-state)
    (setq-default evil-symbol-word-search 'symbol)

    (define-key evil-normal-state-map (kbd "") 'evil-toggle-fold)))

(use-package evil-leader
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key 
      "hb" 'helm-bookmarks
      "ro" 'helm-occur
      "rr" 'rgrep
      "rg" 'helm-git-grep
      "u" 'universal-argument
      "ta" 'align-regexp
      "cc" 'compile
      "cr" 'recompile
      "ff" 'ido-find-file
      "fr" 'recentf-ido-find-file
      "bb" 'ido-switch-buffer
      "bk" 'ido-kill-buffer
      "dd" 'deft
      "op" 'org-preview-latex-fragment
      "s"  'speedbar-get-focus
      "." 'eshell
      "gs" 'magit-status)))

    
  
(setq evilnc-hotkey-comment-operator "gc")
(require 'evil-nerd-commenter)

; window movement
(global-set-key (kbd "C-j") 'other-window)

(use-package yasnippet
  :config
  (progn 
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode t)))


(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "<S-tab>") 'company-complete)


					; Python


(use-package company-anaconda
  :config
  (progn
    (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook 'anaconda-mode)))


(defun python-setup-shell ()
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
	    ;; python-shell-interpreter-args (if (system-is-mac)
	    ;;                                   "--gui=osx --matplotlib=osx --colors=Linux"
	    ;;                                 (if (system-is-linux)
	    ;;                                     "--gui=wx --matplotlib=wx --colors=Linux"))
	    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
	    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
	    python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
	    python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
	    python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    (setq python-shell-interpreter "python")))

(add-hook 'python-mode-hook 'python-setup-shell)

; Path
(setenv "PATH" (concat "/usr/local/bin:/usr/texbin" ":" (getenv "PATH")))

					; Make "_" part of word
(modify-syntax-entry ?_ "w" )

; Global Stuff
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Interactive completion stuff
;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-ls-git)
;; (setq helm-bookmark-show-location t)

(use-package helm
  :init
  (progn
    (require 'helm-config)
    )
  :config
  (progn
    (evil-leader/set-key "bs" 'helm-mini)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (evil-leader/set-key "h" 'helm-command-prefix)
    (helm-mode 1)
    ))


(use-package ido
  :config
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (define-key evil-normal-state-map " b" 'ido-switch-buffer)))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

(use-package imenu
  :config
  (progn
    (evil-leader/set-key "bi" 'idomenu)))
  


;;; Useful for used files
(use-package recentf
  :config
  (progn
    (recentf-mode 1)
    (global-set-key "\C-x\ \C-r" 'recentf-open-files)))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))



(use-package deft
  :config
  (progn
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-directory "~/Dropbox/notes")
    (setq deft-use-filename-as-title t)
    (add-hook 'deft-mode-hook 'evil-emacs-state)))

(use-package org
  :config
  (progn
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (add-hook 'org-mode-hook 'org-indent-mode)))

;Auctex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

; Remove superfluous mode line indicators
(defun hbin-remove-mm-lighter (mm)
  "Remove minor lighter from the mode line."
  (setcar (cdr (assq mm minor-mode-alist)) nil))

(hbin-remove-mm-lighter 'undo-tree-mode)
(hbin-remove-mm-lighter 'yas-minor-mode)


;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(provide 'init)
