
; Using Quelpa to manage packages
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(mapc 'quelpa '(evil evil-org evil-surround evil-leader org
		     cdlatex reftex auctex auto-complete
		     yasnippet deft projectile jedi flycheck helm
		     helm-ls-git magit cython-mode))

; Requires
(require 'org)
(require 'reftex)
(require 'magit)


; Evil Settings 

(setq evil-want-C-u-scroll t)

(require 'evil-leader)
(require 'evil)
(require 'evil-surround)

(global-evil-leader-mode)
(evil-mode '1)
(global-evil-surround-mode 1)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'view-mode-hook 'evil-emacs-state)


; Evil keyboard maps
; (define-key evil-normal-state-map ";" 'execute-extended-command)


(evil-leader/set-key 
 "dd" 'deft 
 "op" 'org-preview-latex-fragment
 "s"  'speedbar-get-focus
 "." 'eshell
 "gs" 'magit-status)

(define-key evil-normal-state-map (kbd "") 'evil-toggle-fold)

; window movement
(global-set-key (kbd "C-j") 'other-window)

;; Turn on snippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t)

;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)


;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(setq-default ac-sources '(ac-source-filename ac-source-functions
				      ac-source-symbols
				      ac-source-variables ))

; Path
(setenv "PATH" (concat "/usr/local/bin/:/usr/texbin" ":" (getenv "PATH")))

; Global Stuff
(add-hook 'after-init-hook #'global-flycheck-mode)


; Helm (really awesome for interactive completion)
(require 'helm)
(require 'helm-config)
(require 'helm-ls-git)
(setq helm-bookmark-show-location t)

(global-set-key (kbd "M-x") 'helm-M-x)


(define-key evil-normal-state-map "  " 'helm-mini)
(define-key evil-normal-state-map " b" 'helm-bookmarks)
(define-key evil-normal-state-map " i" 'helm-imenu)
(define-key evil-normal-state-map " p" 'helm-browse-project)


; Python mode stuff
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'auto-complete-mode)
(setq jedi:complete-on-dot t)                 ; optional
(define-key evil-normal-state-map "`pb" (lambda () (insert "from ipdb import set_trace ; set_trace()")))

;; IPython
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Debugger
(defun pdb-run () 
  (interactive)
  (pdb (concat "python -m pdb " buffer-file-name)))

(evil-leader/set-key "pd" 'pdb-run)

; Deft settings
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Dropbox/Notes")
(setq deft-use-filename-as-title t)
(add-hook 'deft-mode-hook 'evil-emacs-state)

; Org-mode settings
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
