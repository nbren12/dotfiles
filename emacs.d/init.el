
; Using Quelpa to manage packages
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(mapc 'quelpa '(evil evil-org evil-surround evil-leader org
		     reftex auctex auto-complete yasnippet deft
		     projectile jedi flycheck))

; Requires
(require 'org)
(require 'reftex)


; Settings 

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


; Evil keyboard maps
; (define-key evil-normal-state-map ";" 'execute-extended-command)

(define-key evil-insert-state-map "jj" 'evil-normal-state)

(evil-leader/set-key 
 "dd" 'deft 
 "op" 'org-preview-latex-fragment
 "s"  'speedbar-get-focus)

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
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(setq-default ac-sources '(ac-source-filename ac-source-functions
				      ac-source-symbols
				      ac-source-variables ))

; Projectile

(projectile-global-mode)

; Path
(setenv "PATH" (concat "/usr/local/bin/:/usr/texbin" ":" (getenv "PATH")))

; Global Stuff
(add-hook 'after-init-hook #'global-flycheck-mode)


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

; Deft settings
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Dropbox/Notes")
(setq deft-use-filename-as-title t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Dropbox/Notes/Climate Dynamics Rejection.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
