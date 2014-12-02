
; Using Quelpa to manage packages
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(mapc 'quelpa '(evil evil-org evil-surround org reftex auctex
		     auto-complete yasnippet deft projectile  
		     jedi flycheck))

; Requires

(require 'org)
(require 'reftex)
(require 'auto-complete)
(require 'yasnippet)
(require 'evil-surround)


; Settings 
(evil-mode '1)
(global-evil-surround-mode 1)

; Evil keyboard maps
;(define-key evil-normal-state-map ";" 'execute-extended-command)
; (define-key evil-insert-state-map "jj" 'evil-normal-state)
(define-key evil-normal-state-map "`dd" 'deft)
(define-key evil-normal-state-map "`op" 'org-preview-latex-fragment)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1)

; Projectile

(projectile-global-mode)

; Path
(setenv "PATH" (concat "/usr/local/bin/:/usr/texbin" ":" (getenv "PATH")))

; Python mode stuff
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'auto-complete-mode)
(setq jedi:complete-on-dot t)                 ; optional

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
 '(org-agenda-files (quote ("~/Dropbox/Notes/Climate Dynamics Rejection.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
