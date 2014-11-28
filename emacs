
; Using Quelpa to manage packages
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(mapc 'quelpa '(evil evil-org evil-surround org reftex auctex
      auto-complete yasnippet deft projectile ace-jump-mode color-theme))

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
(define-key evil-normal-state-map ";" 'execute-extended-command)
; (define-key evil-insert-state-map "jj" 'evil-normal-state)
(define-key evil-normal-state-map "`d" 'deft)
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1)

; Projectile

(projectile-global-mode)

; Path
(setenv "PATH" (concat "/usr/local/bin/:/usr/texbin" ":" (getenv "PATH")))

; Deft settings
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Dropbox/Notes")
(setq deft-use-filename-as-title t)
