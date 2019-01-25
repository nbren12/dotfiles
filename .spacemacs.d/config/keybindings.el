(evil-leader/set-key
  "ou" 'browse-url-at-point
  "os" 'remove-blank-spaces
  "orc" 'noah-add-to-config
  "ow" 'writeroom-mode
  ;; "oc" 'customize-group
  "oo" 'helm-occur
  "od" 'deft
  "oi" 'ibuffer
  "oc" 'craigslist-org
  "ofl" 'flycheck-list-errors
  "off" 'noah/open-fish-config
  "ofw" 'noah/open-uw
  "ob" 'noah/big-frame
  "ot" 'noah/insert-time-stamp
  )



;; j and k go down visual lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(global-set-key (kbd "M-s M-s") 'save-buffer)

;; auto-completion
(global-company-mode)
(define-key evil-emacs-state-map (kbd "C-.") 'company-files)
(define-key evil-insert-state-map (kbd "C-f") 'company-files)

;; Window navivation (conflicts with key bindings)
;; (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;; (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;; (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; buffer navigation
(global-set-key (kbd "C-x <C-left>") 'spacemacs/previous-useful-buffer)
(global-set-key (kbd "C-x <C-right>") 'spacemacs/next-useful-buffer)
;; kill ring forward binding
(global-set-key "\M-Y" 'evil-paste-pop-next)

;; Insert mode navigation
(define-key evil-insert-state-map (kbd "<M-backspace>") 'evil-delete-backward-word)
(define-key evil-insert-state-map (kbd "S-<left>") 'evil-backward-WORD-begin)
(define-key evil-insert-state-map (kbd "S-<right>") 'evil-forward-WORD-begin)

(define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

;; Paredit bindings
(dolist (lang-map '(emacs-lisp-mode-map))
  (progn
    (evil-define-key 'normal emacs-lisp-mode-map
      (sp-use-smartparens-bindings)
      "-" 'sp-backward-sexp
      ;; "=" 'sp-next-sexp
      "_" 'sp-backward-up-sexp
      "+" 'sp-down-sexp)))

;; cool key bindings for changing windows
(windmove-default-keybindings)

;; function key bindings
(global-set-key (kbd "<f12>") 'org-agenda-list)
(global-set-key (kbd "<f10>") 'org-todo-list)
(global-set-key (kbd "<f1>") 'magit)
