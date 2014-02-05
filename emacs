; -*-Lisp-*-
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/org-8.2.4/lisp")

(load "package.el")

(setq noah-packages '(
	evil 
	evil-leader
	deft
	python-mode
	jedi
	auctex
	ess
	auto-complete  
	websocket
	request
	smartrep 
	popup 
	fuzzy 
	ein 
))


(setq package-archives '(
			 ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       ;(if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package)))
 noah-packages)



;; Requires

(require 'org-install)
(require 'evil)
(require 'ess-site)
(require 'auto-complete)
(require 'deft)

;;;; Org Mode


; Some initial langauges we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))


(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)

;;; EVIL Vim Mode

(evil-mode 1)

;; Custom Evil Mode Key Bindings

(global-evil-leader-mode)

;;; Latex Stuff
;; to fix problems with amsmath conflicting with wasysym:
(setq org-latex-to-pdf-process (list "latexmk -bibtex -f -pdf %s"))
(setq latex-run-command "pdflatex")




;; Ipython
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




(setq ein:use-auto-complete-superpack t)



;; Auto Complete
(setq ac-auto-start 3)
(setq ac-dwim t)
;(global-auto-complete-mode t)

(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)




(c-mode)


;; Deft

(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Dropbox/org")
(setq deft-use-filename-as-title t)

;; Appearance
(tool-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'wombat t)

(setq evil-default-cursor t)
(set-cursor-color "White")

;; Hooks
(add-hook 'org-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'org-mode-hook 'org-cdlatex-mode)
(add-hook 'python-mode-hook 'jedi:setup)



; Keybindings (list of evil-mode [[https://github.com/mbriggs/.emacs.d/blob/master/my-keymaps.el]])

(define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
(evil-leader/set-key "eb" 'eval-buffer)
(evil-leader/set-key "nb" 'ein:notebooklist-open)

;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Deft
(evil-leader/set-key "d" 'deft "l" 'org-preview-latex-fragment)


(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)

;; IPython Notebook
(define-key evil-normal-state-map (kbd "C-n") 'ein:worksheet-goto-next-input)
(define-key evil-normal-state-map (kbd "C-p") 'ein:worksheet-goto-prev-input)
;; Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Autocomplete
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

; Misc
(setq vc-follow-symlinks t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "d070fa185078bf753dcfd873ec63be19fa36a55a0c97dc66848a6d20c5fffdad" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
