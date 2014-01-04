; -*-Lisp-*-
(add-to-list 'load-path "~/.emacs.d/")
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
			; ("gnu" . "http://elpa.gnu.org/packages/")
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



(require 'org-install)
(require 'evil)
(require 'ess-site)
(require 'auto-complete)
(require 'deft)

;;;; Org Mode
(add-to-list 'load-path "~/.emacs.d/org-8.2.4/lisp")


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

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
(add-hook 'org-mode-hook 'org-cdlatex-mode)

;;; EVIL Vim Mode

(evil-mode 1)

;; Custom Evil Mode Key Bindings

(define-key evil-normal-state-map ",l" 'org-preview-latex-fragment)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(global-evil-leader-mode)

;;; Latex Stuff


;; to fix problems with amsmath conflicting with wasysym:
;;
;; From: Lawrence Mitchell <wence <at> gmx.li>
;; Subject: [Orgmode] Re: [bug] latex export ignores org-export-latex-default-packages-alist?
;; To: emacs-orgmode <at> gnu.org
;; Date: Wed, 26 Jan 2011 16:01:52 +0000
(setq org-latex-to-pdf-process (list "latexmk -bibtex -f -pdf %s"))

;;; RefTex Hooks
(add-hook 'org-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(setq latex-run-command "pdflatex")




;; Ipython
(setq-default py-shell-name "/Library/Frameworks/EPD64.framework/Versions/Current/bin/ipython")
(setq-default py-which-bufname "IPython")
(setq py-python-command-args
  '("--pylab"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)
(setq ein:use-auto-complete-superpack t)


;; Auto Complete
(setq ac-auto-start 3)
(setq ac-dwim t)
;(global-auto-complete-mode t)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)    

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)


(c-mode)


;; Deft

(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Dropbox/org")



;; Keybindings

(evil-leader/set-key "d" 'deft)
