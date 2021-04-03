;;; init.el --- My init.el
;;; Commentary:

;; Nothing much to say here. This file is best read using the [[https://github.com/tj64/outshine][outshine]]
;; package for outline-minor-mode.

;; Convection for key bindings:

;; |--------+---------------------|
;; | Prefix | Meaning             |
;; |--------+---------------------|
;; | r      | Regex and searching |
;; | b      | buffers             |
;; | f      | file searching      |
;; | o      | other               |
;; |--------+---------------------|

;;; Code:
;;;; Initial Stuff
;;;;; Package Manager Initialize
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/") t)

(package-initialize)

;;;;; Bootstrap packages

(setq bootstrap-packages '(use-package org))


(dolist (pkg bootstrap-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

(require 'use-package)

;; This needs to be done before outline-mode is loaded
(defvar outline-minor-mode-prefix "\M-#")

;;;; Functions

(defun hbin-remove-mm-lighter (mm)
  "Remove minor lighter from the mode line."
  (setcar (cdr (assq mm minor-mode-alist)) nil))
(setq config-list (list))



(defun open-or-switch-to-ansi-term ()
  ;; Function for opening/switching to a terminal
  (interactive)
  (let ((buf (get-buffer "*ansi-term*")))

    (if buf
        (switch-to-buffer buf)
      (ansi-term "/bin/zsh")
      )))
(defun cimsp ()
  (string-match "cims.nyu.edu$" system-name))

(defun guarded-install (seq)
  ;; Install elements of seq if not already installed
  (dolist (prog seq)
    (unless (package-installed-p prog)
      (package-install prog))))

(defun install-from-git (repo path &optional dep)
  (interactive "P")
  ;; Install the repository to the local path

  ;; Install 
  (unless (file-exists-p path)
    (shell-command
     (concat "git clone " repo " " path)))
  (add-to-list 'load-path path)

  (if dep (guarded-install dep)))
  

;;;; Evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (progn

    (evil-mode '1)
					;    (global-evil-surround-mode 1)      
    (add-hook 'prog-mode-hook 'hs-minor-mode)
    ;; (add-hook 'prog-mode-hook 'linum-mode)
    (setq-default evil-symbol-word-search 'symbol)
    
    (setq evil-emacs-state-modes 
          (append evil-emacs-state-modes 
                  '(view-mode TeX-output-mode view-mode
			      customize-mode dired-mode
			      special-mode)))
    
					; Browse yank ring

    (define-key evil-normal-state-map (kbd "") 'evil-toggle-fold)

    ;; Make movement keys work like they should
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
					; Make horizontal movement cross lines                                    
    (setq-default evil-cross-lines t)
    ))

(use-package evil-leader
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode)
    (evil-leader/set-key 
      "hb" 'helm-bookmarks
      "ro" 'helm-occur
      "rr" 'rgrep
      "rg" 'helm-git-grep
      "u" 'universal-argument
      "ta" 'align-regexp
      "cc" 'compile
      "cr" 'recompile
      "ff" 'helm-find-files
      "fr" 'helm-recentf
      "bb" 'helm-mini
      "dd" 'deft
      "op" 'org-preview-latex-fragment
      "ss"  'speedbar-get-focus
      "." 'eshell
      "gs" 'magit-status)))

    
(use-package evil-nerd-commenter
  :config
  (progn
    (evil-leader/set-key ";" 'evilnc-comment-operator)))


(use-package evil-matchit
  :config (progn
            (global-evil-matchit-mode 1)))

;;;; Autocompletion

;;;;; yasnippet
    
;; This junk is necesary to prevent autoloading on startup. See
;; [[http://emacs.stackexchange.com/questions/3439/stop-yasnippet-from-autoloading][stackoverflow.com]].
(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode) :defer t
  :config
  (progn 
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

    ;; Personal snippets
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/mysnippets")
    (yas-global-mode)
    ))

;;;;; company mode
;;    autocomplete mode is an alternative
(use-package company
  :config
  (progn
    (global-company-mode)
    (global-set-key (kbd "<C-tab>") 'company-complete)
    (hbin-remove-mm-lighter 'company-mode)
    (define-key evil-insert-state-map (kbd "<C-SPC>") 'completion-at-point)))


;; Insert file name at point
;; This code comes from the [[http://www.emacswiki.org/emacs/InsertFileName][emacs wiki]].

 (defun my-insert-file-name (filename &optional args)
    "Insert name of file FILENAME into buffer after point.
  
  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.
  
  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.
  
  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
    ;; Based on insert-file in Emacs -- ashawley 20080926
    (interactive "*fInsert file name: \nP")
    (cond ((eq '- args)
           (insert (file-relative-name filename)))
          ((not (null args))
           (insert (expand-file-name filename)))
          (t
           (insert filename))))

(global-set-key (kbd "C-.") 'my-insert-file-name)

;;;;; Tags browsing


(use-package ggtags
  :config
  (progn
    (defun fix-keybindings ()
      (define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim))
    (add-hook 'ggtags-mode-hook 'fix-keybindings)
    ))

;;;; Helm and friends
;;;;; Helm

(use-package helm
  :config
  (progn

    (require 'helm)
    (require 'helm-config)

;; Some keybindings
    (evil-leader/set-key "bs" 'helm-mini)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (evil-leader/set-key "h" 'helm-command-prefix)
    (helm-mode 1)
    (evil-leader/set-key "hb" 'helm-bookmarks)
    (hbin-remove-mm-lighter 'helm-mode)
    ))

;; in helm-find-files enter directory with enter
(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))


(use-package imenu
  :config
  (progn
    (evil-leader/set-key "hi" 'helm-imenu)))

(use-package projectile
  :config
  (progn
    (projectile-global-mode 1)
    (hbin-remove-mm-lighter 'projectile-mode)
    ))


(use-package helm-projectile
  :config
  (progn
    (evil-leader/set-key
      "pf" 'helm-projectile
      "pg" 'helm-projectile-grep
      "pa" 'helm-projectile-ack
      "pp" 'helm-projectile-switch-project)
    ))

;;;;; Recentf
(use-package recentf)

;;;; Modes
;;;;; Magit (git)

(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

;;;;; Matlab (CIMS only)

(defun config-cims ()
  (add-to-list 'load-path "~/.emacs.d/matlab-emacs")
  (require 'matlab-load))

;; (when (string-match "cims.nyu.edu$" system-name) (config-cims))

;;;;; Python

(use-package snakemake-mode)

(use-package anaconda-mode
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)

    (evil-leader/set-key-for-mode 'python-mode
      "md" 'anaconda-mode-goto
      "mh" 'anaconda-mode-view-doc)
    ))

(use-package company-anaconda
  :config
  (progn
    (add-to-list 'company-backends 'company-anaconda)))
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn 
;;     (elpy-enable)
;;     (elpy-use-ipython)
;;     (setq elpy-rpc-backend "jedi")
;;     (evil-leader/set-key-for-mode 'python-mode "md" 'elpy-goto-definition)))


;; Insert breakpoint with leader binding
(defun python-break-point ()
  (interactive)
  (insert "from IPython import embed; embed()"))
        
(evil-leader/set-key-for-mode 'python-mode "bp" 'python-break-point)


(use-package python-cell
  :config
  (progn
    (add-hook 'python-mode-hook 'python-cell-mode)
    (hbin-remove-mm-lighter 'python-cell-mode)))



;;;;; C/C++
    
(require 'cc-mode)
(require 'semantic)


(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(use-package c-eldoc
  :config
  (progn
    (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)))

(use-package function-args
  :config
  (progn
    (fa-config-default)))

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(use-package  irony
  :defer t
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
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))

(use-package company-irony
  :defer t
  :config
  (progn
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))

    ;; (optional) adds CC special commands to `company-begin-commands' in order to
    ;; trigger completion at interesting places, such as after scope operator
    ;;     std::|
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    ))

;;;;; LaTeX

(setq org-latex-pdf-process (quote  ( "latexmk -pdf %f" )))

;; To enable synctex just make a latexmkrc file that contains:

;; $ cat ~/.latexmkrc
;; $pdflatex='pdflatex -line-error  -synctex=1'


(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

                                        ; Remove superfluous mode line indicators


;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;; .latexmkrc:
;; $pdflatex = 'pdflatex -interaction=nonstopmode -synctex=1 %O %S';
;; $pdf_previewer = 'open -a skim';
;; $clean_ext = 'bbl rel %R-blx.bib %R.synctex.gz';

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

;;;;;; Okular (cims)
(when (cimsp)

  (setq TeX-view-program-list '(("Okular" "okular --unique %u")))

  (add-hook 'LaTeX-mode-hook '(lambda ()
				(add-to-list 'TeX-expand-list
					     '("%u" Okular-make-url))))

  (defun Okular-make-url () (concat
			     "file://"
			     (expand-file-name (funcall file (TeX-output-extension) t)
					       (file-name-directory (TeX-master-file)))
			     "#src:"
			     (TeX-current-line)
			     (expand-file-name (TeX-master-directory))
			     "./"
			     (TeX-current-file-name-master-relative)))

  (setq TeX-view-program-selection '((output-pdf "Okular"))))

;;;;;; Use this function to fill lines on sentence breaks.
(defun fill-sentence ()
  (interactive)
  (save-excursion
    (or (eq (point) (point-max)) (forward-char))
    (forward-sentence -1)
    (indent-relative t)
    (let ((beg (point))
          (ix (string-match "LaTeX" mode-name)))
      (forward-sentence)
      (if (and ix (equal "LaTeX" (substring mode-name ix)))
          (LaTeX-fill-region-as-paragraph beg (point))
        (fill-region-as-paragraph beg (point))))))

;; Key binding for the above function
(global-set-key (kbd "M-j") 'fill-sentence)



;;;;; Writeroom mode

(use-package writeroom-mode
  :config
  (progn
    (evil-leader/set-key "ow" 'writeroom-mode)))



;;;;; Outshine Mode

(use-package outshine
  :defer t
  :commands (outshine-hook-function)
  :preface
  (progn
    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function))
  :init 
  :config
  (progn 
    (require 'outshine)
    ; Fix company completion in outshine buffers
    (add-to-list 'company-begin-commands 'outshine-self-insert-command)

    (evil-leader/set-key "on" 'outshine-navi)
    (add-to-list 'evil-emacs-state-modes 'navi-mode)
    ))

;;;;; Org
(setq org-use-speed-commands t)
(use-package org
  :init
  :config
  (progn
    ;; (require 'org-special-blocks)
    
    ; Use latexmk for latex
    (setq org-latex-pdf-process (list "latexmk -f -pdf %f"))

    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
    (add-hook 'org-mode-hook 'auto-fill-mode)

    (setq org-src-fontify-natively t)    ;; Pretty formatting
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (shell . t)
       (R . t))))
  :bind ("C-c a" . org-agenda))


;; Ipython integration with org
(use-package ob-ipython)

(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;;;;; Deft


(use-package deft
  :config
  (progn
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-directory "~/Dropbox/notes")
    (setq deft-use-filename-as-title t)
    (add-hook 'deft-mode-hook 'evil-emacs-state)))

;;;;; Markdown

(use-package markdown-mode
  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-reftex)
    ))

(use-package pandoc-mode
  :config
  (progn
    (add-hook 'markdown-mode-hook 'pandoc-mode)))

(defun sjoin (list sep)
  ; Join list of strings with the string in sep
  (let ((a (car list)))
    (mapc (lambda (x)
	    (setq a (concat a sep x)))
	  (cdr list))
    a))

;; Pandoc reftex
(defun pandoc-reftex ()
  ; Format pandoc citation using reftex
  (sjoin (mapcar (lambda (x) (concat "@" x))
		 (reftex-citation 1))
	 "; "))


;;;;; AceJump

(use-package ace-jump-mode
  :config
  (progn
    ;; AceJump bindings
    (evil-leader/set-key "jj" 'evil-ace-jump-line-mode)
    (evil-leader/set-key "jk" 'evil-ace-jump-word-mode)
    ))

;;;; Themes

(use-package solarized-theme)

;;;; Reference Software 
;;;;; Helm-bibtex

(use-package helm-bibtex
  :config
  (progn
    (setq helm-bibtex-bibliography '("~/Dropbox/Papers/references.bib"))))

;;;;; Bindings


(defun my-outline-cycle ()
  ;; Cycle outline-mode or org-mode
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-cycle)
    (outline-cycle)))

;; Use tab for outline-cycling
(define-key evil-normal-state-map (kbd "<tab>") 'my-outline-cycle)

;; Tab binding necessary for terminal sessions
(define-key evil-normal-state-map (kbd "TAB") 'my-outline-cycle)

(evil-leader/set-key 
  "ot" 'open-or-switch-to-ansi-term
  "om" 'man)

(evil-leader/set-key
  "bd" 'kill-buffer
  "<right>" 'next-buffer
  "<left>" 'previous-buffer
  "<down>" 'other-window)



;;;;; Find Settings file
(defun find-settings-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(evil-leader/set-key
  "fs" 'find-settings-file)


;;;; Execute all "config" function
(mapcar 'funcall  config-list)
(setq settings-loaded t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
