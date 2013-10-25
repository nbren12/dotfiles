(add-to-list 'load-path "~/.emacs.d/")
(load "package.el")

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 '(evil auctex undo-tree org ess ess-R-data-view ess-R-object-popup auto-complete))

;;;; Org Mode
(require 'org-install)

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



;;; EVIL Vim Mode
(require 'evil)
(evil-mode 1)


;;; Auto Complete
;; Add ac-source-dictionary to ac-sources of all buffer
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-filename))))


;;; Latex Stuff


;; to fix problems with amsmath conflicting with wasysym:
;;
;; From: Lawrence Mitchell <wence <at> gmx.li>
;; Subject: [Orgmode] Re: [bug] latex export ignores org-export-latex-default-packages-alist?
;; To: emacs-orgmode <at> gnu.org
;; Date: Wed, 26 Jan 2011 16:01:52 +0000
(add-to-list 'org-latex-packages-alist '("" "amsmath" t))
;;(setcar (rassoc '("wasysym" t) org-export-latex-default-packages-alist)	"integrals")



;;; RefTex Hooks
(add-hook 'org-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

(setq org-latex-create-formula-image-program 'dvipng)


;; Custom Evil Mode Key Bindings

(define-key evil-normal-state-map ",l" 'org-preview-latex-fragment)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)