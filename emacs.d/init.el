  ;;; init.el --- My init.el
  ;;; Commentary:
  ;;
  ;;; Code:
(require 'package)
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
		   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("d9a09bb02e2a1c54869dfd6a1412553fe5cb2d01a94ba25ef2be4634d1ca2c79" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
