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
