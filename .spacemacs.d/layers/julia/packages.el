;;; packages.el --- julia layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Noah Brenowitz <noah@Noahs-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `julia-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `julia/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `julia/pre-init-PACKAGE' and/or
;;   `julia/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst julia-packages
  '(julia evil-matchit)
  "The list of Lisp packages required by the julia layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")
(defun julia/post-init-julia ())

(defun julia/post-init-evil-matchit ()

  ;;; Enable evil-matchit for all modes
  (global-evil-matchit-mode)

  ;;; Julia language
  (require 'evil-matchit-sdk)

  ;; ruby/bash/lua/vimrc
  (defvar evilmi-julia-match-tags
    '(
      (("function" "quote" "begin" "type" "immutable" "for" "while") () "end")
      ("if" ("elseif" "else") "end")
      ("try" ("catch" "finally") "end")
      ;; (("unless" "if") ("elif" "elsif" "elseif" "else") ( "end" "fi" "endif"))
      ;; ("begin" ("rescue" "ensure") "end")
      ;; ("case" ("when" "else") ("esac" "end"))
      ;; (("fun!" "function!" "class" "def" "while" "function" "do") () ("end" "endfun" "endfunction"))
      ;; ("repeat" ()  "until")
      )
    "The table we look up match tags. This is a three column table.
The first column contains the open tag(s).
The second column contains the middle tag(s).
The third column contains the closed tags(s).
The forth *optional* column defines the relationship between open and close tags. It could be MONOGAMY
")

;;;###autoload
  (defun evilmi-julia-get-tag ()
    (evilmi-sdk-get-tag evilmi-julia-match-tags
                        evilmi-sdk-extract-keyword-howtos))

;;;###autoload
  (defun evilmi-julia-jump (rlt num)
    (evilmi-sdk-jump rlt
                     num
                     evilmi-julia-match-tags
                     evilmi-sdk-extract-keyword-howtos))

  (provide 'evil-matchit-julia)

  (plist-put evilmi-plugins 'ess-julia-mode '((evilmi-julia-get-tag evilmi-julia-jump)))

  )
;;; packages.el ends here
