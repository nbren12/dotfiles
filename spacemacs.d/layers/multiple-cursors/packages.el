;;; packages.el --- multiple-cursors layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Noah Brenowitz <noah@Noahs-MBP>
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
;; added to `multiple-cursors-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `multiple-cursors/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `multiple-cursors/pre-init-PACKAGE' and/or
;;   `multiple-cursors/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst multiple-cursors-packages
  '(evil-mc)
  "The list of Lisp packages required by the multiple-cursors layer.

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

(defun multiple-cursors/post-init-evil-mc ()

  (use-package evil-mc
    :config
    (progn
      (global-evil-mc-mode 1)

      (defun evil-mc-add-cursor-on-click (event)
        "Add a cursor where you click. Borrowed from multiple-cursors.el"
        (interactive "e")
        (mouse-minibuffer-check event)
        ;; Use event-end in case called from mouse-drag-region.
        ;; If EVENT is a click, event-end and event-start give same value.
        (let ((position (event-end event)))
          (if (not (windowp (posn-window position)))
              (error "Position not in text area of window"))
          (select-window (posn-window position))
          (if (numberp (posn-point position))
              (save-excursion
                (goto-char (posn-point position))
                (evil-mc-make-cursor-here)))))
                                        ; (mc/maybe-multiple-cursors-mode)

      ;; Add bindings for adding cursor on next and previous lines
      (defun evil-mc-add-cursor-next-line ()
        (interactive)
        (evil-mc-make-cursor-here)
        (next-line))

      (defun evil-mc-add-cursor-previous-line ()
        (interactive)
        (evil-mc-make-cursor-here)
        (previous-line))

      (define-key evil-normal-state-map (kbd "M-j")
                'evil-mc-add-cursor-next-line)

      (define-key evil-normal-state-map (kbd "M-k")
                'evil-mc-add-cursor-previous-line)

      ;; Bind esc to remove cursors
      (advice-add 'evil-force-normal-state :after
                  (lambda ()
                    (interactive)
                    (message "Removing multiple cursors")
                    (evil-mc-undo-all-cursors)))

      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd  "M-<mouse-1>") 'evil-mc-add-cursor-on-click))))
;;; packages.el ends here
