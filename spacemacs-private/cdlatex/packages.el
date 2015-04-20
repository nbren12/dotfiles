(defvar cdlatex-packages
  '(
    cdlatex
    ;; package cdlatexs go here
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cdlatex-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function cdlatex/init-<package-cdlatex>
;;
;; (defun cdlatex/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun cdlatex/init-cdlatex ()
  (use-package cdlatex
    :defer t
    :init
    (progn
      (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
      (add-hook 'LaTeX-mode-hook '(lambda()
                                    (yas-minor-mode -1)))
      (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
      (add-hook 'LaTeX-mode-hook 'smartparens-mode)
      (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
      (add-hook 'LaTeX-mode-hook '(lambda ()
                                    (electric-indent-mode -1)))
      (setq reftex-plug-into-AuCTeX t)
      (setq cdlatex-paired-parens "$[{(")

      (setq cdlatex-command-alist
            '(
              ("bi" "Insert \\binom{}{}" "\\binom{?}{}" cdlatex-position-cursor nil nil t)
              ("ggr(" "Insert \biggl( \biggr)" "\\biggl(? \\biggr" cdlatex-position-cursor nil nil t)
              ("ggr|" "Insert \biggl| \biggr|" "\\biggl|? \\biggr|" cdlatex-position-cursor nil nil t)
              ("ggr{" "Insert \biggl\{ \biggr\}" "\\biggl\\{? \\biggr\\" cdlatex-position-cursor nil nil t)
              ("ggr[" "Insert \biggl[ \biggr]" "\\biggl[? \\biggr" cdlatex-position-cursor nil nil t)
              ("ce" "Insert ceilings" "\\lceil? \\rceil" cdlatex-position-cursor nil nil t)
              ("fl" "Insert floors" "\\lfloor? \\rfloor" cdlatex-position-cursor nil nil t)
              ("ggrce" "Insert ceilings" "\\biggl\\lceil? \\biggr\\rceil" cdlatex-position-cursor nil nil t)
              ("int" "Insert integrals without limits" "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
              ("sum" "Insert sums without limits" "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
              ("prod" "Insert products without limits" "\\prod_{?}^{}" cdlatex-position-cursor nil nil t)
              ("prodl" "Insert products" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
              ("fl" "Insert floors" "\\biggl\\lfloor? \\biggr\\rfloor" cdlatex-position-cursor nil nil t)
              
              ("axm" "Insert axiom env" "" cdlatex-environment ("axiom") t nil)
              ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
              ("lem" "Insert lemma env" "" cdlatex-environment ("lemma") t nil)
              ("clm" "Insert claim env" "" cdlatex-environment ("claim") t nil)
              ("prop" "Insert proposition env" "" cdlatex-environment ("proposition") t nil)
              ("wts" "Insert want to show env" "" cdlatex-environment ("wts") t nil)
              ("def" "Insert definition env" "" cdlatex-environment ("definition") t nil)
              ))
      
      
      
      (setq cdlatex-math-modify-alist
            '(
              (?t "\\text" nil t nil nil)
              (?s "\\mathscr" nil t nil nil)
              ))
      
      
      (setq cdlatex-env-alist
            '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
              ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
              ("lemma" "\\begin{lemma}\nAUTOLABEL\n?\n\\end{lemma}\n" nil)
              ("claim" "\\begin{claim}\nAUTOLABEL\n?\n\\end{claim}\n" nil)
              ("proposition" "\\begin{proposition}\nAUTOLABEL\n?\n\\end{proposition}\n" nil)
              ("wts" "\\begin{wts}\nAUTOLABEL\n?\n\\end{wts}\n" nil)
              ("definition" "\\begin{definition}\nAUTOLABEL\n?\n\\end{definition}\n" nil)
              ))
      
      
      
      (add-hook 'LaTeX-mode-hook
                (lambda ()
                  (LaTeX-add-environments
                   '("axiom" LaTeX-env-label)
                   '("theorem" LaTeX-env-label)
                   '("lemma" LaTeX-env-label)
                   '("claim" LaTeX-env-label)
                   '("proposition" LaTeX-env-label)
                   '("wts" LaTeX-env-label)
                   '("definition" LaTeX-env-label)
                   )))
      
      (setq reftex-label-alist
            '(("axiom" ?a "ax:" "~\\ref{%s}" t ("axiom" "ax.") -2)
              ("theorem" ?h "thr:" "~\\ref{%s}" t ("theorem" "th.") -3)
              ("lemma" ?l "lem:" "~\\ref{%s}" t ("lemma" "le.") -3)
              ("claim" ?m "clm:" "~\\ref{%s}" t ("claim" "cl.") -3)
              ("proposition" ?p "prop:" "~\\ref{%s}" t ("proposition" "pr.") -3)
              ("wts" ?w "wts:" "~\\ref{%s}" t ("wts" "wt.") -3)
              ("definition" ?d "defn:" "~\\ref{%s}" t ("definition" "de.") -3)
              ))



      (defun my-add-section ()
        (interactive)
        (let ((LaTeX-section-hook '(LaTeX-section-title
                                    LaTeX-section-section
                                    LaTeX-section-label)))
          (LaTeX-section 2)))

      (defun my-add-subsection ()
        (interactive)
        (let ((LaTeX-section-hook '(LaTeX-section-title
                                    LaTeX-section-section
                                    LaTeX-section-label)))
          (LaTeX-section 3)))

      (defun my-add-subsubsection ()
        (interactive)
        (let ((LaTeX-section-hook '(LaTeX-section-title
                                    LaTeX-section-section
                                    LaTeX-section-label)))
          (LaTeX-section 4)))

      (defun change-to-align ()
        (interactive)
        (LaTeX-modify-environment "align"))

      (defun change-to-equation ()
        (interactive)
        (LaTeX-modify-environment "equation"))

      (defun reorder-labels-no-ask ()
        (interactive)
        (flet ((yes-or-no-p (&rest args) t)
               (y-or-n-p (&rest args) t))
          (reftex-renumber-simple-labels)))




      (defun my-insert-header ()
        (interactive)
        (insert "
{\\fontfamily{ccr} \\selectfont
\\begin{center}
\t{\\Large Problem Set}\\\\
\tKevin Li \\quad today
\\end{center}
}"))

      (add-hook 'LaTeX-mode-hook
                (lambda ()
                  (local-set-key (kbd "<f1>") 'my-add-section)
                  (local-set-key (kbd "M-1") 'my-add-section)
                  (local-set-key (kbd "<f2>") 'my-add-subsection)
                  (local-set-key (kbd "M-2") 'my-add-subsection)
                  (local-set-key (kbd "<f3>") 'my-add-subsubsection)
                  (local-set-key (kbd "M-3") 'my-add-subsubsection)

                  (local-set-key (kbd "<f5>") 'change-to-align)
                  (local-set-key (kbd "M-a") 'change-to-align)

                  (local-set-key (kbd "<f6>") 'change-to-equation)
                  (local-set-key (kbd "M-e") 'change-to-equation)


                  (local-set-key (kbd "<f10>") 'reorder-labels-no-ask)
                  (local-set-key (kbd "M-r") 'reorder-labels-no-ask)

                  (local-set-key (kbd "<f12>") 'my-insert-header)

                  ))
      
      )))
