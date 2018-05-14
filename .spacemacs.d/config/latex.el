;; Set default font faces for Info and ERC modes
;; (add-hook 'org-mode-hook 'my-buffer-face-mode-variable)

;; (add-hook 'LaTeX-mode-hook 'turn-off-smartparens-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)
  ;;; Latex settings

;; (add-hook 'LaTeX-mode-hook 'my-buffer-face-mode-variable)
;; (add-hook 'Info-mode-hook 'my-buffer-face-mode-variable)

  ;;; Sentence based filling in Latex modes
;; http://stackoverflow.com/questions/539984/how-do-i-get-emacs-to-fill-sentences-but-not-paragraphs
(defun auto-fill-by-sentences ()
  (if (looking-back (sentence-end))
      ;; Break at a sentence
      (progn
        (LaTeX-newline)
        t)
    ;; Fall back to the default
    (do-auto-fill)))
(add-hook 'LaTeX-mode-hook (lambda () (setq auto-fill-function 'auto-fill-by-sentences)))

;; Modified from http://pleasefindattached.blogspot.com/2011/12/emacsauctex-sentence-fill-greatly.html
(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker---use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or (looking-back "^\\s *")
                  (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))
(ad-activate 'LaTeX-fill-region-as-paragraph)

;; Electric period key
;; (defun period-and-newline ()
;;   (interactive)
;;   (insert-string ".")
;;   (newline-and-indent))

;; (evil-define-key 'insert LaTeX-mode-map (kbd ".") 'period-and-newline)

;; evil-motions for selecting the current environment

(defun kill-beamer-frame ()
  (interactive)
  (re-search-backward "\\\\begin{frame}")
  (push-mark)
  (search-forward-regexp "\\\\end{frame}")
  (evil-visual-select (mark) (point)))

(evil-define-text-object evil-a-latex-frame (count &optional beg end type)
  "Select inner angle bracket."
  :extend-selection t
  (kill-beamer-frame))

(defun latex-narrow-to-env ()
  ;; Narrow to current latex environment
  (interactive)
  (LaTeX-mark-environment)
  (narrow-to-region (region-beginning) (region-end))
  (deactivate-mark))

(defun latex-read-inputs ()
  ;; Read all \input files in a latex file and comment out \input command
  (interactive)
  (push-mark)
  (goto-char 0)
  (while (re-search-forward "^ *\\\\input{\\(.*?\\)}" nil t)
    (let ((input-file (match-string 1)))
      (message (concat "Reading file " input-file " into current buffer and commenting input command"))
      (forward-line)
      (comment-line -1)
      (evil-read nil input-file)))
  (pop-mark))

(define-key evil-outer-text-objects-map "f" 'evil-a-latex-frame)
(spacemacs/set-leader-keys-for-major-mode 'latex-mode
  "ne" 'narrow-to-env)

;; Ask for latex master file
(setq-default TeX-master nil)
