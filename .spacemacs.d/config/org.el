;; Bibliography management
(use-package org-ref
  :config
  (progn
    (setq reftex-default-bibliography '("~/Dropbox/Papers/zotero.bib"))

    ;; see org-ref for use of these variables
    (setq org-ref-bibliography-notes "~/Dropbox/Papers/notes.org"
          org-ref-default-bibliography '("~/Dropbox/Papers/zotero.bib")
          org-ref-pdf-directory "~/Dropbox/Documents/org-ref/bibtex-pdfs/")

    ;; need to setup helm-bibtex as well
    (setq helm-bibtex-bibliography "~/Dropbox/Papers/zotero.bib")))

;; latex shortcuts
;; (use-package cdlatex)
(defun org-archive-done-tasks ()
  ;; Taken from https://stackoverflow.com/a/27043756/1208392
  (interactive)
  (org-map-entries
    (lambda ()
      (org-archive-subtree)
      (setq org-map-continue-from (outline-previous-heading)))
    "/DONE" 'tree))

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq org-capture-templates
      '(("m" "Personal todo" entry (file+headline "~/Dropbox/notes/Personal.org" "Inbox")
         "* TODO %?\nEntered on %U\n  %i\n  %a")
        ("w" "Work todo" entry (file+headline "~/Dropbox/notes/Admin.org" "Inbox")
         "* TODO %?\nEntered on %U\n  %i\n  %a")
        ("j" "Job" entry (file+headline "~/Dropbox/JobSearch2018/readme.org" "Opportunities")
         "* TODO %?\nEntered on %U\n  %i\n  %a")
        ("h" "Howto" entry (file+headline "~/Dropbox/notes/Howto.org" "Inbox")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Skiing" entry (file+olp "~/Dropbox/notes/Personal.org" "Skiing" "Log")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("i" "Idea" entry (file+headline "~/Dropbox/notes/Ideas.org" "Ideas")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("c" "Meeting" entry (file+headline "~/Dropbox/notes/Admin.org" "Meetings")
         "* %?\n\n")))

(require 'ox-md) ;; needed for org markdown export
;; (require 'ob-ipython)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
    (dot . t)
    (shell . t)
    ;; (R . t)
    (gnuplot . t)
    (clojure . t)
    ;;    (graphviz . t)
    (lisp . t)
    ;;    (stan . t)
    (org . t)
    (screen . t)
    (calc . t)
    (js . t)
    (latex . t)
    (plantuml . t)
    (ruby . t)
    (shell . t)
    ;; (ipython . t)
    (python . t)
    (emacs-lisp . t)
    (ditaa . t)
    (awk . t)
    (octave . t)
    (sed . t)
    (sql . t)
    (sqlite . t)))

 (evil-define-key 'normal org-mode-map "t" 'org-todo)
 (spacemacs/set-leader-keys-for-major-mode 'org-mode
   "er" 'org-reveal-export-current-subtree
   "eR" 'org-reveal-export-to-html)


;; Remove evil mode for org-goto
(defadvice org-goto (around make-it-evil activate)
  (let ((orig-state evil-state)
        (evil-emacs-state-modes (cons 'org-mode evil-emacs-state-modes)))
    ad-do-it
    (evil-change-state orig-state)))

;; checkboxes in html export
;; see https://stackoverflow.com/questions/22065589/org-mode-html-export-with-checkbox
(setq org-html-checkbox-type 'html)
