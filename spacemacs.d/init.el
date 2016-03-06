;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     ;; (git :variables
     ;;      git-gutter-use-fringe t)
     ;; markdown
     org
     html
     ;; shell
     syntax-checking
     python
     ess
     clojure
     go
     deft ;; notational velocity clone
     ;; neotree
     ;; For editing markdown files
     markdown
     ;; pandoc
     latex

     ;; My layers
     vimish-fold
     multiple-cursors
     )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(snakemake-mode
                                      org-plus-contrib
                                      org-ref
                                      yaml-mode
                                      ncl-mode
                                      cdlatex
                                      helm-bibtex
                                      ob-ipython
                                      diff-hl ; a better version of git-gutter
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported

  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         leuven
                         spacemacs-dark
                         spacemacs-light
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil)
  ;; User initialization goes here
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; Follow symlinks automatically
  (setq vc-follow-symlinks t)

  ;; Turn on auto fill mode
  (spacemacs/toggle-auto-fill-mode-on)

  (use-package deft
    :config
    (progn
      (setq deft-extension "org")
      (setq deft-text-mode 'org-mode)
      (setq deft-directory "~/Dropbox/notes")
      (setq deft-use-filename-as-title t)
      ))

  ;; Paredit bindings
  (sp-use-smartparens-bindings)
  ;; (define-key evil-normal-state-map (kbd "-") 'sp-backward-sexp)
  ;; (define-key evil-normal-state-map (kbd "=") 'sp-next-sexp)
  ;; (define-key evil-normal-state-map (kbd "_") 'sp-backward-up-sexp)
  ;; (define-key evil-normal-state-map (kbd "+") 'sp-down-sexp))


  ;; j and k go down visual lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)


  ;;; Hide show mode
  ;; (define-key evil-normal-state-map (kbd "<DEL>") 'evil-toggle-fold)


  ;;; Authoring tools

  ;; Bibliography management
  (use-package org-ref
    :config
    (progn
      (setq reftex-default-bibliography '("~/Dropbox/Papers/zotero.bib"))

      ;; see org-ref for use of these variables
      (setq org-ref-bibliography-notes "~/Dropbox/Papers/notes.org"
            org-ref-default-bibliography '("~/Dropbox/Papers/zotero.bib")
            org-ref-pdf-directory "~/Dropbox/Papers/bibtex-pdfs/")

      ;; need to setup helm-bibtex as well
      (setq helm-bibtex-bibliography "~/Dropbox/Papers/zotero.bib")

      ))

  ;; latex shortcuts
  (use-package cdlatex)

  ;;; org mode
  (use-package org-mode
    :config
    (progn
      (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
      (require 'ob-ipython)))

  ;; Remove evil mode for org-goto
  (defadvice org-goto (around make-it-evil activate)
    (let ((orig-state evil-state)
          (evil-emacs-state-modes (cons 'org-mode evil-emacs-state-modes)))
      ad-do-it
      (evil-change-state orig-state)))


  ;; Fortran
  (add-hook 'fortran-mode-hook 'flycheck-mode)

  ;; diff-hl settings

  (use-package diff-hl
    :config
    (progn
      (global-diff-hl-mode)
      (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))


  ;; Flycheck can be really pedantic with many stupid error codes. The following
  ;; configuration file goes in ~/.config/flake8 and disables many of the stupid
  ;; warnings.
  ;; 
  ;; [flake8]
  ;; ignore = E221,E501,E203,E202,E272,E251,E211,E222,E701,E303,E225,E226,E231
  ;; max-line-length = 160
  ;; exclude = tests/*
  ;; max-complexity = 10

  ;; my own functions
  (defun remove-blank-spaces ()
    ;; Remove annoying trailing spaces
    (interactive)
    (beginning-of-buffer)  ;; This adds mark at beginning of buffer
    (replace-regexp " +$" "")
    (pop-global-mark))

  (evil-leader/set-key "ors" 'remove-blank-spaces)
  (evil-leader/set-key "oc" 'customize-group)
  (evil-leader/set-key "oo" 'occur)

  (define-key global-map (kbd "C-.") 'company-files)


  ;; Window navivation
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-h C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; buffer navigation
  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-idle-delay 0.5)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(diff-hl-side (quote right))
 '(fortran-line-length 9999)
 '(global-diff-hl-mode t)
 '(helm-bibtex-bibliography (quote ("~/Dropbox/Papers/My Library.bib")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/cmt/results/notebook/notebook.org" "~/Dropbox/notes/Personal.org" "~/Dropbox/cmt/README.org" "~/Dropbox/notes/Admin.org" "~/Dropbox/notes/Ideas.org")))
 '(org-goto-auto-isearch nil)
 '(org-image-actual-width (quote (400)))
 '(python-shell-interpreter "python")
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((eval setq-local org-babel-default-header-args:Python
           (quote
            ((:exports . "both"))))
     (eval setq-local org-babel-default-header-args:Python
           (quote
            ((:export . "both"))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(flycheck-warning ((t (:underline (:color "Cyan" :style wave))))))