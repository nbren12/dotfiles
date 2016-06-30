;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; (default nil)
   dotspacemacs-enable-lazy-installation nil
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
     (auto-completion :variables
                                     auto-completion-return-key-behavior 'complete
                                     auto-completion-tab-key-behavior 'cycle
                                     auto-completion-complete-with-key-sequence "jk"
                                     auto-completion-complete-with-key-sequence-delay 0.3)
     ;; better-defaults
     emacs-lisp
     git
     ;; (git :variables
     ;;      git-gutter-use-fringe t)
     ;; markdown
     org
     ;; html
     ;; shell
     syntax-checking
     python
     ess
     clojure
     ;; go
     (c-c++
      :variables c-c++-enable-clang-support t
      )
     ;; ycmd
     deft ;; notational velocity clone
     ;; neotree
     ;; For editing markdown files
     markdown
     ;; pandoc
     (latex :variables latex-enable-folding t)
     bibtex
     dash ;; documentation browser (I spent $$$ on this)
     ;; My layers
     vimish-fold
     multiple-cursors
     shell
     bibtex
     )
   ;; List of additional packages that will be installed without being
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
                                      dash-at-point ; for dash browser
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported

  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         default
                         leuven
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 11
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; turn off whitespace
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; turn off line highlighting
  (global-hl-line-mode -1)

  ;; turn off line number
  (global-linum-mode -1)

  ;; Enable evil-matchit for all buffers
  (global-evil-matchit-mode)

  ;; Follow symlinks automatically
  (setq vc-follow-symlinks t)

  ;; Turn on auto fill mode
  (spacemacs/toggle-auto-fill-mode-on)

  ;; Change autosave interval
  (setq auto-save-timeout 30)

  ;; abbreviations
  (setq abbrev-file-name (concat dotspacemacs-directory "abbrev_def"))
  (read-abbrev-file abbrev-file-name)


  ;; YCMD
  ;; (setq ycmd-search-paths '("/Users/noah/.dotfiles/vim/plugged/YouCompleteMe/third_party/ycmd/ycmd"))
  ;; (let (( my-ycmd-path (-first 'file-exists-p ycmd-search-paths)))
  ;;       (message (concat "<noah> Setting my ycmd path to " my-ycmd-path))
  ;;       (set-variable 'ycmd-server-command '("python2" my-ycmd-path)))


  (use-package deft
    :config
    (progn
      (setq deft-extension "org")
      (setq deft-text-mode 'org-mode)
      (setq deft-directory "~/Dropbox/notes")
      (setq deft-use-filename-as-title t)))

  ;; Julia
  (setq inferior-julia-program-name "/Applications/Julia-0.4.0.app/Contents/Resources/julia/bin/julia")

  ;; Paredit bindings
  (dolist (lang-map '(emacs-lisp-mode-map))
    (progn
      (sp-use-smartparens-bindings)
      (evil-define-key 'normal emacs-lisp-mode-map
        "-" 'sp-backward-sexp
        ;; "=" 'sp-next-sexp
        "_" 'sp-backward-up-sexp
        "+" 'sp-down-sexp)))


  ;; j and k go down visual lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)


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
      (setq helm-bibtex-bibliography "~/Dropbox/Papers/zotero.bib")))

  ;; latex shortcuts
  (use-package cdlatex)

  (defun my-org-config ()
    ;; org mode
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

    (setq org-capture-templates
          '(("m" "Personal todo" entry (file+headline "~/Dropbox/notes/Personal.org" "Inbox")
             "* TODO %?\nEntered on %U\n  %i\n  %a")
            ("w" "Work todo" entry (file+headline "~/Dropbox/notes/Admin.org" "Inbox")
             "* TODO %?\nEntered on %U\n  %i\n  %a")
            ("h" "Howto" entry (file+headline "~/Dropbox/notes/Howto.org" "Inbox")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("i" "Idea" entry (file+headline "~/Dropbox/notes/Ideas.org" "Ideas")
             "* %?\nEntered on %U\n  %i\n  %a")))
    (require 'ob-dot)
    (require 'ob-ipython))

  (my-org-config)

  ;; Remove evil mode for org-goto
  (defadvice org-goto (around make-it-evil activate)
    (let ((orig-state evil-state)
          (evil-emacs-state-modes (cons 'org-mode evil-emacs-state-modes)))
      ad-do-it
      (evil-change-state orig-state)))

  ;; Fortran
  (add-to-list 'auto-mode-alist '("\\.F\\'" . f90-mode))

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


  ;; auto-completion
  (define-key global-map (kbd "C-.") 'company-files)
  (evil-define-key 'insert prog-mod-map (kbd "<C-space>") 'company-complete)


  ;; Window navivation
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-h C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; buffer navigation
  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
  (global-set-key (kbd "C-x <C-left>") 'spacemacs/previous-useful-buffer)
  (global-set-key (kbd "C-x <C-right>") 'spacemacs/next-useful-buffer))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/journal/2016-06-09/questions.org" "~/Dropbox/notes/Admin.org" "~/Dropbox/notes/EAC/EAC.org" "~/Dropbox/notes/CRM.org")))
 '(org-directory "~/Dropbox/notes")
 '(org-ref-pdf-directory "~/Dropbox/Papers/bibtex-pdfs/")
 '(projectile-globally-ignored-file-suffixes (quote (".build" ".o" ".png" ".pdf" ".mod" ".bin" ".nc")))
 '(projectile-globally-ignored-files (quote ("TAGS" "*CMakeFiles/*")))
 '(safe-local-variable-values (quote ((TeX-command-extra-options . "-shell-escape"))))
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
