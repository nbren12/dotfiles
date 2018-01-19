;; Custom functions I have defined that don't belong anywhere else

;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Times New Roman" :height 150))
  (buffer-face-mode))

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Monaco" :height 120))
  (buffer-face-mode))

(defun remove-blank-spaces ()
  ;; Remove annoying trailing spaces
  (interactive)
  (beginning-of-buffer)  ;; This adds mark at beginning of buffer
  (replace-regexp " +$" "")
  (pop-global-mark))

(defun noah-add-to-config ()
  ;; Add current file to config using the con alias
  (interactive)
  (shell-command
   (concat "con add " (buffer-file-name)))
  (message "Adding current file to dotfiles"))

(defun craigslist-org ()
  ;; pull info from craigslist page into org-mode header
  (interactive)
  (let ((url (read-string "Enter Craigslist URL: ")))
    (org-insert-heading-respect-content)
    (insert
     (shell-command-to-string (concat "craigslist2org.py -n 0 " url)))))

;; open fish config file
(defun noah/open-fish-config ()
  (interactive)
  (find-file "~/.config/fish/config.fish"))
