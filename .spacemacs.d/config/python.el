;; Fix indentation in python docstrings.
;; https://emacs.stackexchange.com/questions/26435/how-can-i-disable-indentation-rules-within-docstrings-in-python-mode
;; (defun my-python-noindent-docstring (&optional _previous)
;;   (if (eq (car (python-indent-context)) :inside-docstring)
;;       'noindent))

;; (advice-add 'python-indent-line :before-until #'my-python-noindent-docstring)
;; I actually found this kind of annoying
