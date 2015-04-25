
;; Should this be more general?
(defun highlight-tabs ()
  "Make tabs show up in the same color as trailing whitespace."
  (font-lock-add-keywords nil '(("\t" 0 'trailing-whitespace prepend))))

(require 'python) ; Because Aquamacs likes python-mode.el and I don't.
(add-hook 'python-mode-hook 'highlight-tabs)
(add-hook 'python-mode-hook (lambda ()
                              (local-set-key "\C-m" 'newline-and-indent)))


;; (add-to-list 'load-path (get-full-path-for-subdir "third-party/python-docstring-mode"))
;; (require 'python-docstring)
;; (add-hook 'python-mode-hook 'python-docstring-mode)
