
;; Should this be more general?
(defun highlight-tabs ()
  "Make tabs show up in the same color as trailing whitespace."
  (font-lock-add-keywords nil '(("\t" 0 'trailing-whitespace prepend))))


(require 'python) ; Because Aquamacs likes python-mode.el and I don't.
;; (require 'ipython) ; Uses python-mode.el :-(
(add-hook 'python-mode-hook 'highlight-tabs)
(add-hook 'python-mode-hook (lambda ()
                              (local-set-key "\C-m" 'newline-and-indent)))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-copy))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (prefs-binfile "pycheck") (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
