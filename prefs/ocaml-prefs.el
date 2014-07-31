
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.atd$" . tuareg-mode))
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)

;; Use ocp-indent for indentation
(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; (load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))

(require 'ocp-indent)
(require 'merlin)

(define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)

(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
;; (setq merlin-error-after-save nil)

(require 'auto-complete)
(setq ac-auto-start nil)
;; (setq ac-auto-show-menu 0.8)
;; (define-key ac-completing-map "\r" nil)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
