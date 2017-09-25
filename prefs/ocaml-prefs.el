(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.atd$" . tuareg-mode))

;; We only want to do this work if we're actually using ocaml.
(eval-after-load "tuareg"
  '(progn
     ;; Undefine this function to stop `<<' triggering camlp4 syntax stuff.
     (defun tuareg-syntax-propertize (start end))

     (setq opam-share (substring (shell-command-to-string
                                  "opam config var share") 0 -1))
     (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
     (load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))

     (require 'ocp-indent)
     (require 'merlin)

     (define-key merlin-mode-map
       (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
     (define-key merlin-mode-map
       (kbd "C-c <down>") 'merlin-type-enclosing-go-down)

     (add-hook 'tuareg-mode-hook 'merlin-mode)
     (add-hook 'tuareg-mode-hook 'ocp-setup-indent)
     (setq merlin-use-auto-complete-mode 'easy)
     ;; Use opam switch to lookup ocamlmerlin binary
     (setq merlin-command 'opam)
     ;; (setq merlin-error-after-save nil)

     (require 'auto-complete)
     (setq ac-auto-start nil)
     (setq ac-candidate-menu-min 0)
     (setq ac-disable-inline t)
     ;; (setq ac-auto-show-menu 0.8)
     ;; (define-key ac-completing-map "\r" nil)
     (add-hook 'tuareg-mode-hook 'auto-complete-mode)
     ))
