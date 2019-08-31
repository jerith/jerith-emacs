;;; init.el --- Where all the magic begins

;;; Commentary:
;;
;; This is a thin wrapper around emacs-init.el which is generated from
;; emacs-init.org.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar my-init-file (expand-file-name "emacs-init.el" user-emacs-directory)
  "All configurations stored in this file.")

(defvar my-org-file (expand-file-name "emacs-init.org" user-emacs-directory)
  "All configurations tangled from this file.")

(defvar my-elisp-dir (expand-file-name "elisp/" user-emacs-directory)
  "Random elisp that isn't in ELPA or wherever.")

;; (org-babel-load-file
;;  (expand-file-name "emacs-init.org" user-emacs-directory))

(if (file-exists-p my-init-file)
    (load-file my-init-file)
  (progn
    (org-babel-load-file
     (expand-file-name "emacs-init.org" user-emacs-directory))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (cider racer graphql-mode groovy-mode async terraform-mode omnisharp flycheck-gometalinter bazel-mode flycheck-elixir alchemist toml-mode python-docstring yaml-mode xterm-color web-mode utop use-package tuareg sonic-pi sass-mode ruby-electric rainbow-identifiers rainbow-delimiters puppet-mode paredit pallet mediawiki markdown-mode haskell-mode go-mode gist flycheck-rust eruby-mode erlang dockerfile-mode color-theme clojure-mode cargo auto-complete)))
 '(python-fill-docstring-style (quote symmetric))
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-c/c++-gcc-executable "/usr/local/bin/arm-none-eabi-gcc")
     (flycheck-disabled-checkers quote
                                 (c/c++-clang))
     (flycheck-gcc-definitions quote
                               ("EFM32HG"))
     (flycheck-disabled-checkers
      (quote
       (c/c++-clang)))
     (flycheck-gcc-definitions
      (quote
       ("EFM32HG")))
     (setq flycheck-disabled-checkers
           (quote
            (c/c++-clang)))
     (eval setq flycheck-gcc-definitions
           (quote
            ("EFM32HG")))
     (eval setq flycheck-gcc-include-path
           (list
            (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")
            (expand-file-name "~/code/tomu-quickstart/include/")))
     (eval setq flycheck-disabled-checkers
           (quote
            (c/c++-clang)))
     (eval
      (setq flycheck-gcc-include-path
            (list
             (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")))
      (setq flycheck-disabled-checkers
            (quote
             (c/c++-clang))))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")))
     (eval setq flycheck-gcc-include-path
           (list
            (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")))
     (test-case-name . twisted\.test\.test_paths)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
