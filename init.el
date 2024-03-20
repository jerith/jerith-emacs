;;; init.el --- Where all the magic begins

;;; Commentary:
;;
;; This is a thin wrapper around emacs-init.el which is generated from
;; emacs-init.org.

;;; Code:

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
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(yaml-mode web-mode typescript-mode tide terraform-mode sass-mode rust-mode ruby-electric rainbow-delimiters python-mode puppet-mode powershell php-mode pdf-tools ox-reveal org-ref org-re-reveal lua-mode lsp-ui key-chord jsonnet-mode jq-mode ivy helm-bibtex graphql-mode go-mode fountain-mode flycheck-clj-kondo erlang elixir-mode dockerfile-mode dhall-mode csv-mode cider cargo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eruby-comment-face ((t (:inherit font-lock-comment-face :background "color-240"))))
 '(eruby-standard-face ((t (:background "color-240")))))
(put 'dired-find-alternate-file 'disabled nil)
