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
 '(flycheck-yamllintrc ".yamllint.yaml")
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(org-re-reveal ox-reveal csv-mode powershell php-mode tide typescript-mode fountain-mode rust-mode nim-mode julia-mode flycheck-clj-kondo cider racer graphql-mode groovy-mode async terraform-mode omnisharp flycheck-gometalinter bazel-mode flycheck-elixir alchemist toml-mode python-docstring yaml-mode xterm-color web-mode utop use-package tuareg sonic-pi sass-mode ruby-electric rainbow-identifiers rainbow-delimiters puppet-mode paredit pallet mediawiki markdown-mode haskell-mode go-mode gist flycheck-rust eruby-mode erlang dockerfile-mode color-theme clojure-mode cargo auto-complete))
 '(python-fill-docstring-style 'symmetric)
 '(safe-local-variable-values
   '((org-confirm-babel-evaluate)
     (lsp-pylsp-plugins-flake8-enabled)
     (test-case-name . vumi.dispatchers.tests.test_load_balancer)
     (test-case-name . vumi.dispatchers.tests.test_base)
     (test-case-name . vumi.dispatchers.tests.test_endpoint_dispatchers)
     (encoding . utf-8)
     (test-case-name . vxsandbox.tests.test_sandbox_rlimiter)
     (test-case-name . vumi.application.tests.test_test_helpers)
     (test-case-name . vxsandbox.tests.test_worker)
     (test-case-name . vumi.tests.test_service)
     (test-case-name . vumi.application.tests.test_base)
     (test-case-name . vumi.tests.test_config)
     (test-case-name . vumi.tests.test_worker)
     (test-case-name . vxsandbox.resources.tests.test_config)
     (test-case-name . vxsandbox.resources.tests.test_kv)
     (test-case-name . vxsandbox.resources.tests.test_logging)
     (test-case-name . vxsandbox.resources.tests.test_outbound)
     (test-case-name . vxsandbox.resources.tests.test_utils)
     (test-case-name . vxsandbox.tests.test_protocol)
     (eval setq lsp-rust-rustflags
           (concat "-L dependency=" my-project-path "target/sysroot/lib/rustlib/x86_64-blog_os/lib"))
     (lsp-rust-rustflags . "-L dependency=target/sysroot/lib/rustlib/x86_64-blog_os/lib")
     (lsp-rust-sysroot . "target/sysroot")
     (eval setq lsp-rust-rustflags
           (concat "-L dependency=" my-project-path "target/sysrootlib/rustlib/x86_64-blog_os/lib"))
     (eval setq lsp-rust-rustflags
           (concat "-L dependency=" my-project-path "target/sysrootlib/rustlib/x86_64-unknown-none/lib"))
     (lsp-rust-rustflags . "")
     (lsp-rust-sysroot . "")
     (lsp-rust-all-targets)
     (eval setq lsp-rust-sysroot
           (concat my-project-path "target/sysroot"))
     (lsp-rust-sysroot . my-project-path)
     (eval set
           (make-local-variable 'my-project-path)
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (lsp-rust-target . x86_64-blog_os.json)
     (eval add-to-list 'lsp-file-watch-ignored '"[/\\\\]target-cov$")
     (eval setq flycheck-c/c++-gcc-executable "/usr/local/bin/arm-none-eabi-gcc")
     (flycheck-disabled-checkers quote
                                 (c/c++-clang))
     (flycheck-gcc-definitions quote
                               ("EFM32HG"))
     (flycheck-disabled-checkers
      '(c/c++-clang))
     (flycheck-gcc-definitions
      '("EFM32HG"))
     (setq flycheck-disabled-checkers
           '(c/c++-clang))
     (eval setq flycheck-gcc-definitions
           '("EFM32HG"))
     (eval setq flycheck-gcc-include-path
           (list
            (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")
            (expand-file-name "~/code/tomu-quickstart/include/")))
     (eval setq flycheck-disabled-checkers
           '(c/c++-clang))
     (eval
      (setq flycheck-gcc-include-path
            (list
             (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")))
      (setq flycheck-disabled-checkers
            '(c/c++-clang)))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")))
     (eval setq flycheck-gcc-include-path
           (list
            (expand-file-name "~/code/tomu-quickstart/libopencm3/include/")))
     (test-case-name . twisted.test.test_paths)))
 '(warning-suppress-types '((package reinitialization))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eruby-comment-face ((t (:inherit font-lock-comment-face :background "color-240"))))
 '(eruby-standard-face ((t (:background "color-240")))))
