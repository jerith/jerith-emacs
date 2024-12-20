(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(eval-when-compile
  (require 'use-package))
;; (setq use-package-verbose t)

(use-package async
  :ensure t
  :config
  (defun my/init-hook ()
    "If the current buffer is 'emacs-init.org' the code-blocks are tangled."
    (when (equal (buffer-file-name) my-org-file)
      (async-start
       `(lambda ()
          (require 'org)
          (org-babel-tangle-file ,my-org-file))
       (lambda (result)
         (message "Tangled file compiled.")))))
  (add-hook 'after-save-hook 'my/init-hook))

(add-to-list 'load-path my-elisp-dir)

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package ido
  :config
  (ido-mode t)
  :custom
  (ido-everywhere t)
  (ido-use-filename-at-point nil "If the file at point exists, use that. (?)")
  (ido-use-url-at-point nil "Or if it is an URL...")
  (ido-confirm-unique-completion t "Even if TAB completes uniquely, still wait for RET.")
  (ido-auto-merge-work-directories-length -1 "If the input doesn't exist, don't look in unexpected places. I probably want a new file."))

(use-package uniquify
  ;; :ensure t
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator ":"))

;; Syntax highlighting on.
(global-font-lock-mode 1)
(defconst font-lock-maximum-decoration t)

;; Show various whitespace.
(setopt whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq-default show-trailing-whitespace t)

;; Enable highlighting when marking a region
(setq-default transient-mark-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default fill-column 79)

(when (eq system-type 'darwin)
  ;; Fix some keybindings
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key  [end] 'move-end-of-line)
  ;; Avoid dired/ls errors
  (setopt dired-use-ls-dired nil)
  ;; Invoke login shells so that .profile or .bash_profile is read
  (setopt shell-command-switch "-lc")
  ;; Load some pbcopy/pbpaste functions
  (require 'pbstuff))

(unless (display-graphic-p)
  (menu-bar-mode -1))

(when (boundp 'aquamacs-version)
  ;; Make this more Emacsy.
  (one-buffer-one-frame-mode -1)
  (tabbar-mode -1)

  ;; Make some keybindings saner.
  (define-key osx-key-mode-map `[(,osxkeys-command-key w)] nil)
  (define-key osx-key-mode-map [home] 'move-beginning-of-line)
  (define-key osx-key-mode-map  [end] 'move-end-of-line)
  (define-key osx-key-mode-map [A-home] 'beginning-of-buffer)
  (define-key osx-key-mode-map  [A-end] 'end-of-buffer)
  (define-key osx-key-mode-map [C-left] 'backward-word)
  (define-key osx-key-mode-map [C-right] 'forward-word)

  ;; Get rid of the stupid "Mac" modifiers.
  (setq ns-use-mac-modifier-symbols nil)

  ;; Improve zooming.
  (require 'zoom-replacement)
  (define-key osx-key-mode-map `[(,osxkeys-command-key =)] 'zoom-interactive)
  (define-key osx-key-mode-map `[(,osxkeys-command-key +)] 'zoom-interactive)
  (define-key osx-key-mode-map `[(,osxkeys-command-key -)] 'zoom-interactive-out))

(unless (boundp 'aquamacs-version)
  (when (display-graphic-p)
    ;; Nicer font.
    (set-face-attribute
     'default nil
     :family "Inconsolata" :height 140 :weight 'normal)))

;; Autorevert to make VCS nicer
(global-auto-revert-mode 1)

;; Stop trying to copy into the system clipboard!
(setopt select-enable-primary nil)
(setopt select-enable-clipboard nil)

;; No shift-select, please. It gets in the way.
(setopt shift-select-mode nil)

;; One space between sentences, please.
(setopt sentence-end-double-space nil)

;; Undo some cruft that may have been done.
(cua-mode 0)
(if window-system (tool-bar-mode 0))
(setopt inhibit-startup-screen t)

;; Better behaviour when started with multiple files.
(setopt inhibit-startup-buffer-menu t)
(setopt split-width-threshold 150)

;; Current point in mode bar.
(line-number-mode t)
(column-number-mode t)

;; Turn off backups (that's what VCS is for) and move auto-save out the way.
(setopt auto-save-default nil)
(setopt make-backup-files nil)

;; (use-package flycheck
;;   :ensure t
;;   :custom
;;   (global-flycheck-mode t "Turn on flycheck, please.")
;;   (flycheck-check-syntax-automatically '(mode-enabled save) "Don't get in the way.")
;;   (flycheck-yamllintrc ".yamllint.yaml"))

(use-package flymake
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

(use-package flyspell
  :hook
  ((org-mode markdown-mode) . flyspell-mode)
  ;; (prog-mode . flyspell-prog-mode)
  (before-save-hook . flyspell-buffer)
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=normal" "--master=en_GB-ize-w_accents")))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package eglot
  :custom
  ;; (eglot-send-changes-idle-time 60 "I'd rather not do this at all, but it's better than nothing.")
  (eglot-connect-timeout 60 "elixir-ls takes a while to start, sometimes.")
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  (add-to-list 'eglot-stay-out-of 'eldoc)
  :hook ((python-ts-mode elixir-ts-mode kotlin-ts-mode) . eglot-ensure))

(setq-default gist-view-gist t)

(use-package org
  :custom
  (org-src-fontify-natively t "I think this is the default now."))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; Apparently this is special?
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  :config
  ;; Set some custom colours based loosely on the zenburn theme.
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "grey55")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#f0dfaf")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#94bff3")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#dca3a3")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#8fb28f")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#93e0e3")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#dfaf8f")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#dc8cc3"))

(setq-default c-basic-offset 4)
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key "\C-m" 'newline-and-indent)))

(use-package omnisharp-mode
  :hook csharp-mode
  :custom
  (omnisharp-server-executable-path "/usr/local/bin/omnisharp"))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  :config
  (use-package flycheck-clj-kondo
    :ensure t))

(use-package cider
  :ensure t
  :defer t)

(use-package csv-mode
  :ensure t)

;; (use-package dhall-mode
;;   ;; :ensure t
;;   :custom
;;   (dhall-format-at-save nil "Please don't change my text under me."))

(use-package dockerfile-mode
  :ensure t
  :mode "\\.docker\\'")

(use-package elixir-ts-mode
  :ensure t)

;; (use-package alchemist
;;   :ensure t)

;; (use-package elixir-mode
;;   :ensure t
;;   :mode (("\\.exs?$" . elixir-mode))
;;   :config
;;   (require 'alchemist)
;;   (add-hook 'elixir-mode-hook 'alchemist-mode)
;;   (setq alchemist-hooks-compile-on-save t))

(use-package erlang
  :ensure t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.app\\.src\\'" . erlang-mode)
         ("\\.escript" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.xrl\\'" . erlang-mode)
         ("\\.yrl\\'" . erlang-mode)
         ("/ebin/.+\\.app" . erlang-mode)))

(use-package fountain-mode
  :ensure t)

(use-package git-modes
  :ensure t)

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp)
         (go-mode
          . (lambda ()
              ;; Drop tabs from visible whitespace list
              (setq-local whitespace-style '(face empty lines-tail trailing))
              ;; Let LSP rewrite my file, because Go is too annoying otherwise
              (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
              (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))))
  :config
  (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin")))

(use-package graphql-mode
  :ensure t)

;; web-mode, please.
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
  ;; Use tidy5 instead of tidy, because we like HTML5.
  (flycheck-html-tidy-executable "tidy5")
  :config
  (add-hook 'web-mode-hook
        (lambda ()
          (when (string-equal "tsx" (file-name-extension buffer-file-name))
            (setup-tide-mode)))))

;; This is like HTML, right?
(use-package sass-mode
  :ensure t
  :mode "\\.scss\\'")

(setq-default js-indent-level 2)

(use-package jq-mode
  :ensure t
  :mode (("\\.jq\\'" . jq-mode)))

(use-package jsonnet-mode
  :ensure t)

(use-package kotlin-ts-mode
  :ensure t
  :mode "\\.kts?\\'"
  :hook ((kotlin-ts-mode
          . (lambda ()
              (setq-local fill-column 139)
              (setq-local whitespace-line-column 139)
              ;; Use // instead of /* ... */ for comments.
              ;; (Adapted from elixir-ts-mode.)
              (setq-local comment-start "// ")
              (setq-local comment-start-skip
                          (rx "//" (* (syntax whitespace))))
              (setq-local comment-end "")
              (setq-local comment-end-skip
                          (rx (* (syntax whitespace))
                              (group (or (syntax comment-end) "\n"))))))))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((python-mode . lsp-deferred)
;;          ((rust-mode dhall-mode) . lsp))
;;   :config
;;   (setq lsp-prefer-flymake nil
;;         lsp-enable-snippet nil
;;         lsp-headerline-breadcrumb-enable nil)
;;   (lsp-register-custom-settings '(("pylsp.plugins.pylsp_mypy.dmypy" nil t)
;;                                   ("pylsp.plugins.pylsp_mypy.live_mode" nil t)
;;                                   ("pylsp.plugins.pylsp_mypy.report_progress" t t)))
;;   :commands (lsp lsp-deferred))

;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (setq lsp-ui-doc-enable nil
;;         lsp-ui-flycheck-enable t
;;         lsp-ui-flycheck-live-reporting t
;;         lsp-ui-sideline-show-hover nil)
;;   :commands lsp-ui-mode)

;; (use-package lua-mode
;;   :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package tuareg
  :mode (("\\.ml[ily]?\\'" . tuareg-mode)
         ("\\.topml\\'" . tuareg-mode)
         ("\\.atd\\'" . tuareg-mode))
  :config
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
  (add-hook 'tuareg-mode-hook 'auto-complete-mode))

(use-package octave-mode
  :mode "\\.m\\'")

(use-package ox-reveal
  :ensure t
  :config
  (setq org-export-allow-bind-keywords t))

(use-package php-mode
  :ensure t
  :custom
  (php-mode-coding-style 'symfony2))

(use-package powershell
  :ensure t)

(use-package powershell
  :ensure t)

(use-package puppet-mode
  :ensure t)

(use-package python
  :custom
  (python-fill-docstring-style 'django))

;; Assorted old stuff for the _other_ python-mode.
;; (use-package python-mode
;;   :ensure t
;;   :init
;;   (setq py-underscore-word-syntax-p nil)
;;   :custom
;;   ;; This breaks indenting various things.
;;   ;; (py-closing-list-dedents-bos t)
;;   (py-docstring-syle 'django)
;;   (py-docstring-fill-column 79)
;;   (py-mark-decorators t)
;;   (py-indent-list-style 'one-level-to-beginning-of-statement))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :init
  (setq ruby-use-smie nil)
  :config
  (defadvice ruby-indent-line (after unindent-closing-paren activate)
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset)))))
  (setq ruby-deep-indent-paren-style nil)
  (use-package ruby-electric
    :ensure t)
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

;; (add-hook 'rust-mode-hook #'flycheck-rust-setup)

(use-package cargo
  :ensure t
  )

(use-package rust-mode
  :ensure t
  )

(add-hook 'shell-mode-hook 'emacs-lock-mode)

(use-package terraform-mode
  :ensure t)

(add-hook 'text-mode-hook
          (lambda ()
            (setq-local whitespace-style '(face empty tabs trailing))
            (turn-on-visual-line-mode)))

(use-package typescript-mode
  :ensure t)

(setq-default typescript-indent-level 2)

(use-package tide
  :ensure t
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package yaml-mode
  :ensure t)
