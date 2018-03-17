
(require 'cask)
(cask-initialize)

(eval-when-compile
  (require 'use-package))
;; (setq use-package-verbose t)

(use-package async
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
  (setq ido-everywhere t
        ;; If the file at point exists, use that
        ido-use-filename-at-point nil
        ;; Or if it is an URL...
        ido-use-url-at-point nil
        ;; Even if TAB completes uniquely,
        ;; still wait for RET
        ido-confirm-unique-completion t
        ;; If the input does not exist,
        ;; don't look in unexpected places.
        ;; I probably want a new file.
        ido-auto-merge-work-directories-length -1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))

;; Syntax highlighting on.
(global-font-lock-mode 1)
(defconst font-lock-maximum-decoration t)

;; Show various whitespace.
(setq whitespace-style '(face empty tabs lines-tail trailing))
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
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")
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

;; One space between sentences, please.
(setq sentence-end-double-space nil)

;; Undo some cruft that may have been done.
(cua-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-screen t)

;; Better behaviour when started with multiple files.
(setq inhibit-startup-buffer-menu t)
(setq split-width-threshold 150)

;; Current point in mode bar.
(line-number-mode t)
(column-number-mode t)

;; Turn off backups (that's what VCS is for) and move auto-save out the way.
(setq auto-save-default nil)
(setq make-backup-files nil)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

(setq-default gist-view-gist t)

(use-package git-emacs
  :load-path "third-party/git-emacs")

(use-package org
  :config
  (setq org-src-fontify-natively t))

(use-package rainbow-delimiters
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

(use-package dockerfile-mode
  :mode "\\.docker$")

(use-package elixir-mode
  :mode (("\\.exs?$" . elixir-mode))
  :config
  (require 'alchemist)
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (setq alchemist-hooks-compile-on-save t))

(use-package go-mode
  :hook
  (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'exec-path "~/.gostuff/bin")
  (add-hook 'go-mode-hook 'flycheck-mode))

(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (setq flycheck-gometalinter-fast t)
    (setq flycheck-gometalinter-tests t)
    (flycheck-gometalinter-setup)))

;; web-mode, please.
(use-package web-mode
  :mode "\\.html?$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  ;; Use tidy5 instead of tidy, because we like HTML5.
  (setq flycheck-html-tidy-executable "tidy5"))

;; This is like HTML, right?
(use-package sass-mode
  :mode "\\.scss\\'")

(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)
         ("\\.atd$" . tuareg-mode))
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
  :mode "\\.m$")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda () (local-set-key "\C-m" 'newline-and-indent)))
  (use-package python-docstring
    :config
    (setq python-docstring-sentence-end-double-space nil)
    (python-docstring-install)))

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
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(add-hook 'rust-mode-hook #'flycheck-rust-setup)

(add-hook 'shell-mode-hook 'emacs-lock-mode)
