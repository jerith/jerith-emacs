;; This really needs some splitting up.


;; On OSX, we don't get a login shell by default or clipboard access.
(if (eq system-type 'darwin)
    (progn
      ;; Fix some keybindings
      (global-set-key [home] 'move-beginning-of-line)
      (global-set-key  [end] 'move-end-of-line)
      ;; Invoke login shells, so that .profile or .bash_profile is read
      (setq shell-command-switch "-lc")
      ;; Load some pbcopy/pbpaste functions
      (require 'pbstuff)))

(unless (display-graphic-p)
  (menu-bar-mode -1))


;; ;; Autorevert to make VCS nicer
;; (global-auto-revert-mode 1)


;; ;; One space between sentences, please.
;; (setq sentence-end-double-space nil)


;; ;; Better handling for unique buffer names:
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward
;;       uniquify-separator ":")


;; ;; Use ido
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-everywhere t
;;       ;; If the file at point exists, use that
;;       ido-use-filename-at-point nil
;;       ;; Or if it is an URL...
;;       ido-use-url-at-point nil
;;       ;; Even if TAB completes uniquely,
;;       ;; still wait for RET
;;       ido-confirm-unique-completion t
;;       ;; If the input does not exist,
;;       ;; don't look in unexpected places.
;;       ;; I probably want a new file.
;;       ido-auto-merge-work-directories-length -1)


;; ;; Undo some cruft that may have been done.
;; (cua-mode 0)
;; (tool-bar-mode 0)
;; (setq inhibit-startup-message 1)


;; ;; Syntax highlighting on.
;; (global-font-lock-mode 1)
;; (defconst font-lock-maximum-decoration t)
;; ;; Show trailing whitespace.
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)
;; ;; (setq-default show-trailing-whitespace t)


;; ;; Word wrap:
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'latex-mode-hook 'turn-on-auto-fill)
;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;; (add-hook 'nXML-mode-hook 'turn-on-auto-fill)
;; (add-hook 'nxml-mode-hook 'turn-on-auto-fill)


;; ;; Current point in mode bar.
;; (line-number-mode t)
;; (column-number-mode t)

;; ;; Enable highlighting when marking a region
;; (setq-default transient-mark-mode t)

;; ;; Never use tabs to indent:
;; (setq-default indent-tabs-mode nil)

;; ;; Tab stops:
;; (setq-default tab-width 4)

;; ;; Fill column:
;; (setq-default fill-column 79)

;; ;; Bind keys:
;; (global-set-key "\C-cg" 'goto-line)

;; ;; ;; Buffer swapping stuff:
;; ;; (require 'swbuff-y)
;; ;; (swbuff-y-mode)

;; ;; ;; Punish use of arrow keys:
;; ;; (require 'punish-arrows)

;; ;; Undisable some things.
;; (put 'scroll-left 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)

;; (defun fc-eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;;              (current-buffer))
;;     (error (message "Invalid expression")
;;            (insert (current-kill 0)))))

;; (global-set-key (kbd "C-c e") 'fc-eval-and-replace)
;; (global-set-key (kbd "C-x C-i") 'indent-rigidly)

;; ;; Turn off backups (that's what VCS is for) and move auto-save out the way.
;; (setq auto-save-default nil)
;; (setq make-backup-files nil)
