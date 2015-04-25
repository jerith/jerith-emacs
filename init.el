(require 'cask)
(cask-initialize)
(require 'pallet)


;; Autorevert to make VCS nicer
(global-auto-revert-mode 1)


;; One space between sentences, please.
(setq sentence-end-double-space nil)


;; Better handling for unique buffer names:
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")


;; Use ido
(require 'ido)
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
      ido-auto-merge-work-directories-length -1)


;; Undo some cruft that may have been done.
(cua-mode 0)
(setq inhibit-startup-message 1)


;; Syntax highlighting on.
(global-font-lock-mode 1)
(defconst font-lock-maximum-decoration t)
;; Show trailing whitespace.
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq-default show-trailing-whitespace t)

;; Current point in mode bar.
(line-number-mode t)
(column-number-mode t)

;; Enable highlighting when marking a region
(setq-default transient-mark-mode t)

;; Never use tabs to indent:
(setq-default indent-tabs-mode nil)

;; Tab stops:
(setq-default tab-width 4)

;; Fill column:
(setq-default fill-column 79)

;; Bind keys:
(global-set-key "\C-cg" 'goto-line)

;; Turn off backups (that's what VCS is for) and move auto-save out the way.
(setq auto-save-default nil)
(setq make-backup-files nil)

(load-file "~/.emacs.d/setup-prefs.el")
