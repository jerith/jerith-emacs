;; Some nice git extras.

(add-local-elisp-subdir "third-party/git-emacs")
(require 'git-emacs)

(defun update-git-modeline ()
  (interactive)
  (git--update-all-state-marks))

(global-set-key (kbd "C-c u") 'update-git-modeline)
