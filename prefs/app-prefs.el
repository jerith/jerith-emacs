;; Stuff that only matters for Emacs.app.


(if (display-graphic-p)
    (when (not (boundp 'aquamacs-version))
      ;; Make this more Emacsy.
      (tool-bar-mode 0)
      (set-face-attribute 'default nil
			  :family "Inconsolata" :height 140 :weight 'normal)))
