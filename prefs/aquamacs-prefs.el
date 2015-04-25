;; Stuff that only matters for Aquamacs.


(when (boundp 'aquamacs-version)
  ;; Make this more Emacsy.
  (one-buffer-one-frame-mode -1)
  (tabbar-mode -1)
  (tool-bar-mode 0)

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
  (define-key osx-key-mode-map `[(,osxkeys-command-key -)] 'zoom-interactive-out)
  )
