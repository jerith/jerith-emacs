;; Not really a filetype, but still.

;; We only want this if we're going to use it
(when (boundp 'confluence-url)
  (require 'confluence)
  (add-hook 'confluence-mode-hook
            (local-set-key "\C-xw" confluence-prefix-map)
            (local-set-key "\M-j" 'confluence-newline-and-indent)
            (local-set-key "\M-;" 'confluence-list-indent-dwim)))
