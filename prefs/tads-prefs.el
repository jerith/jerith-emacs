;; (add-local-elisp-subdir "third-party/tads3")
;; (require 'tads3)
;; (add-to-list 'auto-mode-alist '("\\.t$" . tads-mode))

(autoload 'tads3-mode "tads3-mode" "TADS 2 editing mode." t)
(add-to-list 'auto-mode-alist '("\\.t$" . tads-mode))
