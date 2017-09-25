(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(put 'for-all 'clojure-backtracking-indent '(1))
