;; web-mode, please.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)

;; Use tidy5 instead of tidy, because we like HTML5.
(setq flycheck-html-tidy-executable "tidy5")

;; This is like HTML, right?
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
