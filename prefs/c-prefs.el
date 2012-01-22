
(setq-default c-basic-offset 4)
(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key "\C-m" 'newline-and-indent)))
