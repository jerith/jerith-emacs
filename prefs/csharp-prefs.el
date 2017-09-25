(setq omnisharp-server-executable-path
      "/Users/jerith/code/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")


(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'company-mode)
(add-hook 'csharp-mode-hook (lambda ()
                              (setq-local fill-column 120)
                              (setq-local whitespace-line-column 120)))
