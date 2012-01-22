;; Should we assume we have nxml-mode here?

;; .xsd and .wsdl files are also XML...
(add-to-auto-modes "\.xsd$" 'nxml-mode)
(add-to-auto-modes "\.wsdl$" 'nxml-mode)

;; Autoindent xml:
(add-hook 'nxml-mode-hook (lambda ()
                            (local-set-key "\C-m" 'newline-and-indent)))
