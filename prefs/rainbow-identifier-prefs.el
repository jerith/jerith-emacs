
(require 'rainbow-identifiers)
(require 'font-lock-keyword-utils)


(setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
      rainbow-identifiers-cie-l*a*b*-lightness 90
      rainbow-identifiers-cie-l*a*b*-saturation 20
      rainbow-identifiers-cie-l*a*b*-color-count 15)

(defun start-rainbow-identifiers ()
  (flku-remove-variable-name-keywords)
  (rainbow-identifiers-mode)
  (flku-remove-variable-name-keywords))

(add-hook 'prog-mode-hook 'start-rainbow-identifiers)
;; Apparently this is special?
(add-hook 'python-mode-hook 'start-rainbow-identifiers)
