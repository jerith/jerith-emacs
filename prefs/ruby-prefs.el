;; Most of this is to unbreak indentation.
(setq ruby-use-smie nil)
(eval-after-load "ruby-mode"
  '(progn
     (defadvice ruby-indent-line (after unindent-closing-paren activate)
       (let ((column (current-column))
             indent offset)
         (save-excursion
           (back-to-indentation)
           (let ((state (syntax-ppss)))
             (setq offset (- column (current-column)))
             (when (and (eq (char-after) ?\))
                        (not (zerop (car state))))
               (goto-char (cadr state))
               (setq indent (current-indentation)))))
         (when indent
           (indent-line-to indent)
           (when (> offset 0) (forward-char offset)))))
     (setq ruby-deep-indent-paren-style nil)
     (add-hook 'ruby-mode-hook 'ruby-electric-mode)))
