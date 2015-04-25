(eval-after-load "scala-mode2"
  '(progn
     (require 'ensime)
     (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))
