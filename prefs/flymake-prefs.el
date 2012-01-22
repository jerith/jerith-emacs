;; Flymake error checking.

(eval-after-load "flymake"
  '(progn
     (defun flymake-on-timer-event (buffer)
       "Start a syntax check for buffer BUFFER if necessary."
       ;; Do nothing, don't want to run checks until I save.
       )))
(eval-after-load "flymake"
  '(progn
    (defun flymake-after-change-function (start stop len)
      "Start syntax check for current buffer if it isn't already running."
      ;; Do nothing, don't want to run checks until I save.
      )))
(require 'flymake-cursor)
(global-set-key [f4] 'flymake-goto-next-error)
