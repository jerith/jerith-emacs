;; Flymake error checking.

(require 'flymake)

;; (defun flymake-on-timer-event (buffer)
;;   "Start a syntax check for buffer BUFFER if necessary."
;;   ;; Do nothing, don't want to run checks until I save.
;;   )

;; (defun flymake-after-change-function (start stop len)
;;   "Start syntax check for current buffer if it isn't already running."
;;   ;; Do nothing, don't want to run checks until I save.
;;   )

;; Clear out flymake things, because they're often broken.
;; TODO: This is a bit insane. We really need it to happen before anything that
;; might need flymake.
(setq-default flymake-allowed-file-name-masks '())

(add-hook 'find-file-hook 'flymake-find-file-hook)

(require 'flymake-cursor)
(global-set-key [f4] 'flymake-goto-next-error)
