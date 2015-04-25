
;; ;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; ;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (require 'xterm-color)

;; (progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;;        (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
;;        (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

;; ;; (progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;; ;;        (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; ;;        (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))

;; OSX has a kernel bug that can be triggered by killing a running shell when
;; exiting. To avoid this, we lock shell buffers and require the subprocess to
;; be manually terminated.
(add-hook 'shell-mode-hook 'emacs-lock-mode)
