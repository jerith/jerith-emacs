;; CEDET and ECB are heavyweight.

(defun load-cedet-and-ecb ()
  ;; CEDET
  (load-file "~/.emacs.d/elisp/cedet/common/cedet.el")
  (semantic-load-enable-code-helpers)
  (semantic-load-enable-primary-exuberent-ctags-support)
  (require 'semantic-ia)
  ;; JDEE
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/jdee"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/elib"))
  (require 'jde)
  ;; ECB
  (add-to-list 'load-path "~/.emacs.d/elisp/ecb")
  (require 'ecb))
