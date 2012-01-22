;; Released to the public domain. See LICENSE for details.

;; Instructions for use
;;
;; In your ~/.emacs, write:
;;   (load-file "~/.emacs.d/setup-prefs.el")
;;
;; You can set `local-elisp-directory' if ~/.emacs.d doesn't suit you, but this isn't
;; very well tested.
;;
;; TODO: Document this more.


(unless (boundp 'local-elisp-directory)
  (defvar local-elisp-directory "~/.emacs.d"
    "Directory beneath which this particular tower of hacks expects to find various things."))

(unless (boundp 'prefs-files-to-skip)
  (defvar prefs-files-to-skip ()
    "Blacklist of prefs files to avoid loading."))

(defun get-full-path-for-subdir (subdir)
  (convert-standard-filename (concat local-elisp-directory "/" subdir)))

(defun add-local-elisp-subdir (subdir)
  "Add a subdirectory of `local-elisp-directory' to the load path."
  (add-to-list 'load-path (get-full-path-for-subdir subdir)))

(defvar prefs-file-regex "^[^\\.].*\\.elc?$")

(defun get-prefs-files-to-load (subdir)
  "Return files in SUBDIR which are eligible for loading. SUBDIR
is also added to the front of `load-path'. See `prefs-file-regex'
for determining which files should be loaded."
  (add-local-elisp-subdir subdir)
  (let ((files (directory-files (get-full-path-for-subdir subdir) nil prefs-file-regex)))
    ; TODO: Handle .el/.elc duplicates?
    ; TODO: Handle blacklist!
    files))

(defun load-prefs-file (file)
  "Load a single prefs file."
  (condition-case err
      (load file)
    (file-error
     (message "Unable to load `%s': %s" file err))))

(defun load-prefs-files (files)
  "Load prefs files in FILES list."
  (message "--== Loading prefs. ==--")
  (while files
    (let ((file (car files)))
      (load-prefs-file file))
    (setq files (cdr files)))
  (message "--== Finished loading prefs. ==--"))

(defun add-to-auto-modes (file-regex file-mode)
  "Convenience function for adding automodes."
  (add-to-list 'auto-mode-alist (cons file-regex file-mode)))

(defun auto-add-to-auto-modes (file-regex file-mode library)
  "Convenience function for autoloading a mode and adding automodes."
  (autoload file-mode library)
  (add-to-auto-modes file-regex file-mode))

(add-local-elisp-subdir "elisp")
(add-local-elisp-subdir "third-party")

;; Package magic.
(when (load-file (get-full-path-for-subdir "elpa/package.el"))
  (package-initialize))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(load-prefs-files (get-prefs-files-to-load "prefs"))
