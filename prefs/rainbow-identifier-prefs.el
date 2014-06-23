
(defun rvnk--matcherp (obj)
  (cond ((stringp obj) t)
        ((functionp obj) t)
        (t nil)))

(defun rvnk--parse-facespec (obj)
  (cond ((symbolp obj) obj)
        ((listp obj) (cond ((equal (car obj) 'face) (car (cdr obj)))
                           (t nil)))
        (t nil)))

(defun rvnk--find-face-name-highlighter (highlighter)
  (cond ((integerp (car highlighter))
         (rvnk--parse-facespec (nth 1 highlighter)))
        ((listp (car highlighter))
         (rvnk--find-face-name-highlighter (car highlighter)))
        (t nil)))

(defun rvnk--find-face-name (keyword)
  (cond ((rvnk--matcherp keyword) nil)
        ((listp keyword)
         (let ((second (cdr keyword)))
           (cond ((rvnk--parse-facespec second) (rvnk--parse-facespec second))
                 (t (rvnk--find-face-name-highlighter second)))))
        (t nil)))

(defun remove-variable-name-keywords ()
  (let (keywords-to-remove)
    (dolist (keyword font-lock-keywords)
      (let ((face-name (rvnk--find-face-name keyword)))
        (when (equal face-name 'font-lock-variable-name-face)
          (setq keywords-to-remove (cons keyword keywords-to-remove)))))
    (message "Removing %d keyword entries." (safe-length keywords-to-remove))
    (font-lock-remove-keywords nil keywords-to-remove)
    (safe-length keywords-to-remove)))

(setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
      rainbow-identifiers-cie-l*a*b*-lightness 90
      rainbow-identifiers-cie-l*a*b*-saturation 20
      rainbow-identifiers-cie-l*a*b*-color-count 15)

(defun start-rainbow-identifiers ()
  (rainbow-identifiers-mode)
  (remove-variable-name-keywords))

(add-hook 'prog-mode-hook 'start-rainbow-identifiers)
;; Apparently this is special?
(add-hook 'python-mode-hook 'start-rainbow-identifiers)
