;; Replacement zoom functionality for aquamacs.


(defun do-zoom-font (delta-percentage)
  "Zoom default face in current buffer by `delta-percentage' percent.
`zoom-font-frame-local-flag' indicates whether the zoom is local
to the selected frame."
  ;; the Zoom is per buffer and per frame.
  ;; we can't set the frame font or change the frame's or a global default face,
  ;; because this can get remapped via face-remapping-alist
  ;; thus, we must go through face-remapping-alist, which will make this setting
  ;; local to the specific buffer.
  ;; to allow
  (let ((factor-delta (/ delta-percentage 100.0))
        (frame (if zoom-font-frame-local-flag (selected-frame) nil))
        (default-face 'default)
        (zoom-face nil))

    ;; set default-face to the face that default remaps to,
    ;; and find the zoom face.
    (mapc
     (lambda (entry)
       (when (and (eq (car entry) 'default)
                  (symbolp (cdr entry)))
         (if (string-match "zoom-.*" (symbol-name (cdr entry)))
             (setq zoom-face (intern (match-string 0 (symbol-name (cdr entry)))))
           (if (and (eq default-face 'default) ;; choose the first matching entry
                    (not (string-match "zoom-.*" (symbol-name (cdr entry)))))
               (setq default-face (cdr entry))))))
     face-remapping-alist)

    (unless zoom-face (setq zoom-face (intern (make-temp-name "zoom-"))))
    (unless (facep zoom-face)
      (make-empty-face zoom-face)
      (set-face-documentation
       zoom-face
       (purecopy "Zoom face.")))
    (set-face-attribute zoom-face nil :inherit default-face) ;; on all frames
    ;; we can't use the global (default) value for face-remapping-alist
    ;; because global and local faces aren't merged in the same way.
    (let ((zoom-factor (face-attribute zoom-face :height frame nil))
          (alist-entry (cons 'default zoom-face)))
      (if (or (not (member alist-entry face-remapping-alist)) (eq 'unspecified zoom-factor))
          (setq zoom-factor 1.0))

      (setq zoom-factor (/ (round (+ zoom-factor factor-delta) 0.01) 100.0))
      ;; to do; only do this if no other frame shows it.
      (when (> zoom-factor 0.0)
        (if (= zoom-factor 1.0)
            ;; remove zoom completely from face-remapping-alist
            (setq face-remapping-alist (delete alist-entry face-remapping-alist))
          (set-face-attribute zoom-face frame :height zoom-factor)
          (unless (member alist-entry face-remapping-alist)
            (setq face-remapping-alist (cons alist-entry face-remapping-alist))))
        (message "Zoom: %0d%%" (* 100 zoom-factor))))))


(defun calc-zoom-amount (delta-prefix)
   (cond ((eq nil delta-prefix) 5)
         ((eq '- delta-prefix) -5)
         ((listp delta-prefix)
          (setq delta-prefix (car delta-prefix))
          (let ((sign (/ delta-prefix (abs delta-prefix)))
                (perc 0))
            (setq delta-prefix (abs delta-prefix))
            (while (> delta-prefix 0)
              (setq perc (+ perc 5))
              (setq delta-prefix (lsh delta-prefix -2)))
            (* sign perc)))
         (t delta-prefix)))


(defun zoom-interactive (&optional delta-prefix)
  "Zoom default face in current buffer.
Prefix handling:
None : Zoom in 5%
M-- : Zoom out 5%
C-u [C-u [...]] : Zoom in (or out with M--) an additional 5% for each C-u
C-u <number> : Zoom in (or out, for negative values) 'number'%
"
  (interactive "P")
  (do-zoom-font (calc-zoom-amount delta-prefix)))


(defun zoom-interactive-out (&optional delta-prefix)
  "Zoom (inversely) default face in current buffer."
  (interactive "P")
  (do-zoom-font (- 0 (calc-zoom-amount delta-prefix))))


(defun zoom-font (&optional dir)
  "Zoom default face in current buffer.
With prefix argument DIR, shrink the face; otherwise enlarge.
`zoom-font-frame-local-flag' indicates whether the zoom is local
to the selected frame."
  (interactive "P")
  ;; the Zoom is per buffer and per frame.
  ;; we can't set the frame font or change the frame's or a global default face,
  ;; because this can get remapped via face-remapping-alist
  ;; thus, we must go through face-remapping-alist, which will make this setting
  ;; local to the specific buffer.
  ;; to allow
  (do-zoom-font (if (listp last-input-event)
                    (if dir -10 10) ;; mouse wheel event
                  (if dir -20 20))))


(provide 'zoom-replacement)
