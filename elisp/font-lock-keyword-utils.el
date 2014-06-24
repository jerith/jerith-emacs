
(defun flku--extract-user-keywords (keywords)
  "Extract a user-level keywords list from KEYWORDS.

If KEYWORDS is a compiled keywords list, the user-level keywords
list within it is returned. Otherwise KEYWORDS is returned
unmodified."
  (cond ((null keywords) keywords)
        ((eq (car keywords) t) (nth 1 keywords))
        (t keywords)))


(defun flku--matcherp (obj)
  "Return non-nil if OBJ is a valid keyword matcher; nil otherwise."
  (cond ((stringp obj) t)
        ((functionp obj) t)
        (t nil)))


(defun flku--parse-facespec (obj)
  "Extract a face from OBJ, which should be a facespec.

If OBJ is a symbol, it is returned as-is; if it is a string, it
is interned and returned; if it is a list starting with 'face' or
'quote', the second item is returned; otherwise nil is returned."
  (cond ((symbolp obj) obj)
        ((stringp obj) (intern obj))
        ((listp obj) (cond ((equal (car obj) 'face) (nth 1 obj))
                           ((equal (car obj) 'quote) (nth 1 obj))
                           (t nil)))
        (t nil)))


(defun flku--highlighterp (obj)
  "Return non-nil if OBJ is a valid highlighter list; nil otherwise."
  (cond ((null obj) nil)
        ((not (listp obj)) nil)
        ((integerp (car obj)) t)
        ((flku--matcherp (car obj)) t)
        (t nil)))


(defun flku--make-highlighter (&optional index facespec)
  "Build a subexp highligher from INDEX and FACESPEC using suitable defaults."
  (list (if (null index) 0 index)
        (if (null facespec) ''font-lock-keyword-face facespec)))


(defun flku--canonicalise-keyword (keyword)
  "Return a canonical form of KEYWORD.

The canonical form is (MATCHER HIGHLIGHTERS...) with simpler
forms converted to a matcher followed by a list of highlighters."
  (cond ((flku--matcherp keyword)
         (list keyword (flku--make-highlighter)))
        ((not (listp keyword))
         (error "Not a valid keyword entry"))
        ((integerp (cdr keyword))
         (list (car keyword) (flku--make-highlighter (cdr keyword))))
        ((flku--parse-facespec (cdr keyword))
         (list (car keyword) (flku--make-highlighter nil (cdr keyword))))
        ((flku--highlighterp (cdr keyword))
         (list (car keyword) (cdr keyword)))
        (t keyword)))


(defun flku--build-new-keywords (keywords process-keyword-function)
  "Construct a new keyword list from KEYWORDS by applying PROCESS-KEYWORD-FUNCTION to each entry.

PROCESS-KEYWORD-FUNCTION takes a keyword list entry as a
parameter and must return a keyword list entry or nil."
  (delq nil (mapcar process-keyword-function
                    (flku--extract-user-keywords keywords))))


(defun flku--filter-highlighter-exclude-face (face highlighter)
  ""
  (cond ((integerp (car highlighter))
         (if (equal face (flku--parse-facespec (nth 1 highlighter)))
             nil
           highlighter))
        ((flku--matcherp (car highlighter))
         (let ((submatchers (flku--filter-highlighters-exclude-face face (nthcdr 3 highlighter))))
           (if submatchers
               (cons (nth 0 highlighter)
                     (cons (nth 1 highlighter)
                           (cons (nth 2 highlighter) submatchers)))
             nil)))))


(defun flku--filter-highlighters-exclude-face (face highlighters)
  ""
  (delq nil (mapcar (apply-partially 'flku--filter-highlighter-exclude-face face)
                    highlighters)))


(defun flku--filter-exclude-face (face keyword)
  ""
  (let ((canonical-keyword (flku--canonicalise-keyword keyword)))
    (let ((highlighters (flku--filter-highlighters-exclude-face face (cdr canonical-keyword))))
      (cond ((null highlighters) nil)
            ((equal highlighters (cdr canonical-keyword)) keyword)
            (t (cons (car keyword) highlighters))))))


(defun flku-remove-variable-name-keywords ()
  (let ((keywords (flku--extract-user-keywords font-lock-keywords)))
    (let ((new-keywords (flku--build-new-keywords keywords (apply-partially 'flku--filter-exclude-face 'font-lock-variable-name-face))))
      (unless (equal keywords new-keywords)
        (font-lock-remove-keywords nil keywords)
        (font-lock-add-keywords nil new-keywords)))))


(provide 'font-lock-keyword-utils)
