
(require 'ert)
(require 'font-lock-keyword-utils)


;; Tests for flku--extract-user-keywords

(ert-deftest flku-test--extract-user-keywords!empty ()
  (should (equal (flku--extract-user-keywords ()) ())))

(ert-deftest flku-test--extract-user-keywords!normal ()
  (setq keywords '("foo" ("bar" . 'font-lock-variable-name-face)))
  (should (equal (flku--extract-user-keywords keywords) keywords)))

(ert-deftest flku-test--extract-user-keywords!compiled ()
  (setq keywords '(t ("foo" ("bar" . 'font-lock-variable-name-face))
                     ("foo" (0 font-lock-keyword-face))
                     ("bar" (0 'font-lock-variable-name-face))))
  (should (equal (flku--extract-user-keywords keywords) (nth 1 keywords))))


;; Tests for flku--matcherp

(ert-deftest flku-test--matcherp!nil ()
  (should (equal (flku--matcherp nil) nil)))

(ert-deftest flku-test--matcherp!string ()
  (should (equal (flku--matcherp "") t)))

(ert-deftest flku-test--matcherp!lambda ()
  (should (equal (flku--matcherp (lambda () nil)) t)))


;; Tests for flku--parse-facespec

(ert-deftest flku-test--parse-facespec!nil ()
  (should (equal (flku--parse-facespec nil) nil)))

(ert-deftest flku-test--parse-facespec!face ()
  (should (equal (flku--parse-facespec font-lock-keyword-face)
                 'font-lock-keyword-face)))

(ert-deftest flku-test--parse-facespec!symbol ()
  (should (equal (flku--parse-facespec 'font-lock-keyword-face)
                 'font-lock-keyword-face)))

(ert-deftest flku-test--parse-facespec!string ()
  (should (equal (flku--parse-facespec "font-lock-keyword-face")
                 'font-lock-keyword-face)))

(ert-deftest flku-test--parse-facespec!face-list ()
  (should (equal (flku--parse-facespec '(face font-lock-keyword-face))
                 'font-lock-keyword-face)))

(ert-deftest flku-test--parse-facespec!quote-list ()
  (should (equal (flku--parse-facespec '(quote font-lock-keyword-face))
                 'font-lock-keyword-face)))


;; Tests for flku--make-highlighter

(ert-deftest flku-test--make-highlighter!no-params ()
  (should (equal (flku--make-highlighter)
                 '(0 'font-lock-keyword-face))))

(ert-deftest flku-test--make-highlighter!index-0 ()
  (should (equal (flku--make-highlighter 0)
                 '(0 'font-lock-keyword-face))))

(ert-deftest flku-test--make-highlighter!index-1 ()
  (should (equal (flku--make-highlighter 1)
                 '(1 'font-lock-keyword-face))))

(ert-deftest flku-test--make-highlighter!facespec-symbol ()
  (should (equal (flku--make-highlighter nil ''font-lock-variable-name-face)
                 '(0 'font-lock-variable-name-face))))

(ert-deftest flku-test--make-highlighter!facespec-list ()
  (should (equal (flku--make-highlighter nil '(face 'font-lock-keyword-face))
                 '(0 (face 'font-lock-keyword-face)))))


;; Tests for flku--highlighterp

(ert-deftest flku-test--highlighterp!nil ()
  (should (equal (flku--highlighterp nil) nil)))

(ert-deftest flku-test--highlighterp!string ()
  (should (equal (flku--highlighterp "foo") nil)))

(ert-deftest flku-test--highlighterp!subexp-highlighter ()
  (should (equal (flku--highlighterp '(0 'font-lock-keyword-face)) t)))

(ert-deftest flku-test--highlighterp!anchored-highlighter ()
  (should (equal (flku--highlighterp '("foo" nil nil)) t)))


;; Tests for flku--canonicalise-keyword

(ert-deftest flku-test--canonicalise-keyword!string ()
  (let ((keyword "foo"))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" (0 'font-lock-keyword-face))))))

(ert-deftest flku-test--canonicalise-keyword!function ()
  (let ((keyword "foo"))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" (0 'font-lock-keyword-face))))))

(ert-deftest flku-test--canonicalise-keyword!matcher-subexp-0 ()
  (let ((keyword '("foo" . 0)))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" (0 'font-lock-keyword-face))))))

(ert-deftest flku-test--canonicalise-keyword!matcher-subexp-1 ()
  (let ((keyword '("foo" . 1)))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" (1 'font-lock-keyword-face))))))

(ert-deftest flku-test--canonicalise-keyword!matcher-facespec-symbol ()
  (let ((keyword '("foo" . 'font-lock-variable-name-face)))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" (0 'font-lock-variable-name-face))))))

(ert-deftest flku-test--canonicalise-keyword!matcher-facespec-list ()
  (let ((keyword '("foo" . '(face 'font-lock-keyword-face))))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" (0 '(face 'font-lock-keyword-face)))))))

(ert-deftest flku-test--canonicalise-keyword!matcher-subexp-highlighter ()
  (let ((keyword '("foo" 1 'font-lock-keyword-face)))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" (1 'font-lock-keyword-face))))))

(ert-deftest flku-test--canonicalise-keyword!matcher-anchored-highlighter ()
  (let ((keyword '("foo" "bar" nil nil '(0 'font-lock-keyword-face))))
    (should (equal (flku--canonicalise-keyword keyword)
                   '("foo" ("bar" nil nil '(0 'font-lock-keyword-face)))))))

(ert-deftest flku-test--canonicalise-keyword!matcher-highlighters ()
  (let ((keyword '("foo" (0 'font-lock-keyword-face)
                   ("bar" nil nil '(0 'font-lock-keyword-face)))))
    (should (equal (flku--canonicalise-keyword keyword) keyword))))


;; Tests for flku--build-new-keywords

(ert-deftest flku-test--build-new-keywords!empty-keywords ()
  (should (equal (flku--build-new-keywords () 'ignore) ())))

(ert-deftest flku-test--build-new-keywords!unmodified ()
  (should (equal (flku--build-new-keywords '("foo" "bar") 'identity)
                 '("foo" "bar"))))

(ert-deftest flku-test--build-new-keywords!func-returns-nil ()
  (should (equal (flku--build-new-keywords '("foo" "bar") 'ignore) ())))


;; Tests for flku--filter-highlighter-exclude-face

(ert-deftest flku-test--filter-highlighter-exclude-face!subexp-wrong-face ()
  (let ((highlighter '(0 'font-lock-variable-name-face)))
    (should (equal (flku--filter-highlighter-exclude-face 'font-lock-keyword-face highlighter)
                   highlighter))))

(ert-deftest flku-test--filter-highlighter-exclude-face!subexp-matching-face ()
  (let ((highlighter '(0 'font-lock-keyword-face)))
    (should (equal (flku--filter-highlighter-exclude-face 'font-lock-keyword-face highlighter)
                   nil))))

(ert-deftest flku-test--filter-highlighter-exclude-face!anchored-subexp-wrong-face ()
  (let ((highlighter '("foo" nil nil (0 'font-lock-variable-name-face))))
    (should (equal (flku--filter-highlighter-exclude-face 'font-lock-keyword-face highlighter)
                   highlighter))))

(ert-deftest flku-test--filter-highlighter-exclude-face!anchored-subexp-matching-face ()
  (let ((highlighter '("foo" nil nil (0 'font-lock-keyword-face))))
    (should (equal (flku--filter-highlighter-exclude-face 'font-lock-keyword-face highlighter)
                   nil))))


;; Tests for flku--filter-exclude-face

(ert-deftest flku-test--filter-exclude-face!no-face ()
  (let ((keyword "foo"))
    (should (equal (flku--filter-exclude-face 'font-lock-variable-name-face keyword)
                   keyword))))

(ert-deftest flku-test--filter-exclude-face!no-face-exclude-keyword-face ()
  (let ((keyword "foo"))
    (should (equal (flku--filter-exclude-face 'font-lock-keyword-face keyword)
                   nil))))

(ert-deftest flku-test--filter-exclude-face!wrong-face ()
  (let ((keyword '("foo" . 'font-lock-variable-name-face)))
    (should (equal (flku--filter-exclude-face 'font-lock-keyword-face keyword)
                   keyword))))

(ert-deftest flku-test--filter-exclude-face!matching-face ()
  (let ((keyword '("foo" . 'font-lock-keyword-face)))
    (should (equal (flku--filter-exclude-face 'font-lock-keyword-face keyword)
                   nil))))

(ert-deftest flku-test--filter-exclude-face!subexp-wrong-face ()
  (let ((keyword '("foo" 0 'font-lock-variable-name-face)))
    (should (equal (flku--filter-exclude-face 'font-lock-keyword-face keyword)
                   keyword))))

(ert-deftest flku-test--filter-exclude-face!subexp-matching-face ()
  (let ((keyword '("foo" 0 'font-lock-keyword-face)))
    (should (equal (flku--filter-exclude-face 'font-lock-keyword-face keyword)
                   nil))))


;; Tests for flku-remove-variable-name-keywords

;; TODO
