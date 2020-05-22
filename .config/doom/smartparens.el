;;; ~/.config/doom/smartparens.el -*- lexical-binding: t; -*-

(defun +private/open-below-sexp-or-line ()
  (interactive)
  (let ((current-line (line-number-at-pos))
        (close-paren-line (save-excursion (evil-next-close-paren) (line-number-at-pos))))
    (print (list current-line close-paren-line))
    (if (= current-line close-paren-line)
        (progn
          (evil-next-close-paren)
          (evil-insert 1)
          (newline-and-indent))
      (evil-open-below 1))))

(map!
 :prefix ("gk" . "smartparens")
 :n "w" #'sp-wrap-round
 :n ")" #'sp-wrap-round
 :n "]" #'sp-wrap-square
 :n "}" #'sp-wrap-curly
 :n "W" #'sp-unwrap-sexp
 :n "s" #'sp-forward-slurp-sexp
 :n "S" #'sp-backward-slurp-sexp
 :n "b" #'sp-forward-barf-sexp
 :n "B" #'sp-backward-barf-sexp
 :n "r" #'sp-raise-sexp
 :n "t" #'sp-split-sexp
 :n "T" #'sp-join-sexp
 :n "o" #'+private/open-below-sexp-or-line)
