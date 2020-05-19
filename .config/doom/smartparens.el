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
 "s-w" #'sp-wrap-round
 "s-)" #'sp-wrap-round
 "s-]" #'sp-wrap-square
 "s-}" #'sp-wrap-curly
 "s-W" #'sp-unwrap-sexp
 "s-s" #'sp-forward-slurp-sexp
 "s-S" #'sp-backward-slurp-sexp
 "s-b" #'sp-forward-barf-sexp
 "s-B" #'sp-backward-barf-sexp
 "s-r" #'sp-raise-sexp
 "s-t" #'sp-split-sexp
 "s-T" #'sp-join-sexp
 "s-o" #'+private/open-below-sexp-or-line)
