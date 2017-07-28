(require 'smartparens)

(defun py-sp-unwrap-sexp ()
  (interactive)
  (sp-backward-slurp-sexp)
  (sp-splice-sexp-killing-backward 1))

(defun py-sp-raise-sexp ()
  (interactive)
  (sp-backward-slurp-sexp)
  (sp-raise-sexp))

;;;###autoload
(define-minor-mode python-smartparens-fixes-mode
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC k W") 'py-sp-unwrap-sexp)
            (define-key map (kbd "SPC k r") 'py-sp-raise-sexp)
            map))

(provide 'python-smartparens-fixes)
