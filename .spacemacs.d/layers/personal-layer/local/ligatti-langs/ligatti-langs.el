
(defvar dj-mode-hook nil)
(defvar dism-mode-hook nil)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dj\\'" . dj-mode))
(add-to-list 'auto-mode-alist '("\\.dism\\'" . dism-mode))

(defvar dj-font-lock-keywords
  (list (cons (regexp-opt '("if" "while" "class" "extends" "new ")) font-lock-keyword-face)
        (cons (regexp-opt '("printNat" "readNat" "this" "main")) font-lock-builtin-face)
        (cons "nat" font-lock-type-face)))

(defun dj-indent-line ()
  "Indent current line in DJ code"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          cur-indent)
      (if (looking-at "^\s*}")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) default-tab-width)))))
      (save-excursion
        (while not-indented
          (forward-line -1)
          (if (looking-at "^\s*}")
              (progn
                (setq cur-indent (current-indentation))
                (setq not-indented nil))
            (if (looking-at "{")
                (progn
                  (setq cur-indent (+ (current-indentation) default-tab-width))
                  (setq not-indented nil))
              (if (bobp)
                  (setq not-indented nil))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defvar dj-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    st)
  "Syntax table for dj-mode")

(defun dj-mode ()
  "Major mode for Diminished Java"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dj-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(dj-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'dj-indent-line)
  (setq major-mode 'dj-mode)
  (setq mode-name "DJ")
  (run-hooks 'dj-mode-hook))


(provide 'ligatti-langs)
