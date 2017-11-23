(defvar dj-mode-hook nil)
(defvar dism-mode-hook nil)

(defgroup ligatti-languages nil "Options for ligatti-langs.el.")
(defcustom dj-mode-indentation 2 "Indentation for DJ files"
  :type 'integer
  :group 'ligatti-languages)
(defcustom dism-mode-comment-gutter 40 "Comment gutter for DISM files"
  :type 'integer
  :group 'ligatti-languages)

(defvar dj-mode-font-lock-keywords
  '(("\\_<\\(?:if\\|else\\|while\\|main\\|class\\|extends\\|new\\)\\_>" . font-lock-keyword-face)
    ("\\_<\\(?:printNat\\|readNat\\|this\\)\\_>" . font-lock-builtin-face)
    ("\\_<\\(?:Object\\|nat\\)\\_>" . font-lock-type-face)
    ("\\_<null\\_>" . font-lock-constant-face)
    ("\\_<\\([A-Za-z][A-Za-z0-9_]*\\)\\s-+\\([A-Za-z][A-Za-z0-9_]*\\)\\s-*"
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face))
    ("\\_<[A-Za-z][A-Za-z0-9_]*\\s-+\\([A-Za-z][A-Za-z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face t)
    ("\\_<\\(new\\|class\\|extends\\)\\s-+\\([A-Za-z][A-Za-z0-9_]*\\)\\_>" 2 font-lock-type-face t)
    ("\\(//\\)\\(.*\\)" (1 font-lock-comment-delimiter-face) (2 font-lock-comment-face t)))
  "font-lock keywords for dj-mode")

(defvar dism-mode-font-lock-keywords
  `(("\\(?:^\\|^\\(#[A-Za-z0-9]+\\)\\s-*:\\)\\s-*\\([a-z]+\\)\\s-"
     (1 font-lock-function-name-face)
     (2 font-lock-keyword-face)))
  "font-lock keywords for dism-mode")

(defvar dj-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Identifiers
    (modify-syntax-entry '(?0 . ?9) "w" st)
    (modify-syntax-entry '(?A . ?Z) "w" st)
    (modify-syntax-entry '(?a . ?z) "w" st)
    (modify-syntax-entry ?_ "_" st)

    ;; Operators
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?= "." st)

    (modify-syntax-entry ?/ "! 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for dj-mode")

(defvar dism-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for dism-mode")

(defun dj-mode-indent-line ()
  (interactive)
  (cond
   ((save-excursion
      (beginning-of-line)
      (looking-at-p "\\s-*}"))
    (indent-line-to (* dj-mode-indentation (1- (car (syntax-ppss))))))
   ((= 0 (current-column))
    (indent-line-to (* dj-mode-indentation (car (syntax-ppss)))))
   (t
    (save-excursion
      (beginning-of-line)
      (indent-line-to (* dj-mode-indentation (car (syntax-ppss))))))))

(defun dj-mode-newline-and-indent ()
  (interactive)
  (cond
   ((and (= ?\{ (char-before)) (= ?\} (char-after)))
    (electric-indent-just-newline 2)
    (dj-mode-indent-line)
    (forward-line -1)
    (dj-mode-indent-line))
   (t (newline-and-indent))))

(defvar dj-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'dj-mode-newline-and-indent)
    map))


(defvar dism-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";" 'dism-mode-comment)
    map))

(defun dism-mode-indent-line ()
  (interactive)
  (beginning-of-line)
  (indent-line-to 0)
  (cond
   ((looking-at "^\\(?:#[A-Za-z0-9]+\\)"))))

(defun dism-mode-comment (&optional n)
  (interactive "p")
  (cond
   ((save-excursion (looking-back "^\\s-+")) ;; user has manually indented
    (self-insert-command n))
   ((save-excursion (looking-back "^\\s-*;")) ;; inside comment gutter comment
    (indent-line-to 0)
    (end-of-line)
    (self-insert-command n))
   ((elt (syntax-ppss) 4) ;; inside any comment
    (self-insert-command n))
   ((save-excursion (search-backward-regexp "^[A-Za-z0-9#: -]+\\(;\\)" nil t))
    (end-of-line)
    (let* ((col (save-excursion
                  (goto-char (match-end 1))
                  (current-column)))
           (spaces (max 0 (- col (current-column) 1))))
      (insert-char ?  spaces)
      (self-insert-command n)))
   (t (if (< (current-column) dism-mode-comment-gutter)
          (move-to-column dism-mode-comment-gutter t))
      (self-insert-command n))))


(defun dj-mode ()
  "Major mode for Diminished Java"
  (interactive)
  (kill-all-local-variables)

  (delay-mode-hooks
    (prog-mode))

  (set-syntax-table dj-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'dj-mode-indent-line)
  (set (make-local-variable 'font-lock-defaults) '(dj-mode-font-lock-keywords))
  (use-local-map dj-mode-map)

  (setq major-mode 'dj-mode)
  (setq mode-name "DJ")
  (run-mode-hooks))

(defun dism-mode ()
  "Major mode for Diminished Instruction Set Machine language"
  (interactive)
  (kill-all-local-variables)

  (delay-mode-hooks
    (prog-mode))

  (set-syntax-table dism-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'dism-mode-indent-line)
  (set (make-local-variable 'font-lock-defaults) '(dism-mode-font-lock-keywords))
  (use-local-map dism-mode-map)

  (setq major-mode 'dism-mode)
  (setq mode-name "DISM")
  (run-mode-hooks))

(provide 'ligatti-langs)
