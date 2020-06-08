;;; ~/.config/doom/org.el -*- lexical-binding: t; -*-

(require 's)

(after! org
  (require 'org-ref)
  (require 'org-ref-ivy-cite)
  (require 'org-roam-bibtex)
  (add-hook! org-roam-mode #'org-roam-bibtex-mode)
  (setq org-preview-latex-default-process 'dvisvgm)
  (add-hook! org-mode #'auto-fill-mode)
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)"))
   )
  )

(setq
 ;; org
 org-directory "~/org/"
 deft-directory org-directory
 org-image-actual-width nil
 org-log-into-drawer t
 org-hierarchical-todo-statistics nil
 org-checkbox-hierarchical-statistics nil
 ;; org-latex-pdf-process '("latexmk -pdf %f && latexmk -c %f")
 org-latex-packages-alist '(("style=numeric,backend=biber" "biblatex" nil)
                            "\\addbibresource{references.bib}"
                            ("" "listings" nil)
                            ("" "sourcecodepro" nil)
                            ("" "mathtools" nil)
                            ("" "mathabx" nil))
 org-latex-listings nil
 org-latex-listings-options '(("prebreak" "\\dlsh")
                              ("basicstyle" "\\small\\ttfamily")
                              ("breaklines" "true"))
 org-startup-with-inline-images t

 ;; org-roam
 org-roam-directory (concat (file-name-as-directory org-directory) "roam")
 org-roam-link-title-format "R:%s"
 org-roam-graph-exclude-matcher '("private" "journal")
 org-roam-graph-executable "dot"
 org-roam-graph-extra-config '(("rankdir" . "LR"))
 org-roam-graph-viewer "chromium"
 org-roam-capture-templates
 (let ((timestamp "%<%Y%m%d%H%M%S>")
       (head (s-join
              "\n"
              '("#+TITLE: ${title}"
                "#+DATE: %<%Y-%m-%d %H:%M:%S>"
                "#+STARTUP: showall"
                ""
                "- tags :: "))))
   `(("d" "default" plain #'org-roam-capture--get-point
      "%?"
      :file-name ,(concat timestamp "-${slug}")
      :head ,head
      :unnarrowed t)
     ("w" "work" plain #'org-roam-capture--get-point
      "%?"
      :file-name ,(concat "work/" timestamp "-${slug}")
      :head ,head
      :unnarrowed t)
     ("p" "private" plain #'org-roam-capture--get-point
      "%?"
      :file-name ,(concat "private/" timestamp "-${slug}")
      :head ,head
      :unnarrowed t)))

 ;; org-journal
 org-journal-dir (concat (file-name-as-directory org-directory) "journal")
 org-journal-file-header "#+TITLE: %Y-%m-%d\n#+STARTUP: showall\n\n"
 org-journal-file-type 'weekly
 org-journal-date-format "%A, %d %B %Y"
 org-journal-time-format "TODO "
 org-journal-file-format "%Y-%m-%d.org"
 org-journal-enable-agenda-integration t
 ;; org-journal-enable-cache t
 org-journal-hide-entries-p nil
 ;; carry over all TO-DO items that are not in a "done"-like state
 org-journal-carryover-items "/!"
 org-journal-search-results-order-by :desc

 ;; org-babel
 org-babel-load-languages '((python . t)
                            (shell . t)
                            (dot . t)
                            (emacs-lisp . t)
                            (lisp . t)))
 ;; org-confirm-babel-evaluate 'personal-layer/org-confirm-babel-evaluate


(setq org-ref-pdf-directory (concat (file-name-as-directory org-directory) "bib"))
(setq-all (list (concat (file-name-as-directory org-directory) "bib/main.bib"))
          reftex-default-bibliography
          org-ref-default-bibliography
          bibtex-completion-bibliography
          bibtex-completion-library-path
          ebib-file-search-dirs)
