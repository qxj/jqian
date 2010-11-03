(deh-section-if "org"
  "~/src/org-7.01h"
  ;; load-path
  (add-to-list 'load-path (expand-file-name "lisp" deh-this-path))
  (add-to-list 'load-path (expand-file-name "contrib/lisp" deh-this-path))
  ;; load required org libraries
  (require 'org-install)

  (setq org-CUA-compatible t)
  (defun my-find-file-function (file)
    "find file according to the file extension."
    (funcall (or (assoc-default file ywb-dired-guess-command-alist
                                'string-match)
                 'find-file) file))
  (deh-add-hook org-load-hook
    (add-to-list 'org-link-frame-setup
                 '(file . my-find-file-function)))
  (deh-add-hook org-mode-hook
    (org-set-local 'comment-start "#+COMMENT:")
    (toggle-truncate-lines nil)
    (auto-fill-mode 1)
    (outline-minor-mode t))

  ;; org keybinds
  (deh-local-set-key org-mode-hook
    ((kbd "C-c o l") . 'org-store-link)
    ((kbd "C-c o a") . 'org-agenda)
    ((kbd "C-c o b") . 'org-iswitchb)
    ((kbd "C-c o r") . 'org-remember)
    ;;## Org Keybinds Reminds ;;;;;;;;;;;;;;;;;;;
    ;; ((kbd "C-c C-b") . 'org-beamer-select-environment)
    ;; ((kbd "C-c C-x p") . 'org-set-property)
    ;; ((kbd "C-c /") . 'org-sparse-tree)
    ;; ((kbd "C-c C-x C-c") . 'org-columns)
    ;; ((kbd "C-c C-x C-l") . 'org-preview-latex-fragment)
    ;; ((kbd "C-c C-e") . 'org-export)
    ;; ((kbd "C-c C-a") . 'org-attach)
    )

  (deh-local-set-key outline-minor-mode-hook
    ((kbd "C-c o s") . 'show-entry)
    ((kbd "C-c o S") . 'show-all)
    ((kbd "C-c o h") . 'hide-entry)
    ((kbd "C-c o H") . 'hide-body))

  ;; (setq org-agenda-include-diary t) ; contain calendar
  ;; (setq org-log-done t)
  ;; (setq org-log-done 'note) ; completed task notes
  (setq org-log-done 'time) ; time stamp

  (setq org-hide-leading-stars t
        org-startup-folded nil)           ; don't fold org items after load

  ;; (setq org-agenda-files my-org-dir) ; cause Shift-Right issue

  (setq org-todo-keywords
        '((sequence  "TODO(t)"  "WAIT(w@/!)" "START(s!)" "|" "CANCEL(c@/!)" "DONE(d!)")))

  ;; Single keys to execute commands at the beginning of a headline
  (setq org-use-speed-commands t
        org-export-with-sub-superscripts nil
        org-file-apps-defaults-gnu '((t . emacs)))

  ;; Remember Settings
  ;; (org-remember-insinuate)
  (setq org-directory my-org-dir
        org-default-notes-file (concat my-org-dir "Notes.org")
        org-remember-templates
        '(("Tasks"  ?t  "* TODO %^{Title} %^g\n       %?     %i\n"
           "Task.org"  "New task")
          ("Personal"  ?g  "* %^{Title} %^g\n       %?     %i\n   Reference: %a"
           "Personal.org"  "New arrangement")
          ("Journal"  ?d  "* %u %^{Title}\n  %?\n   %i\n\n     Reference: %a"
           "Journal.org")
          ("Study"  ?x  "* %u %^{Title}\n  %?\n   %i\n\n     Reference: %a"
           "Study.org" "New item")
          ("Project"  ?s  "* %^{Title}\n  %?\n   %i\n\n     Reference: %a"
           "Project.org"  "New resource")))

  ;; If you are, like me, missing the function org-remember-insinuate, try the following
  (setq remember-annotation-functions '(org-remember-annotation)
        remember-handler-functions '(org-remember-handler))
  (add-hook 'remember-mode-hook 'org-remember-apply-template)

  ;;(require 'org-export-freemind-install)

  (setq org-agenda-sorting-strategy
        '((agenda priority-down time-up)
          (todo priority-down category-keep)
          (tags priority-down category-keep)))

      ;;;# export org documents to latex & pdf
  (deh-require 'org-latex
    (setq org-latex-to-pdf-process
          '("xelatex -interaction nonstopmode %s"
            "xelatex -interaction nonstopmode %s"))
    ;; org + beamer = owesome slides
    (setq org-export-latex-default-packages-alist ; for xelatex
          '(("cm-default" "fontspec" t) ; provides font selecting commands
            ("" "xunicode" t)      ; provides unicode character macros
            ("" "xltxtra" t)       ; provides some fixes/extras
            ("" "indentfirst" t)
            ("english" "babel" t)
            ("AUTO" "inputenc" t)
            ("" "color" t)
            ;;# donot need unicode option
            ("" "hyperref" t)
            ;; ("pdftex" "graphicx" t)
            ;;# listings for source code exporting
            ("" "listings" t)
            ("" "xcolor" t)
            ("" "fancyvrb" t)
            "\\lstset{
   fancyvrb=true,
   %% language=C++,
   basicstyle=\\ttfamily,
   stringstyle=\\ttfamily\\color{green!50!black},
   keywordstyle=\\color{blue}\\bfseries,
   commentstyle=\\color{red!50!black}\\itshape,
   showspaces=false,
   showstringspaces=true,
   fontadjust=true,
   keepspaces=true,
   flexiblecolumns=true,
   frame=single,
   upquote=true
}"
            "\\setmainfont{AR PL ShanHeiSun Uni}"
            "\\setsansfont[BoldFont=AR PL ZenKai Uni]{AR PL ZenKai Uni}"
            "\\setmonofont[BoldFont=DejaVu Sans Mono]{DejaVu Sans Mono}"
            "\\defaultfontfeatures{Mapping=tex-text}"
            "\\XeTeXlinebreaklocale \"zh\""
            "\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt"
            "\\tolerance=1000"))
    ;; Only 2 level headlines will be exported as frames
    ;; (setq org-export-headline-levels 2)
    ;; During HTML export, convert latex fragment
    (setq org-export-with-LaTeX-fragments t)
    ;; fontify source code with listings
    (setq org-export-latex-listings t)
    )

      ;;;# export org documents to html
  (setq org-export-html-inline-images t
        org-export-html-with-timestamp t)

  (setq org-export-html-style
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"wheer.css\">")

  ;; Compatible with yasnippet.el
  (if (featurep 'yasnippet)
      (deh-add-hook org-mode-hook
        (org-set-local 'yas/trigger-key [tab])
        (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)
        (define-key yas/keymap (kbd "M-j") 'yas/next-field-or-maybe-expand)))
  )


(deh-section "rst"
  (add-hook 'rst-adjust-hook 'rst-toc-update)
  ;; Auto fill and outline mode
  (deh-add-hook rst-mode-hook
    (auto-fill-mode 1)
    (outline-minor-mode t)))
