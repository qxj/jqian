
(deh-section-path "org-export"
  "~/src/org-mode"
  ;; load-path
  (add-to-list 'load-path (expand-file-name "lisp" deh-this-path))
  (add-to-list 'load-path (expand-file-name "contrib/lisp" deh-this-path))
  ;; load required org libraries
  (require 'org-install)

  ;;# export org documents to latex & pdf
  (deh-require 'org-latex
    (setq org-latex-to-pdf-process
          '("xelatex -interaction=nonstopmode -output-directory=%o %f"
            "xelatex -interaction=nonstopmode -output-directory=%o %f"))
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
            "\\setmainfont[BoldFont=DejaVu Serif]{DejaVu Serif}"
            "\\setsansfont[BoldFont=DejaVu Sans]{DejaVu Sans}"
            "\\setmonofont[BoldFont=DejaVu Sans Mono]{DejaVu Sans Mono}"
            "\\defaultfontfeatures{Mapping=tex-text}"
            "\\XeTeXlinebreaklocale \"zh\""
            "\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt"
            "\\tolerance=1000"))
    ;; Only 2 level headlines will be exported as frames
    ;; (setq org-export-headline-levels 2)
    ;; During HTML export, convert latex fragment
    (setq org-export-with-LaTeX-fragments t)

    ;; (setq org-export-run-in-background t) ; buggy

    ;;# fontify source code with listings
    (setq org-export-latex-listings t)
    ;; (add-to-list 'org-export-latex-packages-alist '(\"\" \"minted\"))
    ;; (setq org-export-latex-listings 'minted)

    (defalias 'C-mode 'c-mode)
    )

  ;;# export org documents to html
  (deh-section "org-html"
    (setq org-export-html-inline-images t
          org-export-html-with-timestamp t)

    (setq org-export-html-style
          "<link rel=\"stylesheet\" type=\"text/css\" href=\"wheer.css\">"))

  ;;(require 'org-export-freemind-install)
  )

(deh-require 'org
  (setq org-CUA-compatible t)

  (setq org-directory my-org-dir
        org-default-notes-file (concat my-org-dir "Notes.org"))

  ;; Single keys to execute commands at the beginning of a headline
  (setq org-use-speed-commands t
        ;; org-special-ctrl-k t
        org-special-ctrl-a/e 'reserved
        org-export-with-sub-superscripts nil
        org-file-apps-defaults-gnu '((t . emacs)))

  (deh-add-hook org-load-hook
    (add-to-list 'org-link-frame-setup
                 '(file . my-find-file-function)))
  (defun my-find-file-function (file)
    "find file according to the file extension."
    (funcall (or (assoc-default file ywb-dired-guess-command-alist
                                'string-match)
                 'find-file) file))

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
    ((kbd "C-c o o") . 'org-open-at-point) ; "\C-c\C-o"
    ((kbd "C-c o j") . 'org-open-at-point) ; "\C-c\C-o"
    ("\M-\C-l" . 'org-table-sort-lines)
    ("\M-\C-w" . 'org-table-copy-region)
    ("\M-\C-y" . 'org-table-paste-rectangle)
    ("\M-I" . 'org-toggle-iimage-in-org)
    ;;## Org Keybinds Reminds ;;;;;;;;;;;;;;;;;;;
    ;; ((kbd "C-c C-b") . 'org-beamer-select-environment)
    ;; ((kbd "C-c C-x p") . 'org-set-property)
    ;; ((kbd "C-c /") . 'org-sparse-tree)
    ;; ((kbd "C-c C-x C-c") . 'org-columns)
    ;; ((kbd "C-c C-x C-l") . 'org-preview-latex-fragment)
    ;; ((kbd "C-c C-e") . 'org-export)
    ;; ((kbd "C-c C-a") . 'org-attach)
    )

  (defun org-toggle-iimage-in-org ()
    "Display images in your org file."
    (interactive)
    (if (face-underline-p 'org-link)
        (set-face-underline-p 'org-link nil)
      (set-face-underline-p 'org-link t))
    (iimage-mode))

  ;;# Compatible with yasnippet.el
  (if (featurep 'yasnippet)
      (deh-add-hook org-mode-hook
        (org-set-local 'yas/trigger-key [tab])
        (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)
        (define-key yas/keymap (kbd "M-j") 'yas/next-field-or-maybe-expand)))
  )

(deh-section "org-agenda"
  ;; (setq org-agenda-include-diary t) ; contain calendar
  ;; (setq org-log-done t)
  ;; (setq org-log-done 'note) ; completed task notes
  (setq org-log-done 'time) ; time stamp

  (setq org-hide-leading-stars t
        org-startup-folded nil)           ; don't fold org items after load

  ;; (setq org-agenda-files my-org-dir) ; cause Shift-Right issue

  (setq org-todo-keywords
        '((sequence  "TODO(t)"  "WAIT(w@/!)" "START(s!)" "|" "CANCEL(c@/!)" "DONE(d!)")))

  (setq org-agenda-sorting-strategy
        '((agenda priority-down time-up)
          (todo priority-down category-keep)
          (tags priority-down category-keep))))

(deh-require-if 'org-capture
  ;; org-capture supersedes org-remember
  (>= (string-to-int org-version) 7.5)
  (setq org-capture-templates
        '(("a" "Appointments" entry
           (file+headline "taskdiary.org" "Calendar")
           "* APPT %? %^g
    %i
    Added: %U")
          ("n" "Notes" entry
           (file+datetree "taskdiary.org")
           "* %? %^g
    %i")
          ("t" "TODO List" entry
           (file+datetree "taskdiary.org")
           "* TODO %? %^g
    %i")
          ("j" "Work Journal" entry
           (file+datetree "workjournal.org")
           "** %? %^g
    %i")
          ("s" "Source Code" entry
           (file+function "codereview.org"
                          (lambda ()
                            ;; only append to the end
                            (if (org-list-search-backward
                                 (format "\\* %s"
                                         (file-name-nondirectory
                                          (buffer-file-name (current-buffer))))
                                 nil t)
                                (org-end-of-item))))
           "* %f
    %i%?
    Reference: %a")
          )))

(deh-section "rst"
  (add-hook 'rst-adjust-hook 'rst-toc-update)
  ;; Auto fill and outline mode
  (deh-add-hook rst-mode-hook
    (auto-fill-mode 1)
    (outline-minor-mode t)))
