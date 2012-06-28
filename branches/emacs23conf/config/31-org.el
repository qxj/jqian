;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

(deh-section-path "org-external"
  "~/src/org-mode"
  ;; load-path
  (add-to-list 'load-path (expand-file-name "lisp" deh-this-path))
  (add-to-list 'load-path (expand-file-name "contrib/lisp" deh-this-path))
  ;; load required org libraries
  (require 'org-install nil t)
  ;;(require 'org-export-freemind-install)
)

(deh-section-after "org"
  (setq org-CUA-compatible t)

  (setq org-directory my-org-dir
        org-default-notes-file (concat my-org-dir "Notes.org"))

  ;; Single keys to execute commands at the beginning of a headline
  (setq org-use-speed-commands nil
        ;; org-special-ctrl-k t
        org-special-ctrl-a/e 'reserved
        org-export-with-sub-superscripts nil
        org-file-apps-defaults-gnu '((t . emacs)))

  (deh-add-hook 'org-load-hook
    (add-to-list 'org-link-frame-setup
                 '(file . my-find-file-function)))
  (defun my-find-file-function (file)
    "find file according to the file extension."
    (funcall (or (assoc-default file my-dired-guess-command-alist
                                'string-match)
                 'find-file) file))

  (deh-add-hook 'org-mode-hook
    (org-set-local 'comment-start "#+COMMENT:")
    (toggle-truncate-lines nil)
    (auto-fill-mode 1)
    (outline-minor-mode t))

  ;; org keybinds
  (deh-define-key org-mode-map
    ((kbd "C-c o l")  'org-store-link)
    ((kbd "C-c o a")  'org-agenda)
    ((kbd "C-c o b")  'org-iswitchb)
    ((kbd "C-c o o")  'org-open-at-point) ; "\C-c\C-o"
    ((kbd "C-c o j")  'org-open-at-point) ; "\C-c\C-o"
    ("\M-\C-l"  'org-table-sort-lines)
    ("\M-\C-w"  'org-table-copy-region)
    ("\M-\C-y"  'org-table-paste-rectangle)
    ("\M-I"  'org-toggle-iimage-in-org)
    ;;## Org Keybinds Reminds ;;;;;;;;;;;;;;;;;;;
    ((kbd "C-c C-b")  'org-beamer-select-environment)
    ;; ((kbd "C-c C-x p")  'org-set-property)
    ;; ((kbd "C-c /")  'org-sparse-tree)
    ;; ((kbd "C-c C-x C-c")  'org-columns)
    ;; ((kbd "C-c C-x C-l")  'org-preview-latex-fragment)
    ;; ((kbd "C-c C-e")  'org-export)
    ;; ((kbd "C-c C-a")  'org-attach)
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
      (deh-add-hook 'org-mode-hook
        (org-set-local 'yas/trigger-key [tab])
        (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)
        (define-key yas/keymap (kbd "M-j") 'yas/next-field-or-maybe-expand)))
  )

(deh-section-after "org-latex"
  (setq org-latex-to-pdf-process
        '("xelatex -interaction=nonstopmode -output-directory=%o %f"
          "xelatex -interaction=nonstopmode -output-directory=%o %f"
          "xelatex -interaction=nonstopmode -output-directory=%o %f"))
  ;;# org + beamer = owesome slides
  ;; useful packages:
  ;; sudo tlmgr install wrapfig xecjk
  (setq org-export-latex-default-packages-alist
        '(("" "indentfirst" t)
          ("" "tikz" t)                 ; tikz
          ("english" "babel" t)
          ;; ("AUTO" "inputenc" t)
          ("" "color" t)
          ("" "float" t)                ; for figure placement
          ("" "wrapfig" t)
          ;;# donot need unicode option
          ("" "longtable" t)            ; for long tables
          ("" "hyperref" t)             ; for cross reference
          ;; ("pdftex" "graphicx" t)       ; works for pdflatex
          )
        org-export-latex-packages-alist
        '(;;# xelatex related packages
          ("slantfont,boldfont" "xeCJK" t)
          ("cm-default" "fontspec" t) ; provides font selecting commands
          ("" "xunicode" t)       ; provides unicode character macros
          ("" "xltxtra" t)        ; provides some fixes/extras
          ;;# listings for source code exporting
          ("" "listings" t)
          ("" "xcolor" t)
          ("" "fancyvrb" t)
          "\\lstset{
   numbers=none,
   tabsize=2,
   fancyvrb=true,
   %% language=C++,
   %% basicstyle=\\tiny\\ttfamily,
   basicstyle=\\scriptsize\\ttfamily,
   stringstyle=\\color{green!50!black},
   keywordstyle=\\color{blue}\\bfseries,
   identifierstyle=,
   commentstyle=\\color{red!50!black}\\itshape,
   showtabs=false,
   showspaces=false,
   showstringspaces=false,
   fontadjust=true,
   keepspaces=true,
   flexiblecolumns=true,
   frame=single,
   upquote=false
}"
          ;;# default font settings
          "%% \\setmainfont[BoldFont=DejaVu Serif]{DejaVu Serif}"
          "%% \\setsansfont[BoldFont=DejaVu Sans]{DejaVu Sans}"
          "%% \\setmonofont[BoldFont=DejaVu Sans Mono]{DejaVu Sans Mono}"
          "%% \\setCJKmainfont[BoldFont=WenQuanYi Micro Hei]{WenQuanYi Micro Hei}"
          "%% \\setCJKsansfont[BoldFont=WenQuanYi Micro Hei]{WenQuanYi Micro Hei}"
          "%% \\setCJKmonofont[BoldFont=WenQuanYi Micro Hei Mono]{WenQuanYi Micro Hei Mono}"
          "\\defaultfontfeatures{Mapping=tex-text}"
          "\\XeTeXlinebreaklocale \"zh\""
          "\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt"
          "\\tolerance=1000"))
  ;;# for org-preview-latex-fragment template
  (setq org-format-latex-header "\\documentclass{minimal}       % instead of article
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage[mathscr]{eucal}
\[NO-PACKAGES]
\[DEFAULT-PACKAGES]
%\\pagestyle{empty}             % do not remove
%\\usepackage{fullpage}
")

  ;; Only 2 level headlines will be exported as frames
  ;; (setq org-export-headline-levels 2)
  ;; During HTML export, convert latex fragment
  (setq org-export-with-LaTeX-fragments t)

  ;; (setq org-export-run-in-background t) ; buggy
  (setq org-export-copy-to-kill-ring nil) ; cause pasteboard error on some Mac version

  ;;# fontify source code with listings
  (setq org-export-latex-listings t)
  ;; (add-to-list 'org-export-latex-packages-alist '(\"\" \"minted\"))
  ;; (setq org-export-latex-listings 'minted)

  (defalias 'C-mode 'c-mode)
  )

(deh-section "org-html"
  (setq org-export-html-inline-images t
        org-export-html-with-timestamp t)

  (setq org-export-html-style
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"wheer.css\">"))

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

(deh-section-after "org-capture"
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

(deh-section-after "footnode"
  (defun ywb-footnote-detect-style (start end)
    "Detect `footnote-style' by counting match regexp of each style in
`footnote-style-alist', and select the style matches most."
    (let ((case-fold-search nil))
      (caar
       (sort
        (mapcar
         (lambda (footnote-style)
           (cons footnote-style
                 (count-matches
                  (concat "\n"
                          (regexp-quote footnote-start-tag)
                          (Footnote-current-regexp)
                          (regexp-quote footnote-end-tag))
                  start end nil)))
         (mapcar 'car footnote-style-alist))
        (lambda (s1 s2) (> (cdr s1) (cdr s2)))))))

  (defun ywb-footnote-rescan (&optional force)
    "Rescan footnote position in the file."
    (interactive "P")
    (when (or force
              (not (and footnote-text-marker-alist
                        footnote-pointer-marker-alist)))
      (setq footnote-text-marker-alist nil
            footnote-pointer-marker-alist nil)
      (let ((arg 1)                       ; footnote index
            (modified (buffer-modified-p))
            old-point start end match count pointer)
        (save-excursion
          (Footnote-goto-char-point-max)
          (setq end (point))
          (when (re-search-backward (concat "^" footnote-section-tag-regexp) nil t)
            (setq start (point)
                  footnote-style (ywb-footnote-detect-style start end))
            (while (and (< (point) end)
                        (re-search-forward (concat "\n\\("
                                                   (regexp-quote footnote-start-tag)
                                                   (Footnote-current-regexp)
                                                   (regexp-quote footnote-end-tag)
                                                   "\\)")
                                           nil t))
              (setq match (match-string 1))
              (forward-line 0)
              (setq old-point (point))
              ;; find footnote position in text, if the index appear
              ;; more than once, select interactively
              (save-excursion
                (goto-char (point-min))
                (setq count 0)
                (while (re-search-forward (regexp-quote match) start t)
                  (setq pointer (point)
                        count (1+ count))))
              (cond ((= count 0)
                     (setq footnote-text-marker-alist nil
                           footnote-pointer-marker-alist nil)
                     (error "No footnote found for index %s" match))
                    ((> count 1)
                     (setq pointer (ywb-footnote-select-pointer start match))))
              ;; renumber footnote
              (delete-region (point)
                             (progn (re-search-forward (regexp-quote footnote-end-tag))
                                    (point)))
              (Footnote-insert-numbered-footnote arg nil)
              (Footnote-insert-text-marker arg old-point)
              (goto-char pointer)
              (delete-region (point)
                             (progn (re-search-backward (regexp-quote footnote-start-tag))
                                    (point)))
              (Footnote-insert-pointer-marker arg (point))
              (Footnote-insert-numbered-footnote arg t)
              (setq arg (1+ arg))
              (goto-char old-point)
              (end-of-line))))
        (set-buffer-modified-p modified))))

  (defun ywb-footnote-select-pointer (end index)
    "Set message position for INDEX."
    (let ((line (buffer-substring (line-beginning-position)
                                  (min (line-end-position)
                                       (+ 30 (line-beginning-position)))))
          (overlay (make-overlay (point-min) (1+ (point-min))))
          point)
      (overlay-put overlay 'face 'match)
      (save-excursion
        (goto-char (point-min))
        (while (and (null point)
                    (re-search-forward (regexp-quote index) end t))
          (move-overlay overlay (match-beginning 0) (match-end 0))
          (if (y-or-n-p (format "Set point of \"%s\" here? " line))
              (setq point (point)))))
      (delete-overlay overlay)
      point))

  (add-hook 'footnote-mode-hook 'ywb-footnote-rescan))

(deh-section "rst"
  (add-hook 'rst-adjust-hook 'rst-toc-update)
  ;; Auto fill and outline mode
  (deh-add-hook 'rst-mode-hook
    (auto-fill-mode 1)
    (outline-minor-mode t)))
