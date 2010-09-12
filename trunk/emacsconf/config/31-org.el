(deh-section "org"
  (let ((org-path "~/src/org-7.01h"))
    (when (file-exists-p org-path)
      ;; load-path
      (add-to-list 'load-path (expand-file-name "lisp" org-path))
      (add-to-list 'load-path (expand-file-name "contrib/lisp" org-path))
      ;; load required org libraries
      (require 'org-install)

      (setq org-CUA-compatible t)
      (add-hook 'org-load-hook
                (lambda ()
                  ;; (let (org-CUA-compatible)
                  ;;   (define-key org-mode-map (org-key 'S-return)   nil)
                  ;;   (define-key org-mode-map (org-key 'S-up)       nil)
                  ;;   (define-key org-mode-map (org-key 'S-down)     nil)
                  ;;   (define-key org-mode-map (org-key 'S-left)     nil)
                  ;;   (define-key org-mode-map (org-key 'S-right)    nil))
                  (add-to-list 'org-link-frame-setup
                               '(file . my-find-file-function))))
      (add-hook 'org-mode-hook
                (lambda ()
                  (toggle-truncate-lines nil)
                  (auto-fill-mode 1)
                  (outline-minor-mode t)
                  ;; org keybinds
                  (local-set-key (kbd "C-c o l") 'org-store-link)
                  (local-set-key (kbd "C-c o a") 'org-agenda)
                  (local-set-key (kbd "C-c o b") 'org-iswitchb)
                  (local-set-key (kbd "C-c o r") 'org-remember)
                  ;; beamer keybinds
                  ;; (local-set-key (kbd "C-c C-b") 'org-beamer-select-environment)
                  ))

      (add-hook 'outline-minor-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-c o s") 'show-entry)
                  (local-set-key (kbd "C-c o S") 'show-all)
                  (local-set-key (kbd "C-c o h") 'hide-entry)
                  (local-set-key (kbd "C-c o H") 'hide-body)
                  ))

      ;;(setq org-agenda-include-diary t) ; contain calendar
      ;;(setq org-log-done t)
      (setq org-log-done 'note) ; completed task notes
      (setq org-log-done 'time) ; time stamp

      (setq org-hide-leading-stars t)
      (setq org-startup-folded nil)           ; don't fold org items after load

      ;; (setq org-agenda-files my-org-dir) ; cause Shift-Right issue

      (defcustom org-export-html-style
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"wheer.css\">" ""
        :group 'org-export-html
        :type 'string)

      (setq org-todo-keywords
            '((sequence  "TODO(t)"  "WAIT(w@/!)" "START(s!)" "|" "CANCEL(c@/!)" "DONE(d!)")))


      (setq org-export-with-sub-superscripts nil)
      (defun my-find-file-function (file)
        "find file according to the file extension."
        (funcall (or (assoc-default file ywb-dired-guess-command-alist
                                    'string-match)
                     'find-file) file))
      (setq org-file-apps-defaults-gnu '((t . emacs)))

      ;; Remember Settings
      ;; (org-remember-insinuate)
      (setq org-directory my-org-dir)
      (setq org-default-notes-file (concat my-org-dir "Notes.org"))
      (setq org-remember-templates
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
      (setq remember-annotation-functions '(org-remember-annotation))
      (setq remember-handler-functions '(org-remember-handler))
      (add-hook 'remember-mode-hook 'org-remember-apply-template)

      ;;(require 'org-export-freemind-install)

      (setq org-agenda-sorting-strategy
            '((agenda priority-down time-up)
              (todo priority-down category-keep)
              (tags priority-down category-keep)))

      ;; org + beamer = owesome slides
      (require 'org-latex)
      ;; patch org-latex.el
      ;; Add "\\begin{CJK*}{UTF8}{song}" to `org-export-latex-make-header'
      ;; Add "\\end{CJK*}" to `org-export-as-latex'

      (setq org-export-latex-default-packages-alist
            '(("" "CJKutf8" t)
              ("" "indentfirst" t)
              ("english" "babel" t)
              ("AUTO" "inputenc" t)
              ("T1" "fontenc" t)
              ("" "color" t)
              ("" "listings" t)
              ;; ("CJKbookmarks=true" "hyperref" nil)
              ;; ("pdftex" "graphicx" nil)
              "\\tolerance=1000"))
      ;; During HTML export, convert latex fragment
      (setq org-export-with-LaTeX-fragments t)
      )))


(deh-section "rst"
  (add-hook 'rst-adjust-hook 'rst-toc-update)
  ;; Auto fill and outline mode
  (add-hook 'rst-mode-hook
            (function (lambda ()
                        (auto-fill-mode 1)
                        (outline-minor-mode t)))))
