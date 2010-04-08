;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; org-cfg.el --- ilisp settings
;; Time-stamp: <2010-04-08 10:35:44 Thursday by jqian>

(require 'org)
(require 'org-mouse)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-prefix-command 'org-mode-map-prefix)
(global-set-key (kbd "C-c o") 'org-mode-map-prefix)

(dolist (map (list global-map))
  (apply-define-key
   map
   `(("C-c o l" org-store-link)
     ("C-c o a" org-agenda)
     ("C-c o b" org-iswitchb)
     ("C-c o r" org-remember))))


;;(setq org-agenda-include-diary t) ; contain calendar
;;(setq org-log-done t)
(setq org-log-done 'note) ; completed task notes
(setq org-log-done 'time) ; time stamp

(setq org-hide-leading-stars t)
(setq org-startup-folded nil)           ; don't fold org items

;; (setq org-agenda-files my-org-dir) ;; cause Shift-Right issue

(defcustom org-export-html-style
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"wheer.css\">" ""
  :group 'org-export-html
  :type 'string)

(setq org-todo-keywords
      '((sequence  "TODO(t)"  "WAIT(w@/!)" "START(s!)" "|" "CANCEL(c@/!)" "DONE(d!)")))

;; OrgMode & Remember
(unless is-after-emacs-23
  (add-to-list 'load-path
               (concat my-site-lisp-dir "remember-2.0")))
(require 'remember)

;;; (org-remember-insinuate)
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

;;test
;;(require 'org-export-freemind-install)

(setq org-agenda-sorting-strategy
      '((agenda priority-down time-up)
        (todo priority-down category-keep)
        (tags priority-down category-keep)))
(setq org-todo-keyword-faces
      '(("WAIT" . (:foreground "gray" :weight bold))))
