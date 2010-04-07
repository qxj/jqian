;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; company-cfg.el --- company settings
;; Time-stamp: <2010-01-28 15:44:02 julian>

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/company")

(autoload 'company-mode "company" nil t)

(add-hook 'c-mode-hook 
          ' (lambda () 
              (company-mode)))
(add-hook 'c++-mode-hook 
          ' (lambda () 
              (company-mode)))
(add-hook 'php-mode-hook 
          ' (lambda () 
              (company-mode)))
(add-hook 'javascript-mode-hook 
          ' (lambda () 
              (company-mode)))
(add-hook 'html-mode-hook 
          ' (lambda () 
              (company-mode)))
(add-hook 'java-mode-hook 
          ' (lambda () 
              (company-mode)))

;; Emacs lisp keywords completions
(defun company-elisp-finder-keyword-backend (command &optional arg &rest ign)
  "A `company-backend' for finder-keywords."
  (case command
    ('prefix
     (and (require 'finder nil t)
          (or (company-grab ":group '\\(\\(\\sw\\|\\s_\\)*\\)" 1)
              (company-grab "Keywords:.*[ \t]+\\(\\(\\sw\\|\\s_\\)*\\)" 1))))
    ('candidates (all-completions arg finder-known-keywords))
    ('meta (cdr (assoc (intern arg) finder-known-keywords)))))


