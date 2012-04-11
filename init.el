(if (not load-file-name)
    (error "Load me by M-x load-file RET"))

;;; predefine directories
(defconst my-startup-dir (file-name-directory load-file-name)
  "Emacs configuration path, default is `~/.emacs.d/'")
(defconst my-config-dir (expand-file-name "config" my-startup-dir)
  "Emacs customization settings, default is `~/.emacs.d/config'")
(defconst my-site-lisp-dir (expand-file-name "site-lisp" my-startup-dir)
  "Useful lisp projects and products, default is `~/.emacs.d/site-lisp'")
(defconst my-template-dir (expand-file-name "templates" my-startup-dir)
  "Default templates for creating different files")
(defconst my-snippet-dir (expand-file-name "snippets" my-startup-dir)
  "snippets from YASnippets")
(defconst my-org-dir (expand-file-name "~/Dropbox/Notes/")
   "Put my temporary notes here, eg: in dropbox directory")
(defconst my-temp-dir (expand-file-name "~/.tmp-emacs")
   "Temporary directory to store autosave, desktop, session, backup files.")

;;; Load-path
(add-to-list 'load-path my-config-dir)
(add-to-list 'load-path my-site-lisp-dir)
(let ((default-directory my-site-lisp-dir)) (load "subdirs.el"))

(unless (require 'dot-emacs-helper nil t)
  (error "missing dot-emacs-helpler.el"))

;;; ready to load my configurations
(mapc 'load (directory-files my-config-dir t "^[0-9]+-.*.elc?$"))
(server-start)
