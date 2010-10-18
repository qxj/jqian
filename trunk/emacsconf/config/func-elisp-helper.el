;; -*- coding: utf-8 -*-
;; attempt to load a feature/library, failing silently
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)
    (error
     (progn
       (message "Error for library `%s'... Unknown" feature)
       (add-to-list 'missing-packages-list feature 'append)
       (sleep-for 1))
     nil)))

(defun my-command-exist-p (cmd)
  "Check whether command exists in `exec-path'."
  (catch 'loop
    (dolist (path exec-path)
      (if (file-exists-p (expand-file-name cmd path))
              (throw 'loop t)))
      nil))

(defun ywb-generate-loaddefs ()
  (interactive)
  (require 'autoload)
  (with-temp-buffer
    (dolist (file
             (append
              (directory-files my-config-dir t "func-.*.el$")
              (directory-files (expand-file-name "contrib" my-site-lisp-dir) t ".*.el$")
              (directory-files (expand-file-name "goodies" my-site-lisp-dir) t ".*.el$")))
      (unless (file-directory-p file)
        (generate-file-autoloads file)))
    (write-region (point-min) (point-max) "~/.emacs.d/config/100-loaddefs.el")))

(defmacro my (&rest args)
  `(mapc 'make-local-variable ',args))
