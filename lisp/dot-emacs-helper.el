;;; dot-emacs-helper.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Julian Qian

;; Author: Julian Qian <junist@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'use-package)

(defvar deh--sections nil)

(defmacro deh-package (name &rest args)
  "Besides `use-package', it records required packages into
`deh--secionts'."
  (declare (indent 1))
  `(deh-use-package ,name t ,@args))

(defmacro deh-section (name &rest args)
  "A section placeholder to arrange lisp codes."
  (declare (indent 1))
  `(deh-use-package ,name nil ,@args))

(defmacro deh-use-package (name is-package &rest args)
  (declare (indent 1))
  (list 'progn
        ;; won't override previous setting
        `(let ((name (symbol-name ',name)))
           (unless (assoc-string name deh--sections)
             (if ,load-file-name
                 (add-to-list 'deh--sections
                              (cons name ,load-file-name)))))
        (if is-package
            `(use-package ,name ,@args)
          `(progn ,@args))))

(defun deh-locate (name)
  "Locate package configuration by name."
  (interactive
   (list (funcall (if (fboundp 'ido-completing-read)
                      'ido-completing-read 'completing-read)
                  "Locate package: " deh--sections)))
  (let ((section (assoc-string name deh--sections))
        done)
    (if (and section
             (cdr section)
             (file-exists-p (cdr section)))
        (progn
          (find-file (cdr section))
          (goto-char (point-min))
          (setq done t)
          (re-search-forward
           (concat "(\\s-*\\(deh-package\\|deh-section\\)\\s-+" (regexp-quote  (car section))))
          (recenter-top-bottom 0)))
    (unless done
      (message "Failed to locate package %s." name))))

(defconst deh-font-lock-keywords
  '(("(\\(deh-package\\|deh-section\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode deh-font-lock-keywords)

(defmacro deh-add-hook (hook &rest forms)
  "Apply some functions for a hook.

Example:
  (deh-add-hook (c-mode-common-hook text-mode-hook)
    (flyspell-prog-mode)
    (auto-fill-mode 1))

  (deh-add-hook c-mode-common-hook flymake-minor-mode)
"
  (declare (indent 1))
  (let ((hooks (if (listp hook) hook (list hook))))
    (nconc (list 'progn)
           (mapcar (lambda (hk)
                     (list 'add-hook (list 'quote hk)
                           (if (listp (car forms))
                               `(lambda nil ,@forms)
                             (list 'quote (car forms)))))
                   hooks))))

(defmacro deh-remove-hook (hook &rest forms)
  "Remove some functions for a hook. see examples of `deh-add-hook'."
  (declare (indent 1))
  (let ((hooks (if (listp hook) hook (list hook))))
    (nconc (list 'progn)
           (mapcar (lambda (hk)
                     (list 'remove-hook (list 'quote hk)
                           (if (listp (car forms))
                               `(lambda nil ,@forms)
                             (list 'quote (car forms)))))
                   hooks))))

(defsubst deh--stringfy (name)
  (if (symbolp name) (symbol-name name) name))

(defmacro deh-after-load (filename &rest forms)
  "A syntax suger for `eval-after-load' expression."
  (declare (indent 1))
  `(eval-after-load ,(deh--stringfy filename)
     '(progn ,@forms)))

;; (defalias 'bind-keys 'deh-define-key)

(provide 'dot-emacs-helper)
;;; dot-emacs-helper.el ends here
