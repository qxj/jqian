;;; dot-emacs-helper.el --- Some helper functions for .emacs

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Update: Nov 3, 2010 by Julian Qian <junist@gmail.com>
;; Version: 0.02

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * Who need it?
;;  If you only write hundreds lines in .emacs, you won't consider this
;;  extension. But if you have thousands lines in .emacs, you may want
;;  such thing to organize configuration like me.
;;
;; * How to use it?
;;  Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'dot-emacs-helper)
;;
;;  I recommend write this in .emacs for port config to other emacs that
;;  don't have this extension:
;;
;;     (unless (require 'dot-emacs-helper nil t)
;;       (defmacro deh-require-maybe (feature &rest forms)
;;         (declare (indent 1))
;;         `(progn (when (require ,feature nil t) ,@forms)))
;;       (defalias 'deh-require 'deh-require-maybe)
;;       (put 'deh-require 'lisp-indent-function 1)
;;       (defmacro deh-section (section &rest forms)
;;         (declare (indent 1))
;;         `(progn ,@forms)))
;;
;;  And rewrite you .emacs as:
;;
;;    (deh-require 'feature-name
;;      configuration-for-the-feature)
;;    (deh-section "section-name"
;;      some-configuration)
;;
;;  And when you want edit some configuration, use M-x
;;  `deh-customize-inplace' or M-x `deh-customize' to make changes. It
;;  knows where you put the configuration. The former maybe more
;;  reliable, but I like `deh-customize' more.
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(defvar deh-custom-file nil
  "Default coustom file.")

(defvar deh-sections nil
  "The sections that configured by `deh-require-maybe' etc.")

(defvar deh-enable-list nil
  "Features will be enable when need.")

(defvar deh-buffer-name "*Dot Emacs*"
  "Buffer name for edit configuration.")

(defvar deh-eval-when-submit t
  "Eval configuration code when submit.")

(defvar deh-information nil)

(defvar deh-missing-packages-list nil
  "List of packages that `deh-try-require', `deh-require-maybe'
and `deh-section-if' can't find.")

(defmacro deh-try-require (feature &rest forms)
  "Almost identical to (require FEATURE nil t). if failed, record
missing packages into `deh-missing-packages-list'."
  (declare (indent 1))
  `(progn
     (if (require ,feature nil t)
         (progn ,@forms)
       (add-to-list 'deh-missing-packages-list ,feature 'append))))

(defmacro deh-require-maybe (feature &rest forms)
  "Besides `deh-try-require', it records required packages into
`deh-secionts'."
  (declare (indent 1))
  `(progn
     (if ,load-file-name
         (add-to-list 'deh-sections (cons ,feature ,load-file-name)))
     (deh-try-require ,feature ,@forms)))
(defalias 'deh-require 'deh-require-maybe)
(put 'deh-require 'lisp-indent-function 1)

(defmacro deh-section (section &rest forms)
  "A section placeholder to arrange lisp codes."
  (declare (indent 1))
  `(progn
     (if ,load-file-name
         (add-to-list 'deh-sections (cons ,section ,load-file-name)))
     ,@forms))

(defmacro deh-require-if (feature cond &rest forms)
  "If COND is true, require this FEATURE.

Example:
  (deh-require-if 'org-capture
    (>= (string-to-int org-version) 7.5)
    (setq org-capture-templates '(
    )))
"
  (declare (indent 1))
  `(progn
     (when ,cond
       (if ,load-file-name
           (add-to-list 'deh-sections (cons ,feature ,load-file-name)))
       (deh-try-require ,feature ,@forms))))

(defmacro deh-require-reserved (feature &rest forms)
  "Put some elisp into `deh-enable-list' and reserved. You can
use `deh-enable' to active these elisp.

Example:
  (deh-require-reserved 'w3m-load
    (load \"preview-latex.el\" t t t)
    (load \"auctex.el\" t t t))
"
  (declare (indent 1))
  `(progn
     (if ,load-file-name
         (add-to-list 'deh-sections (cons ,feature ,load-file-name)))
     (add-to-list 'deh-enable-list '(,feature
                                     (deh-try-require ,feature
                                       ,@forms)))))

(defmacro deh-section-path (section path &rest forms)
  "If path exists, call `deh-section'. One internal variable
`deh-this-path' indicates the argument path, which you can use in
forms.

Example:
  (deh-section-path \"org\" \"~/src/org-7.01h\"
    (add-to-list 'load-path (expand-file-name \"lisp\" deh-this-path)))
"
  (declare (indent 1))
  `(let ((deh-this-path ,path))
     (if (not (file-exists-p deh-this-path))
         (add-to-list 'deh-missing-packages-list deh-this-path 'append)
       (if (file-directory-p deh-this-path)
           (add-to-list 'load-path deh-this-path))
       (deh-section ,section ,@forms))))

(defmacro deh-section-after (section &rest forms)
  "Eval forms after section file is loaded.

Example:
  (deh-section-after \"outline\"
    (setq outline-minor-mode-prefix (kbd \"C-c C-o\")))
"
  (declare (indent 1))
  `(progn
     (if ,load-file-name
         (add-to-list 'deh-sections (cons ,section ,load-file-name)))
     (eval-after-load ,section
       '(progn ,@forms))))

(defmacro deh-section-reserved (section &rest forms)
  "Put some elisp into `deh-enable-list' and reserved. You can
use `deh-enable' to active these elisp.

Example:
  (deh-section-reserved \"latex\"
    (load \"preview-latex.el\" t t t)
    (load \"auctex.el\" t t t))
"
  (declare (indent 1))
  `(progn
     (if ,load-file-name
         (add-to-list 'deh-sections (cons ,section ,load-file-name)))
     (add-to-list 'deh-enable-list '(,section ,@forms))))


(defmacro deh-section-if (section cond &rest forms)
  "If COND is true, SECTION will be enabled.

Example:
  (deh-section-if \"cedet\"
    cedet-enable
    (require 'cedet nil 'noerror))
"
  (declare (indent 1))
  `(progn
     (when ,cond
       (if ,load-file-name
           (add-to-list 'deh-sections (cons ,section ,load-file-name)))
       ,@forms)))

(defun deh-customize-inplace (name)
  "Configuration the section directly in file"
  (interactive
   (list (completing-read "Which section to modified: " deh-sections)))
  (let ((section (assoc-string name deh-sections))
        done)
    (if (and section
             (cdr section)
             (file-exists-p (cdr section)))
        (progn
          (find-file (cdr section))
          (goto-char (point-min))
          (setq done t)
          (re-search-forward (deh-regexp (car section))))
      (if (and deh-custom-file
               (file-exists-p deh-custom-file))
          (progn
            (find-file deh-custom-file)
            (setq done t)
            (goto-char (point-max)))))
    (unless done
      (message "No place assoc to the section %s. Set deh-custom-file to wrote there." name))))

(defun deh-customize (name)
  "Configuration the section in .emacs."
  (interactive
   (list (completing-read "Which section to modified: " deh-sections)))
  (deh-set-buffer)
  (setq deh-information nil)
  (let ((section (assoc-string name deh-sections)))
    (when (and section
               (cdr section)
               (file-exists-p (cdr section)))
      (let ((conf (deh-get-configuration section)))
        (insert conf)))
    (unless deh-information
      (if deh-custom-file
          (progn
            (message
             "No section found in .emacs. The customize code will write to `deh-custom-file' (%s)."
             deh-custom-file)
            (insert "(deh-section \"" name "\"\n  )\n")
            (backward-char 2))
        (message
         "Warning, No section found in .emacs and `deh-custom-file' is not set. So the code won't write to .emacs.")))
    (set-buffer-modified-p nil)
    (deh-minor-mode 1)
    (goto-char (point-min))))

(defsubst deh-stringfy (name)
  (if (symbolp name)
      (symbol-name name)
    name))

(defun deh-list-section (arg)
  "List all sections defined in .emacs.
With prefix argument sort section by file."
  (interactive "P")
  (require 'button)
  (switch-to-buffer (get-buffer-create "*DEH*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let (name pos)
    (dolist (sec (sort (copy-sequence deh-sections)
                       (if arg
                           (lambda (n1 n2)
                             (string< (cdr n1) (cdr n2)))
                         (lambda (n1 n2)
                           (string< (deh-stringfy (car n1))
                                    (deh-stringfy (car n2)))))))
      (setq pos (point))
      (setq name (deh-stringfy (car sec)))
      (insert (format "%-20s %s\n" name (cdr sec)))
      (make-text-button pos (+ pos (length name))
                        'action (lambda (but)
                                  (deh-customize (button-label but)))))
    (setq buffer-read-only t)))

(defun deh-set-buffer ()
  (switch-to-buffer (get-buffer-create deh-buffer-name))
  (if (or (not (buffer-modified-p))
          (yes-or-no-p "Last configuration not save yet, proceed anyway? "))
      (erase-buffer)
    (error "Save the configuration first!"))
  (emacs-lisp-mode))

(define-minor-mode deh-minor-mode
  "Minor mode for customize.

\\{deh-minor-mode-map}"
  :lighter " DEH"
  :keymap '(("\C-c\C-c" . deh-submit-and-exit)
            ("\C-x\C-s" . deh-submit)
            ("\C-c\C-f" . deh-switch-file))
  )

(defun deh-switch-file ()
  "Jump to configuration file."
  (interactive)
  (deh-customize-inplace
   (deh-stringfy (car (assoc-default "section" deh-information)))))

(defun deh-regexp (name)
  (if (stringp name)
      (concat "(\\s-*deh-section\\(-\\w+\\)?\\s-+\"" (regexp-quote name))
    (concat "(\\s-*deh-require\\(-\\w+\\)?\\s-+'"
            (regexp-quote (symbol-name name)))))

(defun deh-get-configuration (section)
  (let ((name (car section))
        (file (cdr section))
        re pos)
    (setq re (deh-regexp name))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward re nil t)
        (goto-char (match-beginning 0))
        (setq pos (cons (point) (scan-sexps (point) 1))
              deh-information (list (cons "section" section)
                                    (cons "position" pos)
                                    (cons "mtime" (nth 5 (file-attributes file)))))
        (buffer-substring (car pos) (cdr pos))))))

(defun deh-submit (quit)
  (interactive "P")
  (if (not (buffer-modified-p))
      (message "(No changes need to be saved)")
    (let (installed file)
      (if deh-information
          (let ((section (assoc-default "section" deh-information))
                (pos (assoc-default "position" deh-information))
                (mtime (assoc-default "mtime" deh-information))
                (conf (buffer-string))
                re)
            (setq file (cdr section)
                  re (deh-regexp (car section)))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (if (or (equal (nth 5 (file-attributes file)) mtime)
                      (and (re-search-forward re nil t)
                           (progn (goto-char (match-beginning 0))
                                  (setq pos (cons (point) (scan-sexps (point) 1))))
                           (yes-or-no-p "The config file is changed. But I found the place seem meet the section, continue? ")))
                  (progn
                    (goto-char (car pos))
                    (delete-region (car pos) (cdr pos))
                    (insert conf)
                    (write-region (point-min) (point-max) file)
                    ;; update deh-information for succeed submit
                    (setq deh-information (list (cons "section" section)
                                                (cons "position" (cons (car pos) (point)))
                                                (cons "mtime" (nth 5 (file-attributes file)))))
                    (setq installed t))
                (message "Sumbition canceled because config file doesn't match now!"))))
        (if (and deh-custom-file
                 (yes-or-no-p (format "No information found, append config to %s? "
                                      deh-custom-file)))
            (progn
              (write-region (point-min) (point-max) deh-custom-file t)
              (setq installed t
                    file deh-custom-file)
              ;; update deh-information for succeed
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "(\\s-*deh-require\\(-\\w+\\)?\\s-+['\"]" nil t)
                  (backward-char 1)
                  (let ((sec (read (buffer-substring (point) (scan-sexps (point) 1)))))
                    (if (listp sec)
                        (setq sec (cadr sec)))
                    (deh-get-configuration (cons sec deh-custom-file))))))
          (message "Can't install config, because I don't known where to write.")))
      ;; eval code when success and `deh-eval-when-submit'
      (when (and deh-eval-when-submit installed)
        (let ((load-file-name (cdr (assoc-default "section" deh-information))))
          (message "Eval the configuration....")
          (eval-region (point-min) (point-max))
          (message "done")))
      (when installed
        (set-buffer-modified-p nil)
        (message "Install configuration to %s successful" file))
      (if quit (bury-buffer)))))

(defun deh-submit-and-exit ()
  (interactive)
  (deh-submit t))

(defun deh-enable (feature)
  "Eval the form in `deh-enable-list'."
  (interactive
   (list (completing-read "Enable feature: " deh-enable-list
                          nil t)))
  (eval (cons 'progn
              (assoc-default feature deh-enable-list))))

;;; Other helper functions, eg: define-key, local-set-key, add-hook, etc.
(defmacro deh-define-key (map &rest keypairs)
  "Define a batch of keys.

Example:
  (deh-define-key global-map
    (\"\\C-m\"        . 'newline-and-indent)
    (\"\\C-j\"        . 'newline))
"
  (declare (indent 1))
  (cons 'progn
        (mapcar (lambda (pair)
                  `(define-key ,map ,(car pair) ,(cdr pair)))
                keypairs)))

(defmacro deh-local-set-key (hook &rest keypairs)
  "Set a batch of local keys for a hook.

Example:
  (deh-local-set-key 'text-mode-hook
    (\"\\C-m\"        . 'newline-and-indent)
    (\"\\C-j\"        . 'newline))
"
  (declare (indent 1))
  (list 'add-hook hook
        (cons 'lambda
              (cons 'nil (mapcar
                          (lambda (pair)
                            `(local-set-key ,(car pair) ,(cdr pair)))
                          keypairs)))))

(defmacro deh-local-set-keys (hooks &rest keypairs)
  "Set a batch of local keys for a list of hooks.

Example:
  (deh-local-set-keys '(text-mode-hook org-mode-hook)
    (\"\\C-m\"        . 'newline-and-indent)
    (\"\\C-j\"        . 'newline))
"
  (declare (indent 1))
  (list 'dolist (list 'hook hooks)
        (list 'add-hook 'hook
              (cons 'lambda
                    (cons 'nil (mapcar
                                (lambda (pair)
                                  `(local-set-key ,(car pair) ,(cdr pair)))
                                keypairs))))))

(defmacro deh-add-hook (hook &rest forms)
  "Apply some functions for a hook.

Example:
  (deh-add-hook 'c-mode-common-hook
    (flyspell-prog-mode)
    (flymake-minor-mode 1))
"
  (declare (indent 1))
  `(add-hook ,hook (lambda () ,@forms)))

(defmacro deh-remove-hook (hook &rest forms)
  "Remove some functions for a hook.

Example:
  (deh-remove-hook 'c-mode-common-hook
    (flyspell-prog-mode)
    (flymake-minor-mode 1))
"
  (declare (indent 1))
  `(remove-hook ,hook (lambda () ,@forms)))

(defmacro deh-add-hooks (hooks &rest forms)
  "Apply some functions for a list of hooks.

Example:
  (deh-add-hooks '(c-mode-common-hook emacs-lisp-mode-hook)
    (flyspell-prog-mode)
    (flymake-minor-mode 1))
"
  (declare (indent 1))
  `(dolist (hook ,hooks)
     (add-hook hook (lambda () ,@forms))))

(defmacro deh-remove-hooks (hooks &rest forms)
  "Remove some functions for a list of hooks.

Example:
  (deh-remove-hooks '(c-mode-common-hook emacs-lisp-mode-hook)
    (flyspell-prog-mode)
    (flymake-minor-mode 1))
"
  (declare (indent 1))
  `(dolist (hook ,hooks)
     (remove-hook hook (lambda () ,@forms))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<deh-\\(section\\|\\(try-\\)?require\\)\\(-[a-z]+\\)?\\>" . font-lock-keyword-face)))

(provide 'dot-emacs-helper)
;;; dot-emacs-helper.el ends here