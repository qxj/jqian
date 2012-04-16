;;; dot-emacs-helper.el --- Some helper functions for .emacs

;; Copyright 2007, 2012 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Update: Julian Qian <junist@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
  "The sections that configured by `deh-section-with-options' etc.")

(defvar deh-keybinds nil
  "Record all keybinds by `deh-define-key'.")

(defvar deh-hooks nil
  "Record all hooks by `deh-add-hook'.")

(defvar deh-reserves nil
  "Features will be enabled when needed.")

(defvar deh-buffer-name "*Dot Emacs*"
  "Buffer name for edit configuration.")

(defvar deh-eval-when-submit t
  "Eval configuration code when submit.")

(defvar deh-information nil)

(defvar deh-custom-keys-list nil
  "All keybinds customized by `deh-define-key' etc.")

(defvar deh-custom-hooks-list nil
  "All hooks customized by `deh-add-hook' etc.")

(defvar deh-missing-packages-list nil
  "List of packages that `deh-try-require', `deh-require-maybe'
and `deh-section-if' can't find.")

(defsubst deh--getoption (key options)
  (let ((val (memq key options)))
    (if val
        (cons t (cadr (memq key options)))
      nil)))

(defsubst deh--stringfy (name)
  (if (symbolp name) (symbol-name name) name))

(defsubst deh--tolerate (item)
  (if (listp item)         ; quote tolerance @.@
      (if (eq 'quote (car item))
          (if (symbolp (cadr item))
              (list (cadr item))
            (cadr item))
        item)
    (list item)))

(defmacro deh-try-require (feature &rest forms)
  "Almost identical to (require FEATURE nil t). if failed, record
missing packages into `deh-missing-packages-list'."
  (declare (indent 1))
  (if (stringp feature)
    `(progn ,@forms)
    `(if (require ,feature nil t)
         (progn ,@forms)
       (add-to-list 'deh-missing-packages-list ,feature 'append))))

(defmacro deh-after-load (filename &rest forms)
  "A syntax suger for `eval-after-load' expression."
  (declare (indent 1))
  `(eval-after-load ,(deh--stringfy filename)
     '(progn ,@forms)))

(defmacro deh-section-with-options (name options &rest forms)
  "Combine all into one section.

options should have the form

    [KEYWORD VALUE]...

The following keywords are meaningful:

:eval     when to eval this function

    'AFTER 'RESERVED

:cond     whether to declare this section

    a BOOLEAN function

:source   where to download the latest package

    [(LOCAL-PATH EL-URL1 EL-URL2 ...)...]

:autoload load file and functions

    [(LOAD-FILE 'FUNCTION1 'FUNCTION2 ...)...]

:keybind  keymap, key and functions

    [(KEY-MAP (KEY1 'FUNCTION1) (KEY2 'FUNCTION2)...)...]

:hook     hook and functions

    [(HOOK FUNCTION1 FUNCTION2 ...)...]
"
  (declare (indent 1))
  ;; TODO: add timer for profiling
  (list 'progn
        ;; won't override previous setting
        (nconc `(unless (assoc-string ,name deh-sections)
                  (if ,load-file-name
                      (add-to-list 'deh-sections (cons ,name ,load-file-name))))
               ;; autoload
               (let* ((ls (cdr (deh--getoption :autoload options)))
                      (lv (if (vectorp ls) ls (vector ls))))
                 (mapcan (lambda (load)
                           (let ((filename (car load)))
                             (mapcar (lambda (func)
                                       `(autoload ,func ,filename "" t)) (cdr load)))) lv))
               ;; source
               ;; TODO: maintain source for each package
               (let* ((when-to-eval (deh--getoption :eval options)) ; eval
                      (whether-load (deh--getoption :cond options)) ; cond
                      (ks (cdr (deh--getoption :keybind options)))  ; keybind
                      (kv (if ks (if (vectorp ks) ks (vector ks))))
                      (keybinds (mapcar (lambda (keybind)
                                          (let ((keymap (car keybind))
                                                (binds (cdr keybind)))
                                            `(deh-define-key ,keymap ,@binds)))
                                        kv))
                      (hs (cdr (deh--getoption :hook options))) ; hook
                      (hv (if hs (if (vectorp hs) hs (vector hs))))
                      (hooks (mapcar (lambda (hfunc)
                                       (let ((hook (car hfunc))
                                             (funcs (cdr hfunc)))
                                         `(deh-add-hook ,hook ,@funcs)))
                                     hv))
                      (body (nconc forms keybinds hooks)))
                 ;; merge body
                 (list
                  (if whether-load
                      (cond ((eq 'after (cdr when-to-eval))
                             `(if ,(cdr whether-load) (deh-after-load ,name ,@body))
                             )
                            ((eq 'reserved (cdr when-to-eval))
                             `(if ,(cdr whether-load)
                                  (add-to-list 'deh-reserves '(,name (deh-try-require ,name ,@body)))))
                            (t
                             `(if ,(cdr whether-load)
                                  (deh-try-require ,name ,@body))))
                    (cond ((eq 'after (cdr when-to-eval))
                           `(deh-after-load ,name ,@body))
                          ((eq 'reserved (cdr when-to-eval))
                           `(add-to-list 'deh-reserves '(,name (deh-try-require ,name ,@body))))
                          (t
                           `(deh-try-require ,name ,@body)))))))))

;;;
;;; Some derived macros from `deh-section-with-options'
;;;

(defmacro deh-require (feature &rest forms)
  "Besides `deh-try-require', it records required packages into
`deh-secionts'."
  `(deh-section-with-options ,feature nil ,@forms))
(put 'deh-require 'lisp-indent-function 1)

(defmacro deh-require-if (feature cond &rest forms)
  "If COND is true, require this FEATURE."
  (declare (indent 1))
  `(deh-section-with-options ,feature (:eval ,cond) ,@forms))

(defmacro deh-require-reserved (feature &rest forms)
  "Put some elisp into `deh-reserves' and reserved. You can
use `deh-enable' to active these elisp."
  (declare (indent 1))
  `(deh-section-with-options ,feature (:eval reserved) ,@forms))

(defmacro deh-section (section &rest forms)
  "A section placeholder to arrange lisp codes."
  (declare (indent 1))
  `(deh-section-with-options ,section nil ,@forms))

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
  "Eval forms after section file is loaded."
  (declare (indent 1))
  `(deh-section-with-options ,section (:eval after) ,@forms))

(defmacro deh-section-autoload (section autoloads &rest forms)
  "Eval forms after section file is loaded, also integrate
autoloads setting."
  (declare (indent 1))
  `(deh-section-with-options ,section (:eval after :autoload (,section ,@autoloads)) ,@forms))

(defmacro deh-section-reserved (section &rest forms)
  "Put some elisp into `deh-reserves' and reserved. You can
use `deh-enable' to active these elisp."
  (declare (indent 1))
  `(deh-section-with-options ,section (:eval reserved) ,@forms))

(defmacro deh-section-if (section cond &rest forms)
  "If COND is true, SECTION will be enabled."
  (declare (indent 1))
  `(deh-section-with-options ,section (:cond ,cond) ,@forms))


;;; Other helper functions, eg: define-key, local-set-key, add-hook, etc.
(defmacro deh-define-key (map &rest keypairs)
  "Define a batch of keys.

Example:
  (deh-define-key (text-mode-map c-mode-map)
    (\"\\C-m\"  'newline-and-indent)
    (\"\\C-j\"  'newline))

  (deh-define-key global-map
    (\"\\C-m\"  'newline-and-indent)
    (\"\\C-j\"  'newline))
"
  (declare (indent 1))
  (let ((maps (if (listp map) map (list map))))
    (nconc (list 'progn)
           (mapcan (lambda (map)
                     (mapcan (lambda (pair)
                               (list
                               `(define-key ,map ,(car pair) ,(cadr pair))
                               `(deh--set-keybind ',map ,(car pair) ,(cadr pair))))
                             keypairs))
                   maps))))

;; Deperated! Suggest to replace with deh-define-key
(defmacro deh-local-set-key (hook &rest keypairs)
  "Set a batch of local keys for a hook.

Example:
  (deh-local-set-key (text-mode-hook c-mode-common-hook)
    (\"\\C-m\"  'newline-and-indent)
    (\"\\C-j\"  'newline))

  (deh-local-set-key text-mode-hook
    (\"\\C-m\"  'newline-and-indent))
"
  (declare (indent 1))
  (let ((hooks (deh--tolerate hook)))
    (nconc (list 'progn)
           (mapcar (lambda (hk)
                     (list 'add-hook (list 'quote hk)
                           (nconc (list 'lambda 'nil)
                                  (mapcar (lambda (pair)
                                            `(local-set-key ,(car pair) ,(cadr pair)))
                                          keypairs))))
                   hooks))))

(defmacro deh-add-hook (hook &rest forms)
  "Apply some functions for a hook.

Example:
  (deh-add-hook (c-mode-common-hook text-mode-hook)
    (flyspell-prog-mode)
    (auto-fill-mode 1))

  (deh-add-hook c-mode-common-hook flymake-minor-mode)
"
  (declare (indent 1))
  (let ((hooks (deh--tolerate hook)))
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


;; TODO: defadvice completing-read ?
(defun deh-completing-read (prompt collection &optional predicate require-match)
  "Use `ido-completing-read' to replace `completing-read' if possible."
  (if (fboundp 'ido-completing-read)
      (ido-completing-read prompt
                           (mapcar (lambda (x) (if (symbolp (car x))
                                                   (symbol-name (car x))
                                                 (car x))) collection)
                           nil require-match)
    (completing-read prompt collection nil require-match)))

(defun deh-customize-inplace (name)
  "Configuration the section directly in file"
  (interactive
   (list (deh-completing-read "Which section to modified: " deh-sections)))
  (let ((section (assoc-string name deh-sections))
        done)
    (if (and section
             (cdr section)
             (file-exists-p (cdr section)))
        (progn
          (find-file (cdr section))
          (goto-char (point-min))
          (setq done t)
          (re-search-forward (deh--regexp (car section)))
          (recenter-top-bottom 0))
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
   (list (deh-completing-read "Which section to modified: " deh-sections)))
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
                           (string< (deh--stringfy (car n1))
                                    (deh--stringfy (car n2)))))))
      (setq pos (point))
      (setq name (deh--stringfy (car sec)))
      (insert (format "%-20s %s\n" name (cdr sec)))
      (make-text-button pos (+ pos (length name))
                        'action (lambda (but)
                                  (deh-customize (button-label but)))))
    (setq buffer-read-only t)))

;; FIXME: record keybinds error
(defun deh-list-keybind ()
  "List all keybinds by `deh-define-key'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*DEH*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (mapc (lambda (keybind)
          (insert (format "%s\n" (deh--stringfy (car keybind)))) ; map
          (mapc (lambda (bind)
                  (insert (format "\t%s\t%s\n" (key-description (car bind))
                                  (deh--stringfy (cdr bind)))))
                (sort (cdr keybind)
                      (lambda (n1 n2)
                        (string< (key-description (car n1))
                                 (key-description (car n2)))))))
        (sort deh-keybinds
              (lambda (n1 n2)
                (string< (deh--stringfy (car n1))
                         (deh--stringfy (car n2))))))
  (goto-char (point-min)))

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

(defun deh--set-keybind (map key func)
  (let* ((binds (cdr (assoc map deh-keybinds)))
         (new-binds (if binds
                        (if (assoc key binds)
                            binds
                          (append `((,key . ,func)) binds))
                      `((,key . ,func)))))
    (setq deh-keybinds (cons `(,map . ,new-binds)
                             (delq (assoc map deh-keybinds) deh-keybinds)))))

(defun deh--regexp (name)
  (if (stringp name)
      (concat "(\\s-*deh-section\\(-\\w+\\)?\\s-+\"" (regexp-quote name))
    (concat "(\\s-*deh-require\\(-\\w+\\)?\\s-+'"
            (regexp-quote (symbol-name name)))))

(defun deh-switch-file ()
  "Jump to configuration file."
  (interactive)
  (deh-customize-inplace
   (deh--stringfy (car (assoc-default "section" deh-information)))))

(defun deh-get-configuration (section)
  (let ((name (car section))
        (file (cdr section))
        re pos)
    (setq re (deh--regexp name))
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
                  re (deh--regexp (car section)))
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
  "Eval the form in `deh-reserves'."
  (interactive
   (list (deh-completing-read "Enable feature: " deh-reserves
                              nil t)))
  (eval (cons 'progn
              (assoc-default feature deh-reserves))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<deh-\\(section\\|\\(try-\\)?require\\)\\([-a-z]+\\)?\\>" . font-lock-function-name-face)))

(provide 'dot-emacs-helper)
;;; dot-emacs-helper.el ends here
