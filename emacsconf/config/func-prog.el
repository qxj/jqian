(defun my-find-tag (&optional show-only)
  "Show tag in other window with no prompt in minibuf."
  (interactive)
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))
    (if show-only
        (progn (find-tag-other-window default)
               (shrink-window (- (window-height) 12)) ;; limit 12 lines
               (recenter 1)
               (other-window 1))
      (find-tag default))))

;; close paren
(defun ywb-insert-paren ()
  (interactive)
  (condition-case nil
      (progn
        (scan-sexps (point) -1)
        (insert ")")
        (ywb-insert-paren))
    (error (delete-char -1))))

;;{{{  Auto recompile for emacs-lisp
(defvar auto-recompile nil)
(put 'auto-recompile 'safe-local-variable 'booleanp)
(defvar auto-recompile-query t
  "if non-nil, ask user before byte compile.")
(defun auto-recompile-file-maybe ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.el\\(\\.gz\\)?\\'" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "c")))
      (if (and
           (file-exists-p byte-file)
           (file-newer-than-file-p buffer-file-name byte-file)
           (or auto-recompile
               (null auto-recompile-query)
               (called-interactively-p)
               (y-or-n-p (format "byte-compile %s" buffer-file-name))))
          (byte-compile-file buffer-file-name)))))
(defun auto-recompile-save-hook ()
  (add-hook 'kill-buffer-hook 'auto-recompile-file-maybe nil t))
(add-hook 'emacs-lisp-mode-hook 'auto-recompile-save-hook)
;;}}}

(defun ywb-html-insert-newline ()
  (interactive)
  (insert "<br />"))

(defun apropos-function (pattern)
  (interactive (list (apropos-read-pattern "function")))
  (apropos-command pattern nil 'fboundp))

(defun ywb-html-preview-region (beg end)
  (interactive "r")
  (let ((file (make-temp-file "region-" nil ".html")))
    (write-region beg end file)
    (browse-url file)))

(defun ywb-show-format-time-string (locale)
  (interactive
   (list (completing-read "With locale(default current locale): "
                          (split-string (shell-command-to-string "locale -a") "\n")nil t)))
  (let ((system-time-locale
         (if (= (length locale) 0) nil locale))
        (time (current-time)))
    (with-current-buffer (get-buffer-create "*format-time-string*")
      (erase-buffer)
      (insert "Current time: " (format-time-string "%c" time) "\n")
      (dolist (c (nconc (number-sequence ?a ?z)
                        (number-sequence ?A ?Z)))
        (insert (format "%%%c - %s\n" c
                        (format-time-string (format "%%%c" c) time))))
      (display-buffer (current-buffer)))))

(defun ywb-find-bin (file)
  (interactive
   (list
    (completing-read "bin: " 'locate-file-completion
                     (cons exec-path nil))))
  (find-file (locate-file file exec-path)))

