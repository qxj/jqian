;; -*- coding: utf-8 -*-

(setq debug-on-error nil debug-on-quit nil)

;; shorthand for interactive lambdas
(defalias 'λ 'lambda)
(defmacro Λ (&rest body)
  `(λ ()
       (interactive)
       ,@body))
(setq ns-function-modifier 'hyper)
(global-set-key (kbd "H-l") (Λ (insert "\u03bb")))
(global-set-key (kbd "H-L") (Λ (insert "\u039B")))

(defconst my/ismac (equal system-type 'darwin))
(defconst my/iswin (or (eq system-type 'windows-nt) (eq system-type 'cygwin)))

(defvar my/include-dirs
  (let (dirs
        (incs '("include" "inc" "common"))
        (updirs '("./" "../" "../../" "../../../" "../../../../")))
    (dolist (dir updirs)
      (setq dirs (append dirs (mapcar (lambda (x) (concat dir x)) incs))))
    (append updirs dirs)))

(defun gcc-include-path ()
  "Get gcc include path, only tested in linux."
  (with-temp-buffer
    (shell-command "echo | LC_ALL=\"en\" cpp -xc++ -Wp,-v" t)
    (goto-char (point-min))
    (let* ((start (search-forward "#include <...> search starts here:\n" nil t))
           (end (and start (progn (search-forward "End of search list." nil t)
                                  (beginning-of-line)
                                  (backward-char 1)
                                  (point)))))
      (and start end (mapcar (lambda (x) (substring x 1))
                             (split-string (buffer-substring start end) "\n"))))))

(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel
       process
       (lambda (proc change)
         (when (string-match "\\(finished\\|exited\\)" change)
           (kill-buffer (process-buffer proc))))))))

(defun emacs-process-duplicated-p ()
  "Check whether another emacs process is running concorrently by
pgrep, so.. make sure pgrep is already installed in your system."
  (if (executable-find "pgrep")
      (save-excursion
        (let ((buffer (generate-new-buffer (generate-new-buffer-name "*check emacs process*")))
              (process-number 0))
          (set-buffer buffer)
          (when (= 0 (call-process "pgrep" nil t nil "emacs"))
            ;; (setq pid (buffer-substring (point-min) (1- (point-max))))
            (goto-char (point-min))
            (while (search-forward-regexp "^[0-9]+$" nil t)
              (incf process-number)))
          (kill-buffer buffer)
          (> process-number 1)))))

(defun remove-from-list (list key)
  "reverse to `add-to-list' function"
  (set list (remove (assoc key (symbol-value list))
                    (symbol-value list))))

(defun split-file-name (file-name)
  (split-string (directory-file-name file-name) "/"))

(defun basename (file-name)
  "simulate linux command basename(1), require cl.el"
  (car (last (split-file-name file-name))))

(defun directory-depth (file-name)
  "how deep of a file or directory."
  (length (split-file-name file-name)))

(defsubst join (separator sequence)
  (mapconcat 'identity sequence separator))

(defmacro my (&rest args)
  `(mapc 'make-local-variable ',args))

(defmacro define-mode-toggle (name cmd cond &optional restore)
  "Define a function to toggle buffer visible by its mode.

first argument NAME to combine command name, second argument CMD
to open the mode, thrid argument COND is a lisp expression to
check which buffers will be toggled, four optional argument
RESTORE is also a lisp expression to restore all hidden buffers
back.

For example:

 (define-mode-toggle \"gdb\"  gdb
   (derived-mode-p \'gud-mode)
   (call-interactively 'gdb-restore-windows))
"
  (declare (debug t) (indent 2))
  `(defun ,(intern (format "my/toggle-%s" name)) ()
     ,(format "Toggle %s mode visible" name)
     (interactive)
     (if ,cond
         (while ,cond
           (bury-buffer))
       (let ((list (buffer-list)))
         (while list
           (if (with-current-buffer (car list)
                 ,cond)
               (progn
                 ,(if (null ',restore)
                      `(switch-to-buffer (car list))
                    ;; ,@switch
                    ;; `(funcall ',restore)
                    `,restore
                    )
                 (setq list nil))
             (setq list (cdr list))))
         (unless ,cond
           (call-interactively ',cmd))))))

(defun find-subdirs-containing (dir pattern)
  "Return a list of all deep subdirectories of DIR that contain
files that match PATTERN."
  (let* ((ret nil)
         (files (directory-files dir))
         (max-lisp-eval-depth 3000))
    (while files
      (let* ((file (car files))
             (path (expand-file-name file dir)))
        (if (and (file-directory-p path)
                 (not (string-match "^\\.+" file)))
            (setq ret (append ret (find-subdirs-containing path pattern)))
          (if (string-match pattern file)
              (add-to-list 'ret dir))))
      (setq files (cdr files)))
    ret))

(defun find-files-in-directory (dir pattern)
  "Return a list of all files in DIR whose filenames match PATTERN."
  (let* (ret
         (files (directory-files dir))
         (max-lisp-eval-depth 3000))
    (while files
      (let* ((file (car files))
             (path (expand-file-name file dir)))
        (if (file-directory-p path)     ; directory
            (if (not (string-match "^\\.+" file))
                (setq ret (append ret (find-files-in-directory path pattern))))
          (if (string-match pattern file) ; file
              (add-to-list 'ret path))))
      (setq files (cdr files)))
    ret))

;; following functions were removed from advice.el in emacs24.4
(unless (functionp 'ad-advised-definition-p)
  (defmacro ad-macro-p (definition)
    ;;"non-nil if DEFINITION is a macro."
    (` (eq (car-safe (, definition)) 'macro)))

  (defun ad-advised-definition-p (definition)
    ;;"non-nil if DEFINITION was generated from advice information."
    (if (or (ad-lambda-p definition)
            (ad-macro-p definition)
            (ad-compiled-p definition))
        (let ((docstring (ad-docstring definition)))
          (and (stringp docstring)
               (string-match
                ad-advised-definition-docstring-regexp docstring))))))
