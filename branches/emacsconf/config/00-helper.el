;; -*- coding: utf-8 -*-

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

(defun my-command-exists-p (cmd)
  "Check whether command exists in `exec-path'."
  ;; TODO: learn `locate-file' and `executable-find' :(
  (catch 'loop
    (dolist (path exec-path)
      (if (file-exists-p (expand-file-name cmd path))
              (throw 'loop t)))
      nil))

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

(defun indent-marked-files ()
  "Firstly mark files in `dired-mode', then indent them."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

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

;; sort line
(defun sort-lines-1 (reverse beg end predicate)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse 'forward-line 'end-of-line nil nil
                 predicate))))

(defsubst join (separator sequence)
  (mapconcat 'identity sequence separator))

(defmacro my (&rest args)
  `(mapc 'make-local-variable ',args))

(defalias 'pp* 'cl-prettyprint)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'chr 'char-to-string)
(defalias 'list-ascii 'ascii-table-show)
(defalias 'default-generic-mode 'conf-mode)

;; from xwl-util.el
(defun my-shell-command-asynchronously (cmd)
  (start-process-shell-command cmd nil cmd))

(defun my-notify (title message)
  (my-shell-command-asynchronously
   (format "zenity --info --title \"%s\" --text \"%s\""
           title message)))

(defun ascii-table-show ()
  "Print the ascii table"
  (interactive)
  (with-current-buffer (get-buffer-create "*ASCII table*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i   0)
          (tmp 0))
      (insert (propertize
               "                                [ASCII table]\n\n"
               'face font-lock-comment-face))
      (while (< i 32)
        (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
          (insert (concat
                   (propertize (format "%3d " tmp)
                               'face font-lock-function-name-face)
                   (propertize (format "[%2x]" tmp)
                               'face font-lock-constant-face)
                   "    "
                   (propertize (format "%3s" (single-key-description tmp))
                               'face font-lock-string-face)
                   (unless (= tmp (+ 96 i))
                     (propertize " | " 'face font-lock-variable-name-face)))))
        (newline)
        (setq i (+ i 1)))
      (goto-char (point-min)))
    (toggle-truncate-lines 1)
    (toggle-read-only 1)
    (display-buffer (current-buffer))))

(defun run-in-ansi-term (prg &optional use-existing)
  "Run program PRG in a terminal buffer. If USE_EXISTING is
non-nil and PRG is already running, switch to that buffer instead
of starting a new instance."
  (interactive)
  (let ((bufname (format "*%s*" prg)))
    (when (not (and use-existing
                    (let ((buf (get-buffer bufname)))
                      (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg prg))))

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
  `(defun ,(intern (format "my-toggle-%s" name)) ()
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
