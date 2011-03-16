;; -*- coding: utf-8 -*-

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

;; from xwl-util.el
(defun my-shell-command-asynchronously (cmd)
  (start-process-shell-command cmd nil cmd))

(defun my-notify (title message)
  (my-shell-command-asynchronously
   (format "zenity --info --title \"%s\" --text \"%s\""
           title message)))