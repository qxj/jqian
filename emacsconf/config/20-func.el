;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; my functions ;;;;;;;;;;;;;;;;;;;;
;; Most useful interactive function and commands for keybinds

(dolist (func '("func-dired-ext"
                "func-elisp-helper"
                "func-prog"
                "func-misc"))
  (load (expand-file-name func my-config-dir)))



(defun my-comment-or-uncomment-region (&optional line)
  "Comment or uncomment a line or a region."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (save-excursion
        (comment-or-uncomment-region
         (progn
           (beginning-of-line)
           (point))
         (progn
           (end-of-line)
           (point))))
    (call-interactively 'comment-or-uncomment-region)))

;;{{{ polish beginning-of-line & end-of-line (C-a/C-e)
(defun my-end-of-line ()
  "Hack `end-of-line'."
  (interactive)
  (if (eq (point) (line-end-position))
      (skip-chars-backward " \t")
    (move-end-of-line 1)))

(defun my-beginning-of-line ()
  "Hack `beginning-of-line'."
  (interactive)
  (if (eq (point) (line-beginning-position))
      (skip-chars-forward " \t")
    (beginning-of-line)))
;;}}}

(defun vi-open-next-line (arg)
 "Move to the next line (like vi) and then open a new line, bind
to \\[vi-open-next-line]."
 (interactive "p")
 (end-of-line)
 (open-line arg)
 (forward-line 1)
 (indent-according-to-mode))

(defmacro def-redo-command (fun-name redo undo)
  "Make redo command, bind to \\[redo]."
  `(defun ,fun-name ()
     (interactive)
     (if (equal last-command ,redo)
         (setq last-command 'undo)
       (setq last-command nil))
     (call-interactively ,undo)
     (setq this-command ,redo)))
(def-redo-command redo 'redo 'undo)

(defun my-untabify ()
  "My untabify function as discussed and described at
 http://www.jwz.org/doc/tabs-vs-spaces.html
 and improved by Claus Brunzema:
 - return nil to get `write-contents-hooks' to work correctly
   (see documentation there)
 - `make-local-hook' instead of `make-local-variable'
 - when instead of if
 Use some lines along the following for getting this to work in the
 modes you want it to:

 \(add-hook 'some-mode-hook
           '(lambda ()
               (make-local-hook 'write-contents-hooks)
                (add-hook 'write-contents-hooks 'my-untabify nil t)))"
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max)))
    nil))

;;{{{ move and duplicate lines
(defun my-move-line-up (p)
  "Move current line up."
  (interactive "p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (forward-line (- p))
    (beginning-of-line)
    (yank)
    (forward-line -1)
    (move-to-column c)))

(defun my-move-line-down (p)
  "Move current line down."
  (interactive "p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (forward-line p)
    (beginning-of-line)
    (yank)
    (forward-line -1)
    (move-to-column c)))

(defun my-dup-line-down ()
  "Duplicate this line at next line."
  (interactive)
  (let ((c (current-column)))
    (kill-new (buffer-substring (line-beginning-position) (line-end-position)))
    (end-of-line)
    (open-line 1)
    (forward-line 1)
    (beginning-of-line)
    (yank)
    (move-to-column c)))

(defun my-dup-line-down-continued ()
  "Put continued multiple lines into `kill-ring'."
  (interactive)
  (let ((kill-func (if (eq last-command 'ue-select-line-down-continued)
                       (lambda (s) (kill-append s nil))
                     (lambda (s) (kill-new s nil)))))
    (let ((oldpoint (point))
          (next-line-add-newlines))
      (forward-line 1)
      (funcall kill-func (buffer-substring oldpoint (point))))))
;;}}}

;;{{{ clone-buffer, bind to "C-x c"
(defun ywb-clone-buffer (non-indirect)
  "If with prefix argument, clone buffer, other wise, clone
indirect buffer. bind to \\[ywb-clone-buffer]."
  (interactive "P")
  (if non-indirect
      (call-interactively 'clone-buffer)
    (let ((indir-bufs (mapcar (lambda (buf) (cons buf (buffer-base-buffer buf)))
                              (remove-if-not 'buffer-base-buffer (buffer-list))))
          buf)
      (if (setq buf (assoc (current-buffer) indir-bufs))
          (select-window (display-buffer (cdr buf)))
        (if (setq buf (rassoc (current-buffer) indir-bufs))
            (select-window (display-buffer (car buf)))
          (setq current-prefix-arg nil)
          (call-interactively 'clone-indirect-buffer-other-window))))))
;;}}}

;;{{{ camelcase move, rebind "M-f/M-b"
(defun ywb-camelcase-move-word (fw)
  (let ((case-fold-search nil)
        wordpos casepos)
    (save-excursion
      (forward-word fw)
      (setq wordpos (point)))
    (save-excursion
      (and (re-search-forward "\\w[A-Z][a-z]" nil t fw)
           (setq casepos (- (point) (if (> fw 0) 2 -1)))))
    (cond ((and wordpos casepos)
           (goto-char
            (if (< (abs (- casepos (point)))
                   (abs (- wordpos (point))))
                casepos
              wordpos)))
          (wordpos (goto-char wordpos))
          (casepos (goto-char casepos))
          (t (goto-char (if (> fw 0) (point-max) (point-min)))))))

(defun ywb-camelcase-forward-word (arg)
  "Camelcase forward word, rebind to \\[ywb-camelcase-forward-word]."
  (interactive "p")
  (let ((fw (signum arg)))
    (dotimes (i (abs arg))
      (ywb-camelcase-move-word fw))))

(defun ywb-camelcase-backward-word (arg)
  "Camelcase backward word, rebind to \\[ywb-camelcase-move-word]."
  (interactive "p")
  (ywb-camelcase-forward-word (- arg)))
;;}}}

(defun ywb-uniq-region (beg end)
  "Apply `sort | uniq' to selected region."
  (interactive "r")
  (shell-command-on-region beg end "sort | uniq" nil t))

(defun my-switch-scratch ()
  "switch to *scratch* buffer, bind to \\[my-switch-scratch]."
  (interactive)
  (switch-to-buffer "*scratch*"))

;;{{{ Redefine basic operation, non-kill versions
(defun delete-char-or-region ()
  "hack `delete-char', delete char or region, skip kill ring."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))

;; Source: http://xahlee.org/emacs/emacs_kill-ring.html
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.

This command does not push erased text to `kill-ring'."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.

This command does not push erased text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.

If cursor at beginning or end of a line, delete the last RET."
  (interactive)
  (let ((be (or (bolp) (eolp))))
    (delete-region
     (point)
     (save-excursion (move-end-of-line 1) (point)))
    (if be (delete-char 1))))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor
position.

If cursor at beginning or end of a line, delete the previous RET."
  (interactive)
  (let ((be (or (bolp) (eolp))))
    (delete-region
     (point)
     (save-excursion (move-beginning-of-line 1) (point)))
  (if be (delete-char -1))))
;;}}}

(defvar switch-major-mode-history nil)
(defun switch-major-mode (mode)
  "Switch major mode"
  (interactive
   (list
    (intern
     (completing-read "Switch to mode: "
                      obarray (lambda (s)
                                (and (fboundp s)
                                     (string-match "-mode$" (symbol-name s))))
                      t nil 'switch-major-mode-history))))
  (setq switch-major-mode-history
        (cons (symbol-name major-mode) switch-major-mode-history))
  (funcall mode))

(defun my-display-buffer-path (&optional copy)
  "Display the absolute path of current buffer in mini-buffer. If
you call this function by prefix 'C-u', the path will be store
into `kill-ring'."
  (interactive
   (list current-prefix-arg))
  (let ((f (buffer-file-name (current-buffer))))
    (if f
        (case copy
          ((nil)
           (message "Buffer path: %s" f))
          (1                                ; store only path
           (let ((d (file-name-directory f)))
             (kill-new d)
             (message "Copy directory: %s" d)))
          (2                                ; store only file name
           (let ((d (file-name-nondirectory f)))
             (kill-new d)
             (message "Copy filename: %s" d)))
          (t                                ; store absolute file path
           (kill-new f)
           (message "Copy path: %s" f))))))

(defun my-revert-buffer ()
  "Revert buffer without prompt."
  (interactive)
  (revert-buffer nil t nil))

(defun my-toggle-sr-speedbar ()
  "Toggle sr speedbar window."
  (interactive)
  (sr-speedbar-toggle) (sr-speedbar-select-window))

(defun my-switch-recent-buffer ()
  "Swith to the recent visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun my-copy-full-file-name ()
  "Copy full file name of current-buffer."
  (interactive)
  (let ((file (expand-file-name buffer-file-name)))
    (kill-new file)
    (message "File `%s' copied." file)))

(defun ywb-set-paste ()
  "Avoid content indent when paste from clipboard."
  (interactive)
  (fundamental-mode)
  (setq indent-line-function 'ignore))

(defun ywb-generate-loaddefs ()
  "Auto generate autoloads into '100-loaddefs.el'."
  (interactive)
  (require 'autoload)
  (with-temp-buffer
    (let (files)
      (setq files (directory-files my-config-dir t "func-.*\\.el$"))
      (dolist (dir '("contrib" "goodies"))
        (setq files (append files
                            (directory-files
                             (expand-file-name dir my-site-lisp-dir)
                             t ".*\\.el$"))))
      (dolist (file files)
        (unless (file-directory-p file)
          (generate-file-autoloads file)))
      (write-region (point-min) (point-max)
                    (expand-file-name "100-loaddefs.el" my-config-dir)))))

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

(defun my-byte-recompile-startup-dir ()
  "Recompile all the .el files under my-startup-dir, if they're
not up to date. This can be run from the command line with: $
emacs -l ~/.emacs -batch -f byte-recompile-startup-dir"
  (interactive)
  (dolist (dir (find-subdirs-containing my-startup-dir "\\.el$"))
    (byte-recompile-directory dir 0)))

(deh-section "defadvice"
  (defadvice kill-line (before check-position activate)
    "killing the newline between indented lines and remove extra
spaces."
    (if (member major-mode
                '(emacs-lisp-mode scheme-mode lisp-mode
                                  c-mode c++-mode objc-mode python-mode
                                  latex-mode plain-tex-mode))
        (if (and (eolp) (not (bolp)))
            (progn (forward-char 1)
                   (just-one-space 0)
                   (backward-char 1)))))

  (defadvice kill-ring-save (before slickcopy activate compile)
    "When called interactively with no active region, copy the
current single line to `kill-ring' instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  (defadvice kill-region (before slickcut activate compile)
    "When called interactively with no active region, kill the
current single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you
quit Emacs."
    (flet ((process-list ())) ad-do-it))

  (defadvice kill-new (before kill-new-push-xselection-on-kill-ring activate)
    "Before putting new kill onto the kill-ring, add the
clipboard/external selection to the kill ring"
    (let ((have-paste (and interprogram-paste-function
                           (funcall interprogram-paste-function))))
      (when have-paste (push have-paste kill-ring))))

  ;; Auto indent pasted content
  (dolist (command '(yank yank-pop))
    (eval
     `(defadvice ,command (after indent-region activate)
        (and (not current-prefix-arg)
             (member major-mode
                     '(emacs-lisp-mode
                       python-mode
                       c-mode c++-mode
                       latex-mode
                       js-mode
                       php-mode
                       plain-tex-mode))
             (let ((mark-even-if-inactive transient-mark-mode))
               (indent-region (region-beginning) (region-end) nil))))))

  (defadvice occur (around occur-mark-region)
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      ad-do-it))
  (ad-activate 'occur)

  ;; (defadvice browse-url-generic (before ywb-browse-url-generic)
  ;;   (setq url (replace-regexp-in-string "\\cC" 'url-hexify-string url)))
  ;; (ad-activate 'browse-url-generic)

  (defadvice browse-url-file-url (after ywb-url-han)
    (let ((file ad-return-value))
      (while (string-match "[\x7f-\xff]" file)
        (let ((enc (format "%%%x" (aref file (match-beginning 0)))))
          (setq file (replace-match enc t t file))))
      (setq ad-return-value file)))
  (ad-activate 'browse-url-file-url))

(deh-section "hooks"
  ;;# chmod executable files automatically
  (add-hook
   'after-save-hook
   #'(lambda ()
       (and (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (save-match-data
                  (looking-at "^#!"))))
            (not (file-executable-p buffer-file-name))
            (shell-command (concat "chmod u+x " buffer-file-name))
            (message
             (concat "Saved as script: " buffer-file-name)))))
  ;;# convert some .h to c++-mode automatically
  (add-hook
   'c-mode-hook
   #'(lambda ()
       (if (and (string-match "\.h$" (buffer-name))
                (save-excursion
                  (goto-char (point-min))
                  (search-forward-regexp "^class" nil t)))
           (c++-mode))))
  )
