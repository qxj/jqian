;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;{{{ .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

;; Most useful interactive function and commands for keybinds

;; Workaound for `kill-ring-save` in emacs24 under mac os x 10.7 lion
(defun clipboard-kill-ring-save (beg end)
  "Copy region to kill ring, and save in the X clipboard.
Do not copy region to kill ring if that would result in a
duplicate entry."
  (interactive "r")
  (when (or (not transient-mark-mode) mark-active)
    (let ((x-select-enable-clipboard t))
      (kill-ring-save beg end)
      (if (equal (car kill-ring) (cadr kill-ring))
          (setcdr kill-ring (cddr kill-ring))))))

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;;{{{ format
(defun format-region ()
  "Format region, if no region actived, format current buffer.
Like eclipse's Ctrl+Alt+F."
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (if (and (fboundp 'region-active-p) (region-active-p))
        (progn (setq start (region-beginning))
               (setq end (region-end)))
      (progn (when (fboundp 'whitespace-cleanup)
               (whitespace-cleanup))
             (setq end (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) end)
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char start)
        (when (fboundp 'whitespace-cleanup)
          (whitespace-cleanup))
        (untabify start (point-max))
        (indent-region start (point-max) nil)))))

(defun cxx-file-p (file)
  (let ((file-extension (file-name-extension file)))
    (and file-extension
         (string= file (file-name-sans-versions file))
         (find file-extension
               '("h" "hpp" "hxx" "c" "cpp" "cxx")
               :test 'string=))))

(defun format-cxx-file (file)
  "Format a c/c++ file."
  (interactive "F")
  (if (cxx-file-p file)
      (let ((buffer (find-file-noselect file))) ;; open buffer
        (set-buffer buffer)
        ;; (mark-whole-buffer)
        (when (fboundp 'whitespace-cleanup)
          (whitespace-cleanup))
        (untabify (point-min) (point-max))
        (indent-region (point-min) (point-max))
        (save-buffer)
        (kill-buffer)
        (message "Formated c++ file:%s" file))
    (message "%s isn't a c++ file" file)))

(defun format-cxx-directory (dirname)
  "Format all c/c++ file in a directory."
  (interactive "D")
  ;; (message "directory:%s" dirname)
  (let ((files (directory-files dirname t)))
    (dolist (x files)
      (if (not (string= "." (substring (file-name-nondirectory x) 0 1)))
          (if (file-directory-p x)
              (format-cxx-directory x)
            (if (and (file-regular-p x)
                     (not (file-symlink-p x))
                     (cxx-file-p x))
                (format-cxx-file x)))))))
;;}}}

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

;;{{{ sudo find file
(defvar find-file-root-prefix
  (if (featurep 'xemacs)
      "/[sudo/root@localhost]"
    "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")


(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

;;;###autoload
(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."
  (interactive)
  (require 'tramp)
  (let* ((tramp-mode t)                 ; enable tramp-mode internal
         ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    ;;     (when tramp
    ;;       (setq path (tramp-file-name-path tramp)
    ;;             dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))
;;}}}

;;{{{ manipulate item
(defun my-insert-item ()
  (interactive)
  (let (curr next)
    (beginning-of-line)
    (cond ((looking-at "\\(\\s-*\\)\\([0-9]+\\)\\.\\s-*")
           (setq curr (string-to-number (buffer-substring (match-beginning 2)
                                                          (match-end 2))))
           (setq next (number-to-string (1+ curr)))
           (end-of-line)
           (insert "\n" (buffer-substring (match-beginning 1)
                                          (match-end 1))
                   next ". ")
           (my-sync-item))
          ((looking-at "\\s-*[-+]\\s-*")
           (progn
             (end-of-line)
             (insert "\n" (buffer-substring (match-beginning 0)
                                            (match-end 0)))))
          (t
           (progn
             (end-of-line)
             (newline-and-indent))))))

(defun my-sync-item ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\(\\s-*\\)\\([0-9]+\\)\\.\\s-*")
        (let ((curr (string-to-number (buffer-substring (match-beginning 2)
                                                        (match-end 2))))
              (blank1 (buffer-substring (match-beginning 1)
                                        (match-end 1)))
              (blank2 (buffer-substring (match-end 2)
                                        (match-end 0))))
          (while (progn
                   (beginning-of-line 2)
                   (looking-at "\\s-*[0-9]+\\.\\s-*"))
            (setq curr (1+ curr))
            (delete-region (match-beginning 0) (match-end 0))
            (insert blank1 (number-to-string curr) blank2))))))
;;}}}

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
 "Move to the next line (like vi) and then open a new line. bind
to \\[vi-open-next-line]."
 (interactive "p")
 (end-of-line)
 (open-line arg)
 (forward-line 1)
 (indent-according-to-mode))

;;{{{ simulate J, gJ in vi
(defun vi-join-lines(&optional arg)
  "Join next line to current line (like vi), splitted by only one
space. bind to \\[vi-join-lines]."
  (interactive "P")
  (setq arg (abs (if arg (prefix-numeric-value arg) 1)))
  (while (> arg 0)
    (save-excursion
      (end-of-line)
      (delete-char 1)
      (just-one-space))
    (setq arg (- arg 1))))

(defun vi-merge-lines(&optional arg)
  "Merge next line to current line (like vi), without spaces
leaving. bind to \\[vi-merge-lines]."
  (interactive "P")
  (setq arg (abs (if arg (prefix-numeric-value arg) 1)))
  (while (> arg 0)
    (save-excursion
      (end-of-line)
      (delete-char 1)
      (delete-horizontal-space))
    (setq arg (- arg 1))))
;;}}}

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
(defun my-clone-buffer (non-indirect)
  "If with prefix argument, clone buffer, other wise, clone
indirect buffer. bind to \\[my-clone-buffer]."
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

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (when (nth 3 (syntax-ppss))
          (if (< arg 0)
              (progn
                (skip-syntax-forward "^\"")
                (goto-char (1+ (point)))
                (incf arg))
            (skip-syntax-backward "^\"")
            (goto-char (1- (point)))
            (decf arg)))
        (up-list (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

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
  (delete-region (point) (if (and (boundp 'subword-mode) subword-mode)
                             (subword-forward arg)
                           (progn (forward-word arg) (point)))))

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

(defun compact-uncompact-block ()
  "Remove or add line endings on the current block of text.
This command similar to a toggle for `fill-paragraph' and `unfill-paragraph'
When there is a text selection, act on the region.

When in text modes, the “current block” is equivalent to the
current paragraph.  When in programing language modes, “current block”
is defined by between empty lines.

Todo: when in a programing lang mode, make the function more
smart, so that it doesn't cut strings.  Right now, the code uses
fill* functions. A proper implementation to compact is replacing
newline chars by space when the newline char is not inside
string. To uncompact, a proper solution needs to know the basic
syntax of each lang. A simple implementation is to simply insert
newline after “}” or “;” for c-like syntaxes."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”, the
  ;; possible values are t and nil. This property is used to easily
  ;; determine whether to compact or uncompact, when this command is
  ;; called again

  (let (bds currentLineCharCount currentStateIsCompact
            (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; currentLineCharCount is used to determine whether current state
      ;; is compact or not, when the command is run for the first time
      (setq currentLineCharCount
            (progn
              (setq bds (bounds-of-thing-at-point 'line))
              (length (buffer-substring-no-properties (car bds) (cdr bds)))
              ;; Note: 'line includes eol if it is not buffer's last line
              )
            )

      ;; Determine whether the text is currently compact.  when the last
      ;; command is this, then symbol property easily tells, but when
      ;; this command is used fresh, right now we use num of chars of
      ;; the cursor line as a way to define current compatness state
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> currentLineCharCount fill-column) t nil)
              )
            )

      (if (and transient-mark-mode mark-active)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end)))
            )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil))
          )
        )

      (put this-command 'stateIsCompact-p
           (if currentStateIsCompact
               nil t)) ) ) )

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
          ;; TODO: prompt what to be copied
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

(defun my-set-paste ()
  "Avoid content indent when paste from clipboard."
  (interactive)
  (fundamental-mode)
  (setq indent-line-function 'ignore))

(defun my-generate-loaddefs ()
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
not up to date. It can also be run from the command line:

$ emacs -l ~/.emacs -batch -f byte-recompile-startup-dir"
  (interactive)
  (dolist (dir (find-subdirs-containing my-startup-dir "\\.el$"))
    (byte-recompile-directory dir 0)))

(defun antiword (&optional file width)
  "Run antiword on the entire buffer."
  (let ((doc-name (buffer-name)))
    (save-window-excursion
      (shell-command-on-region
       (point-min)
       (point-max)
       (concat
        (format "antiword -w %d " (if (null width) 78 width))
        (if file (replace-regexp-in-string " " "\\ " file t t) "-"))
       "*no-word-temp-name*")
      (kill-buffer (current-buffer)))
    (switch-to-buffer "*no-word-temp-name*")
    (view-mode)
    (rename-buffer (concat "*" doc-name "*"))))

(defun my-count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive
   (if (and mark-active transient-mark-mode)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end)
          total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

(defun print-to-pdf (file)
  "Print current buffer to a pdf file."
  (interactive
   (list (read-file-name "Choose a filename: ")))
  (let ((psfile (make-temp-file "ps"))
        (pdffile (if (string= "pdf" (file-name-extension file))
                     file
                   (format "%s.pdf" file))))
    (ps-spool-buffer-with-faces)
    (switch-to-buffer ps-spool-buffer-name)
    (write-file psfile)
    (shell-command (format "ps2pdf %s %s" psfile pdffile))
    (kill-buffer (file-name-nondirectory psfile))
    (delete-file psfile)
    (message "Saved to: %s" pdffile)))

