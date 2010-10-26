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

;;{{{ toggle char case
(defun ywb-toggle-case-char (char)
  (cond ((and (> char 64) (< char 91)) (downcase char))
        ((and (> char 96) (< char 123)) (upcase char))
        (t char)))
;;;###autoload
(defun ywb-toggle-case-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (and (not (eobp)) (< (point) end))
      (insert-char (ywb-toggle-case-char (char-after)) 1)
      (delete-char 1))))
;;;###autoload
(defun ywb-toggle-case-dwim (arg)
  (interactive "p")
  (let ((start (point))
        (end (+ (point) arg)))
    (if (and mark-active transient-mark-mode)
        (setq start (region-beginning)
              end (region-end)))
    (ywb-toggle-case-region start end)
    (goto-char end)))
;;}}}

;;{{{ manipulate item
;;;###autoload
(defun ywb-insert-item ()
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
           (ywb-sync-item))
          ((looking-at "\\s-*[-+]\\s-*")
           (progn
             (end-of-line)
             (insert "\n" (buffer-substring (match-beginning 0)
                                            (match-end 0)))))
          (t
           (progn
             (end-of-line)
             (newline-and-indent))))))
;;;###autoload
(defun ywb-sync-item ()
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

;;;###autoload
(defun ywb-find-bad-char ()
  "Find not displayable characters in current buffer."
  (interactive)
  (let ((badchar "[\x0-\x1f\x80-\xff]"))
    (goto-char (point-min))
    (if (re-search-forward badchar nil t)
        (let (char)
          (while (progn (setq char (read-event))
                        (cond ((member char '(?\^s ?n))
                               (re-search-forward badchar nil t))
                              ((member char '(?\^r ?p))
                               (re-search-backward badchar nil t)))))
          (setq unread-command-events (list char)))
      (message "No bad char found"))))

(defun ywb-find-not-encodable-char ()
  (interactive)
  (let* ((from (point-min))
         (to (point-max))
         (codings (find-coding-systems-region from to))
         (unsafe (list buffer-file-coding-system))
         (rejected nil))
    (if (member (coding-system-base buffer-file-coding-system)
                codings)
        (message "Current coding system is work!")
      (setq unread-command-events (list ?\^G))
      (select-safe-coding-system-interactively
       from to codings unsafe rejected (car codings)))))

;;;###autoload
(defun ywb-change-ftp-coding-system (coding)
  (interactive "zCoding: ")
  (when (and coding (coding-system-p coding))
    (let ((parsed (ange-ftp-ftp-name (expand-file-name default-directory))))
      (set-process-coding-system
       (get-buffer-process (ange-ftp-ftp-process-buffer
                            (nth 0 parsed) (nth 1 parsed)))
       coding coding))))

;;;###autoload
 (defun ywb-goto-section (section)
  "move to numbered section, like

1.1 blah blah
   other text

1.2 blah blah

M-x ywb-goto-section RET 1.2 RET move to line 1.2
"
  (interactive
   (list
    (let ((default (if (looking-at "\\s-*\\(\\([0-9]+\\.\\)+[0-9]+\\.?\\)")
                       (match-string 1) "")))
      (read-string
       (if default
           (format "Go to section(default %s): " default)
         "Go to section: ")
       nil nil default))))
  (push-mark)
  (goto-char (point-min))
  (while (and (re-search-forward (concat "^" (regexp-quote section)) nil t)
              (not (save-excursion (forward-line 1)
                                   (looking-at "^-+$"))))))

;; find backup
;;;###autoload
(defun ywb-revert-buffer-to-backup (backup)
  "If with prefix arg, revert to newest backup. Otherwise read a
backup file name and revert to it"
  (interactive
   (if (null buffer-file-name)
       (error "No associated file!")
     (list
      (if current-prefix-arg
          (file-newest-backup buffer-file-name)
        (completing-read "Revert to backup file: "
                         (let* ((filename (file-name-sans-versions
                                           (make-backup-file-name (expand-file-name buffer-file-name))))
                                (file (file-name-nondirectory filename))
                                (dir  (file-name-directory    filename))
                                (comp (file-name-all-completions file dir)))
                           (mapcar (lambda (f) (concat dir f)) comp))
                         nil t)))))
  (erase-buffer)
  (insert-file-contents backup))

;; run command
(defvar ywb-command-list
  '(("nautilus")
    ("gnome-terminal")))

(defun ywb-run-command (cmd)
  (interactive (list (completing-read
                      "Run: " ywb-command-list nil t)))
  (setq cmd (assoc cmd ywb-command-list))
  (apply 'start-process
         (append
          (list "cmd" nil (car cmd))
          (cdr cmd))))

(defun ywb-list-command ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*cmd*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (cmd ywb-command-list)
      (insert
       (propertize
        (concat (car cmd) " " (cdr cmd) "\n")
        'command (car cmd))))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (use-local-map
     (let ((map (make-sparse-keymap)))
       (define-key map "\C-m" (lambda ()
                                (interactive)
                                (ywb-run-command
                                 (get-text-property (point) 'command))))
       map))))

;; delete-line-not-match
(defun ywb-do-with-line (func)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (funcall func (point) (progn (forward-line 1) (point))))))
;;;###autoload
(defun ywb-delete-line-not-match (regexp)
  "Delete line not match the REGEXP."
  (interactive "sDelete Line not match: ")
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (ywb-do-with-line (lambda (start end)
                          (unless (string-match regexp (buffer-substring start end))
                            (delete-region start end)))))))

;; save-image-ap
(defun ywb-save-image-at-point ()
  (interactive)
  (let ((img (get-text-property (point) 'display))
        data file)
    (if (eq (car img) 'image)
        (progn
          (setq file (read-file-name "Save image to file: "))
          (if (setq data (plist-get :file (cdr img)))
              (copy-file data file)
            (if (setq data (plist-get :data (cdr img)))
                (with-temp-buffer
                  (insert data)
                  (write-file file))
              (message "Don't know how to save the image!"))))
      (message "No image at point!"))))

;; base64 coding
(defun ywb-base64-decode-region (beg end coding)
  (interactive "r\nzCoding system: ")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (base64-decode-region (point-min) (point-max))
      (decode-coding-region (point-min) (point-max) coding))))
(defun ywb-base64-encode-region (beg end coding)
  (interactive "r\nzCoding system: ")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (encode-coding-region (point-min) (point-max) coding)
      (base64-encode-region (point-min) (point-max)))))

;;;###autoload
;; window split
(defun ywb-toggle-window-split (arg)
  "Split window vertically or horizontally."
  (interactive "P")
  (let ((buf (window-buffer (next-window)))
        (horflag (= (car (window-edges))
                    (car (window-edges (next-window)))))
        (size (and arg (prefix-numeric-value arg))))
    (and size (< size 0)
         (setq size (+ (window-width) size)))
    (delete-other-windows)
    (set-window-buffer (split-window nil size horflag) buf)))

;;;###autoload
;; replace-in-rectangle
(defun ywb-replace-in-rectangle (beg end)
  "Replace text in the rectangle"
  (interactive "r\n")
  (let ((replace (query-replace-read-args "Replace string" nil)))
    (apply-on-rectangle (lambda (s e)
                          (setq s (progn (move-to-column s) (point))
                                e (progn (move-to-column e) (point)))
                          (perform-replace (car replace)
                                           (cadr replace) nil t nil nil nil s e))
                        beg end)))

;; csv-to-tsv
(defun ywb-csv-to-tsv (beg end)
  (interactive "r")
  (save-excursion
    (perform-replace "^[\"']?" "" nil t nil nil nil beg end)
    (perform-replace "[\"']?$" "" nil t nil nil nil beg end)
    (perform-replace "[\"']?,\\s-?[\"']?" "\t" nil t nil nil nil beg
                     end)))
;; html-preview-region
(defun ywb-html-preview-region (beg end)
  (interactive "r")
  (let ((file (make-temp-file "region-" nil ".html")))
    (write-region beg end file)
    (browse-url file)))

;; save macro
(defvar ywb-kbd-macro
  `(("gnus-delete-letter-noconfirm" . [66 backspace 121 14]))
  "my kbd macros")
(defun ywb-install-kbd-macro (macro)
  (interactive (list (completing-read "Install kbd macro(name): "
                                      (mapcar 'car ywb-kbd-macro))))
  (setq last-kbd-macro (cdr (assoc macro ywb-kbd-macro))))
(defun ywb-save-last-kbd-macro (macro)
  (interactive "sName for last macro: ")
  (add-to-list 'ywb-kbd-macro (cons macro (copy-sequence last-kbd-macro))))

;; goto-line
(defun ywb-goto-line (percent)
  (interactive (list (or current-prefix-arg
                         (string-to-number
                          (read-from-minibuffer "Goto percent: ")))))
  (let* ((total (count-lines (point-min) (point-max)))
         (num (round (* (/ total 100.0) percent))))
    ;;    (message "total: %d, go to %d" total num)
    (goto-line num)))

;; shell-command-background
;;;###autoload
(defun ywb-shell-command-background (command &optional no-output)
  (interactive (list (read-from-minibuffer "Shell Command: " nil
                                           nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((buffer (when (null no-output)
                  (get-buffer-create "*Shell Command Output*")))
        (directory default-directory))
    (with-current-buffer buffer
      (setq default-directory directory)
      (erase-buffer)
      (start-process-shell-command "shell" buffer command))
    (pop-to-buffer buffer t)))

;; count-word-region
;;;###autoload
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
;; ascii-table-show
;;;###autoload
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

;; swap-text
;;;###autoload
(defun his-swap-text (str1 str2 beg end)
  "Changes all STR1 to STR2 and all STR2 to STR1 in beg/end region."
  (interactive "sString A: \nsString B: \nr")
  (if mark-active
      (setq deactivate-mark t)
    (setq beg (point-min) end (point-max)))
  (goto-char beg)
  (while (re-search-forward
          (concat "\\(?:\\b\\(" (regexp-quote str1) "\\)\\|\\("
                  (regexp-quote str2) "\\)\\b\\)") end t)
    (if (match-string 1)
        (replace-match str2 t t)
      (replace-match str1 t t))))

;; mode25
;;; steal from `dos-fns'. In linux, don't require `dos-fns'
(defun mode25 ()
  "Changes the number of screen rows to 25."
  (interactive)
  (set-frame-size (selected-frame) 80 25))
(defun mode4350 ()
  "Changes the number of rows to 43 or 50.
Emacs always tries to set the screen height to 50 rows first.
If this fails, it will try to set it to 43 rows, on the assumption
that your video hardware might not support 50-line mode."
  (interactive)
  (set-frame-size (selected-frame) 80 50)
  (if (eq (frame-height (selected-frame)) 50)
      nil  ; the original built-in function returned nil
    (set-frame-size (selected-frame) 80 43)))

;;;###autoload
(defun rgb-mode ()
  "Display rgb.txt"
  (interactive)
  (let ((inhibit-read-only t)
        (modify-p (buffer-modified-p))
        color ov)
    (setq tab-width 8)
    (save-excursion
      (goto-char (point-min))
      (remove-overlays (point-min) (point-max))
      (while (not (eobp))
        (when (looking-at "[ \t]*\\([0-9]+\\)\\s-+\\([0-9]+\\)\\s-+\\([0-9]+\\)\\s-+")
          (setq ov (make-overlay (point) (point))
                color (format "#%02x%02x%02x"
                              (string-to-number (match-string 1))
                              (string-to-number (match-string 2))
                              (string-to-number (match-string 3))))
          (overlay-put ov 'before-string (concat color "\t"))
          (put-text-property (match-end 3) (match-end 0)
                             'face (cons 'background-color color)))
        (forward-line 1))
      (set-buffer-modified-p modify-p))
    (view-mode 1)))

;;;###autoload
(defun ywb-describe-keymap (keymap)
  (interactive (list (completing-read "Keymap: " obarray
                                      (lambda (s)
                                        (and (boundp s)
                                             (or (keymapp s)
                                                 (keymapp (symbol-value s))))))))
  (with-current-buffer (help-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert (substitute-command-keys (format "\\{%s}" keymap)))
      (run-hooks 'temp-buffer-show-hook)))
  (display-buffer (help-buffer)))

;; folding-summary
;;;###autoload
(defun ywb-folding-summary (beg end)
  (interactive "r")
  (let ((end-mark (set-marker (make-marker) end))
        name)
    (goto-char beg)
    (while (< (point) end)
      (if (looking-at "^;; \\(.*\\)$")
          (setq name (cons (match-string 1) name)))
      (forward-line 1))
    (goto-char beg)
    (if (looking-at (concat "^;;" "{{{"))
        (delete-region (line-beginning-position)
                       (progn (forward-line 1) (point))))
    (insert ";;" "{{{ " (mapconcat 'identity (reverse name) ", ") "\n")
    (goto-char end-mark)
    (beginning-of-line)
    (if (looking-at "^;;}}}")
        (delete-region (line-beginning-position)
                       (progn (forward-line 1) (point))))
    (insert ";;}}}\n")))

;; opera->bbdb
;;;###autoload
(defun ywb-bbdb-merge-opera-contact-region (beg end)
  (interactive "r")
  (let (contact)
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (re-search-forward "^#CONTACT" nil t))
        (setq contact
              (mapcar (lambda (info)
                        (string-match "=" info)
                        (cons (substring info 0 (match-beginning 0))
                              (substring info (match-end 0))))
                      (split-string (buffer-substring
                                     (point)
                                     (progn (unless (re-search-forward "\n\n" nil t)
                                              (goto-char end))
                                            (point)))
                                    "\\s-*\n\\s-*" t)))
        (bbdb-merge-interactively (assoc-default "NAME" contact)
                                  nil
                                  (assoc-default "MAIL" contact)
                                  nil nil nil)))))
;; transpose table
;;;###autoload
(defun ywb-transpose-table (beg end del)
  "Transpose Table in the region."
  (interactive "r\nP")
  (let ((table (remove-if 'null
                          (mapcar
                           (lambda (row)
                             (split-string row "\t"))
                           (split-string
                            (funcall (if del 'delete-and-extract-region
                                       'buffer-substring-no-properties)
                                     beg end) "\n")))))
    (dotimes (i (length (car table)))
      (insert (mapconcat (lambda (row) (nth i row)) table "\t") "\n"))))

;; join-buffer
;;;###autoload
(defun ywb-join-buffer (buf1 buf2 &optional sep)
  (interactive
   (list (read-buffer "Buffer A: " (buffer-name) t)
         (read-buffer "Buffer B: " (buffer-name (window-buffer
                                                 (next-window))) t)
         current-prefix-arg))
  (setq sep
        (if sep
            (read-from-minibuffer "Use seperator: ")
          "\t"))
  (let (done)
    (save-excursion
      (set-buffer buf2)
      (goto-char (point-min))
      (set-buffer buf1)
      (goto-char (point-min))
      (while (or done (not (eobp)))
        (end-of-line 1)
        (insert sep
                (save-excursion
                  (set-buffer buf2)
                  (if (eobp)
                      (progn (setq done t) "")
                    (buffer-substring-no-properties
                     (point)
                     (progn
                       (forward-line 1)
                       (1- (point)))))))
        (forward-line 1)))))
;; ooo-table
;;;###autoload
(defun ywb-ooo-table-convert (col beg end)
  (interactive "nHow many columns: \nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (setq col (1- col))
      (while (not (eobp))
        (setq beg (point))
        (forward-line col)
        (perform-replace "\n" "\t" nil nil nil nil nil beg (point))
        (forward-line 1)))))

(defun ywb-quotify-region (char beg end)
  (interactive "sQuote char: \nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (skip-syntax-forward "^w")
      (while (not (eobp))
        (insert "\"")
        (skip-chars-forward "-a-zA-Z")
        (insert "\"")
        (skip-syntax-forward "^w")))))

;;;###autoload
(define-derived-mode asciidoc-mode outline-mode "AsciiDoc"
  "Major mode for editing asciidoc file"
  (setq outline-regexp "[=\f]+")
  )
