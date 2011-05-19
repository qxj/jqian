;;; * patch for some elisp

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
  (ad-activate 'browse-url-file-url)

  ;;# this defadvice is un-necessary, apt-get install emacs23-el
  ;; (defadvice find-library-name (before find-library-new-place activate)
  ;;   "Find library in another source path."
  ;;   (ad-set-arg 0 (replace-regexp-in-string "/usr/share/emacs/23.1/"
  ;;                                           "~/src/emacs-23.2/"
  ;;                                           (ad-get-arg 0))))
  )

;;{{{ imenu -> ido-completing-read
(defun ido-imenu-completion (index-alist &optional prompt)
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
        choice
        (prepared-index-alist
         (if (not imenu-space-replacement) index-alist
           (mapcar
            (lambda (item)
              (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
                                          (car item))
                    (cdr item)))
            index-alist))))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (setq name (ido-completing-read "Index item: "
                                    (mapcar 'car prepared-index-alist)
                                    nil t nil 'imenu--history-list
                                    (and name (imenu--in-alist
                                               name prepared-index-alist) name)))
    (when (stringp name)
      (setq choice (assoc name prepared-index-alist))
      (if (imenu--subalist-p choice)
          (imenu--completion-buffer (cdr choice) prompt)
        choice))))
(defalias 'imenu--completion-buffer 'ido-imenu-completion)
;;}}}

;;{{{ gtags
(eval-after-load "gtags"
  '(progn
     (defun gtags-goto-tag (tagname flag &optional other-win)
       (let (option context save prefix buffer lines flag-char)
         (setq save (current-buffer))
         (setq flag-char (string-to-char flag))
                                        ; Use always ctags-x format.
         (setq option "-x")
         (if (char-equal flag-char ?C)
             (setq context (concat "--from-here=" (number-to-string (gtags-current-lineno)) ":" buffer-file-name))
           (setq option (concat option flag)))
         (cond
          ((char-equal flag-char ?C)
           (setq prefix "(CONTEXT)"))
          ((char-equal flag-char ?P)
           (setq prefix "(P)"))
          ((char-equal flag-char ?g)
           (setq prefix "(GREP)"))
          ((char-equal flag-char ?I)
           (setq prefix "(IDUTILS)"))
          ((char-equal flag-char ?s)
           (setq prefix "(S)"))
          ((char-equal flag-char ?r)
           (setq prefix "(R)"))
          (t (setq prefix "(D)")))
         ;; load tag
         (if gtags-select-buffer-single
             (progn
                                        ; delete "*GTAGS SELECT*" buffer info from gtags-buffer-stack and gtags-point-stack
               (let (now-gtags-buffer-stack now-buffer now-gtags-point-stack now-point)
                 (setq now-gtags-buffer-stack (reverse gtags-buffer-stack))
                 (setq now-gtags-point-stack (reverse gtags-point-stack))
                 (setq gtags-buffer-stack nil)
                 (setq gtags-point-stack nil)
                 (while now-gtags-buffer-stack
                   (setq now-buffer (car now-gtags-buffer-stack))
                   (setq now-point (car now-gtags-point-stack))
                   (if (and (buffer-name now-buffer) (not (string-match "*GTAGS SELECT*" (buffer-name now-buffer))))
                       (progn
                         (setq gtags-buffer-stack (cons now-buffer gtags-buffer-stack))
                         (setq gtags-point-stack (cons now-point gtags-point-stack))))
                   (setq now-gtags-buffer-stack (cdr now-gtags-buffer-stack))
                   (setq now-gtags-point-stack (cdr now-gtags-point-stack))))
                                        ; kill "*GTAGS SELECT*" buffer
               (let (now-buffer-list now-buffer)
                 (setq now-buffer-list (buffer-list))
                 (while now-buffer-list
                   (setq now-buffer (car now-buffer-list))
                   (if (string-match "*GTAGS SELECT*" (buffer-name now-buffer))
                       (kill-buffer now-buffer))
                   (setq now-buffer-list (cdr now-buffer-list))))))
         (setq buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
         (set-buffer buffer)
                                        ;
                                        ; Path style is defined in gtags-path-style:
                                        ;   root: relative from the root of the project (Default)
                                        ;   relative: relative from the current directory
                                        ;   absolute: absolute (relative from the system root directory)
                                        ;
         (cond
          ((equal gtags-path-style 'absolute)
           (setq option (concat option "a")))
          ((equal gtags-path-style 'root)
           (let (rootdir)
             (if gtags-rootdir
                 (setq rootdir gtags-rootdir)
               (setq rootdir (gtags-get-rootpath)))
             (if rootdir (cd rootdir)))))
         (message "Searching %s ..." tagname)
         (if (not (= 0 (if (equal flag "C")
                           (call-process "global" nil t nil option "--encode-path=\" \t\"" context tagname)
                         (call-process "global" nil t nil option "--encode-path=\" \t\"" tagname))))
             (progn (message (buffer-substring (point-min)(1- (point-max))))
                    (gtags-pop-context))

           ;;# patch:
           ;; global return 0, some warning messages should be discarded
           (progn
             (goto-char (point-min))
             (while (search-forward-regexp "^Warning:.+\n" nil t)
               (replace-match "" nil t)))
           ;;# end patch.

           (goto-char (point-min))
           (setq lines (count-lines (point-min) (point-max)))
           (cond
            ((= 0 lines)
             (cond
              ((char-equal flag-char ?P)
               (message "%s: path not found" tagname))
              ((char-equal flag-char ?g)
               (message "%s: pattern not found" tagname))
              ((char-equal flag-char ?I)
               (message "%s: token not found" tagname))
              ((char-equal flag-char ?s)
               (message "%s: symbol not found" tagname))
              (t
               (message "%s: tag not found" tagname)))
             (gtags-pop-context)
             (kill-buffer buffer)
             (set-buffer save))
            ((= 1 lines)
             (message "Searching %s ... Done" tagname)
             (gtags-select-it t other-win))
            (t
             (if (null other-win)
                 (switch-to-buffer buffer)
               (switch-to-buffer-other-window buffer))
             (gtags-select-mode))))))))
;;}}}

;;{{{ woman
;; (setq woman-rebuild-exclude-path
;;       (remove-if-not (lambda (path) (string-match "^/usr" path))
;;                      woman-manpath))
(eval-after-load "woman"
  '(progn
     (defun ywb-directory-files (dir &optional full match nosort)
       (let ((files
              (split-string
               (shell-command-to-string
                (format "perl -e 'opendir(DIR, $ARGV[0]); print join(qq{\n}, grep {/$ARGV[1]/} readdir(DIR))' '%s' '%s'"
                        dir (or match ".")))
               "\n" t)))
         (if full
             (setq files (mapcar (lambda (f) (expand-file-name f dir)) files)))
         (or nosort
             (setq files (sort files 'string<)))
         files))
     (defun woman-file-name-all-completions (topic)
       "Return an alist of the files in all man directories that match TOPIC."
       ;; Support 3 levels of caching: each element of
       ;; woman-topic-all-completions is a list of one of the forms:
       ;;   (topic)
       ;;   (topic (path-index) (path-index) ... )
       ;;   (topic (path-index filename) (path-index filename) ... )
       ;; where the are no duplicates in the value lists.
       ;; Topic must match first `word' of filename, so ...
       (let ((topic-regexp
              (concat "^" (regexp-quote topic)
                      "(\\..+)*\\.([0-9lmnt]\\w*)(\\.(g?z|bz2))?$"))
             (topics woman-topic-all-completions)
             (path woman-expanded-directory-path)
             dir files)
         (if (cdr (car topics))
             ;; Use cached path-info to locate files for each topic:
             (let ((path-info (cdr (assoc topic topics)))
                   filename)
               (while path-info
                 (setq dir (nth (car (car path-info)) path)
                       filename (car (cdr (car path-info)))
                       path-info (cdr path-info)
                       files (nconc files
                                    ;; Find the actual file name:
                                    (if filename
                                        (list (concat dir "/" filename))
                                      (ywb-directory-files dir t topic-regexp)
                                      )))))
           ;; Search path for the files for each topic:
           (while path
             (setq dir (car path)
                   path (cdr path))
             (if (woman-not-member dir path) ; use each directory only once!
                 (setq files (nconc files
                                    (directory-files dir t topic-regexp))))
             ))
         (mapcar 'list files)
         ))))
;;}}}

;;{{{ filesets-add-buffer
(eval-after-load "filesets"
  '(progn
     (defun filesets-add-buffer (&optional name buffer)
       "Add BUFFER (or current-buffer) to the fileset called NAME.
User will be queried, if no fileset name is provided."
       (interactive)
       (let* ((buffer (or buffer
                          (current-buffer)))
              (file (or (buffer-file-name buffer)
                        (if (eq major-mode 'dired-mode)
                            (buffer-local-value 'default-directory buffer)
                          (error "No file name found for the buffer"))))
              (name   (or name
                          (completing-read
                           (format "Add '%s' to fileset: " buffer)
                           filesets-data nil)))
              (entry  (or (assoc name filesets-data)
                          (when (y-or-n-p
                                 (format "Fileset %s does not exist. Create it? "
                                         name))
                            (progn
                              (add-to-list 'filesets-data (list name '(:files)))
                              (message
                               "Fileset %s created.  Call `M-x filesets-save-config' to save."
                               name)
                              (car filesets-data))))))
         (if entry
             (let* ((files  (filesets-entry-get-files entry))
                    (this   (or (buffer-file-name buffer)
                                (substring (buffer-local-value 'default-directory buffer) 0 -1)))
                    (inlist (filesets-member this files
                                             :test 'filesets-files-equalp)))
               (cond
                (inlist
                 (message "Filesets: '%s' is already in '%s'" this name))
                ((and (equal (filesets-entry-mode entry) ':files)
                      this)
                 (filesets-entry-set-files entry (cons this files) t)
                 (filesets-set-config name 'filesets-data filesets-data))
                (t
                 (message "Filesets: Can't add '%s' to fileset '%s'" this name)))))))
     (defun filesets-open-file ()
       (interactive)
       (let (set file)
         (setq set (completing-read "Open file in set: " filesets-data nil t))
         (setq set
               (delq nil
                     (mapcar (lambda (file)
                               (if (and (vectorp file)
                                        (eq (car (aref file 1)) 'filesets-file-open))
                                   (cons (aref file 0) (aref file 1))))
                             (cdr (cadr (member set filesets-submenus))))))
         (find-file (cadr (nth 2 (assoc-default (ido-completing-read "file: " set) set))))))
     ))
;;}}}

;;{{{ sourcepair, auto create paired source code
(eval-after-load "sourcepair"
  '(progn
     (defun sourcepair-load ()
       "Load the corresponding C/C++ header or source file for the current buffer.

This function can be invoked by \\[sourcepair-load].  It will load the the
corresponding header or source file for the current buffer.  For example, if
you are looking at the file FooParser.cpp and press \\[sourcepair-load], the
file FooParser.h will be loaded.  It also works the other way as well.

There are five global variables that can be used to adjust how the function
works:

 `sourcepair-source-extensions'
 `sourcepair-header-extensions'
 `sourcepair-source-path'
 `sourcepair-header-path'
 `sourcepair-recurse-ignore'

See the documentation for these variables for more info.
"
       (interactive)
       (catch 'found-matching-file
         (let* ((temp (sourcepair-analyze-filename (file-name-nondirectory (buffer-file-name))))
                (search-path (car temp))
                (possible-filenames (cdr temp)))
           (if (= (length possible-filenames) 0)
               (message "%s is not a recognized source or header file (consider \
updating sourcepair-source-extensions or sourcepair-header-extensions)"
                        (buffer-name))
             (progn
               (while search-path
                 (let ((path-to-check (car search-path))
                       (matching-filename nil))
                   (if (and (> (length path-to-check) 3)
                            (equal (substring path-to-check -2) "/*"))
                       (setq matching-filename (sourcepair-find-one-of (substring path-to-check 0 -2)
                                                                       possible-filenames
                                                                       t))
                     (setq matching-filename
                           (sourcepair-find-one-of path-to-check possible-filenames nil)))

                   (if (eq matching-filename nil)
                       (setq search-path (cdr search-path))
                     (throw 'found-matching-file (find-file matching-filename)))))
               (if (y-or-n-p "No matching file found. Create one? ")
                   (find-file (completing-read "file name: " possible-filenames))))))))))
;;}}}

;;{{{ template-expand-template
(eval-after-load "template"
  '(defun template-expand-template (template)
  "Expand template file TEMPLATE and insert result in current buffer.
Using a template for inserting some text consists of:
 (1) Template derivation: suggest a reasonable template file to the user
     according to `buffer-file-name', see `template-derivation-alist'.
 (2) Template insertion: insert the template file at point into the
     current buffer.
 (3..9) as steps (6..12) of `template-new-file'."
  (interactive
   (let* ((use (template-derivation (expand-file-name (or buffer-file-name
                                                          "NONE"))
                                    t))
          (tpl (ido-read-file-name "Insert and expand template: "
                               (file-name-directory (cdr use))
                               (file-name-nondirectory (cdr use))
                               t
                               (file-name-nondirectory (cdr use)))))
     (if (string= tpl "")
         (error "No template file provided"))
     (list (expand-file-name tpl (file-name-directory (cdr use))))))
  (save-restriction
    (narrow-to-region (point) (point))
    (template-new-file nil template t))))
;;}}}

;;{{{ shell-cd
(eval-after-load "shell"
  '(defun shell-cd (dir)
     "Do normal `cd' to DIR, and set `list-buffers-directory'."
     (cd dir)
     (when shell-dirtrackp
       (setq list-buffers-directory default-directory)
       (rename-buffer  (concat "*shell: " default-directory "*") t))))
;;}}}



