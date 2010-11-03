;;; * patch for some elisp

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



