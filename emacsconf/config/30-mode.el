;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

;;; autoload
(deh-section "autoloads"
  (autoload 'php-mode "php-mode" "php mode" t)
  (autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
  (autoload 'git-status "git" "" t)
  (autoload 'yaml-mode "yaml-mode" "YAML major mode" t)
  (autoload 'bat-mode "bat-mode" "Bat mode for Windows batch file" t)
  (autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
  (autoload 'python-mode "python" "Python editing mode." t)
  (autoload 'visual-basic-mode "vb-mode" "Visual Basic Mode" t)
  ;; (autoload 'pod-mode "pod-mode" "A major mode to edit pod" t)
  (autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
  (autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)
  (autoload 'sourcepair-load "sourcepair" nil t)
  ;; emacs lock
  (autoload 'toggle-emacs-lock "emacs-lock" "Emacs lock" t)
  ;; iimage
  (autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
  (autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
  (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)
  (autoload 'yaml-mode "yaml-mode" "Simple mode to edit YAML." t)
  (autoload 'woman-mode "woman")
  (add-hook 'woman-mode-hook 'view-mode)
  (autoload 'woman-decode-buffer "woman")
  ;; (autoload 'muse-insert-list-item "muse-mode" t)
  ;; wb-line
  ;; (autoload 'wb-line-number-toggle "wb-line-number" nil t)
  ;; htmlize
  (autoload 'htmlize-buffer "htmlize" "htmlize buffer" t)
  ;; moccur
  (autoload 'moccur-grep "moccur-edit" "Glob search file" t)
  (autoload 'moccur "moccur-edit" "moccur" t)
  ;; blank-mode
  (autoload 'blank-mode-on "blank-mode" "Turn on blank visualization."   t)
  (autoload 'blank-mode-off "blank-mode" "Turn off blank visualization."  t)
  (autoload 'blank-mode "blank-mode" "Toggle blank visualization."    t)
  (autoload 'blank-mode-customize "blank-mode" "Customize blank visualization." t)
  ;; hexl editor
  ;; (autoload 'hexl-mode "hexl+" "Edit a file in a hex dump format" t)
  ;; A visual table editor, very cool
  (autoload 'table-insert "table" "WYGIWYS table editor")
  ;; ansit
  (autoload 'ansit-ansify-this "ansit"  "Ansi the region." t)
  ;; rst-mode
  (autoload 'rst-mode "rst" "" t)
  ;; minibuf-isearch
  (autoload 'minibuf-isearch-next "minibuf-isearch" "" t)
  (autoload 'minibuf-isearch-prev "minibuf-isearch" "" t)
  (autoload 'sdcv-search "sdcv-mode" "Search dictionary using sdcv" t)
  )

;;; auto detect mode
(deh-section "auto-mode"
  (add-to-list 'auto-mode-alist '("\\.doc\\'" . antiword))
  (add-to-list 'auto-mode-alist '("\\.proc?$" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(ya?ml\\|fb\\)$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(php[345]?\\|module\\|phtml\\|inc\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\)$" . visual-basic-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("apache2?/access" . apache-log-generic-mode))
  (add-to-list 'auto-mode-alist '("\\(Makefile\\|Build\\)" . makefile-mode))
  (add-to-list 'auto-mode-alist '("\.schemas" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(p6\\|tdy\\|cgi\\|t\\)$" . perl-mode))
  (add-to-list 'auto-mode-alist '("\\.xs$" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
  (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))
  )

(deh-section "magic-mode"
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*@implementation" . objc-mode))
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*@interface" . objc-mode))
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*@protocol" . objc-mode))
  ;; (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*class" . c++-mode))
  (add-to-list 'magic-mode-alist '("\\`<\\?php" . php-mode))
  )

(deh-section "interpreter-mode"
  (add-to-list 'interpreter-mode-alist '("php" . php-mode))
  )

;;; common mode hook
(deh-section "mode-common"
  (defun my-mode-common-hook ()
    (setq tab-width 4)
    (setq c-basic-offset tab-width)
    (setq indent-tabs-mode nil)
    ;; (abbrev-mode t)
    (set (make-local-variable 'comment-style) 'indent)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence tab-width 80 tab-width))

    ;; (when (fboundp 'whitespace-mode) (whitespace-mode t))
    (hs-minor-mode 1)
    (ignore-errors (imenu-add-menubar-index))

    ;; (local-set-key (kbd "RET")
    ;;                (lambda () (interactive)
    ;;                  (if (my-cursor-on-comment-p) (comment-indent-new-line)
    ;;                    (if (boundp 'autopair-newline) (autopair-newline)
    ;;                      (newline-and-indent)))))

    ;; untabify source code
    (make-local-hook 'write-contents-hooks)
    (add-hook 'write-contents-hooks 'my-untabify nil t)
    )

  ;; comment new line and indent `M-j', as VIM acts.
  (defun my-cursor-on-comment-p (&optional point)
    (memq (get-text-property (or point (point)) 'face)
          '(font-lock-comment-face)))

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
  )

;;; tags
(deh-section "ebrowse"
  (add-to-list 'auto-mode-alist '("BROWSE\\.*" . ebrowse-tree-mode))
  (setq ebrowse-global-prefix-key "\C-z"))

(deh-section "etags"
  (defun my-find-top-directory (file &optional dir)
    (or dir (setq dir (expand-file-name default-directory)))
    (let ((thefile (expand-file-name file dir)))
      (if (file-exists-p thefile)
          thefile
        (setq pdir (directory-file-name (file-name-directory dir)))
        (if (string= pdir dir)
            nil
          (my-find-top-directory file pdir)))))
  (setq tags-add-tables nil
        default-tags-table-function
        (lambda nil
          (my-find-top-directory "TAGS"))))

(deh-section-if "gtags" (executable-find "global")
  (autoload 'gtags-mode "gtags" "" t)

  (deh-add-hooks (c-mode-common-hook)
    (gtags-mode t))

  (setq gtags-mode-hook
    '(lambda ()
       (setq gtags-pop-delete t)
       (setq gtags-path-style 'absolute)
  ))

  (setq gtags-select-mode-hook
    '(lambda ()
       (setq hl-line-face 'underline)
       (hl-line-mode 1)
  ))

  (eval-after-load "gtags"
    '(deh-define-key gtags-mode-map
       ;; Instead of `find-tag' & `pop-tag-mark'
       ((kbd "M-.") . 'gtags-find-tag)
       ((kbd "M-*") . 'gtags-pop-stack)
       ;; other key binds
       ("\C-cgv" . 'gtags-visit-rootdir)
       ("\C-cgt" . 'gtags-find-tag-from-here)
       ("\C-cgo" . 'gtags-find-tag-other-window)
       ("\C-cgr" . 'gtags-find-rtag)
       ("\C-cgs" . 'gtags-find-symbol)
       ("\C-cgp" . 'gtags-find-pattern)
       ("\C-cgg" . 'gtags-find-with-grep)
       ("\C-cgi" . 'gtags-find-with-idutils)
       ("\C-cgf" . 'gtags-find-file)
       ("\C-cga" . 'gtags-parse-file)
       ("\C-cgb" . 'gtags-append-tags)
       ("\C-cgd" . 'gtags-display-tag)
       ("\C-cgq" . 'gtags-display-tag-quit)
       ("q"      . 'gtags-display-tag-quit)))
  (defun gtags-append-tags ()
    (interactive)
    (if gtags-mode
        (progn
          (message "start to global -u")
          (start-process "gtags-name" "*gtags-var*" "global" "-u"))))

  ;; Only display tags in another window, hacked by julian
  (defvar gtags-previous-window-conf nil
    "Window configuration before switching to gtags buffer.")
  (defun gtags-display-tag ()
    "Input tag name and move to the definition."
    (interactive)
    (let (tagname prompt input)
      (setq tagname (gtags-current-token))
      (if tagname
          (setq prompt (concat "Find tag: (default " tagname ") "))
        (setq prompt "Find tag: "))
      (setq input (completing-read prompt 'gtags-completing-gtags
                                   nil nil nil gtags-history-list))
      (if (not (equal "" input)) (setq tagname input))
      (if (and (boundp 'gtags-select-buffer-single) ; >= v5.9.0
               gtags-select-buffer-single)
          (progn
            (let ((now-buffer-list (buffer-list)) now-buffer)
              (while now-buffer-list
                (setq now-buffer (car now-buffer-list))
                (if (string-match "*GTAGS SELECT*" (buffer-name now-buffer))
                    (kill-buffer now-buffer))
                (setq now-buffer-list (cdr now-buffer-list))))))

      ;; save windows configuration
      (setq gtags-previous-window-conf (current-window-configuration))

      (let* ((option "-x")
             (save (current-buffer))
             (prefix "(D)")
             (buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
             context
             lines)
        ;; (set-buffer buffer)
        (pop-to-buffer buffer nil t)    ; keep pop buffer in the same window

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
        (if (not (= 0 (call-process "global" nil t nil option tagname)))
        ;; (if (not (= 0 (call-process "global" nil t nil option "--encode-path=\" \t\"" tagname)))
            (message (buffer-substring (point-min)(1- (point-max))))
          ;; else goto line
          (goto-char (point-min))
          (setq lines (count-lines (point-min) (point-max)))
          (cond
           ((= 0 lines)
            (message "%s: tag not found" tagname)
            (kill-buffer buffer)
            ;; restore window config?
            )
           ((= 1 lines)
            (message "Searching %s ... Done" tagname)
            (gtags-select-it t nil)
            (recenter))
           (t
            (switch-to-buffer buffer)
            (gtags-select-mode)))))))
  (defun gtags-display-tag-quit ()
    "Quit gtags display buffer."
    (interactive)
    (if (window-configuration-p gtags-previous-window-conf)
        (progn
          (bury-buffer)
          (set-window-configuration gtags-previous-window-conf)
          (setq gtags-previous-window-conf nil))
      (self-insert-command 1)))
  )

(deh-require-reserved 'xcscope
  (setq cscope-database-regexps
        '(
          ("^/home/jqian/nbusrc"
           (t)
           ("/home/jqian/tags/")
           ("/home/jqian/")
           t
           ("/net/code/srt/nb_sync/MAIN/cscope" ("-d")))
          ("^/home/jqian/projects"
           (t)
           ("/home/jqian/projects" ("-d" "-I/usr/local/include")))
          ))
  (setq cscope-do-not-update-database t
        cscope-adjust nil)
  ;; keybinds
  (setq cscope-minor-mode-hooks
        '(lambda ()
           ;; Instead of `find-tag' & `pop-tag-mark'
           (deh-define-key cscope:map
             ((kbd "M-.") . 'cscope-find-this-symbol)
             ((kbd "M-*") 'cscope-pop-mark))
           ;; Key bind for cscope-minor-mode
           ))
  ;; hack `xcscope.el', remove hooks
  (deh-remove-hooks (c-mode-hook c++-mode-hook dired-mode-hook)
    (function cscope:hook)))

(deh-require 'tags-view
  (deh-define-key tags-history-mode-map
    ("q" . 'tv-view-history-quit))

  (defvar tv-previous-window-conf nil
    "Window configuration before switching to sdcv buffer.")
  (defun tv-view-history ()
    "The main entry point; pops open a buffer with the list of
locations on the tag stack that can then optionally be operated
on (eg, jumping to that location, deleting it from the list,
etc).  The following options will be available:

\\{tags-history-mode-map}"
    (interactive)
    ;; save windows configuration
    (setq gtags-previous-window-conf (current-window-configuration))
    (let* ((buf (get-buffer-create "*tags history*"))
           (backend (tv-determine-backend))
           (tag-items (tv-get-tags-list backend)))
      (pop-to-buffer buf nil t)
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (tags-history-mode)
      (set (make-local-variable 'tv-tags-backend) backend)
      (tv-insert-items tag-items)
      (setq buffer-read-only t)
      (goto-char 0)))
  (defun tv-jump-to-tag-and-quit (location)
    "Jump to tag location and quit tags view history."
    (interactive "d")
    (with-tag-info location (buf posn stack)
      (switch-to-buffer buf)
      (goto-char posn)
      (if (window-configuration-p gtags-previous-window-conf)
          (progn
            (bury-buffer)
            (set-window-configuration gtags-previous-window-conf)
            (setq gtags-previous-window-conf nil)))))
  (defun tv-view-history-quit ()
    "Quit tags view history buffer."
    (interactive)
    (if (window-configuration-p gtags-previous-window-conf)
        (progn
          (bury-buffer)
          (set-window-configuration gtags-previous-window-conf)
          (setq gtags-previous-window-conf nil))))
  )

(deh-require 'psvn
  (defsubst svn-status-interprete-state-mode-color (stat)
    "Interpret vc-svn-state symbol to mode line color"
    (case stat
      ('up-to-date "GreenYellow")
      ('edited     "tomato")
      ('unknown    "gray")
      ('added      "blue")
      ('deleted    "red")
      ('unmerged   "purple")
      (t           "black")))

  ;; (setq vc-svn-diff-switches nil
  ;;       vc-diff-switches '("--normal" "-bB"))
  )

(deh-require 'git-emacs-autoloads
  (setq git-state-modeline-decoration 'git-state-decoration-large-dot)
)

(deh-section "woman"
  (setq woman-cache-filename (expand-file-name "emacs.wmncach.el" my-temp-dir)
        woman-manpath '("/usr/man"
                        "/usr/share/man"
                        "/usr/X11R6/man"
                        "/usr/local/man"
                        "/usr/share/man/zh_TW"
                        "/usr/share/man/zh_CN")
        woman-manpath-man-regexp (regexp-opt '("man2" "man3" "man7"))
        woman-imenu t
        woman-fontify t
        woman-use-own-frame nil)
  )

(deh-section-after "info"
  (add-to-list 'Info-default-directory-list "~/info")

  (deh-define-key Info-mode-map
    ("j" . 'next-line)
    ("k" . 'previous-line))

  (defun my-toggle-info ()
    "Switch to info buffer or return to the previous buffer."
    (interactive)
    (if (derived-mode-p 'info-mode)
        (while (derived-mode-p 'info-mode)
          (bury-buffer))
      ;; Find the first info buffer
      (let ((list (buffer-list)))
        (while list
          (if (with-current-buffer (car list)
                (derived-mode-p 'info-mode))
              (progn
                (switch-to-buffer (car list))
                (setq list nil))
            (setq list (cdr list))))
        (unless (derived-mode-p 'info-mode)
          (call-interactively 'info)))))

  (require 'info+))

(deh-section-reserved "flyspell"
  ;; flyspell-goto-next-error: `C-,'
  ;; (ispell-change-dictionary)
  (deh-add-hooks (text-mode-hook org-mode-hook) (flyspell-mode 1))
  (deh-add-hooks (change-log-mode-hook log-edit-mode-hook) (flyspell-mode -1))
  (deh-add-hooks (c-mode-common-hook python-mode-hook) (flyspell-prog-mode)))

(deh-section-reserved "flymake"
  (eval-after-load "flymake"
    '(progn
       (autoload 'flymake-find-file-hook "flymake" "" t)
       (defun my-flymake-find-file-hook ()
         (condition-case nil
             (flymake-find-file-hook)
           (error nil)))
       (add-hook 'find-file-hooks 'my-flymake-find-file-hook t)

       ;; (flymake-mode t)

       (setq flymake-gui-warnings-enabled nil
             ;; flymake-allowed-file-name-masks '()
             flymake-log-level 0
             flymake-no-changes-timeout 5.0)

       ;; (deh-add-hooks (c-mode-common-hook makefile-mode-hook)
       ;;      ((kbd "C-c C-v") . 'flymake-goto-next-error))

       (defun flymake-display-current-error ()
         "Display errors/warnings under cursor."
         (interactive)
         (let ((ovs (overlays-in (point) (1+ (point)))))
           (catch 'found
             (dolist (ov ovs)
               (when (flymake-overlay-p ov)
                 (message (overlay-get ov 'help-echo))
                 (throw 'found t))))))
       (defun flymake-goto-next-error-disp ()
         "Go to next error in err ring, then display error/warning."
         (interactive)
         (flymake-goto-next-error)
         (flymake-display-current-error))
       (defun flymake-goto-prev-error-disp ()
         "Go to previous error in err ring, then display error/warning."
         (interactive)
         (flymake-goto-prev-error)
         (flymake-display-current-error))
       (defvar flymake-mode-map (make-sparse-keymap))
       (deh-define-key flymake-mode-map
         ((kbd "C-c <f4>")   . 'flymake-goto-next-error-disp)
         ((kbd "C-c <S-f4>") . 'flymake-goto-prev-error-disp)
         ((kbd "C-c <C-f4>") . 'flymake-display-err-menu-for-current-line))
       (or (assoc 'flymake-mode minor-mode-map-alist)
           (setq minor-mode-map-alist
                 (cons (cons 'flymake-mode flymake-mode-map)
                       minor-mode-map-alist)))

       ;; Directly use gcc instead of Makefile
       (setq flymake-allowed-file-name-masks '())
       (when (executable-find "texify")
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.tex\\'" flymake-simple-tex-init))
         (add-to-list 'flymake-allowed-file-name-masks
                      '("[0-9]+\\.tex\\'"
                        flymake-master-tex-init flymake-master-cleanup)))
       (when (executable-find "xml")
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.xml\\'" flymake-xml-init))
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.html?\\'" flymake-xml-init)))
       (when (executable-find "perl")
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.p[ml]\\'" flymake-perl-init)))
       (when (executable-find "php")
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.php[345]?\\'" flymake-php-init)))
       (when (executable-find "make")
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.idl\\'" flymake-simple-make-init))
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.java\\'"

                        flymake-simple-make-java-init flymake-simple-java-cleanup))
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.cs\\'" flymake-simple-make-init)))
       (add-to-list 'flymake-allowed-file-name-masks
                    '("\\.el$" flymake-elisp-init))
       ;; (add-hook 'write-file-functions (lambda nil
       ;;                                   (when (eq major-mode 'emacs-lisp-mode)
       ;;                                     (check-parens))))
       (defun flymake-elisp-init ()
         (if (string-match "^ " (buffer-name))
             nil
           (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
                  (local-file (file-relative-name
                               temp-file
                               (file-name-directory buffer-file-name))))
             (list
              (expand-file-name invocation-name invocation-directory)
              (list
               "-Q" "--batch" "--eval"
               (prin1-to-string
                (quote
                 (dolist (file command-line-args-left)
                   (with-temp-buffer
                     (insert-file-contents file)
                     (emacs-lisp-mode)
                     (condition-case data
                         (scan-sexps (point-min) (point-max))
                       (scan-error
                        (goto-char(nth 2 data))
                        (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                       file (line-number-at-pos)))))))))
               local-file)))))
       (when (or (executable-find "make")
                 (executable-find "gcc")
                 (executable-find "g++"))
         (defvar flymake-makefile-filenames '("Makefile" "makefile" "GNUmakefile")
           "File names for make.")
         (defun flymake-get-gcc-cmdline (source base-dir)
           (let ((cc (if (string= (file-name-extension source) "c") "gcc" "g++")))
             (list cc
                   (list "-Wall"
                         "-Wextra"
                         "-pedantic"
                         "-fsyntax-only"
                         "-I.."
                         "-I../include"
                         "-I../inc"
                         "-I../common"
                         "-I../public"
                         "-I../.."
                         "-I../../include"
                         "-I../../inc"
                         "-I../../common"
                         "-I../../public"
                         source))))
         (defun flymake-init-find-makfile-dir (source-file-name)
           "Find Makefile, store its dir in buffer data and return its dir, if found."
           (let* ((source-dir (file-name-directory source-file-name))
                  (buildfile-dir nil))
             (catch 'found

               (dolist (makefile flymake-makefile-filenames)
                 (let ((found-dir (flymake-find-buildfile makefile source-dir)))
                   (when found-dir
                     (setq buildfile-dir found-dir)
                     (setq flymake-base-dir buildfile-dir)
                     (throw 'found t)))))
             buildfile-dir))
         (defun flymake-simple-make-gcc-init-impl (create-temp-f
                                                   use-relative-base-dir
                                                   use-relative-source)
           "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
           (let* ((args nil)
                  (source-file-name buffer-file-name)
                  (source-dir (file-name-directory source-file-name))
                  (buildfile-dir
                   (and (executable-find "make")
                        (flymake-init-find-makfile-dir source-file-name)))
                  (cc (if (string= (file-name-extension source-file-name) "c")
                          "gcc"
                        "g++")))
             (if (or buildfile-dir (executable-find cc))
                 (let* ((temp-source-file-name
                         (ignore-errors
                           (flymake-init-create-temp-buffer-copy create-temp-f))))
                   (if temp-source-file-name
                       (setq args
                             (flymake-get-syntax-check-program-args
                              temp-source-file-name
                              (if buildfile-dir buildfile-dir source-dir)
                              use-relative-base-dir
                              use-relative-source
                              (if buildfile-dir
                                  'flymake-get-make-cmdline
                                'flymake-get-gcc-cmdline)))
                     (flymake-report-fatal-status
                      "TMPERR"
                      (format "Can't create temp file for %s" source-file-name))))
               (flymake-report-fatal-status
                "NOMK" (format "No buildfile (%s) found for %s, or can't found %s"

                               "Makefile" source-file-name cc)))
             args))
         (defun flymake-simple-make-gcc-init ()
           (flymake-simple-make-gcc-init-impl 'flymake-create-temp-inplace t t))
         (defun flymake-master-make-gcc-init (get-incl-dirs-f
                                              master-file-masks
                                              include-regexp)
           "Create make command line for a source file
 checked via master file compilation."
           (let* ((args nil)
                  (temp-master-file-name
                   (ignore-errors

                     (flymake-init-create-temp-source-and-master-buffer-copy
                      get-incl-dirs-f
                      'flymake-create-temp-inplace
                      master-file-masks
                      include-regexp)))
                  (cc (if (string= (file-name-extension buffer-file-name) "c")
                          "gcc"
                        "g++")))
             (if temp-master-file-name
                 (let* ((source-file-name buffer-file-name)
                        (source-dir (file-name-directory source-file-name))
                        (buildfile-dir
                         (and (executable-find "make")
                              (flymake-init-find-makfile-dir source-file-name))))
                   (if (or buildfile-dir (executable-find cc))
                       (setq args (flymake-get-syntax-check-program-args
                                   temp-master-file-name
                                   (if buildfile-dir buildfile-dir source-dir)
                                   nil
                                   nil
                                   (if buildfile-dir
                                       'flymake-get-make-cmdline
                                     'flymake-get-gcc-cmdline)))
                     (flymake-report-fatal-status
                      "NOMK"
                      (format "No buildfile (%s) found for %s, or can't found %s"
                              "Makefile" source-file-name cc))))
               (flymake-report-fatal-status
                "TMPERR" (format "Can't create temp file for %s" source-file-name)))
             args))
         (defun flymake-master-make-gcc-header-init ()
           (flymake-master-make-gcc-init
            'flymake-get-include-dirs
            '("\\.cpp\\'" "\\.c\\'")
            "[ \t]*#[ \t]*include[ \t]*\"\\([[:word:]0-9/\\_.]*%s\\)\""))
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.\\(?:h\\(?:pp\\)?\\)\\'"
                        flymake-master-make-gcc-header-init flymake-master-cleanup))
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'"
                        flymake-simple-make-gcc-init)))
       )))

(deh-section "makefile"
  (deh-add-hook makefile-mode-hook
    (my-mode-common-hook)))

(deh-section "change-log"
  (deh-add-hook change-log-mode-hook
    (auto-fill-mode t)
    (add-to-list 'change-log-font-lock-keywords
                 '("^[0-9-]+:? +\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
                   (0 'change-log-date-face)
                   ("\\([^<(]+?\\)[   ]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
                    (1 'change-log-name)
                    (2 'change-log-email)))))
)

(deh-section "elisp"
  (deh-require 'browse-el
    (define-key emacs-lisp-mode-map (kbd "M-.") 'browse-el-find-funtion)
    (define-key emacs-lisp-mode-map (kbd "M-*") 'browse-el-go-back)
    )
  (if (featurep 'ffap)
      (add-to-list 'ffap-alist '(lisp-interaction-mode . ffap-el-mode)))

  (defun my-auto-insert-paren ()
    "Auto close matched parentheses."
    (interactive)
    (condition-case nil
        (progn
          (scan-sexps (point) -1)
          (insert ")")
          (my-auto-insert-paren))
      (error (delete-char -1))))

  (deh-add-hook emacs-lisp-mode-hook
    (my-mode-common-hook)
    (define-key lisp-mode-shared-map (kbd "C-)") 'my-auto-insert-paren)
    (hs-minor-mode 1)
    (turn-on-eldoc-mode)
    (ignore-errors (imenu-add-menubar-index)))
  )

;;; scripts setting
(deh-section "python"
  (deh-add-hook python-mode-hook
    (my-mode-common-hook)
    (when (boundp 'rope-completions) (ac-ropemacs-initialize))
    (make-local-hook 'after-save-hook)
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

  (deh-try-require 'pymacs
    (pymacs-load "ropemacs" "rope-")
    ;;(setq ropemacs-enable-shortcuts nil)
    ;;(setq ropemacs-local-prefix "C-c C-p")
    (setq ropemacs-confirm-saving 'nil)
    (ropemacs-mode t)))

(deh-section "sh-mode"
  (deh-add-hook sh-mode-hook
    ;; (local-unset-key "\C-c\C-o")        ; trigger for `sh-while-getopts'
    (my-mode-common-hook)
    (make-local-hook 'after-save-hook)
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
    ))


;;; tools
(deh-section "gnuplot"
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

  (deh-add-hook gnuplot-after-plot-hook
    (select-window (get-buffer-window gnuplot-comint-recent-buffer)))
  (deh-add-hook gnuplot-comint-setup-hook
    (deh-define-key comint-mode-map
      ("\C-d" . 'comint-delchar-or-maybe-eof))))

(deh-section "graphviz"
  (autoload 'graphviz-dot-mode "graphviz-dot-mode" "graphviz mode" t)

  (setq graphviz-dot-auto-indent-on-semi nil
        graphviz-dot-auto-indent-on-newline nil
        graphviz-dot-toggle-completions t)
  (deh-add-hook graphviz-dot-mode-hook
    (local-unset-key "\C-cc") ; it's prefix key
    (define-key graphviz-dot-mode-map "\t" 'graphviz-dot-tab-action))
  (defun graphviz-dot-tab-action ()
    "If cursor at one word end, try complete it. Otherwise, indent line."
    (interactive)
    (if (looking-at "\\>")
        (graphviz-dot-complete-word)
      (indent-for-tab-command))))

(deh-section-if "protobuf"
  (autoload 'protobuf-mode "protobuf" "Google protobuf mode." t)
  (executable-find "protoc"))

;;; java
(deh-section "java"
  (add-to-list 'load-path "/usr/local/jdee/lisp/")
  (defun jde-init ()
    (interactive)
    (require 'cedet)
    (require 'jde)
    (jde-mode))
  (deh-add-hook java-mode-hook
    (c-set-style "java")
    (setq c-basic-offset 4)))

;;; web related
(deh-section "html"
  (setq sgml-xml-mode t)
  (add-hook 'sgml-mode-hook 'my-mode-common-hook)
  (set 'html-mode-hook
       (lambda ()
         (define-key html-mode-map (kbd "<C-return>") 'ywb-html-insert-newline)
         ;; (tempo-use-tag-list 'tempo-html-tags)
         (let ((str '(""))
               (align '(("align" ("left") ("center") ("right")))))
           (setq sgml-tag-alist `(("style"
                                   ("href" ,str)
                                   ("type" "text/css"))
                                  ("meta"
                                   t
                                   ("http-equiv" ("Content-Type"))
                                   ("content" ("text/html; charset=utf-8" "text/plain") ("Copyright &#169;"))
                                   ("name" ,str))
                                  ("script" (nil "//<![CDATA[" \n _ \n "//]]>")
                                   ("src" ,str)
                                   ("type" "text/javascript"))
                                  ("div" n
                                   ("class" ,str)
                                   ("src", str))
                                  ("object" ("id" ,str))
                                  ("code")
                                  ,@sgml-tag-alist))))))

;; nxhtml: javascript + php + html + css
(deh-section-path "nxhtml"
  "~/src/nxhtml/autostart.el"
  (load-file deh-this-path)
  (setq mumamo-chunk-coloring 5)        ; disable background colors
  )

;;# emacs --batch --eval '(byte-compile-file "js2.el")'
(deh-section "js2"
  (autoload 'js2-mode "js2" "" t)
  (deh-add-hook js2-mode-hook
    (setq forward-sexp-function nil)))

(deh-section-reserved "php"
  (deh-try-require 'php-doc
    (setq php-doc-directory "~/src/php_manual/html"
          php-doc-cachefile (expand-file-name "php-doc" my-temp-dir))
    (deh-local-set-key php-mode-hook
      ("\t"       . 'php-doc-complete-function)
      ("\C-cd" . 'php-doc))
    (set (make-local-variable 'eldoc-documentation-function)
         'php-doc-eldoc-function)
    (eldoc-mode 1)
    ;; hack php-doc.el, in order to select php doc buffer automatically.
    (defun php-doc-w3m (url &rest ignore)
      (let ((buf (get-buffer-create "*php doc*")))
        (pop-to-buffer buf nil t)
        (w3m-goto-url url)))
    )

  (deh-add-hook php-mode-hook
    ;; (tempo-use-tag-list 'tempo-php-tags)
    (font-lock-add-keywords nil gtkdoc-font-lock-keywords)
    (setq php-beginning-of-defun-regexp "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(")
    (setq php-imenu-generic-expression
          '(
            ("Private Methods"
             "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?private\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ("Protected Methods"
             "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?protected\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ("Public Methods"
             "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?public\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ("Classes"
             "^\\s-*class\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*" 1)
            (nil
             "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ))
    (local-set-key (kbd "C-M-a") 'beginning-of-defun)
    (local-set-key (kbd "C-M-e") 'end-of-defun)
    )
;;;; ffap settings
  (defvar ffap-php-path
    (let ((include-path
           (shell-command-to-string "php -r 'echo get_include_path();'")))
      (split-string include-path ":"))
    "php include path")
  (defun my-php-ffap-locate (name)
    "Find php require or include files"
    (if (string-match "^[a-zA-Z0-9_]+$" name)
        (ffap-locate-file (replace-regexp-in-string "_" "/" name) '(".class.php" ".php") ffap-php-path)
      (ffap-locate-file name t ffap-php-path)))
  (if (featurep 'ffap)
      (add-to-list 'ffap-alist '(php-mode . my-php-ffap-locate))))

(deh-section-reserved "latex"
  (load "preview-latex.el" nil t t)
  (load "auctex.el" nil t t)
  (set-default TeX-master nil)
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-electric-escape t
        TeX-save-query nil
        TeX-clean-confirm nil
        TeX-show-compilation nil)
  (deh-add-hook LaTeX-mode-hook
    (turn-off-auto-fill)
    ;; (LaTeX-math-mode 1)
    (outline-minor-mode 1)

    (TeX-PDF-mode t)

    ;;# set evince as pdf viewer
    (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
    ;;# for XeLaTeX
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
    (setq TeX-command-default "XeLaTeX"
          TeX-view-program-selection '((output-pdf "Evince"))
          TeX-global-PDF-mode t
          TeX-engine 'xetex)
    )

  (defun TeX-arg-input-file (optionel &optional prompt local)
    "Prompt for a tex or sty file.

First optional argument is the prompt, the second is a flag.
If the flag is set, only complete with local files."
    (unless (or TeX-global-input-files local)
      (message "Searching for files...")
      (setq TeX-global-input-files
            (mapcar 'list (TeX-search-files (append TeX-macro-private
                                                    TeX-macro-global)
                                            TeX-file-extensions t t))))
    (let ((file (if TeX-check-path
                    (completing-read
                     (TeX-argument-prompt optionel prompt "File")
                     (unless local
                       TeX-global-input-files))
                  (read-file-name
                   (TeX-argument-prompt optionel prompt "File")))))
      (if (null file)
          (setq file ""))
      (if (not (string-equal "" file))
          (TeX-run-style-hooks file))
      (TeX-argument-insert file optionel)))
  )

(deh-section "slime"
  ;;# download [hyperspec|ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz] to localhost, then use "C-c C-d h" to search symbols' hyperspec defines.
  (setq common-lisp-hyperspec-root "/home/jqian/src/HyperSpec/")
  )

(deh-require-if 'evernote-mode
  (executable-find "enclient.rb")
  ;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
  (global-set-key "\C-cec" 'evernote-create-note)
  (global-set-key "\C-ceo" 'evernote-open-note)
  (global-set-key "\C-ces" 'evernote-search-notes)
  (global-set-key "\C-ceS" 'evernote-do-saved-search)
  (global-set-key "\C-cew" 'evernote-write-note)
  (global-set-key "\C-cep" 'evernote-post-region)
  (global-set-key "\C-ceb" 'evernote-browser)
  (setq evernote-mode-hook
        '(lambda () (outline-minor-mode t)))
  )
