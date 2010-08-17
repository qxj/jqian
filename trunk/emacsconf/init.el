(if (not load-file-name)
    (error "Load me by M-x load-file RET"))

;;; To distinguish running environment
(defconst mswin  (eq window-system 'w32)  "Non-nil means windows system.")
(defconst xwin   (eq window-system 'x)    "Non-nil means X window")
(defconst macos  (eq window-system 'mac)  "Non-nil means mac os x system")
(defconst nowin  (eq window-system nil)   "Non-nil means no window manager")
(defconst cygwin (eq system-type 'cygwin) "Non-nil means cygwin system.")
;;; To distinguish different emacs version
(defconst is-before-emacs-21 (>= 21 emacs-major-version) "emacs version before 21")
(defconst is-after-emacs-23  (<= 23 emacs-major-version) "emacs version after 23")
;;; predefine directories
(defconst my-startup-dir (file-name-directory load-file-name)
  "Emacs configuration path, default is `~/.emacs.d/'")
(defconst my-config-dir (expand-file-name "config" my-startup-dir)
  "Emacs customization settings, default is `~/.emacs.d/config'")
(defconst my-site-lisp-dir (expand-file-name "site-lisp" my-startup-dir)
  "Useful lisp projects and products, default is `~/.emacs.d/site-lisp'")
(defconst my-template-dir (expand-file-name "templates" my-startup-dir)
  "Default templates for creating different files")
(defconst my-snippet-dir (expand-file-name "snippets" my-startup-dir)
  "snippets from YASnippets")
(defconst my-org-dir
  (if mswin "d:/My Dropbox/Notes/" "~/Dropbox/Notes/")
  "Put my temporary notes here, eg: in dropbox directory")
(defconst my-temp-dir
  (if mswin "c:/windows/temp/emacs/" "~/.tmp-emacs/")
  "Temporary directory to store autosave, desktop, session, backup files.")

;;; Load-path
(add-to-list 'load-path my-config-dir)
(add-to-list 'load-path my-site-lisp-dir)
(let ((default-directory my-site-lisp-dir))
  (load "subdirs.el"))

;;; if no dot-emacs-helper.el, use this to inhibit load errors
(unless (require 'dot-emacs-helper nil t)
  (defmacro deh-require-maybe (feature &rest forms)
    (declare (indent 1))
    `(progn (when (require ,feature nil t) ,@forms)))
  (defalias 'deh-require 'deh-require-maybe)
  (put 'deh-require 'lisp-indent-function 1)
  (defmacro deh-section (section &rest forms)
    (declare (indent 1))
    `(progn ,@forms))
  (defmacro deh-define-key (map &rest keypairs)
    "Define a batch of keys.

Example:
  (deh-define-key global-map
    (\"\\C-m\"        . 'newline-and-indent)
    (\"\\C-j\"        . 'newline))
"
    (declare (indent 1))
    (cons 'progn
          (mapcar (lambda (pair)
                    `(define-key ,map ,(car pair) ,(cdr pair)))
                  keypairs))))

;;; ready to load my configurations
(mapc 'load (directory-files my-config-dir t "^[0-9]+-.*.el"))
(server-start)
