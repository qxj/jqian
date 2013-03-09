;; -*- coding: utf-8 -*-
;; This config is for portable. The platform relate configuration
;; should appear here.

(defvar my-include-dirs
  (let (dirs
        (incs '("include" "inc" "common" "public" "hdr"))
        (updirs '("./" "../" "../../" "../../../" "../../../../")))
    (dolist (dir updirs)
      (setq dirs (append dirs (mapcar (lambda (x) (concat dir x)) incs))))
    (append updirs dirs)))

(deh-section "env"
  (setenv "GIT_PAGER" "cat")
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient")
  (if (eq system-type 'darwin) (setenv "LC_CTYPE" "zh_CN.UTF-8"))
  )

(deh-section "coding-system"
  (if (eq system-type 'windows-nt)
      (set-language-environment "Chinese-GBK")
    (set-language-environment "UTF-8")
    (set-locale-environment "zh_CN.UTF-8")
    (set-selection-coding-system 'utf-8-unix)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix)
    (set-clipboard-coding-system 'utf-8-unix)
    (set-buffer-file-coding-system 'utf-8-unix)
    (modify-coding-system-alist 'process "*" 'utf-8-unix))

  (add-to-list 'auto-coding-alist '("\\.nfo\\'" . cp437))
  (dolist (char (append "、。．，·ˉˇ¨〃々―～‖…’”）〕〉》」』〗】"
                        "；：？！±×÷∶°′″℃／＼＂＿￣｜ㄥ"  nil))
    (modify-syntax-entry char "." (standard-syntax-table))))

(deh-section "PATH"
  ;; add more directory to environment variable PATH and exec-path
  (let (path)
    (mapc (lambda (p)
            (setq p (convert-standard-filename
                     (expand-file-name p)))
            (add-to-list 'exec-path p)
            (add-to-list 'path p t))
          (append
           ;; let your dirs prepend original PATH
           (if (eq system-type 'windows-nt)
               '("d:/programs/emacs/bin" "d:/cygwin/bin" "d:/cygwin/usr/bin")
             '("~/bin" "/usr/local/bin" "/usr/local/sbin" "/usr/texbin" "/usr/X11/bin"))
           (split-string (getenv "PATH") path-separator)))
    (setenv "PATH" (mapconcat 'identity path path-separator))))

(deh-section-if "win32"
  (eq system-type 'windows-nt)
  (setq abbreviated-home-dir nil)
  (setenv "SHELL" "d:/cygwin/bin/bash.exe")
  (setq explicit-shell-file-name "bash.exe")
  (setq ffap-c-path '("d:/Programs/MSYS/mingw/include/"
                      "d:/Programs/MSYS/mingw/include/c++/3.4.0/"))
  (add-hook 'shell-mode-hook
            (lambda ()
              (comint-send-string (get-buffer-process (current-buffer)) "set PERLIO=:unix\n"))))

(deh-section-if "linux"
  (eq system-type 'gnu/linux)
  (make-variable-buffer-local 'compile-command)

  (dolist (dir '("/usr/lib/info"
                 "/usr/gnu/info"
                 "/usr/gnu/lib/info"
                 "/opt/gnu/info"
                 "/usr/share/lib/info"
                 "/usr/local/share/lib/info"
                 "/usr/gnu/lib/emacs/info"))
    (add-to-list 'Info-default-directory-list dir))
  )

(deh-section-if "macosx"
  (eq system-type 'darwin)
  (setq ns-command-modifier 'meta)
  (dolist (dir '("/usr/include/c++/v1"))
    (add-to-list 'my-include-dirs dir))
  ;; fix launching from spotlight
  ;; $ cat > $HOME/.launchd.conf
  ;; setenv PATH /usr/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin
  (setq browse-url-generic-program "open"))

(deh-section "window-system"
  (cond ((eq window-system 'x)
         (setq x-select-enable-clipboard t)
         ;; fix issue that ibus input method issue cannot be started in emacs
         ;; firstly, $ sudo apt-get install ibus-el
         (deh-try-require 'ibus
           (add-hook 'after-init-hook 'ibus-mode-on))

         (setq my-dired-guess-command-alist
               '(("acroread" "pdf")
                 ("evince" "pdf")
                 ;; ("xpdf" "pdf")
                 ("xdvi" "dvi")
                 ("dvipdf" "dvi")
                 ("zxpdf" "pdf.gz")
                 ("ps2pdf" "ps" "eps")
                 ("gv" "ps" "eps")
                 ("unrar x" "rar")
                 ("kchmviewer" "chm")
                 ("mplayer -stop-xscreensaver" "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv")
                 ("mplayer -playlist" "list")
                 ("display" "gif" "jpeg" "jpg" "tif" "png" )
                 ("eog" "gif" "jpeg" "jpg" "tif" "png")
                 ("docview.pl" "doc")
                 ("ooffice -writer" "ods" "doc")
                 ("ooffice -calc"  "xls")
                 ("ooffice -impress" "odt" "ppt")
                 ("gnumeric" "xls")
                 ("7z x" "7z")
                 ("djview" "djvu")
                 ("perl" "pl")
                 ("firefox" "xml" "html" "htm" "mht"))))
        (t
         (setq my-dired-guess-command-alist nil)))

  ;; If terminal and X is sharing the same emacs server, color-theme
  ;; will affect terminal display. Below function will resolve this
  ;; issue.
  (defun init-window-frame (&optional frame)
    (and frame (select-frame frame))
    (set-variable 'color-theme-is-global nil)
    ;; only enable color theme in window system
    ;; the same color-theme  looks bad in terminal
    (when window-system
      (load (expand-file-name "my-fontset.el" my-config-dir))
      (load (expand-file-name "my-theme.el" my-config-dir))
      ;; no scroll bar
      (set-scroll-bar-mode nil)
      ;; no tool bar
      (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
      ;; transparent frame
      (set-frame-parameter (selected-frame) 'alpha '(95 85))
      (add-to-list 'default-frame-alist '(alpha 95 85))
      ;; for the height, subtract a couple hundred pixels from the
      ;; screen height (for panels, menubars and whatnot), then divide
      ;; by the height of a char to get the height we want
      (add-to-list 'default-frame-alist
                   (cons 'height (/ (- (x-display-pixel-height) 100)
                                    (frame-char-height))))))
  (add-hook 'after-make-frame-functions 'init-window-frame)
  (add-hook 'after-init-hook 'init-window-frame))

;; (deh-add-hook 'find-file-hook
;;   (if (and (buffer-file-name)
;;            (string-match (expand-file-name "~/src/") (buffer-file-name)))
;;       (toggle-read-only 1)))

;; post setting
(when (executable-find "gcc")
  (setq my-include-dirs (append (gcc-include-path) my-include-dirs)))

(let ((autoload-file (expand-file-name "100-loaddefs.el" my-config-dir)))
  (unless (file-exists-p autoload-file) (my-generate-loaddefs t autoload-file)))

;; WORKAROUND: miss define-fringe-bitmap under terminal
(when (null window-system)
  (unless (fboundp 'define-fringe-bitmap)
    (defun define-fringe-bitmap (bitmap bits &optional height width align)))
  (unless (fboundp 'x-hide-tip)
    (defun x-hide-tip ())))

;; WORKAROUND: skip existed server-name
(eval-after-load "server"
  '(let ((file (expand-file-name server-name (if server-use-tcp
                                                 server-auth-dir
                                               server-socket-dir))))
     (if (file-exists-p file)
         (setq server-name (format "%s.%d" server-name (emacs-pid))))))

;; WORKAROUND: avoid eieio error in ede.el
(eval-after-load "ede"
  '(defun ede-customize-forms-menu (menu-def) ()))
