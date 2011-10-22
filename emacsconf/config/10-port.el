;; -*- coding: utf-8 -*-
;; This config is for portable. The platform relate configuration
;; should appear here.

(defvar  user-include-dirs
  '(".." "../include" "../inc" "../common" "../public" "../hdr"
    "../.." "../../include" "../../inc" "../../common" "../../public"
    "../../hdr"))

(deh-section "env"
  (setenv "GIT_PAGER" "cat")
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient")
  )

(deh-section "coding-system"
  (unless (coding-system-p 'gbk)
    (define-coding-system-alias 'gbk 'chinese-iso-8bit))
  (unless (coding-system-p 'chinese-gbk)
    (define-coding-system-alias 'chinese-gbk 'chinese-iso-8bit))
  (prefer-coding-system 'utf-8)

  (add-to-list 'auto-coding-alist '("\\.nfo\\'" . cp437))
  (dolist (char (append
                 "、。．，·ˉˇ¨〃々―～‖…’”）〕〉》」』〗】；：？！±×÷∶°′″℃／＼＂＿￣｜ㄥ"  nil))
    (modify-syntax-entry char "." (standard-syntax-table)))

  (define-abbrev-table 'global-abbrev-table
    '(("alpha" "α" nil 0)
      ("beta" "β" nil 0)
      ("gamma" "γ" nil 0)
      ("theta" "θ" nil 0)
      ("inf" "∞" nil 0)
      ("ar1" "→" nil 0)
      ("ar2" "⇒" nil 0))))

(deh-section "PATH"
  ;; add more directory to environment variable PATH
  (let ((path (split-string (getenv "PATH") path-separator)))
    (mapc (lambda (p)
              (setq p (convert-standard-filename
                       (expand-file-name p)))
              (add-to-list 'exec-path p)
              (add-to-list 'path p))
            (append
             '("~/bin")
             (when (eq system-type 'windows-nt)
               '("d:/programs/emacs/bin" "d:/cygwin/bin" "d:/cygwin/usr/bin"))))
    (setenv "PATH" (mapconcat 'identity path path-separator))))

(deh-section-if "win32"
  (eq system-type 'windows-nt)
  (setq file-name-coding-system 'gbk)
  (set-terminal-coding-system 'gbk)
  (set-keyboard-coding-system 'gbk)
  (setq locale-coding-system 'gbk)
  (set-selection-coding-system 'gbk)
  (set-clipboard-coding-system 'ctext)
  (set-clipboard-coding-system 'gbk)
  (set-terminal-coding-system 'gbk)
  (set-buffer-file-coding-system 'gbk)
  (modify-coding-system-alist 'process "*" 'gbk)
  (setq default-process-coding-system '(gbk . gbk))
  ;; (set-language-environment 'Chinese-GB)
  (setq w32-charset-info-alist
        (cons '("gbk" w32-charset-gb2312 . 936)
              w32-charset-info-alist))
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
      (add-to-list 'Info-default-directory-list dir)))

(deh-section "window-system"
  (when (eq window-system 'x) (setq x-select-enable-clipboard t))
  (when (eq window-system 'ns) (setq ns-command-modifier 'meta))

  ;; If terminal and X is sharing the same emacs server, color-theme
  ;; will affect terminal display. Below function will resolve this
  ;; issue.
  (defun init-window-frame (&optional frame)
    (and frame (select-frame frame))
    (set-variable 'color-theme-is-global nil)
    ;; only enable color theme in window system
    ;; the same color-theme  looks bad in terminal
    (when window-system
      ;; (load (expand-file-name "my-fontset.el" my-config-dir))
      (load (expand-file-name "my-theme.el" my-config-dir))
      ;; no scroll bar
      (set-scroll-bar-mode nil)
      ;; no tool bar
      (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
      ;; transparent frame
      (set-frame-parameter (selected-frame) 'alpha '(95 85))
      (add-to-list 'default-frame-alist '(alpha 95 85))
      ;; use 120 char wide window for largeish displays and smaller 80
      ;; column windows for smaller displays pick whatever numbers make
      ;; sense for you
      (let ((en-font "DejaVu Sans Mono")
            (zh-font "WenQuanYi Micro Hei Mono"))
        (if (> (x-display-pixel-width) 1280)
            (progn
              (add-to-list 'default-frame-alist (cons 'width 100))
              (set-frame-font (concat en-font ":pixelsize=14"))
              (dolist (charset '(kana han symbol cjk-misc bopomofo))
                (set-fontset-font "fontset-default" charset
                                  (font-spec :family zh-font :size 16))))
          (add-to-list 'default-frame-alist (cons 'width 80))
          (set-frame-font (concat en-font ":pixelsize=12"))
          (set-fontset-font "fontset-default" 'han
                            (font-spec :family zh-font :size 14))))
      ;; for the height, subtract a couple hundred pixels from the
      ;; screen height (for panels, menubars and whatnot), then divide
      ;; by the height of a char to get the height we want
      (add-to-list 'default-frame-alist
                   (cons 'height (/ (- (x-display-pixel-height) 100)
                                    (frame-char-height))))))
    (add-hook 'after-make-frame-functions 'init-window-frame)
    (add-hook 'after-init-hook 'init-window-frame))

;; (deh-add-hook find-file-hook
;;   (if (string-match (expand-file-name "~/src/") (buffer-file-name))
;;       (view-mode)))

;; WORKAROUND: miss define-fringe-bitmap under terminal
(when (null window-system)
  (unless (boundp 'define-fringe-bitmap)
    (defun define-fringe-bitmap (bitmap bits &optional height width align))))

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
