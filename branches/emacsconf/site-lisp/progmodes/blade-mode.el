;;; blade-mode.el --- blade mode for BUILD

;; Copyright (C) 2014  Tencent

;; Author: Julian Qian <jqian@tencent.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar blade-mode-hook nil)

(defvar blade-mode-map
  (let ((blade-mode-map (make-sparse-keymap)))
    (define-key blade-mode-map "\C-j" 'newline-and-indent)
    blade-mode-map)
  "Keymap for blade major mode")

(add-to-list 'auto-mode-alist '("^BUILD$" . blade-mode))

(setq blade-targets-keywords
      '("cc_library" "cc_binary" "cc_test" "proto_library" "lex_yacc_library"
        "swig_library" "cc_plugin" "resource_library" "java_jar" "cc_config"
        "cc_test_config" "proto_library_config")
      blade-attributes-keywords
      '("name" "srcs" "deps" "incs" "defs" "prefix" "warning" "optimize"
        "extra_cppflags" "link_all_symbols" "prebuilt")
      blade-buildin-keywords
      '("if")
      blade-variable-keywords
      '("build_target")
      blade-constants-keywords
      '("True" "False"))

;; refer sh-script.el
(defconst blade-font-lock-keywords
  `(
    ;; ("#.*" . font-lock-comment-face)
    ;; ("'[^']*'" . font-lock-string-face)
    (,(regexp-opt blade-targets-keywords 'words) . font-lock-function-name-face)
    (,(regexp-opt blade-attributes-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt blade-buildin-keywords 'words) . font-lock-builtin-face)
    (,(regexp-opt blade-variable-keywords 'words) . font-lock-variable-name-face)
    (,(regexp-opt blade-constants-keywords 'words) . font-lock-constant-face)))

(defun blade-indent-line ()
  "Indent current line as Blade BUILD code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) curr-indent)
      (cond
       ((looking-at (regexp-opt blade-buildin-keywords 'words)) ; if xxx == yyy:
        (indent-line-to 0))
       ((looking-at (regexp-opt blade-buildin-keywords 'words)) ; if xxx == yyy:
        (indent-line-to 0))
       ;; ((looking-at ".*($")           ; when function start
       ;;  (save-excursion
       ;;    (forward-line -1)
       ;;    (setq curr-indent (+ (current-indentation) default-tab-width)))
       ;;  (setq not-indented nil))
       ((looking-at "^[ \t]*)")       ; when function end
        (save-excursion
          (forward-line -1)
          (unless (looking-at ".*($")
            (setq curr-indent (- (current-indentation) default-tab-width))
            (if (< curr-indent 0) (setq curr-indent 0))))
        (setq not-indented nil))
       ;; ((looking-at ".*\\[$")         ; when attribute start
       ;;  (save-excursion
       ;;    (forward-line -1)
       ;;    (setq curr-indent (+ (current-indentation) default-tab-width)))
       ;;  (setq not-indented nil))
       ((looking-at "^[ \t]*\\]")     ; when attribute end
        (save-excursion
          (forward-line -1)
          (unless (looking-at ".*\\[$")
            (setq curr-indent (- (current-indentation) default-tab-width))
            (if (< curr-indent 0) (setq curr-indent 0))))
        (setq not-indented nil))
       (t
        (save-excursion                   ; during function
          (while not-indented
            (forward-line -1)
            (cond
             ((looking-at ".*[(\\[]$") ; first see start paren
              (setq curr-indent (+ (current-indentation) default-tab-width)
                    not-indented nil))
             ((or (looking-at ".*)") (looking-at ".*\\]"))
              (setq curr-indent (current-indentation)
                    not-indented nil))
             ((looking-at (regexp-opt blade-buildin-keywords 'words))
              (progn (setq curr-indent (+ (current-indentation) default-tab-width)
                           not-indented nil)))
             ((bobp)
              (setq not-indented nil)))))))

      (if curr-indent
          (indent-line-to curr-indent)
        (indent-line-to 0)))))

(defvar blade-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_  "w     " st)
    (modify-syntax-entry ?\' "\"    " st)
    ;; bash style comment
    (modify-syntax-entry ?#  "<     " st)
    (modify-syntax-entry ?\n ">     " st)
    st)
  "Syntax table for blade-mode")

(defun blade-font-lock-syntactic-face-function (state)
  (let ((in-string (nth 3 state))       ; parse-partial-sexp
        (in-comment (nth 4 state))
        (start (nth 8 state)))
    (cond
     (in-string 'font-lock-string-face)
     (in-comment 'font-lock-comment-face)
     (t 'font-lock-comment-face))))

(define-derived-mode blade-mode prog-mode "Blade"
  "blade mode for BUILD"
  (kill-all-local-variables)
  (use-local-map blade-mode-map)
  (set-syntax-table blade-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(blade-font-lock-keywords
         ;; nil nil nil nil
         ;; (font-lock-syntactic-face-function
         ;;  . blade-font-lock-syntactic-face-function)
         ))
  (set (make-local-variable 'indent-line-function) 'blade-indent-line)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+[ \t]*")
  (setq mode-name "Blade"))

(provide 'blade-mode)

;;; blade-mode.el ends here
