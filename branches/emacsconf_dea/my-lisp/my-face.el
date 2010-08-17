;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; my-face.el ---
;; Time-stamp: <2010-06-02 10:09:38 Wednesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: my-face.el,v 0.0 2010/01/28 10:41:37 julian Exp $

;; mainly copy from ahei-face.el, thanks.

;;; Code:
;; (eval-when-compile (require 'cl))

;; http://homepage1.nifty.com/blankspace/emacs/emacs_rgb.html

;;; mono color
(defface white-face
  '((((class color)) :foreground "white"))
  "my white face")
(defface gray-face
  '((((class color) (background dark)) (:foreground "gray"))
    (t (:foreground "gray")))
  "my gray face")
(defface red-face
  '((((class color) (background dark)) (:foreground "red" :background "white"))
    (t (:foreground "red")))
  "my red face")
(defface green-face
  '((((type tty)) :bold t :foreground "green")
     (((class color)) :foreground "green")
     (t (:foreground "green")))
  "my green face")
(defface blue-face
  '((((type tty pc)) :foreground "blue")
    (t (:foreground "blue")))
  "my blue face")
(defface yellow-face
  '((t :foreground "gold3"))
  "my yellow face")
(defface cyan-face
  '((t :foreground "cyan"))
  "my cyan face")
(defface magenta-face
  '((((class color)) :foreground "magenta")
    (t (:foreground "magenta")))
  "my magenta face")

;;; font face
(defface underline-face
  '((((class color) (background dark)) (:underline t))
    (t ()))
  "my underline face")

;;; combined color
(defface dark-yellow-face
  '((((type tty pc)) :bold t :foreground "yellow")
    (t (:foreground "yellow" :background "black")))
  "my dark yellow face")
(defface dark-cyan-face
  '((((type tty pc)) :bold t :foreground "cyan")
    (t (:foreground "cyan")))
  "my dark cyan face")
(defface dark-magenta-face
  '((((type tty pc)) :bold t :foreground "magenta")
    (t (:foreground "magenta")))
  "my magenta face")
(defface dark-red-face
  '((((type tty)) :bold t :foreground "red")
    (((class color) (background dark)) (:foreground "black" :background "red"))
    (t (:foreground "chocolate1")))
  "my black-red face")
(defface underline-green-face
  '((((class color) (background dark)) (:underline t :foreground "green"))
    (t ()))
  "my underline green face")
(defface green-red-face
  '((((class color) (background dark)) (:foreground "green" :background "red"))
    (t (:foreground "maroon1")))
  "my green-red face")
(defface yellow-red-face
  '((((class color) (background dark)) (:foreground "yellow" :background "red"))
    (t ()))
  "my yellow-red face")
(defface yellow-blue-face
  '((((class color) (background dark)) (:foreground "yellow" :background "blue"))
    (t :background "wheat" :foreground "darkorchid2"))
  "my yellow-blue face")
(defface white-blue-face
  '((((type tty)) :bold t :background "white" :foreground "blue")
    (t :background "darkslateblue" :foreground "chartreuse"))
  "my while blue face")
(defface yellow-forestgreen-face
  '((((class color) (background dark)) (:foreground "yellow" :background "forest green"))
    (t (:foreground "yellow" :background "forest green")))
  "my yellow-forestgreen face")
(defface red-yellow-face
  '((((class color) (background dark)) (:foreground "red" :background "yellow"))
    (t ()))
  "my red-yellow face")
(defface red-blue-face
  '((((class color) (background dark)) (:foreground "red" :background "blue"))
    (t ()))
  "my red-blue face")
(defface white-red-face
  '((((class color) (background dark)) (:foreground "white" :background "red"))
    (t (:foreground "white" :background "red")))
  "my white-red face")
(defface dark-red-face
  '((((type tty pc)) :bold t :foreground "red")
    (t (:foreground "red")))
  "my dark red face")
(defface dark-green-face
  '((((type tty pc)) :bold t :foreground "green")
    (t (:foreground "green")))
  "my dark green face")
(defface dark-blue-face
  '((((type tty pc)) :bold t :foreground "blue") (t (:foreground "blue")))
  "my dark blue face for terminal")
(defface light-blue-face
  '((((type tty pc)) :foreground "blue")
    (t :foreground "cornflower blue"))
  "my lightblue face for terminal")

;;; heavily customized face
(defface beautiful-blue-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 88) (background dark)) (:foreground "cornflower blue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:weight bold)))
  "beautiful blue face")
(defface beautiful-blue-red-face
  '((((class grayscale) (background light)) (:foreground "red" :background "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "red" :background "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "red" :background "Orchid"))
    (((class color) (min-colors 88) (background dark)) (:foreground "red" :background "cornflower blue"))
    (((class color) (min-colors 16) (background light)) (:foreground "red" :background "Orchid"))
    (((class color) (min-colors 16) (background dark)) (:foreground "red" :background "LightSteelBlue"))
    (((class color) (min-colors 8)) (:foreground "red" :background "blue" :weight bold))
    (t (:weight bold)))
  "background is beautiful blue, foreground is read.")

;; (defface org-level-2
;;   '(
;;     (((class color) (background dark)) (:inherit outline-2 :foreground "green" :weight bold))
;;     (t (:inherit outline-2 :foreground "green" :weight bold)))
;;   "my customized org 2nd level face")

(defface region-face
  '((((class color) (min-colors 88) (background dark))
     :background "#4CAA4CAA4CAA")
    (((class color) (min-colors 88) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 16) (background dark))
     :background "wheat")
    (((class color) (min-colors 16) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 8))
     :background "blue" :foreground "red")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "my region face")

(defface mode-line-lines-face
  '(;; terminal
    (((type tty pc)) :background "red" :foreground "white")
    (t (:background "dark slate blue" :foreground "yellow")))
  "Face used to highlight lines on mode-line.")

(defface mode-line-which-func-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :background "yellow" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :background "yellow" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Orchid" :background "yellow"))
    (((class color) (min-colors 88) (background dark)) (:foreground "yellow" :background "HotPink3"))
    (((class color) (min-colors 16) (background light)) (:foreground "Orchid" :background "yellow"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue" :background "yellow"))
    (((class color) (min-colors 8)) (:foreground "blue" :background "yellow" :weight bold))
    (t (:weight bold)))
  "my which func face")

(defface region-invert nil "Invert of face region.")

(apply-args-list-to-fun
 'defvar
 `(
   (white-face 'white-face                               )
   (gray-face 'gray-face                                 )
   (red-face 'red-face                                   )
   (green-face 'green-face                               )
   (blue-face 'blue-face                                 )
   (yellow-face 'yellow-face                             )
   (cyan-face 'cyan-face                                 )
   (magenta-face 'magenta-face                           )
   (underline-face 'underline-face                       )
   (dark-yellow-face 'dark-yellow-face                   )
   (dark-cyan-face 'dark-cyan-face                       )
   (dark-magenta-face 'dark-magenta-face                 )
   (dark-red-face 'dark-red-face                         )
   (underline-green-face 'underline-green-face           )
   (green-red-face 'green-red-face                       )
   (yellow-red-face 'yellow-red-face                     )
   (yellow-blue-face 'yellow-blue-face                   )
   (white-blue-face 'white-blue-face                     )
   (yellow-forestgreen-face 'yellow-forestgreen-face     )
   (red-yellow-face 'red-yellow-face                     )
   (red-blue-face 'red-blue-face                         )
   (white-red-face 'white-red-face                       )
   (dark-red-face 'dark-red-face                         )
   (dark-green-face 'dark-green-face                     )
   (dark-blue-face 'dark-blue-face                       )
   (light-blue-face 'light-blue-face                     )
   (beautiful-blue-face 'beautiful-blue-face             )
   (beautiful-blue-red-face 'beautiful-blue-red-face     )
   (org-level-2 'org-level-2                             )
   (region-face 'region-face                             )
   (mode-line-lines-face 'mode-line-lines-face           )
   (mode-line-which-func-face 'mode-line-which-func-face )))

;;; my-face.el ends here
