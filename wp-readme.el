;;; wp-readme.el --- A major mode for editing WordPress style markdown.
;;
;; Copyright (C) 2014 Cody Reichert
;;
;; Author: Cody Reichert
;; URL: http://github.com/CodyReichert/wp-readme.el
;; Keywords: WordPress, README, Markdown
;; Version: DEV
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:

;;; Code:



;;; REQUIREMENTS
(require 'font-lock)

(defgroup wp-readme nil
  "Major mode group for Wordpress style markdown."
  :prefix "wp-reamde-"
  :group 'wp)


;;;; mode map
(defvar wp-readme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-al" 'wp-readme-insert-link)
    map)
  "Keymap for `wp-readme-mode'.")

(defun wp-readme-insert-link ()
  "Insert hyperlink syntax at point."
 (insert "[]()"))


;;; font faces
(defgroup wp-readme-faces nil
  "Font faces for wp-readme-mode."
  :group 'wp-readme
  :group 'faces)


(defvar wp-readme-header-one-face 'wp-readme-header-one-face
  "First level header font lock name.")

(defvar wp-readme-header-two-face 'wp-readme-header-two-face
  "Second level header font lock name.")

(defvar wp-readme-header-three-face 'wp-readme-header-three-face
  "Third level header font lock name.")

(defvar wp-readme-italic-face 'wp-readme-italic-face
  "Italic face name.")


(defface wp-readme-header-one-face
  '((t (:inherit font-lock-function-name-face :weight bold :height 125)))
  "Font face for level 1 headers."
  :group 'wp-readme-faces)

(defface wp-readme-header-two-face
  '((t (:inherit font-lock-function-name-face :weight bold :height 115)))
  "Font face for level 2 headers."
  :group 'wp-readme-faces)

(defface wp-readme-header-three-face
  '((t (:inherit font-lock-function-name-face :weight bold :height 105)))
  "Font face for level 3 headers."
  :group 'wp-readme-faces)

(defface wp-readme-italic-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Font face for italic text."
  :group 'wp-readme-faces)


(defvar wp-readme-font-lock-keywords
  (list
   '("^===\\(.*?\\)===" . wp-readme-header-one-face)
   '("^==\\(.*?\\)==" . wp-readme-header-two-face)
   '("^=\\(.*?\\)=" . wp-readme-header-three-face)
   '("\\*\\(.*?\\)\\*" . wp-readme-italic-face)
   )
  "Font lock keywords for wp-readme-mode.")


(define-derived-mode wp-readme-mode text-mode "wp-readme"
  "A major mode for editing Wordpress style markdown
\\{wp-readme-mode-map}"
  (setq tab-width 4)
  (setq font-lock-defaults '(wp-readme-font-lock-keywords)))


(provide 'wp-readme)
;;; wp-readme.el ends here

