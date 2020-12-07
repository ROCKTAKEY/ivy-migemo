;;; ivy-migemo.el --- Use migemo on ivy              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: matching

;; Version: 0.1.2
;; Package-Requires: ((emacs "24.3") (ivy "0.13.0") (migemo "1.9.2"))

;; URL: https://github.com/ROCKTAKEY/ivy-migemo
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Use migemo on ivy.
;;; How to Use?
;;
;;     ;; Toggle migemo and fuzzy by command.
;;     (define-key ivy-minibuffer-map (kbd "M-f")
;;     (define-key ivy-minibuffer-map (kbd "M-m")
;;
;;     ;; If you want to defaultly use migemo on swiper and counsel-find-file:
;;     (setq ivy-re-builders-alist '((t . ivy--regex-plus)
;;                                   (swiper . ivy-migemo--regex-plus)
;;                                   (counsel-find-file . ivy-migemo--regex-plus))
;;                                   ;(counsel-other-function . ivy-migemo--regex-plus)
;;                                   )
;;
;;
;;; License
;;   This package is licensed by GPLv3.



;;; Code:

(require 'ivy)
(require 'migemo)

(defgroup ivy-migemo ()
  "Group for ivy-migemo."
  :group 'ivy
  :prefix "ivy-migemo-")

(defvar ivy-migemo--regex-hash (make-hash-table :test #'equal)
  "Store pre-computed regex.")

(defun ivy-migemo--regex (str &optional greedy)
  "Same as `ivy--regex' except using migemo.
Make regex sequence from STR (greedily if GREEDY is non-nil)."
  (let ((hashed (unless greedy
                  (gethash str ivy-migemo--regex-hash))))
    (if hashed
        (progn
          (setq ivy--subexps (car hashed))
          (cdr hashed))
      (when (string-match-p "\\(?:[^\\]\\|^\\)\\\\\\'" str)
        (setq str (substring str 0 -1)))
      (setq str (ivy--trim-trailing-re str))
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons
                           (setq ivy--subexps 0)
                           (if (string-match-p "\\`\\.[^.]" (car subs))
                               (concat "\\." (substring (car subs) 1))
                             (migemo-get-pattern (car subs))))
                        (cons
                         (setq ivy--subexps (length subs))
                         (replace-regexp-in-string
                          "\\.\\*\\??\\\\( "
                          "\\( "
                          (mapconcat
                           (lambda (x)
                             (if (string-match-p "\\`\\\\([^?][^\0]*\\\\)\\'" x)
                                 x
                               (format "\\(%s\\)"
                                       (migemo-get-pattern x))))
                           subs
                           (if greedy ".*" ".*?"))
                          nil t))))
                    ivy-migemo--regex-hash)))))

(defun ivy-migemo--regex-plus (str)
  "Same as `ivy--regex-plus' except using migemo.
Make regex sequence from STR."
  (let ((parts (ivy--split-negation str)))
    (cl-case (length parts)
      (0
       "")
      (1
       (if (= (aref str 0) ?!)
           (list (cons "" t)
                 (list (ivy-migemo--regex (car parts))))
         (ivy-migemo--regex (car parts))))
      (2
       (cons
        (cons (ivy--regex (car parts)) t)
        (mapcar #'(lambda (arg)
                    (list (migemo-get-pattern arg)))
                  (split-string (cadr parts) " " t))))
      (t (error "Unexpected: use only one !")))))

(defun ivy-migemo--regex-fuzzy (str)
  "Same as `ivy--regex-fuzzy' except using migemo.
Make regex sequence from STR."
  (setq str (ivy--trim-trailing-re str))
  (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (prog1
          (concat (match-string 1 str)
                  (let ((lst (string-to-list (match-string 2 str))))
                    (apply
                     #'concat
                     `("\\("
                       ,@(cl-mapcar
                          #'concat
                          (cons "" (cdr (mapcar (lambda (c) (format "[^%c\n]*" c))
                                                lst)))
                          (mapcar (lambda (x) (format "\\(%s\\)" (regexp-quote (char-to-string x))))
                                  lst))
                       "\\)\\|"
                       ,(migemo-get-pattern (match-string 2 str)))))
                  (match-string 3 str))
        (setq ivy--subexps (length (match-string 2 str))))
    str))

;;;###autoload
(defun ivy-migemo-toggle-fuzzy ()
  "Toggle the re builder to match fuzzy or not."
  (interactive)
  (setq ivy--old-re nil)
  (cond
   ((eq ivy--regex-function 'ivy--regex-fuzzy) (setq ivy--regex-function 'ivy--regex-plus))
   ((eq ivy--regex-function 'ivy--regex-plus)  (setq ivy--regex-function 'ivy--regex-fuzzy))
   ((eq ivy--regex-function 'ivy-migemo--regex-fuzzy) (setq ivy--regex-function 'ivy-migemo--regex-plus))
   ((eq ivy--regex-function 'ivy-migemo--regex-plus)  (setq ivy--regex-function 'ivy-migemo--regex-fuzzy))))

;;;###autoload
(defun ivy-migemo-toggle-migemo ()
  "Toggle the re builder to use/unuse migemo."
  (interactive)
  (setq ivy--old-re nil)
  (cond
   ((eq ivy--regex-function 'ivy--regex-fuzzy) (setq ivy--regex-function 'ivy-migemo--regex-fuzzy))
   ((eq ivy--regex-function 'ivy--regex-plus)  (setq ivy--regex-function 'ivy-migemo--regex-plus))
   ((eq ivy--regex-function 'ivy-migemo--regex-fuzzy) (setq ivy--regex-function 'ivy--regex-fuzzy))
   ((eq ivy--regex-function 'ivy-migemo--regex-plus)  (setq ivy--regex-function 'ivy--regex-plus))))

(provide 'ivy-migemo)
;;; ivy-migemo.el ends here
