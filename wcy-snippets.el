;;; wcy-snippets.el --- snipppets                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Chunye Wang

;; Author: Chunye Wang <wcy123@gmail.com>
;; Version: 1.0
;; Keywords: abbrev

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

;;; Code:
(defconst wcy-snippets-this-directory
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name)))))

(require 'skeleton)
(require 'cl-seq)
(require 'wcy-compose (locate-file "wcy-compose"
                                   (cons wcy-snippets-this-directory
                                         load-path)
                                   '(".elc" ".el") nil))
(defvar wcy-snippets-directories
  (list
   (expand-file-name "snippets" wcy-snippets-this-directory)
   (expand-file-name "wcy-snippets" user-emacs-directory)))

(defvar wcy-snippets-history nil)
(make-variable-buffer-local 'wcy-snippets-history)
;;;###autoload
(defun wcy-snippets()
  (interactive)
  (apply #'wcy-snippets-execute-ac (wcy-snippets-guess-ac)))
(defun wcy-snippets-guess-ac ()
  (let* ((ac-defines (wcy-snippets-build-defs))
         (abbrevations (mapcar 'car ac-defines))
         (ret-old (cond
                   ((looking-back (regexp-opt abbrevations) (line-beginning-position))
                    (list (match-string 0) (match-beginning 0) (match-end 0)))
                   (t
                    (let ((bounds (bounds-of-thing-at-point 'symbol)))
                      (when bounds
                        (let* ((input-text (buffer-substring (car bounds) (cdr bounds)))
                               (completions (all-completions input-text abbrevations)))
                          (list (if (= (length completions) 1)
                                    (car completions)
                                  (completing-read "wcy-snippets:"
                                                   abbrevations nil t input-text
                                                   wcy-snippets-history))
                                (car bounds) (cdr bounds)))))))))
    (cons
     (let ((ac (car ret-old)) it)
       (if (setq it (cdr-safe (assoc ac ac-defines)))
           (expand-file-name (concat ac ".ac") it)
         (error "cannot found abbre file  %s " ac)))
     (cdr ret-old))))

(defun wcy-snippets-execute-ac (file-name beg end)
  (when (file-readable-p file-name)
    (delete-region beg end)
    (skeleton-insert
     (with-temp-buffer
       (insert-file-contents file-name)
       (read (current-buffer))))))
(defun wcy-snippets-get-modes ()
  (let ((ret (list major-mode))
        (m major-mode))
    (while (setq m (get m 'derived-mode-parent))
      (when (not (member m ret))
        (push m ret)))
    (when (not (member 'fundamental-mode ret))
      (push 'fundamental-mode ret))
    (reverse ret)))
(defun wcy-snippets-build-defs ()
  (let* ((modes (mapcar 'symbol-name
                            (wcy-snippets-get-modes)))
         (dirs (cl-mapcan
                #'(lambda (snippet-dir)
                    (cl-remove-if-not
                     #'(lambda (name)
                         (let* ((basename (file-name-nondirectory name)))
                           (member basename modes)))
                     (directory-files snippet-dir "-mode")))
                (cl-remove-if-not #'(lambda (dir) (and
                                                   (file-directory-p dir)
                                                   (file-readable-p
                                                    dir)))
                                  wcy-snippets-directories))))
    (mapcan
     #'(lambda (directory)
         (mapcar
          #'(lambda (f)
              (cons (file-name-sans-extension f) directory))
          (directory-files directory nil "\\.ac$" nil)))
     dirs)))

;;;###autoload
(defun wcy-read-variable (prompt predicate &optional default)
  "read a varable"
  (let ((v (variable-at-point)))
    (when (and (symbolp v)
	       (boundp v)
	       (null (funcall predicate v)))
      (setq v 0)))
  (completing-read
   prompt
   obarray
   `(lambda (vv)
      (and (boundp vv)
	   (funcall ,predicate  vv)))
   t  ;; require match
   nil  ;; initial-input
   nil ;; history
   default
   ))
(provide 'wcy-snippets)
;;; wcy-snippets.el ends here
