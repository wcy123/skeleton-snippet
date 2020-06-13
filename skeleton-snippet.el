;;; skeleton-snippet.el --- snipppets                    -*- lexical-binding: t; -*-

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
(defconst skeleton-snippet-this-directory
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
                                   (cons skeleton-snippet-this-directory
                                         load-path)
                                   '(".elc" ".el") nil))
(defvar skeleton-snippet-directories
  (list
   (expand-file-name "snippets" skeleton-snippet-this-directory)
   (expand-file-name ".skeleton-snippet" (getenv "HOME"))
   (expand-file-name "skeleton-snippet" user-emacs-directory)))

(defvar skeleton-snippet-history nil)
(make-variable-buffer-local 'skeleton-snippet-history)
;;;###autoload
(defun skeleton-snippet()
  (interactive)
  (apply #'skeleton-snippet-execute-ac (skeleton-snippet-guess-ac)))
(defun skeleton-snippet-guess-ac ()
  (let* ((ac-defines (skeleton-snippet-build-defs))
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
                                  (completing-read "skeleton-snippet:"
                                                   abbrevations nil t input-text
                                                   skeleton-snippet-history))
                                (car bounds) (cdr bounds)))))))))
    (cons
     (let ((ac (car ret-old)) it)
       (if (setq it (cdr-safe (assoc ac ac-defines)))
           (expand-file-name (concat ac ".ac") it)
         (error "cannot found abbre file  %s " ac)))
     (cdr ret-old))))

(defun skeleton-snippet-execute-ac (file-name beg end)
  (when (file-readable-p file-name)
    (delete-region beg end)
    (skeleton-insert
     (with-temp-buffer
       (insert-file-contents file-name)
       (read (current-buffer))))))
(defun skeleton-snippet-get-modes ()
  (let ((ret (list major-mode))
        (m major-mode))
    (while (setq m (get m 'derived-mode-parent))
      (when (not (member m ret))
        (push m ret)))
    (when (not (member 'fundamental-mode ret))
      (push 'fundamental-mode ret))
    (reverse ret)))
(defun skeleton-snippet-build-defs ()
  (let* ((modes (mapcar 'symbol-name
                            (skeleton-snippet-get-modes)))
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
                                  skeleton-snippet-directories))))
    (mapcan
     #'(lambda (directory)
         (mapcar
          #'(lambda (f)
              (cons (file-name-sans-extension f) directory))
          (directory-files directory nil "\\.ac$" nil)))
     dirs)))
;;;###autoload
(defun skeleton-snippet-add-snippet-directory (dir)
  (setq skeleton-snippet-directories
        (delete-dups (cons dir skeleton-snippet-directories))))
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
(provide 'skeleton-snippet)
;;; skeleton-snippet.el ends here
