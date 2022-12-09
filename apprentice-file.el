;;; apprentice-file.el --- Functionality to work with directory content

;; Copyright © 2014-2017 Samuel Tonini
;; Copyright © 2022 Fermin MF

;; Author: Samuel Tonini <tonini.samuel@gmail.com
;; Maintainer: Fermin MF <fmfs@posteo.net>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functionality to work with directory content.

;;; Code:
(require 'cl-lib)

(defgroup apprentice-file nil
  "Functionality to work with directory content."
  :prefix "apprentice-file-"
  :group 'apprentice)

(defun apprentice-file-find-files (root directory)
  "Open DIRECTORY inside ROOT and prompt for a file."
  (let* ((files (apprentice-file-read-dir root directory))
         (root-name (car (cdr (reverse (split-string root "/")))))
         (file (completing-read (format "[%s] %s: " root-name directory) files)))
    (find-file (expand-file-name file root))))

(defun apprentice-file-read-dir (root directory)
  "Return all files in DIRECTORY and use ROOT as `default-directory'."
  (let ((default-directory root))
    (mapcar (lambda (file) (file-relative-name file root))
	    (apprentice-file--files-from directory))))

(defun apprentice-file--files-from (directory)
  (when directory
    (cl-loop for d in (directory-files directory t)
	     if (file-directory-p d)
	     nconc (unless (or (equal (file-relative-name d directory) "..")
			       (equal (file-relative-name d directory) "."))
		     (apprentice-file--files-from d))
	     else nconc (list d))))

(provide 'apprentice-file)

;;; apprentice-file.el ends here
