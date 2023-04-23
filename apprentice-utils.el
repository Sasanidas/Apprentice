;;; apprentice-utils.el --- Common utility functions that don't belong anywhere else -*- lexical-binding: t -*-

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

;; Common utility functions that don't belong anywhere else

;;; Code:

(require 'cl-lib)
(require 'apprentice-execute)

(defun apprentice-utils-build-command (command-list)
  "Build COMMAND-LIST for the runner."
  (let* ((command-list (flatten-list
			(if (stringp command-list)
			    (split-string command-list)
			  command-list)))
         (command (cl-remove "" command-list :test #'string-equal)))
    (mapconcat #'concat command " ")))

(defun apprentice-utils-count-char-occurence (regexp str)
  "Count occurrence of char with REGEXP inside STR."
  (cl-loop with start = 0
           for count from 0
           while (string-match regexp str start)
           do (setq start (match-end 0))
           finally return count))

(defun apprentice-utils-test-file-p ()
  "Return non-nil `current-buffer' holds an Elixir test file."
  (string-match "_test\\.exs$" (or (buffer-file-name) "")))

(defun apprentice-utils-remove-dot-at-the-end (string)
  "Remove dot character at the end of STRING."
  (replace-regexp-in-string "\\.$" "" string))

(defun apprentice-utils-prepare-aliases-for-elixir (aliases)
  (let* ((aliases (mapcar (lambda (a)
			    (let ((module (apprentice-utils-remove-dot-at-the-end (car a)))
				  (alias (apprentice-utils-remove-dot-at-the-end (car (cdr a)))))
			      (if (not (or
					(or (null alias)
					    (string= "" alias))
					(string= alias module)))
				  (format "{%s, %s}"
					  (if (or (null alias)
						  (string= "" alias))
					      module
					    alias)
					  module))))
			  aliases))
         (aliases (mapconcat #'identity aliases ",")))
    (format "[%s]" aliases)))

(defun apprentice-utils-prepare-modules-for-elixir (modules)
  (let* ((modules (mapconcat #'identity modules ",")))
    (format "[%s]" modules)))

(defun apprentice-utils--snakecase-to-camelcase (str)
  "Convert a snake_case string STR to a CamelCase string.

This function is useful for converting file names like my_module to Elixir
module names (MyModule)."
  (mapconcat #'capitalize (split-string str "_") ""))

(defun apprentice-utils-add-ext-to-path-if-not-present (path ext)
  "Add EXT to PATH if PATH doesn't already ends with EXT."
  (if (string-suffix-p ext path)
      path
    (concat path ext)))

(defun apprentice-utils-path-to-module-name (path)
  "Convert PATH to its Elixir module name equivalent.

For example, convert my_app/my_module.ex to MyApp.MyModule ."
  (let* ((path (file-name-sans-extension path))
         (path (split-string path "/"))
         (path (cl-remove "" path :test #'string-equal)))
    (mapconcat #'apprentice-utils--snakecase-to-camelcase path ".")))

(defun apprentice-utils-add-trailing-slash (path)
  "Add trailing slash to PATH if not already contain."
  (if (not (string-match-p "/$" path))
      (format "%s/" path)
    path))

(defun apprentice-utils-occur-in-buffer-p (buffer regex)
  "Return non-nil if BUFFER contain at least one occurrence of REGEX."
  (with-current-buffer buffer
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (re-search-forward regex nil t)))))

(defun apprentice-utils-jump-to-regex (regex before-fn after-fn search-fn reset-fn)
  "Jump to REGEX using SEARCH-FN to search for it.
A common use case would be to use `re-search-forward' as the SEARCH-FN.
Call RESET-FN if the regex isn't found at the first try. BEFORE-FN is called
before performing the search while AFTER-FN after."
  (when (apprentice-utils-occur-in-buffer-p (current-buffer) regex)
    (save-match-data
      (funcall before-fn)
      (unless (funcall search-fn regex nil t)
        (funcall reset-fn)
        (funcall search-fn regex nil t))
      (funcall after-fn))))

(defun apprentice-utils-jump-to-next-matching-line (regex after-fn)
  "Jump to the next line matching REGEX.
Call AFTER-FN after performing the search."
  (apprentice-utils-jump-to-regex regex 'end-of-line after-fn 're-search-forward 'beginning-of-buffer))

(defun apprentice-utils-jump-to-previous-matching-line (regex after-fn)
  "Jump to the previous line matching REGEX.

Call AFTER-FN after performing the search."
  (apprentice-utils-jump-to-regex regex 'beginning-of-line after-fn 're-search-backward 'end-of-buffer))

(defun apprentice-utils-elixir-version ()
  "Return the current Elixir version on the system."
  (with-temp-buffer
    (insert (shell-command-to-string
	     (format "%s --version" apprentice-execute-command)))
    (goto-char (point-min))
    (buffer-substring-no-properties
     (1+ (re-search-forward "Elixir"))
     (- (re-search-forward "(") 2))))

(defun apprentice-utils-elixir-version-check-p (supply-version &optional version)
  "Return t if VERSION is greater than of equal to SUPPLY-VERSION."
  (version<= supply-version (or version (apprentice-utils-elixir-version))))

(provide 'apprentice-utils)

;;; apprentice-utils.el ends here
