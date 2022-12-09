;;; apprentice-project.el --- API to identify Elixir mix projects.

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

;; API to identify Elixir mix projects.

;;; Code:

(require 'cl-lib)
(require 'apprentice-file)

(defgroup apprentice-project nil
  "API to identify Elixir mix projects."
  :prefix "apprentice-help-"
  :group 'apprentice)

(defcustom apprentice-project-elixir-source-dir ""
  "Path to the elixir source code."
  :type 'string
  :group 'alchemist-project)

(defconst apprentice-project-mix-project-indicator "mix.exs"
  "File which indicates the root directory of an Elixir Mix project.")

(defconst apprentice-project-hex-pkg-indicator ".hex"
  "File which indicates the root directory of an Elixir Hex package.")

(defvar apprentice-project-root-path-cache nil
  "Variable which holds the cached project root path.")

(defun apprentice-project-elixir-p ()
  "Return non-nil if `default-directory' is inside the Elixir source codebase."
  (stringp (apprentice-project-elixir-root)))

(defun apprentice-project-elixir-root (&optional dir)
  "Return root directory of the Elixir source DIR."
  (if (and (not (or
		 (null apprentice-project-elixir-source-dir)
		 (string= "" apprentice-project-elixir-source-dir)))
	   (string-prefix-p (expand-file-name apprentice-project-elixir-source-dir)
			    (expand-file-name default-directory)))
      apprentice-project-elixir-source-dir
    nil))

(defun apprentice-project-p ()
  "Return non-nil if `default-directory' is inside an Elixir Mix project."
  (stringp (apprentice-project-root)))

(defun apprentice-project-top-level-dir-p (dir)
  "Return non-nil if DIR is the top level directory."
  (equal dir (file-name-directory (directory-file-name dir))))

(defun apprentice-project-root (&optional dir)
  "Return root directory of the current Elixir Mix project.

It starts walking the directory tree to find the Elixir Mix root directory
from `default-directory'.If DIR is non-nil it starts walking the
directory from there instead."
  (if (and apprentice-project-root-path-cache
	   (string-prefix-p apprentice-project-root-path-cache
			    (expand-file-name default-directory)))
      apprentice-project-root-path-cache
    (let* ((dir (file-name-as-directory (or dir (expand-file-name default-directory))))
	   (present-files (directory-files dir)))
      (cond ((apprentice-project-top-level-dir-p dir)
	     nil)
	    ((cl-member apprentice-project-hex-pkg-indicator present-files
			:test #'string-equal)
	     (apprentice-project-root (file-name-directory (directory-file-name dir))))
	    ((cl-member apprentice-project-mix-project-indicator present-files
			:test #'string-equal)
	     (setq apprentice-project-root-path-cache dir)
	     dir)
	    (t
	     (apprentice-project-root (file-name-directory (directory-file-name dir))))))))

(defun apprentice-project-root-or-default-dir ()
  "Return the current Elixir mix project root or `default-directory'."
  (let* ((project-root (apprentice-project-root))
         (dir (if project-root
                  project-root
                default-directory)))
    dir))

(defun apprentice-project-toggle-file-and-tests-other-window ()
  "Toggle between a file and its test in other window."
  (interactive)
  (if (apprentice-utils-test-file-p)
      (apprentice-project-open-file-for-current-tests 'find-file-other-window)
    (apprentice-project-open-tests-for-current-file 'find-file-other-window)))

(defun apprentice-project-toggle-file-and-tests ()
  "Toggle between a file and its test in the current window."
  (interactive)
  (if (apprentice-utils-test-file-p)
      (apprentice-project-open-file-for-current-tests 'find-file)
    (apprentice-project-open-tests-for-current-file 'find-file)))

(defun apprentice-project-file-under-test (file directory)
  "Return the file which are tested by FILE.
DIRECTORY is the place where the file under test is located."
  (let* ((filename (file-relative-name file (apprentice-project-root)))
         (filename (replace-regexp-in-string "^test" directory filename))
         (filename (replace-regexp-in-string "_test\.exs$" "\.ex" filename)))
    (concat (apprentice-project-root) filename)))

(defun apprentice-project-open-file-for-current-tests (opener)
  "Visit the implementation file for the current buffer with OPENER."
  (let* ((filename (apprentice-project-file-under-test (buffer-file-name) "web"))
         (filename (if (file-exists-p filename)
                       filename
                     (apprentice-project-file-under-test (buffer-file-name) "lib"))))
    (funcall opener filename)))

(defun apprentice-project-open-tests-for-current-file (opener)
  "Visit the test file for the current buffer with OPENER."
  (let* ((filename (file-relative-name (buffer-file-name) (apprentice-project-root)))
         (filename (replace-regexp-in-string "^lib/" "test/" filename))
         (filename (replace-regexp-in-string "^apps/\\(.*\\)/lib/" "apps/\\1/test/" filename))
         (filename (replace-regexp-in-string "^web/" "test/" filename))
         (filename (replace-regexp-in-string "^apps/\\(.*\\)/web/" "apps/\\1/test/" filename))
         (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
         (filename (format "%s/%s" (apprentice-project-root) filename)))
    (if (file-exists-p filename)
        (funcall opener filename)
      (if (y-or-n-p "No test file found; create one now?")
          (apprentice-project--create-test-for-current-file
           filename (current-buffer))
        (message "No test file found.")))))

(defun apprentice-project--create-test-for-current-file (filename buffer)
  "Create and populates a test module, FILENAME, for the code in BUFFER.
The module name given to the test module is determined from the name of the
first module defined in BUFFER."
  (let* ((directory-name (file-name-directory filename))
         (module-name (apprentice-project--grok-module-name buffer))
         (test-module-name (concat module-name "Test")))
    (unless (file-exists-p directory-name)
      (make-directory (file-name-directory filename) t))
    (apprentice-project--insert-test-boilerplate
     (find-file-other-window filename) test-module-name)))

(defun apprentice-project--grok-module-name (buffer)
  "Determine the name of the first module defined in BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "defmodule\\s-\\(.+?\\)\\s-?,?\\s-do")
      (match-string 1))))

(defun apprentice-project--insert-test-boilerplate (buffer module)
  "Insert ExUnit boilerplate for MODULE in BUFFER.
Point is left in a convenient location."
  (with-current-buffer buffer
    (insert (concat "defmodule " module " do\n"
                    "  use ExUnit.Case\n\n\n"
                    "end\n"))
    (goto-char (point-min))
    (beginning-of-line 4)
    (indent-according-to-mode)))

(defun apprentice-project-run-tests-for-current-file ()
  "Run the tests related to the current file."
  (interactive)
  (apprentice-project-open-tests-for-current-file 'apprentice-mix-test-file))

(defun apprentice-project-create-file ()
  "Create a file under lib/ in the current project.

The newly created buffer is filled with a module definition based on the file name."
  (interactive)
  (let ((root (apprentice-project-root)))
    (if (not root)
        (message "You're not in a Mix project")
      (let* ((lib-path (concat root "lib/"))
             (abs-path (read-file-name "New file in lib/: " lib-path))
             (abs-path (apprentice-utils-add-ext-to-path-if-not-present abs-path ".ex"))
             (relative-path (file-relative-name abs-path lib-path)))
        (if (file-readable-p abs-path)
            (message "%s already exists" relative-path)
          (make-directory (file-name-directory abs-path) t)
          (find-file abs-path)
          (insert (concat "defmodule "
                          (apprentice-utils-path-to-module-name relative-path)
                          " do\n"
                          "  \n"
                          "end\n"))
          (goto-char (point-min))
          (beginning-of-line 2)
          (back-to-indentation))))))

(defun apprentice-project-name ()
  "Return the name of the current Elixir Mix project."
  (if (apprentice-project-p)
      (car (last (split-string (apprentice-project-root) "/") 2))
    ""))

(defun apprentice-project-find-dir (directory)
  (unless (apprentice-project-p)
    (error "Could not find an Elixir Mix project root."))
  (apprentice-file-find-files (apprentice-project-root) directory))

(defun apprentice-project-find-lib ()
  (interactive)
  (apprentice-project-find-dir "lib"))

(defun apprentice-project-find-test ()
  (interactive)
  (apprentice-project-find-dir "test"))

(provide 'apprentice-project)

;;; apprentice-project.el ends here
