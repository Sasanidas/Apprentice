;;; apprentice-compile.el --- Elixir compilation functionality

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

;; Elixir compilation functionality.

;;; Code:

(require 'compile)
(require 'apprentice-utils)
(require 'apprentice-report)

(defgroup apprentice-compile nil
  "Elixir compilation functionality."
  :prefix "apprentice-compile-"
  :group 'apprentice)

;; Variables

(defcustom apprentice-compile-command "elixirc"
  "The shell command for elixirc."
  :type 'string
  :group 'apprentice-compile)

(defvar apprentice-compile-buffer-name "*apprentice elixirc*"
  "Name of the elixir output buffer.")

(defvar apprentice-compile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map))

;; Private functions

(defun apprentice-compile--file (filename)
  (cond ((not (file-exists-p filename)) (error "The given file doesn't exist"))
        ((string-match "\.exs$" filename) (error "The given file is an Elixir Script"))
        (t (apprentice-compile (list apprentice-compile-command (expand-file-name filename))))))

(defun apprentice-compile--read-command (command)
  (read-shell-command
   "elixirc command: " (format "%s " command)))

;; Public functions

(defun apprentice-compile-this-buffer ()
  "Compile the current buffer with elixirc."
  (interactive)
  (apprentice-compile--file buffer-file-name))

(defun apprentice-compile-file (filename)
  "Compile the given FILENAME."
  (interactive "Felixirc: ")
  (apprentice-compile--file (expand-file-name filename)))

;;TODO: Make the error regex
(define-compilation-mode apprentice-compile-mode "Elixir Compile Mode"
  "Major mode for compiling Elixir files.
\\{apprentice-compile-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t
	      electric-indent-chars nil)
  (add-hook 'compilation-filter-hook #'apprentice-compile--output-filter))

(defun apprentice-compile--output-filter ()
  "Remove control characters from output."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun apprentice-compile (cmdlist)
  "Compile CMDLIST with elixirc."
  (interactive (list (apprentice-compile--read-command apprentice-compile-command)))
  (let ((command (apprentice-utils-build-command cmdlist)))
    (apprentice-report-run command "apprentice-compile-report" apprentice-compile-buffer-name 'apprentice-compile-mode)))

(provide 'apprentice-compile)

;;; apprentice-compile.el ends here
