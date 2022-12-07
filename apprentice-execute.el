;;; apprentice-execute.el --- Elixir's script execution integration

;; Copyright © 2014-2017 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com

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

;; Elixir's script execution integration

;;; Code:

(require 'apprentice-test-mode)
(require 'apprentice-report)

(defgroup apprentice-execute nil
  "Elixir's script execution integration."
  :prefix "apprentice-execute-"
  :group 'apprentice)

;; Variables

(defcustom apprentice-execute-command "elixir"
  "The shell command for elixir."
  :type 'string
  :group 'apprentice-execute)

(defvar apprentice-execute-buffer-name "*apprentice elixir*"
  "Name of the elixir output buffer.")

(defvar apprentice-execute-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map))

;; Private functions

(defun apprentice-execute--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exist"))
  (apprentice-execute (list apprentice-execute-command (expand-file-name filename))))

(defun apprentice-execute--read-command (command)
  (read-shell-command "elixir command: " (concat command " ")))

;; Public functions

(defun apprentice-execute-this-buffer ()
  "Run the current buffer through elixir."
  (interactive)
  (apprentice-execute--file buffer-file-name))

(defun apprentice-execute-file (filename)
  "Run elixir with the given FILENAME."
  (interactive "Felixir: ")
  (apprentice-execute--file (expand-file-name filename)))

(define-derived-mode apprentice-execute-mode fundamental-mode "Elixir Execute Mode"
  "Major mode for execute Elixir files.

\\{apprentice-execute-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil))

(defun apprentice-execute (cmdlist)
  "Run a elixir with CMDLIST."
  (interactive (list (apprentice-execute--read-command apprentice-execute-command)))
  (let ((command (apprentice-utils-build-command cmdlist)))
    (apprentice-report-run command
                          "apprentice-execute-report"
                          apprentice-execute-buffer-name
                          'apprentice-execute-mode)))

(provide 'apprentice-execute)

;;; apprentice-execute.el ends here
