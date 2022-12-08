;;; apprentice-help.el --- Interaction with an Elixir IEx process

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

;; Interaction with an Elixir IEx process

;;; Code:

(require 'comint)
(require 'apprentice-key)
(require 'apprentice-scope)
(require 'apprentice-project)

(defgroup apprentice-iex nil
  "Interaction with an Elixir IEx process."
  :prefix "apprentice-iex-"
  :group 'apprentice)

(defcustom apprentice-iex-program-name "iex"
  "The shell command for iex."
  :type 'string
  :group 'apprentice-iex)

(defvar apprentice-iex-prompt-regexp "^\\(iex\\|\\.\\.\\.\\)(.+)>"
  "Prompt regex pattern of IEx interpreter.

Should match prompts that looks like these:
iex(1)>
...(1)>")

(defcustom apprentice-iex-prompt-read-only t
  "If non-nil, the prompt will be read-only."
  :type 'boolean
  :group 'apprentice-iex)

(defvar apprentice-iex-buffer nil
  "The buffer in which the Elixir IEx process is running.")

(defvar apprentice-iex-mode-hook nil
  "Hook for customizing `apprentice-iex-mode'.")

(defvar apprentice-iex-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; (define-key map "\t" 'company-complete)
    ;; (define-key map (kbd "TAB") 'company-complete)
    (define-key map (kbd (format "%s i r" apprentice-key-command-prefix)) 'apprentice-iex-open-input-ring)
    (define-key map (kbd (format "%s i c" apprentice-key-command-prefix)) 'apprentice-iex-clear-buffer)
    (define-key map (kbd (format "%s h e" apprentice-key-command-prefix)) 'apprentice-help-search-at-point)
    (define-key map (kbd "M-.") 'apprentice-goto-definition-at-point)
    map))

(define-derived-mode apprentice-iex-mode comint-mode "Apprentice-IEx"
  "Major mode for interacting with an Elixir IEx process.

\\<apprentice-iex-mode-map>"
  nil "Apprentice-IEx"
  (set (make-local-variable 'comint-prompt-regexp) apprentice-iex-prompt-regexp)
  (set (make-local-variable 'comint-prompt-read-only) apprentice-iex-prompt-read-only)
  (set (make-local-variable 'comint-input-autoexpand) nil)
  (set (make-local-variable 'comint-input-sender) 'apprentice-iex--send-command)
  (add-hook 'comint-output-filter-functions 'apprentice-iex-spot-prompt nil t))

(defun apprentice-iex-command (arg)
  (split-string-and-unquote
   (if (null arg) apprentice-iex-program-name
     (read-string "Command to run Elixir IEx: " (concat apprentice-iex-program-name arg)))))

(defun apprentice-iex-start-process (command)
  "Start an IEX process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `apprentice-iex-program-name'.
It runs the hook `apprentice-iex-mode-hook' after starting the process and
setting up the IEx buffer."
  (interactive (list (apprentice-iex-command current-prefix-arg)))
  (setq apprentice-iex-buffer
        (apply 'make-comint "Apprentice-IEx" (car command) nil (cdr command)))
  (with-current-buffer apprentice-iex-buffer
    (apprentice-iex-mode)
    (run-hooks 'apprentice-iex-mode-hook)))

(defun apprentice-iex-process (&optional arg)
  (or (if (buffer-live-p apprentice-iex-buffer)
          (get-buffer-process apprentice-iex-buffer))
      (progn
        (let ((current-prefix-arg arg))
          (call-interactively 'apprentice-iex-start-process))
        (apprentice-iex-process arg))))

(defun apprentice-iex--remove-newlines (string)
  (replace-regexp-in-string "\n" " " string))

(defun apprentice-iex-send-last-sexp ()
  "Send the previous sexp to the inferior IEx process."
  (interactive)
  (apprentice-iex-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun apprentice-iex-send-current-line ()
  "Sends the current line to the IEx process."
  (interactive)
  (let ((str (thing-at-point 'line)))
    (apprentice-iex--send-command (apprentice-iex-process) str)))

(defun apprentice-iex-send-current-line-and-go ()
  "Sends the current line to the inferior IEx process.
It also jump to the buffer."
  (interactive)
  (call-interactively 'apprentice-iex-send-current-line)
  (pop-to-buffer (process-buffer (apprentice-iex-process))))

(defun apprentice-iex-send-region-and-go ()
  "Sends the marked region to the inferior IEx process.
It also jump to the buffer."
  (interactive)
  (call-interactively 'apprentice-iex-send-region)
  (pop-to-buffer (process-buffer (apprentice-iex-process))))

(defun apprentice-iex-send-region (beg end)
  "Sends the marked region(BEG END) to the IEx process."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let* ((region (buffer-substring-no-properties beg end)))
    (apprentice-iex--send-command (apprentice-iex-process) region)))

(defun apprentice-iex-compile-this-buffer ()
  "Compiles the current buffer in the IEx process."
  (interactive)
  (let* ((path (if (apprentice-project-p)
		   (format "%s/_build/dev/" (apprentice-project-root))
		 "."))
	 (str (format "c(\"%s\", \"%s\")" (buffer-file-name) path)))
    (apprentice-iex--send-command (apprentice-iex-process) str)))

(defun apprentice-iex-compile-this-buffer-and-go ()
  "Compiles the current buffer in the IEx process and jump to the buffer."
  (interactive)
  (apprentice-iex-compile-this-buffer)
  (pop-to-buffer (process-buffer (apprentice-iex-process))))

(defun apprentice-iex-reload-module ()
  "Recompiles and reloads the current module in the IEx process."
  (interactive)
  (let ((str (format "r(%s)" (apprentice-scope-module))))
    (apprentice-iex--send-command (apprentice-iex-process) str)))

(defun apprentice-iex--send-command (proc str)
  (let ((lines (split-string str "\n" nil)))
    (with-current-buffer (process-buffer proc)
      (mapc (lambda (line)
	      (goto-char (process-mark proc))
	      (insert-before-markers (concat line "\n"))
	      (move-marker comint-last-input-end (point))
	      (comint-send-string proc (concat line "\n")))
	    lines))))

(defun apprentice-iex-spot-prompt (_string)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))))))

(defun apprentice-iex-clear-buffer ()
  "Clear the current iex process buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun apprentice-iex-open-input-ring ()
    "Open the buffer containing the input history."
    (interactive)
    (progn
      (comint-dynamic-list-input-ring)
      (other-window 1)))

;;;###autoload
(defalias 'run-elixir 'apprentice-iex-run)
(defalias 'inferior-elixir 'apprentice-iex-run)

;;;###autoload
(defun apprentice-iex-run (&optional arg)
  "Start an IEx process with ARG.
Show the IEx buffer if an IEx process is already run."
  (interactive "P")
  (let ((proc (apprentice-iex-process arg)))
    (pop-to-buffer (process-buffer proc))))

;;;###autoload
(defun apprentice-iex-project-run ()
  "Start an IEx process with mix 'iex -S mix'.
in the context of an Elixir project.Show the IEx buffer if an
IEx process is already run."
  (interactive)
  (if (apprentice-project-p)
      (let ((default-directory (apprentice-project-root)))
        (pop-to-buffer (process-buffer (apprentice-iex-process " -S mix"))))
    (message "No mix.exs file available. Please use `apprentice-iex-run' instead.")))

(provide 'apprentice-iex)

;;; apprentice-iex.el ends here