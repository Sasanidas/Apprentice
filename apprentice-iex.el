;;; apprentice-help.el --- Interaction with an Elixir IEx process

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

;; Interaction with an Elixir IEx process

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'pcase)
(require 'apprentice-scope)
(require 'apprentice-project)

(declare-function vterm-clear "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-mode "ext:vterm")

(defgroup apprentice-iex nil
  "Interaction with an Elixir IEx process."
  :prefix "apprentice-iex-"
  :group 'apprentice)

(defcustom apprentice-iex-type :comint-mode
  "Type of buffer to use for IEx.
This variable can take 2 values:
 - :comint-mode for `comint-mode' (default)
 - :vterm-mode to use `vterm-mode'(apprentice version)"
  :type 'keyword
  :group 'apprentice-iex)

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

(defvar apprentice-iex-comint-mode-hook nil
  "Hook for customizing `apprentice-iex-comint-mode'.")

(defvar apprentice-iex-vterm-mode-hook nil
  "Hook for customizing `apprentice-iex-vterm-mode'.")

(defvar apprentice-iex-vterm-input-ring nil
  "Apprentice custom input-ring.")

;;TODO: Adapt this keymap to vterm version
(defvar apprentice-iex-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; (define-key map "\t" 'company-complete)
    ;; (define-key map (kbd "TAB") 'company-complete)
    ;; (define-key map (kbd (format "%s i r" apprentice-key-command-prefix)) 'apprentice-iex-open-input-ring)
    ;; (define-key map (kbd (format "%s i c" apprentice-key-command-prefix)) 'apprentice-iex-clear-buffer)
    ;;(define-key map (kbd (format "%s h e" apprentice-key-command-prefix)) 'apprentice-help-search-at-point)
    ;;(define-key map (kbd "M-.") 'apprentice-goto-definition-at-point)
    map))

(define-derived-mode apprentice-iex-comint-mode comint-mode "Apprentice-IEx"
  "Major mode for interacting with an Elixir IEx process.
It uses `comint-mode' for REPL interaction.

\\<apprentice-iex-mode-map>"
  nil "Apprentice-IEx"
  (set (make-local-variable 'comint-prompt-regexp) apprentice-iex-prompt-regexp)
  (set (make-local-variable 'comint-prompt-read-only) apprentice-iex-prompt-read-only)
  (set (make-local-variable 'comint-input-autoexpand) nil)
  (set (make-local-variable 'comint-input-sender) #'apprentice-iex--send-command)
  (add-hook 'comint-output-filter-functions #'apprentice-iex-spot-prompt nil t))

(cl-defun apprentice-iex-vterm-insert-input ()
  "Advice to `vterm-send-return'.
It inserts `apprentice-iex--vterm-cstring' into
`apprentice-iex-vterm-input-ring'."

  (when (equal major-mode 'apprentice-iex-vterm-mode)
    (with-current-buffer (current-buffer)
      (save-excursion
	(beginning-of-line)
	(when (re-search-forward apprentice-iex-prompt-regexp nil t)
	  (let ((be (point))
		(en (line-end-position)))
	    (ring-insert apprentice-iex-vterm-input-ring
			 (string-trim
			  (buffer-substring-no-properties be en)))))))))

(define-derived-mode apprentice-iex-vterm-mode vterm-mode "Apprentice-IEx"
  "Major mode for interacting with an Elixir IEx process.
It uses `vterm-mode' for REPL interaction.

\\<apprentice-iex-mode-map>"
  nil "Apprentice-IEx"
  (set (make-local-variable 'apprentice-iex-vterm-input-ring) (make-ring 500))
  (set (make-local-variable 'apprentice-iex--vterm-cstring) nil)

  (advice-add 'vterm-send-return :before #'apprentice-iex-vterm-insert-input))

(defun apprentice-iex-command (arg)
  "Launch an iex command with ARG as arguments."
  (split-string-and-unquote
   (if (null arg) apprentice-iex-program-name
     (read-string "Command to run Elixir IEx: " (concat apprentice-iex-program-name arg)))))

(cl-defmethod apprentice-iex--start-process (command (type (eql :comint-mode)))
  "Internal function that launch an iex process with COMMAND.
In this case, it uses the TYPE `comint-mode'."
  (setq apprentice-iex-buffer
        (apply #'make-comint "Apprentice-IEx" (car command) nil (cdr command)))
  (with-current-buffer apprentice-iex-buffer
    (apprentice-iex-comint-mode)
    (run-hooks 'apprentice-iex-comint-mode-hook)))

(cl-defmethod apprentice-iex--start-process (command (type (eql :vterm-mode)))
  "Internal function that launch an iex process with COMMAND.
In this case, it uses the TYPE `vterm-mode'."
  (setq apprentice-iex-buffer
	(let ((buffer (generate-new-buffer "*Apprentice-IEx*")))
	  (with-current-buffer buffer
	    (apprentice-iex-vterm-mode))
	  (pop-to-buffer buffer)))
  (with-current-buffer apprentice-iex-buffer
    (vterm-send-string (mapconcat #'identity (append command '("&&" "exit")) " "))
    (vterm-send-return)
    (run-hooks 'apprentice-iex-vterm-mode-hook)))

(defun apprentice-iex-start-process (command)
  "Start an IEX process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `apprentice-iex-program-name'.
It runs the hook `apprentice-iex-mode-hook' after starting the process and
setting up the IEx buffer."
  (interactive (list (apprentice-iex-command current-prefix-arg)))
  (apprentice-iex--start-process command apprentice-iex-type))

(defun apprentice-iex-process (&optional arg)
  "Get the iex process or create a new one.
When calling with ARG it creates the new one with those arguments."
  (or (if (buffer-live-p apprentice-iex-buffer)
          (get-buffer-process apprentice-iex-buffer))
      (progn
        (let ((current-prefix-arg arg))
          (call-interactively #'apprentice-iex-start-process))
        (apprentice-iex-process arg))))

(defun apprentice-iex--remove-newlines (string)
  "Remove newlines from STRING."
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
  (call-interactively #'apprentice-iex-send-current-line)
  (pop-to-buffer (process-buffer (apprentice-iex-process))))

(defun apprentice-iex-send-region-and-go ()
  "Sends the marked region to the inferior IEx process.
It also jump to the buffer."
  (interactive)
  (call-interactively #'apprentice-iex-send-region)
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
  "Recompile and reloads the current module in the IEx process."
  (interactive)
  (let ((str (format "r(%s)" (apprentice-scope-module))))
    (apprentice-iex--send-command (apprentice-iex-process) str)))


(cl-defmethod apprentice-iex--command (proc (lines list) (type (eql :comint-mode)))
  "Process the PROC LINES.
In this case, it uses the TYPE `comint-mode'.`"
  (with-current-buffer (process-buffer proc)
    (cl-loop for line in lines
	     do (progn
		  (goto-char (process-mark proc))
		  (insert-before-markers (concat line "\n"))
		  (move-marker comint-last-input-end (point))
		  (comint-send-string proc (concat line "\n"))))))

(cl-defmethod apprentice-iex--command (proc (lines list) (type (eql :vterm-mode)))
  "Process the PROC LINES.
In this case, it uses the TYPE `vterm-mode'.`"
  (with-current-buffer (process-buffer proc)
    (vterm-send-string
     (mapconcat #'identity lines " "))
    (vterm-send-return)))

(defun apprentice-iex--send-command (proc str)
  "Send STR to PROC."
  (let ((lines (split-string str "\n" nil)))
    (apprentice-iex--command proc lines apprentice-iex-type)))

(defun apprentice-iex-spot-prompt (_string)
  "Move the point to the process mark."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))))))

(cl-defmethod apprentice-iex--clear-buffer ((type (eql :comint-mode)))
  "Clear the buffer of a iex REPL of TYPE `comint-mode'."
  (with-current-buffer (process-buffer
			(apprentice-iex-process))
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

(cl-defmethod apprentice-iex--clear-buffer ((type (eql :vterm-mode)))
  "Clear the buffer of a iex REPL of TYPE `vterm-mode'."
  (with-current-buffer (process-buffer
			(apprentice-iex-process))
    (vterm-clear)))

(defun apprentice-iex-clear-buffer ()
  "Clear the current iex process buffer."
  (interactive)
  (apprentice-iex--clear-buffer apprentice-iex-type))

(defun apprentice-iex-open-input-ring ()
    "Open the buffer containing the input history."
    (interactive)
    (progn
      (comint-dynamic-list-input-ring)
      (other-window 1)))

;;;###autoload
(defun apprentice-iex-run (&optional arg)
  "Start an IEx process with ARG.
Show the IEx buffer if an IEx process is already run."
  (interactive "P")
  (when (eq :vterm-mode apprentice-iex-type)
    (require 'vterm))
  (let ((proc (apprentice-iex-process arg)))
    (pop-to-buffer (process-buffer proc))))

;;;###autoload
(defun apprentice-iex-project-run ()
  "Start an IEx process with mix \"iex -S mix\".
in the context of an Elixir project.Show the IEx buffer if an
IEx process is already run."
  (interactive)
  (when (eq :vterm-mode apprentice-iex-type)
    (require 'vterm))
  (if (apprentice-project-p)
      (let ((default-directory (apprentice-project-root)))
        (pop-to-buffer (process-buffer (apprentice-iex-process " -S mix"))))
    (message "No mix.exs file available. Please use `apprentice-iex-run' instead.")))

(provide 'apprentice-iex)

;;; apprentice-iex.el ends here
