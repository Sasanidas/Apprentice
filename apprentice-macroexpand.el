;;; apprentice-macroexpand.el --- Macro expansion support -*- lexical-binding: t -*-

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

;; Macro expansion support

;;; Code:

(require 'apprentice-interact)

(defgroup apprentice-macroexpand nil
  "Macro expansion support."
  :prefix "apprentice-macroexpand-"
  :group 'apprentice)

(defvar apprentice-macroexpand-filter-output nil)

(defconst apprentice-macroexpand-buffer-name "*apprentice macroexpand*"
  "Name of the Elixir Macro expand buffer.")

;; (defun apprentice-macroexpand-filter (_process output)
;;   (setq apprentice-macroexpand-filter-output (cons output apprentice-macroexpand-filter-output))
;;   (when (apprentice-server-contains-end-marker-p output)
;;     (apprentice-interact-create-popup apprentice-macroexpand-buffer-name
;;                                      (apprentice-server-prepare-filter-output apprentice-macroexpand-filter-output)
;;                                      #'(lambda ()
;;                                          (elixir-mode)
;;                                          (apprentice-macroexpand-mode)))
;;     (setq apprentice-macroexpand-filter-output nil)))

;; (defun apprentice-macroexpand-insert-filter (_process output)
;;   (setq apprentice-macroexpand-filter-output (cons output apprentice-macroexpand-filter-output))
;;   (when (apprentice-server-contains-end-marker-p output)
;;     (apprentice-interact-insert-as-comment
;;      (apprentice-server-prepare-filter-output apprentice-macroexpand-filter-output))
;;     (setq apprentice-macroexpand-filter-output nil)))

;; (defun apprentice-macroexpand-expand-request (expr)
;;   (let ((file (make-temp-file "apprentice-expand" nil ".exs")))
;;     (with-temp-file file
;;       (insert expr))
;;     (apprentice-server-eval (format "{ :expand, '%s' }" file) #'apprentice-macroexpand-filter)))

;; (defun apprentice-macroexpand-expand-and-print-request (expr)
;;   (let ((file (make-temp-file "apprentice-expand" nil ".exs")))
;;     (with-temp-file file
;;       (insert expr))
;;     (apprentice-server-eval (format "{ :expand, '%s' }" file) #'apprentice-macroexpand-insert-filter)))

;; (defun apprentice-macroexpand-expand-once-request (expr)
;;   (let ((file (make-temp-file "apprentice-expand-once" nil ".exs")))
;;     (with-temp-file file
;;       (insert expr))
;;     (apprentice-server-eval (format "{ :expand_once, '%s' }" file) #'apprentice-macroexpand-filter)))

;; (defun apprentice-macroexpand-expand-once-and-print-request (expr)
;;   (let ((file (make-temp-file "apprentice-expand-once" nil ".exs")))
;;     (with-temp-file file
;;       (insert expr))
;;     (apprentice-server-eval (format "{ :expand_once, '%s' }" file) #'apprentice-macroexpand-insert-filter)))

;; (defun apprentice-macroexpand-current-line ()
;;   "Macro expand the Elixir code on the current line."
;;   (interactive)
;;   (apprentice-macroexpand-expand-request (thing-at-point 'line)))

;; (defun apprentice-macroexpand-print-current-line ()
;;   "Macro expand the Elixir code on the current line and insert the result."
;;   (interactive)
;;   (apprentice-macroexpand-expand-and-print-request (thing-at-point 'line)))

;; (defun apprentice-macroexpand-once-current-line ()
;;   "Macro expand the Elixir code on the current line."
;;   (interactive)
;;   (apprentice-macroexpand-expand-once-request (thing-at-point 'line)))

;; (defun apprentice-macroexpand-once-print-current-line ()
;;   "Macro expand the Elixir code on the current line and insert the result."
;;   (interactive)
;;   (apprentice-macroexpand-expand-once-and-print-request (thing-at-point 'line)))

;; (defun apprentice-macroexpand-print-region (beg end)
;;   "Macro expand the Elixir code on marked region and insert the result."
;;   (interactive (list (point) (mark)))
;;   (unless (and beg end)
;;     (error "The mark is not set now, so there is no region"))
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (when (> end beg)
;;       (exchange-point-and-mark))
;;     (apprentice-macroexpand-expand-and-print-request string)))

;; (defun apprentice-macroexpand-region (beg end)
;;   "Macro expand the Elixir code on marked region."
;;   (interactive (list (point) (mark)))
;;   (unless (and beg end)
;;     (error "The mark is not set now, so there is no region"))
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (apprentice-macroexpand-expand-request string)))

;; (defun apprentice-macroexpand-once-print-region (beg end)
;;   "Macro expand the Elixir code on marked region once and insert the result."
;;   (interactive "r")
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (when (> end beg)
;;       (exchange-point-and-mark))
;;     (apprentice-macroexpand-expand-once-and-print-request string)))

;; (defun apprentice-macroexpand-once-region (beg end)
;;   "Macro expand the Elixir code on marked region once."
;;   (interactive (list (point) (mark)))
;;   (unless (and beg end)
;;     (error "The mark is not set now, so there is no region"))
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (apprentice-macroexpand-expand-once-request string)))

;; (defun apprentice-macroexpand-close-popup ()
;;   "Quit the macroexpand buffer window."
;;   (interactive)
;;   (quit-windows-on apprentice-macroexpand-buffer-name))

;; (defvar apprentice-macroexpand-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "q") #'quit-window)
;;     map)
;;   "Keymap for `apprentice-macroexpand-mode'.")

;; (define-minor-mode apprentice-macroexpand-mode
;;   "Minor mode for Apprentice Elixir macroexpand functionality.

;; \\{apprentice-macroexpand-mode-map}"
;;   nil
;;   apprentice-macroexpand-mode-map)

(provide 'apprentice-macroexpand)

;;; apprentice-macroexpand.el ends here
