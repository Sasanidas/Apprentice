;;; apprentice-trace.el --- Trace IEx handy functions -*- lexical-binding: t -*-

;; Copyright © 2023 Fermin MF

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

;; Some IEx interaction with the erlang tracer

;;; Code:

(require 'cl-lib)
(require 'apprentice-iex)

;; TODO: Save a list of all the function so we can untrace dynamically
(defgroup apprentice-trace nil
  "IEx trace utilities."
  :prefix "apprentice-trace-"
  :group 'apprentice)

(defcustom apprentice-trace-start-command ":dbg.tracer()"
  "Default command to start the tracer."
  :type 'string
  :group 'apprentice-trace)

(defcustom apprentice-trace-stop-command
  ":dbg.get_tracer() |> elem(1) |> :dbg.stop_trace_client"
  "Default command to stop the tracer client."
  :type 'string
  :group 'apprentice-trace)

(defun apprentice-trace-start-trace ()
  "Send the command to the REPL to start the trace."
  (interactive)
  (when-let ((process (apprentice-iex-process)))
    (apprentice-iex--send-command process apprentice-trace-start-command)))

(defun apprentice-trace-stop-trace ()
  "Send the command to the REPL to start the trace."
  (interactive)
  (when-let ((process (apprentice-iex-process)))
    (apprentice-iex--send-command process apprentice-trace-stop-command)))

(defun apprentice-trace-all-calls ()
  (interactive)
  (when-let ((process (apprentice-iex-process)))
    (apprentice-iex--send-command process ":dbg.p(:all,:c)")))

(defun apprentice-trace-current-function ()
  (interactive)
  (let* ((module (apprentice-scope-module))
	 (func (apprentice-scope-current-function)))
    (when-let ((process (apprentice-iex-process)))
      (apprentice-iex--send-command
       process (format ":dbg.tpl(%s,:%s,:x)" module func)))))

(defun apprentice-trace-untrace-current-function ()
  (interactive)
  (let* ((module (apprentice-scope-module))
	 (func (apprentice-scope-current-function)))
    (when-let ((process (apprentice-iex-process)))
      (apprentice-iex--send-command
       process (format ":dbg.ctp(%s,:%s)" module func)))))


(provide 'apprentice-trace)

;;; apprentice-trace.el ends here
