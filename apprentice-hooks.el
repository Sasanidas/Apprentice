;;; apprentice-hooks.el --- Hooks functionality

;; Copyright © 2014-2017 Samuel Tonini
;; Copyright © 2022 Fermin MF

;; Author: Samuel Tonini <tonini.samuel@gmail.com
;;         Dave Thomas <http://pragdave.me>
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

;; Hooks functionality

;;; Code:

(require 'apprentice-project)
(require 'apprentice-mix)
(require 'apprentice-report)
(require 'apprentice-test-mode)

(defgroup apprentice-hooks nil
  "Hooks"
  :prefix "apprentice-hooks-"
  :group 'apprentice)

(defcustom apprentice-hooks-test-on-save nil
  "If t, run `apprentice-mix-test' on save."
  :type 'boolean
  :group 'apprentice-hooks)

(defcustom apprentice-hooks-compile-on-save nil
  "If t, run `apprentice-mix-compile' on save."
  :type 'boolean
  :group 'apprentice-hooks)

(defun apprentice-hooks-test-on-save ()
  (when (and apprentice-hooks-test-on-save
             (apprentice-project-p))
    (apprentice-report-run "mix test"
                          apprentice-test-report-process-name
                          apprentice-test-report-buffer-name
                          #'apprentice-test-report-mode
                          #'apprentice-test--handle-exit
                          t)))

(defun apprentice-hooks-compile-on-save ()
  (when (and apprentice-hooks-compile-on-save
             (apprentice-project-p))
    (apprentice-report-run "mix compile"
                          apprentice-mix-process-name
                          apprentice-mix-buffer-name
                          #'apprentice-mix-mode
                          nil
                          t)))


(define-minor-mode apprentice-hooks-mode
  "Run hooks on saving the buffer."
  (if (bound-and-true-p apprentice-hooks-mode)
      (progn
        (add-hook 'after-save-hook #'apprentice-hooks-test-on-save nil t)
        (add-hook 'after-save-hook #'apprentice-hooks-compile-on-save nil t))
    (progn
      (remove-hook 'after-save-hook #'apprentice-hooks-test-on-save t)
      (remove-hook 'after-save-hook #'apprentice-hooks-compile-on-save t))))

(provide 'apprentice-hooks)

;;; apprentice-hooks.el ends here
