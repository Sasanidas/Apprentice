;;; apprentice.el --- Elixir tooling integration into Emacs

;; Copyright © 2014-2017 Samuel Tonini
;; Copyright © 2022 Fermin MF
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>
;; Maintainer: Fermin MF <fmfs@posteo.net>
;; URL: https://github.com/Sasanidas/Apprentice
;; Version: 0.5.0
;; Package-Requires: ((elixir-mode "2.2.5") (emacs "24.4") (company "0.8.0"))
;; Keywords: languages, elixir, elixirc, mix, hex, apprentice

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
;;
;;  What Does Apprentice Do For You?
;;
;;    Apprentice brings you all the Elixir tooling and power inside your Emacs editor.
;;
;;  Apprentice comes with a bunch of features, which are:
;;
;;    * Mix integration
;;    * Compile & Execution of Elixir code
;;    * Inline code evaluation
;;    * Inline macro expanding
;;    * Documentation lookup
;;    * Definition lookup
;;    * Powerful IEx integration
;;    * Elixir project management
;;    * Phoenix support

;;; Code:

;; Tell the byte compiler about autoloaded functions from packages


(defgroup apprentice nil
  "Elixir Tooling Integration Into Emacs."
  :prefix "apprentice-"
  :group 'applications
  :link '(url-link :tag "Website" "")
  :link '(url-link :tag "Github" "https://github.com/Sasanidas/Apprentice")
  :link '(emacs-commentary-link :tag "Commentary" "apprentice"))

(defvar apprentice-mode-keymap nil)

(require 'easymenu)
(require 'elixir-mode)
(require 'apprentice-utils)
(require 'apprentice-key)
(require 'apprentice-eval)
(require 'apprentice-report)
(require 'apprentice-mix)
(require 'apprentice-hex)
(require 'apprentice-hooks)
(require 'apprentice-execute)
(require 'apprentice-iex)
(require 'apprentice-compile)
(require 'apprentice-refcard)
(require 'apprentice-macroexpand)
(require 'apprentice-phoenix)

(defun apprentice-mode-hook ()
  "Hook which enables `apprentice-mode'."
  (apprentice-mode 1))


(defvar apprentice-version "0.5.0")

(defun apprentice-version (&optional show-version)
  "Get the Apprentice version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (message "Apprentice version: %s" apprentice-version))

(defun apprentice-elixir-version ()
  "Display the current Elixir version on the system."
  (interactive)
  (message "Elixir %s" (apprentice-utils-elixir-version)))

(define-prefix-command 'apprentice-mode-keymap)

;;;###autoload
(define-minor-mode apprentice-mode
  "Toggle apprentice mode.

Key bindings:
\\{apprentice-mode-map}"
  nil
  ;; The indicator for the mode line.
  " apprentice"
  :group 'apprentice
  :global nil
  :keymap `((,apprentice-key-command-prefix . apprentice-mode-keymap))
  (cond (apprentice-mode
         ;;(apprentice-server-start-if-not-running)
         (apprentice-test-initialize-modeline))
        (t
         (apprentice-test-reset-modeline))))

(let ((map apprentice-mode-keymap))
  (define-key map (kbd "x") 'apprentice-mix)
  (define-key map (kbd "t") 'apprentice-mix-test)
  (define-key map (kbd "r") 'apprentice-mix-rerun-last-test)

  (define-key map (kbd "m c") 'apprentice-mix-compile)
  (define-key map (kbd "m r") 'apprentice-mix-run)
  (define-key map (kbd "m l") 'apprentice-mix-rerun-last-task)
  (define-key map (kbd "m t f") 'apprentice-mix-test-file)
  (define-key map (kbd "m t b") 'apprentice-mix-test-this-buffer)
  (define-key map (kbd "m t .") 'apprentice-mix-test-at-point)
  (define-key map (kbd "m t s") 'apprentice-mix-test-stale)
  (define-key map (kbd "m t r") 'apprentice-mix-rerun-last-test)

  (define-key map (kbd "c c") 'apprentice-compile)
  (define-key map (kbd "c f") 'apprentice-compile-file)
  (define-key map (kbd "c b") 'apprentice-compile-this-buffer)

  (define-key map (kbd "e e") 'apprentice-execute)
  (define-key map (kbd "e f") 'apprentice-execute-file)
  (define-key map (kbd "e b") 'apprentice-execute-this-buffer)

  (define-key map (kbd "h h") 'apprentice-help)
  (define-key map (kbd "h i") 'apprentice-help-history)
  (define-key map (kbd "h e") 'apprentice-help-search-at-point)
  (define-key map (kbd "h r") 'apprentice-refcard)

  (define-key map (kbd "p s") 'apprentice-project-toggle-file-and-tests)
  (define-key map (kbd "p o") 'apprentice-project-toggle-file-and-tests-other-window)
  (define-key map (kbd "p t") 'apprentice-project-run-tests-for-current-file)
  (define-key map (kbd "p l") 'apprentice-project-find-lib)
  (define-key map (kbd "p f") 'apprentice-project-find-test)

  (define-key map (kbd "i i") 'apprentice-iex-run)
  (define-key map (kbd "i p") 'apprentice-iex-project-run)
  (define-key map (kbd "i l") 'apprentice-iex-send-current-line)
  (define-key map (kbd "i c") 'apprentice-iex-send-current-line-and-go)
  (define-key map (kbd "i r") 'apprentice-iex-send-region)
  (define-key map (kbd "i m") 'apprentice-iex-send-region-and-go)
  (define-key map (kbd "i b") 'apprentice-iex-compile-this-buffer)
  (define-key map (kbd "i R") 'apprentice-iex-reload-module)

  (define-key map (kbd "v l") 'apprentice-eval-current-line)
  (define-key map (kbd "v k") 'apprentice-eval-print-current-line)
  (define-key map (kbd "v j") 'apprentice-eval-quoted-current-line)
  (define-key map (kbd "v h") 'apprentice-eval-print-quoted-current-line)
  (define-key map (kbd "v o") 'apprentice-eval-region)
  (define-key map (kbd "v i") 'apprentice-eval-print-region)
  (define-key map (kbd "v u") 'apprentice-eval-quoted-region)
  (define-key map (kbd "v y") 'apprentice-eval-print-quoted-region)
  (define-key map (kbd "v q") 'apprentice-eval-buffer)
  (define-key map (kbd "v w") 'apprentice-eval-print-buffer)
  (define-key map (kbd "v e") 'apprentice-eval-quoted-buffer)
  (define-key map (kbd "v r") 'apprentice-eval-print-quoted-buffer)
  (define-key map (kbd "v !") 'apprentice-eval-close-popup)

  (define-key map (kbd "o l") 'apprentice-macroexpand-once-current-line)
  (define-key map (kbd "o L") 'apprentice-macroexpand-once-print-current-line)
  (define-key map (kbd "o k") 'apprentice-macroexpand-current-line)
  (define-key map (kbd "o K") 'apprentice-macroexpand-print-current-line)
  (define-key map (kbd "o i") 'apprentice-macroexpand-once-region)
  (define-key map (kbd "o I") 'apprentice-macroexpand-once-print-region)
  (define-key map (kbd "o r") 'apprentice-macroexpand-region)
  (define-key map (kbd "o R") 'apprentice-macroexpand-print-region)
  (define-key map (kbd "o !") 'apprentice-macroexpand-close-popup)

  (define-key map (kbd "X i") 'apprentice-hex-info-at-point)
  (define-key map (kbd "X r") 'apprentice-hex-releases-at-point)
  (define-key map (kbd "X R") 'apprentice-hex-releases)
  (define-key map (kbd "X s") 'apprentice-hex-search)
  (define-key map (kbd "X I") 'apprentice-hex-info)
  (define-key map (kbd "X d") 'apprentice-hex-all-dependencies))

(define-key apprentice-mode-map (kbd "C-c M-r") 'apprentice-test-toggle-test-report-display)

(easy-menu-define apprentice-mode-menu apprentice-mode-map
  "Apprentice mode menu."
  '("Apprentice"
    ("Evaluate"
     ["Evaluate current line" apprentice-eval-current-line]
     ["Evaluate current line and print" apprentice-eval-print-current-line]
     ["Evaluate quoted current line" apprentice-eval-quoted-current-line]
     ["Evaluate quoted current line and print" apprentice-eval-print-quoted-current-line]
     "---"
     ["Evaluate region" apprentice-eval-region]
     ["Evaluate region and print" apprentice-eval-print-region]
     ["Evaluate quoted region" apprentice-eval-quoted-region]
     ["Evaluate quoted region and print" apprentice-eval-print-quoted-region]
     "---"
     ["Evaluate buffer" apprentice-eval-buffer]
     ["Evaluate buffer and print" apprentice-eval-print-buffer]
     ["Evaluate quoted buffer" apprentice-eval-quoted-buffer]
     ["Evaluate quoted buffer and print" apprentice-eval-print-quoted-buffer])
    ;; ("Macroexpand"
    ;;  ["Macro expand once current line" apprentice-macroexpand-once-current-line]
    ;;  ["Macro expand once current line and print" apprentice-macroexpand-print-current-line]
    ;;  ["Macro expand current line" apprentice-macroexpand-current-line]
    ;;  ["Macro expand current line and print" apprentice-macroexpand-print-current-line]
    ;;  "---"
    ;;  ["Macro expand once region" apprentice-macroexpand-once-region]
    ;;  ["Macro expand once region and print" apprentice-macroexpand-print-region]
    ;;  ["Macro expand region" apprentice-macroexpand-region]
    ;;  ["Macro expand region and print" apprentice-macroexpand-print-region])
    ("Compile"
     ["Compile..." apprentice-compile]
     ["Compile this buffer" apprentice-compile-this-buffer]
     ["Compile file" apprentice-compile-file])
    ("Execute"
     ["Execute..." apprentice-compile]
     ["Execute this buffer" apprentice-execute-this-buffer]
     ["Execute file" apprentice-execute-file])
    ("Mix"
     ["Mix compile..." apprentice-mix-compile]
     ["Mix run..." apprentice-mix-run]
     "---"
     ["Mix run whole test suite." apprentice-mix-test]
     ["Mix test this buffer" apprentice-mix-test-this-buffer]
     ["Mix test file..." apprentice-mix-test-file]
     ["Mix test at point" apprentice-mix-test-at-point]
     ["Mix run stale tests (Elixir 1.3+)" apprentice-mix-test-stale]
     "---"
     ["Mix..." apprentice-mix]
     "---"
     ["Display mix buffer" apprentice-mix-display-mix-buffer]
     "---"
     ["Mix help..." apprentice-mix-help])
    ("IEx"
     ["IEx send current line" apprentice-iex-send-current-line]
     ["IEx send current line and go" apprentice-iex-send-current-line-and-go]
     "---"
     ["IEx send last region" apprentice-iex-send-last-sexp]
     ["IEx send region" apprentice-iex-send-region]
     ["IEx send region and go" apprentice-iex-send-region-and-go]
     "---"
     ["IEx compile this buffer" apprentice-iex-compile-this-buffer]
     ["IEx recompile this buffer" apprentice-iex-recompile-this-buffer]
     "---"
     ["IEx run" apprentice-iex-run])
    ("Project"
     ["Project list all files inside test directory" apprentice-project-find-test]
     ["Project list all files inside lib directory" apprentice-project-find-lib]
     ["Project toggle between file and test" apprentice-project-toggle-file-and-tests]
     ["Project toggle between file and test in other window" apprentice-project-toggle-file-and-tests-other-window])
    ("Documentation"
     ["Documentation search..." apprentice-help]
     ["Documentation search history..." apprentice-help-history]
     "---"
     ["Documentation search at point..." apprentice-help-search-at-point])
    ("About"
     ["Show Apprentice version" apprentice-version t])))

(add-hook 'elixir-mode-hook 'apprentice-mode-hook)

(provide 'apprentice)

;;; apprentice.el ends here
