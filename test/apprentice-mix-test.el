;;; apprentice-mix-test.el --- -*- lexical-binding: t; -*-

;; Copyright © 2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

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

;;; Code:
(require 'apprentice-test-helper)

(defun prepare-test-report-buffer ()
  (when (process-live-p (get-buffer-process (get-buffer apprentice-test-report-buffer-name)))
    (set-process-query-on-exit-flag (get-buffer-process (get-buffer apprentice-test-report-buffer-name)) nil)))

(ert-deftest test-mix/run-mix-test ()
  (prepare-test-report-buffer)
  (cd "test/dummy_elixir/test/")
  (apprentice-mix-test)
  (should (equal "" apprentice-last-run-test))
  (delay 2.0 (lambda ()
               (should (apprentice-report--last-run-successful-p))))
  (wait 2.1))

(ert-deftest test-mix/run-mix-test-file ()
  (prepare-test-report-buffer)
  (cd "test/dummy_elixir/test/")
  (apprentice-mix-test-file "dummy_elixir_test.exs")
  (should (equal (expand-file-name "dummy_elixir_test.exs") apprentice-last-run-test))
  (delay 2.0 (lambda ()
               (should (apprentice-report--last-run-successful-p))))
  (wait 2.1))

(ert-deftest test-mix/run-mix-test-stale ()
  (prepare-test-report-buffer)
  (cd "test/dummy_elixir/test/")
  (apprentice-mix-test-stale)
  ;; CI runs multiple elixir versions so check correct version here
  (if (apprentice-utils-elixir-version-check-p 1 3 0)
      (should (equal "--stale" apprentice-last-run-test))
    (should (equal "" apprentice-last-run-test)))
  (delay 2.1 (lambda ()
               (should (apprentice-report--last-run-successful-p))))
  (wait 2.1))

(provide 'apprentice-mix-test)

;;; apprentice-mix-test.el ends here

