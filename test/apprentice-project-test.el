;;; apprentice-project-tests.el ---

;; Copyright © 2014-2017 Samuel Tonini
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


(ert-deftest apprentice-project/list-files-from-directory ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib")
   (f-mkdir "lib" "path")
   (f-touch "lib/file.ex")
   (f-touch "lib/another.ex")
   (f-touch "lib/path/foo.ex")
   (should (equal (apprentice-project-read-dir (apprentice-project-root) "lib")
                  '("lib/another.ex" "lib/file.ex" "lib/path/foo.ex")))))

(ert-deftest test-project-root/no-argument ()
  "Should use `default-directory' when no argument."
  (setq apprentice-project-root-path-cache nil)
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "path" "to" "lib")
   (should (equal (apprentice-project-root) apprentice-sandbox-path))))

(ert-deftest test-project-root/directory-as-argument  ()
  "Should find root directory when directory as argument."
  (setq apprentice-project-root-path-cache nil)
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "path" "to" "lib")
   (should (equal (apprentice-project-root "path/to/lib") apprentice-sandbox-path))))

(ert-deftest test-project-root/no-project-root  ()
  "Should return nil when no root."
  (setq apprentice-project-root-path-cache nil)
  (with-sandbox
   (f-mkdir "path" "to" "lib")
   (should (equal (apprentice-project-root "path/to/lib") nil))))

(ert-deftest test-project-name/no-project-root ()
  "Should return an empty string"
  (setq apprentice-project-root-path-cache nil)
  (with-sandbox
   (should (equal (apprentice-project-name) ""))))

(ert-deftest test-project-name/project-exists ()
  (setq apprentice-project-root-path-cache nil)
  "Should return name of the project"
  (with-sandbox
   (f-touch "mix.exs")
   (should (equal (apprentice-project-name) "sandbox"))))

(ert-deftest apprentice-project/file-under-test ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "web" "controllers")
   (f-mkdir "test" "controllers")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/file.ex")
   (f-touch "test/path/to/file_test.exs")
   (f-touch "web/controllers/my_controller.ex")
   (f-touch "test/controllers/my_controller_test.exs")
   (should (equal (file-name-nondirectory
                   (apprentice-project-file-under-test "test/path/to/file_test.exs" "lib"))
                  "file.ex"))
   (should (equal (file-name-nondirectory
                   (apprentice-project-file-under-test "test/controllers/my_controller_test.exs" "web"))
                  "my_controller.ex"))))

(ert-deftest alchemsit-project/switch-from-test-to-file-under-test-1 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/file.ex")
   (f-touch "test/path/to/file_test.exs")
   (find-file "test/path/to/file_test.exs")
   (apprentice-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "file.ex"))))

(ert-deftest alchemsit-project/switch-from-test-to-file-under-test-2 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "web" "controllers")
   (f-mkdir "test" "controllers")
   (f-touch "web/controllers/my_controller.ex")
   (f-touch "test/controllers/my_controller_test.exs")
   (find-file "test/controllers/my_controller_test.exs")
   (apprentice-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "my_controller.ex"))))

(ert-deftest apprentice-project/switch-from-file-under-test-to-test-file-1 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/other_file.ex")
   (f-touch "test/path/to/other_file_test.exs")
   (find-file "lib/path/to/other_file.ex")
   (apprentice-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "other_file_test.exs"))))

(ert-deftest apprentice-project/switch-from-file-under-test-to-test-file-2 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "web" "views")
   (f-mkdir "test" "views")
   (f-touch "web/views/my_view.ex")
   (f-touch "test/views/my_view_test.exs")
   (find-file "web/views/my_view.ex")
   (apprentice-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "my_view_test.exs"))))

(ert-deftest apprentice-project/inside-elixir-codebase ()
  (setq apprentice-project-elixir-source-dir apprentice-sandbox-path)
  (with-sandbox
   (should (apprentice-project-elixir-p))
   (should (equal (apprentice-project-elixir-root) apprentice-sandbox-path))))

(ert-deftest apprentice-project/not-inside-elixir-codebase ()
  (setq apprentice-project-elixir-source-dir nil)
  (with-sandbox
   (should-not (apprentice-project-elixir-p))
   (should (equal nil (apprentice-project-elixir-root)))))

(provide 'apprentice-project-tests)

;;; apprentice-project-test.el ends here
