;;; apprentice-scope-test.el ---

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

;;

;;; Code:

(ert-deftest test-inside-string-p ()
  (should (with-temp-buffer
            (apprentice-mode)
            (insert "
defmodule Module.Name do

  @moduledoc \"\"\"
  ## Examples

  ....
  \"\"\"

end")
            (goto-line-non-inter 7)
            (apprentice-scope-inside-string-p)))
  (should (not (with-temp-buffer
                 (apprentice-mode)
                 (insert "
defmodule Module.Name do

  @moduledoc \"\"\"
  ## Examples

  ....
  \"\"\"

end")
                 (goto-line-non-inter 3)
                 (apprentice-scope-inside-string-p)))))

(ert-deftest test-inside-module-p ()
  (should (with-temp-buffer
            (apprentice-mode)
            (insert "
defmodule Foo do

end")
            (goto-line-non-inter 3)
            (apprentice-scope-inside-module-p)))
  (should (not (with-temp-buffer
                 (apprentice-mode)
                 (insert "

defmodule Foo do

end")
                 (goto-line-non-inter 2)
                 (apprentice-scope-inside-module-p)))))

(ert-deftest test-aliases-from-current-module ()
  (should (equal (list '("Phoenix.Router.Scope" "Scope")
                       '("Phoenix.Router.Resource" "Special"))
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  alias Phoenix.Router.Resource, as: Special
  alias Phoenix.Router.Scope

  @doc false
  defmacro scope(path, options, do: context) do
    options = quote do
      path = unquote(path)
      case unquote(options) do
        alias when is_atom(alias) -> [path: path, alias: alias]
        options when is_list(options) -> Keyword.put(options, :path, path)
      end
    end
    do_scope(options, context)
  end

end")
                   (apprentice-scope-aliases))))
  (should (equal (list '("String.Break" "Break")
                       '("String.Casing" "Casing"))
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Test do

  alias String.{Break, Casing}

end")
                   (apprentice-scope-aliases))))
  (should (equal (list '("Phoenix.Router.Resource" "Resource")
                       '("Phoenix.Router.Scope" "Scope"))
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  alias Phoenix.Router.{Resource, Scope}

end")
                   (apprentice-scope-aliases))))
  (should (equal (list '("Phoenix.Router.Scope" "Scope")
                       '("Phoenix.Router.Resource" "Special")
                       '("List.Chars.BitString" "BitString")
                       '("List.Chars.Integer" "Integer")
                       '("List.Chars.Atom" "Atom")
                       '("List.Chars.Float" "Float"))
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  alias List.Chars.{Atom, Float}
  alias Phoenix.Router.Resource, as: Special
  alias List.Chars.{BitString, Integer}
  alias Phoenix.Router.Scope

end")
                   (apprentice-scope-aliases)))))

(ert-deftest test-scope-module ()
  (should (equal "Phoenix.Router"
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  defmacro scope(path, options, do: context) do
    options = quote do
      path = unquote(path)
      case unquote(options) do
        alias when is_atom(alias) -> [path: path, alias: alias]
        options when is_list(options) -> Keyword.put(options, :path, path)
      end
    end
    do_scope(options, context)
  end

end")
                   (goto-line-non-inter 6)
                   (apprentice-scope-module)))))

(ert-deftest test-scope-module/skip-heredoc ()
  (should (equal "Module.Name"
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Module.Name do

  @moduledoc \"\"\"
  ## Examples

  Phoenix defines the view template at `web/web.ex`:

      defmodule YourApp.Web do
        def view do
          quote do
            use Phoenix.View, root: \"web/templates\"

            # Import common functionality
            import YourApp.Router.Helpers

            # Use Phoenix.HTML to import all HTML functions (forms, tags, etc)
            use Phoenix.HTML
          end
        end

        # ...
      end
  \"\"\"

end")
                   (goto-line-non-inter 12)
                   (apprentice-scope-module)))))

(ert-deftest test-scope-module/nested-modules ()
  (should (equal "Inside"
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Outside do
  defmodule Inside do

  end
end")
                   (goto-line-non-inter 4)
                   (apprentice-scope-module)))))

(ert-deftest test-scope-use-modules ()
  (should (equal '("GenServer" "Behaviour")
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  use GenServer
  use Behaviour

end")
                   (goto-line-non-inter 6)
                   (apprentice-scope-use-modules)))))

(ert-deftest test-scope-use-modules/nested-modules ()
  (should (equal '("Macro" "Nice.Macro")
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  use GenServer
  use Behaviour

  defmodule Parser do

    use Macro
    use Nice.Macro
  end

end")
                   (goto-line-non-inter 12)
                   (apprentice-scope-use-modules)))))

(ert-deftest test-scope-import-modules ()
  (should (equal '("Test" "ExUnit")
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  import Test
  import ExUnit
  import Mix.Generator

end")
                   (goto-line-non-inter 6)
                   (apprentice-scope-import-modules)))))

(ert-deftest test-scope-import-modules/nested-modules ()
  (should (equal '("Love")
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  import Test
  import ExUnit

  defmodule Parser do

    import Love

  end

end")
                   (goto-line-non-inter 10)
                   (apprentice-scope-import-modules)))))

(ert-deftest test-scope-extract-module ()
  (should (equal (apprentice-scope-extract-function ":gen_tcp.accept")
                 "accept"))
  (should (equal (apprentice-scope-extract-function ":erlang")
                 nil))
  (should (equal (apprentice-scope-extract-function "List.duplicate")
                 "duplicate"))
  (should (equal (apprentice-scope-extract-function "_duplicate")
                 "_duplicate"))
  (should (equal (apprentice-scope-extract-function "_duplicated?")
                 "_duplicated?"))
  (should (equal (apprentice-scope-extract-function "parse!")
                 "parse!"))
  (should (equal (apprentice-scope-extract-function "Enum.take!")
                 "take!"))
  (should (equal (apprentice-scope-extract-function "String.Chars.impl_for")
                 "impl_for"))
  (should (equal (apprentice-scope-extract-function "String.Chars.Atom")
                 nil)))

(ert-deftest test-scope-extract-module ()
  (should (equal (apprentice-scope-extract-module ":gen_tcp.accept")
                 ":gen_tcp"))
  (should (equal (apprentice-scope-extract-module ":erlang")
                 ":erlang"))
  (should (equal (apprentice-scope-extract-module "List.duplicate")
                 "List"))
  (should (equal (apprentice-scope-extract-module "Whatever._duplicate")
                 "Whatever"))
  (should (equal (apprentice-scope-extract-module "Module.read!")
                 "Module"))
  (should (equal (apprentice-scope-extract-module "String.Chars.impl_for")
                 "String.Chars"))
  (should (equal (apprentice-scope-extract-module "String.Chars.Atom")
                 "String.Chars.Atom"))
  (should (equal (apprentice-scope-extract-module "String.Chars.")
                 "String.Chars"))
  (should (equal (apprentice-scope-extract-module "String.concat")
                 "String"))
  (should (equal (apprentice-scope-extract-module "to_string")
                 nil)))

(ert-deftest test-scope-alias-full-path ()
  (should (equal "Phoenix.Router.Scope"
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Phoenix.Router do

  alias Phoenix.Router, as: Special

end")
                   (apprentice-scope-alias-full-path "Special.Scope"))))
  (should (equal "Phoenix.Endpoint.Watcher.Everywhere"
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Foo do

  alias Phoenix.Endpoint.Watcher

end")
                   (apprentice-scope-alias-full-path "Watcher.Everywhere"))))
  (should (equal "List"
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Foo do

  alias List, as: LT

end")
                   (apprentice-scope-alias-full-path "LT"))))
  (should (equal "def"
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Foo do

  alias Phoenix.Endpoint.Watcher

end")
                   (apprentice-scope-alias-full-path "def"))))
  (should (equal nil
                 (with-temp-buffer
                   (apprentice-mode)
                   (insert "
defmodule Foo do

  alias Phoenix.Endpoint.Watcher

end")
                   (apprentice-scope-alias-full-path "")))))

(provide 'apprentice-scope-test)

;;; apprentice-scope-test.el ends here
