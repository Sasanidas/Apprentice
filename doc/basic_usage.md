## Mix

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a x</kbd>|Prompt for a mix command including a list of all available mix commands. `apprentice-mix`|
|<kbd>C-c a m c</kbd>|Compile the whole elixir project. `apprentice-mix-compile`|
|<kbd>C-c a m r</kbd>|Runs the given file or expression in the context of the application. `apprentice-mix-run`|
|<kbd>C-c a m l</kbd>|Rerun the last mix task which was run by apprentice. `apprentice-mix-rerun-last-task`|

Mix tasks could also be executed in a specific environment with the usage of `C-u` (universal-argument).
Default environments are `prod`, `dev` and `test`. [Mix environments](http://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#environments)

### Mix Task Mode

The Mix tasks running in a separate `apprentice-mix-mode`, in which the following keybindings are available:

| Keybinding   | Description                                          |
|--------------|------------------------------------------------------|
|<kbd>q</kbd>  |Quit `*mix*` buffer window                            |
|<kbd>i</kbd>  |Send an input to the current running mix task process.|
|<kbd>r</kbd>  |Rerun the last mix task which was run by apprentice.   |

## Mix Hex

| Keybinding   | Description                                          |
|--------------|------------------------------------------------------|
|<kbd>C-c a X i</kbd>  | Display Hex package information for the package at point. |
|<kbd>C-c a X r</kbd>  | Display Hex package releases for the package at point. |
|<kbd>C-c a X I</kbd>  | Display Hex package info for a certain package. |
|<kbd>C-c a X R</kbd>  | Display Hex package releases for a certain package.|
|<kbd>C-c a X s</kbd>  | Search for Hex packages. |
|<kbd>C-c a X d</kbd>  | Display Hex package dependencies for the current Mix project. |


## Testing

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a t</kbd>|Run the whole elixir test suite. `apprentice-mix-test`|
|<kbd>C-c a r</kbd>|Rerun the last test that was run by apprentice. `apprentice-mix-rerun-last-test`|
|<kbd>C-c a m t f</kbd>|Run `apprentice-mix--test-file` with the FILENAME. `apprentice-mix-test-file`|
|<kbd>C-c a m t b</kbd>|Run the current buffer through mix test. `apprentice-mix-test-this-buffer`|
|<kbd>C-c a m t .</kbd>|Run the test at point. `apprentice-mix-test-at-point`|
|<kbd>C-c a m t s</kbd>|Run only stale tests (Elixir 1.3+). `apprentice-mix-test-stale` |
|<kbd>C-c a m t r</kbd>|Rerun the last test that was run by apprentice. `apprentice-mix-rerun-last-test` |
|<kbd>C-c M-r</kbd>|Toggle between displaying or hidding the test report buffer. `apprentice-test-toggle-test-report-display`|

## Compile And Execute

### Compile functions

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a c b</kbd>|Compile the current buffer with the `elixirc` command. `apprentice-compile-this-buffer`|
|<kbd>C-c a c f</kbd>|Compile the given `FILENAME` with the `elixirc` command. `apprentice-compile-file`|
|<kbd>C-c a c c</kbd>|Run a custom compile command with `elixirc`. `apprentice-compile`|

### Execute functions

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a e b</kbd>|Run the current buffer through `elixir` command. `apprentice-execute-this-buffer`|
|<kbd>C-c a e f</kbd>|Run `elixir` command with the given `FILENAME`. `apprentice-execute-file` |
|<kbd>C-c a e e</kbd>|Run a custom execute command with `elixir`. `apprentice-execute` |

## Project

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a p s</kbd>|Toggle between a file and its tests in the current window. `apprentice-project-toggle-file-and-tests`|
|<kbd>C-c a p o</kbd>|Toggle between a file and its tests in other window. `apprentice-project-toggle-file-and-tests-other-window`|
|<kbd>C-c a p t</kbd>|Run the tests related to the current file. `apprentice-project-run-tests-for-current-file`|
|<kbd>C-c a p f</kbd>|List all files available in the `test` directory. `apprentice-project-find-test`|
|<kbd>C-c a p l</kbd>|List all files available in the `lib` directory. `apprentice-project-find-lib` |

## apprentice-phoenix-mode

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a n w</kbd>|List all files available in the `web` directory. `apprentice-phoenix-find-web`|
|<kbd>C-c a n c</kbd>|List all controllers in `web/controllers` directory. `apprentice-phoenix-find-controllers`|
|<kbd>C-c a n l</kbd>|List all channels in `web/channels` directory. `apprentice-phoenix-find-channels`|
|<kbd>C-c a n t</kbd>|List all templates in `web/templates` directory. `apprentice-phoenix-find-templates`|
|<kbd>C-c a n m</kbd>|List all models in `web/models` directory. `apprentice-phoenix-find-models`|
|<kbd>C-c a n v</kbd>|List all views in `web/views` directory. `apprentice-phoenix-find-views`|
|<kbd>C-c a n s</kbd>|List all files in `web/static` directory. `apprentice-phoenix-find-static`|
|<kbd>C-c a n r</kbd>|Open the `router.ex` file in `web` directory. `apprentice-phoenix-router`|
|<kbd>C-c a n R</kbd>|Run the Mix task `phoenix.routes`. `apprentice-phoenix-routes`|

## Documentation lookup

There is the `apprentice-help-minor-mode` for a complete fully functional
interface to the Elixir documentation. The `apprentice-help-minor-mode` uses the
same functions like Elixir's [IEx](http://elixir-lang.org/docs/stable/iex/).

What does that mean? It means no matter which Elixir version is currently
installed on the system, the documentation you get by `apprentice` is the same
`IEx` would deliver.

| Keybinding | Description                                     |
|------------|-------------------------------------------------|
|<kbd>C-c a h h</kbd>| Run a custom search. `apprentice-help`              |
|<kbd>C-c a h i</kbd>| Look through search history. `apprentice-help-history` |
|<kbd>C-c a h e</kbd>| Run `apprentice-help` with the expression under the cursor. (example: `is_binary`  or `Code.eval_string`). If there is a currently marked region this will be used as the search term. `apprentice-help-search-at-point` |
|<kbd>C-c a h r</kbd>| Open a buffer with a refcard of apprentice bindings. `apprentice-refcard`|

## IEx

Apprentice provides a `REPL` buffer, connected to an
[Elixir IEx](http://elixir-lang.org/docs/master/iex/IEx.html) subprocess.

To start an IEx process just run <kbd>M-x apprentice-iex-run</kbd>

To start an IEx process in the context of an Elixir project (`iex -S mix`) just run <kbd>M-x apprentice-iex-project-run</kbd>

To start a custom IEx process with additional arguments (like: `iex --sname custom`) just use the
[universal-argument](http://www.gnu.org/software/emacs/manual/html_node/emacs/Arguments.html) <kbd>C-u</kbd>
before run <kbd>M-x apprentice-iex-run</kbd>

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c a i i</kbd>| Start an IEx process. `apprentice-iex-run`|
|<kbd>C-c a i p</kbd>| Start an IEx process with mix (`iex -S mix`). `apprentice-iex-project-run`|
|<kbd>C-c a i l</kbd>| Sends the current line to the IEx process. `apprentice-iex-send-current-line`|
|<kbd>C-c a i c</kbd>| Sends the current line to the IEx process and jump to the buffer. `apprentice-iex-send-current-line-and-go`|
|<kbd>C-c a i r</kbd>| Sends the marked region to the IEx process. `apprentice-iex-send-region`|
|<kbd>C-c a i m</kbd>| Sends the marked region to the IEx process and jump to the buffer. `apprentice-iex-send-region-and-go`|
|<kbd>C-c a i b</kbd>| Compiles the current buffer in the IEx process. `apprentice-iex-compile-this-buffer`|
|<kbd>C-c a i R</kbd>| Recompiles and reloads the current module in the IEx process. `apprentice-iex-reload-module`|

## Eval

Apprentice comes with the functionality to evaluate code inside the buffer.

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c a v l</kbd>| Evaluate the Elixir code on the current line. `apprentice-eval-current-line`.|
|<kbd>C-c a v k</kbd>| Evaluate the Elixir code on the current line and insert the result. `apprentice-eval-print-current-line`.|
|<kbd>C-c a v j</kbd>| Get the Elixir code representation of the expression on the current line. `apprentice-eval-quoted-current-line`. |
|<kbd>C-c a v h</kbd>| Get the Elixir code representation of the expression on the current line and insert the result. `apprentice-eval-print-quoted-current-line`. |
|<kbd>C-c a v o</kbd>| Evaluate the Elixir code on marked region. `apprentice-eval-region`.|
|<kbd>C-c a v i</kbd>| Evaluate the Elixir code on marked region and insert the result. `apprentice-eval-print-region`.|
|<kbd>C-c a v u</kbd>| Get the Elixir code representation of the expression on marked region. `apprentice-eval-quoted-region`.|
|<kbd>C-c a v y</kbd>| Get the Elixir code representation of the expression on marked region and insert the result. `apprentice-eval-print-quoted-region`.|
|<kbd>C-c a v q</kbd>| Evaluate the Elixir code in the current buffer. `apprentice-eval-buffer`.|
|<kbd>C-c a v w</kbd>| Evaluate the Elixir code in the current buffer and insert the result. `apprentice-eval-print-buffer`.|
|<kbd>C-c a v e</kbd>| Get the Elixir code representation of the expression in the current buffer. `apprentice-eval-quoted-buffer`.|
|<kbd>C-c a v r</kbd>| Get the Elixir code representation of the expression in the current buffer and insert result. `apprentice-eval-print-quoted-buffer`.|
|<kbd>C-c a v !</kbd>| Quit the Elixir evaluation popup window. `apprentice-eval-close-popup`.|

## Macroexpand

| Keybinding | Description | Command |
|--------------------|------------------------------------------|------------------------------------------------|
|<kbd>C-c a o l</kbd>| Macro expand once on the current line. | `apprentice-macroexpand-once-current-line`.|
|<kbd>C-c a o L</kbd>| Macro expand once on the current line and print the result. | `apprentice-macroexpand-once-print-current-line`.|
|<kbd>C-c a o k</kbd>| Macro expand on the current line. | `apprentice-macroexpand-current-line`.|
|<kbd>C-c a o K</kbd>| Macro expand on the current line and print the result. | `apprentice-macroexpand-print-current-line`.|
|<kbd>C-c a o i</kbd>| Macro expand once on region. | `apprentice-macroexpand-once-region`.|
|<kbd>C-c a o I</kbd>| Macro expand once on region and print the result. | `apprentice-macroexpand-once-print-region`.|
|<kbd>C-c a o r</kbd>| Macro expand on region. | `apprentice-macroexpand-region`.|
|<kbd>C-c a o R</kbd>| Macro expand on region and print the result. | `apprentice-macroexpand-print-region`.|
|<kbd>C-c a o !</kbd>| Quit the Elixir macroexpand popup window. | `apprentice-macroexpand-close-popup`.|

**Note**

Macroexpand works currently only for Elixir core macros, but why is this?

> Macros are lexical: it is impossible to inject code or macros globally. In order to use a macro, you need to explicitly require or import the module that defines the macro.

The [Apprentice-Server](https://github.com/tonini/apprentice-server) is currently under development to handle more knowledge about the current context. After that update expanding custom macros will supported too.

But if you like to expand a custom macro in the mean time and you know where it comes from, you can do something like the following with the Apprentice inline evaluation functionality.

As example, select the code and call the `apprentice-eval-print-region` and you get the macro expansion below.

```elixir
require Unless # In order to use a macro, you need to explicitly require the module
expr = quote do: Unless.macro_unless(true, IO.puts "this should never be printed")
res  = Macro.expand_once(expr, __ENV__)
IO.puts Macro.to_string(res)
# => if(!true) do
# =>   IO.puts("this should never be printed")
# => end
# => :ok
```

## Datatype Informations

With Elixir `v1.2` comes two new `IEx` helper functions `t/1` and `i/1`.

- Display type docs with `t(Module.type)` and `t(Module.type/arity)`
- Prints information about any data type with `i/1`.

These two helper functions are available now with the following keybindings/functions.

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c a f i</kbd>| Prints information about any datatype under the cursor. `apprentice-info-datatype-at-point` |
|<kbd>C-c a f t</kbd>| Prints information of types under the cursor. `apprentice-info-types-at-point` |

## Testing Mode

Apprentice comes with an minor mode for testing which will be enabled by default inside `*_test.exs` files.

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c , s</kbd>| Run the test at point. `apprentice-mix-test-at-point` |
|<kbd>C-c , v</kbd>| Run all tests in the current file. `apprentice-mix-test-this-buffer` |
|<kbd>C-c , a</kbd>| Run the whole elixir test suite. `apprentice-mix-test` |
|<kbd>C-c , f</kbd>| Run all tests of a specific file `apprentice-mix-test-file` |
|<kbd>C-c , n</kbd>| Jump to the next test inside the current file. `apprentice-test-mode-jump-to-next-test` |
|<kbd>C-c , p</kbd>| Jump to the previous test inside the current file `apprentice-test-mode-jump-to-previous-test` |

### Testing Report

The tests are reported in the `apprentice-test-report-mode`, which have the following keybindings:

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>r</kbd>| Rerun the latest test run. `apprentice-mix-rerun-last-test` |
|<kbd>t</kbd>| Toggle truncating of long lines for the current test buffer. `toggle-truncate-lines` |
|<kbd>M-n</kbd>| Jump to the next error in the test report. `apprentice-test-next-result` |
|<kbd>M-p</kbd>| Jump to the previous error in the test report. `apprentice-test-previous-result` |
|<kbd>M-N</kbd>| Jump to the next stacktrace file in the test report. `apprentice-test-next-stacktrace-file` |
|<kbd>M-P</kbd>| Jump to the previous stacktrace file in the test report. `apprentice-test-previous-stacktrace-file` |
|<kbd>C-c C-k</kbd>| Interrupt the current running report process. `apprentice-report-interrupt-current-process` |
|<kbd>q</kbd>| Close the test report window |

## Keymap

Apprentice comes with a default keymap.

The the default prefix keybinding is <kbd>C-c a</kbd>

### Refcards

You find and overview of all the key-bindings on the [Apprentice-Refcard](https://github.com/tonini/apprentice.el/blob/master/doc/apprentice-refcard.pdf?raw=true).

There is also a refcard for usage inside Emacs, which gets dynamically generated with the current adjusted keybindings.
If you use the keybinding <kbd>i</kbd> on a specific row, it will call `describe-function` on that function.

Just `M-x apprentice-refcard RET`
