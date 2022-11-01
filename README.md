# Boogie friends

A collection of tools for interacting with Boogie and related languages.

## Emacs package (`boogie-mode`, `dafny-mode`, `z3-smt2-mode`)

The `boogie-friends` package is an experimental collection of Emacs modes for
writing verified programs in z3 and languages of the Boogie family (including Dafny).

Notable features are listed below:

* Syntax highlighting
* Real-time verification (using `flycheck`)

In addition, the Dafny and Boogie modes offer:

* Completion (using `company`)
* Code folding (using `hideshow`)
* Prettification (using `prettify-symbols-mode`)

And the Dafny mode additionally also has:

* (A few) Snippets (using `yasnippet`)
* (Some) In-Emacs documentation
* (Experimental) Navigation between Dafny and Boogie source files
* (Some support for) jumping to a definition
* (Experimental) support for using Dafny as a verification server. This means that Emacs spawns a server process, and uses Dafny's caching facilities to (massively) improve reactivity.

### Some pictures:

#### A Dafny buffer

![Dafny buffer in Emacs](emacs/pictures/dafny-overview.png)

Notice the error highlighting, the symbol beautification (`forall` appears as `∀`), and the code folding on the last line!

#### A Z3 buffer

![Z3 buffer in Emacs](emacs/pictures/z3-overview.png)

#### A Boogie buffer

![Boogie buffer in Emacs](emacs/pictures/boogie-overview.png)

#### Completion and snippets

![Completion in Boogie](emacs/pictures/boogie-completion.png)
![Completion in Dafny](emacs/pictures/dafny-completion.png)
![Snippets](emacs/pictures/dafny-snippets.png)

#### Documentation (Dafny only)

![Dafny docs](emacs/pictures/dafny-docs.png)

#### Browsing the Boogie translation of a Dafny file

![Dafny buffer in Emacs](emacs/pictures/dafny-to-boogie.png)

### Setup

1. Setup MELPA by adding the following lines to your `.emacs` if you don't have them already (here's [more information](http://melpa.org/#/getting-started) if you have trouble with this step):

    ```elisp
    (require 'package) ;; You might already have this line
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    (package-initialize) ;; You might already have this line
    ```

2. Install the package: `M-x package-refresh-contents RET`, then `M-x package-install RET boogie-friends RET`

3. For Dafny, just use `M-x lsp` to download and run the Dafny language server.  For other languages (or to use Dafny's legacy server), configure Flycheck as shown below.

### On-the-fly verification

### For Boogie and Z3

Use the following settings to point `boogie-friends` to Boogie and Z3 binaries:

```elisp
;; To get real-time error highlights (Flycheck) in Boogie files
(setq flycheck-boogie-executable "PATH-TO-BOOGIE-BINARIES/Boogie")

;; To get real-time error highlights (Flycheck) in Z3 files
(setq flycheck-z3-executable "PATH-TO-Z3-BINARIES/z3")

;; To use Z3's axiom profiler
(setq boogie-friends-profile-analyzer-executable "PATH-TO-Z3-AXIOM-PROFILER")
```

### For Dafny

`dafny-mode` supports three real-time verification mechanisms: `cli`, `server`, and `lsp`.  LSP is the recommended one; it supports automatic downloads and advanced IDE features.  To use it, open a Dafny buffer and type `M-x lsp` (optionally, use `M-x customize-variable RET lsp-dafny-preferred-version` first to chose which version of Dafny to install and run).

As an alternative (if you run into stability or performance issues with LSP), you can either:

- Get on-the-fly verification through the legacy Dafny server, which more basic than LSP but also more robust, by downloading a Dafny release manually and configuring `flycheck-inferior-dafny-executable`:

  ```elisp
  (setq flycheck-inferior-dafny-executable "PATH-TO-DAFNY-BINARIES/DafnyServer")
  (setq dafny-verification-backend 'server)
  ```

- Get on-the-fly verification through the command line, which is slower (no caching) but even more robust, by downloading a Dafny release manually and configuring `flycheck-dafny-executable`:

  ```elisp
  (setq flycheck-dafny-executable "PATH-TO-DAFNY-BINARIES/DafnyServer")
  (setq dafny-verification-backend 'cli)
  ```

If you run into issues, `C-c ! v` (`flycheck-verify-setup`) should have debugging info.

### For Boogie


### Keybindings

#### All modes

* <kbd>C-c C-c</kbd> re-verifies the current file. With a prefix argument (<kbd>C-u C-c C-c</kbd>), extra arguments are sent to the verifier (by default `/trace`).
* <kbd>S-TAB</kbd> manually cycles through reasonable indentation levels.

#### Dafny and Boogie

* <kbd>C-c C-t</kbd> gets a verification trace for the current file, and parses the resulting timings.
* <kbd>C-c C-p</kbd> prompts for a method name, generates a tracing profile of that method, and launches the profile analyzer (`boogie-friends-profile-analyzer-executable`) on the resulting trace.

#### Dafny only

* <kbd>TAB</kbd> auto-indents.
* <kbd>C-c C-?</kbd> opens the Dafny docs.
* <kbd>&lt;C-down-mouse-1></kbd> looks for the definition of the function under point in open buffers.
* <kbd>C-c C-a</kbd> translates the current file to Boogie and shows the translated file.
* <kbd>C-c C-j</kbd> or <kbd>C-S-down-mouse-1</kbd> (aka <kbd>Ctrl-Shift-Click</kbd>) jumps to the Boogie line matching the current Dafny line.

* After inserting a snippet, <kbd>TAB</kbd> moves to the next snippet field, and <kbd>C-d</kbd> removes the current field entirely.
* During completion, <kbd>C-h</kbd> shows documentation for the current snippet, if available.

### Tips

#### General

* Completion, indentation, snippets, syntax coloring, and real-time verification should work out of the box.
* Verification happens as you type, and its status is shows in the mode line (`FlyC*`: busy; `FlyC:a/b`: done with `a` errors and `b` warnings).

#### Real-time error highlighting

Real-time error highlighting is enabled by default for all languages. You can disable it by adding `(setq flycheck-disabled-checkers '(dafny inferior-dafny))` to your `.emacs`.

#### Font support

If you see blocks instead of proper characters, or tall characters, or ugly characters:

1. Install a good font and restart Emacs (Arial Unicode, Cambria, Segoe UI Symbol, DejaVu Sans Mono, FreeMono, STIX, Unifont and [Symbola](http://users.teilar.gr/~g1951d/Symbola.zip) should all work).

2. If that doesn't fix it, setup font fallback by adding the following to your `.emacs` (replace `"Symbola"` by the name of your font):
    ```elisp
    (set-fontset-font t 'unicode (font-spec :name "Symbola") nil 'append)
    ```

3. If that still doesn't work, turn of prettification entirely by adding the following to your `.emacs`:
    ```elisp
    (defun no-prettification-in-dafny-mode ()
      (prettify-symbols-mode -1))
    (add-hook 'dafny-mode-hook #'no-prettification-in-dafny-mode)
    ```

If you don't like the way one particular symbol is rendered, you can adjust the font for just that one:

```elisp
(set-fontset-font t (cons ?≔ ?≔) "FreeSerif" nil 'prepend)
```

#### Profiling

A typical profiling workflow proceeds as follows:

1. Open a file for which verification is slow, or times out.
2. Use <kbd>C-c C-t</kbd> to generate a trace (the default timeout is set to 30s; you can customize it by changing `boogie-friends-profiler-timeout`).
3. Use <kbd>C-c C-p</kbd> to profile a function. The slowest method (as determined by the trace) is presented first.
4. Marvel at the intricacies of the axiom profiler.

Note: The axiom profiler works best if it has a Boogie source file to look at; thus, when profiling a Dafny source file, `boogie-friends` transparently saves it as a translated Boogie file first, and then runs Boogie (with profiling enabled) on it. Thus the profiler is Boogie in all cases, and custom prover arguments need to be set for Boogie if they are to be taken into account for profiling (for tracing and translation, however, Dafny's settings apply).

#### Custom prover configurations

Each time `boogie-friends` calls a prover, it collects arguments from four sources:

* `LANGUAGE-prover-args`, the list of arguments passed to the prover in the default configuration (i.e. `dafny-prover-args` and `boogie-prover-args`). This has pretty good defaults, and probably shouldn't be changed.

* `LANGUAGE-prover-custom-args`, a list of extra flags. This is empty by default, and is a good place to add your own flags.

 `LANGUAGE-prover-local-args`, another list of extra flags. This is empty by default, and is a good place to add per-file or per-directory flags (see below).

* `LANGUAGE-prover-alternate-args`, a list of flags added to the prover invocation when running `verify/compile` with a prefix argument (<kbd>C-u C-c C-c</kbd>). This is a good place to add flags that you do not always need; for example `"/compile:3"` (this is the default).

An example configuration might thus look like this:

```elisp
;; Don't allow assumptions
(setq dafny-prover-custom-args '("/noCheating:1"))

;; Get more debug output when verifying with C-u C-c C-c
(setq dafny-prover-alternate-args '("/proverWarnings:2" "/traceverify" "/z3opt:TRACE=true" "/trace" "/traceTimes" "/tracePOs"))
```

The `LANGUAGE-prover-local-args` is useful if a file requires specific flags (maybe `/vcsMaxKeepGoingSplits`, for example): in that case you can set the `LANGUAGE-prover-local-args` [in just that file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html) or [in the corresponding directory](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html).

For example, you can add the following to the top of a file:

```elisp
// -*- dafny-prover-local-args: ("/vcsMaxKeepGoingSplits:5" "/proverMemoryLimit:250") -*-
```

### Troubleshooting

* If you run into trouble LSP, consider using the legacy Dafny server.  If you run into issues with *that*, then the CLI-based verification backend should be fine, though slower (see instructions at the top of this README).  You can also disable on-the-fly verification using `(setq dafny-verification-backend 'cli)`.

* If the verification seems to be taking forever, `M-x inferior-dafny-reset` may help when using the legacy server.  With LSP, typing any character interrupts verification.

### Acknowledgments

The documentation that ships with this package is auto-generated from the [Dafny Quick Reference](http://research.microsoft.com/en-us/projects/dafny/reference.aspx).

### Pull requests are welcome!

Clone the repo:

```bash
mkdir -p ~/.emacs.d/lisp/ && cd ~/.emacs.d/lisp/
git clone https://github.com/boogie-org/boogie-friends
```

Then in your .emacs (in addition to the stuff above):

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/boogie-friends/emacs/")
(require 'dafny-mode)
(require 'boogie-mode)
```
