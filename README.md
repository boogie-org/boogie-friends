# Boogie friends

A collection of tools for interacting with Boogie and related languages.

## Emacs package (`boogie-mode`, `dafny-mode`)

The `boogie-friends` package is an experimental collection of Emacs modes for
writing verified programs in languages of the Boogie family. Dafny and Boogie
are the two currently supported languages. Features include:

* Syntax highlighting
* Real-time compilation (using `flycheck`)
* Completion (using `company`)
* Code folding (using `hideshow`)
* Prettification (using `prettify-symbols-mode`)

In addition, the Dafny mode offers:

* (A few) Snippets (using `yasnippet`)
* (Some) In-Emacs documentation
* (Experimental) Navigation between Dafny and Boogie source files
* (Some support for) indentation
* (Some support for) jumping to a definition

### Some pictures:

#### A Dafny buffer

![Dafny buffer in Emacs](emacs/pictures/dafny-overview.png)

Notice the error highlighting, the symbol beautification (`forall` appears as `∀`), and the code folding on the last line!

#### A Boogie buffer

![Boogie buffer in Emacs](emacs/pictures/boogie-overview.png)

#### Completion and snippets

![Completion in Boogie](emacs/pictures/boogie-completion.png)
![Completion in Dafny](emacs/pictures/dafny-completion.png)
![Snippets](emacs/pictures/dafny-snippets.png)

#### Documentation (Dafny only)

![Dafny docs](emacs/pictures/dafny-docs.png)

#### Browsing the Boogie representation

![Dafny buffer in Emacs](emacs/pictures/dafny-to-boogie.png)

### Setup:

```bash
mkdir -p ~/.emacs.d/lisp/ && cd ~/.emacs.d/lisp/
git clone https://github.com/boogie-org/boogie-friends
```

Then in your .emacs:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/boogie-friends/emacs/")

(require 'dafny-mode)
(require 'boogie-mode)

(defun setup-boogie-friends ()
  (setq flycheck-dafny-executable "PATH-TO-DAFNY")
  (setq flycheck-boogie-executable "PATH-TO-BOOGIE"))

(add-hook 'boogie-friends-hook #'setup-boogie-friends)
```

### Acknowledgments

The documentation that ships with this package is auto-generated from the [Dafny Quick Reference](http://research.microsoft.com/en-us/projects/dafny/reference.aspx).
