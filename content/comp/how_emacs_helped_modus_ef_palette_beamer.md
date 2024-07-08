+++
title = "How Emacs Helped Me Today: modus-themes and ef-themes Palettes for LaTeX"
author = ["Aatmun Baxi"]
date = 2024-07-05
tags = ["emacs", "lisp", "LaTeX"]
draft = false
weight = 2002
type = "post"
+++

I'm very fond of Prot's `modus-themes` and `ef-themes` for emacs.
They're highly legible, easy on the eyes, and attractive.
Recently I was making a beamer presentation for a talk I'm currently procrastinating on.
For better or for worse, I thought to myself _I wish I could make the PDF output have modus theme colors_.

After manually reading off some colors from `modus-operandi-tinted-palette` into my LaTeX preamble, I remembered: _Wait, this is emacs. There's a better way to do this._

Happily, Prot designed the `modus-themes` and `ef-themes` palettes in simple way; they are just lists of lists with the name and hex value of the color.
Here's an excerpt of `modus-operandi-tinted-palette`:

```emacs-lisp
((bg-main "#fbf7f0")
 (bg-dim "#efe9dd")
 (fg-main "#000000")
 (fg-dim "#595959")
 (fg-alt "#193668")
 (bg-active "#c9b9b0")
 (bg-inactive "#dfd5cf")
 (border "#9f9690")
 (red "#a60000")
 ... )
```

It's easy enough to write a function to read from the palette and generate LaTeX directives for defining (most of) the colors.
We want to fill a file with color directives that look like this:

```text
\definecolor{COLOR-NAME}{HTML}{XXXXXX}
```

Here, the `=COLOR-NAME` will just be the name of the symbol that the palette gives to a color, and the `XXXXXX` will be the HTML (hex) code, sans the leading `#`.

The code I wrote is

```emacs-lisp
(defun gen-emacs-theme-html-latex-colors (theme-palette)
  (pcase-dolist (`(,color-name ,hex) theme-palette)
    (insert
     (concat "\\definecolor{" (symbol-name color-name) "}{HTML}{"))
    (let ((rgb (substring  hex 1)))
      (insert (concat rgb "}\n")))))
```

All this does is loop through `theme-palette`, and for each element, insert the LaTeX directive we wanted, separated by newlines.
Here's an excerpt of the generated output for `modus-operandi-tinted`:

```text
\definecolor{bg-main}{HTML}{fbf7f0}
\definecolor{bg-dim}{HTML}{efe9dd}
\definecolor{fg-main}{HTML}{000000}
\definecolor{fg-dim}{HTML}{595959}
\definecolor{fg-alt}{HTML}{193668}
\definecolor{bg-active}{HTML}{c9b9b0}
\definecolor{bg-inactive}{HTML}{dfd5cf}
\definecolor{border}{HTML}{9f9690}
...
```

There is a limitation, however.
The palette is self referential, meaning the hex color value of some the colors is the symbol of a previously defined color.
The function terminates running on these, but at that point, there are something like 119 unique colors already defined, more than enough to make a coherent beamer theme.

Defining the beamer theme is then a matter of using `\setbeamercolor` commands and change the colors of the desired presentation elements using a guide such as [this one.](https://www.cpt.univ-mrs.fr/~masson/latex/Beamer-appearance-cheat-sheet.pdf)
Note also that the `ef-themes` use the same color palette format, so this function will work exactly the same for them.

Happy scripting!
